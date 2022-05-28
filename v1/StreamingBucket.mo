import Array "mo:base/Array";
import Blob "mo:base/Blob";
import Buffer "mo:base/Buffer";
import Cycles "mo:base/ExperimentalCycles";
import Debug "mo:base/Debug";
import Error "mo:base/Error";
import Iter "mo:base/Iter";
import Nat8 "mo:base/Nat8";
import Nat16 "mo:base/Nat16";
import Nat32 "mo:base/Nat32";

import List "mo:base/List";
import Deque "mo:base/Deque";

import Nat64 "mo:base/Nat64";
import Option "mo:base/Option";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import SM "mo:base/ExperimentalStableMemory";
import Text "mo:base/Text";
import TrieMap "mo:base/TrieMap";
import Types "Lib/Types";

import HttpParser "mo:httpie/Parser";
import HttpResponse "mo:httpie/Response";
import F "mo:format";
import Moh "mo:op";
import Utils "Lib/Utils";

shared(installer) actor class Bucket() = this{
    private type StoreArgs              = Types.StoreArgs;
    private type IspInterface           = Types.IspInterface;
    private let CYCLE_THRESHOLD         = 3_000_000_000_000;
    private let ISP : IspInterface      = actor (Principal.toText(installer.caller));
    private stable var offset           = 0;
    stable let CHUNKS_PER_STORE = 5;
    stable let MAX_BYTES_PER_CALL = 2_097_152;

    //           (chunkId, start, length)
    type Chunk = (Nat8, Nat64, Nat);
    type Stream = [Chunk];

    private stable var stream_entries : [ (Text, Stream)] = [];

    private let stream_assets : TrieMap.TrieMap<Text, Stream> = TrieMap.fromEntries<Text, Stream>(stream_entries.vals(), Text.equal, Text.hash);

    stable var overwrite_stores: Deque.Deque<Nat64> = Deque.empty<Nat64>();
    
    public query({caller}) func get(key : Text) : async Result.Result<Blob, ()> {
        _get_chunk(key, 0)
    };

    public query({caller}) func get_chunk(key : Text, chunkId: Nat8) : async Result.Result<Blob, ()> {
        _get_chunk(key, chunkId)
    };

    public shared({caller}) func store(args : StoreArgs) : async (){
        assert(caller == Principal.fromActor(ISP));

        switch (stream_assets.get(args.key)){
            case (null){
                let (stream_start, _) = _getField(MAX_BYTES_PER_CALL * CHUNKS_PER_STORE);
                _storageData(stream_start, args.value);

                stream_assets.put(args.key, [(0, stream_start, args.value.size())]);
            };
            case (?chunks){
                //  start of first chunk 
                let (_, stream_start, _) = chunks[0];

                let new_chunk: Chunk = if (chunks.size() < CHUNKS_PER_STORE){
                    let chunkId = chunks.size();

                    let chunk_start = Nat64.toNat(stream_start) +  (chunkId * MAX_BYTES_PER_CALL);

                    (
                        Nat8.fromNat(chunkId), 
                        Nat64.fromNat(chunk_start), 
                        args.value.size()
                    )
                }else{

                    let (frontChunkId, chunkPtr, _) = chunks[0];

                    let chunkId = frontChunkId + Nat8.fromNat(chunks.size());

                    (chunkId, chunkPtr, args.value.size())
                    
                };

                _storageData(new_chunk.1, args.value);

                let new_chunks = Utils.fixedLengthAdd(chunks, 5,  new_chunk);

                var index: Nat8 = 0;
                let ordered_chunks = Array.map<Chunk, Chunk>(new_chunks, func(chunk){
                    let result = (index, chunk.1, chunk.2);
                    index+=1;
                    result
                });

                stream_assets.put(args.key, ordered_chunks);
            };
        };
    };

    public shared ({caller}) func delete(key: Text) : async (){
        assert(caller == Principal.fromActor(ISP));
        switch (stream_assets.remove(key)){
            
            case (?chunks){
                let (_, stream_start, _) = Moh.Array.min<Chunk>(chunks, func(a,b){
                    let (_, a_start, _) = a;
                    let (_, b_start, _) = b;
                    Nat64.compare(a_start, b_start)
                });

                overwrite_stores := Deque.pushBack(overwrite_stores, stream_start);
            };
            case (_){};
        };
    };

    func _contains_chunk(key : Text, chunkId: Nat8) : Bool{
        switch (stream_assets.get(key)){
            case (null){
                false
            };
            case (?chunks){
                let (frontChunkId, _, _) = chunks[0];

                let index = chunkId - frontChunkId;
                if (index < Nat8.fromNat(chunks.size())){
                    let (realChunkId, _, _) = chunks[Nat8.toNat(index)];
                    realChunkId == chunkId
                }else{
                    false
                }
            };
        };
    };
    
    func _get_chunk(key : Text, _chunkId: Nat8) : Result.Result<Blob, ()> {
        switch(stream_assets.get(key)) {
            case(null) { 
                #err(())
            };
            case(?chunks) {
                Debug.print(debug_show chunks);
                let chunkId = Nat8.toNat(_chunkId);
                if (chunkId < chunks.size()){
                    let (_, start, len) = chunks[chunkId];

                    #ok(_loadFromSM(start, len))
                }else{
                    #err(())
                }
            };
        }
    };

    // call back to isp canister
    public shared({caller}) func monitor() : async (){
        if (caller != Principal.fromActor(ISP)) return;
        if (Cycles.balance() < CYCLE_THRESHOLD) {
            let need : Nat = CYCLE_THRESHOLD - Cycles.balance() + 1_000_000_000_000; // threshold + 1 T
            ignore await ISP.topUpBucket(need);
        };
    };

    private func _loadFromSM(_offset : Nat64, length : Nat) : Blob {
        SM.loadBlob(_offset, length)
    };

    private func _storageData(_offset : Nat64, data : Blob) {
        SM.storeBlob(_offset, data)
    };

    private func _getField(total_size : Nat) : (Nat64, Nat) {
        switch(Deque.popFront(overwrite_stores)){
            case (?(empty_store, deque)){
                overwrite_stores := deque;
                return (empty_store, total_size);
            };
            case(_){
                let field = (Nat64.fromNat(offset), total_size);
                _growStableMemoryPage(total_size);
                offset += total_size;
                field
            };
        }
    };

    private func _growStableMemoryPage(size : Nat) {
        if(offset == 0){ ignore SM.grow(1 : Nat64) };

        let available : Nat = Nat64.toNat(SM.size() << 16 + 1 - Nat64.fromNat(offset));
        if (available < size) {
            let new : Nat64 = Nat64.fromNat(size - available);
            let growPage = new >> 16 + 1;
            ignore SM.grow(growPage);
        }
    };

    public shared func wallet_receive() : async Nat{
        Cycles.accept(Cycles.available())
    };

    public func id() : async Principal {
        return _id();
    };

    func _id() : Principal {
        return Principal.fromActor(this);
    };

    public type HttpResponse = {
        body : Blob;
        headers : [HttpParser.HeaderField];
        status_code : Nat16;
        streaming_strategy : ?StreamingStrategy;
        update : ?Bool;
    };

    public type StreamingStrategy = {
        #Callback : {
            token : StreamingCallbackToken;
            callback : shared () -> async ();
        };
    };

    public type StreamingCallbackToken = {
        key: Text;
        sha256 : ?Blob;
        index : Nat;
        content_encoding: Text;
    };

    public type StreamingCallbackHttpResponse = {
        body : Blob;
        token: ?StreamingCallbackToken;
    };

    func createToken(key: Text, chunkId: Nat): ?StreamingCallbackToken {

        switch (_contains_chunk(key, Nat8.fromNat(chunkId))) {
            case (true) {
                ?{
                    key = key;
                    content_encoding = "";
                    index = chunkId;     
                    sha256 = null;       
                };
            };
            case (_) {
                null
            };
        };
    };

    public shared query func http_request_streaming_callback(streamingToken: StreamingCallbackToken) : async StreamingCallbackHttpResponse {
        let {key; index} = streamingToken;

        switch (_get_chunk(key, Nat8.fromNat(index))) {
            case (#ok (data)) {
                Debug.print("Found "# key # debug_show index);

                return {
                    token = createToken(key, index + 1);
                    body = data
                };
            };
            case (_) {
                Debug.print("error");
                throw Error.reject("Streamed asset not found: ");
            };
        };
    };

    func createStrategy(key: Text, chunkId: Nat) : ?StreamingStrategy {
        let streamingToken: ?StreamingCallbackToken = createToken(key, chunkId);

        switch (streamingToken) {
            case (null) {
                Debug.print("Couldn't create streaming strategy");
                 null 
            };
            case (?streamingToken) {
                let self: Principal = Principal.fromActor(this);
                let canisterId: Text = Principal.toText(self);

                let canister = actor (canisterId) : actor { http_request_streaming_callback : shared () -> async () };

                return ?#Callback({
                    token = streamingToken;
                    callback = canister.http_request_streaming_callback;
                });
            };
        };
    };

    public query func http_request(rawReq: HttpParser.HttpRequest): async HttpParser.HttpResponse {

        let res = HttpResponse.Builder();

        if (rawReq.method != "GET"){
            return res
            .update(true)
            .unwrap();
        };

        let req = HttpParser.parse(rawReq);

        let {url} = req;
        let {path} = url;

        let bad_request = res
        .status_code(400)
        .bodyFromText("Invalid request")
        .unwrap();

        switch(path.original){
            case("/storage"){
                let key  = Option.get(url.queryObj.get("key"), "");

                switch(_get_chunk(key, 0)){
                    case(#ok(data)){
                        res
                        .body(data)
                        .streaming_strategy(
                            createStrategy(key, 1)
                        )
                        .unwrap()
                    };
                    case(_){
                        Debug.print("Failed to get " # key); 
                        bad_request
                    };
                }
            };
            case("/redirect-to-google"){
                res
                .status_code(302)
                .header("Location", "https://www.google.com")
                .unwrap()
            };
            case(_){
                res
                .status_code(404)
                .bodyFromText("Page not found")
                .unwrap()
            }
        }
    };

    system func preupgrade() {
        stream_entries := Iter.toArray<(Text, Stream)>(stream_assets.entries());
    };

    system func postupgrade() {
        stream_entries := [];
    };

};