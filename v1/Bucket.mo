import Array "mo:base/Array";
import Blob "mo:base/Blob";
import Buffer "mo:base/Buffer";
import Cycles "mo:base/ExperimentalCycles";
import Debug "mo:base/Debug";
import Error "mo:base/Error";
import Iter "mo:base/Iter";
import Nat16 "mo:base/Nat16";
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
import Op "mo:op";
import Utils "Lib/Utils";

shared(installer) actor class Bucket() = this{
    private type StoreArgs              = Types.StoreArgs;
    private type IspInterface           = Types.IspInterface;
    private let CYCLE_THRESHOLD         = 3_000_000_000_000;
    private let ISP : IspInterface      = actor (Principal.toText(installer.caller));
    private stable var offset           = 0;
    private stable var assets_entries : [var (Text, (Nat64, Nat))] = [var];
    private var assets : TrieMap.TrieMap<Text, (Nat64, Nat)> = TrieMap.fromEntries<Text, (Nat64, Nat)>(assets_entries.vals(), Text.equal, Text.hash);
    stable let CHUNKS_PER_STORE = 5;
    stable let MAX_BYTES_PER_CALL = 2_097_152;

    //           (chunkId, start, end)
    type Chunk = (Nat16, Nat64, Nat);

    //                  (start, chunks)
    type StreamStore = (Nat64, [Chunk]);
    private stable var stream_entries : [var (Text, StreamStore)] = [var];

    private var stream_assets : TrieMap.TrieMap<Text, StreamStore> = TrieMap.fromEntries<Text, StreamStore>(stream_entries.vals(), Text.equal, Text.hash);
    stable let overwrite_stores: [StreamStore] = [(0, [])];
    
    public query({caller}) func get(key : Text) : async Result.Result<Blob, ()> {
        switch(assets.get(key)) {
            case(null) { 
                #err(())
            };
            case(?field) {
                #ok(_loadFromSM(field.0, field.1))
            };
        }
    };

    public query({caller}) func get_chunk(key : Text, chunkId: Nat16) : async Result.Result<Blob, ()> {
        switch(stream_assets.get(key)) {
            case(null) { 
                #err(())
            };
            case(?(stream_start, chunks)) {
                label l for ((id, start, end) in chunks.vals()){
                    if (chunkId == id){
                        return #ok(_loadFromSM(start, end));
                    }else if (chunkId > id){
                        break l;
                    }
                };
                #err(())
            };
        }
    };

    public shared({caller}) func store(args : StoreArgs) : async (){
        assert(caller == Principal.fromActor(ISP));
        
        switch (stream_assets.get(args.key)){
            case (null){
                let (stream_start, _) = _getField(MAX_BYTES_PER_CALL * CHUNKS_PER_STORE);
                stream_assets.put(args.key, (stream_start, [(0, stream_start, args.value.size())]));
                _storageData(stream_start, args.value);
            };
            case (?(stream_start, chunks)){

                let chunkId: Nat = if (chunks.size() < CHUNKS_PER_STORE){
                    chunks.size()
                }else{
                    let (last_chunkId, _, _) = chunks[chunks.size() - 1];
                    Nat16.toNat(last_chunkId)
                };

                let chunk_start = Nat64.toNat(stream_start) +  (chunkId * MAX_BYTES_PER_CALL);
                let chunk_end = args.value.size() + chunk_start;

                let new_chunk = 
                    (
                        Nat16.fromNat(chunkId), 
                        Nat64.fromNat(chunk_start), 
                        chunk_end
                    );

                _storageData(Nat64.fromNat(chunk_start), args.value);

                let new_chunks = Utils.fixedLengthAdd(chunks, 5,  new_chunk);
                stream_assets.put(args.key, (stream_start, new_chunks));
            };
        };
    };

    // public shared ({caller}) func delete(key: Text) : async (){
    //     assert(caller == Principal.fromActor(ISP));
    //     switch (assets.get(key)){
            
    //         case (?location){
    //             overwrite_stores.push(location);
    //             assets.remove(key);
    //         };
    //         case (_){};
    //     };
    // };

    func store_internal(args : StoreArgs) : (){
        let _field = _getField(args.value.size());
        assets.put(args.key, _field);
        _storageData(_field.0, args.value);
    };

    func get_internal(key : Text) : Result.Result<Blob, ()> {
        switch(assets.get(key)) {
            case(null) { 
                #err(())
            };
            case(?field) {
                #ok(_loadFromSM(field.0, field.1))
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
        let field = (Nat64.fromNat(offset), total_size);
        _growStableMemoryPage(total_size);
        offset += total_size;
        field
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

    func createId(key: Text, chunkId: Nat): Text{
        F.format("{}-{}", [#text(key), #num(chunkId)])
    };


    func parseId(id: Text):?{key: Text; chunkId: Nat}{
        let parts = Iter.toArray(Text.split(id, #char '-'));
        
        if (parts.size() != 2){
            return null;
        };

        switch (Op.parseNat(parts[1], 10)){
            case (#ok(num)){
                ?{
                    key = parts[0];
                    chunkId = num;
                }
            };
            case (_){
                null
            };
        };
    };

    func createToken(key: Text, chunkId: Nat): ?StreamingCallbackToken {
        let id = createId(key, chunkId);

        switch (get_internal(id)) {
            case (#ok(_)) {
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
        let id = createId(key, index);
        Debug.print("key: " # id);

        switch (get_internal(id)) {
            case (#ok (data)) {
                Debug.print("Found "# id);

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
        let streamingToken: ?StreamingCallbackToken = createToken(key, 0,);

        switch (streamingToken) {
            case (null) {
                Debug.print("Couldn't create streaming strategy");
                 null 
            };
            case (?streamingToken) {
                // Hack: https://forum.dfinity.org/t/cryptic-error-from-icx-proxy/6944/8
                // Issue: https://github.com/dfinity/candid/issues/273

                let self: Principal = Principal.fromActor(this);
                let canisterId: Text = Principal.toText(self);

                let canister = actor (canisterId) : actor { http_request_streaming_callback : shared () -> async () };

                Debug.print("Creating streaming strategy");
                Debug.print(debug_show streamingToken);

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
                // let chunkId  = Option.get(url.queryObj.get("chunkId"), "");
                let id = createId(key, 0);

                Debug.print("Trying to get " # key); 
                switch(get_internal(id)){
                    case(#ok(data)){
                        Debug.print( key # " Exists"); 
                        res
                        // .header("Access-Control-Allow-Origin", "*")
                        // .header("Content-Type", "video/mp4")
                        // .header("accept-ranges", "bytes")
                        // .header("cache-control", "private, max-age=0")
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
            case("/storage/chunk"){
                let key  = Option.get(url.queryObj.get("key"), "");
                // let chunkId  = Option.get(url.queryObj.get("chunkId"), "");

                Debug.print("Trying to get " # key); 
                switch(get_internal(key)){
                    case(#ok(data)){
                        Debug.print( key # " Exists"); 
                        res
                        .header("Access-Control-Allow-Origin", "*")
                        .header("Content-Type", "image/jpeg")
                        .body(data)
                        .unwrap()
                    };
                    case(_){
                        Debug.print("Failed to get " # key); 
                        bad_request
                    };
                }
            };
            case("/storage/keys"){
                let keys = Iter.toArray(assets.keys());

                res
                .header("Content-Type", "application/json")
                .bodyFromText(F.format("{}", [#textArray(keys)]))
                .unwrap();
 
            };
            case("/"){
                res
                .header("Content-Type", "text/html")
                .bodyFromText("<html><body><input type=\"file\" id=\"upload\"></body><html>")
                .unwrap();
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
        assets_entries := Array.init<(Text, (Nat64, Nat))>(assets.size(), ("", (0, 0)));
        var assets_index = 0;
        for (a in assets.entries()) {
            assets_entries[assets_index] := a;
            assets_index += 1;
        };
    };

    system func postupgrade() {
        assets_entries := [var];
    };

};