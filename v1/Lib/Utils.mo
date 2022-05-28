import Blob "mo:base/Blob";
import Principal "mo:base/Principal";
import Array "mo:base/Array";
import Nat8 "mo:base/Nat8";
import Nat "mo:base/Nat";

import Debug "mo:base/Debug";

module Utils{

    public type Token = { e8s : Nat64 };

    // Convert principal id to subaccount id.
    // sub account = [sun_account_id_size, principal_blob, 0,0,···]
    public func principalToSubAccount(id: Principal) : [Nat8] {
        let p = Blob.toArray(Principal.toBlob(id));
        Array.tabulate(32, func(i : Nat) : Nat8 {
            if (i >= p.size() + 1) 0
            else if (i == 0) (Nat8.fromNat(p.size()))
            else (p[i - 1])
        })
    };


    public func fixedLengthAdd<T>(arr: [T], size: Nat, value: T) : [T] {
        let newSize = Nat.min(arr.size() + 1, size);

        Array.tabulate<T>(newSize, func(i : Nat) : T {
            Debug.print("fla: " # debug_show (i, newSize));
            if (i + 1  < newSize) {
                arr[i + (if (arr.size() >= newSize){ 1} else {0})]
            }else{
                Debug.print("value: ");
                value
            }
        })
    };
};