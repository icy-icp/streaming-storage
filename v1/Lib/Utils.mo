import Blob "mo:base/Blob";
import Principal "mo:base/Principal";
import Array "mo:base/Array";
import Nat8 "mo:base/Nat8";

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

    func min(a:Nat, b: Nat): Nat{
        if (a < b) a
        else b
    };

    public func fixedLengthAdd<T>(arr: [T], size: Nat, value: T) : [T] {
        Array.tabulate<T>(min(arr.size() + 1, 5), func(i : Nat) : T {
            if (i < arr.size() - 1) {
                arr[i + 1]
            }else{
                value
            }
        })
    };
};