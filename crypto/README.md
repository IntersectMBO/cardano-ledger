# cardano-crypto-wrapper

The cryptographic primitives used in Cardano

* Cryptographic hashing (again using the [cryptonite] library).

* A wrapper around the [scrypt] library used for password-based key derivation
  functions.

* A wrapper around [pvss-haskell] for Verifiable Secret Sharing (VSS), which
  will be removed soon.

* Secure generation of cryptographically random numbers and `ByteString`s.

* Hierarchical derivation functionality for Hierarchical Deterministic key
  creation (ie for the wallet).

* Cryptographic signing and signature checking.

* `Bi` (serialisation, see the `cardano-binary` package) instances for the
  cryptographic data types.

[cryptonite]: https://hackage.haskell.org/package/cryptonite
[pvss-haskell]: https://github.com/input-output-hk/pvss-haskell
[scrypt]: https://hackage.haskell.org/package/scrypt
