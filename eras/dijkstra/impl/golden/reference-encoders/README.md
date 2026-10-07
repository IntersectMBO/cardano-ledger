# Receiving transaction reference encoder

`receiving.py` uses cbor2 and cryptography independently of ledger code. Run:

```sh
python3 -m venv /tmp/receiving-reference
/tmp/receiving-reference/bin/pip install -r golden/reference-encoders/requirements.txt
/tmp/receiving-reference/bin/python golden/reference-encoders/receiving.py
```

Run from the Dijkstra package directory. The result is
`golden/receiving-interop.json`; `TransactionInteropSpec` constructs equivalent
typed ledger transactions and compares complete mempool bytes, body bytes,
body hashes and Receiving pointers against that file.

The four vectors cover native/key protected outputs with a duplicate native
hash, a distinct spending key and recipient key, unsigned/partial/final witness
assembly, and a shape-only two-target Receiving redeemer map. The encoder also
verifies Ed25519 signatures and invalidation after toggling a protection bit.
It does not import or execute ledger code.

This is a bounded reference encoder, not production wallet support. These
small fixtures do not claim valid minimum fee/value limits or ledger admission.
The redeemer-map fixture deliberately lacks executable scripts, the integrity
hash and key witness; it is only a codec and pointer-ordering fixture. There is
no hardware-wallet, network submission or chain-lifecycle claim here.
