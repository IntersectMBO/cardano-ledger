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
assembly, and a shape-only four-target Receiving redeemer map. The ordered
`receiving_pointers` records contain `output_index`, `script_hash` and
`pointer: [7, output_index]`. The codec fixture has a key output at raw index 1,
script outputs at 0/2/3/4, byte-identical outputs 0/4 and distinct execution budgets
for every index (including equal redeemers at 3/4). Hashes are neither sorted nor
deduplicated. The encoder also
verifies Ed25519 signatures and invalidation after toggling a protection bit.
It does not import or execute ledger code.

The additional `v4_data_vectors` record detailed Data ASTs for
`Receiving ScriptHash Integer` and `ReceivingScript Integer TxOut` under the
reviewed V4 schema. Both use constructor 7 with two fields; hash bytes precede
the index in the purpose, while the index precedes the specific TxOut in script
information. The complete output AST includes protected address, Ada value,
no datum and no reference script. These are Data shape vectors, not claimed
Data CBOR encodings. `TransactionInteropSpec` compares the ASTs with genuine V4
`ToData` and `FromData`, alongside complete transaction bytes, hashes and raw
Receiving pointers. The revised-model suite passes four examples with no
failures. Python reproduction alone does not establish that typed comparison.

This is a bounded reference encoder, not production wallet support. These
small fixtures do not claim valid minimum fee/value limits or ledger admission.
The redeemer-map fixture deliberately lacks executable scripts, the integrity
hash and key witness; it is only a codec and pointer-ordering fixture. There is
no hardware-wallet, network submission or chain-lifecycle claim here.
