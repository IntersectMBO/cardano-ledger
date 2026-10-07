#!/usr/bin/env python3
"""Independent CBOR/Ed25519 serializer for bounded CIP-160 transaction vectors.

Uses cbor2 and cryptography, and does not import or execute Cardano ledger code.
The vectors verify wire format, body hashes, signatures and Receiving ordering;
codec vectors alone do not establish ledger admission or execution success.
"""
import hashlib
import importlib.metadata
import json
from pathlib import Path

import cbor2
from cryptography.exceptions import InvalidSignature
from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey

HERE = Path(__file__).resolve().parent


def encode(value):
    return cbor2.dumps(value, canonical=True)


def digest(value, size=32):
    return hashlib.blake2b(value, digest_size=size).digest()


def receiving_pointers(outputs):
    # Independent header/family inspection and sorting; no ledger implementation.
    hashes = sorted({address[1:29] for address, _ in outputs
                     if address[0] & 8 and address[0] & 16})
    return {hash_.hex(): [7, index] for index, hash_ in enumerate(hashes)}


def vector(name, body, witnesses):
    body_bytes = encode(body)
    tx_bytes = encode([body, witnesses, None])
    assert encode(cbor2.loads(tx_bytes)) == tx_bytes
    return {"name": name, "body_hex": body_bytes.hex(),
            "transaction_hex": tx_bytes.hex(), "txid": digest(body_bytes).hex(),
            "receiving_pointers": receiving_pointers(body[1])}


def main():
    secret = Ed25519PrivateKey.from_private_bytes(bytes(range(32)))
    public = secret.public_key().public_bytes(serialization.Encoding.Raw,
                                            serialization.PublicFormat.Raw)
    recipient = Ed25519PrivateKey.from_private_bytes(bytes(range(32, 64)))
    recipient_public = recipient.public_key().public_bytes(serialization.Encoding.Raw,
                                                         serialization.PublicFormat.Raw)
    key_hash = digest(recipient_public, 28)
    native = [1, []]  # RequireAllOf []
    native_hash = digest(b"\x00" + encode(native), 28)
    inputs = cbor2.CBORTag(258, [[bytes([0x11]) * 32, 0]])
    native_outputs = [[bytes([0x78]) + native_hash, 5],
                      [bytes([0x68]) + key_hash, 7],
                      [bytes([0x78]) + native_hash, 8]]
    native_body = {0: inputs, 1: native_outputs, 2: 10}
    txid = digest(encode(native_body))
    signature = secret.sign(txid)
    secret.public_key().verify(signature, txid)
    recipient_signature = recipient.sign(txid)
    recipient.public_key().verify(recipient_signature, txid)
    key_witnesses = sorted([[public, signature], [recipient_public, recipient_signature]],
                          key=lambda witness: digest(witness[0], 28))
    native_witnesses = {0: cbor2.CBORTag(258, key_witnesses),
                        1: cbor2.CBORTag(258, [native])}
    native_vector = vector("signed-native-and-key-receiving", native_body,
                           native_witnesses)
    native_vector.update(public_key_hex=public.hex(), signature_hex=signature.hex(),
                         payment_key_hash=key_hash.hex(), native_script_hash=native_hash.hex(),
                         recipient_public_key_hex=recipient_public.hex(),
                         recipient_signature_hex=recipient_signature.hex())

    # Shape-only Plutus witness vector: two Receiving targets, a duplicate, and
    # a key output. Deliberately lacks executable scripts/integrity hash; it must
    # never be presented as a phase-1-valid or successfully evaluated transaction.
    outputs = [[bytes([0x78]) + bytes([0xFF]) * 28, 5],
               [bytes([0x68]) + key_hash, 7],
               [bytes([0x78]) + bytes(28), 8],
               [bytes([0x78]) + bytes([0xFF]) * 28, 9]]
    body = {0: inputs, 1: outputs, 2: 10}
    witnesses = {5: {(7, 0): [1, [100, 200]], (7, 1): [2, [300, 400]]}}
    plutus_vector = vector("receiving-redeemer-map-codec-only", body, witnesses)
    plutus_vector["admission"] = "not phase-1-valid: missing scripts/integrity hash/key witness"
    assert len(plutus_vector["receiving_pointers"]) == 2
    assert plutus_vector["receiving_pointers"][bytes(28).hex()] == [7, 0]

    # Independent sender and recipient signatures are assembled without changing
    # the original body; the recipient output key differs from the spending key.
    partial = vector("native-body-with-spending-signature", native_body,
                     {0: cbor2.CBORTag(258, [[public, signature]]),
                      1: cbor2.CBORTag(258, [native])})
    assert partial["body_hex"] == native_vector["body_hex"]
    assert partial["txid"] == native_vector["txid"]
    assert digest(public, 28) != key_hash

    # Witness assembly must preserve the exact original body and body hash.
    unsigned = vector("native-body-before-witness-assembly", native_body, {})
    assert unsigned["body_hex"] == native_vector["body_hex"]
    assert unsigned["txid"] == native_vector["txid"]
    tampered = dict(native_body)
    tampered[1] = [[bytes([native_outputs[0][0][0] ^ 8]) + native_outputs[0][0][1:], 5],
                   *native_outputs[1:]]
    assert digest(encode(tampered)) != txid
    try:
        secret.public_key().verify(signature, digest(encode(tampered)))
    except InvalidSignature:
        pass
    else:
        raise AssertionError("Protection-bit tamper preserved the signature")

    report = {"serializer": "cbor2", "serializer_version": importlib.metadata.version("cbor2"),
              "signature_library": "cryptography",
              "signature_library_version": importlib.metadata.version("cryptography"),
              "scope": "bounded independent full-transaction reference encoder: codec, txid, signing and Receiving pointers; no ledger-admission claim",
              "vectors": [native_vector, plutus_vector, unsigned, partial]}
    (HERE.parent / "receiving-interop.json").write_text(json.dumps(report, indent=2) + "\n")
    print("PASS: 4 independent full-transaction vectors; canonical re-encoding, txids, distinct sender/recipient signatures, partial witness assembly, protection tamper, Receiving ordering")


if __name__ == "__main__":
    main()
