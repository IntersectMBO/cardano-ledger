---
slug: 4
title: |
  4. Untranslatable data in script context
authors: [Jared Corduan]
tags: [Accepted]
---

## Status

Accepted

## Context

Sometimes there is data in a transaction which a Plutus script author might
expect to be present in the script context, but which we do not want to translate. 
One example is the legacy Byron addresses.
Another example is the features which were added to the Babbage ledger era
(such as inline datums) which are not a part of the Plutus V1 script context since they did not
exist when Plutus V1 was released.


There are basically two options for dealing with such information:
1. Silently ignore parts of the relevant script context during the translation.
2. Force a phase 1 validation error.

## Decision

We chose option 2, since option 1 could lead to surprising and unfortunate mistakes by
script authors.

## Consequences

Scripts will fail phase 1 validation if used in a transaction that contains data not available
for the corresponding script context.
For example, one cannot spend a Plutus V1 output and pay to a Plutus V2 output with
inline datum at the same time.

See this test suite for the list of untranslatable data at the time of the creation of this ADR:

https://github.com/intersectmbo/cardano-ledger/blob/064acb8e7ab7b2aeef138ec0c821f8c5f1402621/eras/babbage/test-suite/test/Test/Cardano/Ledger/Babbage/TxInfo.hs#L190-L193
