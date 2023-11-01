# Version history for `cardano-data`

## 1.1.2.0

- Add Data.OMap.Strict #3791

## 1.1.1.0

- Add Data.OSet.Strict #3779

## 1.1.0.0

- Remove `Data.UMap` #3371

## 1.0.1.0

* Fix - A bug was fixed in the `canonicalInsert` function.
  The bug manifested by creating an unbalanced tree in the `Data.Map` internals of the
  'CanonicalMap', which can result in a crash.
  This was the root cause of https://github.com/input-output-hk/cardano-node/issues/4826.

* Added New module Data.Universe, reusable code for defining closed singleton types.

## 1.0.0.0

* First properly versioned released.
