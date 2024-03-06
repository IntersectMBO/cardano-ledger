# Version history for `small-steps`

## 1.1.0.0

* Change `applySTSOptsEither`, `Extended.applySTS`, `Simple.applySTS`
  to use `NonEmpty (PredicateFailure _)` instead of `[PredicateFailure _]`

### `testlib`

* Moved `small-steps-test` library here (`small-steps:testlib`) as sublibrary

## 1.0.1.0

* Add `failOnJust`, `failOnNonEmpty`, `failureOnJust`, `failureOnNonEmpty`

## 1.0.0.0

* First properly versioned release
