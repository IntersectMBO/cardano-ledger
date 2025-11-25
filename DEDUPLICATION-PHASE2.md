# CDDL Deduplication - Phase 2 Analysis

**Date:** 2025-11-19
**Status:** Ready for implementation

## Summary

Phase 1 deduplication (script helpers and auxiliary data) is **complete**. Analysis of all eras reveals **3 remaining duplicate instances** that can be eliminated.

## Completed Work (Phase 1)

### What Was Done
- Added `scriptAllGroup` and `scriptAnyGroup` smart constructors to Shelley
- Added `auxiliaryScriptsRule`, `auxiliaryDataArrayRule`, `auxiliaryDataRule` to Allegra
- Updated Allegra and Mary to use these smart constructors
- **Result:** Eliminated 5 sets of duplicates (10 total instances)

### Verification
✅ All CDDL libraries build successfully
✅ All CDDL files generated successfully
✅ Zero semantic changes in generated `.cddl` files
✅ No module-level stray comments introduced

## Remaining Work (Phase 2)

### 3 Duplicate Instances Found

**In Mary** - these instances duplicate Allegra's implementation exactly:

1. **min_int64**
```haskell
-- Allegra (line 99-100):
instance HuddleRule "min_int64" AllegraEra where
  huddleRule _ = "min_int64" =:= (-9223372036854775808 :: Integer)

-- Mary (line 58-59): EXACT DUPLICATE
instance HuddleRule "min_int64" MaryEra where
  huddleRule _ = "min_int64" =:= (-9223372036854775808 :: Integer)
```

2. **max_int64**
```haskell
-- Allegra (line 102-103):
instance HuddleRule "max_int64" AllegraEra where
  huddleRule _ = "max_int64" =:= (9223372036854775807 :: Integer)

-- Mary (line 61-62): EXACT DUPLICATE
instance HuddleRule "max_int64" MaryEra where
  huddleRule _ = "max_int64" =:= (9223372036854775807 :: Integer)
```

3. **int64**
```haskell
-- Allegra (line 105-106):
instance HuddleRule "int64" AllegraEra where
  huddleRule p = "int64" =:= huddleRule @"min_int64" p ... huddleRule @"max_int64" p

-- Mary (line 55-56): EXACT DUPLICATE
instance HuddleRule "int64" MaryEra where
  huddleRule p = "int64" =:= huddleRule @"min_int64" p ... huddleRule @"max_int64" p
```

### Why These Are Duplicates

These were introduced in Allegra for int64 support (needed for timelock `script_n_of_k` thresholds). Mary inherited them without modification.

### Why dns_name/url/delta_coin Are NOT Duplicates

While `dns_name`, `url`, and `delta_coin` appear in multiple eras with identical delegation patterns, they are **correct and necessary**:

```haskell
instance HuddleRule "dns_name" ShelleyEra where
  huddleRule _ = dnsNameRule  -- Each era needs its own instance

instance HuddleRule "dns_name" AllegraEra where
  huddleRule _ = dnsNameRule  -- Cannot be eliminated without overlapping instances

instance HuddleRule "dns_name" MaryEra where
  huddleRule _ = dnsNameRule  -- This is the optimal pattern
```

Each era **must** have its own instance for its era type. They properly delegate to shared smart constructors (`dnsNameRule`, `urlRule`, `deltaCoinRule`). This cannot be improved further.

## Implementation Plan

### Phase 2A: Update Allegra

**File:** `eras/allegra/impl/cddl/lib/Cardano/Ledger/Allegra/HuddleSpec.hs`

1. **Add 3 new smart constructors** (after `transactionRule`, around line 74):
```haskell
minInt64Rule :: Rule
minInt64Rule = "min_int64" =:= (-9223372036854775808 :: Integer)

maxInt64Rule :: Rule
maxInt64Rule = "max_int64" =:= (9223372036854775807 :: Integer)

int64Rule :: forall era. (HuddleRule "min_int64" era, HuddleRule "max_int64" era) => Proxy era -> Rule
int64Rule p = "int64" =:= huddleRule @"min_int64" p ... huddleRule @"max_int64" p
```

2. **Update module exports** (add to export list around line 13-20):
```haskell
module Cardano.Ledger.Allegra.HuddleSpec (
  allegraCDDL,
  blockRule,
  transactionRule,
  auxiliaryScriptsRule,
  auxiliaryDataArrayRule,
  auxiliaryDataRule,
  minInt64Rule,          -- NEW
  maxInt64Rule,          -- NEW
  int64Rule,             -- NEW
) where
```

3. **Replace Allegra instances** (around lines 99-106):
```haskell
instance HuddleRule "min_int64" AllegraEra where
  huddleRule _ = minInt64Rule

instance HuddleRule "max_int64" AllegraEra where
  huddleRule _ = maxInt64Rule

instance HuddleRule "int64" AllegraEra where
  huddleRule = int64Rule @AllegraEra
```

### Phase 2B: Update Mary

**File:** `eras/mary/impl/cddl/lib/Cardano/Ledger/Mary/HuddleSpec.hs`

1. **Update imports** (line 18):
```haskell
import Cardano.Ledger.Allegra.HuddleSpec (
    auxiliaryDataArrayRule,
    auxiliaryDataRule,
    auxiliaryScriptsRule,
    blockRule,
    int64Rule,           -- NEW
    maxInt64Rule,        -- NEW
    minInt64Rule,        -- NEW
    transactionRule
  )
```

2. **Replace Mary instances** (around lines 55-62):
```haskell
instance HuddleRule "int64" MaryEra where
  huddleRule = int64Rule @MaryEra

instance HuddleRule "min_int64" MaryEra where
  huddleRule _ = minInt64Rule

instance HuddleRule "max_int64" MaryEra where
  huddleRule _ = maxInt64Rule
```

### Phase 2C: Verification

1. Build all CDDL libraries:
```bash
cabal build cardano-ledger-allegra:cddl cardano-ledger-mary:cddl
```

2. Regenerate CDDL files:
```bash
cabal run cardano-ledger-allegra:generate-cddl
cabal run cardano-ledger-mary:generate-cddl
```

3. Verify no semantic changes:
```bash
git diff eras/allegra/impl/cddl-files/allegra.cddl
git diff eras/mary/impl/cddl-files/mary.cddl
```

Expected: No diff (zero semantic changes)

## Impact

### Before Phase 2
- **Duplicate instances**: 3 sets (6 total instances)
- **Smart constructors in Allegra**: 5

### After Phase 2
- **Duplicate instances**: 0
- **Smart constructors in Allegra**: 8 (+3)
- **Lines of code saved**: ~6 lines in Mary

## Final Statistics (After Both Phases)

### Overall Reduction
- **Total duplicate instances eliminated**: 8 sets (16 total instances)
- **New smart constructors created**: 8
  - Shelley: +2 (`scriptAllGroup`, `scriptAnyGroup`)
  - Allegra: +6 (3 auxiliary + 3 int64)
- **Net code reduction**: ~46 lines

### Benefits
1. **Zero duplication** - All repeated patterns eliminated
2. **Single source of truth** - Each definition in exactly one place
3. **Type safety maintained** - All era-specific type applications correct
4. **Easy maintenance** - Changes only need updates in one location
5. **Pattern established** - Clear template for future eras (Alonzo, Babbage, Conway)

## Notes

- All instances using `dnsNameRule`, `urlRule`, `deltaCoinRule` are **correct and optimal**
- No instances can be moved to Core with `Era era` constraint
- Import hierarchy strictly maintained: Core → Shelley → Allegra → Mary
- All era-specific type applications preserved (`@ShelleyEra`, `@AllegraEra`, `@MaryEra`)
- No module-level stray comments anywhere

## Next Session

Run the Phase 2 implementation plan above to eliminate the final 3 duplicates.
