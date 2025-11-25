# CDDL Deduplication Implementation Notes

## Overview

This document captures the key considerations, patterns, and principles established while implementing type-class-based CDDL generation with smart constructors to eliminate code duplication across Cardano ledger eras.

## Core Architectural Principles

### 1. Polymorphic Smart Constructors Over Parameterized Helpers

**Pattern**: Use polymorphic functions with `forall era.` and type application instead of passing `Rule` parameters.

**Good**:
```haskell
mkProtocolParamUpdate :: forall era. HasCDDL "protocol_version" era => HuddleItem
mkProtocolParamUpdate = HIRule $ "protocol_param_update" =:= mp [...]

-- Usage:
instance HasCDDL "protocol_param_update" AllegraEra where
  huddleItem = mkProtocolParamUpdate @AllegraEra
```

**Bad**:
```haskell
mkProtocolParamUpdate :: Rule -> HuddleItem
mkProtocolParamUpdate protocolVersionRule = ...

-- Usage (don't do this):
instance HasCDDL "protocol_param_update" AllegraEra where
  huddleItem = mkProtocolParamUpdate (huddleRule @"protocol_version" @AllegraEra)
```

**Rationale**: Polymorphic approach is cleaner, more type-safe, and follows the era-based architecture pattern.

### 2. Use $ Instead of Parentheses Where Possible

**Good**:
```haskell
mkHeader :: forall era. HasCDDL "header_body" era => HuddleItem
mkHeader =
  HIRule $ "header" =:= arr [a $ huddleRule @"header_body" @era, "body_signature" ==> huddleRule @"kes_signature" @era]
```

**Bad**:
```haskell
mkHeader =
  HIRule ("header" =:= arr [...])
```

**Rationale**: Reduces visual noise and follows Haskell style conventions.

### 3. Era-Specific Instances with Originating Era Smart Constructors

**KEY PRINCIPLE**: All instances must be era-specific. All smart constructors must be fully polymorphic with a single `era` type parameter. Smart constructors live in the module of the **originating era** (the era that first introduces that concept).

**Structure**:
- **Core.CDDL**: Fundamental types (coin, slot, epoch, hash28, hash32, etc.)
- **Shelley.CDDL**: Shelley-introduced concepts (transaction_input, certificate, withdrawals, pool_params, etc.)
- **Allegra.CDDL**: Allegra-introduced concepts (script_invalid_before, script_invalid_hereafter, etc.)

**Pattern**: Each subsequent era:
1. **Imports** smart constructors from originating modules
2. Creates **era-specific instances** using those constructors

**Example** (smart constructor in Shelley, used by both Shelley and Allegra):
```haskell
-- In Shelley.CDDL (originating era):
mkTransactionInput :: forall era. HasCDDL "transaction_id" era => HuddleItem
mkTransactionInput =
  HIRule $
    "transaction_input"
      =:= arr
        [ "id" ==> huddleRule @"transaction_id" @era
        , "index" ==> VUInt `sized` (2 :: Word64)
        ]

instance HasCDDL "transaction_input" ShelleyEra where
  huddleItem = mkTransactionInput @ShelleyEra

-- In Allegra.CDDL (importing from Shelley):
instance HasCDDL "transaction_input" AllegraEra where
  huddleItem = mkTransactionInput @AllegraEra
```

**Benefits**:
- No cross-era type applications (`@ShelleyEra` never appears in Allegra module)
- Code reuse through polymorphic smart constructors
- Each module is self-contained with only its own era type parameter
- Clear ownership: smart constructor lives where concept originates
- Dependency flows correctly: Allegra imports from Shelley (which imports from Core), but never directly references ShelleyEra

### 4. Single Era Parameter in Smart Constructors (CRITICAL)

**Every smart constructor must use a single `era` type parameter.** All type applications within the body must use that same parameter.

**CORRECT**:
```haskell
mkHeaderBody ::
  forall era. (HasCDDL "operational_cert" era, HasCDDL "protocol_version" era) => HuddleItem
mkHeaderBody =
  HIRule $
    "header_body"
      =:= arr
        [ "block_number" ==> huddleRule @"block_number" @era
        , "slot" ==> huddleRule @"slot" @era
        , a $ huddleGroup @"operational_cert" @era      -- Same @era
        , a $ huddleGroup @"protocol_version" @era      -- Same @era
        ]

-- Shelley:
instance HasCDDL "header_body" ShelleyEra where
  huddleItem = mkHeaderBody @ShelleyEra

-- Allegra:
instance HasCDDL "header_body" AllegraEra where
  huddleItem = mkHeaderBody @AllegraEra
```

**INCORRECT** (multiple era parameters - OLD PATTERN, DO NOT USE):
```haskell
-- DON'T DO THIS:
mkHeaderBody :: forall era operationalCertEra.
  (HasCDDL "operational_cert" operationalCertEra, HasCDDL "protocol_version" era)
  => HuddleItem

-- DON'T DO THIS:
instance HasCDDL "header_body" AllegraEra where
  huddleItem = mkHeaderBody @AllegraEra @ShelleyEra  -- Cross-era reference!
```

**Rationale**: Single era parameter ensures type-level isolation between era modules. Each module is self-contained.

## Required Language Extensions

When creating polymorphic smart constructors, ensure these extensions are enabled:

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}  -- For forall era. without era value
{-# LANGUAGE FlexibleContexts #-}     -- For non-trivial constraints
{-# LANGUAGE TypeApplications #-}     -- For @EraName syntax
```

## Constraint Simplification

### Avoid Redundant Constraints

GHC with `-Werror=redundant-constraints` will catch these. Common cases:

**Example 1**: When a constraint is satisfied by instance lookup
```haskell
-- Bad:
mkProposedProtocolParameterUpdates :: forall era.
  (HasCDDL "genesis_hash" era, HasCDDL "protocol_param_update" era)
  => HuddleItem

-- Good (if genesis_hash has instance Era era => HasCDDL "genesis_hash" era):
mkProposedProtocolParameterUpdates :: forall era.
  HasCDDL "protocol_param_update" era
  => HuddleItem
```

**Example 2**: Era constraint when not needed
```haskell
-- Bad:
mkProtocolParamUpdate :: forall era.
  (Era era, HasCDDL "protocol_version" era)
  => HuddleItem

-- Good:
mkProtocolParamUpdate :: forall era.
  HasCDDL "protocol_version" era
  => HuddleItem
```

**Rationale**: Simpler constraints are easier to understand and maintain.

## Import and Export Organization

### Smart Constructor Module (Shelley)

**Exports**:
```haskell
module Cardano.Ledger.Shelley.CDDL (
  shelleyCDDL,
  mkHeader,
  mkHeaderBody,
  mkProposedProtocolParameterUpdates,
  mkUpdate,
  mkProtocolParamUpdate,
) where
```

Export all smart constructors that other eras might need.

### Importing Era (Allegra, Mary, etc.)

**Imports**:
```haskell
import Cardano.Ledger.Shelley.CDDL (
  mkHeader,
  mkHeaderBody,
  mkProposedProtocolParameterUpdates,
  mkProtocolParamUpdate,
  mkUpdate
  )
```

Import only the smart constructors you actually use. Remove unused imports to keep `-Werror` happy.

## Verification Process

After every refactoring, follow this process:

1. **Build both eras**:
   ```bash
   cabal build cardano-ledger-shelley:cddl cardano-ledger-allegra:cddl
   ```

2. **Generate CDDL files**:
   ```bash
   cabal run cardano-ledger-shelley:generate-cddl
   cabal run cardano-ledger-allegra:generate-cddl
   ```

3. **Verify output is identical**:
   ```bash
   git diff --exit-code eras/shelley/impl/cddl-files/shelley.cddl
   git diff --exit-code eras/allegra/impl/cddl-files/allegra.cddl
   ```

**Critical**: The refactoring should produce byte-for-byte identical CDDL output. Any difference indicates a semantic change, which means the refactoring is incorrect.

## Identifying Deduplication Opportunities

### When to Create a Smart Constructor

Create a smart constructor when:

1. **Substantial duplication** (typically 10+ lines)
2. **Identical or near-identical structure** across eras
3. **Only era-specific type parameters differ**

### Marginal Cases

Small definitions (2-3 lines) may not benefit from deduplication:

```haskell
-- Probably not worth a smart constructor:
instance HasCDDL "script_all" AllegraEra where
  huddleItem = HIGroup $ "script_all" =:~ grp [1, a $ arr [0 <+ a (huddleRule @"native_script" @AllegraEra)]]
```

Use judgment based on:
- How many eras will use it
- Likelihood of future changes
- Cognitive overhead of indirection

### Finding Candidates

Compare corresponding instance definitions between eras:

```bash
# Example: Compare all instances in Shelley vs Allegra
diff -u eras/shelley/impl/cddl/Cardano/Ledger/Shelley/CDDL.hs \
        eras/allegra/impl/cddl/Cardano/Ledger/Allegra/CDDL.hs
```

Look for:
- Long blocks that differ only in era type parameters
- Repeated patterns across multiple eras
- Evolution of definitions (what changes, what stays the same)

## Comment Standardization

### Inline Comments in Maps and Arrays

**Standard pattern**:
```haskell
protocol_param_update =
  HIRule $
    "protocol_param_update"
      =:= mp
        [ opt (idx 0 ==> VUInt) //- "minfee A"
        , opt (idx 1 ==> VUInt) //- "minfee B"
        , opt (idx 2 ==> VUInt) //- "max block body size"
        ]
```

**Key**: The `//- "comment"` operator goes **outside** the `opt (...)` expression.

**Bad**:
```haskell
[ opt (idx 0 ==> VUInt //- "minfee A")  -- Comment inside opt
```

### Block Comments

For larger explanatory comments, use `comment` with heredoc:

```haskell
HIRule $
  comment
    [str|Allegra-era native scripts add timelock support:
        |  - Signature verification (script_pubkey)
        |  - Conjunctions (script_all)
        |  - Disjunctions (script_any)
        |  - M-of-N thresholds (script_n_of_k)
        |  - Time validity start (script_invalid_before)
        |  - Time validity end (script_invalid_hereafter)
        |]
    $ "native_script"
      =:= arr [...]
```

## Topological Ordering

CDDL definitions should be organized in topological order (dependencies before dependents).

**Reference files**:
- `/tmp/shelley_topo_order.txt`
- `/tmp/allegra_topo_order.txt`

**Example ordering**:
1. Primitives (hash32, coin, slot)
2. Credentials and keys
3. Certificates
4. Transaction components (inputs, outputs)
5. Scripts
6. Witnesses
7. Transaction body and transaction
8. Block components
9. Block (top-level)

**Benefits**:
- Easier to read and understand
- Matches CDDL output structure
- Makes dependencies explicit

## Work Completed (as of 2025-11-14)

### Eras Completed
- ✅ Shelley (base era with smart constructors)
- ✅ Allegra (imports from Shelley, adds own smart constructors)
- ✅ Mary (imports from both Shelley and Allegra)

### Smart Constructors Created

**In Shelley** (exported for reuse, single `era` parameter):

**Core Infrastructure:**
1. **mkHeader** - Constraint: `HasCDDL "header_body" era`
2. **mkHeaderBody** - Constraints: `HasCDDL "operational_cert" era, HasCDDL "protocol_version" era`
3. **mkProtocolVersion** - Constraint: `HasCDDL "major_protocol_version" era`
4. **mkProtocolParamUpdate** - Constraint: `HasCDDL "protocol_version" era`
5. **mkProposedProtocolParameterUpdates** - Constraints: `HasCDDL "genesis_hash" era, HasCDDL "protocol_param_update" era`
6. **mkUpdate** - Constraint: `HasCDDL "proposed_protocol_parameter_updates" era`

**Transaction Components:**
7. **mkVkeywitness** - Constraint: `Era era`
8. **mkBootstrapWitness** - Constraint: `Era era`
9. **mkTransactionWitnessSet** - Constraints: `HasCDDL "vkeywitness" era, HasCDDL "native_script" era, HasCDDL "bootstrap_witness" era`
10. **mkTransactionId** - Constraint: `Era era`
11. **mkTransactionInput** - Constraint: `HasCDDL "transaction_id" era`
12. **mkTransactionOutput** - Constraint: `Era era`
13. **mkWithdrawals** - Constraint: `Era era`

**Certificate Chain:**
14. **mkAccountRegistrationCert** - Constraint: `Era era`
15. **mkAccountUnregistrationCert** - Constraint: `Era era`
16. **mkDelegationToStakePoolCert** - Constraint: `Era era`
17. **mkPoolRegistrationCert** - Constraint: `HasCDDL "pool_params" era`
18. **mkPoolRetirementCert** - Constraint: `Era era`
19. **mkGenesisDelegationCert** - Constraints: `HasCDDL "genesis_hash" era, HasCDDL "genesis_delegate_hash" era`
20. **mkMoveInstantaneousRewardsCert** - Constraint: `HasCDDL "move_instantaneous_reward" era`
21. **mkCertificate** - All certificate constraints

**Pool Infrastructure:**
22. **mkDnsName** - No constraints (pure definition)
23. **mkUrl** - No constraints (pure definition)
24. **mkPoolMetadata** - Constraint: `HasCDDL "url" era`
25. **mkSingleHostAddr** - Constraint: `Era era`
26. **mkSingleHostName** - Constraint: `HasCDDL "dns_name" era`
27. **mkMultiHostName** - Constraint: `HasCDDL "dns_name" era`
28. **mkRelay** - Constraints: All host type constraints
29. **mkPoolParams** - Constraints: `HasCDDL "relay" era, HasCDDL "pool_metadata" era`

**Other:**
30. **mkGenesisHash** - Constraint: `Era era`
31. **mkGenesisDelegateHash** - Constraint: `Era era`
32. **mkOperationalCert** - Constraint: `Era era`
33. **mkDeltaCoin** - No constraints (pure definition)
34. **mkMoveInstantaneousReward** - Constraint: `HasCDDL "delta_coin" era`
35. **mkScriptPubkey** - Constraint: `Era era`

**In Allegra** (exported for Mary and later eras):
36. **mkBlock** - Constraints: `HasCDDL "header" era, HasCDDL "transaction_body" era, HasCDDL "transaction_witness_set" era, HasCDDL "auxiliary_data" era`
37. **mkTransaction** - Constraints: `HasCDDL "transaction_body" era, HasCDDL "transaction_witness_set" era, HasCDDL "auxiliary_data" era`

### Era-Specific Instances (No Polymorphic Instances)

All instances are now era-specific. For example:
- `instance HasCDDL "genesis_hash" ShelleyEra` → uses `mkGenesisHash @ShelleyEra`
- `instance HasCDDL "genesis_hash" AllegraEra` → uses `mkGenesisHash @AllegraEra`

**No more polymorphic instances like** `Era era => HasCDDL "genesis_hash" era`. Each era explicitly declares its instances.

### Style and Comment Standardization

- Converted all `HIRule (...)` to `HIRule $ ...` and `HIGroup (...)` to `HIGroup $ ...`
- Converted multiline comments to quasiquote `[str|...|]` format (Shelley, Allegra)
- Standardized certificate naming:
  - `stake_registration_cert_deprecated` → `account_registration_cert`
  - `stake_deregistration_cert_deprecated` → `account_unregistration_cert`
  - `stake_delegation_cert` → `delegation_to_stake_pool_cert`
- Added "Pool parameters for stake pool registration" comment to pool_params
- Removed forward-looking era references from comments

### Total Deduplication Achieved

Across Shelley and Allegra (as of 2025-11-17):
- **~300+ lines of duplicate code eliminated**
- **37 smart constructors** covering major structural components
- **Complete type-level isolation**: Each era module uses only its own era type parameter
- **No cross-era references**: `@ShelleyEra` never appears in Allegra module
- **Maintainability improvement**: Changes to common structures now require updates in only one location

## Deduplication Analysis Process

### Systematic Comparison Approach

When analyzing a new era for deduplication opportunities:

1. **Read both files side-by-side**:
   ```bash
   # Open in split view or use diff
   diff -u eras/shelley/impl/cddl/Cardano/Ledger/Shelley/CDDL.hs \
           eras/allegra/impl/cddl/Cardano/Ledger/Allegra/CDDL.hs
   ```

2. **Compare instances one by one**:
   - Look for instances with the same name
   - Check if structure is identical except for era parameters
   - Note any semantic differences (field changes, new fields, etc.)

3. **Categorize each comparison**:
   - **Good candidates**: Substantial duplication (10+ lines), identical structure, only era params differ
   - **Non-candidates**: Structural differences, semantic evolution, marginal size (2-3 lines)

### Good Deduplication Candidates

Examples from our work:

**transaction_witness_set** (Shelley vs Allegra):
```haskell
-- Shelley:
instance HasCDDL "transaction_witness_set" ShelleyEra where
  huddleItem =
    HIRule $
      "transaction_witness_set"
        =:= mp
          [ opt $ idx 0 ==> arr [0 <+ a (huddleRule @"vkeywitness" @ShelleyEra)]
          , opt $ idx 1 ==> arr [0 <+ a (huddleRule @"native_script" @ShelleyEra)]
          , opt $ idx 2 ==> arr [0 <+ a (huddleRule @"bootstrap_witness" @ShelleyEra)]
          ]

-- Allegra:
instance HasCDDL "transaction_witness_set" AllegraEra where
  huddleItem =
    HIRule $
      "transaction_witness_set"
        =:= mp
          [ opt $ idx 0 ==> arr [0 <+ a (huddleRule @"vkeywitness" @ShelleyEra)]
          , opt $ idx 1 ==> arr [0 <+ a (huddleRule @"native_script" @AllegraEra)]  -- Only difference!
          , opt $ idx 2 ==> arr [0 <+ a (huddleRule @"bootstrap_witness" @ShelleyEra)]
          ]
```

**Analysis**: Identical structure, only `native_script` era differs → **EXCELLENT CANDIDATE**

**block** (Allegra vs Mary):
```haskell
-- Both have identical structure but parameterized by different eras for:
-- - header (changes)
-- - transaction_body (changes)
-- - transaction_witness_set (Mary uses AllegraEra!)
-- - auxiliary_data (stays AllegraEra)
```

**Analysis**: 4 era parameters needed, but pattern is identical → **GOOD CANDIDATE**

### Non-Candidates (Do Not Deduplicate)

**transaction_body** (Shelley vs Allegra):
```haskell
-- Shelley: field 3 is REQUIRED
idx 3 ==> huddleRule @"slot" @ShelleyEra

-- Allegra: field 3 is OPTIONAL, and field 8 added
opt (idx 3 ==> huddleRule @"slot" @AllegraEra)
...
opt (idx 8 ==> huddleRule @"slot" @AllegraEra)  -- NEW!
```

**Analysis**: Structural change (required→optional), new field added → **NOT A CANDIDATE** (represents genuine semantic evolution)

**Small instances** (1-3 lines):
```haskell
instance HasCDDL "major_protocol_version" AllegraEra where
  huddleItem = HIRule $ "major_protocol_version" =:= (0 :: Integer) ... (4 :: Integer)
```

**Analysis**: Could deduplicate, but marginal benefit. We did it for completeness, but this is a judgment call.

### Decision Criteria

Create a smart constructor when **ALL** of these are true:
1. ✅ Substantial size (typically 10+ lines, minimum 5 lines)
2. ✅ Identical structure across eras
3. ✅ Only era-specific type parameters differ
4. ✅ Will be used by 2+ eras

Do NOT create a smart constructor when **ANY** of these are true:
1. ❌ Structural differences (field changes, new/removed fields)
2. ❌ Represents semantic evolution between eras
3. ❌ Very small (1-2 lines) and unlikely to change
4. ❌ Only used in one era

## Smart Constructor Patterns (Single Era Parameter)

### The Single Era Pattern (REQUIRED)

All smart constructors use a single `era` type parameter. This ensures type-level isolation.

```haskell
mkProtocolVersion :: forall era. HasCDDL "major_protocol_version" era => HuddleItem
mkProtocolVersion =
  HIGroup $ "protocol_version" =:~ grp [a $ huddleRule @"major_protocol_version" @era, a VUInt]

-- Shelley:
instance HasCDDL "protocol_version" ShelleyEra where
  huddleItem = mkProtocolVersion @ShelleyEra

-- Allegra:
instance HasCDDL "protocol_version" AllegraEra where
  huddleItem = mkProtocolVersion @AllegraEra
```

### Aggregating Structures

Even complex structures like `block` use single era parameter:

```haskell
mkBlock ::
  forall era.
  ( HasCDDL "header" era
  , HasCDDL "transaction_body" era
  , HasCDDL "transaction_witness_set" era
  , HasCDDL "auxiliary_data" era
  ) =>
  HuddleItem
mkBlock =
  HIRule $
    "block"
      =:= arr
        [ a $ huddleRule @"header" @era
        , "transaction_bodies" ==> arr [0 <+ a (huddleRule @"transaction_body" @era)]
        , "transaction_witness_sets" ==> arr [0 <+ a (huddleRule @"transaction_witness_set" @era)]
        , "auxiliary_data_set"
            ==> mp
              [ 0
                  <+ asKey (huddleRule @"transaction_index" @era)
                  ==> huddleRule @"auxiliary_data" @era
              ]
        ]

-- Allegra:
instance HasCDDL "block" AllegraEra where
  huddleItem = mkBlock @AllegraEra

-- Mary:
instance HasCDDL "block" MaryEra where
  huddleItem = mkBlock @MaryEra
```

### Providing All Required Instances

To use a smart constructor for an era, **all constraints must be satisfied** by that era's instances.

**Example**: To use `mkBlock @AllegraEra`, AllegraEra must have instances for:
- `HasCDDL "header" AllegraEra`
- `HasCDDL "transaction_body" AllegraEra`
- `HasCDDL "transaction_witness_set" AllegraEra`
- `HasCDDL "auxiliary_data" AllegraEra`

These instances may themselves use smart constructors from Shelley:
```haskell
-- Allegra provides its own header instance using Shelley's constructor
instance HasCDDL "header" AllegraEra where
  huddleItem = mkHeader @AllegraEra

-- Which requires AllegraEra to have header_body
instance HasCDDL "header_body" AllegraEra where
  huddleItem = mkHeaderBody @AllegraEra

-- Which requires AllegraEra to have operational_cert and protocol_version
instance HasCDDL "operational_cert" AllegraEra where
  huddleItem = mkOperationalCert @AllegraEra

instance HasCDDL "protocol_version" AllegraEra where
  huddleItem = mkProtocolVersion @AllegraEra
```

**The chain of dependencies is satisfied within the same era module**, using smart constructors from the originating era.

### Benefit: No Cross-Era References

With this pattern, `@ShelleyEra` **never appears** in Allegra's module. Each module is self-contained:
- Shelley.CDDL uses only `@ShelleyEra`
- Allegra.CDDL uses only `@AllegraEra`
- Mary.CDDL uses only `@MaryEra`

Dependencies are on **smart constructors** (polymorphic functions), not on specific era instances.

## Lessons Learned

### Language Extension Requirements

When creating smart constructors with multiple type parameters, you need:

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}  -- Required for forall without value parameters
{-# LANGUAGE FlexibleContexts #-}     -- Required for complex constraints
```

Without `AllowAmbiguousTypes`, you'll get:
```
Cannot satisfy: ... In the ambiguity check for 'mkBlock'
To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
```

### Comment Improvements Are Valuable

We made two types of comment improvements:

1. **Added missing comments**:
   ```diff
   +; Pool parameters for stake pool registration
   pool_params = ...
   ```
   This improves documentation for all eras.

2. **Removed forward-looking references**:
   ```diff
   -; This is the 6-variant native script format used by
   -; Allegra, Mary, Alonzo, Babbage, and Conway.
   ```
   Following the principle: Each era should describe itself, not predict the future.

### Style Consistency Matters

Converting `HIRule (...)` to `HIRule $ ...` throughout:
- Reduces visual noise
- Makes structure more apparent
- Follows Haskell community conventions
- Easier to read diffs

### Certificate Naming Standardization

Original names were inconsistent between implementation and spec:
- `stake_registration_cert_deprecated` vs `account_registration_cert`
- `stake_deregistration_cert_deprecated` vs `account_unregistration_cert`
- `stake_delegation_cert` vs `delegation_to_stake_pool_cert`

Standardizing to spec names improves clarity and reduces confusion.

### Build-Verify-Diff Workflow Is Critical

**Always** follow this sequence:
1. Make changes
2. Build: `cabal build cardano-ledger-{era}:cddl`
3. Generate: `cabal run cardano-ledger-{era}:generate-cddl`
4. Verify: `git diff --exit-code eras/{era}/impl/cddl-files/{era}.cddl`

**Any unexpected diff means something went wrong.** Investigate before proceeding.

### Cabal Dependencies

When removing cross-era references, update cabal dependencies:
- Remove `cardano-ledger-shelley` from Allegra's cddl library build-depends
- Keep only `cardano-ledger-shelley:cddl` for smart constructor imports

```diff
  build-depends:
    base,
    cardano-ledger-allegra,
    cardano-ledger-core:cddl,
-   cardano-ledger-shelley,         -- Remove: no longer used
    cardano-ledger-shelley:cddl,    -- Keep: for smart constructors
    cuddle >=0.4,
    heredoc,
```

### Incremental Approach Works Best

We proceeded: Shelley → Shelley+Allegra → Shelley+Allegra+Mary

Benefits:
- Catch issues early
- Build confidence in the pattern
- Easier to debug when things go wrong
- Natural learning progression

## Future Work

### Immediate Next Steps

1. **Apply to Alonzo era** ⬅️ NEXT
   - Handle Plutus script additions (V1, V2)
   - Redeemer and execution units structures
   - Script data and cost models
   - Analyze transaction_witness_set evolution (adds plutus_v1_script, plutus_data, redeemers)
   - Likely will need new smart constructors for Plutus-specific structures

2. **Apply to Babbage era**
   - Inline datums in transaction outputs
   - Reference inputs
   - Reference scripts
   - Collateral return
   - Analyze transaction_output evolution (biggest change from Mary)

3. **Apply to Conway era**
   - Governance certificates
   - DRep delegation
   - Protocol parameter groups restructuring
   - Voting and proposals

### Key Considerations for Alonzo

Alonzo introduces **Plutus scripts**, which is a major addition. Expected differences:

**transaction_witness_set** will likely gain:
- `plutus_v1_script` array (index 3)
- `plutus_data` array (index 4)
- `redeemers` array (index 5)

**Strategy**:
- Analyze if `mkTransactionWitnessSet` can be extended with optional parameters
- Or create `mkPlutusTransactionWitnessSet` for Alonzo+
- Compare Alonzo vs Babbage witness sets to see if pattern continues

**transaction_output** evolution:
- Shelley/Allegra: `[address, coin]`
- Mary: `[address, value]` (multi-asset)
- Alonzo: Likely adds datum hash
- Babbage: Adds inline datum, reference script

**Cannot deduplicate** transaction_output across these eras - represents genuine evolution.

### Potential Additional Smart Constructors

Will discover during Alonzo analysis:
1. Plutus-related structures (if reused by Babbage/Conway)
2. Cost model structures
3. Script purpose/redeemer structures

### Long-term Maintenance

After completing all eras:
1. **Document smart constructor dependencies** (which era imports from where)
2. **Create dependency diagram** showing era → era smart constructor reuse
3. **Update cabal file documentation** for cddl libraries
4. **Consider extracting to Core.CDDL.Constructors module** if it becomes too large in Shelley

## Common Pitfalls

### 1. Adding Unnecessary Parameters

**Symptom**: Creating helpers with `Rule` or `HuddleItem` parameters
**Fix**: Use polymorphic type variables with constraints instead

### 2. Over-Constraining

**Symptom**: Adding `Era era` when it's already implied by other constraints
**Fix**: Let GHC's constraint solver handle implied constraints, only specify minimal required constraints

### 3. Forgetting to Export

**Symptom**: Compile errors in importing era about "not in scope"
**Fix**: Remember to add smart constructors to export list in Shelley.CDDL module

### 4. Not Verifying CDDL Output

**Symptom**: Silent semantic changes in generated CDDL
**Fix**: Always run generate-cddl and git diff after refactoring

### 5. Inconsistent Naming

**Symptom**: Helper called `makeHeader` in one place, `mkHeader` in another
**Fix**: Stick to `mk` prefix for all smart constructors

## Questions for Consideration

1. **Should we create a separate module for smart constructors?**
   - Currently they live in Shelley.CDDL
   - Could create Core.CDDL.Constructors or similar
   - Tradeoff: Better organization vs. more imports

2. **How to handle era-specific extensions?**
   - Some definitions only exist in later eras (e.g., Plutus scripts in Alonzo)
   - Pattern: Define in the era where introduced, later eras import if unchanged

3. **Type-level era relationships?**
   - Could we encode "Allegra extends Shelley" at type level?
   - Would that help with constraints or make things more complex?

## Resources

- **Huddle library**: `Codec.CBOR.Cuddle.Huddle`
- **CDDL specification**: RFC 8610
- **HasCDDL type class**: `libs/cardano-ledger-core/cddl/Cardano/Ledger/HasCDDL.hs`
- **Generated CDDL files**: `eras/{era}/impl/cddl-files/{era}.cddl`

## Branch Information

- **Branch**: `aniketd/cddl-type-classes`
- **Base**: `master`
- **Key files**:
  - `libs/cardano-ledger-core/cddl/Cardano/Ledger/Core/CDDL.hs`
  - `eras/shelley/impl/cddl/Cardano/Ledger/Shelley/CDDL.hs`
  - `eras/allegra/impl/cddl/Cardano/Ledger/Allegra/CDDL.hs`
  - Similar paths for Mary, Alonzo, Babbage

---

## PR Review Feedback and Refactoring Plan (2025-11-18)

### Review Summary from PR #5419

**Reviewer:** lehins
**Date:** November 2025
**Status:** Implementation in progress - comprehensive type-safe refactoring
**Scope:** Core, Shelley, and Allegra eras (Mary, Alonzo, Babbage deferred)

### Key Changes Requested

#### 1. Type Class Architecture - Split into Three Classes (CRITICAL)

**Current Problem:**
- Single `HasCDDL` class returns `HuddleItem`
- Helper functions (`huddleRule`, `huddleGroup`, `huddleGRule`) pattern match on `HuddleItem`
- Uses `error` for type mismatches - **not type-safe**

**Current Implementation (HasCDDL.hs:22-41):**
```haskell
class Era era => HasCDDL (name :: Symbol) era where
  huddleItem :: HuddleItem

huddleRule :: forall name era. (HasCDDL name era, KnownSymbol name) => Rule
huddleRule =
  case huddleItem @name @era of
    HIRule rule -> rule
    _ -> error $ "Expected Rule for " <> symbolVal (Proxy @name)

huddleGroup :: forall name era. (HasCDDL name era, KnownSymbol name) => Named Group
huddleGroup =
  case huddleItem @name @era of
    HIGroup g -> g
    _ -> error $ "Expected Group for " <> symbolVal (Proxy @name)
```

**Requested Solution:**
```haskell
class (KnownSymbol name, Era era) => HuddleRule (name :: Symbol) era where
  huddleRule :: Proxy era -> Rule

class (KnownSymbol name, Era era) => HuddleGRule (name :: Symbol) era where
  huddleGRule :: Proxy era -> GRuleDef

class (KnownSymbol name, Era era) => HuddleGroup (name :: Symbol) era where
  huddleGroup :: Proxy era -> Named Group
```

**Benefits:**
- ✅ **Fully type-safe** - no runtime errors possible
- ✅ **Compiler-enforced correctness** - wrong type class = compile error
- ✅ **Better type inference** - Proxy parameter helps GHC
- ✅ **Clearer intent** - class name indicates return type

#### 2. Add Proxy Parameter to Methods

**Current:**
```haskell
huddleItem :: HuddleItem
-- Usage: huddleRule @"name" @era
```

**Requested:**
```haskell
huddleRule :: Proxy era -> Rule
-- Usage: huddleRule (Proxy @era) @"name"
```

**Benefits:**
- Reduces type application verbosity in complex expressions
- Better type inference
- Follows standard Haskell proxy pattern

#### 3. Module and File Naming

**Module Rename:**
- `Cardano.Ledger.HasCDDL` → `Cardano.Ledger.Huddle`
- Rationale: "CDDL is generated from Huddle" - emphasize the DSL, not the output

**File Renames:**
- `*/cddl/Cardano/Ledger/*/CDDL.hs` → `*/huddle/Cardano/Ledger/*/HuddleSpec.hs`
- `*/testlib/Test/Cardano/Ledger/*/CDDL.hs` → keep as is (these are tests)
- Rationale: Distinguish Huddle specifications from CDDL validation tests

**Smart Constructor Naming:**
- **Decision:** Keep `mk` prefix convention
- Rationale: Simple, consistent, already established across 37+ constructors

#### 4. Build Configuration Cleanup

**Remove unnecessary flags from `generate-cddl` executables:**
```diff
  executable generate-cddl
    ghc-options:
      ...
      -threaded
      -rtsopts
-     -with-rtsopts=-N
```

**Rationale:** CDDL generation is single-threaded, no benefit from parallelization

#### 5. Data File Handling

**Current (generate-cddl/Main.hs):**
```haskell
main = do
  let outputPath = "eras/shelley/impl/cddl-files/shelley.cddl"
  createDirectoryIfMissing True (takeDirectory outputPath)
  writeSpec shelleyCDDL outputPath
```

**Requested:**
```haskell
import Paths_cardano_ledger_shelley (getDataFileName)

main = do
  outputPath <- getDataFileName "cddl-files/shelley.cddl"
  createDirectoryIfMissing True (takeDirectory outputPath)
  writeSpec shelleyCDDL outputPath
```

**Rationale:** Works correctly when executable is installed outside repo context

---

### Implementation Strategy (Least-Effort-First, Core/Shelley/Allegra Only)

#### Phase 1: Quick Wins - Build Configuration
**Effort:** ⭐ (15 minutes)

**Tasks:**
- Remove `-with-rtsopts=-N` from Shelley `cardano-ledger-shelley.cabal:174`
- Remove `-with-rtsopts=-N` from Allegra `cardano-ledger-allegra.cabal` (similar location)

**No code changes**, just build configuration cleanup.

#### Phase 2: Data File Handling
**Effort:** ⭐⭐ (30 minutes)

**Shelley (`eras/shelley/impl/generate-cddl/Main.hs`):**
```diff
  module Main where

  import Cardano.Ledger.Shelley.CDDL (shelleyCDDL)
+ import Paths_cardano_ledger_shelley (getDataFileName)
  import System.Directory (createDirectoryIfMissing)
  import System.FilePath (takeDirectory)
  import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

  main :: IO ()
  main = do
-   let outputPath = "eras/shelley/impl/cddl-files/shelley.cddl"
+   outputPath <- getDataFileName "cddl-files/shelley.cddl"
    createDirectoryIfMissing True (takeDirectory outputPath)
    writeSpec shelleyCDDL outputPath
    putStrLn $ "Generated CDDL file at: " ++ outputPath
```

**Allegra:** Same pattern with `Paths_cardano_ledger_allegra`

#### Phase 3: File & Module Renames
**Effort:** ⭐⭐ (1 hour)

**Core:**
- `libs/cardano-ledger-core/cddl/Cardano/Ledger/HasCDDL.hs` → `libs/cardano-ledger-core/huddle/Cardano/Ledger/Huddle.hs`
- `libs/cardano-ledger-core/cddl/Cardano/Ledger/Core/CDDL.hs` → `libs/cardano-ledger-core/huddle/Cardano/Ledger/Core/HuddleSpec.hs`
- Update module declarations in both files
- Update `cardano-ledger-core.cabal`:
  - Library name: `cddl` → `huddle`
  - hs-source-dirs: `cddl` → `huddle`

**Shelley:**
- `eras/shelley/impl/cddl/` → `eras/shelley/impl/huddle/`
- `Cardano.Ledger.Shelley.CDDL` → `Cardano.Ledger.Shelley.HuddleSpec`
- Update `cardano-ledger-shelley.cabal`:
  - Library exposed-modules
  - hs-source-dirs
  - build-depends: `cardano-ledger-core:cddl` → `cardano-ledger-core:huddle`

**Allegra:**
- Same pattern as Shelley

#### Phase 4: Core Type Class Split
**Effort:** ⭐⭐⭐ (2 hours)

**New `Huddle.hs`:**
```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Ledger.Huddle (
  HuddleRule (..),
  HuddleGRule (..),
  HuddleGroup (..),
  toHuddleItem,
  toHuddleItemGroup,
  toHuddleItemGRule,
) where

import Cardano.Ledger.Core (Era)
import Codec.CBOR.Cuddle.Huddle
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol)

-- | Type class for CDDL rules (top-level definitions)
-- Example: transaction_body = [...]
class (KnownSymbol name, Era era) => HuddleRule (name :: Symbol) era where
  huddleRule :: Proxy era -> Rule

-- | Type class for CDDL group rules (used in generic maps/arrays)
-- Example: operational_cert = (...)
class (KnownSymbol name, Era era) => HuddleGRule (name :: Symbol) era where
  huddleGRule :: Proxy era -> GRuleDef

-- | Type class for CDDL named groups
-- Example: protocol_version = (major: uint, minor: uint)
class (KnownSymbol name, Era era) => HuddleGroup (name :: Symbol) era where
  huddleGroup :: Proxy era -> Named Group

-- | Helper functions to convert back to HuddleItem for collection
toHuddleItem :: Rule -> HuddleItem
toHuddleItem = HIRule

toHuddleItemGroup :: Named Group -> HuddleItem
toHuddleItemGroup = HIGroup

toHuddleItemGRule :: GRuleDef -> HuddleItem
toHuddleItemGRule = HIGRule
```

#### Phase 5: Update Core Instances
**Effort:** ⭐⭐ (1 hour)

**Pattern for Core/HuddleSpec.hs:**
```diff
- import Cardano.Ledger.HasCDDL
+ import Cardano.Ledger.Huddle

- instance Era era => HasCDDL "coin" era where
-   huddleItem = HIRule $ "coin" =:= VUInt
+ instance Era era => HuddleRule "coin" era where
+   huddleRule _ = "coin" =:= VUInt

- instance Era era => HasCDDL "hash28" era where
-   huddleItem = HIRule $ "hash28" =:= VBytes `sized` (28 :: Word64)
+ instance Era era => HuddleRule "hash28" era where
+   huddleRule _ = "hash28" =:= VBytes `sized` (28 :: Word64)
```

**Decision Matrix:**
- `HIRule $` → `HuddleRule` + `huddleRule _`
- `HIGroup $` → `HuddleGroup` + `huddleGroup _`
- `HIGRule $` → `HuddleGRule` + `huddleGRule _`

#### Phase 6: Update Shelley Smart Constructors
**Effort:** ⭐⭐⭐⭐ (3 hours)

**Example 1: mkHeader (HIRule → Rule)**
```diff
- mkHeader :: forall era. HasCDDL "header_body" era => HuddleItem
+ mkHeader :: forall era. HuddleRule "header_body" era => Rule
  mkHeader =
-   HIRule $
      "header"
-       =:= arr [a $ huddleRule @"header_body" @era, "body_signature" ==> huddleRule @"kes_signature" @era]
+       =:= arr [a $ huddleRule (Proxy @era) @"header_body", "body_signature" ==> huddleRule (Proxy @era) @"kes_signature"]
```

**Example 2: mkProtocolVersion (HIGroup → Named Group)**
```diff
- mkProtocolVersion :: forall era. HasCDDL "major_protocol_version" era => HuddleItem
+ mkProtocolVersion :: forall era. HuddleRule "major_protocol_version" era => Named Group
  mkProtocolVersion =
-   HIGroup $ "protocol_version" =:~ grp [a $ huddleRule @"major_protocol_version" @era, a VUInt]
+   "protocol_version" =:~ grp [a $ huddleRule (Proxy @era) @"major_protocol_version", a VUInt]
```

**All 35 Shelley smart constructors need this pattern applied.**

#### Phase 7: Update Shelley Instances
**Effort:** ⭐⭐⭐ (2 hours)

**Pattern:**
```diff
- instance HasCDDL "protocol_version" ShelleyEra where
-   huddleItem = mkProtocolVersion @ShelleyEra
+ instance HuddleGroup "protocol_version" ShelleyEra where
+   huddleGroup _ = mkProtocolVersion @ShelleyEra

- instance HasCDDL "header" ShelleyEra where
-   huddleItem = mkHeader @ShelleyEra
+ instance HuddleRule "header" ShelleyEra where
+   huddleRule _ = mkHeader @ShelleyEra
```

**Update collection function:**
```diff
- shelleyCDDL :: Huddle
- shelleyCDDL =
+ shelleyHuddle :: Huddle
+ shelleyHuddle =
    collectFrom
-     [ huddleItem @"block" @ShelleyEra
-     , huddleItem @"transaction" @ShelleyEra
-     , huddleItem @"signkey_kes" @ShelleyEra
+     [ toHuddleItem $ huddleRule (Proxy @ShelleyEra) @"block"
+     , toHuddleItem $ huddleRule (Proxy @ShelleyEra) @"transaction"
+     , toHuddleItem $ huddleRule (Proxy @ShelleyEra) @"signkey_kes"
      ]
```

#### Phase 8: Update Allegra
**Effort:** ⭐⭐⭐ (2 hours)

**Same pattern as Shelley:**
- Update smart constructors (mkBlock, mkTransaction)
- Update instances
- Update imports from Shelley
- Update collection function to `allegraHuddle`

#### Phase 9: Verification
**Effort:** ⭐⭐ (1 hour)

**Build:**
```bash
cabal clean
cabal build cardano-ledger-core:huddle
cabal build cardano-ledger-shelley:cddl
cabal build cardano-ledger-allegra:cddl
```

**Generate CDDL:**
```bash
cabal run cardano-ledger-shelley:generate-cddl
cabal run cardano-ledger-allegra:generate-cddl
```

**Verify byte-for-byte identical:**
```bash
git diff --exit-code eras/shelley/impl/cddl-files/shelley.cddl
git diff --exit-code eras/allegra/impl/cddl-files/allegra.cddl
```

**Run tests:**
```bash
cabal test cardano-ledger-shelley:tests --test-show-details=direct
cabal test cardano-ledger-allegra:tests --test-show-details=direct
```

---

### Conversion Patterns and Examples

#### Determining Which Type Class to Use

**Decision Tree:**
1. Look at the definition's right-hand side
2. If it's `"name" =:= ...` → uses `HuddleRule`, returns `Rule`
3. If it's `"name" =:~ grp [...]` → uses `HuddleGroup`, returns `Named Group`
4. If it's `"name" =:~ ...` (other) → uses `HuddleGRule`, returns `GRuleDef`

**Examples:**
```haskell
-- Rule (=:=)
instance HuddleRule "coin" era where
  huddleRule _ = "coin" =:= VUInt

-- Named Group (=:~ grp [...])
instance HuddleGroup "protocol_version" era where
  huddleGroup _ = "protocol_version" =:~ grp [a VUInt, a VUInt]

-- GRuleDef (=:~ without grp)
instance HuddleGRule "operational_cert" era where
  huddleGRule _ = "operational_cert" =:~ ...
```

#### Smart Constructor Conversion Checklist

For each smart constructor:
- [ ] Update return type: `HuddleItem` → `Rule` or `Named Group` or `GRuleDef`
- [ ] Update constraints: `HasCDDL` → `HuddleRule` or `HuddleGroup` or `HuddleGRule`
- [ ] Remove wrapper: Delete `HIRule $` or `HIGroup $` or `HIGRule $`
- [ ] Update all internal `huddleRule @"name" @era` → `huddleRule (Proxy @era) @"name"`
- [ ] Update all internal `huddleGroup @"name" @era` → `huddleGroup (Proxy @era) @"name"`
- [ ] Update all internal `huddleGRule @"name" @era` → `huddleGRule (Proxy @era) @"name"`

#### Instance Conversion Checklist

For each instance:
- [ ] Update type class: `HasCDDL` → correct type class
- [ ] Update method name: `huddleItem` → `huddleRule _` or `huddleGroup _` or `huddleGRule _`
- [ ] Verify type class matches what the smart constructor returns

---

### Type Safety Demonstration

**Before (can compile with wrong type):**
```haskell
-- This would compile but error at runtime:
instance HasCDDL "foo" ShelleyEra where
  huddleItem = HIGroup $ "foo" =:~ grp [...]  -- Returns Group

elsewhere :: Rule
elsewhere = huddleRule @"foo" @ShelleyEra  -- Expects Rule, runtime error!
```

**After (compile error if wrong):**
```haskell
-- This won't compile:
instance HuddleRule "foo" ShelleyEra where
  huddleRule _ = "foo" =:~ grp [...]  -- TYPE ERROR: returns Named Group, not Rule

-- Must use correct class:
instance HuddleGroup "foo" ShelleyEra where
  huddleGroup _ = "foo" =:~ grp [...]  -- Correct!
```

---

### Benefits of This Refactoring

**Type Safety:**
- ✅ Eliminates all `error` calls - no runtime failures
- ✅ Compile-time guarantee of correct type usage
- ✅ Wrong type class → immediate compile error

**Code Clarity:**
- ✅ Class name indicates return type
- ✅ Explicit separation of Rules, Groups, and GRules
- ✅ Better IDE support and type hints

**Maintainability:**
- ✅ Easier to add new eras (clear patterns)
- ✅ Refactoring is safer (type checker catches mistakes)
- ✅ Better documentation through types

**Correctness:**
- ✅ Data file handling works in all contexts
- ✅ Build configuration optimized
- ✅ Follows Haskell best practices

---

### Timeline Estimate (Core, Shelley, Allegra Only)

- **Phase 1:** ~15 minutes (build config)
- **Phase 2:** ~30 minutes (data file handling)
- **Phase 3:** ~1 hour (renames)
- **Phase 4:** ~2 hours (core type classes)
- **Phase 5:** ~1 hour (core instances)
- **Phase 6:** ~3 hours (Shelley smart constructors)
- **Phase 7:** ~2 hours (Shelley instances)
- **Phase 8:** ~2 hours (Allegra)
- **Phase 9:** ~1 hour (verification)

**Total:** ~12-13 hours of focused work

---

### Work Status (as of 2025-11-18)

**Planning:** ✅ Complete
**Implementation:** 🚧 In progress
**Scope:** Core, Shelley, Allegra (Mary, Alonzo, Babbage deferred for future work)

**Next Immediate Steps:**
1. Phase 1: Build configuration cleanup (easiest wins)
2. Phase 2: Data file handling
3. Phase 3: File and module renames
4. Phase 4+: Type class refactoring
---

## Implementation Complete (2025-11-18)

### All PR #5419 Review Requirements Addressed ✅

**Status:** 100% Complete - All review feedback implemented

#### 1. Type Class Architecture - COMPLETED ✅
- ✅ Split `HasCDDL` into three type-safe classes:
  - `HuddleRule` (returns `Rule`)
  - `HuddleGroup` (returns `Named Group`)
  - `HuddleGRule` (returns `GRuleDef`)
- ✅ All `error` calls eliminated - fully type-safe at compile time
- ✅ `Proxy era` parameter added to all methods
- Location: `libs/cardano-ledger-core/cddl/Cardano/Ledger/Huddle.hs`

#### 2. Module & File Naming - COMPLETED ✅
- ✅ `Cardano.Ledger.HasCDDL` → `Cardano.Ledger.Huddle`
- ✅ `CDDL.hs` → `HuddleSpec.hs` (all eras)
- ✅ Sublibrary name: `cddl` (per user preference)
- ✅ Directory structure: `cddl/lib/` and `cddl/exe/`

#### 3. Smart Constructor Naming - COMPLETED ✅
- ✅ Removed `mk` prefix from all constructors
- ✅ Added type suffixes: `Rule`, `Group`, `GRule`
- Examples: `headerRule`, `protocolVersionGroup`, `operationalCertGroup`

#### 4. Build Configuration - COMPLETED ✅
- ✅ Removed `-with-rtsopts=-N` from all `generate-cddl` executables
- ✅ Shelley: cleaned up
- ✅ Allegra: removed duplicate `huddle-cddl` executable
- ✅ Added `Paths_*` modules to `other-modules`

#### 5. Data File Handling - COMPLETED ✅
- ✅ Shelley uses `getDataFileName "cddl-files/shelley.cddl"`
- ✅ Allegra uses `getDataFileName "cddl-files/allegra.cddl"`
- Works correctly when executable is installed outside repo context

#### 6. Instance Conversion - COMPLETED ✅
- ✅ All ~30 Core instances converted to new type classes
- ✅ All Shelley instances converted
- ✅ All Allegra instances converted
- Pattern: `HasCDDL "name"` → `HuddleRule "name"` / `HuddleGroup "name"` / `HuddleGRule "name"`

#### 7. CDDL Output Preservation - VERIFIED ✅
- ✅ `shelley.cddl` - byte-for-byte identical
- ✅ `allegra.cddl` - byte-for-byte identical
- All critical fixes applied:
  - `majorProtocolVersionRule` uses `@ByronEra` for lower bound
  - `transactionWitnessSetRule` uses array pattern (not `untagged_set`)
  - `bootstrapWitnessRule` includes field names
  - `moveInstantaneousRewardRule` uses correct structure
  - `protocolParamUpdateRule` includes all 17 fields with correct constraints

### Additional Code Simplifications (2025-11-18) ✅

Beyond the PR requirements, two major simplifications were implemented:

#### Simplification 1: Remove Redundant `@era` Type Application

**Pattern:** `huddleRule @"name" @era p` → `huddleRule @"name" p`

**Rationale:** Since `p :: Proxy era`, GHC can infer the `era` type from the proxy argument, so the explicit `@era` type application is redundant.

**Impact:**
- ~122 occurrences simplified across all files
- Applies to `huddleRule`, `huddleGroup`, and `huddleGRule`

**Example:**
```haskell
-- Before:
arr [a $ huddleRule @"header_body" @era p, "body_signature" ==> huddleRule @"kes_signature" @era p]

-- After:
arr [a $ huddleRule @"header_body" p, "body_signature" ==> huddleRule @"kes_signature" p]
```

#### Simplification 2: Reuse Proxy Parameter Instead of Reconstructing

**Pattern:** `huddleRule _ = ... huddleRule @"name" (Proxy @era)` → `huddleRule p = ... huddleRule @"name" p`

**Rationale:** Instead of ignoring the proxy parameter and reconstructing it with `(Proxy @era)`, simply pass the received parameter along to nested calls.

**Impact:**
- ~17 `(Proxy @era)` constructions eliminated in Core
- Removed redundant import of `Data.Proxy` from Core/HuddleSpec.hs
- More idiomatic Haskell (pass parameters, don't reconstruct them)

**Example:**
```haskell
-- Before:
instance Era era => HuddleRule "positive_int" era where
  huddleRule _ = "positive_int" =:= (1 :: Integer) ... huddleRule @"max_word64" (Proxy @era)
  --        ^                                                                     ^^^^^^^^^^^^
  --     ignore proxy                                                         reconstruct it!

-- After:
instance Era era => HuddleRule "positive_int" era where
  huddleRule p = "positive_int" =:= (1 :: Integer) ... huddleRule @"max_word64" p
  --        ^                                                                     ^
  --     use proxy                                                         pass it along!
```

**Total Simplifications:**
- Removed ~122 redundant `@era` type applications
- Removed ~17 `(Proxy @era)` constructions
- Removed 1 redundant import
- Much cleaner, more readable code

### Files Modified (Final State)

**Core:**
- `libs/cardano-ledger-core/cddl/Cardano/Ledger/Huddle.hs` - Type class definitions
- `libs/cardano-ledger-core/cddl/Cardano/Ledger/Core/HuddleSpec.hs` - Basic type instances
- `libs/cardano-ledger-core/cardano-ledger-core.cabal` - Build configuration

**Shelley:**
- `eras/shelley/impl/cddl/lib/Cardano/Ledger/Shelley/HuddleSpec.hs` - Shelley instances and smart constructors
- `eras/shelley/impl/cddl/exe/Main.hs` - CDDL generation executable
- `eras/shelley/impl/cardano-ledger-shelley.cabal` - Build configuration

**Allegra:**
- `eras/allegra/impl/cddl/lib/Cardano/Ledger/Allegra/HuddleSpec.hs` - Allegra instances and smart constructors
- `eras/allegra/impl/cddl/exe/Main.hs` - CDDL generation executable
- `eras/allegra/impl/cardano-ledger-allegra.cabal` - Build configuration
- DELETED: `eras/allegra/impl/huddle-cddl/` - Removed duplicate executable

### Verification Results

**Compilation:**
```bash
cabal build cardano-ledger-core:cddl         # ✅ Success
cabal build cardano-ledger-shelley:cddl      # ✅ Success
cabal build cardano-ledger-allegra:cddl      # ✅ Success
```

**CDDL Generation:**
```bash
cabal run cardano-ledger-shelley:generate-cddl   # ✅ Success
cabal run cardano-ledger-allegra:generate-cddl   # ✅ Success
```

**Output Verification:**
```bash
git diff --exit-code eras/shelley/impl/cddl-files/shelley.cddl    # ✅ No changes
git diff --exit-code eras/allegra/impl/cddl-files/allegra.cddl    # ✅ No changes
```

### Benefits Achieved

**Type Safety:**
- ✅ Zero runtime errors possible - all type mismatches caught at compile time
- ✅ Class name indicates return type (`HuddleRule` → `Rule`, etc.)
- ✅ Wrong type class usage → immediate compile error

**Code Quality:**
- ✅ 139 total simplifications (~122 `@era` + ~17 `Proxy` constructions)
- ✅ More concise, readable code
- ✅ More idiomatic Haskell (infer what you can, pass what you receive)
- ✅ Eliminated all unnecessary boilerplate

**Maintainability:**
- ✅ Clear separation of Rules, Groups, and GRules
- ✅ Better IDE support and type hints
- ✅ Easier to add new eras (clear patterns established)
- ✅ Refactoring is safer (type checker catches mistakes)

**Correctness:**
- ✅ Data file handling works in all installation contexts
- ✅ Build configuration optimized
- ✅ All review feedback addressed
- ✅ CDDL output verified identical

### Next Steps

**Completed for Core, Shelley, Allegra:**
- ✅ Type-safe architecture
- ✅ All simplifications applied
- ✅ All tests passing
- ✅ CDDL output verified

**Future Work (Deferred):**
- Mary era - apply same patterns
- Alonzo era - handle Plutus script additions
- Babbage era - handle inline datums, reference scripts
- Conway era - handle governance certificates

**Ready for:**
- Code review
- Merge to master
- Extension to remaining eras (using established patterns)

---

## Key Learnings from Implementation

### 1. Type Applications and Parameter Order Matter

**Discovery:** Type applications (`@"name"`) must come BEFORE value arguments in Haskell.

**Wrong:** `huddleRule p @"name"` - tries to apply `@"name"` to the result of `(huddleRule p)`
**Correct:** `huddleRule @"name" p` - applies type, then value

### 2. GHC Can Infer Types from Proxy Arguments

**Before we knew:** Explicitly specified both type and proxy
```haskell
huddleRule @"name" @era (Proxy @era)  -- redundant!
```

**After understanding:** GHC infers `era` from the proxy
```haskell
huddleRule @"name" p  -- where p :: Proxy era
```

### 3. Pass Parameters, Don't Reconstruct Them

**Anti-pattern:**
```haskell
huddleRule _ = ... huddleRule @"other" (Proxy @era)  -- ignoring then reconstructing
```

**Good pattern:**
```haskell
huddleRule p = ... huddleRule @"other" p  -- pass it along
```

This is more idiomatic Haskell and reduces boilerplate significantly.

### 4. Tool Choice Matters for Complex Replacements

**Experience:** 
- `sed` with complex patterns can fail due to escaping issues
- `sd` (modern replacement for sed) handles regex better
- For simple text replacements, `sd` is more reliable

**Lesson:** Use the right tool for the job (`sd` for search/replace, `Edit` for surgical changes)

### 5. Incremental Verification Is Critical

**Process that worked:**
1. Make changes
2. Build immediately
3. Generate CDDL
4. Verify output byte-for-byte identical
5. Only then proceed to next change

**Result:** Caught issues early, never broke CDDL output

### 6. User Insights Drive Best Solutions

**User observation:** "Can't we just pass the proxy parameter instead of reconstructing it?"

**Impact:** Led to eliminating ~17 unnecessary `(Proxy @era)` constructions we hadn't noticed.

**Lesson:** Always be open to simplification suggestions - fresh eyes see patterns we miss.

---

## Documentation Updates Needed

Before final merge, update:

1. **README.md** - Document the new type class architecture
2. **Contributing guide** - Add examples of using `HuddleRule`/`HuddleGroup`/`HuddleGRule`
3. **Haddock comments** - Add to `Huddle.hs` explaining the type classes
4. **Migration guide** - For future eras to follow the pattern

---

## Final Status Summary

**Scope:** Core, Shelley, Allegra eras
**PR Requirements:** 100% addressed
**Code Quality:** Significantly improved with simplifications
**Test Status:** All passing
**CDDL Output:** Verified byte-for-byte identical
**Ready for:** Code review and merge

**Next session:** Continue to Mary/Alonzo/Babbage eras or await code review feedback.

