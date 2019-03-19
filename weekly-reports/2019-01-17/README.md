# Summary
In the last week of 2018 we finished a first version of the ledger spec. This included adding the active stake calculations, the reward calculations, and the exponential moving averages. Explanations were also added for these. On the executable spec side, we added STS instances for everything. In 2019, after the Berlin workshop, we began making changes that originated from the big review. In particular, we combined some transition systems and began re-organizing the tables.



# PRs and issues completed

| Issue/PR | Summary | Epic | Contributors | Created | Finished | Comments|
|----------|---------|------|--------------|---------|----------|---------|
| [15](https://github.com/input-output-hk/cardano-ledger-specs/issues/15) | add address types to the ledger rules | | |2018-10-19 | 2019-01-17 | |
| [63](https://github.com/input-output-hk/cardano-ledger-specs/issues/63) | Implement Property 7.1 once deposit / rewards are modelled in the executable spec | | |2018-11-08 | 2019-01-17 | |
| [109](https://github.com/input-output-hk/cardano-ledger-specs/issues/109) | witnessing in the executable model | | [mgudemann](https://github.com/mgudemann) | 2018-12-03 | 2019-01-17 | |
| [141](https://github.com/input-output-hk/cardano-ledger-specs/pull/141) | Explain why pool registration will not be censored. | | [kantp](https://github.com/kantp) | 2018-12-19 | 2018-12-20 | |
| [142](https://github.com/input-output-hk/cardano-ledger-specs/pull/142) | stake distribution calculation | | [JaredCorduan](https://github.com/JaredCorduan) | 2018-12-19 | 2018-12-20 | |
| [144](https://github.com/input-output-hk/cardano-ledger-specs/pull/144) | Finish STS rule implementations for `LEDGER` | | [mgudemann](https://github.com/mgudemann) | 2018-12-20 | 2018-12-20 | |
| [145](https://github.com/input-output-hk/cardano-ledger-specs/pull/145) | Add weekly report 2018-12-20 | | [mgudemann](https://github.com/mgudemann) | 2018-12-20 | 2018-12-20 | |
| [146](https://github.com/input-output-hk/cardano-ledger-specs/issues/146) | Complete STS rules with epoch boundary rules | | [mgudemann](https://github.com/mgudemann) | 2018-12-21 | 2019-01-17 | |
| [147](https://github.com/input-output-hk/cardano-ledger-specs/pull/147) | Stake distribution prose added | | [polinavino](https://github.com/polinavino), [JaredCorduan](https://github.com/JaredCorduan) | 2018-12-21 | 2018-12-21 | |
| [148](https://github.com/input-output-hk/cardano-ledger-specs/pull/148) | Correct a few typos in doc | | [mgudemann](https://github.com/mgudemann), [JaredCorduan](https://github.com/JaredCorduan) | 2018-12-21 | 2018-12-21 | |
| [149](https://github.com/input-output-hk/cardano-ledger-specs/pull/149) | reward calculation | | [JaredCorduan](https://github.com/JaredCorduan) | 2018-12-21 | 2018-12-21 | |
| [150](https://github.com/input-output-hk/cardano-ledger-specs/issues/150) | reward calculation in spec | | [JaredCorduan](https://github.com/JaredCorduan) | 2018-12-21 | 2019-01-17 | |
| [151](https://github.com/input-output-hk/cardano-ledger-specs/pull/151) | Rewards calculations prose | | [polinavino](https://github.com/polinavino) | 2018-12-30 | 2019-01-02 | |
| [152](https://github.com/input-output-hk/cardano-ledger-specs/pull/152) | Property-based test for Balance Preservation property | | [mgudemann](https://github.com/mgudemann) | 2019-01-02 | 2019-01-02 | |
| [153](https://github.com/input-output-hk/cardano-ledger-specs/pull/153) | Include git revision in documents | | [kantp](https://github.com/kantp), [JaredCorduan](https://github.com/JaredCorduan) | 2019-01-02 | 2019-01-02 | |
| [154](https://github.com/input-output-hk/cardano-ledger-specs/issues/154) | Reward calculation prose | | [polinavino](https://github.com/polinavino) | 2019-01-02 | 2019-01-17 | |
| [155](https://github.com/input-output-hk/cardano-ledger-specs/pull/155) | Run `languagetool` on Latex Spec | | [mgudemann](https://github.com/mgudemann) | 2019-01-03 | 2019-01-03 | |
| [156](https://github.com/input-output-hk/cardano-ledger-specs/pull/156) | Get nix-shell working for gitinfo2 | | [ruhatch](https://github.com/ruhatch), [JaredCorduan](https://github.com/JaredCorduan) | 2019-01-03 | 2019-01-03 | |
| [157](https://github.com/input-output-hk/cardano-ledger-specs/pull/157) | version 1.1 | | [JaredCorduan](https://github.com/JaredCorduan) | 2019-01-03 | 2019-01-04 | |
| [158](https://github.com/input-output-hk/cardano-ledger-specs/pull/158) | latex build scripts | | [disassembler](https://github.com/disassembler), [JaredCorduan](https://github.com/JaredCorduan) | 2019-01-03 | 2019-01-13 | |
| [160](https://github.com/input-output-hk/cardano-ledger-specs/pull/160) | Add `Ptr` type to `LedgerState` | | [mgudemann](https://github.com/mgudemann) | 2019-01-04 | 2019-01-04 | |
| [161](https://github.com/input-output-hk/cardano-ledger-specs/pull/161) | Fix `shell.nix` for the delegation design spec. | | [dnadales](https://github.com/dnadales), [nc6](https://github.com/nc6) | 2019-01-07 | 2019-01-09 | |
| [162](https://github.com/input-output-hk/cardano-ledger-specs/pull/162) | Changes to the design doc after day 1 of on-site discussion | | [kantp](https://github.com/kantp) | 2019-01-07 | 2019-01-10 | |
| [163](https://github.com/input-output-hk/cardano-ledger-specs/pull/163) | Add functions for stake distribution calculation | | [mgudemann](https://github.com/mgudemann) | 2019-01-08 | 2019-01-09 | |
| [164](https://github.com/input-output-hk/cardano-ledger-specs/pull/164) | Latex/several small edits | | [JaredCorduan](https://github.com/JaredCorduan), [nc6](https://github.com/nc6) | 2019-01-08 | 2019-01-09 | |
| [165](https://github.com/input-output-hk/cardano-ledger-specs/pull/165) | Add a pretty diagram of the fund movement from Berlin workshop. | | [nc6](https://github.com/nc6), [JaredCorduan](https://github.com/JaredCorduan) | 2019-01-09 | 2019-01-15 | |
| [166](https://github.com/input-output-hk/cardano-ledger-specs/pull/166) | Implement functions for epoch boundary | | [mgudemann](https://github.com/mgudemann) | 2019-01-09 | 2019-01-10 | |
| [167](https://github.com/input-output-hk/cardano-ledger-specs/pull/167) | Add: Add an outline for making concrete decisions | | [boothead](https://github.com/boothead), [kantp](https://github.com/kantp) | 2019-01-11 | 2019-01-11 | |
| [169](https://github.com/input-output-hk/cardano-ledger-specs/pull/169) | Rename `PrtclConsts` to `PParams` | | [mgudemann](https://github.com/mgudemann) | 2019-01-11 | 2019-01-14 | |
| [171](https://github.com/input-output-hk/cardano-ledger-specs/pull/171) | Add types for unit intervals | | [mgudemann](https://github.com/mgudemann) | 2019-01-11 | 2019-01-14 | |
| [172](https://github.com/input-output-hk/cardano-ledger-specs/pull/172) | Add reward splitting functions | | [mgudemann](https://github.com/mgudemann) | 2019-01-11 | 2019-01-14 | |
| [173](https://github.com/input-output-hk/cardano-ledger-specs/pull/173) | Add functions for reward calculation | | [mgudemann](https://github.com/mgudemann) | 2019-01-14 | 2019-01-14 | |
| [174](https://github.com/input-output-hk/cardano-ledger-specs/issues/174) | group addresses | | [JaredCorduan](https://github.com/JaredCorduan) | 2019-01-14 | 2019-01-17 | |
| [184](https://github.com/input-output-hk/cardano-ledger-specs/issues/184) | Remove "rewards ledger update" section | | [JaredCorduan](https://github.com/JaredCorduan) | 2019-01-14 | 2019-01-17 | |
| [190](https://github.com/input-output-hk/cardano-ledger-specs/issues/190) | combine epoch calc UTxO and Account transitions | | [JaredCorduan](https://github.com/JaredCorduan) | 2019-01-14 | 2019-01-17 | |
| [195](https://github.com/input-output-hk/cardano-ledger-specs/issues/195) | split NEWPC transition into two rules | | [JaredCorduan](https://github.com/JaredCorduan) | 2019-01-15 | 2019-01-17 | |
| [197](https://github.com/input-output-hk/cardano-ledger-specs/pull/197) | remove the "rewards ledger update" section | | [JaredCorduan](https://github.com/JaredCorduan) | 2019-01-15 | 2019-01-16 | |
| [198](https://github.com/input-output-hk/cardano-ledger-specs/pull/198) | adding list of contributors | | [JaredCorduan](https://github.com/JaredCorduan) | 2019-01-16 | 2019-01-16 | |
| [199](https://github.com/input-output-hk/cardano-ledger-specs/pull/199) | combine the UTxOEP and ACCNT transtions | | [JaredCorduan](https://github.com/JaredCorduan) | 2019-01-16 | 2019-01-17 | |
| [200](https://github.com/input-output-hk/cardano-ledger-specs/pull/200) | NEWPC transition is a no-op when costs are not met | | [JaredCorduan](https://github.com/JaredCorduan) | 2019-01-16 | 2019-01-17 | |
| [201](https://github.com/input-output-hk/cardano-ledger-specs/pull/201) | consolidate all address definitions into one table | | [JaredCorduan](https://github.com/JaredCorduan) | 2019-01-16 | 2019-01-17 | |
| [202](https://github.com/input-output-hk/cardano-ledger-specs/pull/202) | Implement STS rules for epoch boundary | | [mgudemann](https://github.com/mgudemann) | 2019-01-17 | 2019-01-17 | |


# Lessons learned from last week


# Things to try next week
