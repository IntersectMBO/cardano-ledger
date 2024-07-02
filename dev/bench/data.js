window.BENCHMARK_DATA = {
  "lastUpdate": 1719913377650,
  "repoUrl": "https://github.com/IntersectMBO/cardano-ledger",
  "entries": {
    "Haskell Benchmark": [
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "89243fc316d66006c5dc178719309e1c54501e86",
          "message": "Merge pull request #4121 from IntersectMBO/s-newconstaintsPhase3-addSize\n\nAdd `cardinality` and generalize `length` and `setSize`.",
          "timestamp": "2024-03-14T14:31:12+01:00",
          "tree_id": "f76386f5db286febbf9cf6ea60fa77dc9fa60484",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/89243fc316d66006c5dc178719309e1c54501e86"
        },
        "date": 1710423236640,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.0000661889706056617,
            "unit": "Nanoseconds",
            "range": 4.528627352978285e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.0000716296510086856,
            "unit": "Nanoseconds",
            "range": 3.6840275987518313e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008080970968586679,
            "unit": "Nanoseconds",
            "range": 3.634336249605946e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011400935578393104,
            "unit": "Nanoseconds",
            "range": 0.000003631437838450205
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010434467558236445,
            "unit": "Nanoseconds",
            "range": 8.568601158012177e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018826641154216845,
            "unit": "Nanoseconds",
            "range": 1.4095335663086598e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017849517767255744,
            "unit": "Nanoseconds",
            "range": 4.139136450658307e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008877150337345796,
            "unit": "Nanoseconds",
            "range": 1.9615387054077204e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "6d860294304749a767d99db92fab42f7c18d9af4",
          "message": "Merge pull request #4197 from IntersectMBO/PR-add-unsafe-proposals\n\nadd unsafeMkProposals to be used for testing",
          "timestamp": "2024-03-14T21:19:41+01:00",
          "tree_id": "c98da252f72bc4a4430bdcb5d643237c2fa5e479",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/6d860294304749a767d99db92fab42f7c18d9af4"
        },
        "date": 1710447744780,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006644766324814557,
            "unit": "Nanoseconds",
            "range": 0.0000011985228329410774
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007211771384351224,
            "unit": "Nanoseconds",
            "range": 0.0000030356769498019587
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000821078273316263,
            "unit": "Nanoseconds",
            "range": 0.0000036162034585588688
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011324972589550829,
            "unit": "Nanoseconds",
            "range": 0.000002392656484107698
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001104182464938528,
            "unit": "Nanoseconds",
            "range": 3.061064231015083e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001865731816739063,
            "unit": "Nanoseconds",
            "range": 6.722601686578167e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018016969941484005,
            "unit": "Nanoseconds",
            "range": 5.481654696029907e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009119992914336083,
            "unit": "Nanoseconds",
            "range": 1.7215584170069753e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "joosep.jaager@iohk.io",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "committer": {
            "email": "joosep.jaager@gmail.com",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "distinct": true,
          "id": "6764449651dd69be31a1565871975e5c0313e2de",
          "message": "Add additional EraRuleEvent type instances to Conway",
          "timestamp": "2024-03-15T13:14:01Z",
          "tree_id": "3a1e0228a69411b9d988e0f6cfe868eb1e239f1d",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/6764449651dd69be31a1565871975e5c0313e2de"
        },
        "date": 1710508617565,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006609956832562492,
            "unit": "Nanoseconds",
            "range": 6.829843338037309e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.000071016710411206,
            "unit": "Nanoseconds",
            "range": 0.0000010895386065880405
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008114607862429768,
            "unit": "Nanoseconds",
            "range": 0.0000017199402090139224
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011368141649856654,
            "unit": "Nanoseconds",
            "range": 0.000003421251456997198
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010991561406323633,
            "unit": "Nanoseconds",
            "range": 1.9427764599980354e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019227288637481233,
            "unit": "Nanoseconds",
            "range": 4.4076925516741113e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001822307844843586,
            "unit": "Nanoseconds",
            "range": 2.0097182176449106e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009030887062496593,
            "unit": "Nanoseconds",
            "range": 9.454101149036566e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "neil.mayhew@iohk.io",
            "name": "Neil Mayhew",
            "username": "neilmayhew"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0e115dd5a38484da1ac2d72559aff8579b887970",
          "message": "Merge pull request #4195 from IntersectMBO/neilmayhew/fix-haddock-ci\n\nFix Haddocks CI",
          "timestamp": "2024-03-15T12:49:13-06:00",
          "tree_id": "df6ed8a8a8eb3ed68937e097f23f1e053e9b7784",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/0e115dd5a38484da1ac2d72559aff8579b887970"
        },
        "date": 1710528721008,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006701842326701328,
            "unit": "Nanoseconds",
            "range": 0.0000035625454268344126
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007122220701488236,
            "unit": "Nanoseconds",
            "range": 0.0000033862085329611155
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007976981505648556,
            "unit": "Nanoseconds",
            "range": 5.138496037292668e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011586740061597496,
            "unit": "Nanoseconds",
            "range": 0.000013055120772620494
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010748822833515165,
            "unit": "Nanoseconds",
            "range": 0.0000010633802763510074
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018986259553717424,
            "unit": "Nanoseconds",
            "range": 3.3969698339231423e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018409058990870453,
            "unit": "Nanoseconds",
            "range": 7.246003741658406e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009163682054543037,
            "unit": "Nanoseconds",
            "range": 7.456003318389456e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Lucsanszky@users.noreply.github.com",
            "name": "Lucsanszky",
            "username": "Lucsanszky"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "387df931738ac8f0fa787ebfa10ee7011c51c4ec",
          "message": "Merge pull request #4175 from IntersectMBO/lucsanszky/fourmolu-column-limit\n\nSet the `column-limit` in `fourmolu` config\r\n\r\nAlso:\r\n* Remove the idempotence check when running `fourmolize.sh` as that doesn't play well with `column-limit`.\r\n  See: https://fourmolu.github.io/config/column-limit/\r\n* Add `.git-blame-ignore-revs` file so we can keep track of commits with large formatting changes and ignore them when blaming.\r\n* Make it possible to run `fourmolize.sh` on changed files only by supplying the `--changes` flag.\r\n* Run the `fourmolu` GitHub action on changed files only (compared to `origin/master`).\r\n* Setup `pre-commit` in a non-intrusive way.\r\n\r\nResolves #4069",
          "timestamp": "2024-03-18T02:05:53+01:00",
          "tree_id": "158548593ee976b435381cebfeb9f1c0046bcc06",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/387df931738ac8f0fa787ebfa10ee7011c51c4ec"
        },
        "date": 1710724128011,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006733176266451879,
            "unit": "Nanoseconds",
            "range": 0.0000011144277084205982
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007164404154005087,
            "unit": "Nanoseconds",
            "range": 0.0000014673789596197106
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008057430813282555,
            "unit": "Nanoseconds",
            "range": 0.0000017831728075399253
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011291919932861552,
            "unit": "Nanoseconds",
            "range": 0.0000014721339761339916
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010931430873912105,
            "unit": "Nanoseconds",
            "range": 1.0227129102980276e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001855083266832179,
            "unit": "Nanoseconds",
            "range": 1.7194157747629427e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017572267474900885,
            "unit": "Nanoseconds",
            "range": 2.374376634009324e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009044039100826726,
            "unit": "Nanoseconds",
            "range": 9.385633156386933e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "5f227e89997386362fbc6a2d94a873a0055ed0bc",
          "message": "Merge pull request #4200 from IntersectMBO/PR-fix-prop_GOV\n\nFix `prop_GOV` so that it runs again + make a bunch of performance improvements",
          "timestamp": "2024-03-20T13:22:37+01:00",
          "tree_id": "b0fbc0712474b25837ceb5108a15e34fac41a6bc",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/5f227e89997386362fbc6a2d94a873a0055ed0bc"
        },
        "date": 1710937516550,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006633394402088936,
            "unit": "Nanoseconds",
            "range": 0.0000010123398419197607
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007159663103929655,
            "unit": "Nanoseconds",
            "range": 5.232849204756254e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008041436444908662,
            "unit": "Nanoseconds",
            "range": 7.873272343043009e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011228952159093609,
            "unit": "Nanoseconds",
            "range": 9.640976620954195e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010342074448484656,
            "unit": "Nanoseconds",
            "range": 9.358954570391355e-8
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001871784010276965,
            "unit": "Nanoseconds",
            "range": 1.5155372318044693e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017871672773953954,
            "unit": "Nanoseconds",
            "range": 7.783323432789609e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009075398563681759,
            "unit": "Nanoseconds",
            "range": 8.878576622550031e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "teodora.danciu@tweag.io",
            "name": "teodanciu",
            "username": "teodanciu"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "a08a72e5251ba1582249b1639116a90dce39bbd1",
          "message": "Merge pull request #4206 from IntersectMBO/td/update-hackage\n\nUpdate hackage flake",
          "timestamp": "2024-03-20T15:24:05Z",
          "tree_id": "c75c0571e42bd14bd4467008fa8ed4aa1c1ba0b8",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/a08a72e5251ba1582249b1639116a90dce39bbd1"
        },
        "date": 1710948419090,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006569718743503752,
            "unit": "Nanoseconds",
            "range": 3.9891993991965784e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.0000714369639609343,
            "unit": "Nanoseconds",
            "range": 0.0000018768384964876844
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008077442404333241,
            "unit": "Nanoseconds",
            "range": 0.0000024367883317739537
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011246072535085728,
            "unit": "Nanoseconds",
            "range": 0.0000014023349775351925
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011849014762654698,
            "unit": "Nanoseconds",
            "range": 1.729325978552408e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000020250435260129528,
            "unit": "Nanoseconds",
            "range": 9.72206714120495e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019970269718229434,
            "unit": "Nanoseconds",
            "range": 3.419063085886715e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009708043845202292,
            "unit": "Nanoseconds",
            "range": 1.782699600773338e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "joosep.jaager@iohk.io",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "committer": {
            "email": "joosep.jaager@gmail.com",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "distinct": true,
          "id": "b1be2ff804ba19f9f4e4300205d8165da39139ab",
          "message": "Modified ratification logic so it accounts for resigned CC members",
          "timestamp": "2024-03-21T16:20:36Z",
          "tree_id": "3c60e4eef1b882a06d2a36271daff733894efce0",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/b1be2ff804ba19f9f4e4300205d8165da39139ab"
        },
        "date": 1711038256772,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006757615404613577,
            "unit": "Nanoseconds",
            "range": 0.000003961582356893099
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007178196071586042,
            "unit": "Nanoseconds",
            "range": 9.039453992993168e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008173193128442862,
            "unit": "Nanoseconds",
            "range": 0.0000014169363992267134
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011688651454066416,
            "unit": "Nanoseconds",
            "range": 0.000004031889596303268
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011045485729458786,
            "unit": "Nanoseconds",
            "range": 1.8548344312205442e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019070699717152768,
            "unit": "Nanoseconds",
            "range": 3.1308619650937726e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018422453809106888,
            "unit": "Nanoseconds",
            "range": 2.1303773828687326e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009031481112951877,
            "unit": "Nanoseconds",
            "range": 1.0599298519366438e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "teodora.danciu@tweag.io",
            "name": "teodanciu",
            "username": "teodanciu"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "c2fd86ea034a8021766a794cd06f888b9cfbf62c",
          "message": "Merge pull request #4210 from IntersectMBO/td/remove-small-steps-test-folder\n\nRemove small steps test folder",
          "timestamp": "2024-03-21T18:37:52Z",
          "tree_id": "868bf409c56fd357b4d7b92f52cbdd202cd13a45",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/c2fd86ea034a8021766a794cd06f888b9cfbf62c"
        },
        "date": 1711046439608,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006820841194978025,
            "unit": "Nanoseconds",
            "range": 0.000003248392934105613
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.0000728776195336861,
            "unit": "Nanoseconds",
            "range": 6.818714955721503e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008218326127822487,
            "unit": "Nanoseconds",
            "range": 0.0000015628014142210804
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011393386387851683,
            "unit": "Nanoseconds",
            "range": 7.834942175455765e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011619959969408063,
            "unit": "Nanoseconds",
            "range": 1.8778675334269634e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019709981415100236,
            "unit": "Nanoseconds",
            "range": 1.5718091304615407e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019253777425136698,
            "unit": "Nanoseconds",
            "range": 1.755636523972465e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000010103960810183481,
            "unit": "Nanoseconds",
            "range": 1.1968023505012516e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "neil.mayhew@iohk.io",
            "name": "Neil Mayhew",
            "username": "neilmayhew"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "88f0157ea6ace54d66ee47cba1ddc18ac67728fe",
          "message": "Merge pull request #4208 from IntersectMBO/neilmayhew/missingScriptsSymmetricDifference\n\nRemove missingScriptsSymmetricDifference",
          "timestamp": "2024-03-25T14:11:22-06:00",
          "tree_id": "1b23bca86c4eb1c53fba8b1ec00659872f3298a0",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/88f0157ea6ace54d66ee47cba1ddc18ac67728fe"
        },
        "date": 1711397659992,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006592074251383277,
            "unit": "Nanoseconds",
            "range": 9.595154167938307e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007107149244095101,
            "unit": "Nanoseconds",
            "range": 6.844449766793569e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008087057460747342,
            "unit": "Nanoseconds",
            "range": 0.000005695725156598513
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011177897014273672,
            "unit": "Nanoseconds",
            "range": 0.000003335962610452422
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010770232094015744,
            "unit": "Nanoseconds",
            "range": 3.70074047438975e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001845836314891268,
            "unit": "Nanoseconds",
            "range": 1.4887008628453436e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017938876499437232,
            "unit": "Nanoseconds",
            "range": 8.225972512586021e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008983632250286312,
            "unit": "Nanoseconds",
            "range": 1.6139283130629626e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "ab8d57cf43be912a336e872b68d1a2526c93dc6a",
          "message": "Merge pull request #4216 from IntersectMBO/PR-improve-gov-generator\n\nimprove the `GOV` generator to generate more interesting signals",
          "timestamp": "2024-03-27T12:53:11+01:00",
          "tree_id": "21a6f9e0f95ef2637757cf9be9b4e63cd0a9bfd4",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/ab8d57cf43be912a336e872b68d1a2526c93dc6a"
        },
        "date": 1711541141957,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006666658065314562,
            "unit": "Nanoseconds",
            "range": 5.0487150907388e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007148169460086495,
            "unit": "Nanoseconds",
            "range": 8.27816194896511e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008124129373385476,
            "unit": "Nanoseconds",
            "range": 7.609696689529148e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011405314521033148,
            "unit": "Nanoseconds",
            "range": 0.0000012096779240157114
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010494701211834218,
            "unit": "Nanoseconds",
            "range": 1.0027657941082701e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018517753252179234,
            "unit": "Nanoseconds",
            "range": 1.214864824631218e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001786073788743906,
            "unit": "Nanoseconds",
            "range": 2.5648630318278126e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008909148945220302,
            "unit": "Nanoseconds",
            "range": 7.545667756040075e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e526e148cf9a60fc46c66f7b87bfc469a49811e6",
          "message": "Merge pull request #4219 from IntersectMBO/td/fixes-for-release\n\nFixes for 8.10 release",
          "timestamp": "2024-03-28T22:52:13-06:00",
          "tree_id": "7fd44f6c9549b3b5fc42d55dacaf0efae2f36509",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/e526e148cf9a60fc46c66f7b87bfc469a49811e6"
        },
        "date": 1711688096070,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006507090441073775,
            "unit": "Nanoseconds",
            "range": 4.83271211895433e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007005235880758694,
            "unit": "Nanoseconds",
            "range": 3.294058509272797e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007910847912042781,
            "unit": "Nanoseconds",
            "range": 5.068635107506637e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011214215643249254,
            "unit": "Nanoseconds",
            "range": 0.000004339536419757924
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010026210040505318,
            "unit": "Nanoseconds",
            "range": 1.7591080259791681e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001839912840625238,
            "unit": "Nanoseconds",
            "range": 1.5502586690760906e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001741279417327591,
            "unit": "Nanoseconds",
            "range": 4.820598534645001e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008748925714149199,
            "unit": "Nanoseconds",
            "range": 5.44451052944505e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "2924845c389b0890cd3e72a44ffb7e78cf664427",
          "message": "Merge pull request #4225 from IntersectMBO/td/post-release-updates\n\nPost release updates",
          "timestamp": "2024-03-29T01:10:46-06:00",
          "tree_id": "ce6d3d81a4e11e1455fb4bc0b2080c1f9dfa7692",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/2924845c389b0890cd3e72a44ffb7e78cf664427"
        },
        "date": 1711696409885,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006554336654596692,
            "unit": "Nanoseconds",
            "range": 0.0000018926904473462703
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007017174769698541,
            "unit": "Nanoseconds",
            "range": 5.746128849665314e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007904471805048433,
            "unit": "Nanoseconds",
            "range": 0.0000017232087143386316
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011215602555470504,
            "unit": "Nanoseconds",
            "range": 0.0000021625276044572185
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010609290347712677,
            "unit": "Nanoseconds",
            "range": 3.064615070279334e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018310459148212004,
            "unit": "Nanoseconds",
            "range": 1.5284406047058114e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001759671895735936,
            "unit": "Nanoseconds",
            "range": 1.2093114812257908e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009061213898193449,
            "unit": "Nanoseconds",
            "range": 9.808637367084803e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "6c48c2bccb7eea269e26cab3c718d9ea936c1a50",
          "message": "Merge pull request #4221 from IntersectMBO/neilmayhew/4184-MockChainState-thunks\n\nFix a NoThunks test failure on nightly builds",
          "timestamp": "2024-04-01T17:11:56-06:00",
          "tree_id": "bc53011731bc33be3a0a90cb55456fb0b8b7f13b",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/6c48c2bccb7eea269e26cab3c718d9ea936c1a50"
        },
        "date": 1712013292396,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006679515167849694,
            "unit": "Nanoseconds",
            "range": 0.0000039670570298443305
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007079226814471246,
            "unit": "Nanoseconds",
            "range": 0.0000012131527259079512
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008031528783567327,
            "unit": "Nanoseconds",
            "range": 0.000005737496695539457
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011376829132672171,
            "unit": "Nanoseconds",
            "range": 0.000005564288060069056
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011003718800033258,
            "unit": "Nanoseconds",
            "range": 2.3425243319882458e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018807875081725816,
            "unit": "Nanoseconds",
            "range": 4.2063623271475507e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001835154976615961,
            "unit": "Nanoseconds",
            "range": 0.0000020254542646811856
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009292108459923771,
            "unit": "Nanoseconds",
            "range": 1.2939846244755994e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "97963a4e83a18d51d069bf5535ee4e9de2cb3def",
          "message": "Merge pull request #4214 from IntersectMBO/lucsanszky/fix-estimateminfeetx-underestimation\n\nFix `estimateMinFeeTx` w/ bootstrap test",
          "timestamp": "2024-04-01T19:38:11-06:00",
          "tree_id": "e89e959fc01bd84cb7403af76c4f7ea961af1aa7",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/97963a4e83a18d51d069bf5535ee4e9de2cb3def"
        },
        "date": 1712022068340,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006498560272568178,
            "unit": "Nanoseconds",
            "range": 0.0000010160577015806704
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006995163413242475,
            "unit": "Nanoseconds",
            "range": 0.000003026882980048703
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007862573078484585,
            "unit": "Nanoseconds",
            "range": 7.802773436243512e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011194542764530908,
            "unit": "Nanoseconds",
            "range": 9.18722036811902e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001075450576178592,
            "unit": "Nanoseconds",
            "range": 4.790984975571655e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001838855178438297,
            "unit": "Nanoseconds",
            "range": 3.1657202314296096e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017434124194074644,
            "unit": "Nanoseconds",
            "range": 4.261828834838057e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009064022677963147,
            "unit": "Nanoseconds",
            "range": 9.934578590420736e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "b933089ecc07edb1ce39639efe5f52e4409f0519",
          "message": "Merge pull request #4235 from IntersectMBO/lehins/various-fixes\n\nRevert to full formolu runs on CI",
          "timestamp": "2024-04-03T19:37:39+03:00",
          "tree_id": "5e2b00952b97900d7dcdec8b53b5926058866e6c",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/b933089ecc07edb1ce39639efe5f52e4409f0519"
        },
        "date": 1712162435808,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006686565827902031,
            "unit": "Nanoseconds",
            "range": 0.00000734798574294048
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006987674789024505,
            "unit": "Nanoseconds",
            "range": 8.23170851696855e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007875116002093447,
            "unit": "Nanoseconds",
            "range": 0.000002273956742038911
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011109465429541757,
            "unit": "Nanoseconds",
            "range": 0.000001573332457713689
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001148497484106933,
            "unit": "Nanoseconds",
            "range": 1.8200378711453703e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019701380470483728,
            "unit": "Nanoseconds",
            "range": 4.8931603266028e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019251309998682647,
            "unit": "Nanoseconds",
            "range": 2.902783025498551e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009692901425304488,
            "unit": "Nanoseconds",
            "range": 1.3732679773908334e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "2b816e58cb714061176d888e6d4c8e57bcc848b2",
          "message": "Merge pull request #4236 from IntersectMBO/td/fix-committee-typo\n\nFix typo in `ToJSON` of `ConwayGovState`",
          "timestamp": "2024-04-04T01:10:57+03:00",
          "tree_id": "339bb2b2e53f21267e3c35c799afb4d2506b5872",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/2b816e58cb714061176d888e6d4c8e57bcc848b2"
        },
        "date": 1712182416323,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006479898695486271,
            "unit": "Nanoseconds",
            "range": 0.000005042888484948204
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006934190150922414,
            "unit": "Nanoseconds",
            "range": 0.000003294840672792415
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008045279396252509,
            "unit": "Nanoseconds",
            "range": 0.000010916803425749596
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010719589000757883,
            "unit": "Nanoseconds",
            "range": 0.0000022868198732827386
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010574399919848592,
            "unit": "Nanoseconds",
            "range": 2.2580039328152217e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018440847862624747,
            "unit": "Nanoseconds",
            "range": 2.3794912224229547e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018151126955812477,
            "unit": "Nanoseconds",
            "range": 3.912568485888047e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009184840187402545,
            "unit": "Nanoseconds",
            "range": 2.5297722586788803e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "304c71a0f520c1cc56bc7ad246c8c36532806dab",
          "message": "Merge pull request #4189 from IntersectMBO/td/aniketd/more-imptests-treasury-withdrawals\n\nImptests -  treasury withdrawals",
          "timestamp": "2024-04-04T06:03:53+03:00",
          "tree_id": "61402f1c91ee48606f9cc0ffee253c067f2a7f85",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/304c71a0f520c1cc56bc7ad246c8c36532806dab"
        },
        "date": 1712200002157,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006739736715708725,
            "unit": "Nanoseconds",
            "range": 0.000003974349423730568
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006937604744224865,
            "unit": "Nanoseconds",
            "range": 6.111042742278452e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007823748824500427,
            "unit": "Nanoseconds",
            "range": 4.846311777327256e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011108830501240852,
            "unit": "Nanoseconds",
            "range": 9.61189365735439e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011436929781670782,
            "unit": "Nanoseconds",
            "range": 1.7503725183634444e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019645866071773396,
            "unit": "Nanoseconds",
            "range": 3.222875466380553e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019052642261882458,
            "unit": "Nanoseconds",
            "range": 1.4580468958765678e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009681928657351856,
            "unit": "Nanoseconds",
            "range": 7.087365164155891e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "joosep.jaager@iohk.io",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "6edffa2932f151f7c35dcba3a59a816b72c56d79",
          "message": "Added test for a deserialization bug (#4207)",
          "timestamp": "2024-04-04T14:52:32Z",
          "tree_id": "3ef7107f605c44b6e7f9dc71169ad4084cfad568",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/6edffa2932f151f7c35dcba3a59a816b72c56d79"
        },
        "date": 1712242517730,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006666945214590231,
            "unit": "Nanoseconds",
            "range": 5.238256392024108e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.0000712072071578865,
            "unit": "Nanoseconds",
            "range": 6.647737480292026e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007980065462416496,
            "unit": "Nanoseconds",
            "range": 6.82565946546939e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011350541722019405,
            "unit": "Nanoseconds",
            "range": 9.62066222042347e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011485700904932156,
            "unit": "Nanoseconds",
            "range": 1.3947955475347866e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001975389489629612,
            "unit": "Nanoseconds",
            "range": 1.636750696983098e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019689812142762394,
            "unit": "Nanoseconds",
            "range": 0.0000015002497479881848
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000967384509044624,
            "unit": "Nanoseconds",
            "range": 1.0491781070141552e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "6ab53168ebd44cc0091d0d93cf9909cd073a524f",
          "message": "Merge pull request #4239 from IntersectMBO/neilmayhew/Cabal-syntax\n\nUse the Cabal-syntax package instead of the Cabal package",
          "timestamp": "2024-04-04T11:55:18-06:00",
          "tree_id": "ce5e13236c29975bbb2db30ed411eb1d8704c026",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/6ab53168ebd44cc0091d0d93cf9909cd073a524f"
        },
        "date": 1712253817599,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006633650480683179,
            "unit": "Nanoseconds",
            "range": 0.0000015244352070048832
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007110145654543939,
            "unit": "Nanoseconds",
            "range": 3.352165989268379e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008012862665429707,
            "unit": "Nanoseconds",
            "range": 0.0000017965715379879744
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011478852150396661,
            "unit": "Nanoseconds",
            "range": 0.0000016381527392304275
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011077735393037883,
            "unit": "Nanoseconds",
            "range": 1.01109371256445e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019024152181052522,
            "unit": "Nanoseconds",
            "range": 3.445538136361054e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018059423662126453,
            "unit": "Nanoseconds",
            "range": 1.7297044709023971e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009215007056420002,
            "unit": "Nanoseconds",
            "range": 6.911302499576228e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "neil.mayhew@iohk.io",
            "name": "Neil Mayhew",
            "username": "neilmayhew"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e47a836b45c6ad7384dc9ba270c752284a947a92",
          "message": "Merge pull request #4234 from IntersectMBO/lucsanszky/remove-precommit\n\nAdd separate `devShell` for `pre-commit`",
          "timestamp": "2024-04-04T18:04:20-06:00",
          "tree_id": "6760ef98d83b7aff5d038ff8a9c8f979c5747784",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/e47a836b45c6ad7384dc9ba270c752284a947a92"
        },
        "date": 1712275621980,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006606170095992777,
            "unit": "Nanoseconds",
            "range": 0.000003048044507934229
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006978649644379848,
            "unit": "Nanoseconds",
            "range": 8.445428402091715e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000786672512157938,
            "unit": "Nanoseconds",
            "range": 0.0000010124371685151438
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011234074153404322,
            "unit": "Nanoseconds",
            "range": 0.0000029096856552555876
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010724202575077877,
            "unit": "Nanoseconds",
            "range": 3.952723474529125e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001880566663956377,
            "unit": "Nanoseconds",
            "range": 5.509207666711199e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001792844606734202,
            "unit": "Nanoseconds",
            "range": 4.682841354127876e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009074102442828925,
            "unit": "Nanoseconds",
            "range": 1.112053978368384e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "aniketd@users.noreply.github.com",
            "name": "Aniket Deshpande",
            "username": "aniketd"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "65122e4e188a90d7afa67f2afd82418d6bb9a5a6",
          "message": "Imptests: ParameterChange affects ratification (#4238)\n\nFor DReps and SPOs:\r\n1. Increasing a threshold prevents in-flight proposals that had enough\r\nstake in favour before, to fail ratification.\r\n2. Decreasing a threshold ratifies in-flight proposals that didnt have\r\nenough stake before.",
          "timestamp": "2024-04-08T16:36:04Z",
          "tree_id": "be8218f7519f7c351e3964a6217ff2aa56b96623",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/65122e4e188a90d7afa67f2afd82418d6bb9a5a6"
        },
        "date": 1712594332898,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006654213711120271,
            "unit": "Nanoseconds",
            "range": 0.000002120605492172049
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007040456061948883,
            "unit": "Nanoseconds",
            "range": 6.175630321539167e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007952671033373637,
            "unit": "Nanoseconds",
            "range": 8.413378171017212e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011349981018008867,
            "unit": "Nanoseconds",
            "range": 7.043749308234919e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011515671354565747,
            "unit": "Nanoseconds",
            "range": 0.0000011357818323990033
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019927982259669866,
            "unit": "Nanoseconds",
            "range": 2.7035464824216027e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019915172588447082,
            "unit": "Nanoseconds",
            "range": 0.0000024077647959575775
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000972325192782382,
            "unit": "Nanoseconds",
            "range": 1.1824728358892742e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "88ff0c593a0e0b4167636ba947d68521b3e19e2a",
          "message": "Merge pull request #4246 from IntersectMBO/td/changelog-8.10\n\nChangelog for node release 8.10",
          "timestamp": "2024-04-08T14:59:50-06:00",
          "tree_id": "822285cf1dfd31d47ae0ee74b454d7f6b593c8eb",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/88ff0c593a0e0b4167636ba947d68521b3e19e2a"
        },
        "date": 1712610170109,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.0000670211424170104,
            "unit": "Nanoseconds",
            "range": 0.0000025468265949709242
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007207504162481787,
            "unit": "Nanoseconds",
            "range": 6.327866641819909e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008217951015150658,
            "unit": "Nanoseconds",
            "range": 0.000008851479523255415
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.0001156461062277803,
            "unit": "Nanoseconds",
            "range": 0.000003495772930369694
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011546944923107368,
            "unit": "Nanoseconds",
            "range": 3.4225947541069853e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001965614473224384,
            "unit": "Nanoseconds",
            "range": 3.8308045016164645e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001937131007415846,
            "unit": "Nanoseconds",
            "range": 2.0521757140731243e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009774836217968245,
            "unit": "Nanoseconds",
            "range": 1.421003119031515e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "315e78d1baed5efcd18206f3d5113ffefc81de74",
          "message": "Merge pull request #4243 from IntersectMBO/lucsanszky/convert-small-steps-test\n\nConvert `small-steps` testsuite to `Hspec`",
          "timestamp": "2024-04-08T22:46:14-06:00",
          "tree_id": "15210de292af2c49b33843ab03127f71a707069f",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/315e78d1baed5efcd18206f3d5113ffefc81de74"
        },
        "date": 1712638137888,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006611765255666771,
            "unit": "Nanoseconds",
            "range": 0.000002001874253239584
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006999112867517878,
            "unit": "Nanoseconds",
            "range": 4.817132429258655e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007949154271469511,
            "unit": "Nanoseconds",
            "range": 8.369183960913402e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011282728972301637,
            "unit": "Nanoseconds",
            "range": 0.000001541574865013012
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001146161465379142,
            "unit": "Nanoseconds",
            "range": 2.1143906036608875e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001986136798992785,
            "unit": "Nanoseconds",
            "range": 2.5409179229560413e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001912301236985656,
            "unit": "Nanoseconds",
            "range": 3.67182639588018e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000974122056183088,
            "unit": "Nanoseconds",
            "range": 3.2491019971492e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "kwxm@inf.ed.ac.uk",
            "name": "Kenneth MacKenzie",
            "username": "kwxm"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "3acfea9b8662b32a7fd134064eb7ec5e80451767",
          "message": "Plutus 1.25.0.0 (#4245)\n\n* Plutus 1.25.0.0\r\n\r\n* Replace `fromList` with `unsafeFromList`\r\n\r\n`fromList` was deprecated in `1.24.0.0` by\r\nhttps://github.com/IntersectMBO/plutus/pull/5838\r\n\r\n* Bump patch versions\r\n\r\n---------\r\n\r\nCo-authored-by: Lucsanszky <daniel.lucsanszky@iohk.io>\r\nCo-authored-by: Lucsanszky <Lucsanszky@users.noreply.github.com>",
          "timestamp": "2024-04-09T15:30:11Z",
          "tree_id": "141a55cc4d07e2e4d587502d79436ef1379ac633",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/3acfea9b8662b32a7fd134064eb7ec5e80451767"
        },
        "date": 1712676773172,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006462049332176228,
            "unit": "Nanoseconds",
            "range": 5.449056372445038e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.0000699532355459099,
            "unit": "Nanoseconds",
            "range": 6.246609531297797e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007917888819666931,
            "unit": "Nanoseconds",
            "range": 0.0000016293420474942896
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011092749651531383,
            "unit": "Nanoseconds",
            "range": 0.000001779077185290503
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010946063647021627,
            "unit": "Nanoseconds",
            "range": 2.642391837788622e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018776722790808794,
            "unit": "Nanoseconds",
            "range": 1.8750254861451217e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.0000180619211463177,
            "unit": "Nanoseconds",
            "range": 1.221089695492715e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008767381781024273,
            "unit": "Nanoseconds",
            "range": 9.270202529696367e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "b31635f1b03d58ca9b4ad3076d01b226015200a4",
          "message": "Merge pull request #4248 from IntersectMBO/td/fix-treasury-test\n\nFix withdrawals test data generation in `EnactSpec`",
          "timestamp": "2024-04-09T12:51:34-06:00",
          "tree_id": "567888da22199da022ba043e9ab068a1e7538dc8",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/b31635f1b03d58ca9b4ad3076d01b226015200a4"
        },
        "date": 1712688877647,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006779510820401301,
            "unit": "Nanoseconds",
            "range": 0.000002516250091776805
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007365446167497491,
            "unit": "Nanoseconds",
            "range": 0.0000012508813839092616
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008414597270700731,
            "unit": "Nanoseconds",
            "range": 0.0000015958350012043188
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011578217873278473,
            "unit": "Nanoseconds",
            "range": 0.0000031146166339311203
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010980967043879874,
            "unit": "Nanoseconds",
            "range": 1.9212083738372538e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019488924745421592,
            "unit": "Nanoseconds",
            "range": 3.695605070099871e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018892345306406832,
            "unit": "Nanoseconds",
            "range": 0.000001106658695368632
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009217171533697716,
            "unit": "Nanoseconds",
            "range": 1.969659051493443e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d17deb97717121f9d3c5e731ae1ebd62e449f985",
          "message": "Merge pull request #4249 from IntersectMBO/lehins/update-tooling-and-fixups\n\nUpdate tooling and minor fixups",
          "timestamp": "2024-04-09T18:13:32-06:00",
          "tree_id": "e497580b41b21a44eb7104497964766f84e709c6",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/d17deb97717121f9d3c5e731ae1ebd62e449f985"
        },
        "date": 1712708174028,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006544073086519906,
            "unit": "Nanoseconds",
            "range": 0.000003001991747797217
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007084501020897858,
            "unit": "Nanoseconds",
            "range": 5.667306476828792e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007988229095476911,
            "unit": "Nanoseconds",
            "range": 0.0000013304150010719388
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011182408507083656,
            "unit": "Nanoseconds",
            "range": 0.0000017363429787361682
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010611939194094299,
            "unit": "Nanoseconds",
            "range": 4.639342028517165e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018848508437448844,
            "unit": "Nanoseconds",
            "range": 1.0974222526854278e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001790947215903811,
            "unit": "Nanoseconds",
            "range": 1.1492012406088597e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009054488925241043,
            "unit": "Nanoseconds",
            "range": 2.3668326447964814e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "joosep.jaager@iohk.io",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "72f2f174de0378cac0356a967540793d91c80ff3",
          "message": "Update and reorganize conformance tests (#4212)\n\n* Updated conformance tests\r\n\r\n* Moved Conway constrained specs to cardano-ledger-conway:testlib\r\n\r\n* Fixed infinite loop\r\n\r\n* Added additional constraints to UTXO generators\r\n\r\n* Exported functions from Conway.Constrained.Instances\r\n\r\n* Bump cardano-ledger-executable-spec\r\n\r\n* Add SpecTranslate instances to run GOV rule\r\n\r\n* Moved conformance modules back into cardano-ledger-conformance\r\n\r\n* Moved constrained specs back to cardano-ledger-test",
          "timestamp": "2024-04-10T15:24:05Z",
          "tree_id": "8f7e5e763af6ee587214963870d20ac105870e1c",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/72f2f174de0378cac0356a967540793d91c80ff3"
        },
        "date": 1712762811539,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006441276041296643,
            "unit": "Nanoseconds",
            "range": 0.000002306167571979563
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006996928434411394,
            "unit": "Nanoseconds",
            "range": 0.000001745253741968615
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008181042098433308,
            "unit": "Nanoseconds",
            "range": 0.000006807293503738121
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011153377260953527,
            "unit": "Nanoseconds",
            "range": 0.000005885352430241849
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010742441993624885,
            "unit": "Nanoseconds",
            "range": 1.437310654568266e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001907822938235061,
            "unit": "Nanoseconds",
            "range": 2.941686321529839e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001833526820688017,
            "unit": "Nanoseconds",
            "range": 0.0000017906507134126612
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008989781561408858,
            "unit": "Nanoseconds",
            "range": 9.953324412137586e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "neil.mayhew@iohk.io",
            "name": "Neil Mayhew",
            "username": "neilmayhew"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "002b5777e9cfdfa46c31fb21f0f4388eb0fb4756",
          "message": "Merge pull request #4250 from IntersectMBO/neilmayhew/3418-node-orphan-instances\n\nAdd some ToJSON instances needed by cardano-node",
          "timestamp": "2024-04-10T12:15:41-06:00",
          "tree_id": "c745a6021291746ae69745f955b6e622130e2f35",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/002b5777e9cfdfa46c31fb21f0f4388eb0fb4756"
        },
        "date": 1712773370815,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.0000654252886443784,
            "unit": "Nanoseconds",
            "range": 0.0000026553653343041634
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.0000684935280492078,
            "unit": "Nanoseconds",
            "range": 0.0000011014355834054636
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007641354180893511,
            "unit": "Nanoseconds",
            "range": 3.5965569973814754e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011246227818630447,
            "unit": "Nanoseconds",
            "range": 0.0000020861255996704728
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010803861349099054,
            "unit": "Nanoseconds",
            "range": 1.3490603495810445e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018712453115620938,
            "unit": "Nanoseconds",
            "range": 1.8387674894932884e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017661476041928962,
            "unit": "Nanoseconds",
            "range": 1.5284353877021813e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009011329409982316,
            "unit": "Nanoseconds",
            "range": 8.724560700994761e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "191a0fef46db14ad8fd2dec1e6909c57bda68e2f",
          "message": "Merge pull request #4242 from IntersectMBO/ts-unitTestTools-version2\n\nAdded UnitTestTools and IncrementalStakeTest",
          "timestamp": "2024-04-10T14:37:19-06:00",
          "tree_id": "2298bc6546e49ea27cec27165fbed7b9168a00b5",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/191a0fef46db14ad8fd2dec1e6909c57bda68e2f"
        },
        "date": 1712781609534,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006562390601690024,
            "unit": "Nanoseconds",
            "range": 9.317787472905441e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007020082558963523,
            "unit": "Nanoseconds",
            "range": 0.000001419258363843172
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007888457088323356,
            "unit": "Nanoseconds",
            "range": 0.000001987558955305132
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011271124882631202,
            "unit": "Nanoseconds",
            "range": 9.432934544258344e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011261378054179328,
            "unit": "Nanoseconds",
            "range": 1.7134822156991057e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019520197367200124,
            "unit": "Nanoseconds",
            "range": 6.379366595711409e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001857193437247073,
            "unit": "Nanoseconds",
            "range": 1.647265586472025e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009352976685636021,
            "unit": "Nanoseconds",
            "range": 1.0100791742133459e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "neil.mayhew@iohk.io",
            "name": "Neil Mayhew",
            "username": "neilmayhew"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "dbce4e4a3ad508b2bfd2df897b617c19b7f049de",
          "message": "Merge pull request #4257 from IntersectMBO/neilmayhew/3582-haddock-hidden-modules\n\nStop generating Haddocks for internal modules",
          "timestamp": "2024-04-10T18:53:30-06:00",
          "tree_id": "370a94a42bb913a9d6114c88c5c0a6627674d0eb",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/dbce4e4a3ad508b2bfd2df897b617c19b7f049de"
        },
        "date": 1712796977270,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.0000655463605003384,
            "unit": "Nanoseconds",
            "range": 3.579130819818428e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006942876799927956,
            "unit": "Nanoseconds",
            "range": 4.2502538086698964e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007737831509611606,
            "unit": "Nanoseconds",
            "range": 4.3446317730479064e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011179521555706659,
            "unit": "Nanoseconds",
            "range": 0.0000016091614615987755
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001069208409251491,
            "unit": "Nanoseconds",
            "range": 1.1222768041878875e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018563858893082922,
            "unit": "Nanoseconds",
            "range": 1.8158910069192437e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001815139232147081,
            "unit": "Nanoseconds",
            "range": 4.6238304734734055e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000893382796262722,
            "unit": "Nanoseconds",
            "range": 8.848296791294182e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0adbd6a5509cdace231b4134319ecd7323305a50",
          "message": "Merge pull request #4241 from IntersectMBO/td/imp-post-fixup-hook\n\nAdd fixup combinators to ImpTest framework",
          "timestamp": "2024-04-11T03:33:21-06:00",
          "tree_id": "5939eece26797152a665f30dd46a0660120b4198",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/0adbd6a5509cdace231b4134319ecd7323305a50"
        },
        "date": 1712828164147,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006613335542298787,
            "unit": "Nanoseconds",
            "range": 3.97163654286005e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006935912251468462,
            "unit": "Nanoseconds",
            "range": 8.72079252415012e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007763226306666141,
            "unit": "Nanoseconds",
            "range": 4.758338345817475e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011335274171579101,
            "unit": "Nanoseconds",
            "range": 0.000004692881222561573
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010943606142325709,
            "unit": "Nanoseconds",
            "range": 2.9019742256054243e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019219903243660767,
            "unit": "Nanoseconds",
            "range": 9.828121466687473e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018063254953910038,
            "unit": "Nanoseconds",
            "range": 1.1479273838111104e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009143000481346051,
            "unit": "Nanoseconds",
            "range": 1.4365465992245142e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "9589f2c040fd9367c012eb68669a3f1ea07e031c",
          "message": "Merge pull request #4229 from IntersectMBO/PR-shrinking\n\nShrinking for `constrained-generators`",
          "timestamp": "2024-04-11T14:10:19+02:00",
          "tree_id": "3bccd0625b5b79aec467c061868a10d1ae759a95",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/9589f2c040fd9367c012eb68669a3f1ea07e031c"
        },
        "date": 1712837585963,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006938385753285333,
            "unit": "Nanoseconds",
            "range": 0.000005132224347945145
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007332059437728117,
            "unit": "Nanoseconds",
            "range": 0.000001110046262034023
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008199821193657332,
            "unit": "Nanoseconds",
            "range": 0.0000015679790865496916
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011465310989629229,
            "unit": "Nanoseconds",
            "range": 0.0000030911012475832647
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000012098841047718469,
            "unit": "Nanoseconds",
            "range": 4.425517340206883e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000020558052581199806,
            "unit": "Nanoseconds",
            "range": 9.742225605465294e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019344083176116558,
            "unit": "Nanoseconds",
            "range": 6.86080805647482e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009953377204975418,
            "unit": "Nanoseconds",
            "range": 1.5329584052758432e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "7ade3802c70193e59e92e398323f71a44493eb07",
          "message": "Merge pull request #4260 from IntersectMBO/dependabot/pip/doc/idna-3.7\n\nBump idna from 3.3 to 3.7 in /doc",
          "timestamp": "2024-04-11T22:14:51-06:00",
          "tree_id": "00591788cc58db2444cbcee60f240b728a44f510",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/7ade3802c70193e59e92e398323f71a44493eb07"
        },
        "date": 1712895451203,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006704590728716894,
            "unit": "Nanoseconds",
            "range": 0.0000019412115748962257
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007177588704598274,
            "unit": "Nanoseconds",
            "range": 0.0000013529807925760563
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007936770922276786,
            "unit": "Nanoseconds",
            "range": 0.0000013556135744818934
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011340143094808713,
            "unit": "Nanoseconds",
            "range": 0.0000020791705631584036
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011968805890428387,
            "unit": "Nanoseconds",
            "range": 6.301669695975204e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00002007327075350848,
            "unit": "Nanoseconds",
            "range": 2.439943210487222e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00002017019516294363,
            "unit": "Nanoseconds",
            "range": 8.946348365541087e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009840322896073695,
            "unit": "Nanoseconds",
            "range": 1.251811133587469e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "8a99c2d773a38fd4e52ac462bcec817ee0cfb057",
          "message": "Merge pull request #4259 from IntersectMBO/undeprecate-redeemerPointer\n\nUndeprecate redeemerPointer and expose it in cardano-ledger-api",
          "timestamp": "2024-04-12T05:15:26-06:00",
          "tree_id": "ad8fb7e913c970be6abcafb175d3befe99213812",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/8a99c2d773a38fd4e52ac462bcec817ee0cfb057"
        },
        "date": 1712920693057,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006729841515491661,
            "unit": "Nanoseconds",
            "range": 0.000006813949224804655
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007117777161194086,
            "unit": "Nanoseconds",
            "range": 9.506380958155494e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008125556644747286,
            "unit": "Nanoseconds",
            "range": 0.0000014067106313548222
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011353489840385492,
            "unit": "Nanoseconds",
            "range": 0.0000018633816087436488
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011836709777905227,
            "unit": "Nanoseconds",
            "range": 1.6628357539363217e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019971963363540364,
            "unit": "Nanoseconds",
            "range": 1.7867401407918797e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001954076010173698,
            "unit": "Nanoseconds",
            "range": 2.1390483611127006e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009898438374352849,
            "unit": "Nanoseconds",
            "range": 1.4712621813555252e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "joosep.jaager@iohk.io",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "2c9ba10d085331fa99840a63b40b8e8b716fba5d",
          "message": "Change the balance in `InsufficientCollateral` to `DeltaCoin` (#4247)\n\n* Added test for a deserialization bug\r\n\r\n* Fixed the predicate failure deserialization bug",
          "timestamp": "2024-04-12T17:57:32Z",
          "tree_id": "9feb54367a530cf1fe55274b11dff4f01dba93bf",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/2c9ba10d085331fa99840a63b40b8e8b716fba5d"
        },
        "date": 1712944817965,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006633074226841298,
            "unit": "Nanoseconds",
            "range": 0.0000020372948014073225
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007127284311427652,
            "unit": "Nanoseconds",
            "range": 5.328014114755026e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007963690341494089,
            "unit": "Nanoseconds",
            "range": 7.844755550919365e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011119288312355356,
            "unit": "Nanoseconds",
            "range": 8.50569485266239e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001095957268674981,
            "unit": "Nanoseconds",
            "range": 0.0000013218532411429337
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018630488191603804,
            "unit": "Nanoseconds",
            "range": 9.798767949336866e-8
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018501371527023574,
            "unit": "Nanoseconds",
            "range": 1.857790375950558e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009106306066383708,
            "unit": "Nanoseconds",
            "range": 1.0566223974274815e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "aniketd@users.noreply.github.com",
            "name": "Aniket Deshpande",
            "username": "aniketd"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "8193b4ae7f45da28515d612175f65634440af97b",
          "message": "Imptests: CommitteeMinSize affects in-flight props (#4244)\n\n* Imptests: CommitteeMinSize affects in-flight props",
          "timestamp": "2024-04-16T09:18:48Z",
          "tree_id": "53f46070e474829277acf639e3e2aad42e7f9a4a",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/8193b4ae7f45da28515d612175f65634440af97b"
        },
        "date": 1713259290668,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006650383605982059,
            "unit": "Nanoseconds",
            "range": 0.000003921228067855194
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007129936112088571,
            "unit": "Nanoseconds",
            "range": 4.4714449866620134e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007913807364355497,
            "unit": "Nanoseconds",
            "range": 0.0000012283005013831505
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011250559008975505,
            "unit": "Nanoseconds",
            "range": 0.0000019181530043303673
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010897846021344702,
            "unit": "Nanoseconds",
            "range": 1.288413741104562e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018943182783435663,
            "unit": "Nanoseconds",
            "range": 2.2539845464178762e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018237754851334893,
            "unit": "Nanoseconds",
            "range": 1.1124593057538297e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009242245124398985,
            "unit": "Nanoseconds",
            "range": 1.2916884237198616e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "aniketd@users.noreply.github.com",
            "name": "Aniket Deshpande",
            "username": "aniketd"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "57bb4c1e66fa9336a4960bdc716f96fbc11c74d6",
          "message": "Add PlutusV3 CostModel to UpgradeConwayPParams (#4252)\n\nUse `updateCostModels` to add them to Conway\r\nPParams during tranlation/upgrade from Babbage",
          "timestamp": "2024-04-16T13:02:43Z",
          "tree_id": "c1099efd8637b08c0bee936b62289bd696bfec2e",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/57bb4c1e66fa9336a4960bdc716f96fbc11c74d6"
        },
        "date": 1713272731904,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006818672234553819,
            "unit": "Nanoseconds",
            "range": 0.000003273076934252015
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007238308092699819,
            "unit": "Nanoseconds",
            "range": 8.200674979327604e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008188895858739335,
            "unit": "Nanoseconds",
            "range": 5.509155665726717e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011489372298352839,
            "unit": "Nanoseconds",
            "range": 0.0000013025909017151744
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011222222324439437,
            "unit": "Nanoseconds",
            "range": 1.7577752638858264e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019471685256942726,
            "unit": "Nanoseconds",
            "range": 1.6863401666729028e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019023874911728512,
            "unit": "Nanoseconds",
            "range": 0.0000014458785454613483
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009668050281930296,
            "unit": "Nanoseconds",
            "range": 9.016591701055394e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "a11c2ca88dc19a71a8f9cab26e81547c87465a84",
          "message": "Merge pull request #4269 from IntersectMBO/PR-fix-map-sum-bug\n\nFix generation bug for sums of positive member spec",
          "timestamp": "2024-04-16T18:01:59+02:00",
          "tree_id": "6fc3a85d1fb0264830507261cb72b1e0373d188d",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/a11c2ca88dc19a71a8f9cab26e81547c87465a84"
        },
        "date": 1713283492223,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006792789223294881,
            "unit": "Nanoseconds",
            "range": 0.0000029276109458538176
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007184721060649586,
            "unit": "Nanoseconds",
            "range": 5.084279900219015e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008063090072858829,
            "unit": "Nanoseconds",
            "range": 6.111054145173071e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011378753301927989,
            "unit": "Nanoseconds",
            "range": 0.0000011174539306180726
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000012054183683271106,
            "unit": "Nanoseconds",
            "range": 2.549521236845579e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000020015223596966884,
            "unit": "Nanoseconds",
            "range": 4.3139721083505474e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001987031549243315,
            "unit": "Nanoseconds",
            "range": 6.729153025935344e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009806428422608256,
            "unit": "Nanoseconds",
            "range": 1.6899491974712007e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "1b9466fc2eba351bcef1a91d46b573ce058ece76",
          "message": "Merge pull request #4267 from IntersectMBO/lehins/expand-TxAuxData-interface\n\nExpand TxAuxData interface",
          "timestamp": "2024-04-16T15:47:50-06:00",
          "tree_id": "75add1a1b816e714fcecf123db1fb5143acf3f8a",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/1b9466fc2eba351bcef1a91d46b573ce058ece76"
        },
        "date": 1713304250111,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.0000672731815847863,
            "unit": "Nanoseconds",
            "range": 5.713781383919905e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007318539642290348,
            "unit": "Nanoseconds",
            "range": 0.0000059531403843322146
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008321556248717901,
            "unit": "Nanoseconds",
            "range": 0.000006882054818961481
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011586079334107071,
            "unit": "Nanoseconds",
            "range": 0.0000034817086953753664
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011645343048334845,
            "unit": "Nanoseconds",
            "range": 1.1912837757122471e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019934827297892673,
            "unit": "Nanoseconds",
            "range": 8.623265130527034e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019591095400845148,
            "unit": "Nanoseconds",
            "range": 5.248416332546136e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000010195940601657868,
            "unit": "Nanoseconds",
            "range": 1.0300409563900898e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "aniketd@users.noreply.github.com",
            "name": "Aniket Deshpande",
            "username": "aniketd"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "208819bc3ad8922ad387f85130d3ff2f5f105662",
          "message": "Add imptest to propose and enact unknown costmodels (#4266)",
          "timestamp": "2024-04-16T18:51:47-06:00",
          "tree_id": "7571d9d542542e9c2201ce5a00e25bea9f8df6f2",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/208819bc3ad8922ad387f85130d3ff2f5f105662"
        },
        "date": 1713315290041,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006912727507960092,
            "unit": "Nanoseconds",
            "range": 0.00000815264245337153
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007209850150229566,
            "unit": "Nanoseconds",
            "range": 0.000003338976909047295
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008045644704478132,
            "unit": "Nanoseconds",
            "range": 0.0000011233560906529507
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011534464504308074,
            "unit": "Nanoseconds",
            "range": 0.000003233449596770451
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001156698522963062,
            "unit": "Nanoseconds",
            "range": 1.1569067573841501e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001989663309627,
            "unit": "Nanoseconds",
            "range": 3.6208671928630443e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001959520799746081,
            "unit": "Nanoseconds",
            "range": 8.64328743419656e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00001022344771797358,
            "unit": "Nanoseconds",
            "range": 1.7351696792403654e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d04f0f406077dab2fa4772c8f6231c02aca2bbe0",
          "message": "Merge pull request #4261 from IntersectMBO/PR-constrained-cabal-cleanup\n\n`constrained-generators` cleanup for hackage",
          "timestamp": "2024-04-17T15:00:48+02:00",
          "tree_id": "c4f9e565734638ee73be430212807ce8a409c4e5",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/d04f0f406077dab2fa4772c8f6231c02aca2bbe0"
        },
        "date": 1713359018262,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006765285042655071,
            "unit": "Nanoseconds",
            "range": 7.55510460191775e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007186053357976154,
            "unit": "Nanoseconds",
            "range": 7.104797253938228e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008152341117017981,
            "unit": "Nanoseconds",
            "range": 0.0000023706440771814047
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011382061735176211,
            "unit": "Nanoseconds",
            "range": 0.0000014365695085300792
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000012159516654171756,
            "unit": "Nanoseconds",
            "range": 1.515083772434054e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00002022238454014948,
            "unit": "Nanoseconds",
            "range": 4.5216145263993553e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019951050690684278,
            "unit": "Nanoseconds",
            "range": 2.527671567388712e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00001002375654773092,
            "unit": "Nanoseconds",
            "range": 9.565491518922056e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "6db4d47997723d26b03dffbabeb7e9ccd3c1ee78",
          "message": "Merge pull request #4265 from IntersectMBO/lucsanszky/stop-embedding-predfailures\n\nInline UTxO and UTxOW `PredFailure` for `Conway`",
          "timestamp": "2024-04-18T20:24:44-06:00",
          "tree_id": "60059c4d3215c0f2f1e50be2a6648d524e630bdf",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/6db4d47997723d26b03dffbabeb7e9ccd3c1ee78"
        },
        "date": 1713493652181,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006636245870911602,
            "unit": "Nanoseconds",
            "range": 0.0000030094489533181345
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007013721310398392,
            "unit": "Nanoseconds",
            "range": 3.7000541284906995e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008004655105682234,
            "unit": "Nanoseconds",
            "range": 0.0000010280805287070038
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011299958332559019,
            "unit": "Nanoseconds",
            "range": 0.0000010331744128107774
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001202489513825315,
            "unit": "Nanoseconds",
            "range": 1.7145459350539844e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019992893987383644,
            "unit": "Nanoseconds",
            "range": 1.9821040270085965e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019378801929180514,
            "unit": "Nanoseconds",
            "range": 2.3918813916619796e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009838012803679618,
            "unit": "Nanoseconds",
            "range": 1.386154205038898e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "086c1aac1b32e58492f7259d7b6a9e9be569d517",
          "message": "Merge pull request #4279 from IntersectMBO/PR-fix-map-toPred\n\n`constrained-generators`: Fix bug in toPreds for maps + add additional tests",
          "timestamp": "2024-04-19T16:41:45+02:00",
          "tree_id": "e266858b3f4c9c5b4d7736b07d821ac420483922",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/086c1aac1b32e58492f7259d7b6a9e9be569d517"
        },
        "date": 1713537873481,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006914295006006444,
            "unit": "Nanoseconds",
            "range": 0.0000034009373515032824
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007038187972487163,
            "unit": "Nanoseconds",
            "range": 6.905553411631137e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007903806870233184,
            "unit": "Nanoseconds",
            "range": 0.0000011193047407260371
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011372432646588232,
            "unit": "Nanoseconds",
            "range": 0.0000010144080322929183
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010718611492945465,
            "unit": "Nanoseconds",
            "range": 1.7032986061221393e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001885820434071454,
            "unit": "Nanoseconds",
            "range": 3.061186511438293e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001815438909519502,
            "unit": "Nanoseconds",
            "range": 2.184152108652556e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009032932778106148,
            "unit": "Nanoseconds",
            "range": 5.995361732607463e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "f0d090c61857376ffe6b0ecdb2301aa2d6e0f150",
          "message": "Merge pull request #4272 from IntersectMBO/PR-foldMap-simplify\n\nsimplify foldMap interface to higher order syntax",
          "timestamp": "2024-04-19T18:59:28+02:00",
          "tree_id": "7dc430dae6967f9a4ce7a6ad5d35fef5c2a4bba2",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/f0d090c61857376ffe6b0ecdb2301aa2d6e0f150"
        },
        "date": 1713546129199,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006448296407673783,
            "unit": "Nanoseconds",
            "range": 3.2673539115076115e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006958747292786238,
            "unit": "Nanoseconds",
            "range": 3.211986453184174e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007789246945899532,
            "unit": "Nanoseconds",
            "range": 5.555405962306776e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.0001116692030979935,
            "unit": "Nanoseconds",
            "range": 0.00000351323833705517
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010784242886908598,
            "unit": "Nanoseconds",
            "range": 2.0358679422555622e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019270731604683722,
            "unit": "Nanoseconds",
            "range": 6.736700744354995e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018500455966392084,
            "unit": "Nanoseconds",
            "range": 2.482429775602337e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008894217910862254,
            "unit": "Nanoseconds",
            "range": 7.777863589732513e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "joosep.jaager@iohk.io",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "committer": {
            "email": "joosep.jaager@gmail.com",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "distinct": true,
          "id": "772e12491bd2bbb9c13f5138d9cd5a966c4ad7e3",
          "message": "Fixed formatting in `HowToProfileLedger.md`",
          "timestamp": "2024-04-19T19:30:46Z",
          "tree_id": "4b7a0e17ea118a8b65a809a61e8a1298a16f69c8",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/772e12491bd2bbb9c13f5138d9cd5a966c4ad7e3"
        },
        "date": 1713555211564,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006860620246095004,
            "unit": "Nanoseconds",
            "range": 0.000006019392646072084
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006991197499485547,
            "unit": "Nanoseconds",
            "range": 9.264150369858008e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.000078802812697116,
            "unit": "Nanoseconds",
            "range": 0.0000015461226530565088
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011214511082488667,
            "unit": "Nanoseconds",
            "range": 0.0000010869121929704009
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001090993044339889,
            "unit": "Nanoseconds",
            "range": 1.4518315112224766e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001915712780592071,
            "unit": "Nanoseconds",
            "range": 7.156629052888191e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018519850939516357,
            "unit": "Nanoseconds",
            "range": 2.611561875750214e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009111805309718307,
            "unit": "Nanoseconds",
            "range": 1.0465330489762445e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "1e10022423d54762ed3949bf463e374079e9f859",
          "message": "add Tim's test to test suite (#4283)",
          "timestamp": "2024-04-19T21:53:04Z",
          "tree_id": "17e57f732e931a83deed6ae1b428937d6a8261d7",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/1e10022423d54762ed3949bf463e374079e9f859"
        },
        "date": 1713563747726,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006648269440459653,
            "unit": "Nanoseconds",
            "range": 6.21567938120058e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007043740695135704,
            "unit": "Nanoseconds",
            "range": 9.359159367408876e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000792726234508639,
            "unit": "Nanoseconds",
            "range": 0.0000015063337195095296
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011197054392610931,
            "unit": "Nanoseconds",
            "range": 0.0000012769443633308836
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010625986341752896,
            "unit": "Nanoseconds",
            "range": 1.1363206478371792e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001887570434091842,
            "unit": "Nanoseconds",
            "range": 2.2309456964869423e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017742950834749644,
            "unit": "Nanoseconds",
            "range": 2.3549504267656966e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000885971343245462,
            "unit": "Nanoseconds",
            "range": 1.3990890838609204e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "f52d1f2e9cc7a3d7024dccebb4e25b914c6464fd",
          "message": "Merge pull request #4286 from IntersectMBO/PR-fix-reify\n\n`constrained-generators`: refactor `reify` to reduce the number of binding sites + delay simplification more to avoid variable capture in higher order syntax",
          "timestamp": "2024-04-23T10:13:25+02:00",
          "tree_id": "c4fcca688bb65b7337055aa21e628a937eeac162",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/f52d1f2e9cc7a3d7024dccebb4e25b914c6464fd"
        },
        "date": 1713860168704,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006554358584455918,
            "unit": "Nanoseconds",
            "range": 0.0000031521847574333198
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006940166219896298,
            "unit": "Nanoseconds",
            "range": 0.0000015953549909776001
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007821485490076011,
            "unit": "Nanoseconds",
            "range": 5.608554939198636e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011260565987680861,
            "unit": "Nanoseconds",
            "range": 0.0000016396730717999767
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010330246462050285,
            "unit": "Nanoseconds",
            "range": 1.1556200655313768e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001840097115910622,
            "unit": "Nanoseconds",
            "range": 2.7708911823763036e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017399170373921044,
            "unit": "Nanoseconds",
            "range": 1.5853752003725123e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000892318618988437,
            "unit": "Nanoseconds",
            "range": 1.6838469527220572e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "9a81c0563251e7b468cb53da45fb17fb2db9038a",
          "message": "Merge pull request #4281 from IntersectMBO/aniketd/count-ccsize\n\nDiscount expired CC from CC-size calculation",
          "timestamp": "2024-04-23T18:17:12-06:00",
          "tree_id": "9c62c0bdb0833abb66ea8207a431238f4938aef9",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/9a81c0563251e7b468cb53da45fb17fb2db9038a"
        },
        "date": 1713918003141,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006651366705322649,
            "unit": "Nanoseconds",
            "range": 0.0000012503262489187085
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007025907274819464,
            "unit": "Nanoseconds",
            "range": 4.476981100744804e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000795046089124129,
            "unit": "Nanoseconds",
            "range": 5.457862275783733e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011316088582892834,
            "unit": "Nanoseconds",
            "range": 5.87212685926256e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010887435727312217,
            "unit": "Nanoseconds",
            "range": 0.0000014049766847884683
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018459452717800623,
            "unit": "Nanoseconds",
            "range": 1.625525197801946e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017870144937308445,
            "unit": "Nanoseconds",
            "range": 0.0000010837411606179843
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008783177259902004,
            "unit": "Nanoseconds",
            "range": 1.1056725774538193e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "7cc1d7bb2003a79c5f134f54b894f3f5280f9390",
          "message": "Merge pull request #4290 from IntersectMBO/lucsanszky/conway-nothunks-utxopredfailures\n\nAdd `NoThunks` instance for UTxO pred failures",
          "timestamp": "2024-04-23T23:01:12-06:00",
          "tree_id": "cef82b4250737aeb816fc20c5f60b1b5181590c1",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/7cc1d7bb2003a79c5f134f54b894f3f5280f9390"
        },
        "date": 1713935033916,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006560491463092729,
            "unit": "Nanoseconds",
            "range": 3.7603234614017363e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006922053000089389,
            "unit": "Nanoseconds",
            "range": 5.649774127146713e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007872310353285399,
            "unit": "Nanoseconds",
            "range": 4.855548301969342e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011404488542176402,
            "unit": "Nanoseconds",
            "range": 6.983719201084943e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010770694223311985,
            "unit": "Nanoseconds",
            "range": 6.734930599493865e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018301991347104755,
            "unit": "Nanoseconds",
            "range": 3.924790995482329e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017772177001025753,
            "unit": "Nanoseconds",
            "range": 7.271676761642e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008860386981778655,
            "unit": "Nanoseconds",
            "range": 9.910938171050988e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "45069775+ana-pantilie@users.noreply.github.com",
            "name": "Ana Pantilie",
            "username": "ana-pantilie"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "b2545891eb7b057fe486a6616484400ab2a102f9",
          "message": "Bump plutus deps to 1.26 (#4282)\n\n* Bump plutus deps to 1.26\r\n\r\n* Update cost model param names\r\n\r\nSigned-off-by: Ana Pantilie <ana.pantilie95@gmail.com>",
          "timestamp": "2024-04-24T15:57:29Z",
          "tree_id": "59ad63235840be09a04fad5656f8bab22dc2ba3f",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/b2545891eb7b057fe486a6616484400ab2a102f9"
        },
        "date": 1713974421220,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006591225956388506,
            "unit": "Nanoseconds",
            "range": 0.0000024548658816033477
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007062567421664151,
            "unit": "Nanoseconds",
            "range": 3.645189767033505e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008038265725376696,
            "unit": "Nanoseconds",
            "range": 0.0000011155350654792466
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011557000789509378,
            "unit": "Nanoseconds",
            "range": 0.0000011542958220092892
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011543662107301345,
            "unit": "Nanoseconds",
            "range": 1.587849176631421e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019861399953405618,
            "unit": "Nanoseconds",
            "range": 4.841566578496068e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001938780754249933,
            "unit": "Nanoseconds",
            "range": 1.6342015394575942e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000998567401757565,
            "unit": "Nanoseconds",
            "range": 1.7813373162224285e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "neil.mayhew@iohk.io",
            "name": "Neil Mayhew",
            "username": "neilmayhew"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "3ce23865480b3e2bf69d286775dc388d7b465d4b",
          "message": "Merge pull request #4294 from IntersectMBO/neilmayhew/ci-concurrency-groups\n\nAvoid cancelling scheduled CI when a new merge happens on master",
          "timestamp": "2024-04-24T15:01:53-06:00",
          "tree_id": "c584f92524ada4323e08b55c46917ff3955a066e",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/3ce23865480b3e2bf69d286775dc388d7b465d4b"
        },
        "date": 1713992682468,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006654728146742766,
            "unit": "Nanoseconds",
            "range": 7.030705502807247e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007307784945608324,
            "unit": "Nanoseconds",
            "range": 0.0000016498357846384188
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008209670315186953,
            "unit": "Nanoseconds",
            "range": 7.524112492821712e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011606074707908586,
            "unit": "Nanoseconds",
            "range": 8.345018851355253e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001171829692059492,
            "unit": "Nanoseconds",
            "range": 2.858173628800878e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000020561310879991084,
            "unit": "Nanoseconds",
            "range": 2.9902113553479344e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019670113154434254,
            "unit": "Nanoseconds",
            "range": 3.9934297960011584e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009962919596444366,
            "unit": "Nanoseconds",
            "range": 1.3285330210629198e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "5e009516cfbc7aac135f5c788f2781f89d9445a7",
          "message": "Merge pull request #4288 from IntersectMBO/lehins/fix-burning-tokens-predicate-failure\n\nFix burning tokens predicate failure",
          "timestamp": "2024-04-24T17:34:05-06:00",
          "tree_id": "df207cd6f79ccd850e99aac5c5a432edd7583c8f",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/5e009516cfbc7aac135f5c788f2781f89d9445a7"
        },
        "date": 1714001814883,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.000067754889181194,
            "unit": "Nanoseconds",
            "range": 0.0000058757920429629234
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007108762608449988,
            "unit": "Nanoseconds",
            "range": 7.208737926578861e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.000082144445743632,
            "unit": "Nanoseconds",
            "range": 0.0000013198069674605236
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.0001144919683344975,
            "unit": "Nanoseconds",
            "range": 0.00000226114356641037
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011455306367061455,
            "unit": "Nanoseconds",
            "range": 2.0770121161427974e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000020160286024589454,
            "unit": "Nanoseconds",
            "range": 3.5709699653127166e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019458989736372614,
            "unit": "Nanoseconds",
            "range": 2.7677394968988556e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009869880738369276,
            "unit": "Nanoseconds",
            "range": 6.403319770744263e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "3c3a5f129fd3f2618a40ebc9495de32019755c38",
          "message": "Merge pull request #4295 from IntersectMBO/lehins/improve-generator-in-imptests\n\nImprove generator in ImpTestsState",
          "timestamp": "2024-04-24T20:18:25-06:00",
          "tree_id": "b3d684b3bfa5ac150758b84e5752c44df712876c",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/3c3a5f129fd3f2618a40ebc9495de32019755c38"
        },
        "date": 1714011670184,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006725090304953213,
            "unit": "Nanoseconds",
            "range": 0.000004111409832555049
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007119514579179006,
            "unit": "Nanoseconds",
            "range": 0.0000011758705267352801
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008022180404065482,
            "unit": "Nanoseconds",
            "range": 7.858746885317513e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.0001137660532872114,
            "unit": "Nanoseconds",
            "range": 0.000002539854764080261
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010963385659260642,
            "unit": "Nanoseconds",
            "range": 1.2108116025980893e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001889146018728094,
            "unit": "Nanoseconds",
            "range": 7.044571983269199e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018236748573895775,
            "unit": "Nanoseconds",
            "range": 2.932449421878169e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009154949313266521,
            "unit": "Nanoseconds",
            "range": 7.553868217583387e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "53cd7f991bdee289400ea7288a735ca7a74d9c99",
          "message": "Merge pull request #4292 from IntersectMBO/PR-map-genHint\n\n`constrained-generators`: add genHint for maps",
          "timestamp": "2024-04-25T11:17:22+02:00",
          "tree_id": "6d3a292d8e56152837941bf548c96e1dd5478070",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/53cd7f991bdee289400ea7288a735ca7a74d9c99"
        },
        "date": 1714036805424,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.0000696509284087409,
            "unit": "Nanoseconds",
            "range": 0.000007845794004971976
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007045845410707277,
            "unit": "Nanoseconds",
            "range": 7.718347293730271e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.000081869065021302,
            "unit": "Nanoseconds",
            "range": 0.0000024395319488373996
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011394596297579534,
            "unit": "Nanoseconds",
            "range": 0.0000029972949170795963
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011841980036302038,
            "unit": "Nanoseconds",
            "range": 2.3544204648402044e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019893191528828076,
            "unit": "Nanoseconds",
            "range": 1.591455871349342e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001959089125489695,
            "unit": "Nanoseconds",
            "range": 2.262596768115385e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009645387203353604,
            "unit": "Nanoseconds",
            "range": 1.4876955312546521e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "2f0ad36b34ee0bb97ce961c016981e48597380f7",
          "message": "Merge pull request #4298 from IntersectMBO/PR-assertReified\n\n`constrained-generators`: utility function for asserting over a reified value",
          "timestamp": "2024-04-25T14:52:31+02:00",
          "tree_id": "be11d0d7172c16363a251643bf3a9d27e691d104",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/2f0ad36b34ee0bb97ce961c016981e48597380f7"
        },
        "date": 1714049716224,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006668409149566802,
            "unit": "Nanoseconds",
            "range": 0.0000024424332392890365
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007137490595708378,
            "unit": "Nanoseconds",
            "range": 8.183564654441099e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008313297247152825,
            "unit": "Nanoseconds",
            "range": 0.000001094573584773152
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011625852206140711,
            "unit": "Nanoseconds",
            "range": 0.000003842313794112291
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011580055531123427,
            "unit": "Nanoseconds",
            "range": 1.9571116507950486e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000020381748688632632,
            "unit": "Nanoseconds",
            "range": 6.94939672462979e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019769630493585065,
            "unit": "Nanoseconds",
            "range": 8.396714921341105e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009977514085590864,
            "unit": "Nanoseconds",
            "range": 1.660584957167447e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "a2d7a817533d551e471a41bfeb499523ac52e1dd",
          "message": "Merge pull request #4300 from IntersectMBO/PR-assert-hotfix\n\n`constrained-generators`: hotfix of latest derp...",
          "timestamp": "2024-04-25T19:24:56+02:00",
          "tree_id": "93c206149941c7da3fdb99d4eebe34367371f163",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/a2d7a817533d551e471a41bfeb499523ac52e1dd"
        },
        "date": 1714066063172,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006887427562868741,
            "unit": "Nanoseconds",
            "range": 0.000008580926484182505
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007093472140662276,
            "unit": "Nanoseconds",
            "range": 3.567124898478253e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008166581216841776,
            "unit": "Nanoseconds",
            "range": 5.890222003459402e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011490302627394554,
            "unit": "Nanoseconds",
            "range": 0.0000011403939753537264
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011846414688617944,
            "unit": "Nanoseconds",
            "range": 1.9360359057134708e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019621682426868677,
            "unit": "Nanoseconds",
            "range": 1.8899624902601229e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001866621428491735,
            "unit": "Nanoseconds",
            "range": 3.177036376309448e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000949674248009043,
            "unit": "Nanoseconds",
            "range": 1.306913479885475e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "fa421acb8b4c5a80363a1862a718d22b6558c1c2",
          "message": "Merge pull request #4299 from IntersectMBO/aniketd/strange-ci-failure\n\nFix strange CI failure.",
          "timestamp": "2024-04-25T15:46:04-06:00",
          "tree_id": "6e42dd4ed7687bca61aad576d96644e9d1c49e9e",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/fa421acb8b4c5a80363a1862a718d22b6558c1c2"
        },
        "date": 1714081725691,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006759009515171173,
            "unit": "Nanoseconds",
            "range": 6.535871912181253e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007235810680581598,
            "unit": "Nanoseconds",
            "range": 0.0000013748580323856585
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000821959166539594,
            "unit": "Nanoseconds",
            "range": 4.2026227529296744e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011625187541938743,
            "unit": "Nanoseconds",
            "range": 0.000003220839090869991
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011841291216571652,
            "unit": "Nanoseconds",
            "range": 3.196316748489545e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000020066398681747606,
            "unit": "Nanoseconds",
            "range": 3.8687926477641736e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019898412266232788,
            "unit": "Nanoseconds",
            "range": 4.162902721476913e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009926372218115345,
            "unit": "Nanoseconds",
            "range": 1.1235242202175519e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "27baa600f5e3c19a7a99e84abc3aa2ac1b028d7a",
          "message": "Merge pull request #4276 from IntersectMBO/neilmayhew/4039-ci-separate-test-jobs\n\nUse a separate job for each test suite in GitHub CI",
          "timestamp": "2024-04-25T23:20:30-06:00",
          "tree_id": "e11eb54c2020fe7c3c28bdaf543f28e8edff5cc8",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/27baa600f5e3c19a7a99e84abc3aa2ac1b028d7a"
        },
        "date": 1714108987619,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006880466430736719,
            "unit": "Nanoseconds",
            "range": 0.0000019021521899218307
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007207908319101683,
            "unit": "Nanoseconds",
            "range": 0.000004648501107686683
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008247543517811552,
            "unit": "Nanoseconds",
            "range": 6.926309407521763e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011454127069545736,
            "unit": "Nanoseconds",
            "range": 0.0000012882846195629436
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010792184695184634,
            "unit": "Nanoseconds",
            "range": 9.260824610403168e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001992874657145762,
            "unit": "Nanoseconds",
            "range": 0.0000017895508059767424
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001921998651359361,
            "unit": "Nanoseconds",
            "range": 9.626991160060373e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009888647658627476,
            "unit": "Nanoseconds",
            "range": 1.8291076157536502e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "teodora.danciu@tweag.io",
            "name": "teodanciu",
            "username": "teodanciu"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "2e3f1be5efdd0706663187e8972d20d93b20cdfa",
          "message": "Merge pull request #4285 from IntersectMBO/td/imptest-initial-committee\n\nStart Conway Imp tests with an initial committee and constitution",
          "timestamp": "2024-04-26T11:24:11+01:00",
          "tree_id": "23d8f171170118c9110d72077c54cfd13419350e",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/2e3f1be5efdd0706663187e8972d20d93b20cdfa"
        },
        "date": 1714127210696,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006785166084493167,
            "unit": "Nanoseconds",
            "range": 5.696583427726151e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007139100800884961,
            "unit": "Nanoseconds",
            "range": 6.627932987737872e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000815208930824466,
            "unit": "Nanoseconds",
            "range": 0.00000113528183457206
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011504645278565237,
            "unit": "Nanoseconds",
            "range": 0.0000026298381775223014
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011477538362625203,
            "unit": "Nanoseconds",
            "range": 1.5465056349540427e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000020059413642548415,
            "unit": "Nanoseconds",
            "range": 8.561184668821655e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019353764401766744,
            "unit": "Nanoseconds",
            "range": 3.3839396192617253e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009721173540189713,
            "unit": "Nanoseconds",
            "range": 1.56729641450537e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "aniketd@users.noreply.github.com",
            "name": "Aniket Deshpande",
            "username": "aniketd"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "127efb52d8e2925a7c30b56a8fd44d6b0e443e8b",
          "message": "Merge pull request #4273 from IntersectMBO/aniketd/rewards-in-drep-distr\n\nDRepDistr: Iterate over the DRep delegations in UMap",
          "timestamp": "2024-04-26T18:44:24+05:30",
          "tree_id": "031adea9d457f84bfeeba63ba2d427fb4df4c8dd",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/127efb52d8e2925a7c30b56a8fd44d6b0e443e8b"
        },
        "date": 1714137429309,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006838590008279203,
            "unit": "Nanoseconds",
            "range": 0.000009681876807997883
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007088818558905278,
            "unit": "Nanoseconds",
            "range": 7.79825720746754e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008155495289662417,
            "unit": "Nanoseconds",
            "range": 4.731800524106959e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011705573593195573,
            "unit": "Nanoseconds",
            "range": 0.000008683450935497713
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011102757802383798,
            "unit": "Nanoseconds",
            "range": 0.0000013759617234417385
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019762114970116145,
            "unit": "Nanoseconds",
            "range": 6.065386153322345e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.0000190839481064243,
            "unit": "Nanoseconds",
            "range": 6.697252201481636e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009650131049173258,
            "unit": "Nanoseconds",
            "range": 1.1138144284375816e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "f64e29f92ba4d604fdad165954af17f1bc20afb7",
          "message": "Merge pull request #4303 from IntersectMBO/td/fix-test-in-master\n\nFix test caused by erroneous merge",
          "timestamp": "2024-04-26T12:05:21-06:00",
          "tree_id": "43d65203f0890b1b5da14cd5a8f6bc690793949e",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/f64e29f92ba4d604fdad165954af17f1bc20afb7"
        },
        "date": 1714154887376,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006969405125372903,
            "unit": "Nanoseconds",
            "range": 0.000005895812918677746
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007186742225784876,
            "unit": "Nanoseconds",
            "range": 9.259857403037814e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008131912684133313,
            "unit": "Nanoseconds",
            "range": 0.0000010388513362117633
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011526292959611701,
            "unit": "Nanoseconds",
            "range": 0.0000030840411037890647
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011673272282590553,
            "unit": "Nanoseconds",
            "range": 4.3683450521826674e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00002004321417423004,
            "unit": "Nanoseconds",
            "range": 7.632148539642349e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019578836440906244,
            "unit": "Nanoseconds",
            "range": 2.7553428977813325e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009889092725404362,
            "unit": "Nanoseconds",
            "range": 1.2686107119734262e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "cc24717dcbd7fbfbcd83bfc26cbb2aaafe2945bb",
          "message": "Merge pull request #4304 from IntersectMBO/neilmayhew/ci-complete\n\nEnsure the CI complete step fails when tests fail",
          "timestamp": "2024-04-26T13:46:47-06:00",
          "tree_id": "d34d11e68356a3b2d9409d2a32d68f80c5da9db2",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/cc24717dcbd7fbfbcd83bfc26cbb2aaafe2945bb"
        },
        "date": 1714160976343,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006626820038245421,
            "unit": "Nanoseconds",
            "range": 0.0000015483255926522314
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007078278905387777,
            "unit": "Nanoseconds",
            "range": 9.58782345252424e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000812026086647815,
            "unit": "Nanoseconds",
            "range": 0.0000012528474419565688
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011415326962163642,
            "unit": "Nanoseconds",
            "range": 0.0000036859175008512644
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011395307013390349,
            "unit": "Nanoseconds",
            "range": 2.517064363403335e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019818930061820073,
            "unit": "Nanoseconds",
            "range": 1.563670793094986e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019544076900660676,
            "unit": "Nanoseconds",
            "range": 9.812898355013295e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009574546366042858,
            "unit": "Nanoseconds",
            "range": 1.3021349936739568e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "66e2e02d884875f2bce7d38eb7af0971410cd92d",
          "message": "Merge pull request #4305 from IntersectMBO/neilmayhew/ci-setup-haskell\n\nUse the correct iohk action for installing Haskell in GitHub CI",
          "timestamp": "2024-04-29T09:52:01-06:00",
          "tree_id": "1ce1a8a1d87d6eb8529e6ef442d9741c2377e9e0",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/66e2e02d884875f2bce7d38eb7af0971410cd92d"
        },
        "date": 1714406094379,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006738509386701592,
            "unit": "Nanoseconds",
            "range": 0.0000016749534270978535
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007068881504573328,
            "unit": "Nanoseconds",
            "range": 8.836532509741848e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008129299718208722,
            "unit": "Nanoseconds",
            "range": 0.0000011287254710332868
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011364544650062654,
            "unit": "Nanoseconds",
            "range": 0.000001605004078940466
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001156558934691499,
            "unit": "Nanoseconds",
            "range": 2.118051274217447e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001970914492615097,
            "unit": "Nanoseconds",
            "range": 4.904790096675922e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001929831243494011,
            "unit": "Nanoseconds",
            "range": 1.6613446614532705e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009795449328056347,
            "unit": "Nanoseconds",
            "range": 1.1356310378524784e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "ad4afe1f9a83c40f599d467231db75bf052547c9",
          "message": "`constrained-generators`: Fix `ifElse` dependencies (#4297)\n\n* IfElse as its own Pred with reversed dependencies\r\n\r\n* Apply suggestions from code review",
          "timestamp": "2024-04-29T21:04:47Z",
          "tree_id": "a55a1510b57e0ff94bcd87c5696f1a2e42b51793",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/ad4afe1f9a83c40f599d467231db75bf052547c9"
        },
        "date": 1714424858430,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006857225829415326,
            "unit": "Nanoseconds",
            "range": 6.337373422489648e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007299452604629245,
            "unit": "Nanoseconds",
            "range": 9.911499107392926e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008370387044737406,
            "unit": "Nanoseconds",
            "range": 9.9908157738798e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011774180507687326,
            "unit": "Nanoseconds",
            "range": 0.0000021662559778354517
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010978767645091265,
            "unit": "Nanoseconds",
            "range": 2.364329416773728e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019626145653052836,
            "unit": "Nanoseconds",
            "range": 2.9681966958268545e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.0000181176843563511,
            "unit": "Nanoseconds",
            "range": 3.1575988013380945e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000916695505239173,
            "unit": "Nanoseconds",
            "range": 1.6992688400784002e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "teodora.danciu@tweag.io",
            "name": "teodanciu",
            "username": "teodanciu"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "587818ac6531909c0fdc16d316ecab415187e4a2",
          "message": "Merge pull request #4275 from IntersectMBO/td/bootstrap\n\nRestrict gov actions during bootstrap",
          "timestamp": "2024-04-30T01:27:59+01:00",
          "tree_id": "023facc9f6e33710345d09b6e721db20e76b9cb4",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/587818ac6531909c0fdc16d316ecab415187e4a2"
        },
        "date": 1714437043000,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.0000678587755697651,
            "unit": "Nanoseconds",
            "range": 0.0000018437547869554383
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007411786019031714,
            "unit": "Nanoseconds",
            "range": 0.0000056402047477939005
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008269284002823987,
            "unit": "Nanoseconds",
            "range": 0.0000013949902422832183
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011671742559905197,
            "unit": "Nanoseconds",
            "range": 0.0000018853422138376469
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010874101722949142,
            "unit": "Nanoseconds",
            "range": 2.6723087186069657e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019245638717314664,
            "unit": "Nanoseconds",
            "range": 5.082681877858119e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018648056108368272,
            "unit": "Nanoseconds",
            "range": 7.470699421466297e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009313948799240576,
            "unit": "Nanoseconds",
            "range": 1.1178532891826814e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "42d5b6d69edb0ca1aab967146656c62f370e4c84",
          "message": "Merge pull request #4308 from IntersectMBO/neilmayhew/4307-ci-check-branch-history\n\nAdd a CI status check to prevent merging PRs that contain merges",
          "timestamp": "2024-04-29T23:19:28-06:00",
          "tree_id": "9280d1c8ff9248e459ec6f93e9f43801c0875c83",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/42d5b6d69edb0ca1aab967146656c62f370e4c84"
        },
        "date": 1714454532921,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006677647410342274,
            "unit": "Nanoseconds",
            "range": 7.046283837935456e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007224053614706746,
            "unit": "Nanoseconds",
            "range": 0.000002255866143382964
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008233983688084075,
            "unit": "Nanoseconds",
            "range": 6.49312833402415e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011534442483398507,
            "unit": "Nanoseconds",
            "range": 0.000002816080492840829
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010843355356124234,
            "unit": "Nanoseconds",
            "range": 3.0727172749146915e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001882226118144217,
            "unit": "Nanoseconds",
            "range": 3.008730066737491e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018077073824109372,
            "unit": "Nanoseconds",
            "range": 5.376815421169166e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009013115192446952,
            "unit": "Nanoseconds",
            "range": 9.26429130597317e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "c73cad97db9fa7665db32132aa665c197be1f1ea",
          "message": "`constrained-generators`: Add monitoring capability to get a handle on test case distribution (#4301)\n\n* add monitoring capability to start getting a handle on test case\r\ndistribution\r\n\r\n* Add haddocks for monitor and monitorSpec\r\n\r\n* Add case for IfElse to monitorPred\r\n\r\n---------\r\n\r\nCo-authored-by: Ulf Norell <ulf.norell@gmail.com>",
          "timestamp": "2024-04-30T15:15:18Z",
          "tree_id": "6eb1c2dd8f2e45b3e1ddd814eb6cb16ac51400dd",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/c73cad97db9fa7665db32132aa665c197be1f1ea"
        },
        "date": 1714491639109,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006709592895952596,
            "unit": "Nanoseconds",
            "range": 0.000001580817299722105
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.0000713756770306846,
            "unit": "Nanoseconds",
            "range": 9.044349710773339e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008073948594634961,
            "unit": "Nanoseconds",
            "range": 8.397739140946805e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011643819131802711,
            "unit": "Nanoseconds",
            "range": 0.0000026425976701347457
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001074614880791087,
            "unit": "Nanoseconds",
            "range": 2.1606106991571753e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001856760599894599,
            "unit": "Nanoseconds",
            "range": 0.000001007457755146838
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017634992707176726,
            "unit": "Nanoseconds",
            "range": 1.849139504698603e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009354273427538478,
            "unit": "Nanoseconds",
            "range": 9.802937542230624e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "joosep.jaager@iohk.io",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "committer": {
            "email": "joosep.jaager@gmail.com",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "distinct": true,
          "id": "be59c234150b6c5a24596ec6829810c2d2dfa732",
          "message": "Fixed OMap.assocList",
          "timestamp": "2024-04-30T17:49:18Z",
          "tree_id": "431506bf1f7f7daecf2237ef009a14b2e6bbcf12",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/be59c234150b6c5a24596ec6829810c2d2dfa732"
        },
        "date": 1714499525760,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.0000660191646637156,
            "unit": "Nanoseconds",
            "range": 0.0000030918535335513307
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007033256855574169,
            "unit": "Nanoseconds",
            "range": 0.0000013998625565433933
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007906122571102193,
            "unit": "Nanoseconds",
            "range": 6.209696113527826e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011101866564120834,
            "unit": "Nanoseconds",
            "range": 0.000001950326962287471
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010994916587185485,
            "unit": "Nanoseconds",
            "range": 9.255266385200877e-8
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001897174106254277,
            "unit": "Nanoseconds",
            "range": 2.801461084418956e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001839288898535645,
            "unit": "Nanoseconds",
            "range": 1.2894742797405873e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008942231921686025,
            "unit": "Nanoseconds",
            "range": 1.1598115268088842e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "ef72e18ff59f09801869936813ec89ff96b83c1b",
          "message": "`constrained-generators`: Improve error messages and make the tree generator reasonably sized (#4315)\n\n* Improve error messages, make the default tree generator not generate\r\nenourmous trees, and remove some old comments that aren't super relevant\r\nany more.\r\n\r\n* add forAllSpec and forAllSpecShow\r\n\r\n* Better explanation of intersections\r\n\r\n* Better error reporting",
          "timestamp": "2024-05-02T16:44:51Z",
          "tree_id": "3f08d42d0510f0e1f45988e96b333209e285848a",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/ef72e18ff59f09801869936813ec89ff96b83c1b"
        },
        "date": 1714668470099,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006695758226679635,
            "unit": "Nanoseconds",
            "range": 0.0000063054886157086124
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006992028319828338,
            "unit": "Nanoseconds",
            "range": 5.092544621184176e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008050466556638869,
            "unit": "Nanoseconds",
            "range": 5.986787143620367e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011521362624395121,
            "unit": "Nanoseconds",
            "range": 0.0000012224846454337398
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.0000110595500060062,
            "unit": "Nanoseconds",
            "range": 0.000001363613996841929
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018956248764808865,
            "unit": "Nanoseconds",
            "range": 4.910967905204069e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001828425494512025,
            "unit": "Nanoseconds",
            "range": 0.0000012255083090395303
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009217509233881606,
            "unit": "Nanoseconds",
            "range": 1.8772023783592152e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "joosep.jaager@iohk.io",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "committer": {
            "email": "joosep.jaager@gmail.com",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "distinct": true,
          "id": "572b6fd9540d1236dd19edee94bceeed210ac501",
          "message": "Removed GovRelation from GovEnv\n\nFixed a bug in prevGovActionId translation\n\nFixed TestRep of Proposals\n\nAdded generation timeout tests\n\nBump cardano-ledger-executable-spec\n\nFixed `OMap.assocList`\n\nAdded Testable instance for `ImpTestM`\n\nCo-authored-by: Maximilian Algehed <MaximilianAlgehed@users.noreply.github.com>\n\nCo-authored-by: Alexey Kuleshevich <alexey.kuleshevich@iohk.io>",
          "timestamp": "2024-05-03T12:58:42Z",
          "tree_id": "a3bd4059534bb505ab8e587150e804ef92881c6e",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/572b6fd9540d1236dd19edee94bceeed210ac501"
        },
        "date": 1714741288519,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006744216141409973,
            "unit": "Nanoseconds",
            "range": 6.075099507370792e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007047856737325667,
            "unit": "Nanoseconds",
            "range": 4.3728539045876615e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008039385507067024,
            "unit": "Nanoseconds",
            "range": 3.929403038286133e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011178569307205996,
            "unit": "Nanoseconds",
            "range": 0.0000015907910648449593
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001204536489614892,
            "unit": "Nanoseconds",
            "range": 1.6182361090559447e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001966962347273998,
            "unit": "Nanoseconds",
            "range": 1.4551104970879723e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019506322594014225,
            "unit": "Nanoseconds",
            "range": 0.0000013849497071138788
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009504697177869181,
            "unit": "Nanoseconds",
            "range": 9.407659293771754e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "899f396627db2013d08958345350b19d6350f4c1",
          "message": "Merge pull request #4317 from IntersectMBO/PR-fix-reifies-bug\n\n`constrained-generators`: Fix bug in reifies",
          "timestamp": "2024-05-06T21:58:21+02:00",
          "tree_id": "313b2a79a3ee2c9a75daba94980a569e0f90c39c",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/899f396627db2013d08958345350b19d6350f4c1"
        },
        "date": 1715025679196,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006597298729729502,
            "unit": "Nanoseconds",
            "range": 0.000003284827807246535
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.0000704774455466771,
            "unit": "Nanoseconds",
            "range": 0.0000018092842835413718
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008008135967104765,
            "unit": "Nanoseconds",
            "range": 0.0000021069731950015507
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.0001142551850092344,
            "unit": "Nanoseconds",
            "range": 5.96481331845617e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010789278628465262,
            "unit": "Nanoseconds",
            "range": 1.4835335322971527e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018938950972578276,
            "unit": "Nanoseconds",
            "range": 2.608156163242615e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017819240829467107,
            "unit": "Nanoseconds",
            "range": 1.9060629026446783e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000903747499459952,
            "unit": "Nanoseconds",
            "range": 1.196135042476958e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "092f4b293397250f451acae61ee2890e859fe2ff",
          "message": "Merge pull request #4322 from IntersectMBO/dependabot/pip/doc/jinja2-3.1.4\n\nBump jinja2 from 3.1.3 to 3.1.4 in /doc",
          "timestamp": "2024-05-06T17:00:12-06:00",
          "tree_id": "f5610def69f0ae7d31fec7818b8af6836c1779b3",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/092f4b293397250f451acae61ee2890e859fe2ff"
        },
        "date": 1715036584765,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006667632476325867,
            "unit": "Nanoseconds",
            "range": 0.000006029652893283782
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007018028174731749,
            "unit": "Nanoseconds",
            "range": 0.0000013204932553688766
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008016994817819472,
            "unit": "Nanoseconds",
            "range": 0.000002878135882857644
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.0001138201789189914,
            "unit": "Nanoseconds",
            "range": 0.000002159893634776642
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010679733036503214,
            "unit": "Nanoseconds",
            "range": 1.362054795665121e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018697384249154888,
            "unit": "Nanoseconds",
            "range": 4.215338397219623e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001796091635221685,
            "unit": "Nanoseconds",
            "range": 1.9663645894046548e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009048796185103727,
            "unit": "Nanoseconds",
            "range": 1.6268955178636934e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "63ddaf6f3102e06d26ac2c69b2d8530d83d26a19",
          "message": "Merge pull request #4309 from IntersectMBO/aniketd/proposal-deposits-in-voting-stake\n\nAdd proposal deposits to DRep active voting stake.",
          "timestamp": "2024-05-06T19:15:56-06:00",
          "tree_id": "7b1441e9994161f5a9002442151d5ae0535ac055",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/63ddaf6f3102e06d26ac2c69b2d8530d83d26a19"
        },
        "date": 1715044726220,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006851836471385845,
            "unit": "Nanoseconds",
            "range": 7.846731543173745e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007159257876654968,
            "unit": "Nanoseconds",
            "range": 7.213278907089326e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008278345787905226,
            "unit": "Nanoseconds",
            "range": 0.0000029315057658427582
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.0001154698816525756,
            "unit": "Nanoseconds",
            "range": 6.561850293768296e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011745333695928527,
            "unit": "Nanoseconds",
            "range": 1.3538975036855234e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000020180636473405996,
            "unit": "Nanoseconds",
            "range": 1.3174529812728175e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001993970618242004,
            "unit": "Nanoseconds",
            "range": 1.6089906945166026e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000994427665947993,
            "unit": "Nanoseconds",
            "range": 1.062366697423384e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "71b8cb1eb1d5af4461a522dc4baa35225a866fcb",
          "message": "Merge pull request #4253 from IntersectMBO/lehins/hardfork-initiation\n\nHardfork Initiation into a new era",
          "timestamp": "2024-05-07T14:15:32-06:00",
          "tree_id": "6d5fda996d122a820971ff884d95f375a041dc99",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/71b8cb1eb1d5af4461a522dc4baa35225a866fcb"
        },
        "date": 1715113104310,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006762243701107134,
            "unit": "Nanoseconds",
            "range": 0.000003239123273673421
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.0000716002597004589,
            "unit": "Nanoseconds",
            "range": 0.0000014326711098114808
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008393876626663124,
            "unit": "Nanoseconds",
            "range": 9.790987833756356e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011774665771206693,
            "unit": "Nanoseconds",
            "range": 0.0000041028904223925256
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011048668151989345,
            "unit": "Nanoseconds",
            "range": 1.8867561595686053e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019253678175553377,
            "unit": "Nanoseconds",
            "range": 2.334834463953711e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018431537003336255,
            "unit": "Nanoseconds",
            "range": 4.092823897158242e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.0000091503760641478,
            "unit": "Nanoseconds",
            "range": 8.94439837881267e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "dbb9f4e846f17660f7690301a4f23da1dfb842df",
          "message": "Merge pull request #4284 from IntersectMBO/lehins/flexible-costmodel-params\n\nFlexible costmodel params",
          "timestamp": "2024-05-07T18:52:47-06:00",
          "tree_id": "2beff5d9f8e447c9d384f14d7b9ee0adeed0a05b",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/dbb9f4e846f17660f7690301a4f23da1dfb842df"
        },
        "date": 1715129740821,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00007287117809162612,
            "unit": "Nanoseconds",
            "range": 0.000009333398450528238
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007033673048287544,
            "unit": "Nanoseconds",
            "range": 7.856282367590103e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008288636649211169,
            "unit": "Nanoseconds",
            "range": 0.000002200476735317758
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011716638815420882,
            "unit": "Nanoseconds",
            "range": 0.000008548835065001841
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010783432030218041,
            "unit": "Nanoseconds",
            "range": 1.9798248742073995e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001906723257712619,
            "unit": "Nanoseconds",
            "range": 5.847936791711429e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018038070506401985,
            "unit": "Nanoseconds",
            "range": 4.1442008572058386e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008845025567487744,
            "unit": "Nanoseconds",
            "range": 9.803750330077952e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "teodora.danciu@tweag.io",
            "name": "teodanciu",
            "username": "teodanciu"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "15afd0483d81f281018cf7d42629ed561845bc86",
          "message": "Merge pull request #4328 from IntersectMBO/lehins/disable-drep-in-bootstrap\n\nDisable drep thresholds in bootstrap",
          "timestamp": "2024-05-08T21:16:22+01:00",
          "tree_id": "39da3ac54ee1289bb31e79d877ebf11c92c5da9b",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/15afd0483d81f281018cf7d42629ed561845bc86"
        },
        "date": 1715199552191,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006762830004375235,
            "unit": "Nanoseconds",
            "range": 0.000004367377290649384
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006998502026321343,
            "unit": "Nanoseconds",
            "range": 0.0000013097960208605248
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008016844060896004,
            "unit": "Nanoseconds",
            "range": 0.0000029617070658277452
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011242256553717228,
            "unit": "Nanoseconds",
            "range": 0.0000035706587569452806
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010640267154167317,
            "unit": "Nanoseconds",
            "range": 1.7525688659572285e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018710827688302823,
            "unit": "Nanoseconds",
            "range": 7.222992929302253e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017902859770592956,
            "unit": "Nanoseconds",
            "range": 2.296084674018633e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008878371523017697,
            "unit": "Nanoseconds",
            "range": 8.609467857477268e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "daniel.lucsanszky@iohk.io",
            "name": "Lucsanszky",
            "username": "Lucsanszky"
          },
          "committer": {
            "email": "disasm@gmail.com",
            "name": "Samuel Leathers",
            "username": "disassembler"
          },
          "distinct": true,
          "id": "83625e963ee80d99b5c901990da1d4be000267bd",
          "message": "Fix `cardano-ledger-conway-test` version\n\nIt was not updated when some dependency version bounds were bumped.",
          "timestamp": "2024-05-13T09:09:15-04:00",
          "tree_id": "4c499eeb0918ea5cf271c894d75f51c986848b8d",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/83625e963ee80d99b5c901990da1d4be000267bd"
        },
        "date": 1715605930026,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006583767340064739,
            "unit": "Nanoseconds",
            "range": 0.000002534147603607393
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006958376416851014,
            "unit": "Nanoseconds",
            "range": 5.749041599386748e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000808083396151703,
            "unit": "Nanoseconds",
            "range": 0.0000025710642122135095
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011311503293760793,
            "unit": "Nanoseconds",
            "range": 8.878168457925841e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010703531551777394,
            "unit": "Nanoseconds",
            "range": 1.306938029419677e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018934188934605085,
            "unit": "Nanoseconds",
            "range": 1.8918055274075913e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001804962182889362,
            "unit": "Nanoseconds",
            "range": 3.4401177167918033e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009004694399862092,
            "unit": "Nanoseconds",
            "range": 1.8589151874354048e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "aniketd@users.noreply.github.com",
            "name": "Aniket Deshpande",
            "username": "aniketd"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "14c839429862291ae7af9347b27d642e197fbba3",
          "message": "Merge pull request #4324 from IntersectMBO/aniketd/proposal-deposits-in-spo-voting-stake\n\nProposal deposits in SPO voting stake",
          "timestamp": "2024-05-14T00:46:11+05:30",
          "tree_id": "2aa3dbbd477170037ee4d91875b81bb34a42d872",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/14c839429862291ae7af9347b27d642e197fbba3"
        },
        "date": 1715627939799,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006572988487572183,
            "unit": "Nanoseconds",
            "range": 0.00000238947127606588
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006938491806822883,
            "unit": "Nanoseconds",
            "range": 0.0000015467964285883617
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008019203639365021,
            "unit": "Nanoseconds",
            "range": 0.000002054321797501673
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011307387462438665,
            "unit": "Nanoseconds",
            "range": 8.171668907725864e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010595886305725866,
            "unit": "Nanoseconds",
            "range": 1.2915819575781077e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018470089658204073,
            "unit": "Nanoseconds",
            "range": 2.35440737886357e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001743768206321569,
            "unit": "Nanoseconds",
            "range": 2.815114069186792e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008741824363828595,
            "unit": "Nanoseconds",
            "range": 9.570378756356935e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "885d01d1ab32052c1fb5a260f374eb831e079269",
          "message": "Merge pull request #4323 from IntersectMBO/PR-remove-unused-exports\n\n`constrained-generators`: clean up interface",
          "timestamp": "2024-05-14T09:16:56+02:00",
          "tree_id": "aa2f59790ccfd143426e7b10e6f3c9a5afa4b51e",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/885d01d1ab32052c1fb5a260f374eb831e079269"
        },
        "date": 1715671182599,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006558465266358095,
            "unit": "Nanoseconds",
            "range": 0.0000012289928734719652
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006989315686975852,
            "unit": "Nanoseconds",
            "range": 4.202240621767992e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008143353300623055,
            "unit": "Nanoseconds",
            "range": 0.000002431714252832122
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011312482077316139,
            "unit": "Nanoseconds",
            "range": 0.0000036418064000253758
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010550300547403934,
            "unit": "Nanoseconds",
            "range": 1.5052609151526234e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018726510530584662,
            "unit": "Nanoseconds",
            "range": 6.816023629617131e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017359289456736237,
            "unit": "Nanoseconds",
            "range": 1.3973297988901037e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008749724591509967,
            "unit": "Nanoseconds",
            "range": 7.034427039393318e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Lucsanszky@users.noreply.github.com",
            "name": "Lucsanszky",
            "username": "Lucsanszky"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "5ed689084d2a7aec3af275bec0922ba6a27e937e",
          "message": "Merge pull request #4332 from IntersectMBO/ldan/update-changelogs\n\nUpdate `CHANGELOG`s",
          "timestamp": "2024-05-15T04:27:58+02:00",
          "tree_id": "9a3d93f081ad19aaaf967ae556c71401eea17dc1",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/5ed689084d2a7aec3af275bec0922ba6a27e937e"
        },
        "date": 1715740244172,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006663257908806017,
            "unit": "Nanoseconds",
            "range": 0.000004013572433479526
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006987445130785818,
            "unit": "Nanoseconds",
            "range": 4.787106639097862e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007979229217272563,
            "unit": "Nanoseconds",
            "range": 5.798886693530345e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.0001130828986482171,
            "unit": "Nanoseconds",
            "range": 7.258115323105173e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010635652017783573,
            "unit": "Nanoseconds",
            "range": 1.1410588740754649e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018421776451349053,
            "unit": "Nanoseconds",
            "range": 2.443748913880896e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017417401691105677,
            "unit": "Nanoseconds",
            "range": 1.2976828160958578e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008812615521250583,
            "unit": "Nanoseconds",
            "range": 1.210612955907175e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "joosep.jaager@iohk.io",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "a71a029e7a04bf6badbab69558f4376f05a9261b",
          "message": "Add conformance testing for CERT. Rework TestRep. (#4320)",
          "timestamp": "2024-05-15T12:11:23Z",
          "tree_id": "6ecd72e7d038a336f98d533a0c2d4d975aace634",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/a71a029e7a04bf6badbab69558f4376f05a9261b"
        },
        "date": 1715775254702,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006557533447315323,
            "unit": "Nanoseconds",
            "range": 5.658432368694561e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006938227326157063,
            "unit": "Nanoseconds",
            "range": 4.686108831281468e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008079059949680073,
            "unit": "Nanoseconds",
            "range": 0.0000028230988224707075
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011324420840559706,
            "unit": "Nanoseconds",
            "range": 0.000003197864248429886
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010618591572669801,
            "unit": "Nanoseconds",
            "range": 2.136064882545174e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018489156275840225,
            "unit": "Nanoseconds",
            "range": 2.468073620226643e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017557095315892584,
            "unit": "Nanoseconds",
            "range": 1.6761859615774173e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008735934345610413,
            "unit": "Nanoseconds",
            "range": 1.1248464319709396e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "m.algehed@gmail.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "distinct": true,
          "id": "9245c7f56fd147f4beeb2bc00646c6bcf94d0441",
          "message": "Added additional ways of generating constraints",
          "timestamp": "2024-05-15T16:24:30+02:00",
          "tree_id": "ae65bef057c55f017e9901798db54033989ffb8d",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/9245c7f56fd147f4beeb2bc00646c6bcf94d0441"
        },
        "date": 1715783236300,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.000069209239765667,
            "unit": "Nanoseconds",
            "range": 0.00000431950936431904
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006965212127588517,
            "unit": "Nanoseconds",
            "range": 9.958507147675847e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000798488045955271,
            "unit": "Nanoseconds",
            "range": 9.113463284367677e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011445956350341721,
            "unit": "Nanoseconds",
            "range": 0.0000030764207164456584
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010870319699823393,
            "unit": "Nanoseconds",
            "range": 4.3250612755405154e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001885143257697655,
            "unit": "Nanoseconds",
            "range": 6.186890790091262e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001799911288581215,
            "unit": "Nanoseconds",
            "range": 2.1487465995532733e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008825111038646835,
            "unit": "Nanoseconds",
            "range": 5.7756968097061024e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "joosep.jaager@iohk.io",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "committer": {
            "email": "joosep.jaager@gmail.com",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "distinct": true,
          "id": "28ab3884cac8edbb7270fd4b8628a16429d2ec9e",
          "message": "Add conformance testing for RATIFY",
          "timestamp": "2024-05-15T16:16:58Z",
          "tree_id": "ca450898cf5d536e9198d7b804be53c58d7767d3",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/28ab3884cac8edbb7270fd4b8628a16429d2ec9e"
        },
        "date": 1715789989716,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006520080469182939,
            "unit": "Nanoseconds",
            "range": 9.848725488034593e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006889370860797874,
            "unit": "Nanoseconds",
            "range": 0.000002009317483567601
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000789715224631103,
            "unit": "Nanoseconds",
            "range": 0.000001198373590831427
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011023178663828867,
            "unit": "Nanoseconds",
            "range": 7.397022452776435e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001088605374402281,
            "unit": "Nanoseconds",
            "range": 3.130604692229129e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018591955884669353,
            "unit": "Nanoseconds",
            "range": 1.1417046524034923e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018364591153209787,
            "unit": "Nanoseconds",
            "range": 1.6384108489808847e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000889065071519963,
            "unit": "Nanoseconds",
            "range": 9.568304272240162e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "teodora.danciu@tweag.io",
            "name": "teodanciu",
            "username": "teodanciu"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "9fa49a237f9da9a48de1352a5dbc59535888720a",
          "message": "Merge pull request #4316 from IntersectMBO/td/shelley-allegra-erascript\n\nComplete `EraScript` hierarchy with missing  classes",
          "timestamp": "2024-05-21T09:19:33+01:00",
          "tree_id": "d8c7c6ba27a35fdd8a8df679222756e1f15f507d",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/9fa49a237f9da9a48de1352a5dbc59535888720a"
        },
        "date": 1716279736986,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006675354677447443,
            "unit": "Nanoseconds",
            "range": 3.8836954539792223e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.0000702429457095964,
            "unit": "Nanoseconds",
            "range": 7.910484613557177e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008008992906216399,
            "unit": "Nanoseconds",
            "range": 0.0000010886523962933556
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011217794335901685,
            "unit": "Nanoseconds",
            "range": 0.0000025712317163880096
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011674417295994707,
            "unit": "Nanoseconds",
            "range": 1.1166773052545705e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001897601126332888,
            "unit": "Nanoseconds",
            "range": 1.369800930696279e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019365687008182275,
            "unit": "Nanoseconds",
            "range": 1.6820157798893814e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000010053662944542761,
            "unit": "Nanoseconds",
            "range": 1.8219470536253877e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "760ea5ae5de8ba0ee0568252e36e57ef4737ed63",
          "message": "Merge pull request #4287 from IntersectMBO/andre/spec-fix\n\nFix various minor issues in the Shelley & Babbage specs",
          "timestamp": "2024-05-21T10:41:35-06:00",
          "tree_id": "e9bf9fc9b01e6ad8d49c3261cb361b46633e4898",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/760ea5ae5de8ba0ee0568252e36e57ef4737ed63"
        },
        "date": 1716309860567,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006843371336145979,
            "unit": "Nanoseconds",
            "range": 0.0000011558111051494845
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007329744530259585,
            "unit": "Nanoseconds",
            "range": 0.0000029838439766247274
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008405300275600701,
            "unit": "Nanoseconds",
            "range": 8.385508158270071e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011723067046484323,
            "unit": "Nanoseconds",
            "range": 0.0000022040081763611338
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011798283721227415,
            "unit": "Nanoseconds",
            "range": 1.262532677204857e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.0000192293917559399,
            "unit": "Nanoseconds",
            "range": 3.4117963664830525e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000020257215817888083,
            "unit": "Nanoseconds",
            "range": 5.221537124739334e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009755069521978371,
            "unit": "Nanoseconds",
            "range": 1.1626416979668373e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "04685e17800e0d19b8b799971d7d5eaa75c1ec00",
          "message": "Merge pull request #4343 from IntersectMBO/dependabot/pip/doc/requests-2.32.0\n\nBump requests from 2.31.0 to 2.32.0 in /doc",
          "timestamp": "2024-05-21T15:33:48-06:00",
          "tree_id": "44c922adad27a5d559636fc0a8a98b18982d76b8",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/04685e17800e0d19b8b799971d7d5eaa75c1ec00"
        },
        "date": 1716327392151,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006675038672092294,
            "unit": "Nanoseconds",
            "range": 9.220672792196261e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.0000702206886080163,
            "unit": "Nanoseconds",
            "range": 0.00000127066345550457
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008265158389187964,
            "unit": "Nanoseconds",
            "range": 7.789043911441745e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011512009068483519,
            "unit": "Nanoseconds",
            "range": 0.0000011270015383220881
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011687717182016604,
            "unit": "Nanoseconds",
            "range": 3.69141025868828e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001918108654980709,
            "unit": "Nanoseconds",
            "range": 0.0000011286854886796607
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001993286775355089,
            "unit": "Nanoseconds",
            "range": 5.504401708782e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009906251226491465,
            "unit": "Nanoseconds",
            "range": 1.10137584793846e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "aniketd@users.noreply.github.com",
            "name": "Aniket Deshpande",
            "username": "aniketd"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0462183b6d59126e4412920986408007c026cb93",
          "message": "Merge pull request #4337 from IntersectMBO/aniketd/conformance-ratify\n\nFix RATIFY conformance",
          "timestamp": "2024-05-22T17:30:12+05:30",
          "tree_id": "9d3647a2417ffa4aa149cb0458decf0d5eba8c6e",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/0462183b6d59126e4412920986408007c026cb93"
        },
        "date": 1716379377760,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.0000678813485716254,
            "unit": "Nanoseconds",
            "range": 8.230435954088485e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007077045006323355,
            "unit": "Nanoseconds",
            "range": 5.834644142882744e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008143504645584347,
            "unit": "Nanoseconds",
            "range": 4.827437341881551e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011473770688107717,
            "unit": "Nanoseconds",
            "range": 0.000003284189725596272
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011988413358372583,
            "unit": "Nanoseconds",
            "range": 7.32523327909583e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019731072275337914,
            "unit": "Nanoseconds",
            "range": 7.690832601204733e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019075273681001313,
            "unit": "Nanoseconds",
            "range": 0.0000010487685405919782
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000952655891470338,
            "unit": "Nanoseconds",
            "range": 8.197619109052588e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d14be87ecdf08196101d33c5a3e40fad230a626d",
          "message": "Merge pull request #4336 from IntersectMBO/PR-toSet\n\n`constrained-generators`: Introduce `fromList_ :: (HasSpec fn a, Ord a) => Term fn [a] -> Term fn (Set a)`",
          "timestamp": "2024-05-22T17:32:15+02:00",
          "tree_id": "f95a49895e4ddf3c34f56a00f21d856061351427",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/d14be87ecdf08196101d33c5a3e40fad230a626d"
        },
        "date": 1716392106565,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006612734174869344,
            "unit": "Nanoseconds",
            "range": 7.249176804120856e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007070956444200226,
            "unit": "Nanoseconds",
            "range": 7.038133929448395e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008231709191741941,
            "unit": "Nanoseconds",
            "range": 0.000001660351267641986
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011394757383209119,
            "unit": "Nanoseconds",
            "range": 0.0000014724392538127993
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010896519020515626,
            "unit": "Nanoseconds",
            "range": 2.0440606268078722e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001770621722997366,
            "unit": "Nanoseconds",
            "range": 3.139677488646298e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018466584398292154,
            "unit": "Nanoseconds",
            "range": 6.70525343274512e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008945299795395131,
            "unit": "Nanoseconds",
            "range": 8.321401081707611e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "db1bc03a6f598a25b0254836acd46ec1f2873d1c",
          "message": "Merge pull request #4339 from IntersectMBO/PR-constrained-distributions\n\n`constrained-generators`: introduce tools for controlling test case distribution",
          "timestamp": "2024-05-23T13:40:59+02:00",
          "tree_id": "e9ec227de871109a029dfd930b9923843158634b",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/db1bc03a6f598a25b0254836acd46ec1f2873d1c"
        },
        "date": 1716464625127,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006655677120861641,
            "unit": "Nanoseconds",
            "range": 0.0000034637378128949516
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006945142376897386,
            "unit": "Nanoseconds",
            "range": 9.860037035388552e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007988156589551551,
            "unit": "Nanoseconds",
            "range": 6.134613085888274e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011301148203210101,
            "unit": "Nanoseconds",
            "range": 0.000005338657374536882
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010478044058088876,
            "unit": "Nanoseconds",
            "range": 2.8118609585188826e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017233784460743227,
            "unit": "Nanoseconds",
            "range": 2.444713701746478e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017885342116739102,
            "unit": "Nanoseconds",
            "range": 6.996128896631893e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008787325401866018,
            "unit": "Nanoseconds",
            "range": 8.869479935744782e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "aniketd@users.noreply.github.com",
            "name": "Aniket Deshpande",
            "username": "aniketd"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e6b6d4f85fb72b5cb5b5361e534d3bb71bb9e55e",
          "message": "Merge pull request #4348 from IntersectMBO/aniketd/conformance-govcert\n\nConformance: GOVCERT",
          "timestamp": "2024-05-23T20:01:41+05:30",
          "tree_id": "eb637f47410eba674633f5871240abfe8d8be383",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/e6b6d4f85fb72b5cb5b5361e534d3bb71bb9e55e"
        },
        "date": 1716474875592,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006632603427625471,
            "unit": "Nanoseconds",
            "range": 0.000004167506299971998
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007083766195187931,
            "unit": "Nanoseconds",
            "range": 0.0000034816034879222904
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000805762500394098,
            "unit": "Nanoseconds",
            "range": 0.0000026464083458104384
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011130204648933441,
            "unit": "Nanoseconds",
            "range": 6.154905780082606e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010601223020628006,
            "unit": "Nanoseconds",
            "range": 1.710592660104266e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017576181798019404,
            "unit": "Nanoseconds",
            "range": 1.2215056194961935e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017933056176143638,
            "unit": "Nanoseconds",
            "range": 1.6424306519793354e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008818773575723577,
            "unit": "Nanoseconds",
            "range": 8.992419871585949e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "4605be31f9d5e78c7ede5d85e3ec19d49db1f5bb",
          "message": "Merge pull request #4350 from IntersectMBO/lehins/inject-id\n\nAdd identity instance for `Inject`",
          "timestamp": "2024-05-23T11:03:27-06:00",
          "tree_id": "86d7cbb529bd872a2360c5b51acf6b2287aa53eb",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/4605be31f9d5e78c7ede5d85e3ec19d49db1f5bb"
        },
        "date": 1716483984165,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006822504810677271,
            "unit": "Nanoseconds",
            "range": 0.000008365483286812237
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007049121392170931,
            "unit": "Nanoseconds",
            "range": 5.406645467266026e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000815345467508661,
            "unit": "Nanoseconds",
            "range": 0.0000019322491527599083
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011247794732130739,
            "unit": "Nanoseconds",
            "range": 0.0000016060361546491792
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010703191033576892,
            "unit": "Nanoseconds",
            "range": 3.011581609891378e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.0000177645031105055,
            "unit": "Nanoseconds",
            "range": 2.515563497749322e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018105275091562577,
            "unit": "Nanoseconds",
            "range": 1.539226316098077e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000908711162909787,
            "unit": "Nanoseconds",
            "range": 8.810930074288064e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "b18a293a3e78863237fb6bba7b12ba0d7aa43071",
          "message": "Merge pull request #4352 from IntersectMBO/erikd/prettyprinter\n\nRemove dependency on deprecated ansi-wl-print package",
          "timestamp": "2024-05-23T13:33:01-06:00",
          "tree_id": "613ce933dbc5b978b0bf4ab0990a1e938b168af3",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/b18a293a3e78863237fb6bba7b12ba0d7aa43071"
        },
        "date": 1716492955449,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006638028157920078,
            "unit": "Nanoseconds",
            "range": 0.000006801199335958897
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006904010773043433,
            "unit": "Nanoseconds",
            "range": 9.198279981531927e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007909767087810486,
            "unit": "Nanoseconds",
            "range": 0.0000020179948195178814
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011060500511827434,
            "unit": "Nanoseconds",
            "range": 0.0000033365773721405605
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010540427403128511,
            "unit": "Nanoseconds",
            "range": 6.223688387242545e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017394075833832266,
            "unit": "Nanoseconds",
            "range": 3.5084860384793896e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017966991637697406,
            "unit": "Nanoseconds",
            "range": 7.140179244724013e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008728466115291963,
            "unit": "Nanoseconds",
            "range": 1.128448224091536e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "teodora.danciu@tweag.io",
            "name": "teodanciu",
            "username": "teodanciu"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "dcf9280ea63524b4f70dbee1731538323dab1c2f",
          "message": "Merge pull request #4213 from IntersectMBO/td/utxow-predfailures-tests\n\nUtxow Predicate failure tests",
          "timestamp": "2024-05-23T22:52:13+01:00",
          "tree_id": "2ea68caca7f4ba63cac35070d71799c0a15e0368",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/dcf9280ea63524b4f70dbee1731538323dab1c2f"
        },
        "date": 1716501299667,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006717866425876141,
            "unit": "Nanoseconds",
            "range": 0.000004840197976814562
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006938083119721074,
            "unit": "Nanoseconds",
            "range": 0.0000018364195457529026
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000799329714760496,
            "unit": "Nanoseconds",
            "range": 0.000001517393322958943
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011102001290294669,
            "unit": "Nanoseconds",
            "range": 0.0000012860927166058322
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.0000108821184908169,
            "unit": "Nanoseconds",
            "range": 2.0274079767568677e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017615404375205324,
            "unit": "Nanoseconds",
            "range": 7.37418939491538e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001797850964539601,
            "unit": "Nanoseconds",
            "range": 1.8969056018885075e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008933559616838511,
            "unit": "Nanoseconds",
            "range": 1.0181612531660929e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "neil.mayhew@iohk.io",
            "name": "Neil Mayhew",
            "username": "neilmayhew"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "509a6e3f168b0d8a8ef1e8c77716ce6f9df2607a",
          "message": "Merge pull request #4351 from IntersectMBO/neilmayhew/retry-flaky-tests\n\nEnable retrying flaky tests in nightly CI",
          "timestamp": "2024-05-23T19:40:44-06:00",
          "tree_id": "4dfad167f64c0a17169473ca0c1a71c3845efecd",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/509a6e3f168b0d8a8ef1e8c77716ce6f9df2607a"
        },
        "date": 1716515012696,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006503597104017339,
            "unit": "Nanoseconds",
            "range": 0.000003030045811539307
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006728777917152326,
            "unit": "Nanoseconds",
            "range": 7.93248533969859e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007846875928692157,
            "unit": "Nanoseconds",
            "range": 0.0000017067892614986185
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.0001112183578447423,
            "unit": "Nanoseconds",
            "range": 8.714299772865534e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010708404686954811,
            "unit": "Nanoseconds",
            "range": 1.1954471255249361e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000016963426127117496,
            "unit": "Nanoseconds",
            "range": 2.3321513955533244e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017793024578147227,
            "unit": "Nanoseconds",
            "range": 1.867248204665907e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008866302848377558,
            "unit": "Nanoseconds",
            "range": 1.478148340783231e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "andre.knispel@gmx.de",
            "name": "Andre Knispel",
            "username": "WhatisRT"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "4298b7cf8020e620a5dca41ddf0936712d2864d1",
          "message": "Merge pull request #4355 from IntersectMBO/andre/spec-fix\n\nFix order of arguments to verifyVrf",
          "timestamp": "2024-05-24T14:12:43+01:00",
          "tree_id": "07b27eaaafb5b573a17ed2860aa5d050947cead5",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/4298b7cf8020e620a5dca41ddf0936712d2864d1"
        },
        "date": 1716556531803,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006575112353503175,
            "unit": "Nanoseconds",
            "range": 6.243286841695454e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007009768451242457,
            "unit": "Nanoseconds",
            "range": 0.0000023212460151170808
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008040415548553563,
            "unit": "Nanoseconds",
            "range": 9.70419044391349e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011169972105765621,
            "unit": "Nanoseconds",
            "range": 0.0000016403813520703206
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010527994891223662,
            "unit": "Nanoseconds",
            "range": 2.3881891307010875e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.0000177418638276112,
            "unit": "Nanoseconds",
            "range": 2.552866065231891e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001830987481782324,
            "unit": "Nanoseconds",
            "range": 3.9305799256034345e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009029075858569888,
            "unit": "Nanoseconds",
            "range": 8.065564371031125e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "fd46bf1c7891c68c6c47e009b659052a9fc64915",
          "message": "Merge pull request #4344 from IntersectMBO/PR-fix-ifElseMultiple\n\n`constrained-generators`: identify and fix an issue with big bodies to ifElse",
          "timestamp": "2024-05-24T17:13:24+02:00",
          "tree_id": "1bcb241731ea6a4a3273f73587806b034d1f4d18",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/fd46bf1c7891c68c6c47e009b659052a9fc64915"
        },
        "date": 1716563771018,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006823095614093833,
            "unit": "Nanoseconds",
            "range": 0.000006554011218718842
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007112947553731688,
            "unit": "Nanoseconds",
            "range": 0.0000011767277370170383
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008104184940164196,
            "unit": "Nanoseconds",
            "range": 4.920752938219647e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011333501077317528,
            "unit": "Nanoseconds",
            "range": 9.412774311255025e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011838905123680595,
            "unit": "Nanoseconds",
            "range": 2.3069044889081393e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001927891654273008,
            "unit": "Nanoseconds",
            "range": 2.4443555417491294e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019322319929215714,
            "unit": "Nanoseconds",
            "range": 2.567929390888573e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009914213833201488,
            "unit": "Nanoseconds",
            "range": 1.4835341220302127e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "joosep.jaager@iohk.io",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "committer": {
            "email": "joosep.jaager@gmail.com",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "distinct": true,
          "id": "d02e935c540935f5f94bc71789259afae28a314c",
          "message": "Added conformance test for ENACT",
          "timestamp": "2024-05-27T15:06:31Z",
          "tree_id": "1c68cacfb5d64181608caed1706fc3782ef7a9ae",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/d02e935c540935f5f94bc71789259afae28a314c"
        },
        "date": 1716822832473,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006890022499992106,
            "unit": "Nanoseconds",
            "range": 0.00001010710717470062
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.0000694315254573822,
            "unit": "Nanoseconds",
            "range": 3.30586542407405e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008074120674936178,
            "unit": "Nanoseconds",
            "range": 3.5618772956716303e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011175941187040386,
            "unit": "Nanoseconds",
            "range": 6.159863693301017e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001073461931486866,
            "unit": "Nanoseconds",
            "range": 8.610127644266836e-8
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017497955384060054,
            "unit": "Nanoseconds",
            "range": 1.4640946829283655e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018011648733439856,
            "unit": "Nanoseconds",
            "range": 1.9445066958259534e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009051870517667732,
            "unit": "Nanoseconds",
            "range": 8.258794721972406e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "joosep.jaager@iohk.io",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "committer": {
            "email": "joosep.jaager@gmail.com",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "distinct": true,
          "id": "cf406305bea1d89a6eb9fe89e76035bf0d3cd0e9",
          "message": "Added conformance test for DELEG",
          "timestamp": "2024-05-27T16:50:36Z",
          "tree_id": "1a0330363c5dba4628e9e02e189877f4d4e74ceb",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/cf406305bea1d89a6eb9fe89e76035bf0d3cd0e9"
        },
        "date": 1716828810568,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006747718755133741,
            "unit": "Nanoseconds",
            "range": 5.035306865971121e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007233884307734016,
            "unit": "Nanoseconds",
            "range": 0.0000019124440310463482
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008131707637106095,
            "unit": "Nanoseconds",
            "range": 7.88180615004564e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011467222657580813,
            "unit": "Nanoseconds",
            "range": 9.40689672519251e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001169363865734926,
            "unit": "Nanoseconds",
            "range": 3.1216892328398106e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001903175095853344,
            "unit": "Nanoseconds",
            "range": 1.8872965038865675e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019635500522294276,
            "unit": "Nanoseconds",
            "range": 2.1408468170148503e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009888420171905567,
            "unit": "Nanoseconds",
            "range": 1.6559476738522142e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "joosep.jaager@iohk.io",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "committer": {
            "email": "joosep.jaager@gmail.com",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "distinct": true,
          "id": "a48af5947a61271ba149d085403cd77638e4c8a1",
          "message": "Fixed a bug in RATIFY",
          "timestamp": "2024-05-28T10:46:25Z",
          "tree_id": "312ebe5ff590d30fa6f4b1815bb67844c2b0c948",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/a48af5947a61271ba149d085403cd77638e4c8a1"
        },
        "date": 1716893875849,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006647251903982799,
            "unit": "Nanoseconds",
            "range": 6.862193894951052e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007135231406416746,
            "unit": "Nanoseconds",
            "range": 0.0000011427160709923393
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008155897338420785,
            "unit": "Nanoseconds",
            "range": 5.241454674131651e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011570836066184991,
            "unit": "Nanoseconds",
            "range": 7.942199902832695e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011891239136391526,
            "unit": "Nanoseconds",
            "range": 1.5275050381523693e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001947246923551824,
            "unit": "Nanoseconds",
            "range": 1.9141733934033933e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019592568930531417,
            "unit": "Nanoseconds",
            "range": 2.6397426066268914e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009883790343332192,
            "unit": "Nanoseconds",
            "range": 1.3574546338318973e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "739af9e23c11ac17191c1d72908adbccd2dd47ba",
          "message": "Merge pull request #4345 from IntersectMBO/PR-explanation\n\n`constrained-generators`: Add `explanation :: [String] -> Pred fn -> Pred fn`",
          "timestamp": "2024-05-28T16:35:15+02:00",
          "tree_id": "cbc5c94180eb8ec0749cc61eecd67b90eaeeac3e",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/739af9e23c11ac17191c1d72908adbccd2dd47ba"
        },
        "date": 1716907086405,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006777608227976401,
            "unit": "Nanoseconds",
            "range": 0.000005199242011313051
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006965664035429671,
            "unit": "Nanoseconds",
            "range": 5.942307623632649e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008042466245485127,
            "unit": "Nanoseconds",
            "range": 0.000004020228220880597
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.0001130585253706718,
            "unit": "Nanoseconds",
            "range": 0.000002780649868038765
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010709313487423019,
            "unit": "Nanoseconds",
            "range": 1.8257802555174217e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001830133961286216,
            "unit": "Nanoseconds",
            "range": 0.0000010303341753162155
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018702175560423256,
            "unit": "Nanoseconds",
            "range": 1.6946302086403748e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009221793230024134,
            "unit": "Nanoseconds",
            "range": 7.628183904313382e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "joosep.jaager@iohk.io",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "committer": {
            "email": "joosep.jaager@gmail.com",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "distinct": true,
          "id": "12253dda0962f46af045b6bafe3f59187492bf13",
          "message": "Added threshold translation",
          "timestamp": "2024-05-28T16:57:09Z",
          "tree_id": "4f6f265e13c244d61bbc70d94c6941fae4735625",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/12253dda0962f46af045b6bafe3f59187492bf13"
        },
        "date": 1716915598733,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006752733308361317,
            "unit": "Nanoseconds",
            "range": 6.932438203371898e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007075410312536793,
            "unit": "Nanoseconds",
            "range": 4.008913595347363e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000818889164810318,
            "unit": "Nanoseconds",
            "range": 5.665804969398455e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.0001150458164650187,
            "unit": "Nanoseconds",
            "range": 8.008659113298395e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011895098362949634,
            "unit": "Nanoseconds",
            "range": 1.6749616664700365e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019308923094846206,
            "unit": "Nanoseconds",
            "range": 3.6297535818796697e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019687990189524928,
            "unit": "Nanoseconds",
            "range": 3.3882389529788577e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009803823103345474,
            "unit": "Nanoseconds",
            "range": 1.2980390118980965e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "b3b8b3d34742374319e7afef236ed1d29effd6b9",
          "message": "Merge pull request #4367 from IntersectMBO/jj/redundant-constraints-fix\n\nFixed `8.10` not building",
          "timestamp": "2024-05-28T20:33:33-06:00",
          "tree_id": "4b358cd6b9c66d899bc05187cb340c2e26f2e66e",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/b3b8b3d34742374319e7afef236ed1d29effd6b9"
        },
        "date": 1716950180916,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00007401445059957421,
            "unit": "Nanoseconds",
            "range": 0.000009974243982209935
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007125116623705373,
            "unit": "Nanoseconds",
            "range": 8.671970687869609e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008097497392185884,
            "unit": "Nanoseconds",
            "range": 6.448273063836379e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011340795837142652,
            "unit": "Nanoseconds",
            "range": 0.0000028095184782089985
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011769580662735038,
            "unit": "Nanoseconds",
            "range": 1.9188204534901974e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019266796010970732,
            "unit": "Nanoseconds",
            "range": 7.542724383616677e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001915404497094864,
            "unit": "Nanoseconds",
            "range": 7.720803059331676e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009780903255613081,
            "unit": "Nanoseconds",
            "range": 1.8236684750542587e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "neil.mayhew@iohk.io",
            "name": "Neil Mayhew",
            "username": "neilmayhew"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e30e474370f4c3ca55a0a67b4e71bab7aca22a26",
          "message": "Ensure GitHub CI fails when tests are skipped due to a build failure (#4368)",
          "timestamp": "2024-05-29T11:16:22Z",
          "tree_id": "1c0e975f4db5d97a152d7fefa2ba264d88636adc",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/e30e474370f4c3ca55a0a67b4e71bab7aca22a26"
        },
        "date": 1716981551326,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006702129613096713,
            "unit": "Nanoseconds",
            "range": 0.0000030221194486693645
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007221191530436249,
            "unit": "Nanoseconds",
            "range": 5.764801455862924e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008103709880740021,
            "unit": "Nanoseconds",
            "range": 0.0000018239481321674086
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011338501602895141,
            "unit": "Nanoseconds",
            "range": 0.000005518666685644058
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011799231537066211,
            "unit": "Nanoseconds",
            "range": 1.8390210547922447e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019344387589736815,
            "unit": "Nanoseconds",
            "range": 6.623954746610881e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001942069497110481,
            "unit": "Nanoseconds",
            "range": 2.406881365015383e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000977352416991848,
            "unit": "Nanoseconds",
            "range": 1.6772623729968594e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "m.algehed@gmail.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "distinct": true,
          "id": "96fe94fded691aa45c0f8dda4a6d0b821009b003",
          "message": "`constrained-generators`: Add `flip_` to avoid having to add new native\nfunctions",
          "timestamp": "2024-05-29T18:01:48+02:00",
          "tree_id": "24bceb8f4ce532b554516abaebfbed34b06fdf41",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/96fe94fded691aa45c0f8dda4a6d0b821009b003"
        },
        "date": 1716998679214,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006476950344740166,
            "unit": "Nanoseconds",
            "range": 9.367681603275113e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.0000686068527552763,
            "unit": "Nanoseconds",
            "range": 3.422189553039274e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007825330980390544,
            "unit": "Nanoseconds",
            "range": 4.4548563655074077e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011240861317808002,
            "unit": "Nanoseconds",
            "range": 8.521009169698905e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010567407506389612,
            "unit": "Nanoseconds",
            "range": 9.356466792596964e-8
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017181756135285932,
            "unit": "Nanoseconds",
            "range": 2.6845772484797163e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017474858106915124,
            "unit": "Nanoseconds",
            "range": 1.6869538718885877e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008936250395529471,
            "unit": "Nanoseconds",
            "range": 1.2485925173116906e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "af944f93b2578081a320f61189fdb432912facb2",
          "message": "Merge pull request #4377 from IntersectMBO/PR-linearize-better\n\n`constrained-generators`: propagate information backwards in the solver",
          "timestamp": "2024-05-30T12:56:43+02:00",
          "tree_id": "11c97648e4355c68124cafd61d37508a7be4a2bf",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/af944f93b2578081a320f61189fdb432912facb2"
        },
        "date": 1717066768809,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006627604832590746,
            "unit": "Nanoseconds",
            "range": 0.0000016810316777145516
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007159857072665165,
            "unit": "Nanoseconds",
            "range": 0.000002052215802933249
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007905090796325242,
            "unit": "Nanoseconds",
            "range": 6.058121061273633e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.0001118582795657488,
            "unit": "Nanoseconds",
            "range": 0.0000012126119529887197
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011687255158654544,
            "unit": "Nanoseconds",
            "range": 1.3030232991986616e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019188065196823627,
            "unit": "Nanoseconds",
            "range": 2.1176371871115833e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019163823728942366,
            "unit": "Nanoseconds",
            "range": 2.0024202851420175e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009800155909294248,
            "unit": "Nanoseconds",
            "range": 1.202739172678266e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "neil.mayhew@iohk.io",
            "name": "Neil Mayhew",
            "username": "neilmayhew"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "6313cb583f17353a67222e7b0a0e059972f85d94",
          "message": "Merge pull request #4373 from IntersectMBO/neilmayhew/ci-memory-limit\n\nAdd -rtsopts to all test suites",
          "timestamp": "2024-05-30T13:44:51-06:00",
          "tree_id": "115dbfde2269f8e47a36592dea5814234cfe11c2",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/6313cb583f17353a67222e7b0a0e059972f85d94"
        },
        "date": 1717098459673,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006781322612241946,
            "unit": "Nanoseconds",
            "range": 0.000003858342217024635
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006967882518525696,
            "unit": "Nanoseconds",
            "range": 3.885148001206363e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000816902412647968,
            "unit": "Nanoseconds",
            "range": 0.000006469746811777247
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011456799640412094,
            "unit": "Nanoseconds",
            "range": 0.000003325392891475355
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010826041544035659,
            "unit": "Nanoseconds",
            "range": 1.74471038874652e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017425180865202806,
            "unit": "Nanoseconds",
            "range": 2.767503928257266e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001812152280883613,
            "unit": "Nanoseconds",
            "range": 2.506925307025166e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000915786378504164,
            "unit": "Nanoseconds",
            "range": 1.0352766760893694e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "aniketd@users.noreply.github.com",
            "name": "Aniket Deshpande",
            "username": "aniketd"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e85410c8caed92b451e8657ed1465fc9a11c4930",
          "message": "DRep expiry update with number of dormant epochs (#4358)\n\n- Fix DRep expiry updates based on numDormantEpochs\r\n- Remove DRepPulser.dormantEpoch as it is no longer required.\r\n- Add updateDormantDRepExpiry to only update active DRep expiries.\r\n- Fix updateNumDormantEpochs to take current proposals, rather than old ones.\r\n- In GOVCERT, make ConwayUpdateDRep calculate (epochNo + drepActivity - numDormantEpochs).\r\n- Add relevant tests.\r\n- Add `vsActualDRepExpiry`.\r\n- Consider that effective DRep expiry is always drepExpiry + numDormantEpochs at all places.\r\n- Make resetting numDormantEpochs part of update function\r\n- Refactor updating DRep expiry\r\n- Add passNEpochsChecking",
          "timestamp": "2024-05-30T21:38:09Z",
          "tree_id": "9718a918d3ce3deca86d7a0bcf85103370a0f4cb",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/e85410c8caed92b451e8657ed1465fc9a11c4930"
        },
        "date": 1717105258912,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006841383090396655,
            "unit": "Nanoseconds",
            "range": 0.000004661287822652332
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007100565982840723,
            "unit": "Nanoseconds",
            "range": 0.0000031342202608628837
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008256680761413146,
            "unit": "Nanoseconds",
            "range": 8.488931150517672e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011320939359167375,
            "unit": "Nanoseconds",
            "range": 9.556273970965482e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011103652944217405,
            "unit": "Nanoseconds",
            "range": 0.0000013648260108126199
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018046199828162948,
            "unit": "Nanoseconds",
            "range": 0.0000010625939224304668
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018756193624249712,
            "unit": "Nanoseconds",
            "range": 0.0000018428964949379788
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009363033582566632,
            "unit": "Nanoseconds",
            "range": 7.701241455191294e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "1ae4e302018c949219cbd804f4894deec4065fce",
          "message": "Merge pull request #4365 from IntersectMBO/ldan/spo-stake-distr-query\n\nCreate pool stake distribution query for voting",
          "timestamp": "2024-06-01T10:31:02-06:00",
          "tree_id": "043f65ed35975aa142e4b0a0af8e8a9abd915c58",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/1ae4e302018c949219cbd804f4894deec4065fce"
        },
        "date": 1717259645590,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006636497458712563,
            "unit": "Nanoseconds",
            "range": 0.0000012148667832304439
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006937195058392602,
            "unit": "Nanoseconds",
            "range": 8.348390167816092e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000806018349071564,
            "unit": "Nanoseconds",
            "range": 0.0000021315356478591367
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011240302847158254,
            "unit": "Nanoseconds",
            "range": 0.0000010289973605217585
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001068314799881987,
            "unit": "Nanoseconds",
            "range": 2.2469195118434973e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017481956817806888,
            "unit": "Nanoseconds",
            "range": 2.32654835758039e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018233740874387725,
            "unit": "Nanoseconds",
            "range": 1.7153873483920022e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000900337975522134,
            "unit": "Nanoseconds",
            "range": 1.447486767160279e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Lucsanszky@users.noreply.github.com",
            "name": "Lucsanszky",
            "username": "Lucsanszky"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0cf058223210a9c6641b94892c86d95a755c9c91",
          "message": "Merge pull request #4376 from IntersectMBO/ldan/ignore-committeeminsize-bootstrap\n\nIgnore `ppCommitteeMinSize` during bootstrap\r\n\r\nResolves #4359",
          "timestamp": "2024-06-02T20:00:32+02:00",
          "tree_id": "b9a481b0e5644b8fa01f6acfe71c3bd9f470b8e0",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/0cf058223210a9c6641b94892c86d95a755c9c91"
        },
        "date": 1717351406779,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.0000673462885912813,
            "unit": "Nanoseconds",
            "range": 0.0000033845576758264027
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006889774203867133,
            "unit": "Nanoseconds",
            "range": 6.90688933855893e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008006933025580176,
            "unit": "Nanoseconds",
            "range": 3.514879252115979e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.0001129549591401301,
            "unit": "Nanoseconds",
            "range": 0.0000022135016944368154
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010842996851983896,
            "unit": "Nanoseconds",
            "range": 1.789409719894767e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017887637532898737,
            "unit": "Nanoseconds",
            "range": 1.7254540331089624e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018754151175254203,
            "unit": "Nanoseconds",
            "range": 3.312344828833524e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009049946987399912,
            "unit": "Nanoseconds",
            "range": 7.9157381198897e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "aniketd@users.noreply.github.com",
            "name": "Aniket Deshpande",
            "username": "aniketd"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "fdd6a9a402a2831fa3381686b5bf5ba1b4d2144e",
          "message": "DRep state query and related tests (#4364)\n\n\r\n* DRepState query now returns the correct expiry, regardless of the filtering: fixes #4349",
          "timestamp": "2024-06-03T05:05:14Z",
          "tree_id": "bdd9991a1cb256d732f98663174c00700e44186e",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/fdd6a9a402a2831fa3381686b5bf5ba1b4d2144e"
        },
        "date": 1717391283079,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006628691397155788,
            "unit": "Nanoseconds",
            "range": 0.000003714591450439179
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006988361989992899,
            "unit": "Nanoseconds",
            "range": 0.0000013981869643775274
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000813768044389954,
            "unit": "Nanoseconds",
            "range": 4.355398298112102e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011421713036041442,
            "unit": "Nanoseconds",
            "range": 0.0000010175745510439933
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010796353612307202,
            "unit": "Nanoseconds",
            "range": 1.9110602682077565e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017950285077894077,
            "unit": "Nanoseconds",
            "range": 5.283927437033567e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001843397645296036,
            "unit": "Nanoseconds",
            "range": 1.2484508794266064e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009013040913218627,
            "unit": "Nanoseconds",
            "range": 1.479030188946103e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "teodora.danciu@tweag.io",
            "name": "teodanciu",
            "username": "teodanciu"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "efc9e8821235f5ae075461f9b710db9c2a52a7e5",
          "message": "Merge pull request #4357 from IntersectMBO/td/improve-conway-cbor\n\nSpecify numeric ranges explicitly in conway cddl files",
          "timestamp": "2024-06-05T15:21:09+01:00",
          "tree_id": "61cb856ae46ad9ac38f58ec2c7b355cc26289b57",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/efc9e8821235f5ae075461f9b710db9c2a52a7e5"
        },
        "date": 1717597457897,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006683232010786662,
            "unit": "Nanoseconds",
            "range": 0.000004966005476978033
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006925116509018709,
            "unit": "Nanoseconds",
            "range": 0.000002726155351932324
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007854525252346386,
            "unit": "Nanoseconds",
            "range": 9.7174535581233e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.0001116708912552702,
            "unit": "Nanoseconds",
            "range": 0.000004683034486430965
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010267669642377592,
            "unit": "Nanoseconds",
            "range": 2.054510865593079e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001734654999153308,
            "unit": "Nanoseconds",
            "range": 6.464287701370762e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001777335204336874,
            "unit": "Nanoseconds",
            "range": 4.933773287427832e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008993339330842164,
            "unit": "Nanoseconds",
            "range": 6.254453363334939e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "a5d74bcb642c5b8fb7ce474839285161780fea14",
          "message": "Merge pull request #4389 from IntersectMBO/ts-fixissue-4340\n\nFixed issue #4340. Problem with futurePParams not adequate in Conway.",
          "timestamp": "2024-06-05T10:46:09-06:00",
          "tree_id": "e8e35dadd04ae773c8fb2d2e70e3b295f16b9319",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/a5d74bcb642c5b8fb7ce474839285161780fea14"
        },
        "date": 1717606146876,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006499796097836942,
            "unit": "Nanoseconds",
            "range": 0.0000033792951097492916
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006843421643938797,
            "unit": "Nanoseconds",
            "range": 0.0000017828490540297588
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007761687108704216,
            "unit": "Nanoseconds",
            "range": 0.00000167926705639243
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010864603778504022,
            "unit": "Nanoseconds",
            "range": 0.0000011359881782515863
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010418007858374912,
            "unit": "Nanoseconds",
            "range": 1.0961224084419176e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017453526312911893,
            "unit": "Nanoseconds",
            "range": 6.496541952287691e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001781148139481062,
            "unit": "Nanoseconds",
            "range": 1.6040990103764974e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008955787819967577,
            "unit": "Nanoseconds",
            "range": 9.446303903631138e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "c1afafd7ea02d045ca0f33bd370ec98d33612226",
          "message": "Merge pull request #4378 from IntersectMBO/aniketd/conformance-pools\n\nConformance: POOL",
          "timestamp": "2024-06-05T13:38:31-06:00",
          "tree_id": "c49fe8133212b8e2d7a113ba90f7127a7ec69cb6",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/c1afafd7ea02d045ca0f33bd370ec98d33612226"
        },
        "date": 1717616509772,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006642465435531212,
            "unit": "Nanoseconds",
            "range": 0.0000035661552149298023
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006876504276877082,
            "unit": "Nanoseconds",
            "range": 0.0000012136713246281356
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007926409304660471,
            "unit": "Nanoseconds",
            "range": 7.269926446430302e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011190689631951145,
            "unit": "Nanoseconds",
            "range": 0.000001116668219001277
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001072759100643691,
            "unit": "Nanoseconds",
            "range": 2.2184730115151878e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001815687748130702,
            "unit": "Nanoseconds",
            "range": 2.0715252958308985e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.0000184000689353979,
            "unit": "Nanoseconds",
            "range": 2.4672841218482513e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009275670385722532,
            "unit": "Nanoseconds",
            "range": 7.751585631361601e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "joosep.jaager@iohk.io",
            "name": "Joosep Jääger",
            "username": "Soupstraw"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "abaf27d5bd14883a799cbda8011b583f3f3819a0",
          "message": "Improved the translation of `PParamUpdate` in conformance (#4388)\n\n* Improved the translation of PParamUpdate in conformance",
          "timestamp": "2024-06-05T21:28:58Z",
          "tree_id": "c6fb21016a50d68eb606372d9f8fff82643a7d67",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/abaf27d5bd14883a799cbda8011b583f3f3819a0"
        },
        "date": 1717623110497,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006604032464999593,
            "unit": "Nanoseconds",
            "range": 0.0000032687959834467454
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006984826500024224,
            "unit": "Nanoseconds",
            "range": 0.000001598789206200553
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008071221540876195,
            "unit": "Nanoseconds",
            "range": 0.000003280521598035114
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011247430065124231,
            "unit": "Nanoseconds",
            "range": 0.0000011322274085310908
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010813041835984385,
            "unit": "Nanoseconds",
            "range": 1.4513568434863453e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001809009577094971,
            "unit": "Nanoseconds",
            "range": 2.4118128641083935e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018611641679025177,
            "unit": "Nanoseconds",
            "range": 1.669236693500819e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009244225481166502,
            "unit": "Nanoseconds",
            "range": 7.651154121478346e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "4f60ba531f420bec8b432a400e57e312ed2513e8",
          "message": "Merge pull request #4390 from IntersectMBO/td/costmodels-cddl\n\nFix cddl representing CostModels in Conway",
          "timestamp": "2024-06-05T17:59:05-06:00",
          "tree_id": "d9da5e953b6de77be59ba502364a4ee3413ebfc2",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/4f60ba531f420bec8b432a400e57e312ed2513e8"
        },
        "date": 1717632124275,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006778379943903209,
            "unit": "Nanoseconds",
            "range": 0.000004159555446469972
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007013296358864654,
            "unit": "Nanoseconds",
            "range": 5.278363766971317e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008117439746306588,
            "unit": "Nanoseconds",
            "range": 7.654575757361647e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011347501401919988,
            "unit": "Nanoseconds",
            "range": 0.000006448089119743693
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010967799698209707,
            "unit": "Nanoseconds",
            "range": 2.3762101952065487e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018525070490162,
            "unit": "Nanoseconds",
            "range": 4.164732413854301e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001912637478134425,
            "unit": "Nanoseconds",
            "range": 0.0000015392900738184252
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009358311195593402,
            "unit": "Nanoseconds",
            "range": 1.8348693750524985e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0a098670a7cf0d084c4087d4140f704f82784977",
          "message": "Merge pull request #4384 from IntersectMBO/ts-fixIssue4341\n\nRe-enabled Full NewEpochstate test",
          "timestamp": "2024-06-07T13:17:46+02:00",
          "tree_id": "b5d2fb1e1c9ed4f76fa7dd545168c913d1a7b118",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/0a098670a7cf0d084c4087d4140f704f82784977"
        },
        "date": 1717759230646,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006491562893212609,
            "unit": "Nanoseconds",
            "range": 0.0000011857895437766295
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006872909732083334,
            "unit": "Nanoseconds",
            "range": 0.0000012925214398667424
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007992488236117542,
            "unit": "Nanoseconds",
            "range": 0.0000015738292710377064
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011200389608752676,
            "unit": "Nanoseconds",
            "range": 0.0000020374709645341717
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010766220090421424,
            "unit": "Nanoseconds",
            "range": 2.8691872061424224e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018176968960267576,
            "unit": "Nanoseconds",
            "range": 6.977225027934708e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001806000862398642,
            "unit": "Nanoseconds",
            "range": 1.9285983500216902e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009253570259189397,
            "unit": "Nanoseconds",
            "range": 6.812925142124382e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "ad8e222e41dc6faff7495552e97ef4b3d2892c53",
          "message": "Merge pull request #4394 from IntersectMBO/lehins/fix-certifying-bug\n\nFix Certifying Redeemer issue",
          "timestamp": "2024-06-13T14:25:43+02:00",
          "tree_id": "0833f72ee669825f658502cb9d16a961ea0d41a5",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/ad8e222e41dc6faff7495552e97ef4b3d2892c53"
        },
        "date": 1718281717851,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.0000651321590232309,
            "unit": "Nanoseconds",
            "range": 0.0000032720314150358425
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.0000701772187778681,
            "unit": "Nanoseconds",
            "range": 0.0000014091805090106986
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007743156459957461,
            "unit": "Nanoseconds",
            "range": 3.080576757003775e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011097778687084373,
            "unit": "Nanoseconds",
            "range": 6.555337923464047e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010700774177781953,
            "unit": "Nanoseconds",
            "range": 1.5452045057666908e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001743413896260682,
            "unit": "Nanoseconds",
            "range": 2.2252017219253636e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001800558377032798,
            "unit": "Nanoseconds",
            "range": 1.3768895507036833e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009237171664818052,
            "unit": "Nanoseconds",
            "range": 9.463183101487479e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "1fa149fe456238a1a7a019834eaadbda3fa1908d",
          "message": "Merge pull request #4399 from IntersectMBO/PR-mapSpec-example\n\n`constrained-generators`: new simple example for maps",
          "timestamp": "2024-06-14T12:40:26+02:00",
          "tree_id": "e4ebef69aa107c72103d7ef69092c97d91d62310",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/1fa149fe456238a1a7a019834eaadbda3fa1908d"
        },
        "date": 1718363313434,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006501191430296647,
            "unit": "Nanoseconds",
            "range": 0.000004422282881912392
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006907618519008587,
            "unit": "Nanoseconds",
            "range": 5.552532497322176e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007892517585790659,
            "unit": "Nanoseconds",
            "range": 7.152160995884613e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010999476814324307,
            "unit": "Nanoseconds",
            "range": 0.0000037673917411794407
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00001052340338366302,
            "unit": "Nanoseconds",
            "range": 2.3281954717889483e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017395147248432725,
            "unit": "Nanoseconds",
            "range": 7.959488093825993e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017493471405147123,
            "unit": "Nanoseconds",
            "range": 2.402365599849313e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009160933466792587,
            "unit": "Nanoseconds",
            "range": 6.949951426811756e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "71d5432eded7cf4449767074412be2f3b1191626",
          "message": "Merge pull request #4400 from IntersectMBO/andre/fix-certs\n\nCheck that the pool being delegated to exists for `ConwayDelegCert`",
          "timestamp": "2024-06-14T17:18:32+02:00",
          "tree_id": "3702118f980379e1f48fc2fec2e4fef17fbc28d5",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/71d5432eded7cf4449767074412be2f3b1191626"
        },
        "date": 1718379635270,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006582297140423345,
            "unit": "Nanoseconds",
            "range": 0.0000019172043086370425
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007001847521827869,
            "unit": "Nanoseconds",
            "range": 9.158588819934676e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007944775301929037,
            "unit": "Nanoseconds",
            "range": 0.000001151321085676244
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011133563959202796,
            "unit": "Nanoseconds",
            "range": 0.0000010835686341726668
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010630908902607664,
            "unit": "Nanoseconds",
            "range": 1.1800288118020825e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017487134111007842,
            "unit": "Nanoseconds",
            "range": 1.225292739103051e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018116713353472437,
            "unit": "Nanoseconds",
            "range": 4.584537088217333e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009246004281443699,
            "unit": "Nanoseconds",
            "range": 9.007788903141206e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "5958f3df188d5286746c74d9c39fd58c2838bacd",
          "message": "Merge pull request #4374 from IntersectMBO/lehins/cip-0069\n\nCIP-0069",
          "timestamp": "2024-06-14T23:55:42+02:00",
          "tree_id": "d37a61bf235ed79c72a301c8c134400ea11f61ca",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/5958f3df188d5286746c74d9c39fd58c2838bacd"
        },
        "date": 1718402310395,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006739534266014787,
            "unit": "Nanoseconds",
            "range": 0.000004994623588370798
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007098942771543872,
            "unit": "Nanoseconds",
            "range": 4.0456481305777803e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008442139414487166,
            "unit": "Nanoseconds",
            "range": 0.000005242976973432964
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011552457822944306,
            "unit": "Nanoseconds",
            "range": 0.000004887079536034286
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011355152097997004,
            "unit": "Nanoseconds",
            "range": 7.357682774838234e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018700496873617128,
            "unit": "Nanoseconds",
            "range": 5.079496813821605e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019333557389972573,
            "unit": "Nanoseconds",
            "range": 0.0000011658384270787265
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009526178883795563,
            "unit": "Nanoseconds",
            "range": 2.134728952661764e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "m.algehed@gmail.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "distinct": true,
          "id": "c2f3e26ea7c3a300fd49c5075b26a6c8d582d1b5",
          "message": "`constrained-generators`: Add `lookup_` for maps",
          "timestamp": "2024-06-16T18:46:10+02:00",
          "tree_id": "195b9cd3ea1c0e60d88d629a67ed1f08e86f7f4d",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/c2f3e26ea7c3a300fd49c5075b26a6c8d582d1b5"
        },
        "date": 1718556535387,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006424785392970761,
            "unit": "Nanoseconds",
            "range": 4.78552486626256e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006888327698525401,
            "unit": "Nanoseconds",
            "range": 6.443757433407117e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008025764578177507,
            "unit": "Nanoseconds",
            "range": 6.777908218372652e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011058396739112289,
            "unit": "Nanoseconds",
            "range": 0.000003796505742051506
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010687132465641317,
            "unit": "Nanoseconds",
            "range": 1.208992748637666e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017874617375348373,
            "unit": "Nanoseconds",
            "range": 3.5891701817858e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018488137403684825,
            "unit": "Nanoseconds",
            "range": 1.9455895522633905e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009127973985752108,
            "unit": "Nanoseconds",
            "range": 2.666744628664415e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "neil.mayhew@iohk.io",
            "name": "Neil Mayhew",
            "username": "neilmayhew"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "ada29ced001bfd98bc9b883977b97867e911c664",
          "message": "Merge pull request #4397 from IntersectMBO/neilmayhew/substate\n\nAdd a lens to HasSubState",
          "timestamp": "2024-06-17T11:12:18-06:00",
          "tree_id": "7df9d8cb342e21f445e64c6f54ca1cc46f7fb59f",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/ada29ced001bfd98bc9b883977b97867e911c664"
        },
        "date": 1718644515341,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006790983994793537,
            "unit": "Nanoseconds",
            "range": 0.0000042426706483606254
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007067882038785309,
            "unit": "Nanoseconds",
            "range": 8.452444008781972e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000827936536796526,
            "unit": "Nanoseconds",
            "range": 0.000001324141006135223
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011613592110797727,
            "unit": "Nanoseconds",
            "range": 0.0000015966145798346169
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011372929016027281,
            "unit": "Nanoseconds",
            "range": 2.182399181884091e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018155459158689102,
            "unit": "Nanoseconds",
            "range": 1.922793769368151e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000019103323926457946,
            "unit": "Nanoseconds",
            "range": 1.633770511254385e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009534209710612449,
            "unit": "Nanoseconds",
            "range": 1.0224223938059609e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "m.algehed@gmail.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "distinct": true,
          "id": "cb30d895b866ccd5d8c529212208aca0c913b9c2",
          "message": "`constrained-generators`: fix the shrinking for `exists` to get better\nfailures",
          "timestamp": "2024-06-17T23:22:28+02:00",
          "tree_id": "6df7f5ba1bd9798cc17cbc938679e07ef1fae26a",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/cb30d895b866ccd5d8c529212208aca0c913b9c2"
        },
        "date": 1718659515457,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006646202856708747,
            "unit": "Nanoseconds",
            "range": 4.338006767495976e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006990147493678076,
            "unit": "Nanoseconds",
            "range": 8.147635522644111e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008129336513961374,
            "unit": "Nanoseconds",
            "range": 6.704854234044189e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011186576612370785,
            "unit": "Nanoseconds",
            "range": 0.000001710052765040972
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010323724101758288,
            "unit": "Nanoseconds",
            "range": 8.073032229838545e-8
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001772424670068843,
            "unit": "Nanoseconds",
            "range": 0.0000016770061334265784
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018089489562624773,
            "unit": "Nanoseconds",
            "range": 5.600151366826371e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009099290295466907,
            "unit": "Nanoseconds",
            "range": 9.274867910956824e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "samuel.leathers@iohk.io",
            "name": "Samuel Leathers",
            "username": "disassembler"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "923e75bf3e16da01f26ed1ab53d9aac7184fc699",
          "message": "Merge pull request #4409 from IntersectMBO/lehins/plutus-master\n\nUpdate to plutus-ledger-api-1.30",
          "timestamp": "2024-06-18T08:40:04-04:00",
          "tree_id": "4c4128bb8ea02f0ab21bd7ac92d3ed93d32591ea",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/923e75bf3e16da01f26ed1ab53d9aac7184fc699"
        },
        "date": 1718714574753,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006571741634468581,
            "unit": "Nanoseconds",
            "range": 5.850532515646332e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006911898403481065,
            "unit": "Nanoseconds",
            "range": 3.152306402989145e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000813491846006734,
            "unit": "Nanoseconds",
            "range": 5.328477333351247e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011252498494420932,
            "unit": "Nanoseconds",
            "range": 0.0000019009283999631373
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010765696221925651,
            "unit": "Nanoseconds",
            "range": 1.9804724961767022e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017914214121141792,
            "unit": "Nanoseconds",
            "range": 4.832864760592261e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018412922784196768,
            "unit": "Nanoseconds",
            "range": 0.0000015188126889060456
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000934444689513512,
            "unit": "Nanoseconds",
            "range": 9.76995922004654e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "dbf36bb69a7a525ba75127b27ed822e16d556661",
          "message": "Merge pull request #4424 from IntersectMBO/neilmayhew/ci-download-artifact-downgrade\n\nGHA: Downgrade the version of actions/upload-artifact",
          "timestamp": "2024-06-18T20:50:28-06:00",
          "tree_id": "c3a2a15fdeb71a5c783e6dc71f78f11cfc426281",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/dbf36bb69a7a525ba75127b27ed822e16d556661"
        },
        "date": 1718765592065,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.0000658761645391232,
            "unit": "Nanoseconds",
            "range": 0.000005634829526397127
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006866365912807924,
            "unit": "Nanoseconds",
            "range": 0.0000012972696324155199
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008129846466626864,
            "unit": "Nanoseconds",
            "range": 0.0000039975357683223075
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011056948244348585,
            "unit": "Nanoseconds",
            "range": 7.422786235626742e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010657791784858918,
            "unit": "Nanoseconds",
            "range": 4.0375814284391914e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017699388148879734,
            "unit": "Nanoseconds",
            "range": 3.5016571080419655e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018066393263965863,
            "unit": "Nanoseconds",
            "range": 3.460218789900689e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009183452613730215,
            "unit": "Nanoseconds",
            "range": 9.36018853539324e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "m.algehed@gmail.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "distinct": true,
          "id": "b6a51dc0181e2217a0e8ffa16dbb886b0d442572",
          "message": "`constrained-generators`: introduce a hook for naming variables using\neither view patterns or quasi quoting",
          "timestamp": "2024-06-19T16:11:42+02:00",
          "tree_id": "3748d58cafaa7232fa5467b541051b35ad88414b",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/b6a51dc0181e2217a0e8ffa16dbb886b0d442572"
        },
        "date": 1718806493289,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00006930829464010455,
            "unit": "Nanoseconds",
            "range": 0.000006129738923958004
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007077324760191234,
            "unit": "Nanoseconds",
            "range": 4.1666772932735664e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008222623516571472,
            "unit": "Nanoseconds",
            "range": 6.442185634493055e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011382361346227865,
            "unit": "Nanoseconds",
            "range": 0.0000012735630012756903
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000011895841502968594,
            "unit": "Nanoseconds",
            "range": 3.07648211665361e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000019901299795031954,
            "unit": "Nanoseconds",
            "range": 3.252813118565853e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001974434428098398,
            "unit": "Nanoseconds",
            "range": 1.6194549570686547e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000010061650747227979,
            "unit": "Nanoseconds",
            "range": 8.816177821139724e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "neil.mayhew@iohk.io",
            "name": "Neil Mayhew",
            "username": "neilmayhew"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "45df347147968267e72f84919baaf52783d10379",
          "message": "Merge pull request #4426 from IntersectMBO/neilmayhew/3990-ghc-9.8-Werror\n\nTake care of all compiler warnings for GHC-9.8",
          "timestamp": "2024-06-19T12:28:24-06:00",
          "tree_id": "ea9c43b0248015972224c263de5cfe50a62d8431",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/45df347147968267e72f84919baaf52783d10379"
        },
        "date": 1718821866077,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.0000668542250810089,
            "unit": "Nanoseconds",
            "range": 0.000004461908398485548
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006968213961013217,
            "unit": "Nanoseconds",
            "range": 0.0000025662664779938163
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008071623969425102,
            "unit": "Nanoseconds",
            "range": 0.00000245220159077873
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011285046269621517,
            "unit": "Nanoseconds",
            "range": 0.000004844494069984802
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010575708624146152,
            "unit": "Nanoseconds",
            "range": 3.438478142790626e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017570621466539613,
            "unit": "Nanoseconds",
            "range": 2.0957748004823166e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018049983839313508,
            "unit": "Nanoseconds",
            "range": 4.3291054972441673e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000009077985004385436,
            "unit": "Nanoseconds",
            "range": 6.749476902303888e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "neil.mayhew@iohk.io",
            "name": "Neil Mayhew",
            "username": "neilmayhew"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0fecea936037695721ef370f61f2c854f71ac954",
          "message": "Merge pull request #4407 from IntersectMBO/neilmayhew/default-ghc\n\nChange the default ghc version to 9.6.5",
          "timestamp": "2024-06-19T14:54:47-06:00",
          "tree_id": "c3f74823f48f8d270c26a32cbcb269e27bc026e9",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/0fecea936037695721ef370f61f2c854f71ac954"
        },
        "date": 1718830650761,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.000054829187404701674,
            "unit": "Nanoseconds",
            "range": 0.0000018395298743413423
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00005719761986229856,
            "unit": "Nanoseconds",
            "range": 3.106993463968659e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006643821422067762,
            "unit": "Nanoseconds",
            "range": 3.613989768901177e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010022381999674607,
            "unit": "Nanoseconds",
            "range": 0.0000010494726357188265
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00000954866150825992,
            "unit": "Nanoseconds",
            "range": 1.512661180133994e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001771143255036777,
            "unit": "Nanoseconds",
            "range": 4.806859805637406e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017839281941172225,
            "unit": "Nanoseconds",
            "range": 1.6734458278360118e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008592171511317886,
            "unit": "Nanoseconds",
            "range": 6.247357987862479e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "49699333+dependabot[bot]@users.noreply.github.com",
            "name": "dependabot[bot]",
            "username": "dependabot[bot]"
          },
          "committer": {
            "email": "neil@mayhew.name",
            "name": "Neil Mayhew",
            "username": "neilmayhew"
          },
          "distinct": true,
          "id": "9e9ec74faad95f14007e02f5e6e9862eff5255be",
          "message": "Bump urllib3 from 1.26.18 to 1.26.19 in /doc\n\nBumps [urllib3](https://github.com/urllib3/urllib3) from 1.26.18 to 1.26.19.\n- [Release notes](https://github.com/urllib3/urllib3/releases)\n- [Changelog](https://github.com/urllib3/urllib3/blob/1.26.19/CHANGES.rst)\n- [Commits](https://github.com/urllib3/urllib3/compare/1.26.18...1.26.19)\n\n---\nupdated-dependencies:\n- dependency-name: urllib3\n  dependency-type: indirect\n...\n\nSigned-off-by: dependabot[bot] <support@github.com>",
          "timestamp": "2024-06-19T16:57:07-06:00",
          "tree_id": "1f33b38c00c4ebbb21892cc0f033f5f1873d4399",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/9e9ec74faad95f14007e02f5e6e9862eff5255be"
        },
        "date": 1718837990011,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.000053762640657779226,
            "unit": "Nanoseconds",
            "range": 2.814415601603489e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00005779135417919595,
            "unit": "Nanoseconds",
            "range": 0.0000017993064251594012
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006667653080435411,
            "unit": "Nanoseconds",
            "range": 5.459631731277235e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010101775961132348,
            "unit": "Nanoseconds",
            "range": 0.0000014813551716893955
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000009535110970279917,
            "unit": "Nanoseconds",
            "range": 2.0036316651607706e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001770068566186526,
            "unit": "Nanoseconds",
            "range": 6.904267465848821e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017990795478656655,
            "unit": "Nanoseconds",
            "range": 2.975230559224852e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000853992255229074,
            "unit": "Nanoseconds",
            "range": 5.716920467545769e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "a44a5b5ac9f68c5d7a2ff38b040a5fcac8ccfc6a",
          "message": "Merge pull request #4429 from IntersectMBO/ldan/post-release\n\nBump `CHANGELOG` versions post-release",
          "timestamp": "2024-06-19T19:44:29-06:00",
          "tree_id": "da81281de430a9bb8fc7553d5690ba40640ebe58",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/a44a5b5ac9f68c5d7a2ff38b040a5fcac8ccfc6a"
        },
        "date": 1718848030560,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.000053860455200674656,
            "unit": "Nanoseconds",
            "range": 7.023972280410554e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00005727579475366674,
            "unit": "Nanoseconds",
            "range": 2.919933390031954e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006708368233637225,
            "unit": "Nanoseconds",
            "range": 0.00000121249900944378
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010103404600144981,
            "unit": "Nanoseconds",
            "range": 5.731766222691432e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000009763077748303078,
            "unit": "Nanoseconds",
            "range": 8.966411832456794e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001771710070803736,
            "unit": "Nanoseconds",
            "range": 8.891002021404965e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018048687337194848,
            "unit": "Nanoseconds",
            "range": 2.698223832116199e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000850472073340364,
            "unit": "Nanoseconds",
            "range": 7.162043909197308e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "m.algehed@gmail.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "committer": {
            "email": "MaximilianAlgehed@users.noreply.github.com",
            "name": "Maximilian Algehed",
            "username": "MaximilianAlgehed"
          },
          "distinct": true,
          "id": "de33cee5bd16f020dd87a8f4af20fb263955ff37",
          "message": "`constrained-generators`: use consistent warning pragma",
          "timestamp": "2024-06-20T08:06:40+02:00",
          "tree_id": "f7a8b589066e130942526abeb038afd2e6c02601",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/de33cee5bd16f020dd87a8f4af20fb263955ff37"
        },
        "date": 1718863764631,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00005427461096682319,
            "unit": "Nanoseconds",
            "range": 5.130741672241256e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.000057250403591549,
            "unit": "Nanoseconds",
            "range": 5.770583967719722e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006760434268676221,
            "unit": "Nanoseconds",
            "range": 4.909336850437056e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010265858674770865,
            "unit": "Nanoseconds",
            "range": 0.0000019876098314567155
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000009401732531453251,
            "unit": "Nanoseconds",
            "range": 3.706003130735855e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017489609724383038,
            "unit": "Nanoseconds",
            "range": 3.08514547333618e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018165641901606992,
            "unit": "Nanoseconds",
            "range": 1.3656259528157209e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008487189447541107,
            "unit": "Nanoseconds",
            "range": 6.540143905960004e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e2aaf98b5ff2f0983059dc6ea9b1378c2112101a",
          "message": "Merge pull request #4431 from IntersectMBO/PR-fix-flaky-tests\n\n`constrained-generators`: fix flakyness in set generator",
          "timestamp": "2024-06-20T09:16:35-06:00",
          "tree_id": "a4131a9c9b53583ecfe7ea8ce61e312a9bb4bb1d",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/e2aaf98b5ff2f0983059dc6ea9b1378c2112101a"
        },
        "date": 1718896760958,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.000055135900635626915,
            "unit": "Nanoseconds",
            "range": 0.00000454823495896703
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00005718305221954997,
            "unit": "Nanoseconds",
            "range": 0.000002374078257344301
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006756356685676513,
            "unit": "Nanoseconds",
            "range": 6.608376390489151e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010191095420858288,
            "unit": "Nanoseconds",
            "range": 0.000002554131974915081
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000010037684812678999,
            "unit": "Nanoseconds",
            "range": 1.4510738926989663e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000018066776630162022,
            "unit": "Nanoseconds",
            "range": 9.192258326268066e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001814541376642868,
            "unit": "Nanoseconds",
            "range": 2.0759082289489477e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008791838520901849,
            "unit": "Nanoseconds",
            "range": 1.0279983358483433e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "42de4423e4cc3dc2b86262419fff8007df69d918",
          "message": "Merge pull request #4430 from IntersectMBO/lehins/costmodel-json-parsing\n\nCostModel json parsing",
          "timestamp": "2024-06-20T12:13:55-06:00",
          "tree_id": "0b368fe01ae9d575df7b0dc2036704a7e8fa4bb7",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/42de4423e4cc3dc2b86262419fff8007df69d918"
        },
        "date": 1718907417294,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00005618929231215462,
            "unit": "Nanoseconds",
            "range": 6.370558272091046e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00006060162432092387,
            "unit": "Nanoseconds",
            "range": 8.728796190919589e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00007058046727300976,
            "unit": "Nanoseconds",
            "range": 0.0000027593238777731586
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010377012622384086,
            "unit": "Nanoseconds",
            "range": 0.0000031996289400308237
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000009659952929410724,
            "unit": "Nanoseconds",
            "range": 4.0793825566587527e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001795547895310259,
            "unit": "Nanoseconds",
            "range": 0.0000013771263591856801
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018165666913871966,
            "unit": "Nanoseconds",
            "range": 1.2180125347869875e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008825059331128026,
            "unit": "Nanoseconds",
            "range": 8.513539213521558e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "ac1beaefec7e945055bdd4c689636db782585e16",
          "message": "Merge pull request #4432 from IntersectMBO/lehins/8.12-changelog\n\n`cardano-node-8.12` ledger changelog",
          "timestamp": "2024-06-20T14:18:44-06:00",
          "tree_id": "94b69265b2d0f99e0c4dc08597c4fcfa2778d802",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/ac1beaefec7e945055bdd4c689636db782585e16"
        },
        "date": 1718914908118,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.000054768957335675524,
            "unit": "Nanoseconds",
            "range": 4.4327049954905727e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00005851172965686867,
            "unit": "Nanoseconds",
            "range": 5.555090440079241e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006874838600355039,
            "unit": "Nanoseconds",
            "range": 0.0000036809153258453497
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010009786727480845,
            "unit": "Nanoseconds",
            "range": 8.965201445340543e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000009449597682284584,
            "unit": "Nanoseconds",
            "range": 3.3465783593670736e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017585909097443434,
            "unit": "Nanoseconds",
            "range": 4.05319316438594e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017925754024527785,
            "unit": "Nanoseconds",
            "range": 4.583677731222023e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008567436060838498,
            "unit": "Nanoseconds",
            "range": 1.4124237418692153e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "neil.mayhew@iohk.io",
            "name": "Neil Mayhew",
            "username": "neilmayhew"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0e3e1c3c96229a00e5dc3ba2ae488c46dcd260e6",
          "message": "Merge pull request #4312 from IntersectMBO/neilmayhew/4180-AlonzoValidTxUTXOW-to-ImpTest\n\nConvert AlonzoValidTxUTXOW to ImpTest",
          "timestamp": "2024-06-20T18:37:05-06:00",
          "tree_id": "a006bfdd7e1d752be8eebeb4537e694d8e9cb8a3",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/0e3e1c3c96229a00e5dc3ba2ae488c46dcd260e6"
        },
        "date": 1718930392633,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.000054642616837579864,
            "unit": "Nanoseconds",
            "range": 5.717425821143043e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.000059039244031609626,
            "unit": "Nanoseconds",
            "range": 6.168803973495294e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.0000677670747802302,
            "unit": "Nanoseconds",
            "range": 3.6101875229536596e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010408985499999841,
            "unit": "Nanoseconds",
            "range": 0.0000010210887080887866
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000009529701464975747,
            "unit": "Nanoseconds",
            "range": 1.0883087968727502e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.0000177004714971617,
            "unit": "Nanoseconds",
            "range": 5.849244804964537e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001800923949512547,
            "unit": "Nanoseconds",
            "range": 3.893691850349377e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008655310518453452,
            "unit": "Nanoseconds",
            "range": 8.728430613144362e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Lucsanszky@users.noreply.github.com",
            "name": "Lucsanszky",
            "username": "Lucsanszky"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "c4fbc05999866fea7c0cb1b211fd5288f286b95d",
          "message": "Increase the size of the committee for testing (#4386)\n\n* Increase committee size to `2` in tests\r\n\r\n* Add helper function for committee yes votes\r\n\r\n* Improve HF initiation w/ `< CommitteeMinSize` test",
          "timestamp": "2024-06-21T14:41:36Z",
          "tree_id": "81c1c8655601a912adf8ea1981d58dc03d6cd924",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/c4fbc05999866fea7c0cb1b211fd5288f286b95d"
        },
        "date": 1718981060141,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00005604884138625991,
            "unit": "Nanoseconds",
            "range": 0.00000302052193228491
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.000058563587423898805,
            "unit": "Nanoseconds",
            "range": 0.0000011379642365908315
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006988229122240255,
            "unit": "Nanoseconds",
            "range": 8.640832461934909e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010321983354179481,
            "unit": "Nanoseconds",
            "range": 0.000001352298899129867
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000009815010278162082,
            "unit": "Nanoseconds",
            "range": 6.042913248163874e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001754022657665778,
            "unit": "Nanoseconds",
            "range": 1.4138513363737074e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017933393909044298,
            "unit": "Nanoseconds",
            "range": 1.1422842273242323e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008655992062963762,
            "unit": "Nanoseconds",
            "range": 1.1383199651162388e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "aniketd@users.noreply.github.com",
            "name": "Aniket Deshpande",
            "username": "aniketd"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "fd5024f9eee0a5cb838173ca1674d688f2a2d142",
          "message": "Merge pull request #4425 from IntersectMBO/aniketd/conformance-epoch\n\nConformance test plumbing: EPOCH",
          "timestamp": "2024-06-24T19:43:41+02:00",
          "tree_id": "d5419d34293c47984ede488c20ca46c00b7b70c7",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/fd5024f9eee0a5cb838173ca1674d688f2a2d142"
        },
        "date": 1719251198397,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.000055377463790488147,
            "unit": "Nanoseconds",
            "range": 0.000004259727212818423
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.000059021511494503636,
            "unit": "Nanoseconds",
            "range": 5.264431210495714e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006776471961171003,
            "unit": "Nanoseconds",
            "range": 0.0000010005971566412455
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010272883945367507,
            "unit": "Nanoseconds",
            "range": 0.000002586900443772217
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000009462690734309533,
            "unit": "Nanoseconds",
            "range": 1.9412246929751617e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017681338231135054,
            "unit": "Nanoseconds",
            "range": 1.8989179228928894e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001805266854079599,
            "unit": "Nanoseconds",
            "range": 1.0782794479459909e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008553072706600764,
            "unit": "Nanoseconds",
            "range": 9.217403041992489e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "aniketd@users.noreply.github.com",
            "name": "Aniket Deshpande",
            "username": "aniketd"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d529098771f9104e5990849107113e58dd032977",
          "message": "Merge pull request #4438 from IntersectMBO/aniketd/update-graphviz-diagrams\n\nFix UTXOW era in diagrams for Conway",
          "timestamp": "2024-06-26T18:05:10+02:00",
          "tree_id": "e61c6636ecd10aa372219ee015a2328def047cd7",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/d529098771f9104e5990849107113e58dd032977"
        },
        "date": 1719418087402,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00005559685307305862,
            "unit": "Nanoseconds",
            "range": 0.0000025783181987945797
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.000059107238357828756,
            "unit": "Nanoseconds",
            "range": 7.597744918436189e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006976900848929105,
            "unit": "Nanoseconds",
            "range": 0.000001168576411054653
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010573260849194949,
            "unit": "Nanoseconds",
            "range": 0.000001015114521119412
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000009658872398611538,
            "unit": "Nanoseconds",
            "range": 7.20539981846655e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001774950684762396,
            "unit": "Nanoseconds",
            "range": 4.0292742187567625e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000018139762846123008,
            "unit": "Nanoseconds",
            "range": 1.385261840075858e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008721923995839121,
            "unit": "Nanoseconds",
            "range": 6.489646003959334e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "81b2f07cc4c875fa24a4a979ea99e49b974b161a",
          "message": "Merge pull request #4443 from IntersectMBO/lehins/ref-script-pricing-tiers\n\nMake reference scripts fee grow exponentially with size",
          "timestamp": "2024-06-26T17:05:52-06:00",
          "tree_id": "20df9b5cce9a985c1854e89005aebb61e44891de",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/81b2f07cc4c875fa24a4a979ea99e49b974b161a"
        },
        "date": 1719443326607,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00005438364610091993,
            "unit": "Nanoseconds",
            "range": 7.874100268049005e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00005804666022662092,
            "unit": "Nanoseconds",
            "range": 7.183545555970416e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006792359636813111,
            "unit": "Nanoseconds",
            "range": 6.036677207160431e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010075233423191726,
            "unit": "Nanoseconds",
            "range": 4.985390268794542e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000009531453426093546,
            "unit": "Nanoseconds",
            "range": 1.138460990219998e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017351088275790673,
            "unit": "Nanoseconds",
            "range": 1.7827983760271725e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017971050483258437,
            "unit": "Nanoseconds",
            "range": 1.176194448750778e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008599053101499425,
            "unit": "Nanoseconds",
            "range": 8.709765267542698e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d391b670d79cf30f0d70409d14bf5eadbf06f498",
          "message": "Merge pull request #4446 from IntersectMBO/lehins/post-release-changes\n\nUpdate changelog to reflect recent release",
          "timestamp": "2024-06-26T20:26:33-06:00",
          "tree_id": "5ffb576949b4698ac0711674a5c231bc8515e337",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/d391b670d79cf30f0d70409d14bf5eadbf06f498"
        },
        "date": 1719455358923,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00005548459358611319,
            "unit": "Nanoseconds",
            "range": 0.0000010650381307042435
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.000060087382094489745,
            "unit": "Nanoseconds",
            "range": 8.27284535570602e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006909881633798359,
            "unit": "Nanoseconds",
            "range": 0.0000022249085820347517
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010301398586403606,
            "unit": "Nanoseconds",
            "range": 0.0000011706540434374192
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000009630897897304097,
            "unit": "Nanoseconds",
            "range": 4.449814092205378e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017623707787861253,
            "unit": "Nanoseconds",
            "range": 5.1675938116497e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.0000181224603327765,
            "unit": "Nanoseconds",
            "range": 6.853058621603238e-8
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008622205784577383,
            "unit": "Nanoseconds",
            "range": 8.646501200955234e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0ed1bfce5ddd18df9edabd00b4aa23379ab5b97d",
          "message": "Merge pull request #4436 from IntersectMBO/lehins/authorize-known-cc-members-only\n\nAuthorize known cc members only",
          "timestamp": "2024-06-26T22:32:36-06:00",
          "tree_id": "b066d4c6793167a1892e1f93601271e9c4d06dcd",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/0ed1bfce5ddd18df9edabd00b4aa23379ab5b97d"
        },
        "date": 1719462922295,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00005437287958208387,
            "unit": "Nanoseconds",
            "range": 2.827154731464244e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00005837908258644551,
            "unit": "Nanoseconds",
            "range": 7.241428111851664e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006809210525099847,
            "unit": "Nanoseconds",
            "range": 0.000004361902125189339
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010282397526124753,
            "unit": "Nanoseconds",
            "range": 0.000005495427744560188
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000009607343165950864,
            "unit": "Nanoseconds",
            "range": 5.166970532402983e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017895487863627854,
            "unit": "Nanoseconds",
            "range": 0.000001799022273804714
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001823198209657952,
            "unit": "Nanoseconds",
            "range": 0.0000010833947116307925
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008609323495318717,
            "unit": "Nanoseconds",
            "range": 6.727729995797088e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "16f57e4d4b53d6ff5ef4eca6dad7e03ae75ea9e5",
          "message": "Merge pull request #4433 from IntersectMBO/lehins/improve-resilience-of-futurePParams\n\nImprove resilience of future PParams",
          "timestamp": "2024-06-27T00:31:48-06:00",
          "tree_id": "59d02a3ba060e9720517eacb0e92aaab558f5a52",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/16f57e4d4b53d6ff5ef4eca6dad7e03ae75ea9e5"
        },
        "date": 1719470075769,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00005660033640367623,
            "unit": "Nanoseconds",
            "range": 0.0000042059417358661015
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.000060038449706585254,
            "unit": "Nanoseconds",
            "range": 0.0000029064961425367985
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006883221423443592,
            "unit": "Nanoseconds",
            "range": 9.491384363678667e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010304109218831307,
            "unit": "Nanoseconds",
            "range": 0.000002234169481653384
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000009637129614569846,
            "unit": "Nanoseconds",
            "range": 4.21865019233851e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017332385013822947,
            "unit": "Nanoseconds",
            "range": 3.001762142535353e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017949738188802813,
            "unit": "Nanoseconds",
            "range": 2.1134075142743777e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008670941653969775,
            "unit": "Nanoseconds",
            "range": 2.2665092613876282e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "aniketd@users.noreply.github.com",
            "name": "Aniket Deshpande",
            "username": "aniketd"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "5f43df407e4fef30ad1f99939f3ac1ac7f2057b5",
          "message": "Merge pull request #4442 from IntersectMBO/aniketd/conformance-update-formal-spec\n\nUpdate executable-spec SRP",
          "timestamp": "2024-06-28T15:13:59+02:00",
          "tree_id": "de15cffbb1996f31e48dc1af677956df80fccfff",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/5f43df407e4fef30ad1f99939f3ac1ac7f2057b5"
        },
        "date": 1719580607282,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00005548483446288726,
            "unit": "Nanoseconds",
            "range": 0.000006549363944763331
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00005761387950745565,
            "unit": "Nanoseconds",
            "range": 7.533260307504073e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006885075967745097,
            "unit": "Nanoseconds",
            "range": 0.0000023660622711470116
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010163830290694123,
            "unit": "Nanoseconds",
            "range": 5.825107288680166e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.00000946755115202889,
            "unit": "Nanoseconds",
            "range": 1.3568938330358477e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001750010517377442,
            "unit": "Nanoseconds",
            "range": 2.0042046526815987e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017779339784718374,
            "unit": "Nanoseconds",
            "range": 9.664661151497095e-8
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000850380102287293,
            "unit": "Nanoseconds",
            "range": 9.100039836323673e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "a53e43f3bca4753af683e6a1140eff83cdacd124",
          "message": "Merge pull request #4405 from IntersectMBO/aniketd/alonzo-utxos-tests\n\nAdd more scripts to alonzo utxosspec",
          "timestamp": "2024-06-28T19:47:38-06:00",
          "tree_id": "209c53662940ef3919cedfd218eb00fafaf10801",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/a53e43f3bca4753af683e6a1140eff83cdacd124"
        },
        "date": 1719625832179,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00005554879758860914,
            "unit": "Nanoseconds",
            "range": 0.0000020067748532587742
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00005766203789172027,
            "unit": "Nanoseconds",
            "range": 6.805067974434546e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006796052210225064,
            "unit": "Nanoseconds",
            "range": 0.000002064357367818281
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010225684965379185,
            "unit": "Nanoseconds",
            "range": 0.000003815128166309194
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000009460355569383691,
            "unit": "Nanoseconds",
            "range": 2.1720172773744693e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017277102235888257,
            "unit": "Nanoseconds",
            "range": 7.381675166665339e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001768793496456525,
            "unit": "Nanoseconds",
            "range": 2.726047762295686e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008439605541606695,
            "unit": "Nanoseconds",
            "range": 1.3786845333518195e-7
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "51f8d6005997ba914d6731a26d563dd398d87f18",
          "message": "Merge pull request #4453 from IntersectMBO/lehins/tx-refscritp-size-check\n\nTx refscript size check",
          "timestamp": "2024-06-29T13:46:55-06:00",
          "tree_id": "3e5a9ff20ac8f70b1ccbe4df020ec54abbf485dc",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/51f8d6005997ba914d6731a26d563dd398d87f18"
        },
        "date": 1719690585943,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.000054833742197816615,
            "unit": "Nanoseconds",
            "range": 5.265497144741663e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00005855302534003189,
            "unit": "Nanoseconds",
            "range": 7.369595855473431e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006770299895868583,
            "unit": "Nanoseconds",
            "range": 0.0000018673397892761225
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010099125288686222,
            "unit": "Nanoseconds",
            "range": 6.911797622607028e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000009479875501824199,
            "unit": "Nanoseconds",
            "range": 3.669840326442295e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017311527785377875,
            "unit": "Nanoseconds",
            "range": 1.2082207299571607e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017771374212300002,
            "unit": "Nanoseconds",
            "range": 9.54487238112004e-8
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008472631355850932,
            "unit": "Nanoseconds",
            "range": 7.208828843844801e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Lucsanszky@users.noreply.github.com",
            "name": "Lucsanszky",
            "username": "Lucsanszky"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "610593bc99cf5b467f1ec5e65f82cd6f4387f3b9",
          "message": "Merge pull request #4445 from IntersectMBO/ldan/conftest-plumb-newepoch\n\nConformance tests plumbing: `NEWEPOCH`\r\n\r\nResolves #4445",
          "timestamp": "2024-07-01T02:49:30+02:00",
          "tree_id": "c10e5041107148d66cdb4d6ed8b045a19cc8ea15",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/610593bc99cf5b467f1ec5e65f82cd6f4387f3b9"
        },
        "date": 1719795135804,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00005900988576748135,
            "unit": "Nanoseconds",
            "range": 0.000010006998042750712
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.000056340395645129864,
            "unit": "Nanoseconds",
            "range": 5.967272512931451e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006590430491402796,
            "unit": "Nanoseconds",
            "range": 5.307050398248683e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010098983833146351,
            "unit": "Nanoseconds",
            "range": 0.000003592334730480696
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000009777264358438389,
            "unit": "Nanoseconds",
            "range": 7.612611247082037e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017649233340492907,
            "unit": "Nanoseconds",
            "range": 8.74321148922837e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00001783464658929149,
            "unit": "Nanoseconds",
            "range": 0.0000010057643875248727
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008670523720844975,
            "unit": "Nanoseconds",
            "range": 9.18251396968996e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "fa01c457efb9ffd240cf2a84dadbbec07c103422",
          "message": "Merge pull request #4452 from IntersectMBO/lehins/prevent-votes-for-non-existent-entities\n\nPrevent votes for non existent entities",
          "timestamp": "2024-07-01T15:33:01-06:00",
          "tree_id": "f20441a73fdbbdb5af581c2448c4f67fa758fe99",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/fa01c457efb9ffd240cf2a84dadbbec07c103422"
        },
        "date": 1719869746389,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00005306735023438502,
            "unit": "Nanoseconds",
            "range": 4.851870213784614e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.000057548896657477114,
            "unit": "Nanoseconds",
            "range": 0.000004870559916078871
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006620776196978115,
            "unit": "Nanoseconds",
            "range": 6.493988079281335e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00009949276328649926,
            "unit": "Nanoseconds",
            "range": 0.00000353721382973041
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000009660941949824339,
            "unit": "Nanoseconds",
            "range": 3.251303971344407e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017630061974549772,
            "unit": "Nanoseconds",
            "range": 9.74073503402152e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017803832278771853,
            "unit": "Nanoseconds",
            "range": 2.0805463487435687e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008402758157477491,
            "unit": "Nanoseconds",
            "range": 8.77111125035056e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "teodora.danciu@tweag.io",
            "name": "teodanciu",
            "username": "teodanciu"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "698a24c1b5064468ef614532a6524dce2b41d7f5",
          "message": "Merge pull request #4450 from IntersectMBO/td/bbody-refscript-size-check\n\nBBODY refscript size check",
          "timestamp": "2024-07-02T02:11:18+01:00",
          "tree_id": "894ba57bb6e266d6803e19168dfc0961f234cfbc",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/698a24c1b5064468ef614532a6524dce2b41d7f5"
        },
        "date": 1719882848375,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.000054855338518463616,
            "unit": "Nanoseconds",
            "range": 0.0000028858945400247836
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00005733869563838912,
            "unit": "Nanoseconds",
            "range": 0.000001994133430145012
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006683480750747928,
            "unit": "Nanoseconds",
            "range": 7.486023487493241e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010299806157877923,
            "unit": "Nanoseconds",
            "range": 0.000004105231391615411
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.0000095510314109784,
            "unit": "Nanoseconds",
            "range": 1.449742380440332e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000017732090005554092,
            "unit": "Nanoseconds",
            "range": 9.191884828110386e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017929408039827344,
            "unit": "Nanoseconds",
            "range": 2.671340372903153e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.00000858688615033351,
            "unit": "Nanoseconds",
            "range": 8.050835285038204e-8
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "aniketd@users.noreply.github.com",
            "name": "Aniket Deshpande",
            "username": "aniketd"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "936a79333c5d4da6129408eb4ddce51116cf5f94",
          "message": "Merge pull request #4456 from IntersectMBO/lehins/fixup-for-the-release\n\nBump up version for `cardano-data`",
          "timestamp": "2024-07-02T15:09:56+05:30",
          "tree_id": "995a9057b17c2ab1403baa87e1cd54f2c6db4d63",
          "url": "https://github.com/IntersectMBO/cardano-ledger/commit/936a79333c5d4da6129408eb4ddce51116cf5f94"
        },
        "date": 1719913358574,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.00005307536382933648,
            "unit": "Nanoseconds",
            "range": 4.7014549346111226e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00005682510038073984,
            "unit": "Nanoseconds",
            "range": 2.9818478795856446e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00006642277955617327,
            "unit": "Nanoseconds",
            "range": 7.182216634268702e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00010163320372064312,
            "unit": "Nanoseconds",
            "range": 5.554119874645229e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000009561796192109777,
            "unit": "Nanoseconds",
            "range": 1.5139878447088473e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.00001769638126875949,
            "unit": "Nanoseconds",
            "range": 9.92755521739027e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.000017769688442176285,
            "unit": "Nanoseconds",
            "range": 1.5802821356762968e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000008504563750790511,
            "unit": "Nanoseconds",
            "range": 8.313882910367919e-8
          }
        ]
      }
    ]
  }
}