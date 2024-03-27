window.BENCHMARK_DATA = {
  "lastUpdate": 1711541160467,
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
      }
    ]
  }
}