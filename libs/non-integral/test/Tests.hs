{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Tests.Cardano.Ledger.NonIntegral

main :: IO ()
main = do
  property_negative_taylorExpCmp_comparison
  property_exponential_is_monotonic_db
  property_logarithm_is_monotonic_db
  property_exp_maps_unit_interval_to_unit_interval_db
  property_exp_of_ln_db
  property_ln_of_exp_db
  property_power_diff_db
  property_exponential_law_db
  property_log_law_db
  property_log_power_db
  property_bound_findE_db
  prop_exp_is_monotonic_fp
  prop_log_is_monotonic_fp
  property_exp_maps_unit_interval_to_unit_interval_fp
  property_exp_of_ln_fp
  property_ln_of_exp_fp
  property_power_diff_fp
  property_exponential_law_fp
  property_log_law_fp
  property_log_power_fp
  property_bound_findE_fp
  property_praos_leader_comparison
  prop_exp_is_monotonic_q
  prop_log_is_monotonic_q
  property_exp_maps_unit_interval_to_unit_interval_q
  property_exp_of_ln_q
  property_ln_of_exp_q
  property_power_diff_q
  property_exponential_law_q
  property_log_law_q
  property_log_power_q
  property_bound_findE_q
