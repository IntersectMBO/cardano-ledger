section \<open> Proofs \<close>

theory Ledger_Rules
  imports Main
begin

text \<open> Non-standard map operators \<close>

definition dom_exc :: "'a set \<Rightarrow> ('a \<rightharpoonup> 'b) \<Rightarrow> ('a \<rightharpoonup> 'b)" ("_ \<lhd>'/ _" [61, 61] 60) where
  "s \<lhd>/ m = m |` (- s)"

lemma dom_exc_unit:
  assumes "dom m \<inter> s = {}"
  shows "s \<lhd>/ m = m"
proof -
  have "s \<lhd>/ m \<subseteq>\<^sub>m m"
    by (simp add: dom_exc_def map_le_def)
  moreover from assms have "m \<subseteq>\<^sub>m s \<lhd>/ m"
    by (metis disjoint_eq_subset_Compl dom_exc_def map_le_def restrict_in rev_subsetD)
  ultimately show ?thesis
    by (simp add: map_le_antisym)
qed

lemma dom_exc_empty:
  shows "{} \<lhd>/ m = m"
  by (simp add: dom_exc_unit) 

lemma dom_exc_union_sec:
  shows "(s\<^sub>1 \<union> s\<^sub>2) \<lhd>/ m = s\<^sub>1 \<lhd>/ (s\<^sub>2 \<lhd>/ m)"
  by (simp add: dom_exc_def inf_commute)

lemma dom_exc_append_distr:
  shows "s \<lhd>/ (m\<^sub>1 ++ m\<^sub>2) = (s \<lhd>/ m\<^sub>1) ++ (s \<lhd>/ m\<^sub>2)"
proof -
  have ltr: "s \<lhd>/ (m\<^sub>1 ++ m\<^sub>2) \<subseteq>\<^sub>m (s \<lhd>/ m\<^sub>1) ++ (s \<lhd>/ m\<^sub>2)"
    by (simp add: dom_exc_def map_add_def map_le_def restrict_map_def)
  then have "(s \<lhd>/ m\<^sub>1) ++ (s \<lhd>/ m\<^sub>2) \<subseteq>\<^sub>m s \<lhd>/ (m\<^sub>1 ++ m\<^sub>2)"
    by (simp add: dom_exc_def dom_map_add dom_restrict inf_sup_distrib2 map_le_def)
  then show ?thesis
    by (simp add: ltr map_le_antisym)
qed

lemma dom_exc_assoc:
  assumes "dom m\<^sub>1 \<inter> dom m\<^sub>2 = {}"
  and "s \<inter> dom m\<^sub>2 = {}"
  shows "(s \<lhd>/ m\<^sub>1) ++ m\<^sub>2 = s \<lhd>/ (m\<^sub>1 ++ m\<^sub>2)"
proof -
  from assms(1) have "dom (s \<lhd>/ m\<^sub>1) \<inter> dom m\<^sub>2 = {}"
    by (simp add: dom_exc_def inf_commute inf_left_commute)
  with assms(2) have rtl: "(s \<lhd>/ m\<^sub>1) ++ m\<^sub>2 \<subseteq>\<^sub>m s \<lhd>/ (m\<^sub>1 ++ m\<^sub>2)"
    by (smt disjoint_eq_subset_Compl disjoint_iff_not_equal dom_exc_def map_add_dom_app_simps(1) map_add_dom_app_simps(3) map_le_def restrict_map_def subsetCE)
  moreover have "s \<lhd>/ (m\<^sub>1 ++ m\<^sub>2) \<subseteq>\<^sub>m (s \<lhd>/ m\<^sub>1) ++ m\<^sub>2"
    by (smt rtl domIff dom_exc_def map_add_None map_le_def restrict_map_def)
  ultimately show ?thesis
    using map_le_antisym by blast
qed

text \<open> Abstract types \<close>

typedecl tx_id
typedecl ix
typedecl addr
typedecl tx

text \<open> Derived types \<close>

type_synonym coin = int
type_synonym tx_in = "tx_id \<times> ix"
type_synonym tx_out = "addr \<times> coin"
type_synonym utxo = "tx_in \<rightharpoonup> tx_out"

text \<open> Transaction Types \<close>

type_synonym tx_body = "tx_in set \<times> (ix \<rightharpoonup> tx_out)"

text \<open> Abstract functions \<close>

fun txid :: "tx \<Rightarrow> tx_id" where
  "txid _ = undefined"

fun txbody :: "tx \<Rightarrow> tx_body" where
  "txbody _ = undefined"

text \<open> Accessor functions \<close>

fun txins :: "tx \<Rightarrow> tx_in set" where
  "txins tx = (let (inputs, _) = txbody tx in inputs)"

fun txouts :: "tx \<Rightarrow> utxo" where
  "txouts tx = (
    let (_, outputs) = txbody tx in (
    \<lambda>(id, ix). if id \<noteq> txid tx then None else case outputs ix of None \<Rightarrow> None | Some txout \<Rightarrow> Some txout))"

lemma dom_txouts_is_txid:
  shows "\<And>i ix. (i, ix) \<in> dom (txouts tx) \<Longrightarrow> i = txid tx"
  by (smt case_prod_conv domIff surj_pair txouts.simps)

text \<open> UTxO transition-system types \<close>

\<comment> \<open> UTxO environment \<close>
typedecl utxo_env \<comment> \<open> Abstract, don't care for now \<close>

\<comment> \<open> UTxO states \<close>
type_synonym utxo_state = utxo \<comment> \<open> Simplified \<close>

text \<open> UTxO inference rules \<close>

inductive utxo_sts :: "utxo_env \<Rightarrow> utxo_state \<Rightarrow> tx \<Rightarrow> utxo_state \<Rightarrow> bool"
  ("_ \<turnstile> _ \<rightarrow>\<^bsub>UTXO\<^esub>{_} _" [51, 0, 51] 50)
  for \<Gamma>
  where
    utxo_inductive: "
      \<lbrakk>
        txins tx \<subseteq> dom utxo_st;
        txins tx \<noteq> {};
        txouts tx \<noteq> Map.empty;
        \<forall>(_, c) \<in> ran (txouts tx). c > 0
      \<rbrakk>
      \<Longrightarrow>
      \<Gamma> \<turnstile> utxo_st \<rightarrow>\<^bsub>UTXO\<^esub>{tx} (txins tx \<lhd>/ utxo_st) ++ txouts tx"

text \<open> Transaction sequences \<close>

inductive utxows :: "utxo_env \<Rightarrow> utxo_state \<Rightarrow> tx list \<Rightarrow> utxo_state \<Rightarrow> bool"
  ("_ \<turnstile> _ \<rightarrow>\<^bsub>UTXOWS\<^esub>{_} _" [51, 0, 51] 50)
  for \<Gamma>
  where
    empty: "\<Gamma> \<turnstile> s \<rightarrow>\<^bsub>UTXOWS\<^esub>{[]} s" |
    step: "\<Gamma> \<turnstile> s \<rightarrow>\<^bsub>UTXOWS\<^esub>{txs @ [tx]} s''" if "\<Gamma> \<turnstile> s \<rightarrow>\<^bsub>UTXOWS\<^esub>{txs} s'" and "\<Gamma> \<turnstile> s' \<rightarrow>\<^bsub>UTXO\<^esub>{tx} s''"

text \<open> Auxiliary lemmas \<close>

abbreviation txid_injectivity :: bool where
  "txid_injectivity \<equiv> \<forall>tx tx'. txid tx = txid tx' \<longrightarrow> tx = tx'"

lemma lemma_1:
  assumes "\<Gamma> \<turnstile> utxo\<^sub>0 \<rightarrow>\<^bsub>UTXOWS\<^esub>{T} utxo"
  and "\<forall>T\<^sub>i \<in> set T. txins T\<^sub>i \<inter> txins tx = {}"
  and "dom (txouts tx) \<inter> dom utxo\<^sub>0 = {}"
  and txid_injectivity
  shows "(\<Union>T\<^sub>i \<in> set T. txins T\<^sub>i) \<inter> dom (txouts tx) = {}"
  and "dom (txouts tx) \<inter> dom utxo = {}"
  using assms
proof (induction rule: utxows.induct)
  case (empty s)
  { case 1
    then show ?case
      by force
  next
    case 2
    then show ?case
      by blast
  }
next
  case (step utxo\<^sub>0 T utxo tx' utxo')
  { case 1
    then have "txins tx' \<inter> dom (txouts tx) = {}"
    proof -
      from "1.prems"(1) and assms(4) have tx_excl: "\<forall>T\<^sub>i \<in> set T. txins T\<^sub>i \<inter> txins tx = {}"
        by auto
      with step.IH(1) and "1.prems"(2) and assms(4)
      have "(\<Union>T\<^sub>i \<in> set T. txins T\<^sub>i) \<inter> dom (txouts tx) = {}"
        by simp
      moreover from step.hyps(2) have "txins tx' \<subseteq> dom utxo"
        using utxo_sts.simps by blast
      ultimately show ?thesis
        by (smt "1.prems"(2) assms(4) tx_excl inf.orderE inf_bot_right inf_sup_aci(1) inf_sup_aci(2) step.IH(2))
    qed
    moreover from "1.prems" and step.IH(1) have "(\<Union>T\<^sub>i \<in> set T. txins T\<^sub>i) \<inter> dom (txouts tx) = {}"
      by simp
    ultimately show ?case
      by (smt Int_empty_right SUP_empty UN_Un UN_insert empty_set inf_commute inf_sup_distrib1 list.simps(15) set_append)
  next
    case 2
    from "2.prems"(1) and assms(4) and step.hyps(2) and step.IH(2)
    have "dom (txouts tx) \<inter> dom (txins tx' \<lhd>/ utxo) = {}"
      using utxo_sts.simps by auto
    moreover have "dom (txouts tx) \<inter> dom (txouts tx') = {}"
    proof -
      from "2.prems"(1) have "txins tx' \<inter> txins tx = {}"
        by (meson in_set_conv_decomp)
      with step.hyps(2) have "txins tx' \<noteq> txins tx"
        using inf.idem and utxo_sts.cases by auto
      then have "tx' \<noteq> tx"
        by blast
      with assms(4) have "txid tx' \<noteq> txid tx"
        by blast
      then show ?thesis
        using dom_txouts_is_txid by (simp add: ComplI disjoint_eq_subset_Compl subrelI)
    qed
    ultimately have "dom (txouts tx) \<inter> dom ((txins tx' \<lhd>/ utxo) ++ txouts tx') = {}"
      by blast
    with step.hyps(2) show ?case
      using utxo_sts.simps by simp
    }
qed

lemma aux_lemma:
  assumes "\<Gamma> \<turnstile> utxo\<^sub>0 \<rightarrow>\<^bsub>UTXOWS\<^esub>{T} utxo"
  and "\<Gamma> \<turnstile> utxo \<rightarrow>\<^bsub>UTXO\<^esub>{tx} utxo'"
  and "\<forall>T\<^sub>i \<in> set (T @ [tx]). dom (txouts T\<^sub>i) \<inter> dom utxo\<^sub>0 = {}"
  and "\<forall>T\<^sub>i \<in> set T. dom (txouts T\<^sub>i) \<inter> dom utxo\<^sub>0 = {} \<Longrightarrow> \<forall>T\<^sub>i \<in> set T. txins T\<^sub>i \<inter> dom utxo = {}"
  and txid_injectivity
  shows "txins tx \<inter> dom (txouts tx) = {}"
proof -
  from assms(2) have "txins tx \<subseteq> dom utxo"
    using utxo_sts.simps by blast
  with assms(3,4) have "\<forall>T\<^sub>i \<in> set T. txins T\<^sub>i \<inter> txins tx = {}"
    by (smt butlast_snoc in_set_butlastD inf.orderE inf_bot_right inf_left_commute)
  with assms(1-3,5)
  have "(\<Union>T\<^sub>i \<in> set T. txins T\<^sub>i) \<inter> dom (txouts tx) = {}" and "dom (txouts tx) \<inter> dom utxo = {}"
    using lemma_1 by auto
  with assms(2) show ?thesis
    by (metis (no_types, lifting) disjoint_iff_not_equal subsetCE utxo_sts.simps)
qed

lemma lemma_3:
  assumes "\<Gamma> \<turnstile> utxo\<^sub>0 \<rightarrow>\<^bsub>UTXOWS\<^esub>{T} utxo"
  and "\<forall>T\<^sub>i \<in> set T. dom (txouts T\<^sub>i) \<inter> dom utxo\<^sub>0 = {}"
  and txid_injectivity
  shows "\<forall>T\<^sub>i \<in> set T. txins T\<^sub>i \<inter> dom utxo = {}"
using assms
proof (induction rule: utxows.induct)
  case (empty s)
  then show ?case
    by simp
next
  case (step utxo\<^sub>0 T utxo tx utxo')
  then have "\<And>T\<^sub>i. T\<^sub>i \<in> set T \<Longrightarrow> txins T\<^sub>i \<inter> dom utxo' = {}"
  proof -
    fix T\<^sub>i
    assume "T\<^sub>i \<in> set T"
    then have "txins T\<^sub>i \<inter> dom (txins tx \<lhd>/ utxo) = {}" 
    proof -
      from step.IH and \<open>T\<^sub>i \<in> set T\<close> and step.prems have "txins T\<^sub>i \<inter> dom utxo = {}"
        by (metis butlast_snoc in_set_butlastD)
      then show ?thesis
        by (simp add: disjoint_iff_not_equal dom_exc_def)
    qed
    moreover have "txins T\<^sub>i \<inter> dom (txouts tx) = {}"
    proof -
      from step.hyps(2) have "txins tx \<subseteq> dom utxo"
        using utxo_sts.simps by blast
      with step.IH and \<open>T\<^sub>i \<in> set T\<close> and step.prems have "\<forall>T\<^sub>i \<in> set T. txins T\<^sub>i \<inter> txins tx = {}"
        by (smt butlast_snoc in_set_butlastD inf.orderE inf_bot_right inf_left_commute)
      with step.hyps(1) and step.prems have "(\<Union>T\<^sub>i \<in> set T. txins T\<^sub>i) \<inter> dom (txouts tx) = {}"
        using lemma_1 in_set_conv_decomp by auto
      with \<open>T\<^sub>i \<in> set T\<close> show ?thesis
        by blast
    qed
    ultimately have "txins T\<^sub>i \<inter> dom ((txins tx \<lhd>/ utxo) ++ txouts tx) = {}"
      by blast
    with step.hyps(2) show "txins T\<^sub>i \<inter> dom utxo' = {}"
      using utxo_sts.simps by auto
  qed
  moreover have "txins tx \<inter> dom utxo' = {}"
  proof -
    have "txins tx \<inter> dom (txins tx \<lhd>/ utxo) = {}"
      by (simp add: dom_exc_def)
    moreover with step.IH and step.hyps(1-2) and step.prems have "txins tx \<inter> dom (txouts tx) = {}"
      using aux_lemma by blast
    ultimately have "txins tx \<inter> dom ((txins tx \<lhd>/ utxo) ++ txouts tx) = {}"
      by blast
    with step.hyps(2) show ?thesis
      using utxo_sts.simps by auto
  qed
  ultimately show ?case
    by simp
qed

subsection \<open> No Double-Spending Property \<close>

theorem no_double_spending:
  assumes "\<Gamma> \<turnstile> utxo\<^sub>0 \<rightarrow>\<^bsub>UTXOWS\<^esub>{T} utxo"
  and "\<forall>T\<^sub>i \<in> set T. dom (txouts T\<^sub>i) \<inter> dom utxo\<^sub>0 = {}"
  and txid_injectivity
  shows "\<forall>i \<ge> 0. \<forall>j < length T. i < j \<longrightarrow> txins (T ! i) \<inter> txins (T ! j) = {}"
  using assms
proof (induction arbitrary: utxo rule: utxows.induct)
  case (empty s)
  then show ?case
    by simp
next
  case (step utxo\<^sub>0 T utxo tx utxo')
  then show ?case
  proof (intro allI impI)
    fix i j
    assume "i \<ge> 0" and "j < length (T @ [tx])" and "i < j"
    then consider
      (a) "j < length T" |
      (b) "j = length T"
      by fastforce
    then show "txins ((T @ [tx]) ! i) \<inter> txins ((T @ [tx]) ! j) = {}"
    proof (cases)
      case a
      with \<open>i \<ge> 0\<close> and \<open>i < j\<close> and step.prems and step.IH show ?thesis
        by (smt butlast_snoc in_set_conv_nth length_append_singleton less_Suc_eq less_trans nth_butlast)
    next
      case b
      with \<open>i < j\<close> have "(T @ [tx]) ! i = T ! i"
        by (simp add: nth_append)
      moreover with \<open>j = length T\<close> have "(T @ [tx]) ! j = tx"
        by simp
      ultimately have "txins (T ! i) \<inter> txins tx = {}"
      proof -
        have "txins (T ! i) \<inter> dom utxo = {}" 
          using lemma_3 \<open>\<Gamma> \<turnstile> utxo\<^sub>0 \<rightarrow>\<^bsub>UTXOWS\<^esub>{T} utxo\<close> \<open>i < j\<close> b step.prems
          by (metis UnCI nth_mem set_append)
        moreover from \<open>\<Gamma> \<turnstile> utxo \<rightarrow>\<^bsub>UTXO\<^esub>{tx} utxo'\<close> and utxo_sts.simps have "txins tx \<subseteq> dom utxo"
           by simp
        ultimately show ?thesis by blast
      qed
      with \<open>(T @ [tx]) ! j = tx\<close> and \<open>(T @ [tx]) ! i = T ! i\<close> show ?thesis
        by simp
    qed
  qed
qed

subsection \<open> UTxO Difference Property \<close>

primrec general_append :: "['a \<Rightarrow> ('b \<rightharpoonup> 'c), 'a list] \<Rightarrow> ('b \<rightharpoonup> 'c)" where
  "general_append _ [] = Map.empty" |
  "general_append f (x # xs) = f x ++ general_append f xs"

syntax
  "_general_append" :: "pttrn => 'a list => 'b \<rightharpoonup> 'c => 'b \<rightharpoonup> 'c" ("(3\<Oplus>_\<leftarrow>_. _)" [0, 0, 100] 100)
translations
  "\<Oplus>x\<leftarrow>xs. p" \<rightleftharpoons> "CONST general_append (\<lambda>x. p) xs"

lemma general_append_rev:
  shows "(\<Oplus>x\<leftarrow>(xs @ [y]). P x) = (\<Oplus>x\<leftarrow>xs. P x) ++ P y"
  by (induction xs) simp_all

theorem utxo_is_outputs_minus_inputs:
  assumes "\<Gamma> \<turnstile> utxo\<^sub>0 \<rightarrow>\<^bsub>UTXOWS\<^esub>{T} utxo"
  and "\<forall>T\<^sub>i \<in> set T. dom (txouts T\<^sub>i) \<inter> dom utxo\<^sub>0 = {}"
  and txid_injectivity
  shows "(\<Union>T\<^sub>i \<in> set T. txins T\<^sub>i) \<lhd>/ (utxo\<^sub>0 ++ (\<Oplus>T\<^sub>i\<leftarrow>T. txouts T\<^sub>i)) = utxo"
  using assms
proof (induction rule: utxows.induct)
  case (empty s)
  then show ?case
    by (simp add: dom_exc_empty)
next
  case (step utxo\<^sub>0 T utxo tx utxo')
  let ?T' = "T @ [tx]"
  from step.hyps(1,2) have "\<Gamma> \<turnstile> utxo\<^sub>0 \<rightarrow>\<^bsub>UTXOWS\<^esub>{?T'} utxo'"
    by (simp add: utxows.step)
  with step.prems(1,2)
  have "\<forall>i \<ge> 0. \<forall>j < length ?T'. i < j \<longrightarrow> txins (?T' ! i) \<inter> txins (?T' ! j) = {}"
    using no_double_spending by simp
  with step.hyps(2) have *: "\<forall>T\<^sub>i \<in> set T. txins T\<^sub>i \<inter> txins tx = {}"
    using subsetCE and utxo_sts.simps by auto
  have PO\<^sub>0: "(\<Union>T\<^sub>i \<in> set T. txins T\<^sub>i) \<lhd>/ txouts tx = txouts tx"
  proof -
    from * and step.hyps(1) and step.prems(1,2)
    have "(\<Union>T\<^sub>i \<in> set T. txins T\<^sub>i) \<inter> dom (txouts tx) = {}"
      using lemma_1(1) by auto
    with * show ?thesis
      using dom_exc_empty by auto
  qed
  have PO\<^sub>1: "txins tx \<lhd>/ txouts tx = txouts tx"
  proof -
    from * and step.hyps(1) and step.prems(1,2)
    have "dom (txouts tx) \<inter> dom utxo = {}"
      using lemma_1(2) by simp
    moreover from step.hyps(2) have "txins tx \<subseteq> dom utxo"
      using utxo_sts.simps by blast
    ultimately have "dom (txouts tx) \<inter> txins tx = {}"
      by blast
    then show ?thesis
      by (simp add: dom_exc_unit) 
  qed
  have "
    (\<Union>T\<^sub>i \<in> set (T @ [tx]). txins T\<^sub>i) \<lhd>/ (utxo\<^sub>0 ++ (\<Oplus>T\<^sub>i\<leftarrow>(T @ [tx]). txouts T\<^sub>i))
    =
    (txins tx \<union> (\<Union>T\<^sub>i \<in> set T. txins T\<^sub>i)) \<lhd>/ (utxo\<^sub>0 ++ (\<Oplus>T\<^sub>i\<leftarrow>(T @ [tx]). txouts T\<^sub>i))"
    by simp
  also have "
    \<dots>
    =
    txins tx \<lhd>/ ((\<Union>T\<^sub>i \<in> set T. txins T\<^sub>i) \<lhd>/ ((utxo\<^sub>0 ++ (\<Oplus>T\<^sub>i\<leftarrow>T. txouts T\<^sub>i)) ++ txouts tx))"
    by (simp add: dom_exc_union_sec general_append_rev)
  also have "
    \<dots>
    =
    txins tx \<lhd>/
    (
      ((\<Union>T\<^sub>i \<in> set T. txins T\<^sub>i) \<lhd>/ (utxo\<^sub>0 ++ (\<Oplus>T\<^sub>i\<leftarrow>T. txouts T\<^sub>i)))
      ++
      ((\<Union>T\<^sub>i \<in> set T. txins T\<^sub>i) \<lhd>/ txouts tx)
    )"
    by (simp add: dom_exc_append_distr)
  also from step.IH and step.prems(1,2) have "
    \<dots>
    =
    txins tx \<lhd>/
    (
      utxo
      ++
      ((\<Union>T\<^sub>i \<in> set T. txins T\<^sub>i) \<lhd>/ txouts tx)
    )"
    by simp
  also from PO\<^sub>0 have "\<dots> = txins tx \<lhd>/ (utxo ++ txouts tx)"
    by simp
  also have "\<dots> = (txins tx \<lhd>/ utxo) ++ (txins tx \<lhd>/ txouts tx)"
    by (simp add: dom_exc_append_distr)
  also from PO\<^sub>1 have "\<dots> = (txins tx \<lhd>/ utxo) ++ txouts tx"
    by simp
  finally show ?case
    using step.hyps(2) and utxo_sts.simps by simp
qed

end
