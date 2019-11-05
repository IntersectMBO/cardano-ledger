(* NOTES:
    - Using `fmap` instead of `map` because of the following reasons:
      * Lemmas such as `sum_diff1` require finite sets.
      * Allows for proofs by structural induction on `FSet` and `fmap`.
      * UTxO's are conceptually finite maps.
*)
section \<open> Proofs \<close>

theory Ledger_Rules
  imports "HOL-Library.Finite_Map"
begin

text \<open> Some extra lemmas and syntactic sugar for \<open>fmap\<close> \<close>

abbreviation fmap_update ("_'(_ $$:= _')" [1000,0,0] 1000) where
  "fmap_update m k v \<equiv> fmupd k v m"

notation fmlookup (infixl "$$" 999)

notation fmempty ("{$$}")

abbreviation fmap_singleton ("{_ $$:= _}" [0, 0] 1000) where
  "fmap_singleton k v \<equiv> {$$}(k $$:= v)"

abbreviation fmap_lookup_the (infixl "$$!" 999) where
  "fmap_lookup_the m k \<equiv> the (m $$ k)"

lemma fmfilter_fmsubset: "fmfilter p m \<subseteq>\<^sub>f m"
proof -
  have "\<forall>k \<in> fmdom' m. \<exists>v. (fmfilter p m) $$ k = v \<longrightarrow> m $$ k = v"
    by blast
  then show ?thesis
    by (simp add: Ball_def_raw domIff fmsubset.rep_eq map_le_def)
qed

text \<open> Non-standard map operators \<close>

abbreviation dom_res :: "'a set \<Rightarrow> ('a, 'b) fmap \<Rightarrow> ('a, 'b) fmap" (infixl "\<lhd>" 150) where
  "s \<lhd> m \<equiv> fmfilter (\<lambda>x. x \<in> s) m"

abbreviation dom_exc :: "'a set \<Rightarrow> ('a, 'b) fmap \<Rightarrow> ('a, 'b) fmap" (infixl "\<lhd>'/" 150) where
  "s \<lhd>/ m \<equiv> fmfilter (\<lambda>x. x \<notin> s) m"

lemma dom_res_not_in_absorb:
  assumes "x \<in> A"
  and "x \<notin> fmdom' m"
  shows "(A - {x}) \<lhd> m = A \<lhd> m"
  using assms
proof (induction m rule: fmap_induct)
  case fmempty
  then show ?case
    by simp
next
  case (fmupd k v m)
  have "(A - {x}) \<lhd> m(k $$:= v) = (A - {x}) \<lhd> (m ++\<^sub>f {k $$:= v})"
    by simp
  also have "\<dots> = ((A - {x}) \<lhd> m) ++\<^sub>f ((A - {x}) \<lhd> {k $$:= v})"
    by simp
  also from fmupd.IH and fmupd.prems(1,2) have "\<dots> = (A \<lhd> m) ++\<^sub>f ((A - {x}) \<lhd> {k $$:= v})"
    by simp
  also have "\<dots> = (A \<lhd> m) ++\<^sub>f (A \<lhd> {k $$:= v})"
  proof (cases "k \<in> A")
    case True
    from fmupd.prems(2) have "x \<noteq> k"
      by fastforce
    with True show ?thesis
      by simp
  next
    case False
    then have "(A \<lhd> m) ++\<^sub>f ((A - {x}) \<lhd> {k $$:= v}) = A \<lhd> m"
      by simp
    with False show ?thesis
      by simp
  qed
  also have "\<dots> = A \<lhd> (m ++\<^sub>f {k $$:= v})"
    by simp
  finally show ?case
    by simp
qed

lemma dom_res_fmupd:
  assumes "k \<in> A"
  and "k \<notin> fmdom' m"
  shows "A \<lhd> m(k $$:= v) = ((A - {k}) \<lhd> m) ++\<^sub>f {k $$:= v}"
  using assms and dom_res_not_in_absorb
  by (metis fmadd_empty(2) fmadd_fmupd fmfilter_upd)

lemma dom_exc_unit:
  assumes "fmdom' m \<inter> A = {}"
  shows "A \<lhd>/ m = m"
proof -
  have "A \<lhd>/ m \<subseteq>\<^sub>f m"
    by (blast intro: fmfilter_fmsubset)
  moreover from assms have "m \<subseteq>\<^sub>f A \<lhd>/ m"
    by (metis (no_types, lifting) Diff_iff Diff_triv fmdom'I fmfilter_true fmsubset.rep_eq map_le_def)
  ultimately show ?thesis
    by (metis (no_types, lifting) fmfilter_true fmlookup_filter fmsubset.rep_eq map_le_antisym option.simps(3))
qed

lemma dom_exc_empty:
  shows "{} \<lhd>/ m = m"
  by (simp add: dom_exc_unit)

lemma dom_exc_union_sec:
  shows "(A \<union> B) \<lhd>/ m = A \<lhd>/ (B \<lhd>/ m)"
  by (simp add: inf_commute)

lemma dom_exc_add_distr:
  shows "A \<lhd>/ (m\<^sub>1 ++\<^sub>f m\<^sub>2) = (A \<lhd>/ m\<^sub>1) ++\<^sub>f (A \<lhd>/ m\<^sub>2)"
  by (blast intro: fmfilter_add_distrib)

lemma dom_exc_assoc:
  assumes "A \<inter> fmdom' m\<^sub>2 = {}"
  shows "(A \<lhd>/ m\<^sub>1) ++\<^sub>f m\<^sub>2 = A \<lhd>/ (m\<^sub>1 ++\<^sub>f m\<^sub>2)"
  using assms
  by (simp add: dom_exc_unit inf_commute)

lemma dom_exc_fmupd:
  assumes "k \<in> A"
  and "m $$ k = None"
  shows "(A - {k}) \<lhd>/ m = A \<lhd>/ m(k $$:= v)"
  using assms
  by (smt Diff_iff Diff_insert_absorb domIff fmfilter_cong fmfilter_upd option.simps(3))

abbreviation
  fmsub :: "('a, 'b) fmap \<Rightarrow> ('a, 'b) fmap \<Rightarrow> ('a, 'b) fmap" (infixl "--\<^sub>f" 100) where
  "m\<^sub>1 --\<^sub>f m\<^sub>2 \<equiv> fmfilter (\<lambda>x. x \<notin> fmdom' m\<^sub>2) m\<^sub>1"

lemma fmsub_fmadd_distr: "m\<^sub>1 --\<^sub>f (m\<^sub>2 ++\<^sub>f m\<^sub>3) = m\<^sub>1 --\<^sub>f m\<^sub>2 --\<^sub>f m\<^sub>3"
  using fmfilter_comm by force

lemma fmsub_fmupd: "m $$ k = None \<Longrightarrow> m(k $$:= v) --\<^sub>f {k $$:= v} = m"
  by (smt Diff_iff Diff_insert_absorb fmdom'_empty fmdom'_fmupd fmdom'_notD fmdom'_notI fmfilter_true fmfilter_upd option.simps(3) singletonI)

lemma fmsub_fmdrop:
  shows "m\<^sub>1 --\<^sub>f m\<^sub>2(k $$:= v) = fmdrop_fset {|k|} m\<^sub>1 --\<^sub>f m\<^sub>2"
proof -
  have "m\<^sub>1 --\<^sub>f m\<^sub>2(k $$:= v) = fmfilter (\<lambda>z. z |\<notin>| finsert k (fmdom m\<^sub>2)) m\<^sub>1"
    by (simp add: fmdom'_alt_def fmember.rep_eq)
  also have "\<dots> = fmfilter (\<lambda>z. z |\<notin>| {|k|}) (fmfilter (\<lambda>z. z |\<notin>| fmdom m\<^sub>2) m\<^sub>1)"
    by simp
  also have "\<dots> = fmfilter (\<lambda>z. z |\<notin>| fmdom m\<^sub>2) (fmfilter (\<lambda>z. z |\<notin>| {|k|}) m\<^sub>1)"
    by (blast intro: fmfilter_comm)
  also have "\<dots> = fmfilter (\<lambda>z. z \<notin> fmdom' m\<^sub>2) (fmfilter (\<lambda>z. z |\<notin>| {|k|}) m\<^sub>1)"
    by (simp add: fmdom'_alt_def fmember.rep_eq)
  also have "\<dots> = fmdrop_fset {|k|} m\<^sub>1 --\<^sub>f m\<^sub>2"
    by (metis fmfilter_alt_defs(3))
  finally show ?thesis .
qed

text \<open> Abstract types \<close>

typedecl tx_id
typedecl ix
typedecl addr
typedecl tx

axiomatization ix_to_nat :: "ix \<Rightarrow> nat" where
  ix_to_nat_injectivity: "inj ix_to_nat"

instantiation ix :: countable
begin
instance by (standard, intro exI) (fact ix_to_nat_injectivity)
end

instantiation ix :: linorder
begin

definition less_ix :: "ix \<Rightarrow> ix \<Rightarrow> bool" where
  "less_ix x y = (ix_to_nat x < ix_to_nat y)"

definition less_eq_ix :: "ix \<Rightarrow> ix \<Rightarrow> bool" where
  "less_eq_ix x y = (ix_to_nat x \<le> ix_to_nat y)"

instance
proof
  fix x y z :: ix
  show "(x < y) = (x \<le> y \<and> \<not> y \<le> x)"
    unfolding less_eq_ix_def and less_ix_def by auto
  show "x \<le> x"
    unfolding less_eq_ix_def by simp
  show "\<lbrakk>x \<le> y; y \<le> z\<rbrakk> \<Longrightarrow> x \<le> z"
    unfolding less_eq_ix_def and less_ix_def by simp
  show "\<lbrakk>x \<le> y; y \<le> x\<rbrakk> \<Longrightarrow> x = y"
    unfolding less_eq_ix_def using ix_to_nat_injectivity by (meson antisym injD)
  show "x \<le> y \<or> y \<le> x"
    unfolding less_eq_ix_def by auto
qed

end

text \<open> Derived types \<close>

type_synonym lovelace = int
type_synonym tx_in = "tx_id \<times> ix"
type_synonym tx_out = "addr \<times> lovelace"
type_synonym utxo = "(tx_in, tx_out) fmap"

text \<open> Transaction Types \<close>

type_synonym tx_body = "tx_in set \<times> (ix, tx_out) fmap"

text \<open> Abstract functions \<close>

fun txid :: "tx \<Rightarrow> tx_id" where
  "txid _ = undefined"

fun txbody :: "tx \<Rightarrow> tx_body" where
  "txbody _ = undefined"

text \<open> Accessor functions \<close>

fun txins :: "tx \<Rightarrow> tx_in set" where
  "txins tx = (let (inputs, _) = txbody tx in inputs)"

fun txouts :: "tx \<Rightarrow> utxo" where
  "txouts tx = fmap_of_list [((txid tx, ix), txout). (ix, txout) \<leftarrow> sorted_list_of_fmap (snd (txbody tx))]"

lemma dom_txouts_is_txid:
  assumes "(i, ix) \<in> fmdom' (txouts tx)"
  shows "i = txid tx"
proof -
  from assms have "(i, ix) \<in> fset (fmdom (fmap_of_list [((txid tx, ix), txout). (ix, txout) \<leftarrow> sorted_list_of_fmap (snd (txbody tx))]))"
    by (simp add: fmdom'_alt_def)
  then have "(i, ix) \<in> fset (fset_of_list (map fst [((txid tx, ix), txout). (ix, txout) \<leftarrow> sorted_list_of_fmap (snd (txbody tx))]))"
    by simp
  then have "(i, ix) \<in> fset (fset_of_list [(txid tx, ix). (ix, txout) \<leftarrow> sorted_list_of_fmap (snd (txbody tx))])"
    by auto
  then have "(i, ix) \<in> set [(txid tx, ix). (ix, txout) \<leftarrow> sorted_list_of_fmap (snd (txbody tx))]"
    by (simp add: fset_of_list.rep_eq)
  then show "i = txid tx"
    by auto
qed

fun balance :: "utxo \<Rightarrow> int" where
  "balance utxo = (\<Sum>txin \<in> fmdom' utxo. snd (utxo $$! txin))"

fun fee :: "utxo \<Rightarrow> tx \<Rightarrow> int" where
  "fee utxo tx = balance (txins tx \<lhd> utxo) - balance (txouts tx)"

text \<open> UTxO transition-system types \<close>

\<comment> \<open> UTxO environment \<close>
typedecl utxo_env \<comment> \<open> Abstract, don't care for now \<close>

\<comment> \<open> UTxO states \<close>
type_synonym utxo_state = "utxo \<times> lovelace"

text \<open> UTxO inference rules \<close>

inductive utxo_sts :: "utxo_env \<Rightarrow> utxo_state \<Rightarrow> tx \<Rightarrow> utxo_state \<Rightarrow> bool"
  ("_ \<turnstile> _ \<rightarrow>\<^bsub>UTXO\<^esub>{_} _" [51, 0, 51] 50)
  for \<Gamma>
  where
    utxo_inductive: "
      \<lbrakk>
        txins tx \<subseteq> fmdom' (fst s);
        txins tx \<noteq> {};
        txouts tx \<noteq> {$$};
        \<forall>(_, c) \<in> fmran' (txouts tx). c > 0;
        finite (fmdom' (fst s))
      \<rbrakk>
      \<Longrightarrow>
      \<Gamma> \<turnstile> s \<rightarrow>\<^bsub>UTXO\<^esub>{tx} ((txins tx \<lhd>/ (fst s)) ++\<^sub>f txouts tx, (snd s) + fee (fst s) tx)"

text \<open> Transaction sequences \<close>

inductive utxows :: "utxo_env \<Rightarrow> utxo_state \<Rightarrow> tx list \<Rightarrow> utxo_state \<Rightarrow> bool"
  ("_ \<turnstile> _ \<rightarrow>\<^bsub>UTXOWS\<^esub>{_} _" [51, 0, 51] 50)
  for \<Gamma>
  where
    empty: "\<Gamma> \<turnstile> s \<rightarrow>\<^bsub>UTXOWS\<^esub>{[]} s" |
    step: "\<Gamma> \<turnstile> s \<rightarrow>\<^bsub>UTXOWS\<^esub>{\<T> @ [tx]} s''" if "\<Gamma> \<turnstile> s \<rightarrow>\<^bsub>UTXOWS\<^esub>{\<T>} s'" and "\<Gamma> \<turnstile> s' \<rightarrow>\<^bsub>UTXO\<^esub>{tx} s''"

text \<open> Auxiliary lemmas \<close>

abbreviation txid_injectivity :: bool where
  "txid_injectivity \<equiv> \<forall>tx tx'. txid tx = txid tx' \<longrightarrow> tx = tx'"

lemma lemma_1:
  assumes "\<Gamma> \<turnstile> s\<^sub>0 \<rightarrow>\<^bsub>UTXOWS\<^esub>{\<T>} s"
  and "\<forall>T\<^sub>i \<in> set \<T>. txins T\<^sub>i \<inter> txins tx = {}"
  and "fmdom' (txouts tx) \<inter> fmdom' (fst s\<^sub>0) = {}"
  and txid_injectivity
  shows "(\<Union>T\<^sub>i \<in> set \<T>. txins T\<^sub>i) \<inter> fmdom' (txouts tx) = {}"
  and "fmdom' (txouts tx) \<inter> fmdom' (fst s) = {}"
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
  case (step s\<^sub>0 \<T> s tx' s')
  let ?utxo = "fst s"
  { case 1
    then have "txins tx' \<inter> fmdom' (txouts tx) = {}"
    proof -
      from "1.prems"(1) and assms(4) have tx_excl: "\<forall>T\<^sub>i \<in> set \<T>. txins T\<^sub>i \<inter> txins tx = {}"
        by auto
      with step.IH(1) and "1.prems"(2) and assms(4) have "(\<Union>T\<^sub>i \<in> set \<T>. txins T\<^sub>i) \<inter> fmdom' (txouts tx) = {}"
        by simp
      moreover from step.hyps(2) have "txins tx' \<subseteq> fmdom' ?utxo"
        using utxo_sts.simps by blast
      ultimately show ?thesis
        using "1.prems"(1) and assms(4) by simp
    qed
    moreover from "1.prems" and step.IH(1) have "(\<Union>T\<^sub>i \<in> set \<T>. txins T\<^sub>i) \<inter> fmdom' (txouts tx) = {}"
      by simp
    ultimately show ?case
      by simp
  next
    case 2
    from "2.prems"(1) and assms(4) and step.hyps(2) and step.IH(2) have "fmdom' (txouts tx) \<inter> fmdom' (txins tx' \<lhd>/ ?utxo) = {}"
      by (auto simp add: utxo_sts.simps)
    moreover have "fmdom' (txouts tx) \<inter> fmdom' (txouts tx') = {}"
    proof -
      from "2.prems"(1) have "txins tx' \<inter> txins tx = {}"
        by (meson in_set_conv_decomp)
      with step.hyps(2) have "txins tx' \<noteq> txins tx"
        by (metis inf.idem utxo_sts.cases)
      then have "tx' \<noteq> tx"
        by blast
      with assms(4) have "txid tx' \<noteq> txid tx"
        by blast
      then show ?thesis
        by simp
    qed
    ultimately have "fmdom' (txouts tx) \<inter> fmdom' ((txins tx' \<lhd>/ ?utxo) ++\<^sub>f txouts tx') = {}"
      by simp
    with step.hyps(2) show ?case
      by (simp add: utxo_sts.simps)
    }
qed

lemma aux_lemma:
  assumes "\<Gamma> \<turnstile> s\<^sub>0 \<rightarrow>\<^bsub>UTXOWS\<^esub>{\<T>} s"
  and "\<Gamma> \<turnstile> s \<rightarrow>\<^bsub>UTXO\<^esub>{tx} s'"
  and "\<forall>T\<^sub>i \<in> set (\<T> @ [tx]). fmdom' (txouts T\<^sub>i) \<inter> fmdom' (fst s\<^sub>0) = {}"
  and "\<forall>T\<^sub>i \<in> set \<T>. fmdom' (txouts T\<^sub>i) \<inter> fmdom' (fst s\<^sub>0) = {} \<Longrightarrow> \<forall>T\<^sub>i \<in> set \<T>. txins T\<^sub>i \<inter> fmdom' (fst s) = {}"
  and txid_injectivity
  shows "txins tx \<inter> fmdom' (txouts tx) = {}"
proof -
  let ?utxo = "fst s"
  from assms(2) have *: "txins tx \<subseteq> fmdom' ?utxo"
    by (simp add: utxo_sts.simps)
  with assms(3-5) have "\<forall>T\<^sub>i \<in> set \<T>. txins T\<^sub>i \<inter> txins tx = {}"
    by (metis (mono_tags, hide_lams) in_set_conv_decomp inf.idem inf.orderE txid.simps)
  with assms(1-3,5) have **: "fmdom' (txouts tx) \<inter> fmdom' ?utxo = {}"
    using lemma_1 by simp
  then show ?thesis
  proof -
    from * have "txins tx = txins tx \<inter> fmdom' (fst s)"
      by blast
    with ** have "txins tx \<inter> fmdom' (txouts tx) = txins tx \<inter> fmdom' (fst s) \<inter> {}"
      by blast
    then show ?thesis
      by simp
  qed
qed

lemma txins_txouts_exc:
  assumes "txid tx \<notin> {tid | tid ix. (tid, ix) \<in> fmdom' utxo}"
  shows "fmdom' (txins tx \<lhd>/ utxo) \<inter> fmdom' (txouts tx) = {}"
proof -
  from assms have "txid tx \<notin> {tid | tid ix. (tid, ix) \<in> fmdom' (txins tx \<lhd>/ utxo)}"
    by simp
  then have "\<And>txin. txin \<in> fmdom' (txins tx \<lhd>/ utxo) \<Longrightarrow> fst txin \<noteq> txid tx"
    by (smt mem_Collect_eq prod.collapse)
  moreover have "\<And>txin. txin \<in> fmdom' (txouts tx) \<Longrightarrow> fst txin = txid tx"
    using dom_txouts_is_txid by auto
  ultimately show ?thesis
    by blast
qed

lemma lemma_3:
  assumes "\<Gamma> \<turnstile> s\<^sub>0 \<rightarrow>\<^bsub>UTXOWS\<^esub>{\<T>} s"
  and "\<forall>T\<^sub>i \<in> set \<T>. fmdom' (txouts T\<^sub>i) \<inter> fmdom' (fst s\<^sub>0) = {}"
  and txid_injectivity
  shows "\<forall>T\<^sub>i \<in> set \<T>. txins T\<^sub>i \<inter> fmdom' (fst s) = {}"
using assms
proof (induction rule: utxows.induct)
  case (empty s)
  then show ?case
    by simp
next
  case (step s\<^sub>0 \<T> s tx s')
  let ?utxo = "fst s" and ?utxo' = "fst s'"
  have "\<And>T\<^sub>i. T\<^sub>i \<in> set \<T> \<Longrightarrow> txins T\<^sub>i \<inter> fmdom' ?utxo' = {}"
  proof -
    fix T\<^sub>i
    assume "T\<^sub>i \<in> set \<T>"
    then have "txins T\<^sub>i \<inter> fmdom' (txins tx \<lhd>/ ?utxo) = {}"
    proof -
      from step.IH and \<open>T\<^sub>i \<in> set \<T>\<close> and step.prems have "txins T\<^sub>i \<inter> fmdom' ?utxo = {}"
        by (metis butlast_snoc in_set_butlastD)
      then show ?thesis
        by (simp add: disjoint_iff_not_equal)
    qed
    moreover have "txins T\<^sub>i \<inter> fmdom' (txouts tx) = {}"
    proof -
      from step.hyps(2) have "txins tx \<subseteq> fmdom' ?utxo"
        using utxo_sts.simps by blast
      with step.IH and \<open>T\<^sub>i \<in> set \<T>\<close> and step.prems have "\<forall>T\<^sub>i \<in> set \<T>. txins T\<^sub>i \<inter> txins tx = {}"
        by (simp add: inf.absorb_iff2 inf_sup_aci(1))
      with step.hyps(1) and step.prems have "(\<Union>T\<^sub>i \<in> set \<T>. txins T\<^sub>i) \<inter> fmdom' (txouts tx) = {}"
        using lemma_1 by simp
      with \<open>T\<^sub>i \<in> set \<T>\<close> show ?thesis
        by blast
    qed
    ultimately have "txins T\<^sub>i \<inter> fmdom' ((txins tx \<lhd>/ ?utxo) ++\<^sub>f txouts tx) = {}"
      using \<open>T\<^sub>i \<in> set \<T>\<close> and step.IH and step.hyps(2) and step.prems(1,2)
      by (metis (mono_tags, hide_lams) inf.orderE last_in_set snoc_eq_iff_butlast txid.simps utxo_sts.simps)
    with step.hyps(2) show "txins T\<^sub>i \<inter> fmdom' ?utxo' = {}"
      by (auto simp add: utxo_sts.simps)
  qed
  moreover have "txins tx \<inter> fmdom' ?utxo' = {}"
  proof -
    have "txins tx \<inter> fmdom' (txins tx \<lhd>/ ?utxo) = {}"
      by fastforce
    moreover with step.IH and step.hyps(1-2) and step.prems have *: "txins tx \<inter> fmdom' (txouts tx) = {}"
      using aux_lemma by simp
    ultimately have "txins tx \<inter> fmdom' ((txins tx \<lhd>/ ?utxo) ++\<^sub>f txouts tx) = {}"
    proof -
      from * have "(txins tx \<lhd>/ ?utxo) ++\<^sub>f txouts tx = txins tx \<lhd>/ (?utxo ++\<^sub>f txouts tx)"
        using dom_exc_assoc by blast
      then show ?thesis
        by fastforce
      qed
    with step.hyps(2) show ?thesis
      by (auto simp add: utxo_sts.simps)
  qed
  ultimately show ?case
    by simp
qed

subsection \<open> No Double-Spending Property \<close>

theorem no_double_spending:
  assumes "\<Gamma> \<turnstile> s\<^sub>0 \<rightarrow>\<^bsub>UTXOWS\<^esub>{\<T>} s"
  and "\<forall>T\<^sub>i \<in> set \<T>. fmdom' (txouts T\<^sub>i) \<inter> fmdom' (fst s\<^sub>0) = {}"
  and txid_injectivity
  shows "\<forall>i \<ge> 0. \<forall>j < length \<T>. i < j \<longrightarrow> txins (\<T> ! i) \<inter> txins (\<T> ! j) = {}"
  using assms
proof (induction arbitrary: s rule: utxows.induct)
  case (empty s)
  then show ?case
    by simp
next
  case (step s\<^sub>0 \<T> s tx s')
  let ?utxo\<^sub>0 = "fst s\<^sub>0" and ?utxo = "fst s" and ?utxo' = "fst s'"
  show ?case
  proof (intro allI impI)
    fix i j
    assume "i \<ge> 0" and "j < length (\<T> @ [tx])" and "i < j"
    then consider
      (a) "j < length \<T>" |
      (b) "j = length \<T>"
      by fastforce
    then show "txins ((\<T> @ [tx]) ! i) \<inter> txins ((\<T> @ [tx]) ! j) = {}"
    proof (cases)
      case a
      with \<open>i \<ge> 0\<close> and \<open>i < j\<close> and step.prems and step.IH show ?thesis
        by auto
    next
      case b
      with \<open>i < j\<close> have "(\<T> @ [tx]) ! i = \<T> ! i"
        by (simp add: nth_append)
      moreover with \<open>j = length \<T>\<close> have "(\<T> @ [tx]) ! j = tx"
        by simp
      ultimately have "txins (\<T> ! i) \<inter> txins tx = {}"
      proof -
        from \<open>\<Gamma> \<turnstile> s\<^sub>0 \<rightarrow>\<^bsub>UTXOWS\<^esub>{\<T>} s\<close> and \<open>i < j\<close> and b and step.prems have "txins (\<T> ! i) \<inter> fmdom' ?utxo = {}"
          using lemma_3 by (metis UnCI nth_mem set_append)
        moreover from \<open>\<Gamma> \<turnstile> s \<rightarrow>\<^bsub>UTXO\<^esub>{tx} s'\<close> and utxo_sts.simps have "txins tx \<subseteq> fmdom' ?utxo"
           by simp
         ultimately show ?thesis
           by blast
      qed
      with \<open>(\<T> @ [tx]) ! j = tx\<close> and \<open>(\<T> @ [tx]) ! i = \<T> ! i\<close> show ?thesis
        by simp
    qed
  qed
qed

subsection \<open> UTxO Difference Property \<close>

primrec general_append :: "['a \<Rightarrow> ('b, 'c) fmap, 'a list] \<Rightarrow> ('b, 'c) fmap" where
  "general_append _ [] = {$$}" |
  "general_append f (x # xs) = f x ++\<^sub>f general_append f xs"

syntax
  "_general_append" :: "pttrn => 'a list => 'b \<rightharpoonup> 'c => 'b \<rightharpoonup> 'c" ("(3\<Oplus>_\<leftarrow>_. _)" [0, 0, 100] 100)
translations
  "\<Oplus>x\<leftarrow>xs. p" \<rightleftharpoons> "CONST general_append (\<lambda>x. p) xs"

lemma general_append_rev:
  shows "(\<Oplus>x\<leftarrow>(xs @ [y]). P x) = (\<Oplus>x\<leftarrow>xs. P x) ++\<^sub>f P y"
  by (induction xs) simp_all

theorem utxo_is_outputs_minus_inputs:
  assumes "\<Gamma> \<turnstile> s\<^sub>0 \<rightarrow>\<^bsub>UTXOWS\<^esub>{\<T>} s"
  and "\<forall>T\<^sub>i \<in> set \<T>. fmdom' (txouts T\<^sub>i) \<inter> fmdom' (fst s\<^sub>0) = {}"
  and txid_injectivity
  shows "(\<Union>T\<^sub>i \<in> set \<T>. txins T\<^sub>i) \<lhd>/ (fst s\<^sub>0 ++\<^sub>f (\<Oplus>T\<^sub>i\<leftarrow>\<T>. txouts T\<^sub>i)) = fst s"
  using assms
proof (induction rule: utxows.induct)
  case (empty s)
  then show ?case
    using dom_exc_empty by simp
next
  case (step s\<^sub>0 \<T> s tx s')
  let ?T' = "\<T> @ [tx]" and ?utxo\<^sub>0 = "fst s\<^sub>0" and ?utxo = "fst s"
  from step.hyps(1,2) have "\<Gamma> \<turnstile> s\<^sub>0 \<rightarrow>\<^bsub>UTXOWS\<^esub>{?T'} s'"
    by (simp add: utxows.step)
  with step.prems(1,2)
  have "\<forall>i \<ge> 0. \<forall>j < length ?T'. i < j \<longrightarrow> txins (?T' ! i) \<inter> txins (?T' ! j) = {}"
    using no_double_spending by simp
  with step.hyps(2) have *: "\<forall>T\<^sub>i \<in> set \<T>. txins T\<^sub>i \<inter> txins tx = {}"
    by (auto simp add: subsetCE utxo_sts.simps)
  have PO\<^sub>0: "(\<Union>T\<^sub>i \<in> set \<T>. txins T\<^sub>i) \<lhd>/ txouts tx = txouts tx"
  proof -
    from * and step.hyps(1) and step.prems(1,2)
    have "(\<Union>T\<^sub>i \<in> set \<T>. txins T\<^sub>i) \<inter> fmdom' (txouts tx) = {}"
      using lemma_1(1) by auto
    with * show ?thesis
      using dom_exc_empty by auto
  qed
  have PO\<^sub>1: "txins tx \<lhd>/ txouts tx = txouts tx"
  proof -
    from * and step.hyps(1) and step.prems(1,2)
    have "fmdom' (txouts tx) \<inter> fmdom' ?utxo = {}"
      using lemma_1(2) by simp
    moreover from step.hyps(2) have "txins tx \<subseteq> fmdom' ?utxo"
      using utxo_sts.simps by blast
    ultimately have "fmdom' (txouts tx) \<inter> txins tx = {}"
      by blast
    then show ?thesis
      using dom_exc_unit by blast
  qed
  have "
    (\<Union>T\<^sub>i \<in> set (\<T> @ [tx]). txins T\<^sub>i) \<lhd>/ (?utxo\<^sub>0 ++\<^sub>f (\<Oplus>T\<^sub>i\<leftarrow>(\<T> @ [tx]). txouts T\<^sub>i))
    =
    (txins tx \<union> (\<Union>T\<^sub>i \<in> set \<T>. txins T\<^sub>i)) \<lhd>/ (?utxo\<^sub>0 ++\<^sub>f (\<Oplus>T\<^sub>i\<leftarrow>(\<T> @ [tx]). txouts T\<^sub>i))"
    by simp
  also have "
    \<dots>
    =
    txins tx \<lhd>/ ((\<Union>T\<^sub>i \<in> set \<T>. txins T\<^sub>i) \<lhd>/ ((?utxo\<^sub>0 ++\<^sub>f (\<Oplus>T\<^sub>i\<leftarrow>\<T>. txouts T\<^sub>i)) ++\<^sub>f txouts tx))"
    using dom_exc_union_sec by (metis fmadd_assoc general_append_rev)
  also have "
    \<dots>
    =
    txins tx \<lhd>/
    (
      ((\<Union>T\<^sub>i \<in> set \<T>. txins T\<^sub>i) \<lhd>/ (?utxo\<^sub>0 ++\<^sub>f (\<Oplus>T\<^sub>i\<leftarrow>\<T>. txouts T\<^sub>i)))
      ++\<^sub>f
      ((\<Union>T\<^sub>i \<in> set \<T>. txins T\<^sub>i) \<lhd>/ txouts tx)
    )"
    using dom_exc_add_distr by simp
  also from step.IH and step.prems(1,2) have "
    \<dots>
    =
    txins tx \<lhd>/
    (
      ?utxo
      ++\<^sub>f
      ((\<Union>T\<^sub>i \<in> set \<T>. txins T\<^sub>i) \<lhd>/ txouts tx)
    )"
    by (metis butlast_snoc in_set_butlastD)
  also from PO\<^sub>0 have "\<dots> = txins tx \<lhd>/ (?utxo ++\<^sub>f txouts tx)"
    by simp
  also have "\<dots> = (txins tx \<lhd>/ ?utxo) ++\<^sub>f (txins tx \<lhd>/ txouts tx)"
    using dom_exc_add_distr by simp
  also from PO\<^sub>1 have "\<dots> = (txins tx \<lhd>/ ?utxo) ++\<^sub>f txouts tx"
    by simp
  finally show ?case
    using step.hyps(2) by (simp add: utxo_sts.simps)
qed

subsection \<open> Constant Money Property \<close>

lemma utxo_add_balance:
  assumes "txin \<notin> fmdom' utxo"
  shows "balance (utxo(txin $$:= txout)) = balance utxo + snd txout"
proof -
  let ?utxo' = "utxo(txin $$:= txout)"
  have "balance ?utxo' = (\<Sum>txin\<^sub>i \<in> (fmdom' utxo \<union> {txin}). snd (?utxo' $$! txin\<^sub>i))"
    by simp
  also from assms have "\<dots> = (\<Sum>txin\<^sub>i \<in> fmdom' utxo. snd (?utxo' $$! txin\<^sub>i)) + snd (?utxo' $$! txin)"
    by (simp add: fmdom'.rep_eq)
  also have "\<dots> = (\<Sum>txin\<^sub>i \<in> fmdom' utxo. snd (?utxo' $$! txin\<^sub>i)) + snd txout"
    by simp
  also from assms have "\<dots> = (\<Sum>txin\<^sub>i \<in> fmdom' utxo. snd (utxo $$! txin\<^sub>i)) + snd txout"
    by (metis (no_types, lifting) fmupd_lookup sum.cong)
  finally show ?thesis
    by simp
qed

lemma balance_union_distr:
  assumes "fmdom' utxo\<^sub>1 \<inter> fmdom' utxo\<^sub>2 = {}"
  shows "balance (utxo\<^sub>1 ++\<^sub>f utxo\<^sub>2) = balance utxo\<^sub>1 + balance utxo\<^sub>2"
  using assms
proof (induction utxo\<^sub>2 arbitrary: utxo\<^sub>1 rule: fmap_induct)
  case fmempty
  then show ?case sorry
next
  case (fmupd txin txout utxo\<^sub>2)
  have "balance (utxo\<^sub>1 ++\<^sub>f utxo\<^sub>2(txin $$:= txout)) = balance ((utxo\<^sub>1 ++\<^sub>f utxo\<^sub>2)(txin $$:= txout))"
    by simp
  also have "\<dots> = balance (utxo\<^sub>1 ++\<^sub>f utxo\<^sub>2) + snd txout"
  proof -
    have "fmdom' (utxo\<^sub>1 ++\<^sub>f utxo\<^sub>2) = fmdom' utxo\<^sub>1 \<union> fmdom' utxo\<^sub>2"
      by simp
    moreover from fmupd.prems have "txin \<notin> fmdom' utxo\<^sub>1"
      by auto
    moreover from fmupd.hyps have "txin \<notin> fmdom' utxo\<^sub>2"
      by (simp add: fmdom'_notI)
    ultimately have "txin \<notin> fmdom' (utxo\<^sub>1 ++\<^sub>f utxo\<^sub>2)"
      by simp
    then show ?thesis
      using utxo_add_balance by blast
  qed
  also from fmupd.IH and fmupd.prems have "\<dots> = balance utxo\<^sub>1 + balance utxo\<^sub>2 + snd txout"
    by simp
  also have "\<dots> = balance utxo\<^sub>1 + balance (utxo\<^sub>2(txin $$:= txout))"
  proof -
    from fmupd.hyps have "txin \<notin> fmdom' utxo\<^sub>2"
      by (simp add: fmdom'_notI)
    then show ?thesis
      using utxo_add_balance by simp
  qed
  finally show ?case .
qed

lemma utxo_txins_compl:
  assumes "A \<subseteq> fmdom' m"
  shows "A \<lhd>/ m = m --\<^sub>f (A \<lhd> m)"
  using assms
proof (induction m arbitrary: A rule: fmap_induct)
  case fmempty
  then show ?case
    by simp
next
  case (fmupd k v m)
  then show ?case
  proof (cases "k \<in> A")
    case True
    with fmupd.hyps have "m(k $$:= v) --\<^sub>f (A \<lhd> m(k $$:= v)) = m(k $$:= v) --\<^sub>f (((A - {k}) \<lhd> m) ++\<^sub>f {k $$:= v})"
      using dom_res_fmupd by (metis fmdom'_notI)
    also have "\<dots> = m(k $$:= v) --\<^sub>f ((A - {k}) \<lhd> m) --\<^sub>f {k $$:= v}"
      using fmsub_fmadd_distr .
    also have "\<dots> = (m(k $$:= v) --\<^sub>f {k $$:= v}) --\<^sub>f ((A - {k}) \<lhd> m)"
      by (blast intro: fmfilter_comm)
    also from fmupd.hyps have "\<dots> = m --\<^sub>f ((A - {k}) \<lhd> m)"
      using fmsub_fmupd by metis
    also have "\<dots> = (A - {k}) \<lhd>/ m"
    proof -
      from fmupd.prems and fmupd.hyps and True have "A - {k} \<subseteq> fmdom' m"
        by (simp add: subset_insert_iff)
      with fmupd.IH show ?thesis
        by presburger
    qed
    finally show ?thesis
      using True and fmupd.hyps and dom_exc_fmupd by fastforce
  next
    case False
    with fmupd.hyps have "m(k $$:= v) --\<^sub>f (A \<lhd> m(k $$:= v)) = m(k $$:= v) --\<^sub>f (A \<lhd> m)"
      by simp
    also from False have "\<dots> = (m --\<^sub>f (A \<lhd> m)) ++\<^sub>f {k $$:= v}"
      by simp
    also have "\<dots> = (A \<lhd>/ m) ++\<^sub>f {k $$:= v}"
    proof -
      from fmupd.prems and False have "A \<subseteq> fmdom' m"
        by auto
      with fmupd.IH show ?thesis
        by simp
    qed
    finally show ?thesis
      using False by simp
  qed
qed

lemma balance_exclusion:
  assumes "finite (fmdom' utxo)"
  and "utxo $$ txin = Some txout"
  shows "balance (fmdrop txin utxo) = balance utxo - snd txout"
proof -
  have "balance (fmdrop txin utxo) = (\<Sum>txin\<^sub>i \<in> (fmdom' utxo - {txin}). snd (utxo $$! txin\<^sub>i))"
    by simp
  with assms show ?thesis
    by (simp add: fmdom'I sum_diff1)
qed

lemma balance_minus_distr:
  assumes "finite (fmdom' utxo\<^sub>1)"
  and "finite (fmdom' utxo\<^sub>2)"
  and "utxo\<^sub>2 \<subseteq>\<^sub>f utxo\<^sub>1"
  shows "balance (utxo\<^sub>1 --\<^sub>f utxo\<^sub>2) = balance utxo\<^sub>1 - balance utxo\<^sub>2"
  using assms
proof (induction utxo\<^sub>2 arbitrary: utxo\<^sub>1 rule: fmap_induct)
  case fmempty
  then show ?case
    by simp
next
  case (fmupd txin txout utxo\<^sub>2)
  from fmupd.hyps(1) have "balance (utxo\<^sub>1 --\<^sub>f utxo\<^sub>2(txin $$:= txout)) = balance (fmdrop txin utxo\<^sub>1 --\<^sub>f utxo\<^sub>2)"
    using fmsub_fmdrop by (metis fmdrop_fset_single)
  also have "\<dots> = balance (fmdrop txin utxo\<^sub>1) - balance utxo\<^sub>2"
  proof -
    from fmupd.prems and fmupd.hyps have "utxo\<^sub>2 \<subseteq>\<^sub>f fmdrop txin utxo\<^sub>1"
      using fmsub_fmupd by (metis fmdom'_empty fmdom'_fmupd fmdrop_set_single fmfilter_alt_defs(2) fmsubset_drop_mono)
    moreover have "finite (fmdom' (fmdrop txin utxo\<^sub>1))"
    proof -
      have "fmdom' (fmdrop txin utxo\<^sub>1) \<subseteq> fmdom' utxo\<^sub>1"
        by (simp add: Diff_subset)
      with fmupd.prems(1) show ?thesis
        by (auto simp add: rev_finite_subset)
    qed
    moreover have "finite (fmdom' utxo\<^sub>2)"
      by (simp add: fmdom'.rep_eq)
    ultimately show ?thesis
      using fmupd.IH by fastforce
  qed
  also have "\<dots> = (balance utxo\<^sub>1 - snd txout) - balance utxo\<^sub>2"
  proof -
    from fmupd.prems(3) have "utxo\<^sub>1 $$ txin = Some txout"
      by (fastforce simp add: fmsubset_alt_def)
    with fmupd.prems(1) show ?thesis
      using balance_exclusion by auto
  qed
  also have "\<dots> = balance utxo\<^sub>1 - balance (utxo\<^sub>2(txin $$:= txout))"
  proof -
    have "\<dots> = balance utxo\<^sub>1 - (balance utxo\<^sub>2 + snd txout)"
      by simp
    with fmupd.hyps show ?thesis
      using utxo_add_balance by (metis fmdom'_notI)
  qed
  finally show ?case .
qed

theorem constant_money:
  assumes "\<Gamma> \<turnstile> s\<^sub>0 \<rightarrow>\<^bsub>UTXOWS\<^esub>{\<T>} s"
  and "\<forall>T\<^sub>i \<in> set \<T>. fmdom' (txouts T\<^sub>i) \<inter> fmdom' (fst s\<^sub>0) = {}"
  and txid_injectivity
  shows "snd s\<^sub>0 + balance (fst s\<^sub>0) = snd s + balance (fst s)"
  using assms
proof (induction rule: utxows.induct)
  case (empty s)
  then show ?case
    by simp
next
  case (step s\<^sub>0 \<T> s' tx s)
  let ?T' = "\<T> @ [tx]"
  let "(?utxo\<^sub>0, ?reserves\<^sub>0)" = "(fst s\<^sub>0, snd s\<^sub>0)"
  let "(?utxo, ?reserves)" = "(fst s, snd s)"
  let "(?utxo', ?reserves')" = "(fst s', snd s')"
  from step.hyps(2) have "?reserves = ?reserves' + balance (txins tx \<lhd> ?utxo') - balance (txouts tx)"
    by (simp add: utxo_sts.simps fee.simps)
  moreover have "balance ?utxo = balance ?utxo' - balance (txins tx \<lhd> ?utxo') + balance (txouts tx)"
  proof -
    have "fmdom' (txins tx \<lhd>/ ?utxo') \<inter> fmdom' (txouts tx) = {}"
    proof -
      from step.hyps(1,2) have "\<Gamma> \<turnstile> s\<^sub>0 \<rightarrow>\<^bsub>UTXOWS\<^esub>{?T'} s"
        by (simp add: utxows.step)
      with step.prems(1,2) have "\<forall>i \<ge> 0. \<forall>j < length ?T'. i < j \<longrightarrow> txins (?T' ! i) \<inter> txins (?T' ! j) = {}"
        using no_double_spending by auto
      with step.hyps(2) have "\<forall>T\<^sub>i \<in> set \<T>. txins T\<^sub>i \<inter> txins tx = {}"
        by (auto simp add: subsetCE utxo_sts.simps)
      with step.hyps(1) and step.prems(1,2) have "fmdom' (txouts tx) \<inter> fmdom' ?utxo' = {}"
        using lemma_1(2) by auto
      moreover have "fmdom' (txins tx \<lhd>/ ?utxo') \<subseteq> fmdom' ?utxo'"
        by auto
      ultimately show ?thesis
        by blast
    qed
    then have "balance ((txins tx \<lhd>/ ?utxo') ++\<^sub>f txouts tx) = balance (txins tx \<lhd>/ ?utxo') + balance (txouts tx)"
      using balance_union_distr by blast
    also have "\<dots> = balance (?utxo' --\<^sub>f (txins tx \<lhd> ?utxo')) + balance (txouts tx)"
    proof -
      from step.hyps(2) have "txins tx \<subseteq> fmdom' ?utxo'"
        by (simp add: utxo_sts.simps)
      then show ?thesis
        by (simp add: utxo_txins_compl)
    qed
    also have "\<dots> = balance ?utxo' - balance (txins tx \<lhd> ?utxo') + balance (txouts tx)"
    proof -
      have "txins tx \<lhd> ?utxo' \<subseteq>\<^sub>f ?utxo'"
        by (simp add: fmfilter_fmsubset)
      moreover from step.hyps(2) have "finite (fmdom' ?utxo')" and "finite (fmdom' (txins tx \<lhd> ?utxo'))"
        by (simp add: utxo_sts.simps)+
      ultimately show ?thesis
        using balance_minus_distr by presburger
    qed
    finally show ?thesis
      using step.hyps(2) by (auto simp add: utxo_sts.simps)
  qed
  ultimately have "?reserves + balance ?utxo = ?reserves' + balance (txins tx \<lhd> ?utxo') - balance (txouts tx) + balance ?utxo' - balance (txins tx \<lhd> ?utxo') + balance (txouts tx)"
    by simp
  also have "\<dots> = ?reserves' + balance ?utxo'"
    by linarith
  finally show ?case
    using step.IH and step.prems(1,2) by auto
qed

end
