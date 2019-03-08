#include <math.h>
#include <stdio.h>

#include "non_integral.h"

mpz_t one;
mpz_t zero;
mpz_t precision;
mpz_t e;
mpz_t eps;

void initialize(const mpz_t _precision, const mpz_t epsilon)
{
  mpz_init_set(precision, _precision);
  mpz_init_set_ui(one, 1); mpz_mul(one, one, precision);
  mpz_init_set_ui(zero, 0);
  mpz_init_set(eps, epsilon);

  mpz_init(e);
  ref_exp(e, one);
}

void div_qr(mpz_t q, mpz_t r, const mpz_t x, const mpz_t y)
{
  mpz_t temp_r, temp_q;
  mpz_init(temp_r); mpz_init(temp_q);
  mpz_tdiv_qr(temp_q, temp_r, x, y);
  mpz_set(r, temp_r);
  mpz_set(q, temp_q);
  mpz_clear(temp_r); mpz_clear(temp_q);
}

void div(mpz_t rop, const mpz_t x, const mpz_t y)
{
  mpz_t temp_r, temp_q, temp;
  mpz_init(temp_r); mpz_init(temp_q); mpz_init(temp);

  div_qr(temp_q, temp_r, x, y);

  mpz_mul(temp, temp_q, precision);
  mpz_mul(temp_r, temp_r, precision);
  div_qr(temp_q, temp_r, temp_r, y);

  mpz_add(temp, temp, temp_q);
  mpz_set(rop, temp);

  mpz_clear(temp_r); mpz_clear(temp_q); mpz_clear(temp);
}

void scale(mpz_t rop)
{
  mpz_t temp, a;
  mpz_init(temp); mpz_init(a);

  div_qr(a, temp, rop, precision);
  if(mpz_sgn(rop) < 0 && mpz_cmp(temp, zero) != 0)
    mpz_sub_ui(a, a, 1);

  mpz_set(rop, a);
  mpz_clear(temp); mpz_clear(a);
}

void cleanup()
{
  mpz_clear(one);
  mpz_clear(zero);
  mpz_clear(precision);
  mpz_clear(eps);
  mpz_clear(e);
}

void ipow_(mpz_t rop, const mpz_t x, int n)
{
  if(n == 0)
    mpz_set(rop, one);
  else if(n % 2 == 0)
    {
      mpz_t res;
      mpz_init(res);
      ipow_(res, x, n / 2);
      mpz_mul(rop, res, res);
      scale(rop);
      mpz_clear(res);
    }
  else
    {
      mpz_t res;
      mpz_init(res);
      ipow_(res, x, n - 1);
      mpz_mul(rop, res, x);
      scale(rop);
      mpz_clear(res);
    }
}

void ipow(mpz_t rop, const mpz_t x, int n)
{
  if(n < 0)
    {
      mpz_t temp;
      mpz_init(temp);

      ipow_(temp, x, -n);
      div(rop, one, temp);

      mpz_clear(temp);
    }
  else
    ipow_(rop, x, n);
}

/* Compute approximation of 'exp(x)' using continued fractions. Either for a
   maximum of 'maxN' iterations or until the absolute difference between two
   succeeding convergents is smaller than 'eps'. Assumes 'x' to be within
   [0,1]. */
void mp_expN(mpz_t rop, const int maxN, const mpz_t x, const mpz_t epsilon)
{
  mpz_t AnM2, BnM2, AnM1, BnM1, ba, aa, A, bb, ab, B, convergent, last, a, b;
  mpz_t curr_n, diff, temp_q, temp_r;
  bool first = true;
  int n = 1;

  /* initialize all MP variables */
  mpz_init(AnM2); mpz_init(BnM2); mpz_init(AnM1); mpz_init(BnM1);
  mpz_init(ba); mpz_init(aa); mpz_init(bb); mpz_init(ab);
  mpz_init(A); mpz_init(B); mpz_init(convergent); mpz_init(last);
  mpz_init(curr_n); mpz_init(a); mpz_init(b);
  mpz_init(diff); mpz_init(temp_q); mpz_init(temp_r);

  /* initialize values */
  /* this is initially 1 and then -n */
  mpz_set_si(curr_n, -1); mpz_mul(curr_n, curr_n, precision);
  mpz_set(a, x);
  mpz_set(b, one);

  mpz_set(AnM2, one);
  mpz_set_ui(BnM2, 0);
  mpz_set(AnM1, one);
  mpz_set(BnM1, one);

  while(n <= maxN + 1)
    {
      mpz_mul(ba, b, AnM1); scale(ba);
      mpz_mul(aa, a, AnM2); scale(aa);
      mpz_add(A, ba, aa);

      mpz_mul(bb, b, BnM1); scale(bb);
      mpz_mul(ab, a, BnM2); scale(ab);
      mpz_add(B, bb, ab);

      div(convergent, A, B);

      if(first)
        first = false;
      else
        {
          mpz_sub(diff, convergent, last);
          if(mpz_cmpabs(diff, epsilon) < 0)
            break;
        }

      mpz_set(last, convergent);
      n++;
      mpz_set(AnM2, AnM1);
      mpz_set(BnM2, BnM1);
      mpz_set(AnM1, A);
      mpz_set(BnM1, B);

      mpz_mul(a, curr_n, x); scale(a);
      mpz_sub(curr_n, curr_n, one);
      mpz_sub(b, x, curr_n);
    }

  mpz_set(rop, convergent);

  /* clear all MP values */
  mpz_clear(AnM2); mpz_clear(BnM2); mpz_clear(AnM1); mpz_clear(BnM1);
  mpz_clear(ba); mpz_clear(aa); mpz_clear(bb); mpz_clear(ab);
  mpz_clear(A); mpz_clear(B); mpz_clear(convergent); mpz_clear(last);
  mpz_clear(curr_n); mpz_clear(a); mpz_clear(b);
  mpz_clear(diff); mpz_clear(temp_q); mpz_clear(temp_r);
}

/* Compute an approximation of 'ln(1 + x)' via continued fractions. Either for a
   maximum of 'maxN' iterations or until the absolute difference between two
   succeeding convergents is smaller than 'eps'. Assumes 'x' to be within
   [1,e). */
void mp_lnN(mpz_t rop, const int maxN, const mpz_t x, const mpz_t epsilon)
{
  mpz_t AnM2, BnM2, AnM1, BnM1, ba, aa, A, bb, ab, B, convergent, last, a, b;
  mpz_t curr_n, diff, temp_q, temp_r;
  bool first = true;
  int n = 1;

  /* initialize all MP variables */
  mpz_init(AnM2); mpz_init(BnM2); mpz_init(AnM1); mpz_init(BnM1);
  mpz_init(ba); mpz_init(aa); mpz_init(bb); mpz_init(ab);
  mpz_init(A); mpz_init(B); mpz_init(convergent); mpz_init(last);
  mpz_init(curr_n); mpz_init(a); mpz_init(b);
  mpz_init(diff); mpz_init(temp_q); mpz_init(temp_r);

  /* initialize values */
  /* this is initially 1 and then -n */
  mpz_set_si(curr_n, -1); mpz_mul(curr_n, curr_n, precision);
  mpz_set(a, x);
  mpz_set(b, one);

  mpz_set(AnM2, one);
  mpz_set_ui(BnM2, 0);
  mpz_set_ui(AnM1, 0);
  mpz_set(BnM1, one);

  size_t curr_a = 1;

  while(n <= maxN + 2)
    {
      const size_t curr_a_2 = curr_a * curr_a;
      mpz_mul_ui(a, x, curr_a_2);
      if(n > 1 && n % 2 == 1)
        curr_a++;

      mpz_mul(ba, b, AnM1); scale(ba);
      mpz_mul(aa, a, AnM2); scale(aa);
      mpz_add(A, ba, aa);

      mpz_mul(bb, b, BnM1); scale(bb);
      mpz_mul(ab, a, BnM2); scale(ab);
      mpz_add(B, bb, ab);

      div(convergent, A, B);

      if(first)
        first = false;
      else
        {
          mpz_sub(diff, convergent, last);
          if(mpz_cmpabs(diff, epsilon) < 0)
            break;
        }

      mpz_set(last, convergent);
      n++;
      mpz_set(AnM2, AnM1);
      mpz_set(BnM2, BnM1);
      mpz_set(AnM1, A);
      mpz_set(BnM1, B);

      mpz_add(b, b, one);
    }

  mpz_set(rop, convergent);

  /* clear all MP values */
  mpz_clear(AnM2); mpz_clear(BnM2); mpz_clear(AnM1); mpz_clear(BnM1);
  mpz_clear(ba); mpz_clear(aa); mpz_clear(bb); mpz_clear(ab);
  mpz_clear(A); mpz_clear(B); mpz_clear(convergent); mpz_clear(last);
  mpz_clear(curr_n); mpz_clear(a); mpz_clear(b);
  mpz_clear(diff); mpz_clear(temp_q); mpz_clear(temp_r);
}

/* Entry point for 'exp' approximation. First does the scaling of 'x' to [0,1]
   and then calls the continued fraction approximation function. */
void ref_exp(mpz_t rop, const mpz_t x)
{
  mpz_t temp_q, temp_r;
  mpz_init(temp_q); mpz_init(temp_r);

  if(mpz_cmp(x, zero) == 0)
    mpz_set(rop, one);
  else if(mpz_cmp(x, zero) < 0)
    {
      mpz_t temp, x_;
      mpz_init(temp); mpz_init(x_);
      mpz_neg(x_, x);

      ref_exp(temp, x_);

      div(rop, one, temp);

      mpz_clear(x_);
      mpz_clear(temp);
    }
  else
    {
      mpz_t n_exponent, x_, temp_r, temp_q;
      mpz_init(n_exponent); mpz_init(x_); mpz_init(temp_r); mpz_init(temp_q);

      mpz_cdiv_q(n_exponent, x, precision);
      int n = mpz_get_ui(n_exponent);
      mpz_mul(n_exponent, n_exponent, precision); /* ceil(x) */

      mpz_tdiv_q_ui(x_, x, n);
      mp_expN(rop, 1000, x_, eps);

      ipow(rop, rop, n);
      mpz_clear(n_exponent); mpz_clear(x_); mpz_clear(temp_r); mpz_clear(temp_q);
    }

  mpz_clear(temp_r); mpz_clear(temp_q);
}

int findE(const mpz_t x)
{
  mpz_t x_, x__, temp_q, temp_r;
  mpz_init(x_); mpz_init(x__); mpz_init(temp_q); mpz_init(temp_r);

  div(x_, one, e);
  mpz_set(x__, e);

  int l = -1;
  int u =  1;
  while(mpz_cmp(x_, x) > 0 || mpz_cmp(x__, x) < 0)
    {

      /* x'_{n + 1} = x'_n ^ 2 */
      mpz_mul(x_, x_, x_);
      scale(x_);

      /* x''_{n + 1} = x''_n ^ 2 */
      mpz_mul(x__, x__, x__);
      scale(x__);

      l   *= 2;
      u   *= 2;
    }

  while(l+1 != u)
    {
      const int mid = l + ((u - l) / 2);

      ipow(x_, e, mid);
      if(mpz_cmp(x, x_) < 0)
        u = mid;
      else
        l = mid;
    }

  mpz_clear(x_); mpz_clear(x__); mpz_clear(temp_q); mpz_clear(temp_r);
  return l;
}

/* Entry point for 'ln' approximation. First does the necessary scaling, and
   then calls the continued fraction calculation. For any value outside the
   domain, i.e., 'x in (-inf,0]', the function returns '-INFINITY'. */
bool ref_ln(mpz_t rop, const mpz_t x)
{
  if(!(mpz_cmp(x, zero) > 0))
    return false;

  const int n = findE(x);
  mpz_t temp_r, temp_q, x_, factor;
  mpz_init(temp_r); mpz_init(temp_q); mpz_init(x_); mpz_init(factor);

  /* integral part of ln */
  mpz_set_si(rop, n);
  mpz_mul(rop, rop, precision);
  ref_exp(factor, rop);

  div(x_, x, factor);

  mpz_sub(x_, x_, one);

  mp_lnN(x_, 1000, x_, eps);
  mpz_add(rop, rop, x_);

  mpz_clear(temp_r); mpz_clear(temp_q); mpz_clear(x_); mpz_clear(factor);
  return true;
}

void ref_pow(mpz_t rop, const mpz_t base, const mpz_t exponent)
{
  /* x^y = exp(y * ln x) */

  mpz_t tmp;
  mpz_init(tmp);

  ref_ln(tmp, base);
  mpz_mul(tmp, tmp, exponent);
  scale(tmp);
  ref_exp(rop, tmp);

  mpz_clear(tmp);
}
