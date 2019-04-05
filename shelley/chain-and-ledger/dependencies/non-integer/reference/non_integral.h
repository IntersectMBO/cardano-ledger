#ifndef NONINTEGRAL_H

#define NONINTEGRAL_H

#include <stdbool.h>

#include <gmp.h>

void initialize(const mpz_t, const mpz_t);
void cleanup();
bool ref_ln(mpz_t, const mpz_t);
int ref_exp(mpz_t, const mpz_t);
void ref_pow(mpz_t, const mpz_t, const mpz_t);
void scale(mpz_t);
void ref_div(mpz_t, const mpz_t, const mpz_t);

typedef enum { GT, LT, UNKNOWN } estimation;

typedef struct {
  int iterations;
  estimation estimate;
} mp_exp_cmp_result;

mp_exp_cmp_result ref_exp_cmp(mpz_t, const int, const mpz_t, const int, const mpz_t);

#endif
