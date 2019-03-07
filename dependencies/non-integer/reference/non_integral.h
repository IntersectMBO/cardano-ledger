#ifndef NONINTEGRAL_H

#define NONINTEGRAL_H

#include <stdbool.h>

#include <gmp.h>

void initialize(const mpz_t, const mpz_t);
void cleanup();
bool ref_ln(mpz_t, const mpz_t);
void ref_exp(mpz_t, const mpz_t);
void ref_pow(mpz_t, const mpz_t, const mpz_t);
void ref_pos_taylor(mpz_t rop, const mpz_t x);

#endif
