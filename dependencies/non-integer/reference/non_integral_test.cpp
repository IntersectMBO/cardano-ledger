#include <chrono>
#include <iostream>
#include <sstream>
#include <string>

#include <gmpxx.h>

#include "non_integral.hpp"

const std::string print_fixedp(
  const mpz_class &n,
  const mpz_class &precision,
  const size_t width)
{
  mpz_class temp_r, temp_q;
  // use truncate rounding here for consistency
  mpz_tdiv_qr(temp_q.get_mpz_t(),
              temp_r.get_mpz_t(),
              n.get_mpz_t(),
              precision.get_mpz_t());

  std::stringstream s;
  s << temp_q << ".";
  s.fill('0');
  s.width(width);
  s << temp_r;

  return s.str();
}

int main()
{

  mpz_class precision("10");
  mpz_pow_ui(precision.get_mpz_t(), precision.get_mpz_t(), 34);

  mpz_class epsilon("10");
  mpz_pow_ui(epsilon.get_mpz_t(), epsilon.get_mpz_t(), 34 - 24);

  mpz_class resolution("10");
  mpz_pow_ui(resolution.get_mpz_t(), resolution.get_mpz_t(), 17);

  initialize(precision.get_mpz_t(), epsilon.get_mpz_t());

  std::chrono::duration<double> total_exp =
    std::chrono::duration<double>::zero();
  std::chrono::duration<double> total_pow =
    std::chrono::duration<double>::zero();
  std::chrono::duration<double> total_cf =
    std::chrono::duration<double>::zero();
  std::chrono::duration<double> maximal_exp =
    std::chrono::duration<double>::zero();
  std::chrono::duration<double> maximal_pow =
    std::chrono::duration<double>::zero();
  std::chrono::duration<double> maximal_cf =
    std::chrono::duration<double>::zero();

  // format is "base exponent"
  size_t n = 0;
  for (std::string s; std::getline(std::cin, s); )
    {
      size_t split = s.find(' ');
      if(split != std::string::npos)
        {
          mpz_class base(s.substr(0, split));
          mpz_class exponent(s.substr(split, std::string::npos));
          mpz_class result;
          mpz_class result_cf;
          mpz_class result_pow;
          std::chrono::duration<double> diff;

          {
            auto before = std::chrono::high_resolution_clock::now();

            ref_exp_taylor(result.get_mpz_t(), base.get_mpz_t());
            auto after = std::chrono::high_resolution_clock::now();
            diff = after - before;
            total_exp += diff;
            if(maximal_exp < diff)
              maximal_exp = diff;
          }

          // {
          //   auto before = std::chrono::high_resolution_clock::now();
          //   ref_pow(result_pow.get_mpz_t(), base.get_mpz_t(), exponent.get_mpz_t());
          //   auto after = std::chrono::high_resolution_clock::now();
          //   diff = after - before;
          //   total_pow += diff;
          //   if(maximal_pow < diff)
          //     maximal_pow = diff;
          // }

          {
            auto before = std::chrono::high_resolution_clock::now();
            ref_exp(result_cf.get_mpz_t(), base.get_mpz_t());
            auto after = std::chrono::high_resolution_clock::now();
            diff = after - before;
            total_cf += diff;
            if(maximal_cf < diff)
              maximal_cf = diff;
          }

          n++;
          std::cout // << print_fixedp(result_pow, precision, 34)
                    // << " "
                    << print_fixedp(result, precision, 34)
                    << " "
                    << print_fixedp(result_cf, precision, 34)
                    << std::endl;
        }
    }

  std::cerr << "exp avg: " << (total_exp.count() / n)
            << " maximal time: " << maximal_exp.count()
            << std::endl;
  std::cerr << "cf avg: " << (total_cf.count() / n)
            << " maximal time: " << maximal_cf.count()
            << std::endl;
  // std::cerr << "pow avg: " << (total_pow.count() / n)
  //           << " maximal time: " << maximal_pow.count()
  //           << std::endl;

  cleanup();
  return 0;
}
