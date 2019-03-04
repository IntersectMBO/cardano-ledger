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

  std::chrono::duration<double> total = std::chrono::duration<double>::zero();
  std::chrono::duration<double> maximal = std::chrono::duration<double>::zero();

  // format is "base exponent"
  size_t n = 0;
  for (std::string s; std::getline(std::cin, s); )
    {
      size_t split = s.find(' ');
      if(split != std::string::npos)
        {
          mpz_class base(s.substr(0, split));
          mpz_class exponent(s.substr(split, std::string::npos));

          auto before = std::chrono::high_resolution_clock::now();
          mpz_class result;
          ref_pow(
            result.get_mpz_t(),
            base.get_mpz_t(),
            exponent.get_mpz_t());

          auto after = std::chrono::high_resolution_clock::now();
          n++;
          std::chrono::duration<double> diff = after - before;
          total += diff;
          if(maximal < diff)
            maximal = diff;
          std::cout << print_fixedp(result, precision, 34)
                    << std::endl;
        }
    }

  std::cerr << "total time: " << total.count()
            << " average time: " << (total.count() / n)
            << " maximal time: " << maximal.count()
            << std::endl;

  cleanup();
  return 0;
}
