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

  mpz_class ten("10");
  mpz_class precision;
  mpz_pow_ui(precision.get_mpz_t(), ten.get_mpz_t(), 34);

  mpz_class epsilon;
  mpz_pow_ui(epsilon.get_mpz_t(), ten.get_mpz_t(), 34 - 24);

  mpz_class resolution;
  mpz_pow_ui(resolution.get_mpz_t(), ten.get_mpz_t(), 17);

  initialize(precision.get_mpz_t(), epsilon.get_mpz_t());

  std::chrono::duration<double> total_exp =
    std::chrono::duration<double>::zero();
  std::chrono::duration<double> total_pow =
    std::chrono::duration<double>::zero();
  std::chrono::duration<double> total_cmp =
    std::chrono::duration<double>::zero();
  std::chrono::duration<double> maximal_exp =
    std::chrono::duration<double>::zero();
  std::chrono::duration<double> maximal_pow =
    std::chrono::duration<double>::zero();
  std::chrono::duration<double> maximal_cmp =
    std::chrono::duration<double>::zero();

  // format is "sigma p"
  size_t n = 0;
  int iterations = 0;
  int max_iters = 0;

  int cmp_iterations = 0;
  int cmp_max_iters = 0;

  mpz_class f;
  mpz_class one("1");
  one *= precision;
  f = one / ten;

  mpz_class base;
  mpz_class exponent;
  mpz_class e;
  ref_exp(e.get_mpz_t(), one.get_mpz_t());

  for (std::string s; std::getline(std::cin, s); )
    {
      size_t split = s.find(' ');
      if(split != std::string::npos)
        {
          mpz_class sigma(s.substr(0, split));
          mpz_class p(s.substr(split, std::string::npos));
          mpz_class result;
          mpz_class result_cf;
          mpz_class result_pow;
          mpz_class result_cmp;
          std::chrono::duration<double> diff;
          mp_exp_cmp_result res;
          bool leader = false;

          std::cout << "data: " << print_fixedp(sigma, precision, 34)
                    << " " << print_fixedp(p, precision, 34)
                    << std::endl;

          {
            auto before = std::chrono::high_resolution_clock::now();
            base = one - f;
            ref_pow(result_pow.get_mpz_t(), base.get_mpz_t(), sigma.get_mpz_t());
            if(p < (one - result_pow))
              leader = true;
            auto after = std::chrono::high_resolution_clock::now();

            diff = after - before;
            total_pow += diff;
            if(maximal_pow < diff)
              maximal_pow = diff;
          }

          {
            mpz_class temp;
            mpz_class c;
            c = one - f;
            ref_ln(temp.get_mpz_t(), c.get_mpz_t());
            auto before = std::chrono::high_resolution_clock::now();
            temp = temp * sigma;
            scale(temp.get_mpz_t());
            int iters = ref_exp(result.get_mpz_t(), temp.get_mpz_t());
            if(!((p < (one - result) && leader) || (p >= (one - result) && !leader)))
              std::cout << "differing results!" << std::endl;
            auto after = std::chrono::high_resolution_clock::now();

            iterations += iters;
            if(iters > max_iters)
              max_iters = iters;
            diff = after - before;
            total_exp += diff;
            if(maximal_exp < diff)
              maximal_exp = diff;
          }

          {
            mpz_class temp;
            mpz_class c;
            c = one - f;
            ref_ln(temp.get_mpz_t(), c.get_mpz_t());

            mpz_class alpha = -sigma * temp;
            scale(alpha.get_mpz_t());

            mpz_class q_ = one - p;
            mpz_class q;
            ref_div(q.get_mpz_t(), one.get_mpz_t(), q_.get_mpz_t());

            auto before = std::chrono::high_resolution_clock::now();
            // e is used as bound, might need to be adapted for other use cases
            res =
              ref_exp_cmp(result_cmp.get_mpz_t(), 1000, alpha.get_mpz_t(), 3, q.get_mpz_t());

            // we compare 1/(1-p) < e^-(1-(1-f)^sigma)
            if(leader && res.estimate != LT)
              {
                std::cout << "wrong result should be leader "
                          << print_fixedp(temp, precision, 34)
                          << " should be more like "
                          << print_fixedp(result_pow, precision, 34)
                          << std::endl;
              }

            if(!leader && res.estimate != GT)
              {
                std::cout << "wrong result should not be leader "
                          << print_fixedp(temp, precision, 34)
                          << " should be more like "
                          << print_fixedp(result_pow, precision, 34)
                          << std::endl;
              }

            auto after = std::chrono::high_resolution_clock::now();
            cmp_iterations += res.iterations;
            if(res.iterations > cmp_max_iters)
              cmp_max_iters = res.iterations;
            diff = after - before;
            total_cmp += diff;
            if(maximal_cmp < diff)
              maximal_cmp = diff;
          }

          n++;
          std::cout // << print_fixedp(result_pow, precision, 34)
                    // << " "
                    << print_fixedp(result_pow, precision, 34)
                    << " "
                    << print_fixedp(result, precision, 34)
                    << " leader " << leader
                    << " cmp " << ((res.estimate == GT) ? "GT" : "LT")
                    << std::endl;
        }
    }

  std::cout << "exp(x * const) avg: " << (total_exp.count() / n)
            << " maximal time: " << maximal_exp.count()
            << " iterations avg " << (iterations * 1.0) / n
            << " maximal iterations " << max_iters
            << std::endl;
  std::cout << "pow avg: " << (total_pow.count() / n)
            << " maximal time: " << maximal_pow.count()
            << std::endl;

  std::cout << "cmp avg: " << (total_cmp.count() / n)
            << " maximal time: " << maximal_cmp.count()
            << " iterations avg " << (cmp_iterations * 1.0) / n
            << " maximal iterations " << cmp_max_iters
            << std::endl;

  cleanup();
  return 0;
}
