#include <iostream>
#include <random>

#include <gmpxx.h>

// create random number numbers for golden tests to test all functions
//
// format is: x   : [0.1; 100.0]
//            a b : [0.0000000000001; 1.0]
//
// where all are of precision 10^34
//
// expected output:
//    exp' x
//    -ln' a   - to avoid negative numbers
//    1 - (1 - f) *** b  - as in VRF leader validation
//    Taylor approx(exp' (b * ln' (1 - f))) with error bars and iteration count for
//    a < 1 - (1 - f) *** b <=> 1/(1-a) < exp(-b * ln' (1 - f))
int main()
{
  size_t n;
  std::cin >> n;

  const double l = 0.0000000000001;
  const double u = 1.0;

  const double l_x = 0.1;
  const double u_x = 100.0;
  std::uniform_real_distribution<double> uniform(l, u);
  std::uniform_real_distribution<double> uniform_x(l_x, u_x);
  std::default_random_engine generator;

  for (size_t i = 0; i < n; i++)
    {
      double x_ = uniform_x(generator);
      x_ *= 1e34;
      mpz_class x(x_);

      double a_ = uniform(generator);
      a_ *= 1e34;
      mpz_class a(a_);

      double b_ = uniform(generator);
      b_ *= 1e34;
      mpz_class b(b_);

      std::cout << x << " " << a << " " << b << std::endl;
    }
  return 0;
}
