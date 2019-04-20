#include <iostream>
#include <random>

#include <gmpxx.h>

// create random number pairs for exponentiation testing
//
// format is: base exponent
// where both are of precision 10^34, taken from the interval [0.1; 19.1]
int main()
{
  size_t n;
  std::cin >> n;

  const double l = 0.0000000000001;
  const double u = 1.0;

  std::uniform_real_distribution<double> uniform(l, u);
  std::default_random_engine generator;

  for (size_t i = 0; i < n; i++)
    {
      double b = uniform(generator);
      b *= 1e34;
      mpz_class base(b);
      double e = uniform(generator);
      e *= 1e34;
      mpz_class exponent(e);

      std::cout << base << " " << exponent << std::endl;
    }
  return 0;
}
