#include <Rcpp.h>
#include <list>
#include <sstream>
#include <string>
#include <vector>

template<typename T>
void
print_anything(T x)
{
  for (auto&& x_i : x) {
    Rcpp::Rcout << x_i << "\n";
  }
}

std::vector<std::string>
get_unique_setnames(std::vector<std::string>& x)
{
  std::vector<std::string> out;

  for (auto x_i : x) {
    std::stringstream stream(x_i);
    std::string segment;

    while (std::getline(stream, segment, '&')) {
      if (std::find(std::begin(out), std::end(out), segment) == std::end(out)) {
        out.push_back(segment);
      }
    }
  }

  return out;
}

arma::umat
bit_index(const int n)
{
  umat out(std::pow(2, n) - 1, n);

  for (size_t i = 1, k = 0; i < n + 1; ++i) {
    std::vector<bool> v(n);
    std::fill(v.begin(), v.begin() + i, true);
    do {
      for (int j = 0; j < n; ++j) {
        out(k, j) = v[j] ? 1 : 0;
      }
      k++;
    } while (std::prev_permutation(v.begin(), v.end()));
  }
  return out;
}

// [[Rcpp::export]]
void
euler_diagram_cpp(std::vector<std::string> combination_names,
                  std::vector<double> combination_values)
{
  std::vector<std::string> setnames = get_unique_setnames(combination_names);

  for (auto x_i : setnames) {
    print_anything(x_i);
  }
}
