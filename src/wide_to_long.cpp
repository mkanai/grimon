#include <Rcpp.h>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
NumericMatrix wide_to_long(NumericMatrix x, int z_interval) {
  int n = x.rows(), m = x.cols() / 2;
  NumericMatrix ret(n * m, 3);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < m; j++) {
      ret(n*j + i, 0) = x(i, 2*j);
      ret(n*j + i, 1) = x(i, 2*j+1);
      ret(n*j + i, 2) = z_interval * (j + 1);
    }
  }

  return ret;
}
