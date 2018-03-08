#include <Rcpp.h>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
NumericMatrix construct_segment_matrix(NumericMatrix x) {
  int n = x.rows(), m = x.cols() / 3;
  NumericMatrix ret(n * 2 * (m-1), 3);
  for (int i = 0; i < n; i++) {
    // start points on the most left (min z) plane.
    for (int j = 0; j < 3; j++) {
      ret(2*(m-1)*i, j) = x(i, j);
    }

    for (int j = 1; j < m-1; j++) {
      for (int k = 0; k < 3; k++) {
        ret(2*(m-1)*i+2*j-1, k) = x(i, 3*j+k);
        ret(2*(m-1)*i+2*j, k) = x(i, 3*j+k);
      }
    }

    // end points on the most right (max z) plane.
    for (int j = 3*(m-1); j < 3*m; j++) {
      ret(2*(m-1)*i+2*m-3, j - 3*(m-1)) = x(i, j);
    }
  }
  return ret;
}
