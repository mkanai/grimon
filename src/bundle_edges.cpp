#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;
using Eigen::Map;
using Eigen::MatrixXd;

// [[Rcpp::depends("RcppEigen")]]

MatrixXd generate_sub_coordinates(MatrixXd x, int n_subs) {
  int n = x.rows(), m = x.cols() / 3;

  // (x, y, z) for each sub coordinates (n_subs) of each line (m-1)
  MatrixXd subcoords(n, 3 * n_subs * (m-1));

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < m - 1; j++) {
      // compute sub coordinates for the given line.
      MatrixXd v = (x.block(i, 3*(j+1), 1, 3) - x.block(i, 3*j, 1, 3)) / (n_subs + 1);
      for (int k = 0; k < n_subs; k++) {
        subcoords.block(i, 3*(j*n_subs+k), 1, 3) = x.block(i, 3*j, 1, 3) + v * (k+1);
      }
    }
  }

  return subcoords;
}

// [[Rcpp::export]]
NumericMatrix generate_sub_coordinates(Eigen::Map<Eigen::MatrixXd> x, int n_subs = 3) {
  MatrixXd xx(x);
  return (wrap(generate_sub_coordinates(xx, n_subs)));
}


// MatrixXd compute_fij(MatrixXd x, double max_r = 0.1) {
//   int n = x.rows(), m = x.cols() / 3;
//
//   for (int i = 0; i < n; i++) {
//     for (int j = 0; j < m - 1; j++) {
//       MatrixXd c1 = (x.block(i, 3*j, n, 1) - x.block(i, 3*j, 1, 1)).pow(2) + (x.block(i, 3*j+1, n, 1) - x.block(i, 3*j+1, 1, 1)).pow(2);
//       MatrixXd c2 = (x.block(i, 3*(j+1), n, 1) - x.block(i, 3*(j+1), 1, 1)).pow(2) + (x.block(i, 3*(j+1)+1, n, 1) - x.block(i, 3*(j+1)+1, 1, 1)).pow(2);
//       c1 = (max_r*max_r - c1.array()) > 0 && (max_r*max_r - c2.array()) > 0;
//     }
//   }
// }

// NumericMatrix compute_force(NumericMatrix x, NumericMatrix subcoords, int nl, double q_alpha, double q_d) {
//
// }
