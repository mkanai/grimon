#include <math.h>
#include <limits>
#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;
using Eigen::Map;
using Eigen::MatrixXd;

// [[Rcpp::depends("RcppEigen")]]

double get_length(double x1, double y1, double x2, double y2) {
  return pow(x1-x2, 2) + pow(y1-y2, 2);
}

double get_angle(double x1, double y1, double x2, double y2) {
  return atan2(sqrt(get_length(x1, y1, x2, y2)), 1);
}

double angle_score(MatrixXd x, IntegerMatrix segment_mat) {
  int s = segment_mat.rows();
  double score = 0;
  for (int i = 0; i < s; i++) {
    int ii = segment_mat(i, 0) - 1;
    int jj = segment_mat(i, 1) - 1;
    score += pow(get_angle(x(ii, 0), x(ii, 1), x(jj, 0), x(jj, 1)), 2);
  }
  return score;
}

double length_score(MatrixXd x, IntegerMatrix segment_mat) {
  int s = segment_mat.rows();
  double score = 0;
  for (int i = 0; i < s; i++) {
    int ii = segment_mat(i, 0) - 1;
    int jj = segment_mat(i, 1) - 1;
    score += get_length(x(ii, 0), x(ii, 1), x(jj, 0), x(jj, 1));
  }
  return score;
}

MatrixXd norm_matrix(MatrixXd x, double scale = 2, double shift = 1) {
  int n = x.rows(), m = x.cols();

  for (int j = 0; j < m; j++) {
    double xmin = x.block(0, j, n, 1).minCoeff();
    double xmax = x.block(0, j, n, 1).maxCoeff();
    x.block(0, j, n, 1) = (x.block(0, j, n, 1).array() - xmin) / (xmax - xmin);
    x.block(0, j, n, 1) = x.block(0, j, n, 1).array() * scale - shift;
  }
  return x;
}

//' @export
// [[Rcpp::export("norm_matrix")]]
NumericMatrix norm_matrix(Eigen::Map<Eigen::MatrixXd> x, IntegerVector z_idx,
                          NumericVector scale, NumericVector shift) {
  int m = z_idx.length() - 1;
  MatrixXd xx(x);
  for (int i = 0; i < m; i++) {
    int ii = z_idx[i];
    int n = z_idx[i+1] - ii;
    xx.block(ii, 0, n, 2) = norm_matrix(xx.block(ii, 0, n, 2), scale[i], shift[i]);
  }
  return (wrap(xx));
}

MatrixXd rotate_matrix(const MatrixXd x, double theta) {
  MatrixXd rotate2D(2, 2);
  rotate2D <<  cos(theta), sin(theta),
              -sin(theta), cos(theta);
  MatrixXd ret = x * rotate2D;
  return ret;
}

MatrixXd flip_matrix_z(MatrixXd x, int z, int to_flip) {
  int n = x.rows();
  MatrixXd ret = x;
  ret.block(0, 3*(z-1)+to_flip, n, 1) = -x.block(0, 3*(z-1)+to_flip, n, 1);
  return ret;
}

//' @export
// [[Rcpp::export("optimize_coordinates")]]
NumericMatrix optimize_coordinates(const Eigen::Map<Eigen::MatrixXd> x,
                                   IntegerVector z_idx, IntegerMatrix segment_mat,
                                   int maxiter, double T, double alpha,
                                   int score_function,
                                   bool to_norm,
                                   NumericVector norm_scale, NumericVector norm_shift,
                                   bool to_rotate = true,
                                   bool to_flip = true, bool to_shift = true,
                                   bool progress = false) {
  int n = x.rows(), m = z_idx.length() - 1;

  // score function
  double (*f_score)(MatrixXd, IntegerMatrix);
  switch (score_function) {
  case 0: // edge angle
    f_score = angle_score;
    break;
  case 1: // edge length
    f_score = length_score;
    break;
  }

  MatrixXd xx(x);
  MatrixXd ret = xx;
  MatrixXd cur_xx = xx;
  double score = angle_score(xx, segment_mat), best_score = DBL_MAX;
  NumericVector theta(m);
  IntegerVector flip(m);
  NumericVector shift(2*m);
  for (int iter = 0; iter < maxiter; iter++) {
    MatrixXd new_xx = cur_xx;
    int rz = (int)(R::runif(0, m));
    int zstart = z_idx[rz];
    int nn = z_idx[rz+1] - zstart;

    double new_theta = theta[rz];
    if (to_rotate) {
      new_theta = R::runif(-0.5, 0.5) * M_PI;
      new_xx.block(zstart, 0, nn, 2) = rotate_matrix(cur_xx.block(zstart, 0, nn, 2), new_theta);
    }
    if (false && to_flip) {
      int to_flip = (int)R::runif(0, 2);
      new_xx = flip_matrix_z(cur_xx, rz, to_flip);
    }

    if (to_norm) {
      new_xx.block(zstart, 0, nn, 2) = norm_matrix(new_xx.block(zstart, 0, nn, 2), norm_scale[rz], norm_shift[rz]);
    }

    double new_score = f_score(new_xx, segment_mat);

    double p = exp(-fabs(new_score - score)/(T * 100));

    if (new_score < score || R::runif(0, 1) < p) {
      score = new_score;
      theta[rz] += new_theta;
      cur_xx = new_xx;
      if (new_score < best_score) {
        best_score = new_score;
        ret = cur_xx;
      }
    }
    T *= alpha;
    if (progress && iter % (int)(maxiter/20) == 0) {
       Rprintf("%d\t T:%.4f, score:%.4f, best:%.4f, p: %.4f, rz: %d, new_theta: %.4f\n", iter, T, score, best_score, p, rz, new_theta);
    }
  }

  return (wrap(ret));
}
