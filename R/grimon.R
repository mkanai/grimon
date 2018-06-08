#' grimon
#'
#' @param x an input \code{matrix}, \code{data.frame}, or \code{list}.
#'   It should be in \code{"wide"} or \code{"long"} format.
#'   The \code{"wide"} format is \code{n} x 2\code{m} matrix,
#'   where \code{n} represents a number of points (samples) and \code{m} represents a number of planes (layers).
#'
#'   The \code{"long"} format is basically \code{nm} x 2 matrix,
#'   but accepts different number of samples across planes (layers).
#' @param format format of an input \code{x}, \code{"wide"} or \code{"long"}.
#' @param segment_mat a 2-column matrix to specify segment (edge) connections by the indexes of connecting points.
#'   The index is calculated based on the \code{"long"} format. It is required particularly when \code{format} is \code{"long"}.
#'   If \code{NULL} and \code{format} is \code{"wide"}, all points in the same row are connected.
#' @param col colors of points.
#' @param label labels of planes (layers).
#' @param optimize_coordinates a logical value indicating whether to optimize points coordinates across layers.
#' @param maxiter a maximum number of iterations for optimization by simulated annealing.
#' @param initT a initial value of the temperature parameter \code{T} for simulated annealing.
#' @param alpha a numerical value of the cooling rate \code{alpha} for simulated annealing.
#'   If \code{NULL}, \code{alpha} will be set as \code{1 - 5 / maxiter}.
#' @param score_function an objective score function to minimize, \code{"angle"} or \code{"length"}.
#'   If \code{"angle"}, the sum of angles of segments (edges) from the flat ground (the horizontal line) will be minimized.
#'   If \code{"length"}, the sum of length of segments will be minimized.
#' @param progress a logical value indicating whether to show optimization progress.
#' @param norm a logical value indicating whether to normalize point scales.
#'   Point coordinates for x axis, \code{px}, will be normalized as: \code{(px - min(px)) / (max(px) - min(px)) * scale - shift}.
#'   Ditto for y axis.
#' @param norm_scale a numerical value of the scaling parameter for normalization.
#'   If \code{NULL}, the default value is 2.
#' @param norm_shift a numerical value of the shifting parameter for normalization.
#'   if \code{NULL}, the default value is 1.
#' @param z_interval a numerical value of the interval length between planes (layers).
#' @param point_size a point size.
#' @param plane_col colors of planes.
#' @param plane_alpha alpha transparency of planes.
#' @param border_col a color of borders surrounding a plane.
#' @param border_alpha alpha transparency of borders.
#' @param border_lwd line width of borders.
#' @param segment_col colors of segments (edges).
#'   If \code{NULL}, the same color for connecting points is used.
#' @param segment_alpha alpha transparency of segments (edges).
#' @param segment_lwd line width of segments (edges).
#' @param new_device a logical value indicating whether to open a new rgl device.
#' @param reset_view a logical value indicating whether to reset viewpoint perspective.
#' @param userMatrix \code{userMatrix} parameter to be passed to \code{rgl::par3d}.
#' @param windowRect \code{windowRect} parameter to be passed to \code{rgl::par3d}.
#' @param plot_2d_panels a logical value indicating whether to additionally plot two-dimensional panels of each layer.
#'
#' @examples
#' data("grimon.example")
#' grimon(x = example, col = example_col, label = 1:6,
#'        optimize_coordinates = TRUE, maxiter = 1e3,
#'        score_function = "angle",
#'        segment_alpha = 0.5)
#' @export
grimon = function(x, format = "wide",
                     segment_mat = NULL,
                     col = 'black', label = NULL,
                     optimize_coordinates = FALSE,
                     maxiter = 1000, initT = 1, alpha = NULL,
                     score_function = "angle",
                     progress = FALSE,
                     norm = TRUE, norm_scale = NULL, norm_shift = NULL,
                     z_interval = 1,
                     point_size = 2,
                     plane_col = 'black', plane_alpha = 0.1,
                     border_col = 'black', border_alpha = 1, border_lwd = 1,
                     segment_col = NULL, segment_alpha = 0.3, segment_lwd = 1,
                     new_device = FALSE, reset_view = TRUE,
                     userMatrix = rotationMatrix(pi/2, 0, 1, 0),
                     windowRect = c(0, 0, 800, 600),
                     plot_2d_panels = FALSE) {

  # argument check
  if (!is.list(x) && !is.matrix(x) && !is.data.frame(x)) {
    stop("x must be a list, matrix, or data.frame.")
  }
  if (!format %in% c("wide", "long")) {
    stop('format must be "wide" or "long".')
  }
  if (format == "long") {
    if (is.null(segment_mat)) {
      stop("long-format input must be with segment_mat.")
    }
    s = nrow(segment_mat)
    if (!length(segment_col) %in% c(1, s, 2*s)) {
      stop("long-format input requires the same length segment_mat and segment_col.")
    }
    if (!length(segment_alpha) %in% c(1, s, 2*s)) {
      stop("long-format input requires the same length segment_mat and segment_alpha.")
    }
    if (!length(segment_lwd) %in% c(1, s, 2*s)) {
      stop("long-format input requires the same length segment_mat and segment_lwd.")
    }
  }

  if (is.null(alpha)) {
    alpha = 1 - 5/maxiter
  }
  if (!score_function %in% c("angle", "length")) {
    stop('score_function must be "angle" or "length".')
  }
  if (is.null(norm_shift)) {
    if (is.null(norm_scale)) {
      norm_scale = 2
      norm_shift = 1
    } else {
      norm_shift = norm_scale / 2
    }
  }

  if (is.list(x) && !is.data.frame(x)) {
    if (!is.null(names(x)) && is.null(label)) {
      label = names(x)
    }
    xx = NULL
    for (i in 1:length(x)) {
      if (format == "wide") {
        xx = cbind(xx, as.matrix(x[[i]][, 1:2]))
      } else if (format == "long") {
        xx = rbind(xx, cbind(as.matrix(x[[i]][, 1:2]), rep(i*z_interval, nrow(x[[i]]))))
      }
    }
    x = xx
  }
  if (is.data.frame(x)) {
    if (format == "long" & is.factor(x[,3])) {
      x[,3] = as.numeric(x[,3])
    }
    x = as.matrix(x)
  }

  if (format == "wide") {
    x = as.matrix(x)
    n = nrow(x)
    m = ncol(x) / 2

    if (is.null(segment_mat)) {
      segment_mat = cbind(1:(n*(m-1)), (n+1):(n*m))
    }
    if (is.null(label)) {
      label = 1:m
    }
    if (length(col) < n) {
      col = rep(col, length = n)
    }
    if (is.null(segment_col)) {
      segment_col = col
    }
    if (length(col) == n) {
      col = rep(col, times = m)
    }
    mat = wide_to_long(x, z_interval)
    na_idx = which(is.na(mat[,1]))
    valid_segment_idx = which(!(segment_mat[,1] %in% na_idx | segment_mat[,2] %in% na_idx))
    segment_mat = segment_mat[valid_segment_idx,]
    z_idx = n * (0:m)

    if (length(segment_col) == n) {
      segment_col = rep(rep(segment_col, each = 2), times = m-1)
      segment_col = segment_col[rep(valid_segment_idx*2, each = 2) - (1:0)]
    }
    if (length(segment_alpha) == n) {
      segment_alpha = rep(rep(segment_alpha, each = 2), times = m-1)
      segment_alpha = segment_alpha[rep(valid_segment_idx*2, each = 2) - (1:0)]
    }
    if (length(segment_lwd) == n) {
      segment_lwd = rep(rep(segment_lwd, each = 2), times = m-1)
      segment_lwd = segment_lwd[rep(valid_segment_idx*2, each = 2) - (1:0)]
    }
  } else if (format == "long") {
    n = nrow(x)
    s = nrow(segment_mat)
    if (is.null(label)) {
      label = unique(x[,3])
    }
    if (is.factor(x[,3])) {
      x[,3] = as.numeric(x[,3])
    }
    x = as.matrix(x)

    if (length(segment_col) == s) {
      segment_col = rep(segment_col, each = 2)
    }
    if (length(segment_alpha) == s) {
      segment_alpha = rep(segment_alpha, each = 2)
    }
    if (length(segment_lwd) == s) {
      segment_lwd = rep(segment_lwd, each = 2)
    }

    mat_order = order(x[,3],x[,1],x[,2])
    mat = x[mat_order,]
    segment_mat_ = segment_mat
    segment_mat = apply(segment_mat, 2, function(x){match(x, mat_order)})

    z_idx = c(0, cumsum(table(mat[,3])))
    m = length(z_idx) - 1
  }

  if (norm) {
    if (length(norm_scale) == 1) {
      norm_scale = rep(norm_scale, m)
    }
    if (length(norm_shift) == 1) {
      norm_shift = rep(norm_shift, m)
    }

    if (any(is.na(mat))) {
      mat = .norm_matrix(mat, z_idx, norm_scale, norm_shift)
    } else {
      mat = norm_matrix(mat, z_idx, norm_scale, norm_shift)
    }
  }

  if (optimize_coordinates) {
    if (score_function == "angle") {
      score_function = 0
    } else if (score_function == "length") {
      score_function = 1
    }
    mat = optimize_coordinates(mat, z_idx, segment_mat,
                               maxiter, initT, alpha,
                               score_function,
                               norm, norm_scale, norm_shift,
                               progress = progress)
  }

  if (format == "long") {
    mat[mat_order,] = mat
    segment_mat = segment_mat_
  }

  # rgl
  requireNamespace("rgl")
  if (new_device) {
    open3d()
  } else {
    rgl.set(.check3d())
    clear3d()
  }
  if (new_device || reset_view) {
    par3d(userMatrix = userMatrix)
    par3d(windowRect = windowRect)
  }

  # planes
  z = mat[z_idx, 3]
  # b = c(rep(-1, 3), rep(1, 4), -1)
  if (norm) {
    b = c(rep(0, 3), rep(1, 4), 0)
    px = b * rep(norm_scale, each = 8) - rep(norm_shift, each = 8)
    py = rev(b) * rep(norm_scale, each = 8)  - rep(norm_shift, each = 8)
  } else {
    b = apply(matrix(rep(seq(m+1), each = 2)[2:(2*m+1)], nrow = 2), 2,
          function(i) {
            ii = seq(z_idx[i[1]], z_idx[i[2]])
            xmin = min(mat[ii, 1]); xmax = max(mat[ii, 1])
            ymin = min(mat[ii, 2]); ymax = max(mat[ii, 2])
            c(rep(xmin, 3), rep(xmax, 4), xmin, ymin, rep(ymax, 4), rep(ymin, 3))})
    px = as.vector(b[1:8,])
    py = as.vector(b[9:16,])
  }
  plane = cbind(px, py, rep(z, each = 8))
  quads3d(plane, col = plane_col, alpha = plane_alpha)
  segments3d(plane, col = border_col, alpha = border_alpha, lwd = border_lwd)

  points3d(mat, col = col, size = point_size)
  segments3d(mat[as.vector(t(segment_mat)),], col = segment_col, alpha = segment_alpha, lwd = segment_lwd)

  # axes
  axis3d('z', at = z, labels = label)

  if (plot_2d_panels) {
    oldpar = par(no.readonly = T)
    par(mfrow = c(1, 1), pty = 's')
    for (i in 1:m) {
      ii = z_idx[i] + 1
      n = z_idx[i+1]
      lim = c(0, 1) * norm_scale[i] - norm_shift[i]
      plot(mat[ii:n, 1:2],
           col = as.character(col), pch = 19,
           xlab = '', ylab = '',
           xlim = lim, ylim = lim,
           main = label[i])
    }
    par(oldpar)
  }
}


.norm_matrix = function(mat, z_idx, norm_scale, norm_shift) {
  m = length(z_idx) - 1
  for (i in 1:m) {
    ii = z_idx[i] + 1
    n = z_idx[i+1]
    for (j in 1:2) {
      xmin = min(mat[ii:n, j], na.rm = T)
      xmax = max(mat[ii:n, j], na.rm = T)
      mat[ii:n, j] = (mat[ii:n, j] - xmin) / (xmax - xmin) * norm_scale - norm_shift
    }
  }
  return(mat)
}
