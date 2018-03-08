#' grimon
#'
#' @param x x
#'
#' @examples
#' data("grimon.example")
#' grimon(x, col = col, label = 1:6,
#'        optimize_coordinates = TRUE, maxiter = 1e3,
#'        segment_alpha = 0.5)
#' @export
grimon = function(x, format = "wide",
                     segment_mat = NULL,
                     col = 'black', label = NULL,
                     optimize_coordinates = FALSE,
                     maxiter = 1000, initT = 1, alpha = NULL,
                     progress = FALSE,
                     norm = TRUE, norm_scale = NULL, norm_shift = NULL,
                     z_interval = 1,
                     point_size = 2,
                     plane_col = 'black', plane_alpha = 0.1,
                     border_col = 'black', border_alpha = 1, border_lwd = 1,
                     segment_col = NULL, segment_alpha = 0.3, segment_lwd = 1,
                     new_device = FALSE, reset_view = TRUE,
                     userMatrix = rotationMatrix(pi/2, 0, 1, 0)) {

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
        xx = cbind(xx, x[[i]][, 1:2])
      } else if (format == "long") {
        xx = rbind(xx, cbind(x[[i]][, 1:2], rep(i*z_interval, nrow(x[[i]]))))
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
      col = rep(col, rep = m)
    }
    if (length(segment_col) == n) {
      segment_col = rep(rep(segment_col, each = 2), rep = m-1)
    }
    if (length(segment_alpha) == n) {
      segment_alpha = rep(rep(segment_alpha, each = 2), rep = m-1)
    }
    if (length(segment_lwd) == n) {
      segment_lwd = rep(rep(segment_lwd, each = 2), rep = m-1)
    }

    mat = wide_to_long(x, z_interval)
    z_idx = n * (0:m)
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
    mat = norm_matrix(mat, z_idx, norm_scale, norm_shift)
  }

  if (optimize_coordinates) {
    mat = optimize_coordinates(mat, z_idx, segment_mat,
                               maxiter, initT, alpha,
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
    par3d(windowRect = c(0, 0, 800, 600))
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
}
