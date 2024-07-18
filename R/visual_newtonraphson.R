#' Visualize Newton-Raphson Method
#'
#' This function applies the Newton-Raphson method to find x such that ftn(x)[1] == 0,
#' and visualizes the process using ggplot2.
#'
#' @param ftn A function that takes a numeric input x and returns a vector of two elements:
#'   the function value f(x) and its derivative f'(x).
#' @param x0 Numeric. The starting point for the Newton-Raphson method.
#' @param iter Integer. The number of iterations to perform. Default is 5.
#'
#' @return The final x value after the specified number of iterations.
#'
#' @details
#' The function performs the following steps:
#' 1. Applies the Newton-Raphson method for the specified number of iterations.
#' 2. Tracks the progress of each iteration.
#' 3. Creates a ggplot visualization showing the function, the vertical segments to the x-axis,
#'    and the tangent lines used in each iteration.
#'
#' @note
#' This function requires the ggplot2 package to be installed and loaded.
#'
#' @examples
#' # Define a function that returns f(x) and f'(x)
#‘ a <- function(x) {
#’   value <- cos(x) - x # f(x)
#‘   derivative <- -sin(x) - 1 # f'(x)
#’   c(value, derivative)
#‘ }
#' # Apply and visualize Newton-Raphson method
# visual_newtonraphson(a, 3, iter = 8)
#'
#' @import ggplot2
#'
#' @export
visual_newtonraphson <- function(ftn, x0, iter = 5) {

  df_points_1 <- data.frame(
    x1 = numeric(0),
    y1 = numeric(0),
    x2 = numeric(0),
    y2 = numeric(0)
  )
  df_points_2 <- df_points_1
  xnew <- x0
  cat("Starting value is:", xnew, "\n")
  for (i in 1:iter) {
    xold <- xnew
    f_xold <- ftn(xold)
    xnew <- xold - f_xold[1] / f_xold[2]
    cat("Next x value:", xnew, "\n")
    df_points_1[i,] <- c(xold, 0, xold, f_xold[1])
    df_points_2[i, ] <- c(xold, f_xold[1], xnew, 0)
  }
  plot_start <- min(df_points_1$x1, df_points_1$x2, x0) - 0.1
  plot_end <- max(df_points_1$x1, df_points_1$x2, x0) + 0.1
  x <- seq(plot_start, plot_end, length.out = 200)
  fx <- rep(NA, length(x))
  for (i in seq_along(x)) { fx[i] <- ftn(x[i])[1] }
  function_data <- data.frame(x, fx)
  p <- ggplot2::ggplot(function_data, ggplot2::aes(x = x, y = fx)) +
    ggplot2::geom_line(color = "royalblue", linewidth = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
                 data = df_points_1, color = "red", lty = 2) +
    ggplot2::geom_segment(ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
                 data = df_points_2, color = "black", lty = 1) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::theme_bw()

  print(p)
  xnew
}
