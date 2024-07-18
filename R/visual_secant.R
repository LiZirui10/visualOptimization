#' Visualize Secant Method
#'
#' This function applies the Secant method to find x such that ftn(x) == 0,
#' and visualizes the process using ggplot2.
#'
#' @param ftn A function that takes a numeric input x and returns the function value f(x).
#' @param x0 Numeric. The initial x-value at iteration 0.
#' @param x1 Numeric. The initial x-value at iteration 1.
#' @param iter Integer. The number of iterations to perform. Default is 5.
#'
#' @return The final x value after the specified number of iterations.
#'
#' @details
#' The function performs the following steps:
#' 1. Applies the Secant method for the specified number of iterations.
#' 2. Tracks the progress of each iteration.
#' 3. Creates a ggplot visualization showing the function, the vertical segments to the x-axis,
#'    and the secant lines used in each iteration.
#'
#' @note
#' This function requires the ggplot2 package to be installed and loaded.
#'
#' @examples
#' # Define a function that returns f(x)
#' f <- function(x) {
#'   return(x^2 - 2)
#' }
#'
#' # Apply and visualize Secant method
#' visual_secant(f, x0 = 1, x1 = 2, iter = 10)
#'
#' @import ggplot2
#'
#' @export
visual_secant <- function(ftn, x0, x1, iter = 5) {
  df_points_1 <- data.frame(
    x1 = numeric(0),
    y1 = numeric(0),
    x2 = numeric(0),
    y2 = numeric(0)
  )
  df_points_2 <- df_points_1
  df_points_1[1,] <- c(x0, 0, x0, ftn(x0))
  df_points_1[2,] <- c(x1, 0, x1, ftn(x1))
  xnew0 <- x0; xnew1 <- x1
  cat("Starting values are:", xnew0, xnew1, "\n")
  for (i in 1:iter) {
    xold1 <- xnew1; xold0 <- xnew0
    f_xold1 <- ftn(xold1)
    f_xold0 <- ftn(xold0)
    xnew0 <- xold1
    xnew1 <- xold1 - f_xold1 * ((xold1 - xold0)/(f_xold1-f_xold0))
    if (is.nan(xnew1)) {xnew1 <- xnew0; break}
    line <- function(x, x1=xold1, x0=xold0)
    { ((ftn(x1)-ftn(x0))/(x1-x0)) * (x-x0) + ftn(x0) }
    cat("Next x value:", xnew1, "\n")
    df_points_1[i+2,] <- c(xnew1, 0, xnew1, ftn(xnew1))
    left <- min(xold0, xold1, xnew1)
    right <- max(xold0, xold1, xnew1)
    df_points_2[i,] <- c(left, line(left), right, line(right))
  }
  plot_start <- min(df_points_1$x1, df_points_1$x2, x0) - 0.1
  plot_end <- max(df_points_1$x1, df_points_1$x2, x0) + 0.1
  x <- seq(plot_start, plot_end, length.out = 200)
  fx <- rep(NA, length(x))
  for (i in seq_along(x)) { fx[i] <- ftn(x[i]) }
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
  xnew1
}
