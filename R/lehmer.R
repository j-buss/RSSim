#' Create sequence of integers from Lehmer Random Number Generator
#'
#' @param x0 Initial seed value
#' @param a Multiplier
#' @param m Modulo
#' @param iterations Number of iterations
#'
#' @return vector of integers
#' @export
#'
#' @examples
#' lehmer.seq(x0=5, 2, 150)
#' lehmer.seq(x0=5, 2, 150, iterations=10)
lehmer.seq <- function(x0, a, m, iterations = 1) {
  counter <- 1
  temp_val <- x0
  output_vector <- c(temp_val)
  while(counter < iterations){
    temp_val <- (a * temp_val) %% m
    output_vector <- c(output_vector, temp_val)
    counter <- counter + 1
  }
  output_vector
}
#' Create uniform random numbers using Lehmer function
#'
#' @param x0 Initial seed value
#' @param a Multiplier
#' @param m Modulo
#' @param iterations Number of iterations
#'
#' @return vector of doubles
#' @export
#'
#' @examples
#' lehmer.rng(5, 2, 150, 25)
#' lehmer.rng(5, 2, 150)
lehmer.rng <- function(x0, a, m, iterations=1){lehmer.seq(x0, a, m, iterations) / m}
#' Create sequence of integers from Linear Congruential Generator
#'
#' @param x0 Initial seed value
#' @param a Multiplier
#' @param c Shift
#' @param m Modulo
#' @param iterations Number of iterations
#'
#' @return vector of integers
#' @export
#'
#' @examples
#' lcg.seq(6, 3, 9, 275)
#' lcg.seq(6, 0, 9, 900, 20)
lcg.seq <- function(x0, a, c, m, iterations = 1) {
  counter <- 1
  temp_val <- x0
  output_vector <- c(temp_val)
  while(counter < iterations){
    temp_val <- (a * temp_val + c) %% m
    output_vector <- c(output_vector, temp_val)
    counter <- counter + 1
  }
  output_vector
}
#' Create uniform random numbers using Linear Congruential Generator
#'
#' @param x0 Initial seed value
#' @param a Multiplier
#' @param c Shift
#' @param m Modulo
#' @param iterations Number of iterations
#'
#' @return vector of doubles
#' @export
#'
#' @examples
#' lcg.rng(6, 3, 9, 275)
#' lcg.rng(6, 3, 9, 275, 20)
lcg.rng <- function(x0, a, c, m, iterations=1){lcg.seq(x0, a, c, m, iterations) / m}
