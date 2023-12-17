lehmer.seq <- function(x0, a, m, iterations = 1) {
  counter <- 1
  temp_val <- x0
  output_vector <- c(temp_val)
  while(counter <= iterations){
    temp_val <- (a * temp_val) %% m
    output_vector <- c(output_vector, temp_val)
    counter <- counter + 1
  }
  output_vector
}
