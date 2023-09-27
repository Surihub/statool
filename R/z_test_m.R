z_test_m = function(data, alternative, m0, sigma){
  x = mean(data)
  n = length(data)
  z = (x - m0) / (sigma / sqrt(n))

  if (alternative == "two.sided") {
    p = 2 * (1 - pnorm(abs(z)))
  } else if (alternative == "less") {
    p = pnorm(z)
  } else if (alternative == "greater") {
    p = 1 - pnorm(z)
  }
  result = paste0('p-value(유의확률)은 ',p,'이다. ')
  return(result)
}
