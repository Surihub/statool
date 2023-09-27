z_conf_int_m = function(data, cl, sigma){
  x = mean(data)
  n = length(data)
  z = qnorm(0.5+0.5*a)
  ci_lower = x - (z * (sigma / sqrt(n)))
  ci_upper = x + (z * (sigma / sqrt(n)))
  ci = paste0("신뢰구간은 [", ci_lower,", ", ci_upper, "] 이다.")
  return(ci)
}
