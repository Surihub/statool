



conf_int_p = function(data, target, cl){
  n = length(data)
  X = table(data)[target]
  s_p = X / n
  z = qnorm(0.5 + 0.5 * cl)
  sqrt((s_p*(1 - s_p)/n))
  ci_lower = s_p - (z * sqrt((s_p*(1 - s_p)/n)))
  ci_upper = s_p + (z * sqrt((s_p*(1 - s_p)/n)))
  result = paste0("신뢰구간은 [", ci_lower,", ", ci_upper, "] 이다.")
  return(result)
}
