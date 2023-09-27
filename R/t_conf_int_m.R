

t_conf_int_m = function(data, cl){
  x = mean(data)
  n = length(data)
  t_critical = qt(0.5+(cl/2), df = n-1)
  ci_lower = x - (t_critical * (sd(data) / sqrt(n)))
  ci_upper = x + (t_critical * (sd(data) / sqrt(n)))
  ci = paste0("신뢰구간은 [", ci_lower,", ", ci_upper, "] 이다.")
  return(ci)
}
