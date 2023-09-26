z.conf.int.m = function(data, cl, sigma){
  x = mean(data)
  n = length(data)
  z = qnorm(0.5+0.5*a)
  ci_lower = x - (z * (sigma / sqrt(n)))
  ci_upper = x + (z * (sigma / sqrt(n)))
  ci = paste0("신뢰구간은 [", ci_lower,", ", ci_upper, "] 이다.")
  return(ci)
}

t.conf.int.m = function(data, cl){
  x = mean(data)
  n = length(data)
  t_critical = qt(0.5+(cl/2), df = n-1)
  ci_lower = x - (t_critical * (sd(data) / sqrt(n)))
  ci_upper = x + (t_critical * (sd(data) / sqrt(n)))
  ci = paste0("신뢰구간은 [", ci_lower,", ", ci_upper, "] 이다.")
  return(ci)
}

conf.int.p = function(data, target, cl){
  n = length(data)
  X = table(data)[target]
  s_p = X / n # 0.75
  z = qnorm(0.5 + 0.5 * cl) # 1.959964
  sqrt((s_p*(1 - s_p)/n))
  ci_lower = s_p - (z * sqrt((s_p*(1 - s_p)/n)))
  ci_upper = s_p + (z * sqrt((s_p*(1 - s_p)/n)))
  result = paste0("신뢰구간은 [", ci_lower,", ", ci_upper, "] 이다.")
  return(result)
}

z.test.m = function(data, alternative, m0, sigma){
  x = mean(data) # 샘플 평균
  n = length(data)
  z = (x - m0) / (sigma / sqrt(n))

  if (alternative == "two.sided") {
    p = 2 * (1 - pnorm(abs(z))) # 양측 검정
  } else if (alternative == "less") {
    p = pnorm(z) # 단측 검정 (좌측)
  } else if (alternative == "greater") {
    p = 1 - pnorm(z) # 단측 검정 (우측)
  }
  result = paste0('p-value(유의확률)은 ',p,'이다. ')
  return(result)
}

t.test.m = function(data, alternative, m0){
  p.value = t.test(data, alternative = alternative, mu = m0)$p.value
  result = paste0('p-value(유의확률)은 ',p.value,'이다. ')
  return(result)
}


test.p = function(data, target, alpha, alternative, p0){
  p.value = prop.test(table(data)[target], sum(table(data)), p = p0, conf.level = 1 - alpha, correct = FALSE)$p.value
  result = paste0('p-value(유의확률)은 ',p.value,'이다. ')
  return(result)
}

