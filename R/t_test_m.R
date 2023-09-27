
t_test_m = function(data, alternative, m0){
  p.value = t.test(data, alternative = alternative, mu = m0)$p.value
  result = paste0('p-value(유의확률)은 ',p.value,'이다. ')
  return(result)
}

