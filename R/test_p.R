test_p = function(data, target, alpha, alternative, p0){
  p.value = prop.test(table(data)[target], sum(table(data)), p = p0, conf.level = 1 - alpha, correct = FALSE)$p.value
  result = paste0('p-value(유의확률)은 ',p.value,'이다. ')
  return(result)
}
