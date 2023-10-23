ex1 = function(n, sigma, proc, sample_mean) {
  alfa = 1 - proc
  critical_z = qnorm(1 - alfa/2, 0, 1)
  a = sample_mean - critical_z*sigma/sqrt(n)
  b = sample_mean + critical_z*sigma/sqrt(n)
  interval = c(a, b)
  return(interval)
}

print(ex1(20, 30, 0.9, 300))
print(ex1(20, 30, 0.95, 300))

ex2 = function(m, alfa, n, lim) {
  count = 0
  for(i in 1:n) {
    x=sample(0:9, m, replace=T)
    sample_mean=mean(x)
    sigma = sd(x)
    critical_z = qnorm(1 - alfa/2, 0, 1)
    a = sample_mean - critical_z*sigma/sqrt(m)
    b = sample_mean + critical_z*sigma/sqrt(m)
    interval = c(a, b)
    cat("Intervalul de incredere este: ", interval, "\n")
    if(lim >= a & lim <= b)
      count = count + 1
  }
  return(count)
}

print(ex2(40, 1-0.95, 100, 4.5))
print(ex2(40, 1-0.99, 100, 4.5))

ex3_4 = function(n, succese, alfa, p0, tip) {
  p_prim = succese / n
  z_score = (p_prim - p0) / (sqrt(p0 * (1 - p0) / n))
  if(tip == 'left') {
    critical_z = qnorm(alfa, 0, 1)
    if(z_score < critical_z) {
      cat("Ipoteza nula respinsa, se accepta ipoteza alternativa. \n")
    }
    else{
      cat("Nu avem suficiente dovezi. \n")
    }
  }
  if(tip == 'right') {
    critical_z = qnorm(1 - alfa, 0, 1)
    if(z_score > critical_z) {
      cat("Ipoteza nula respinsa, se accepta ipoteza alternativa. \n")
    }
    else {
      cat("Nu avem suficiente dovezi. \n")
    }
  }
  if(tip == 'sim') {
    critical_z = qnorm(1 - alfa / 2)
    if(abs(z_score) > abs(critical_z)) {
      cat("Ipoteza nula respinsa, se accepta ipoteza alternativa. \n")
    }
    else{
      cat("Nu avem suficiente dovezi. \n")
    }
  }
  #cat("Scorul:", z_score,"\nValoarea critica: ", critical_z, "\n")
}

#D3
ex3_4(1250, 852, 0.01, 0.72, "left")
ex3_4(1250, 852, 0.05, 0.72, "left")

#D4
ex3_4(1020, 623, 0.01, 0.6, "right")
ex3_4(1020, 623, 0.05, 0.6, "right")


