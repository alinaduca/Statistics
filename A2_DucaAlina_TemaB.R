ex1 = function(n, a, b, c, h) {
  V = (pi * a * b * h^2) / (2 * c)
  count = 0
  p_rad = a * sqrt(h/c)
  s_rad = b * sqrt(h/c)
  for(i in 1:n) {
    x = runif(1, -p_rad, p_rad)
    y = runif(1, -s_rad, s_rad)
    z = runif(1, 0, h)
    if(x^2/a^2 + y^2/b^2 <= z/c) {
      count = count+1;
    } 
  }
  valMC=(p_rad*2)*(s_rad*2)*h*count/n
  er_abs=abs(valMC-V)
  er_rel=er_abs/abs(V)
  cat("Valoare aproximata: ", valMC, ", valoarea exacta: ", V)
  cat("\nEroarea relativa: ", er_rel)
}
a = 4
b = 3
c = 4
h = 4

ex1(20000, a, b, c, h)
ex1(50000, a, b, c, h)
ex1(100000, a, b, c, h)

ex2 = function(n, a, b, c, d) {
  count = 0
  for(i in 1:n) {
    x = runif(1, a, b)
    y = runif(1, c, d)
    if(x >= 1 & y <= 2 & y <= x - 1 & y <= 7 - x & y >= 0) {
      count = count + 1
    }
  }
  return(abs((b-a)*(d-c))*count/n)
}
   
ex2(20000, 1, 6, 0, 2)

ex3a = function(n, a, b) {
  sum = 0
  for(i in 1:n) {
    x = runif(1, a, b)
    sum = sum + x / (x^2 + 2)^3
  }
  aria = ((b - a) * sum / n)
  cat("Aria aproximata intre", a,"si",b, ":", aria, "\n")
  cat("Valoarea exacta:", 1/48)
}

ex3a(10000, 1, 2)

ex3b = function(n, a, b) {
  sum = 0
  for(i in 1:n) {
    x = runif(1, a, b)
    sum = sum + 1 / (x^2 + 9)
  }
  aria = ((b - a) * sum / n)
  cat("Aria aproximata intre", a,"si",b, ":", aria, "\n")
  cat("Valoarea exacta:", pi/6)
}

ex3b(10000, -3, 3)

ex3c = function(n) {
  sum = 0
  for(i in 1:n) {
    u = rexp(1, 1)
    sum = sum + (u * exp(-u^2)) / (exp(-u))
  }
  aria = sum / n
  cat("Aria aproximata intre 0 si Inf:", aria, "\n")
  cat("Valoarea exacta intre 0 si Inf:", 1/2)
}

ex3c(10000)

ex4 = function(n) {
  S=0
  for(i in 1:n) {
    lambda=dexp(4)
    a=rgamma(1,shape=4, scale=3)
    b=rgamma(1,shape=4, scale=2)
    c=rgamma(1,shape=5, scale=2)
    d=rgamma(1,shape=5, scale=3)
    prob=runif(1,0,1)
    if(prob<=0.25)
      S=S+a+lambda
    else if(prob<=0.5)
      S=S+b+lambda
    else if(prob<=0.8)
      S=S+c+lambda
    else
      S=S+d+lambda
  }
  timp_mediu=S/n
  return(timp_mediu)
}
ex4(10000)

#Simulez vectorul initial din care unul este infectat

vector_init=function(n) {  #Infecteaza random un singur calculator din retea
  infectate = vector(mode = "logical", length = n)
  pos = sample(1:n, 1)
  infectate[pos] = TRUE
  return(infectate)
}

#functie care numara computerele infectate

comp_inf = function(v) {
  c = 0
  for(i in 1:length(v))
    if(v[i] == TRUE)
      c = c + 1
  return(c)
}

#functie de curatare pentru k calculatoare

curatare = function(v, k){
  curatat = vector()
  j = 1
  for(i in 1:length(v))
    if(v[i] == TRUE) {
      curatat[j] = i
      j = j + 1
    }
  if(length(curatat) > 0)
    h = k
  if(k > length(curatat))
    h = length(curatat)
  index = sample(length(curatat), h, replace = F)
  for(i in 1:h){
    v[curatat[index[i]]] = FALSE
  }
  return(v)
}

#infectam cu probabilitate PROB
infectam = function(v, prob){
  for(i in 1:length(v)){
    if(v[i] == T) {
      for(j in 1:length(v)) {
        if(v[j] == F) {
          x = runif(1, 0, 1)
          if(x <= prob) {
            v[j] = TRUE
          }
        }
      }
    }
  }
  return(v)
}

ex5a = function(nrsimulari, n, k, prob) {
  c = 0
  for(t in 1:nrsimulari) {
    f = vector(mode="logical", length = n)
    cnt = 0
    calculatoare=vector_init(n)
    for(i in 1:length(calculatoare)) {
      if(calculatoare[i] == T & f[i] == F) {
        cnt = cnt + 1
        f[i] = T
      }
    }
    while(cnt < n & comp_inf(calculatoare) != 0) {
      calculatoare = infectam(calculatoare, prob)
      for(i in 1:length(calculatoare)) {
        if(calculatoare[i] == T & f[i] == F) {
          cnt = cnt + 1
          f[i] = T
        }
      }
      #cat(f, "\n\n")
      calculatoare = curatare(calculatoare, k)
    }
    if(cnt == n)
      c = c + 1
    # cat("Contorul dupa a ", t, "-a rulare este: ", c, "\n\n")
  }
  return(c * 100 / nrsimulari)
}

cat("Probabilitatea este: ", ex5a(1000, 50, 8, 0.05), "\n")
cat("Probabilitatea este: ", ex5a(1000, 50, 8, 0.1), "\n")
cat("Probabilitatea este: ", ex5a(1000, 50, 8, 0.2), "\n")


ex5b = function(nrsimulari, n, nrzile, k, prob) {
  c = 0
  for(t in 1:nrsimulari) {
    days = 0
    calculatoare = vector_init(n)
    while(days < nrzile) {
      calculatoare = infectam(calculatoare, prob)
      calculatoare = curatare(calculatoare, k)
      days = days + 1
    }
    if(comp_inf(calculatoare) != 0)
      c = c + 1
    #cat("Contorul dupa a ", t, "-a rulare este: ", c, "\n\n")
  }
  return(c * 100 / nrsimulari)
}

cat("Probabilitatea este: ", ex5b(1000, 50, 8, 7, 0.05), "\n")
cat("Probabilitatea este: ", ex5b(1000, 50, 8, 7, 0.1), "\n")
cat("Probabilitatea este: ", ex5b(1000, 50, 8, 7, 0.2), "\n")

ex5c = function(err, prob, p) {
  alfa = 1 - prob
  z = qnorm(alfa / 2)
  Nmin = p * (1 - p) * (z / err) ^ 2;
  return(Nmin);
}

print(ex5c(0.01, 0.99, 0.2))
