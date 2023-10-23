ex1=function(n,k,p,lambda) {
  n1=n+k
  x=k:n1
  y1=dpois(x, lambda)
  y2=dgeom(x,p)
  xb=0:n
  y3=dbinom(xb,n,p)
  barplot(y1, col = "lightgreen")
  barplot(y2, add=T, col = "magenta")
  barplot(y3, add=T, col = "cyan")
}

ex1(40,5,0.5,15)

ex2a=function(nume_fisier) {
  note=scan(nume_fisier)
  v = vector()
  v[1] = median(note)
  v[2] = mean(note)
  v[3] = sd(note)
  v[4] = as.vector(quantile(note))[1+1]
  v[5] = as.vector(quantile(note))[3+1]
  return(v)
}

ex2a(file.choose())

ex2b=function(nume_fisier) {
  note=scan(nume_fisier)
  M=mean(note)
  s=sd(note)
  cat("Media: ", M, " sd: ", s, "\n")
  note1=vector()
  j=0
  cat("Valorile aberante sunt: ")
  for(i in 1:length(note)) {
    if((note[i]>=(M-2*s))&(note[i]<=(M+2*s))) {
      j=j+1
      note1[j]=note[i]
    }
    else
      cat(note[i], " ") #afisez valorile aberante
  }
  cat("\n")
  return(note1) #returnez vectorul curatat
}

ex2b(file.choose())

ex2c=function(nume_fisier) {
  note=scan(nume_fisier)
  x = ex2b(nume_fisier) #vectorul curatat
  interval = seq(0, 70, 10)
  hist(x, breaks = interval, freq = T, right = T)
}

ex2c(file.choose())
