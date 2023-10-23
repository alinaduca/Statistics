#Ex 1
nivel_AND = function(leaves, h) {
  vec = vector()
  a = runif(1, 0, 1)
  b = runif(1, 0, 1)
  for(i in 1:3^(h)) {
    if(a <= 0.333) {
      if(leaves[3 * i - 2] == 1) {
        if(b <= 0.5) {
          if(leaves[3 * i - 1] == 1)
            vec[i] = leaves[3 * i]
          else
            vec[i] = 0
        }
        else {
          if(leaves[3 * i])
            vec[i] = leaves[3 * i - 1]
          else
            vec[i] = 0
        }
      }
      else {
        vec[i] = 0
      }
    }
    else {
      if(a < 0.667) {
        if(leaves[3 * i - 1] == 1) {
          if(b <= 0.5) {
            if(leaves[3 * i - 2] == 1)
              vec[i] = leaves[3 * i]
            else
              vec[i] = 0
          }
          else {
            if(leaves[3 * i])
              vec[i] = leaves[3 * i - 2]
            else
              vec[i] = 0
          }
        }
        else {
          vec[i] = 0
        }
      }
      else {
        if(leaves[3 * i]) {
          if(b <= 0.5) {
            if(leaves[3 * i - 2] == 1)
              vec[i] = leaves[3 * i - 1]
            else
              vec[i] = 0
          }
          else {
            if(leaves[3 * i - 1] == 1)
              vec[i] = leaves[3 * i - 2]
            else
              vec[i] = 0
          }
        }
        else {
          vec[i] = 0
        }
      }
    }
  }
  return(vec)
}


nivel_OR = function(leaves, h) {
  vec = vector()
  a = runif(1, 0, 1)
  b = runif(1, 0, 1)
  for(i in 1:3^(h)) {
    if(a <= 0.333) {
      if(leaves[3 * i - 2] == 0) {
        if(b <= 0.5) {
          if(leaves[3 * i - 1] == 0)
            vec[i] = leaves[3 * i]
          else
            vec[i] = 1
        }
        else {
          if(!leaves[3 * i])
            vec[i] = leaves[3 * i - 1]
          else
            vec[i] = 1
        }
      }
      else {
        vec[i] = 1
      }
    }
    else {
      if(a < 0.667) {
        if(leaves[3 * i - 1] == 0) {
          if(b <= 0.5) {
            if(leaves[3 * i - 2] == 0)
              vec[i] = leaves[3 * i]
            else
              vec[i] = 1
          }
          else {
            if(!leaves[3 * i])
              vec[i] = leaves[3 * i - 2]
            else
              vec[i] = 1
          }
        }
        else {
          vec[i] = 1
        }
      }
      else {
        if(leaves[3 * i] == 0) {
          if(b <= 0.5) {
            if(leaves[3 * i - 2] == 0)
              vec[i] = leaves[3 * i - 1]
            else
              vec[i] = 1
          }
          else {
            if(leaves[3 * i - 1] == 0)
              vec[i] = leaves[3 * i - 2]
            else
              vec[i] = 1
          }
        }
        else {
          vec[i] = 1
        }
      }
    }
  }
  return(vec)
}


ex1 = function(h) {
  leaves = sample(0:1, 3 ^ (2 * h), replace=T)
  var = 2 * h - 1
  while(var != 0) {
    #cat(leaves, "\n")
    if(var %% 2 == 0) { #nivel AND
      leaves = nivel_AND(leaves, var)
    }
    else { #nivel OR
      leaves = nivel_OR(leaves, var)
    }
    var = var - 1
  }
  #cat(leaves, "\n")
  return(leaves[1])
}

ex1(2)


#Ex 2
fVerif = function(W, M, lista1, n) {
  for(i in 1:n) {
    if(lista1[i] == M) {
      return(i)
    }
  }
  return(0)
}
bVerif = function(M, W, lista2, n) {
  for(i in 1:n) {
    if(lista2[i] == W) {
      return(i)
    }
  }
  return(0)
}
verif = function(f, n) {
  for(i in 1:n)
    if(f[i] == -1)
      return(1)
  return(0)
}
stabilitate = function(M, W, lista1, lista2, f, b, n) {
  for(M1 in 1:n) {
    if(M1 != M) {
      W2 = f[M1]
      W1 = f[M]
      cuplu11 = bVerif(M, W1, lista2, n)
      cuplu12 = bVerif(M, W2, lista2, n)
      cuplu21 = fVerif(W2, M, lista1, n)
      cuplu22 = fVerif(W2, M1, lista1, n)
      if(cuplu11 < cuplu12 & cuplu22 < cuplu21)
        return(0)
    }
  }
  return(1)
}
ex3 = function(n) {
  f = vector()
  b = vector()
  for(i in 1:n) {
    f[i] = -1
    b[i] = -1
  }
  M = 1
  lista1 = sample(n, n)
  lista2 = sample(n, n)
  while(verif(f, n)) {
    if(f[M] == -1) {
      W = sample(n, 1)
      if(b[W] != -1) {
        M2 = b[W]
        if(bVerif(W, M, lista2, n) > fVerif(W, M2, lista1, n)) {
          f[M2] = -1
          f[M] = W
          b[W] = M
        }
      }
      if(b[W] == -1) {
        f[M] = W
        b[W] = M
      }
    }
    M = M %% n + 1
  }
  for(i in 1:n) {
    if(stabilitate(i, f[i], lista1, lista2, f, b, n))
      cat("barbatul", i, "+ femeia", f[i], "= cuplu stabil.\n")
    else
      cat("barbatul", i, "+ femeia", f[i], "= cuplu instabil.\n")
  }
}
ex3(10)

#Ex 3
Prime = function(n) {
  if(n == 2)
    return(1)
  if(n %% 2 == 0)
    return(0)
  d = 3
  while(d * d <= n & n %% d != 0)
    d = d + 3
  if(d * d > n)
    return(1)
  return(0)
}
number = function(v) {
  sum = 0
  for(i in 1:length(v)) {
    sum = sum + (v[i] %% 10) * 2 ^ (i - 1)
    v = v / 10
  }
  return(sum)
}

ex3 = function(u, n, m) {
  p = sample(1:n^2, 1)
  r = vector()
  while(Prime(p) != 0) {
    p = sample(1:n^2, 1)
  }
  r_prim = number(u) %% p
  for(j in 1:m) {
    cuvant = sample(0:1, n, replace=T) #voi genera un cuvant din U 
    r[j] = number(cuvant) %% p
  }
  for(j in 1:m) {
    if(r_prim == r[j])
      return("u apartine lui U.")
  }
  return("u nu apartine lui U.")
}
u = sample(0:1, 8, replace=T)
print(ex3(u, 8, 100))

