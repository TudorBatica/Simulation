
lambda_fun <- function(t) {
  if(t < 0 | t > 12) {
    return(-1);
  }
  if(t <= 2) {
    return(2)
  }
  return ((2*t)/log(t))
}


generateTs <- function(s) {
  lambda = 10
  t = s
  while (1) {
    u1 = runif(1)
    u2 = runif(1)
    t = t - log2(u1)/lambda
    if(u2 <= lambda_fun(t) / lambda) {
      return(t)
    }
  }
}

y1 <- function() {
  x1 = rpois(1, 7.3)
  x2 = rpois(1, 6.2)
  return (3 * min(x1, x2))
}

poissonRandomVar <- function(lambda) {
  p = 0
  mysum = 0
  while(1) {
    unif = runif(1)
    mysum = mysum + log(1 - unif)
    if(mysum * -1 >= lambda) {
      return(p)
    }
    p = p + 1
  }
}

y1 <- function() {
  return (3*min(poissonRandomVar(7.3), poissonRandomVar(6.2))/60)
}

y2 <- function() {
  return((floor(exp(rnorm(1, 2, 1))) + 2) / 60) 
}

t = generateTs(0)
eventsList <- c(t, Inf, Inf)
Na = 0
Nd = 0
systemState <- c(0, 0)



while(t < 12) {
  if(eventsList[[1]] == min(eventsList[[1]], eventsList[[2]], eventsList[[3]])) {
    t = eventsList[[1]]
    Na = Na + 1
    systemState[[1]] = systemState[[1]] + 1
    eventsList[[1]] = generateTs(t)
    if(systemState[[1]] == 1) {
      eventsList[[2]] = t + y1()
      print("in if")
      print(eventsList)
    }
    output = paste("Intra in sistem: ", toString(t))
    print(output)
  }
  print("in while")
  print(eventsList)
  if(eventsList[[2]] < eventsList[[1]] & eventsList[[2]] <= eventsList[[3]]) {
    t = eventsList[[2]]
    systemState[[1]] = systemState[[1]] - 1
    systemState[[2]] = systemState[[2]] + 1
    if(systemState[[1]] == 0) {
      eventsList[[2]] = Inf
    } 
    else {
      eventsList[[2]] = t + y1()
    }
    if(systemState[[2]] == 1) {
      eventsList[[3]] = t + y2()
    }
    output = paste("Cazu 2: ", toString(t))
    print(output)
  }
  if(eventsList[[3]] < eventsList[[1]] & eventsList[[3]] < eventsList[[2]]) {
    t = eventsList[[3]]
    Nd = Nd + 1
    systemState[[2]] = systemState[[2]] - 1
    if(systemState[[2]] == 0) {
      eventsList[[3]] = Inf
    }
    if(systemState[[2]] == 1) {
      eventsList[[3]] = t + y2()
    }
    output = paste("Cazu 3: ", toString(t))
    print(output)
  }
}




