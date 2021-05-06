## Functia de intensitate lambda
lambda_fun <- function(t) {
  if(t <= 2) {
    return(2)
  }
  return ((2*t)/log(t))
}

## Proces Poisson neomogen ce determina 
## ora aparitiei urmatorului client
generateTs <- function(s) {
  lambda = 9.66
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

## Simuleaza o variabila aleatoare de repartitie Poisson
customRPois <- function(lambda) {
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

## Simuleaza timpul necesar servirii unui
## client de catre server-ul 1
y1 <- function() {
  return (3*min(customRPois(7.3), customRPois(6.2))/60)
}

## Simuleaza timpul necesar servirii unui
## client de catre server-ul 2
y2 <- function() {
  return((floor(exp(customRNorm(1, 2))) + 2) / 60)
}

## Simuleaza o variabila aleatoare de repartitie normala
customRNorm <- function(sd, mean) {
  U.1 <- runif(1)
  U.2 <- runif(1)
  theta <- 2*pi*U.1
  E <- -log(U.2)
  R <- sqrt(2*E)
  X <- R*cos(theta)
  return(sd*X + mean)
}

## simulates one full day
simulateOneDay <- function() {
  MAX_NUMBER_OF_CLIENTS_IN_QUEUE = 15
  
  t = generateTs(0)
  eventsList <- c(t, Inf, Inf)
  Na = 0
  Nd = 0
  systemState <- c(0, 0)
  
  A1 = list()
  A2 = list()
  D = list()
  lost_clients_timestamps = list()
  lost_clients = 0
  
  
  while(t < 12) {
    if(eventsList[[1]] == min(eventsList[[1]], eventsList[[2]], eventsList[[3]])) {
      t = eventsList[[1]]
      next_client = generateTs(t)
      eventsList[[1]] = next_client
      if(next_client < 12) {
      if(systemState[[1]] < MAX_NUMBER_OF_CLIENTS_IN_QUEUE) {
        Na = Na + 1
        systemState[[1]] = systemState[[1]] + 1
      }
      else {
        lost_clients = lost_clients + 1
        lost_clients_timestamps[[lost_clients]] = next_client
      }
      
        if(systemState[[1]] == 1) {
          eventsList[[2]] = t + y1()
        }
        A1[[Na]] = t
        
        #output = paste("A1[", toString(Na), "]", "=", toString(A1[[Na]]), " ; t=", toString(t)) 
        #print(output)
      }
    }
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
      A2[[Na - systemState[[1]]]] = t
      
      #output = paste("A2[", Na - systemState[[1]], "]", "=", toString(A2[[Na - systemState[[1]]]]), " ; t=", toString(t)) 
      #print(output)
    }
    if(eventsList[[3]] < eventsList[[1]] & eventsList[[3]] < eventsList[[2]]) {
      t = eventsList[[3]]
      Nd = Nd + 1
      systemState[[2]] = systemState[[2]] - 1
      if(systemState[[2]] == 0) {
        eventsList[[3]] = Inf
      }
      if(systemState[[2]] >= 1) {
        eventsList[[3]] = t + y2()
      }
      D[[Nd]] = t
      
      #output = paste("D[", Nd, "]", "=", toString(D[[Nd]]), " ; t=", toString(t)) 
      #print(output)
    }
  }

  return(list(A1, A2, D, lost_clients_timestamps))
}

## Determina numarul mediu de clienti serviti pe zi si 
## numarul mediu de clienti pierduti pe zi din cauza cozii prea mari
avgClientsStats <- function(noOfDays) {
  avgServedClients = 0
  avgLostClients = 0
  for(i in 1:noOfDays) {
    day = simulateOneDay()
    avgServedClients = avgServedClients + length(day[[3]])
    avgLostClients = avgLostClients + length(day[[4]])
  }
  return(list(avgServedClients /noOfDays, avgLostClients / noOfDays))
}

## Returneaza timpul minim, timpul maxim si timpul mediu de asteptare
## pentru intregul sistem, pentru serverul 1 si pentru serverul 2
waitingTimes <- function(results) {
  A1 = results[[1]]
  A2 = results[[2]]
  D = results[[3]]
  
  systemClientDurations <- list() # cat timp petrece fiecare client in sistem
  systemAvgWaitingTime = 0 # timpul mediu petrecut in sistem
  
  serverOneDurations <- list()
  serverOneAvgWaitingTime = 0
  
  serverTwoDurations <- list()
  serverTwoAvgWaitingTime = 0
  
  for (i in 1:length(D)) {
    systemClientDurations[[i]] = D[[i]] - A1[[i]]
    serverOneDurations[[i]] = A2[[i]] - A1[[i]]
    serverTwoDurations[[i]] = D[[i]] - A2[[i]]
    
    systemAvgWaitingTime = systemAvgWaitingTime + systemClientDurations[[i]]
    serverTwoAvgWaitingTime = serverTwoAvgWaitingTime + serverTwoDurations[[i]]
    serverOneAvgWaitingTime = serverOneAvgWaitingTime + serverOneDurations[[i]]
  }
  
  systemAvgWaitingTime = systemAvgWaitingTime / length(D)
  systemMinWaitingTime = min(unlist(systemClientDurations))
  systemMaxWaitingTime = max(unlist(systemClientDurations))
  
  serverOneAvgWaitingTime = serverOneAvgWaitingTime / length(D)
  serverOneMinWaitingTime = min(unlist(serverOneDurations))
  serverOneMaxWaitingTime = max(unlist(serverOneDurations))
  
  serverTwoAvgWaitingTime = serverTwoAvgWaitingTime / length(D)
  serverTwoMinWaitingTime = min(unlist(serverTwoDurations))
  serverTwoMaxWaitingTime = max(unlist(serverTwoDurations))
  
  systemStats <- list(systemAvgWaitingTime, systemMinWaitingTime, systemMaxWaitingTime)
  serverOneStats <- list(serverOneAvgWaitingTime, serverOneMinWaitingTime, serverOneMaxWaitingTime)
  serverTwoStats <- list(serverTwoAvgWaitingTime, serverTwoMinWaitingTime, serverTwoMaxWaitingTime)
  
  return (list(systemStats, serverOneStats, serverTwoStats))
}

## Analizeaza clientii pierduti pe parcursul a mai multe zile
lostClientsStats <- function(noOfDays) {
  earliestFirstLoss = 13
  latestFirstLoss = 0
  avgFirstLoss = 0
  daysWithLosses = 0
  for(i in 1:noOfDays) {
    day = simulateOneDay()
    if(length(day[[4]]) < 1) {
      next
    }
    firstLoss = day[[4]][[1]]
    if(firstLoss < earliestFirstLoss) {
      earliestFirstLoss = firstLoss
    }
    if(firstLoss > latestFirstLoss) {
      latestFirstLoss = firstLoss
    }
    avgFirstLoss = avgFirstLoss + firstLoss
    daysWithLosses = daysWithLosses + 1
  }
  if(daysWithLosses > 0) {
    avgFirstLoss = avgFirstLoss / daysWithLosses
  }
  return(list(earliestFirstLoss, latestFirstLoss, avgFirstLoss))
}
print(lostClientsStats(365))

results = simulateOneDay()
print("Results")
print(results)
print("Lost")
lost = results[[4]]
print(lost)
print("Length")
print(length(lost))


#waitTimes = waitingTimes(results)
#print(waitTimes)

print(avgClientsStats(365))