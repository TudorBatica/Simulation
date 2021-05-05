
lambda_fun <- function(t) {
  if(t <= 2) {
    return(2)
  }
  return ((2*t)/log(t))
}


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
  return((floor(exp(customrnorm())) + 2) / 60)
}

customrnorm <- function() {
  U.1 <- runif(1)
  U.2 <- runif(1)
  theta <- 2*pi*U.1
  E <- -log(U.2)
  R <- sqrt(2*E)
  X <- R*cos(theta)
  return(X + 2)
}

simulateOneDay <- function() {
  MAX_NUMBER_OF_CLIENTS_IN_QUEUE = 14
  
  t = generateTs(0)
  eventsList <- c(t, Inf, Inf)
  Na = 0
  Nd = 0
  systemState <- c(0, 0)
  
  A1 = list()
  A2 = list()
  D = list()
  left_timestamps = list()
  left_clients = 0
  
  
  while(t < 12) {
    if(eventsList[[1]] == min(eventsList[[1]], eventsList[[2]], eventsList[[3]])) {
      t = eventsList[[1]]
      next_client = generateTs(t)
      eventsList[[1]] = next_client
      
      if(systemState[[1]] < MAX_NUMBER_OF_CLIENTS_IN_QUEUE) {
        Na = Na + 1
        systemState[[1]] = systemState[[1]] + 1
      }
      else {
        left_clients = left_clients + 1
        left_timestamps[[left_clients]] = next_client
      }
      
      if(next_client < 12) {
        if(systemState[[1]] == 1) {
          eventsList[[2]] = t + y1()
        }
        A1[[Na]] = t
        
        output = paste("A1[", toString(Na), "]", "=", toString(A1[[Na]]), " ; t=", toString(t)) 
        print(output)
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
      
      output = paste("A2[", Na - systemState[[1]], "]", "=", toString(A2[[Na - systemState[[1]]]]), " ; t=", toString(t)) 
      print(output)
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
      
      output = paste("D[", Nd, "]", "=", toString(D[[Nd]]), " ; t=", toString(t)) 
      print(output)
    }
  }
  print(" ------------- left!!!!")
  print(left_timestamps)
  print("printed left timestamps -------------")
  return(list(A1, A2, D))
}

simulateOneDay()

results = simulateOneDay()
A1 = results[[1]]
A2 = results[[2]]
D = results[[3]]

avgClientsPerDay <- function(noOfDays) {
  avgClients = 0
  for(i in 1:noOfDays) {
    day = simulateOneDay()
    avgClients = avgClients + length(day[[3]])
  }
  return(avgClients /noOfDays)
}

system_client_durations <- list() # cat timp petrece fiecare client in sistem
system_avg_waiting_time = 0 # timpul mediu petrecut in sistem

server_one_durations <- list()
server_one_avg_waiting_time = 0

server_two_durations <- list()
server_two_avg_waiting_time = 0

for (i in 1:length(D)) {
  print('##########')
  system_client_durations[[i]] = D[[i]] - A1[[i]]
  print(system_client_durations[[i]])
  server_one_durations[[i]] = A2[[i]] - A1[[i]]
  print(server_one_durations[[i]])
  server_two_durations[[i]] = D[[i]] - A2[[i]]
  print(server_two_durations[[i]])
  
  system_avg_waiting_time = system_avg_waiting_time + system_client_durations[[i]]
  server_two_avg_waiting_time = server_two_avg_waiting_time + server_two_durations[[i]]
  server_one_avg_waiting_time = server_one_avg_waiting_time + server_one_durations[[i]]
  
  print('---------')
}

system_avg_waiting_time = system_avg_waiting_time / length(D)
system_min_waiting_time = min(unlist(system_client_durations))
system_max_waiting_time = max(unlist(system_client_durations))

server_one_avg_waiting_time = server_one_avg_waiting_time / length(D)
server_one_min_waiting_time = min(unlist(server_one_durations))
server_one_max_waiting_time = max(unlist(server_one_durations))

server_two_avg_waiting_time = server_two_avg_waiting_time / length(D)
server_two_min_waiting_time = min(unlist(server_two_durations))
server_two_max_waiting_time = max(unlist(server_two_durations))


print("System:")
print(system_client_durations)
print(system_avg_waiting_time)
print(system_min_waiting_time)
print(system_max_waiting_time)

print("Server One:")
print(server_one_durations)
print(server_one_avg_waiting_time)
print(server_one_min_waiting_time)
print(server_one_max_waiting_time)

print("Server Two:")
print(server_two_durations)
print(server_two_avg_waiting_time)
print(server_two_min_waiting_time)
print(server_two_max_waiting_time)

print("Average clients/day:")
print(avgClientsPerDay(100))




