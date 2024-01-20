# Load the data

data <- read.csv("D://Studia//Semestr_5//Inteligencja Obliczeniowa//Distances//Dane_TSP_29.csv")
# data <- read.csv("D://Studia//Semestr_5//Inteligencja Obliczeniowa//Distances//Dane_TSP_48.csv")
# data <- read.csv("D://Studia//Semestr_5//Inteligencja Obliczeniowa//Distances//Dane_TSP_76.csv")
# data <- read.csv("D://Studia//Semestr_5//Inteligencja Obliczeniowa//Distances//Dane_TSP_127.csv")

distances <- as.matrix(data)
distances <- distances[,-1]
colnames(distances) <- NULL

# Create table for the results

results_df <- data.frame(matrix(ncol = length(distances[1,]) + 4))
entry_length <- length(distances[1,]) + 4
colnames(results_df) <- c("start_a", "start_T", "swaps", "rand", 1:length(distances[1,]), "final_distance", "calc_time")

# Function random_swap to randomly swap two points on the way

random_swap <- function(vector) {
  if(length(vector) < 2) {
    warning("Vector must have at least 2 elements for swapping.")
    return(vector)
  }
  random_indices <- sample(length(vector), 2)
  vector[random_indices] <- vector[random_indices[2:1]]
  return(vector)
}

# Create a first default set of 1:n and randomly create a way

set <- c(1:length(distances[,1]))
way <- array(data = sample(set, replace = FALSE, prob = NULL), dim = length(distances[,1]))

# Function calculate_distance to calculate distance bewteen first and last point of way

calculate_distance <- function(selected_way){
  
  dist <- 0
  
  for (i in 1:(length(distances[,1])-1)){
    dist <- dist + distances[selected_way[i], selected_way[i+1]]
  }
  
  return(dist)
}

# Calculate first distance (starting point)

distance <- calculate_distance(way)

# Function simulated_annealing with stop criterion based on temperature

simulated_annealing <- function(a, T, s, p){
  
  start_a <- a
  start_t <- T
  
  # parameters
  R <- way

  #variables
  n <- 0
  
  lngths <- c()
  
  # start time measurement
  start_time <- Sys.time()
  
  while (T >= 0.01) {
    
    n <- n + 1
    k <- 0
    
    while (k <= 5){
      
      k <- k + 1
      T <- T*a
    
      new_way <- way
      
      for (swaps in 1:s) {
        new_way <- random_swap(new_way)
      }
      
      new_distance <- calculate_distance(new_way)
      
      #cat("\n\nTrying new way, distance: ", new_distance, " versus", distance)
      
      if (p == "unif") {
        random <- runif(1, 0, 1)
      } else if (p == "norm") {
        random <- ifelse(abs(rnorm(1, 0, 0.5)) >= 1, 1, abs(rnorm(1, 0, 0.5)))
      }

      delta <- (new_distance - distance)
      
      if (new_distance < distance){
        distance <- new_distance
        way <- new_way
        lngths <- c(lngths, distance)
        #cat("\nNew way accepted, delta:", delta)
        #cat("\nN:", n, "K:", k, "T:", T)
      }
      
      else if (random < exp(-delta/T)){
        distance <- new_distance
        way <- new_way
        lngths <- c(lngths, distance)
        #cat("\nNew way accepted conditionally:")
        #cat("\nN:", n, "K:", k, "T:", T, "R:", random, " D:", delta, "E:", format(exp(-delta/T), scientific = FALSE), "C:", ifelse(random < exp(-delta/T), 1, 0))
      }
      else{
        #cat("\nNew way rejected.")
      }
      cat("\r ", distance)
      
    }

  }
  
  if (T < 0.01){
    #cat("\n\nFinal way: ", way, "\nFinal distance: ", distance, "\n\n")
    
    end_time <- Sys.time()
    time <- (end_time - start_time)
    return(c(start_a, start_t, s, p, way, distance, time))
  }
}

# Values for testing

alphas <- c(0.999, 0.9999, 0.99999)
temps <- c(100, 1000, 10000)
swaps <- c(1, 2, 3)
prob <- c("unif", "norm")

iterations <- 0

for (j in 1:length(alphas)) {
  for (i in 1:length(temps)) {
    for (k in 1:length(swaps)) {
      for (l in 1:length(prob)) {
        for (a in 1:5) {
          
          iterations <- iterations + 1
          
          if (is.na(results_df[1, 1])) {
            temp <- simulated_annealing(alphas[j], temps[i], swaps[k], prob[l])
            write(temp, ncolumns = entry_length, file = "SA_LOG.txt", append = TRUE)
            results_df[1, ] <- temp
            cat("\n Completion rate: ", round(100*(iterations/(length(alphas)*length(temps)*length(swaps)*length(prob)*5)), 2), "%\n")
          } 
          else {
            temp <- simulated_annealing(alphas[j], temps[i], swaps[k], prob[l])
            write(temp, ncolumns = entry_length, file = "SA_LOG.txt", append = TRUE)
            results_df <- rbind(results_df, temp)
            cat("\n Completion rate: ", round(100*(iterations/(length(alphas)*length(temps)*length(swaps)*length(prob)*5)), 2), "%\n")
          }
          
        }
      }
    }
  }
}

# for (i in 1:20) {
  
  if (is.na(results_df[1, 1])) {
    results_df[1, ] <- simulated_annealing(alphas[j], temps[i])
  } 
  
  else {
    results_df <- rbind(results_df, simulated_annealing(alphas[j], temps[i]))
  }

}

# kryterium stopu
# liczba iteracji


# write.csv(results_df, "results_29_sumilated_annealing.csv")
write.csv(results_df, "results_29_sumilated_annealing2.csv")
# write.csv(results_df, "results_48_sumilated_annealing.csv")
# write.csv(results_df, "results_76_sumilated_annealing.csv")
# write.csv(results_df, "results_127_sumilated_annealing.csv")
