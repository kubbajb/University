# LIBRARIES ====

library(tidyr)
library(seqinr)

# DATA ====

# data <- read.csv("D://Studia//Semestr_5//Inteligencja Obliczeniowa//Distances//Dane_TSP_29.csv")
# data <- read.csv("D://Studia//Semestr_5//Inteligencja Obliczeniowa//Distances//Dane_TSP_48.csv")
# data <- read.csv("D://Studia//Semestr_5//Inteligencja Obliczeniowa//Distances//Dane_TSP_76.csv")
data <- read.csv("D://Studia//Semestr_5//Inteligencja Obliczeniowa//Distances//Dane_TSP_127.csv")

distances <- as.matrix(data[,-1]) # Saving source data as "distances"
colnames(distances) <- NULL # Removing colnames
rownames(distances) <- NULL # Removing colnames
len_way <- length(distances[1,]) # Calculating the standard length of a way (count of points in "distances")
sort_column <- len_way + 1 # Establishing the waylemgth column index

# RESULT TABLE ====

results_df <- data.frame(matrix(ncol = len_way + 6)) #Creating vector for results
colnames(results_df) <- c("algorithm", "population_size", "combination_parameter", "mutation_parameter", 1:len_way, "final_distance", "calc_time")

# PRIMARY SET 1:n ====

base_set <- c(1:len_way) # Establishing base set of point indexes as 1:n (length of way)

# DISTANCE ====

calculate_distance <- function(selected_way){ # Calculating the total distance in given way
  dist <- 0
  for (i in 1:(length(distances[,1])-1)){
    dist <- dist + distances[selected_way[i], selected_way[i+1]] # For each point, add the distance of the way + the distance to the next point
  }
  return(dist)
}

# INITIAL WAY ====

get_way <- function(){
  return(array(data = sample(base_set, replace = FALSE, prob = NULL), dim = length(distances[,1]))) # Randomly generate a way
}

# INITIAL POPULATION ====

get_pop <- function(p){ # Gen an initial population by iterating the "initial way" function p times (number of population)
  pop <- array(data = NA, dim = c(p, len_way + 3))
  for (i in 1:p) { # This number (population) is a potential parameter
    pop[i,] <- c(get_way(), NA, NA, NA)
  }
  return(pop)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# COMBINATION ====

replace_na <- function(matrix1, matrix2) { # Two matrices are passed
  
  placeholder <- matrix1 # Placeholder is equal to matrix1 
  my_nas <- which(is.na(matrix1)) # All NAs indexes from matrix1 are saved for reference
  
  exist_vals <- matrix1[complete.cases(matrix1[])] # Non-NA values are saved in a vector called exist_vals
  miss_vals <- matrix2[!(matrix2[] %in% exist_vals)] # Missing values from matrix1 are calculated and saved in a vectotr called miss_vals
  
  for (i in 1:length(my_nas)) { # Loop iterates over the number of NAs
    placeholder[my_nas[i]] <- miss_vals[i] 
    # For each NA, we take its index and at this index in the placeholder variable, paste consecutive values from miss_vals (which are missing)
  }
  return(placeholder)
}

combination <- function(pair) { # Actual combination of two parents. Called from combination2()
  
  primary_parent <- pair[1,] # First item in input matrix is the first parent
  secondary_parent <- pair[2,] #second item in the matrix is the second parent
  
  child <- primary_parent # The child is in the beginning, by default, a copy of the primary parent
  
  crossover <- sample(1:len_way, 1) # A crossover point is selected by default

  for (i in 1:crossover) {
    child[i] <- NA # Genes from 1 up to the crossover point are assingned NA
  }
  
  child <- c(replace_na(child[1:len_way], secondary_parent[1:len_way]),0,0,0) 
  # Each NA is replaced with the first gene from the secondary parent, which does not alreay exist in child
  
  return(child) #Child is safely returned ❤️
  
}

combination2 <- function(set, p) { # Recombines the set p times to get new population
  
  #generation <- array(data = NA, dim = c(0, len_way + 3)) <- used to get completely new population without no.1 parent from previous generation
  generation <- set[1:2,] # used to get new population INCLUDING no.1 parent from previous generation
  
  for (i in 1:p) { # For 1:p (number in population)
    mating_set <- sample(1:p, 2, replace = TRUE, prob = set[1:p,len_way + 3]) # Sample two parents from 1:p with probabilities based on fitness calculated in benchmark()
    selection <- set[c(mating_set),] # Estalish parent set
    children <- combination(selection) # Combine parents to get one child
    generation <- as.matrix(rbind(generation, children)) # Add child to set of children (new generation)
  }
  return(generation) # Return new generation
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# MUTATION ====

gt_mut_prob <- function(not_prob) { # Get mutation probability
  scenarios <- c(TRUE, FALSE) # Two scenarios: mutation and NOT mutation
  my_prob <- not_prob # Placeholdre for inverse of probability
  result <- sample(scenarios, 1, replace = TRUE, prob = c(1 - my_prob, my_prob)) # The result is sampled from scenarios with probabilities equal to fitness score
  return(result) # Return TRUE or FALSE
}

similarity <- function(vector1, vector2) { # Calculate similarity of two vectors
  comparison_vector <- vector1 == vector2 # Get array of TRUE for each possible corresponding value and FALSE otherwise
  similarity_score <- mean(comparison_vector) # Get mean of TRUE (1) vs FALSE (0)
  return(similarity_score) # Return score
}

mutation <- function(set, m, p) { # Mutate offspring based on fitness
  
  mutation_rate <- similarity(set[1,1:len_way],set[p,1:len_way]) 
  # Mutation rate based on similarity of first and last individual in generation (by fitness)
  # High similarity increases the probability of mutation, while low lowers it (it is never 0)
  
  for (i in 1:length(set[,1])) { # For each individual in generation
    #if ((gt_mut_prob(set[i,(len_way + 2)]) || mutation_rate > 0.8) & (i > 1) ) {
    #if ((gt_mut_prob(set[i,(len_way + 2)]) || mutation_rate > 0.8 || sample(c(FALSE, TRUE), 1)) & (i > 1)) {
    if ((gt_mut_prob(set[i,(len_way + 2)]) || mutation_rate > 0.7) & (i > 1)) {
        
    # IF gt_mut_prob() returns TRUE (sampled but based on fitness) 
      # OR
    # IF overall mutation rate exceeds 0.8
      # OR
    # IF mutation occurs randomly (with 50% chance)
      # AND
    # We're not at index 1 (we never mutate the best individual)
      
      swaps <- round(m*i*mutation_rate, 0) - 1 
      # The number of mutations (swaps) is calculated by using the mutation parameter m,
      # multiplied by the index we're at (lower mutation rate for individuals at the top),
      # multiplied by the overall mutation rate, minus 1.
      
      for (j in 1:swaps) { # Loop over the number of swaps
        a <- sample(1:len_way, 1) # Select A swap point
        b <- sample(1:len_way, 1) # Select B swap point
        swap(set[i,a], set[i,b]) # Swap them 
      }
    }
  }
  return(set)
  
}

# - - - - - -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# BENCHMARK ====

benchmark <- function(set){ # Benchmark generation based on length of way
  
  for (i in 1:length(set[,1])) { # For each individual, calculate the distance and save it to column len_way + 1
    set[i,len_way + 1] <- calculate_distance(set[i,1:len_way]) 
  }
  set <- set[order(set[,sort_column],decreasing = FALSE),] # Order generation by fitness
  
  min <- min(set[,len_way + 1]) # Get shortest way in generation
  max <- max(set[,len_way + 1])# Get longest way in generation
  
  if (min == max) {
    set[,len_way + 2] <- 1
    set[,len_way + 3] <- 1/p
  }
  
  else {
    set[,len_way + 2] <- ifelse(is.na(-1*(scale(set[,len_way + 1], center = max, scale = max - min))), 0, -1*(scale(set[,len_way + 1], center = max, scale = max - min)))
    # Assign each individual a value by scalin the best individual to 1 and the worst to 0. 
    # Assign the rest a value based on the distance from both.
    
    set[,len_way + 3] <- ifelse(is.na(set[,len_way + 2] / sum(set[,len_way + 2])), 0, set[,len_way + 2] / sum(set[,len_way + 2]))
    # Scale previously calculated value so that the sum for all is 1 (essentially calculating a vector or probabilities).
  }
  
  return(set)
}

benchmark_w_cut <- function(set, p){ # Same as above, but this time the population is reduced to initial p parameter
  
  for (i in 1:length(set[,1])) {
    set[i,len_way + 1] <- calculate_distance(set[i,1:len_way])
  }
  
  set <- set[order(set[,sort_column],decreasing = FALSE),]
  
  min <- min(set[,len_way + 1])
  max <- max(set[,len_way + 1])
  
  if (min == max) {
    set[,len_way + 2] <- 1
    set[,len_way + 3] <- 1/p
  }
  
  else {
    set[,len_way + 2] <- ifelse(is.na(-1*(scale(set[,len_way + 1], center = max, scale = max - min))), 0, -1*(scale(set[,len_way + 1], center = max, scale = max - min)))
    # Assign each individual a value by scalin the best individual to 1 and the worst to 0. 
    # Assign the rest a value based on the distance from both.
    
    set[,len_way + 3] <- ifelse(is.na(set[,len_way + 2] / sum(set[,len_way + 2])), 0, set[,len_way + 2] / sum(set[,len_way + 2]))
    # Scale previously calculated value so that the sum for all is 1 (essentially calculating a vector or probabilities).
  }
  set <- set[1:p,]
  return(set)
}

# ACTUAL LOOP ====

find_shortest_path <- function(p, b, m) {
  
  # Parameter P - Population size
  # Parameter B - Defines how many iterations the internal while loop should run, if the result is unchanged
  # Parameter M - Directly multiplies the amount of mutations
  
  condition <- TRUE
  
  start_time <- Sys.time()
  
  way <- get_way()
  distance <- calculate_distance(way)
  pop <- get_pop(p)
  
  top <- c()
  loop <- 1
  half_pop <- p/2
  
  while (condition == TRUE) {
    
    pop <- benchmark(pop) # benchmark the population and compute path lengths
    #print(pop)
    pop <- combination2(pop, p) # combine members of population according to fit
    pop <- benchmark_w_cut(pop, p) # benchmark enlarged population and select top 20
    #print(pop)
    pop <- mutation(pop, m, p) # based on fitness
    pop <- benchmark(pop)
    #print(pop)

    top <- c(top, pop[1,len_way + 1])
    cat("\r", top[loop])
    loop <- loop + 1
    
    if (loop < (b + 1)) {
      next
      
    } else if (top[loop - 1] != top[loop - b]) {
      next
      
    } else {
      condition <- FALSE
      
    }
    
  }
  
  end_time <- Sys.time()
  time <- (end_time - start_time)
  cat('\n')
  return(unname(c("genetic algorithm v7", p, b, m, pop[1,1:len_way], pop[1,len_way + 1], time)))
}

find_shortest_path(70, 2500, (0.5))

for (i in 1:10) {
  if (is.na(results_df[1, 1])) {
    results_df[1, ] <- find_shortest_path(70, 2500, 0.4)
  } 
  else {
    results_df <- rbind(results_df, find_shortest_path(70, 2500, 0.4))
  }
}

write.csv(results_df, "results_127_genetic_algorithm.csv")


