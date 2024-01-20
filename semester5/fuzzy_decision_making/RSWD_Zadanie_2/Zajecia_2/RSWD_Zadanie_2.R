# Resources and Libraries

library(reshape)
library(dplyr)

# Data Loading and Reshaping

raw_data <- read.csv("topsis_data.csv")
data <- melt(raw_data, na.rm = FALSE, value.name = "Variable", id = colnames(raw_data[0,1:2]))
colnames(data) <- c("DecisionMaker", "Candidate", "Criterion", "Value")

# Criteria Types

crit_type <- c("Beneficial", "Beneficial", "Cost")

# Value Interpretation Table and Translator

values <- c(1,1,3,5,7,1,3,5,7,9,3,5,7,9,9)
ValueInterpretation <- array(values,dim = c(5, 3))
rownames(ValueInterpretation) <- c("Very Low","Low","Average","High","Very High")
colnames(ValueInterpretation) <- c("min", "mid", "max")

fuzzy_dict <- function(key){
  x <- c(ValueInterpretation[key, ])
  return(unname(x))
}

# Weigths

raw_weights <- c("High", "Very High", "Average")
weights <- array(NA, c(length(raw_weights),3))

for (i in 1:length(raw_weights)){
  weights[i,] <- fuzzy_dict(raw_weights[i])
}

# Variable Level Counts

decisionmakers <- c(unique(data[,1]))
candidates <- c(unique(data[,2]))
criterions <- c(unique(data[,3]))

# Create 4D Matrix

fuzzy4Dmatrix <- array(NA,c(length(candidates), length(decisionmakers), length(criterions), 3))

# Level Maping:
#  1st dimension: Candidate
#  2nd dimension: Decision Maker
#  3rd dimension: Criterion
#  4th dimension: Fuzzy Number Level (always 3 levels)

for (i in 1:length(candidates)){
  for (j in 1:length(decisionmakers)){
    for (k in 1:length(criterions)){
      val <- filter(data, DecisionMaker == decisionmakers[j], Candidate == candidates[i], Criterion == criterions[k])[,4]
      fuzzy4Dmatrix[i,j,k,] <- fuzzy_dict(val)
    }
  }
}

# Reduction of Decision Makers by c(min(), mean(), max())
# tutaj sprawdzić czemu mean() zwraca błąd a sum()/length() ni zwraca błędu!

fuzzy3Dmatrix <- array(NA,c(length(candidates), length(criterions), 3))


# Level Maping:
#  1st dimension: Candidate
#  2nd dimension: Criterion
#  3rd dimension: Fuzzy Number Level (always 3 levels)

for (i in 1:length(candidates)){
  for (j in 1:length(criterions)){
    
    lower <- min(fuzzy4Dmatrix[i,,j,1])
    mid <- sum(fuzzy4Dmatrix[i,,j,2])/length(fuzzy4Dmatrix[i,,j,2])
    upper <- max(fuzzy4Dmatrix[i,,j,3])
      
    fuzzy3Dmatrix[i,j,] <- c(lower, mid, upper)

  }
}

fuzzy3Dmatrix

# Normalization

for (i in 1:length(candidates)){
  for (j in 1:length(criterions)){
    
    if (crit_type[j] == "Beneficial"){
      fuzzy3Dmatrix[i,j,] <- fuzzy3Dmatrix[i,j,] / max(weights[j,])
    }
    else if (crit_type[j] == "Cost"){
      fuzzy3Dmatrix[i,j,] <- min(weights[j,]) / fuzzy3Dmatrix[i,j,]
    }
    else stop("Wrong Criteria Type!")
    
  }
}

fuzzy3Dmatrix

# Weigting

for (i in 1:length(candidates)){
  for (j in 1:length(criterions)){
    fuzzy3Dmatrix[i,j,] <- fuzzy3Dmatrix[i,j,] * weights[j,]
  }
}

fuzzy3Dmatrix


# Ideal solutions

ideal_solutions <- array(NA, c(2, length(raw_weights),length(criterions)))

for (j in 1:length(criterions)){
  
  #FPIS
  
  temp_max <- fuzzy3Dmatrix[,j,]
  maxcrit <- 0
  
  for (k in 3:1){
    if (length(which(fuzzy3Dmatrix[,j,k] == max(fuzzy3Dmatrix[,j,k]), arr.ind = TRUE)) > 1){
      if (k == 1) {
        maxcrit <- first(which(fuzzy3Dmatrix[,j,k] == max(fuzzy3Dmatrix[,j,k]), arr.ind = TRUE))
      }
      else next
    }
    else{
      maxcrit <- which(fuzzy3Dmatrix[,j,k] == max(fuzzy3Dmatrix[,j,k]), arr.ind = TRUE)
      break
    }
  }
  
  ideal_solutions[1,j,] <- fuzzy3Dmatrix[maxcrit,j,]
  
  #FNIS
  
  temp_min <- fuzzy3Dmatrix[,j,]
  mincrit <- 0
  
  for (k in 1:3){
    if (length(which(fuzzy3Dmatrix[,j,k] == min(fuzzy3Dmatrix[,j,k]), arr.ind = TRUE)) > 1){
      if (k == 1) {
        mincrit <- first(which(fuzzy3Dmatrix[,j,k] == min(fuzzy3Dmatrix[,j,k]), arr.ind = TRUE))
      }
      else next
    }
    else{
      mincrit <- which(fuzzy3Dmatrix[,j,k] == min(fuzzy3Dmatrix[,j,k]), arr.ind = TRUE)
      break
    }
  }
  
  ideal_solutions[2,j,] <- fuzzy3Dmatrix[mincrit,j,]
  
}

# Ideal_solutions

# FPIS

pos_dist_matrix <- array(NA,c(length(candidates), length(criterions)))

for (i in 1:length(candidates)){
  for (j in 1:length(criterions)){
    
    pos_dist_matrix[i,j] <- sqrt(1/3*((fuzzy3Dmatrix[i,j,1]-ideal_solutions[1,j,1])**2+
                                      (fuzzy3Dmatrix[i,j,2]-ideal_solutions[1,j,2])**2+
                                      (fuzzy3Dmatrix[i,j,3]-ideal_solutions[1,j,3])**2))
  }
}
pos_dist_matrix

# FNIS

neg_dist_matrix <- array(NA,c(length(candidates), length(criterions)))

for (i in 1:length(candidates)){
  for (j in 1:length(criterions)){
    
    neg_dist_matrix[i,j] <- sqrt(1/3*((fuzzy3Dmatrix[i,j,1]-ideal_solutions[2,j,1])**2+
                                      (fuzzy3Dmatrix[i,j,2]-ideal_solutions[2,j,2])**2+
                                      (fuzzy3Dmatrix[i,j,3]-ideal_solutions[2,j,3])**2))
  }
}

neg_dist_matrix

# Candidate Distances

cand_dist <- array(NA, c(length(candidates), 4))

for (i in 1:length(candidates)){
  
  cand_dist[i, 1] <- sum(pos_dist_matrix[i,])
  cand_dist[i, 2] <- sum(neg_dist_matrix[i,])
  cand_dist[i, 3] <- sum(pos_dist_matrix[i,])/(sum(pos_dist_matrix[i,])+sum(neg_dist_matrix[i,]))
  
} 

rownames(cand_dist) <- candidates
colnames(cand_dist) <- c("FPIS", "FNIS", "CC", "Rank")

cand_dist[,4] <- rank(cand_dist[,3])

cand_dist
