install.packages("FuzzyAHP")
library(FuzzyAHP)

# Resources:
# https://cran.r-project.org/web/packages/FuzzyAHP/FuzzyAHP.pdf
# https://hemi1984.medium.com/a-fuzzy-analytical-hierarchy-process-fahp-using-r-64c4c5a13b69
# https://cran.r-project.org/web/packages/FuzzyAHP/vignettes/examples.html

random_index_dict <- c(0.00, 0.00, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49)

# - - - - - - - - - - - - - - - - - - - - - - - - - 
# 0. Data

  n = 4
  
  fuzzy_weight_matrix <- array(NA,dim = c(n,n))
  fuzzy_weight_matrix[1,] = c(1,5,4,7)
  fuzzy_weight_matrix[2,] = c(1/5,1,1/2,3)
  fuzzy_weight_matrix[3,] = c(1/4,2,1,4)
  fuzzy_weight_matrix[4,] = c(1/7, 1/3, 1/4, 1)

# - - - - - - - - - - - - - - - - - - - - - - - - - 
# 1. Manual attempt
  
  # Defining Function to Calculate Consistency Index
  # Calculating the CI
  
  calculate <- function(){
    
    weight_sums = array(NA,dim = n)
    
    for (i in 1:n){
      weight_sums[i] = sum(fuzzy_weight_matrix[,i])
    }
    
    normalized_fuzzy_weight_matrix <- array(NA,dim = c(n,n))
    
    for (j in 1:n){
      for (i in 1:n){
        normalized_fuzzy_weight_matrix[i,j] = fuzzy_weight_matrix[i,j]/weight_sums[j]
      }
    }
    
    normalized_criteria_weights = array(NA,dim = n)
    
    for (i in 1:n){
      normalized_criteria_weights[i] = mean(normalized_fuzzy_weight_matrix[i,])
    }
    
    # STEP 3 - CONSISTENCY
    
    consistent_fuzzy_weight_matrix <- array(NA,dim = c(n,n))
    
    for (i in 1:n){
      consistent_fuzzy_weight_matrix[,i] = fuzzy_weight_matrix[,i]*normalized_criteria_weights[i]
    }
    
    weighted_sums = array(NA,dim = n)
    
    for (i in 1:n){
      weighted_sums[i] = sum(consistent_fuzzy_weight_matrix[i,])
    }
    
    weighted_criteria_sums = array(NA,dim = n)
    weighted_criteria_sums = weighted_sums/normalized_criteria_weights
    
    delta_max = mean(weighted_criteria_sums)
    consistency_index = (delta_max-n)/(n-1)
    
    return(consistency_index)
    
  }
  calculate()
  
  # Calculating the CI/RI ratio and final weight vector
  
  calculate_score <- function(){
      
    score <- calculate()/random_index_dict[n]
    
    if (score > 0.10){
      return(FALSE)
    }
    else {
      return(normalized_criteria_weights)
    }
    
  }
  calculate_score()

# - - - - - - - - - - - - - - - - - - - - - - - - - 
# 2. Attempt using FuzzyAHP

  # Define Matrix
  pairwiseComparisonMatrix <- pairwiseComparisonMatrix(fuzzy_weight_matrix)
  
  # Matrix Text Representation
  (textMatrix = textRepresentation(pairwiseComparisonMatrix, whole = FALSE))
  
  # Check Matrix Consistency & Ratio (CI/RI)
  consistencyIndex(pairwiseComparisonMatrix)
  consistencyRatio(pairwiseComparisonMatrix, print.report = TRUE)
  
  # Fuzzify Matrix
  
  (fuzzyPairwiseComparisonMatrix <- 
    fuzzyPairwiseComparisonMatrix(pairwiseComparisonMatrix, 
                                  fuzzyScale = getFuzzyScale(type = "full"),
                                  comparsionNotInScale = FALSE, 
                                  width = 1))

  
  # Get fuzzy values
  
  my_fuzzyData <- array(NA,dim = c(n*n,3))
  
  for (i in 1:n){
    for (j in 1:n){
      my_fuzzyData[(i*n)+j-n,1] <- fuzzyPairwiseComparisonMatrix@fnMin[j,i]
      my_fuzzyData[(i*n)+j-n,2] <- fuzzyPairwiseComparisonMatrix@fnModal[j,i]
      my_fuzzyData[(i*n)+j-n,3] <- fuzzyPairwiseComparisonMatrix@fnMax[j,i]
      
    }
  }
  
  plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 1), xlab = "X-axis", ylab = "Y-axis")
  
  # Plot the first fuzzy set as cones
  
  for (i in 1:(n*n)){
    polygon(c(my_fuzzyData[i,1], my_fuzzyData[i,2], my_fuzzyData[i,3]), c(0, 1, 0), col = sample(seq(0, 100, by = 3), replace = FALSE, prob = NULL))
  }
  
  # Calculate weights
  
  weights <- calculateWeights(pairwiseComparisonMatrix)
  
  # Input data
  
  values = c(250,16,12,5,
             250,16,8,3,
             300,32,16,4,
             275,32,8,4,
             225,16,16,2)
             
  values = matrix(values, nrow = length(values)/length(weights@weights), ncol = length(weights@weights), byrow = TRUE)
  
  
  # Calculare Result
  
  result <- calculateAHP(weights, values)
  rank <- compareResults(result)

  result <- cbind(values, result, rank)
  colnames(result) <- c("crit1", "crit2", "crit3", "crit4", "result_value", "ranking")
  print(result)

  
