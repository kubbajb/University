library("data.table")
library("dplyr")
library("data.tree")
library("DiagrammeR")

# - - - - - - - - - - - - - - BINARY TABLE CREATION - - - - - - - - - - - -

# load dataset
data <- read.csv("D:/Dokumenty/Studia/Semestr_4/elementy_sztucznej_inteligencji/Projekt/projekt_BDD.csv")

# convert all categorical data to factor variables
for (i in 1:ncol(data)){
  data[,i] <- factor(data[,i], )
}

# - - - - - - - - - - - - - - - OTHER FUNCTIONS - - - - - - - - - - - - - -

max_factor_levels <- function(df) {
  max_levels <- 0
  for (col in colnames(df)) {
    if (is.factor(df[[col]])) {
      n_levels <- length(levels(df[[col]]))
      if (n_levels > max_levels) {
        max_levels <- n_levels
      }
    }
  }
  return(max_levels)
}

# - - - - - - - - - - - - - - PRIMARY VALUE SELECTION - - - - - - - - - - - -

# calculate number of variables and their distinct values

variables <- variable.names(data)

# Select the primary attribute
var_select <- function(){
  cat("Select primary attribute:\n")
  
  for (i in seq_along(variables)) {
    cat(i, "-", variables[i], "\n")
  }
  
  selected_var_index <- 
    as.integer(readline(prompt = "Enter the index of the variable: "))
  
  readline(prompt = "Press [enter] to accept.")
  
  selected_var_name <- 
    variables[selected_var_index]
  
  return(selected_var_index)
}

# Assign primary attribute
primary_variable <- var_select()

# - - - - - - - - - - - DEFINITION OF ENTROPY FUNCTIONS - - - - - - - - - - -

# FUNCTION: Simple entropy (one vector)
entropy <- function(vctr){
  # This function analyzes the entropy of a vector passed in the argument
  
  tbl <- table(vctr) #summary of vector
  entr <- 0
  
  for (vls in 1:n_distinct(vctr)){
    p <- unname(tbl[vls]) / sum(unname(tbl))
    entr = entr + (-p*log2(p))
  }
  result <- entr
} #ok

# FUNCTION: Conditional entropy (two vectors, condition)
cond_entropy <- function(atr2, atr1, cond){
  
  sbst <- subset(atr2, atr1 == cond)
  tbl <- table(sbst)
  cdet <- 0
  
  for (vls in 1:n_distinct(sbst)){
    p <- unname(tbl[vls]) / sum(unname(tbl))
    cdet = cdet + (-p*log2(p))
  }
  result <- cdet
} #ok
#  - - - - - - - - - - - CALCULATEING LOWEST ENTROPY - - - - - - - - - - - - - -

rep1 <- (sum(sapply(lapply(data, nlevels), sum))-1)
rep2 <- ncol(data)

# Calculations
for (j in 1:rep1){
  base_entropy <- entropy(data[primary_variable]) #ok
  inf_gain_table <- data.frame(matrix(NA, nrow = ncol(data), ncol = max_factor_levels(data)))
  colnames(inf_gain_table) <- seq(1:max_factor_levels(data))
  rownames(inf_gain_table) <- names(data)
  
  for (i in 1:rep2) {
    for (k in 1:(length(levels(data[,i])))){

      subset <- data %>%
        select(primary_variable, i) %>%
        filter(data[,i] == data[,i][k])
      
      inf_gain_table[i,k] <- base_entropy-(entropy(subset))
    }
  }
  
  max_inf_gain <- max(inf_gain_table[-primary_variable,], na.rm = TRUE)
  matches <- which(inf_gain_table == max_inf_gain, arr.ind = TRUE)
  row_index <- matches[1, 1]
  col_index <- matches[1, 2]

  #row_index <- which(inf_gain_table[-primary_variable,] == max_inf_gain, arr.ind = TRUE)[1]
  #col_index <- which(inf_gain_table[-primary_variable,] == max_inf_gain, arr.ind = TRUE)[2]
  
  # Print the decision
  cat("\n\n - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - \n")
  cat("\nStep ", j, ": Maximum information gain (", max_inf_gain, ") when variable ", rownames(inf_gain_table)[row_index], " is ", levels(data[,row_index])[col_index], ".\n", sep = "")
  
  subsubset <- data %>%
    select(primary_variable) %>%
    filter(data[,row_index] == levels(data[,row_index])[col_index])
  
  for (l in 1:length(levels(data[,primary_variable]))){
    cat("  removed:  ", levels(data[,primary_variable])[l], ": ", sum(subsubset == levels(data[,primary_variable])[l]), "   ", sep = "")
  }
  cat("\n")
  data <- data %>%
    filter(data[,row_index] != levels(data[,row_index])[col_index])
  
  for (l in 1:length(levels(data[,primary_variable]))){
    cat("  remaining: ", levels(data[,primary_variable])[l], ": ", sum(data == levels(data[,primary_variable])[l]), "   ", sep = "")
  }
  
  data <- data %>% mutate_all(funs(droplevels))
  
  if (max_inf_gain == 0){
    cat("\n - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - \n\n")
    cat("This was the final step. Binary Decision Tree complete.")
    break
  }
}

