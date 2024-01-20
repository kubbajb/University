library("data.table")
library("dplyr")
library("data.tree")
library("DiagrammeR")

#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# load dataset
raw_data <- data.frame(iris)

# convert category to a factor variable
raw_data$Species <- factor(raw_data$Species, )

# convert factor to numeric variable
raw_data <- raw_data %>%
  mutate(Species = as.numeric(Species)) %>%
  mutate(across(-Species, ~ if_else(. > mean(., na.rm = TRUE), 1, 0)))

data <- raw_data %>%
  select(-Species)

# calculate number of variables and their distinct values
variables <- variable.names(raw_data)
variables_number <- length(variables)

#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# select the primary attribute
var_select <- function(){
  cat("Select primary attribute:\n")
  
  for (i in seq_along(variables)) {
    cat(i, "-", variables[i], "\n")
  }
  
  selected_var_index <- 
    as.integer(readline(prompt = "Enter the index of the variable: "))
  
  selected_var_name <- 
    variables[selected_var_index]
  
  return(selected_var_index)
}

primary_variable <- var_select()
plot(acme)
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# FUNCTION: entropy (one vector)
entropy <- function(vctr){
  # This function analyzes the entropy of a vector passed in the argument
  
  tbl <- table(vctr) #summary of vector
  entr <- 0
  
  for (vls in 1:n_distinct(vctr)){
    p <- unname(tbl[vls]) / sum(unname(tbl))
    entr = entr + (-p*log2(p))
  }
  result <- entr
}

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
}

# (entropy(data$Species))
# (cond_entropy(data$Petal.Width, data$Species, 1))

#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Calculations

calculate_node <- function(condition = FALSE){

  while (condition == FALSE){
    
    lowest_entropy = 0
    run = 0
    
    for (i in 1:length(variable.names(data))){
      temp_entropy = 0
      for (j in 1:length(unique(raw_data$Species))){
        temp_entropy = temp_entropy + (cond_entropy(raw_data[i], raw_data$Species, j)*sum(table(subset.data.frame(raw_data[i], raw_data$Species == 1)))/length(unname(raw_data$Species)))
        run = run + 1
        cat("\n diagnostic run, species: ", j, ", temp entropy: ", temp_entropy, ", run: ", run)
      }
      
      cat("\n\n Combination \n variable: ", colnames(raw_data)[i], "\n total entropy: ", temp_entropy, "\n\n")
    }
    condition = TRUE
  }
}

calculate_node()

(cond_entropy(raw_data$Petal.Length, raw_data$Species, 2))

#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Build decision tree 2

