library(ggplot2)
library(nortest)
library(stats)

rejection_kolmogorov_classical <- vector("numeric", n_simulations)
rejection_kolmogorov_pit <- vector("numeric", n_simulations)
rejection_chi2_classical <- vector("numeric", n_simulations)
rejection_chi2_pit <- vector("numeric", n_simulations)


n_data <- 50
n <- n_data
df <- 10
dgf <- df
n_simulations <- 1000  
cut_points <- 10
cuts <- cut_points
  
data <- rt(n_data, df)


# Perform chi-square test (classical)
obs_table_classical <- table(cut(data, breaks = quantile(data, probs = seq(0, 1, 0.1)), include.lowest = TRUE))
expected_prob_classical <- diff(pt(attr(obs_table_classical, "breaks"), df))
chi2_test_classical <- chisq.test(obs_table_classical, p = expected_prob_classical)
rejection_chi2_classical[i] <- as.integer(chi2_test_classical$p.value < 0.05)


set.seed(123)

# Generate random data from a t-distribution with 10 degrees of freedom
data <- rt(n = 100, df = 10)

# Define the expected frequencies based on the t-distribution
expected_freq <- dt(sort(data), df = 10)


sum(expected_freq)

# Perform the chi-squared test
chi_sq_test <- chisq.test()







# normal chi squared test
chisq_p_val <- function(n, dgf, cuts){
  
  pvals <- c()
  
  # simulation loop
  for (j in 0:1000){
    
    # sample data
    data <- rt(n = n, df = dgf)

    # min,max and bin calculation
    lowest <- min(data)
    largest <- max(data)
    cut_points <- seq(lowest, largest, length.out = cuts)
    
    # binning
    observed <- as.vector(table(cut(data, breaks = cut_points)))

    # cut distribution to obtain theoretical probabilities
    for (i in 1:(cuts-1)){
      probability[i] <- pt(cut_points[i+1], df) - pt(cut_points[i], df)
    }
    
    # multiply probabilities by n to obtain expected frequencies
    expected <- vector("numeric", cuts)
    expected <- round(probability * n, 0)

    chisq.test(x = observed, y = expected)$p.value
    
    pval <- 
    pvals[j] <- pval
  }
  return(mean(pvals))
}



chi2_test <- function(data, distribution) {
  chi2_result <- chisq.test(data, p = distribution)
  return(chi2_result$p.value)
}


chi2_test(data, pt)


