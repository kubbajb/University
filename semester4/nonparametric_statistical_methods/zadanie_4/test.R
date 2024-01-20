library(dplyr)

n <- 100
dgf <- 10
cuts <- 10

pvals <- c()

chisq_p_val <- function(n, dgf, cuts){

  # simulation loop
  for (j in 0:100){
    
    # sample data
    data <- rchisq(n = n, df = dgf)

    # division into bins
    lowest <- 0
    largest <- max(data)
    cut_points <- seq(lowest, largest, length.out = cuts)

    # calculate the theoretical probabilities of the values  
    pdf_probabilities <- dchisq(1:(length(cut_points)-1), dgf)
    pdf_probabilities <- c(pdf_probabilities,1-sum(pdf_probabilities))  

    # sample binning
    observed <- c()
    for (i in 0:length(cut_points)){
      observed[i] <- as.numeric(sum(data > cut_points[i] & na.omit(data <= cut_points[i+1])))
    }
    
    pval <- chisq.test(x = observed, p = pdf_probabilities)$p.value
    print(pval)
    pvals[j] <- pval
  }
  return(pvals)
}

x <- chisq_p_val(100, 10, 3)



mean(pvals)
sum(observed)
