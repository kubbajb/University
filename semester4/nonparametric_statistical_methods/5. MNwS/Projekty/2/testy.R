library(dplyr)
library(readr)

files <- file.choose()

results <- tibble(name = files)

##1 Test d�ugo�ci 
n = 10
family_1 = "norm"
par_1 = c(1,0)
family_2 = "norm"
par_2 = c(0,1)
p = 0.5

results$test_1 <- NA

for (i in 1:nrow(results)){
  tryCatch(source(results$name[i]), error=function(e) NULL)
  tryCatch(results$test_1[i] <- length(rmix(n = n,
                                            family_1 = family_1,
                                            par_1 = par_1,
                                            family_2 = family_2,
                                            par_2 = par_2,
                                            p = p)) == n, error=function(e) NULL)
  tryCatch(rm(rmix), error=function(e) NULL)
}

##2 Test mediany norm-norm
n = 1000
family_1 = "norm"
par_1 = c(-1,1)
family_2 = "norm"
par_2 = c(1,1)
p = 0.5

qs <- quantile(c(rnorm(1000, -1, 1), rnorm(1000, 1, 1)), probs = c(.4, .6))

results$test_2 <- NA

for (i in 1:nrow(results)){
  tryCatch(source(results$name[i]), error=function(e) NULL)
  rm(sample)
  tryCatch(sample <- rmix(n = n,
                          family_1 = family_1,
                          par_1 = par_1,
                          family_2 = family_2,
                          par_2 = par_2,
                          p = p), error=function(e) NULL)
  
  tryCatch(results$test_2[i] <- median(sample) > qs[1] & median(sample) < qs[2], error=function(e) NULL)
  tryCatch(rm(rmix), error=function(e) NULL)
}


##3 Test kwantyla norm-t
n = 1000
family_1 = "norm"
par_1 = c(0,1)
family_2 = "t"
par_2 = c(10)
p = 0.1

qs <- quantile(c(rnorm(1000, 0,1), rt(9000, 10)), probs = c(.94, .96))

results$test_3 <- NA

for (i in 1:nrow(results)){
  tryCatch(source(results$name[i]), error=function(e) NULL)
  
  rm(sample)
  tryCatch(sample <- rmix(n = n,
                          family_1 = family_1,
                          par_1 = par_1,
                          family_2 = family_2,
                          par_2 = par_2,
                          p = p), error=function(e) NULL)
  
  tryCatch(results$test_3[i] <- quantile(sample, probs = .95) > qs[1] & quantile(sample, probs = .95) < qs[2], error=function(e) NULL)
  tryCatch(rm(rmix), error=function(e) NULL)
}

##4 Test kwantyla pois-pois
n = 1000
family_1 = "pois"
par_1 = c(1)
family_2 = "pois"
par_2 = c(5)
p = 0.9

qs <- quantile(c(rpois(1000, 5), rpois(9000, 1)), probs = c(.09, .11))

results$test_4 <- NA

for (i in 1:nrow(results)){
  tryCatch(source(results$name[i]), error=function(e) NULL)
  
  rm(sample)
  tryCatch(sample <- rmix(n = n,
                          family_1 = family_1,
                          par_1 = par_1,
                          family_2 = family_2,
                          par_2 = par_2,
                          p = p), error=function(e) NULL)
  
  tryCatch(results$test_4[i] <- quantile(sample, probs = .05) >= qs[1] & quantile(sample, probs = .05) <= qs[2], error=function(e) NULL)
  tryCatch(rm(rmix), error=function(e) NULL)
}


##5 Test EX pois-binom
n = 1000
family_1 = "pois"
par_1 = c(1)
family_2 = "binom"
par_2 = c(10, .5)
p = 0.5

qs <- mean(c(rpois(3000, 1), rbinom(3000, 10, .5)))*c(.9,1.1)

results$test_5 <- NA

for (i in 1:nrow(results)){
  tryCatch(source(results$name[i]), error=function(e) NULL)
  
  rm(sample)
  tryCatch(sample <- rmix(n = n,
                          family_1 = family_1,
                          par_1 = par_1,
                          family_2 = family_2,
                          par_2 = par_2,
                          p = p), error=function(e) NULL)
  
  tryCatch(results$test_5[i] <- mean(sample) >= qs[1] & mean(sample) <= qs[2], error=function(e) NULL)
  tryCatch(rm(rmix), error=function(e) NULL)
}


##6 Test EX unif-t
n = 10000
family_1 = "unif"
par_1 = c(0,1)
family_2 = "t"
par_2 = c(10)
p = 0.5

qs <- mean(c(runif(10000, 0, 1), rt(10000, 10))) + c(-.5, .5)

results$test_6 <- NA

for (i in 1:nrow(results)){
  tryCatch(source(results$name[i]), error=function(e) NULL)
  
  rm(sample)
  tryCatch(sample <- rmix(n = n,
                          family_1 = family_1,
                          par_1 = par_1,
                          family_2 = family_2,
                          par_2 = par_2,
                          p = p), error=function(e) NULL)
  
  tryCatch(results$test_6[i] <- mean(sample) >= qs[1] & mean(sample) <= qs[2], error=function(e) NULL)
  tryCatch(rm(rmix), error=function(e) NULL)
}

##7 Test domy�lnych argument�w 
n = 1000
family_1 = "norm"
par_1 = c(1)
family_2 = "norm"
par_2 = c(-1,1)
p = 0.5

qs <- quantile(c(rnorm(1000, 1, 1), rnorm(1000, -1, 1)), probs = c(.4, .6))

results$test_7 <- NA

for (i in 1:nrow(results)){
  tryCatch(source(results$name[i]), error=function(e) NULL)
  
  rm(sample)
  tryCatch(sample <- rmix(n = n,
                          family_1 = family_1,
                          par_1 = par_1,
                          family_2 = family_2,
                          par_2 = par_2,
                          p = p), error=function(e) NULL)
  
  tryCatch(results$test_7[i] <- median(sample) > qs[1] & median(sample) < qs[2], error=function(e) NULL)
  tryCatch(rm(rmix), error=function(e) NULL)
}

##8 Test EX lnorm-weibull
n = 1000
family_1 = "lnorm"
par_1 = c(0,1)
family_2 = "weibull"
par_2 = c(1, 1)
p = 0.5

qs <- mean(c(rlnorm(3000, 0, 1), rweibull(3000, 1, 1)))*c(.5,2)

results$test_8 <- NA

for (i in 1:nrow(results)){
  tryCatch(source(results$name[i]), error=function(e) NULL)
  
  rm(sample)
  tryCatch(sample <- rmix(n = n,
                          family_1 = family_1,
                          par_1 = par_1,
                          family_2 = family_2,
                          par_2 = par_2,
                          p = p), error=function(e) NULL)
  
  tryCatch(results$test_8[i] <- mean(sample) >= qs[1] & mean(sample) <= qs[2], 
           error=function(e) NULL)
  tryCatch(rm(rmix), error=function(e) NULL)
}

#SUMA

results$suma <- 0

for(i in 1:nrow(results)){
  results$suma[i] <- sum(results[i, 2:9] == TRUE, na.rm = T)
}
results$punkty <- results$suma * 2.5

print(results)