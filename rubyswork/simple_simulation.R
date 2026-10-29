# simple simulation study 

set.seed(22)

mu <- 7 
var <- 5
s <- sqrt(2)
n <- 50
rep <- 100000

#count to keep track of the number in CI
count = 0

for (i in 1:rep) {
  
  sample <- rnorm(n, mean=mu, sd=s)
  x_bar <- mean(sample)
  se_bar <- sd(sample)/sqrt(n)
  
  #caluculating bounds 
  lb <- x_bar-1.96*se_bar 
  ub <- x_bar+1.96*se_bar
  
  #check in CI 
  if (mu >= lb && mu <= ub){
    count = count + 1
  }
  
}

#total in CI as percentage
sucess_perc = count/rep

print(sucess_perc)
