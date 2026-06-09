library(MASS)

####################### Functions############################
#purpose: check the number of missing data pattern in the dataset
#argument: data: data.frame or matrix that you want to check 
num.of.miss.pattern <- function(data){
  incomp.na <- is.na(data)
  pat <- 0
  while(dim(incomp.na)[1]>0){
    now <- incomp.na[1,]
    ind <- vector()
    
    for(j in 1:nrow(incomp.na)){
      check <- sum(now==incomp.na[j, ])!=ncol(incomp.na)
      ind <- c(ind, check)
    }
    incomp.na<-incomp.na[ ind,]
    pat <- pat+1
  }
  pat
}





#generating complete data
set.seed(123)
mu <- rep(0,6)
Sigma <- matrix(c(1, 0.5, 0.5, 0.5, 0.5, 0.5, 
                  0.5, 1, 0.5, 0.5, 0.5, 0.5, 
                  0.5, 0.5, 1, 0.5, 0.5, 0.5, 
                  0.5, 0.5, 0.5, 1, 0.5, 0.5, 
                  0.5, 0.5, 0.5, 0.5, 1, 0.5, 
                  0.5, 0.5, 0.5, 0.5, 0.5, 1), nrow=6, ncol=6)
sample.size <- 10000
complete.data <- mvrnorm(sample.size, mu, Sigma)


#generating 50% missing data for each of the first three variables
missing.percentage <- 0.5 #generating 50% missing data
var.with.missing <- 1:3 #first three variables have missing data

missing.percentage2 <- 0.25
var.with.missing2 <- 3:3


#################### Generating MCAR Data #########################
#generating MCAR data with minimum number of patterns 
mcar.min.pattern <- complete.data
ind <- as.logical(rbinom(sample.size, 1, missing.percentage)) #randomly deleting 50% data; univariate
mcar.min.pattern[ind,var.with.missing] <- NA
head(mcar.min.pattern,10)
num.of.miss.pattern(mcar.min.pattern)








#generating MCAR data with maximum number of patterns 
mcar.max.pattern <- complete.data
for(i in var.with.missing){
  ind <- as.logical(rbinom(sample.size, 1, missing.percentage))
  mcar.max.pattern[ind,i] <- NA
}
head(mcar.max.pattern,10)
num.of.miss.pattern(mcar.max.pattern)



############################generating MAR data#####################################
######Single cut-off method###################
#generating MAR data with strong dependence and minimum number of patterns
mar.min.strong <- complete.data
cutoff<- qnorm(missing.percentage, lower.tail = F)
mar.min.strong[mar.min.strong [,4] > cutoff,var.with.missing] <- NA
head(mar.min.strong,10)
num.of.miss.pattern(mar.min.strong)





#generating MAR data with weak dependence and minimum number of patterns
mar.min.weak <- complete.data
cutoff<- qnorm(missing.percentage, lower.tail = F)
data.greater.than.cutoff <- which(mar.min.weak [,4] > cutoff)
which.delete <- as.logical(sample(0:1,length(data.greater.than.cutoff),replace=T, prob=c(0.15, 0.85))) #1=delete
row.delete <- data.greater.than.cutoff[which.delete]
data.less.than.cutoff <- which(mar.min.weak [,4] < cutoff)
which.delete2<- as.logical(sample(0:1,length(data.less.than.cutoff),replace=T, prob=c(0.85, 0.15))) #1=delete
row.delete2 <- data.less.than.cutoff[which.delete2]
mar.min.weak[c(row.delete, row.delete2),var.with.missing] <-NA 
head(mar.min.weak,20)
num.of.miss.pattern(mar.min.weak)



#generating MAR with strong dependencies and maximum number of patterns
mar.max.strong <- complete.data
cutoff<- qnorm(missing.percentage, lower.tail = F)
for(i in var.with.missing){
  mar.max.strong[mar.max.strong [,i+3] > cutoff,i] <- NA
}
head(mar.min.strong,10)
num.of.miss.pattern(mar.max.strong)



#generating MAR with weak dependencies and maximum number of patterns
mar.max.weak <- complete.data
cutoff <- qnorm(missing.percentage, lower.tail = F)
for(i in var.with.missing){
  data.greater.than.cutoff <- which(mar.max.weak[,i+3] > cutoff)
  which.delete <- 
    as.logical(sample(0:1,length(data.greater.than.cutoff),replace=T, prob=c(0.15, 0.85))) #1=delete
  row.delete <- data.greater.than.cutoff[which.delete]
  data.less.than.cutoff <- which(mar.max.weak [,i+3] < cutoff)
  which.delete2<- 
    as.logical(sample(0:1,length(data.less.than.cutoff),replace=T, prob=c(0.85, 0.15))) #1=delete
  row.delete2 <- data.less.than.cutoff[which.delete2]
  mar.max.weak[c(row.delete, row.delete2),i] <-NA 
  }
head(mar.max.weak,10)
num.of.miss.pattern(mar.max.weak)





######Percentile Method###########
#minimum number of patterns
percentile.min <- complete.data

which.delete <- logical()
  for(i in 1:1000){
    perc <- pnorm(complete.data[,4])[i]
    which.delete[i] <- 
      as.logical(sample(0:1, 1, replace=T, prob=c((1-perc),perc)))
    
  }
percentile.min[which.delete,1:3] <- NA
  


head(percentile.min , 10)
num.of.miss.pattern(percentile.min)


#maximum number of patterns
percentile.max <- complete.data
for(j in var.with.missing){
  which.delete <- logical()
  for(i in 1:1000){
    perc <- pnorm(complete.data[,j+3])[i]
    which.delete[i] <- 
      as.logical(sample(0:1, 1, replace=T, prob=c((1-perc),perc)))
    
  }
  percentile.max[which.delete,j] <- NA
  
}

head(percentile.max , 10)
num.of.miss.pattern(percentile.max )



####Logistical regression method######
#minimum number of patterns
logistic.min <- complete.data
log.equation <-2*(complete.data[,4])
p <- 1/(1+exp(-log.equation))
which.delete <- logical()
for(i in 1:sample.size){
  which.delete[i]<- as.logical(sample(0:1,1,replace=T, prob=c((1-p[i]),p[i])))
}
logistic.min[which.delete,1:3] <- NA
head(logistic.min,10)
num.of.miss.pattern(logistic.min )


#maximum number of patterns:
logistic.max <- complete.data

for(j in var.with.missing){
  log.equation <-2*(complete.data[,j+3])
  p <- 1/(1+exp(-log.equation))
  which.delete <- logical()
  for(i in 1:sample.size){
    which.delete[i]<- as.logical(sample(0:1,1,replace=T, prob=c((1-p[i]),p[i])))
  }
  logistic.max[which.delete,j] <- NA
  
}

head(logistic.max,10)
num.of.miss.pattern(logistic.max )
