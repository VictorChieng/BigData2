df <-read.csv ("C:/Users/victo/OneDrive/Desktop/year_borough_grocery.csv")

#Use Sequential

df3 <- function(i){
  library(data.table)
  library(tidyverse)
  library(microbenchmark)
  list.files(path = "/Users/victo/OneDrive/Desktop/bds/DataAreaGrocery/", pattern = "*.csv") %>%
    map_df(~fread(.))
}

mbm4 <- microbenchmark("sequential Processing" = {lapply(1:100, df3)})
mbm4
library(ggplot2)
autoplot(mbm4)



#Use Parallel
parallelProcessing <- function(i){
  library(data.table)
  library(tidyverse)
  library(microbenchmark)
  library(parallel)
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, library(lme4))
  parLapply(cl, 1:100, df3)
  stopCluster(cl)
}

mbm5 <- microbenchmark("Parallel Processing" = parallelProcessing)
mbm5
library(ggplot2)
autoplot(mbm5)

#detectCores()


mbm6 <- microbenchmark("Sequential Processing" = {lapply(1:100, df3)}, "Parallel Processing"= parallelProcessing)
mbm6
library(ggplot2)
autoplot(mbm6)

# PARALLEL PROCESSING CODE======================================================



a2 <-read.csv ("C:/Users/victo/OneDrive/Desktop/ProjectBD.csv")
str(a2)
summary(a2)

library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(data.table)

View (df) 
colnames(df) 

View (a2) 
colnames(df) 


#code for mean value
mean1 <- mean(a2$male)
print(mean1)

mean2 <- mean(a2$female)
print(mean2)

mean3 <- mean(a2$num_transactions)
print(mean3)

mean4 <- mean(a2$weight)
print(mean4)

mean5 <- mean(a2$volume)
print(mean5)

mean6 <- mean(a2$age_0_17)
print(mean6)

mean7 <- mean(a2$age_18_64)
print(mean7)

mean8 <- mean(a2$age_65.)
print(mean8)


#Visualization for Mean and Median
hist(a2$male)

hist(a2$female)

hist(a2$num_transactions)

hist(a2$weight)

hist(a2$volume)

hist(a2$age_0_17)

hist(a2$age_18_64)

hist(a2$age_65.)






# Calculating Standard deviation
std1 <- sd(a2$male)
print(std1)

std5 <- sd(a2$female)
print(std5)

std3 <- sd(a2$num_transactions)
print(std3)

std4 <- sd(a2$weight)
print(std4)

std5 <- sd(a2$volume)
print(std5)

std6 <- sd(a2$age_0_17)
print(std6)

std7 <- sd(a2$age_18_64)
print(std7)

std8 <- sd(a2$age_65.)
print(std8)



# Visualization for Mean and Standard Deviation
standard_df <- data.frame(meanValue = c(mean(a2$male), 
                                        mean(a2$female), 
                                        mean(a2$num_transactions),
                                        mean(a2$weight),
                                        mean(a2$volume),
                                        mean(a2$age_0_17),
                                        mean(a2$age_18_64),
                                        mean(a2$age_65.)),
                          
                          
                          stdValue = c(sd(a2$male), 
                                       sd(a2$female), 
                                       sd(a2$num_transactions),
                                       sd(a2$weight),
                                       sd(a2$volume),
                                       sd(a2$age_0_17),
                                       sd(a2$age_18_64),
                                       sd(a2$age_65.)),
                          
                          Category=c("male","female","num_transactions","weight","volume","age_0_17" ,"age_18_64","age_65."))




# Load ggplot2
library(ggplot2)

ggplot(standard_df, aes(x=Category, y=meanValue)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour='black') +
  geom_errorbar(aes(ymin=meanValue-stdValue, ymax=meanValue+stdValue), width=.2)

# Visualization for Mean and Standard Deviation
standard_df2 <- data.frame(meanValue = c(mean(a2$male), 
                                         mean(a2$female), 
                                         mean(a2$weight),
                                         mean(a2$volume),
                                         mean(a2$age_0_17),
                                         mean(a2$age_18_64),
                                         mean(a2$age_65.)),
                           
                           
                           stdValue = c(sd(a2$male), 
                                        sd(a2$female), 
                                        sd(a2$weight),
                                        sd(a2$volume),
                                        sd(a2$age_0_17),
                                        sd(a2$age_18_64),
                                        sd(a2$age_65.)),
                           
                           Category=c("male","female","weight","volume","age_0_17" ,"age_18_64","age_65."))


# Load ggplot2
library(ggplot2)

ggplot(standard_df2, aes(x=Category, y=meanValue)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour='black') +
  geom_errorbar(aes(ymin=meanValue-stdValue, ymax=meanValue+stdValue), width=.2)



# Visualization for Mean and Standard Deviation
standard_df3 <- data.frame(meanValue = c(mean(a2$weight),
                                         mean(a2$volume)),
                           
                           
                           
                           stdValue = c(sd(a2$weight),
                                        sd(a2$volume)),
                           
                           
                           Category=c("weight","volume"))


# Load ggplot2
library(ggplot2)

ggplot(standard_df3, aes(x=Category, y=meanValue)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour='black') +
  geom_errorbar(aes(ymin=meanValue-stdValue, ymax=meanValue+stdValue), width=.2)


#======================================================================================================================

# Compute the median value
median1 <- median(a2$male)
print(median1)

median2 <- median(a2$female)
print(median2)

median3 <- median(a2$num_transactions)
print(median3)

median4 <- median(a2$weight)
print(median4)

median5 <- median(a2$volume)
print(median5)

median6 <- median(a2$age_0_17)
print(median6)

median7 <- median(a2$age_18_64)
print(median7)

median8 <- median(a2$age_65.)
print(median8)




# DESCRIPTIVE ANALYSIS==========================================================

getwd()



# HYPOTHESIS TESTING CODE==================================================================

num_trans_male <- lm(num_transactions ~ male, data = a2)
print(summary(num_trans_male))

num_trans_female <- lm(num_transactions ~ female, data = a2)
print(summary(num_trans_female))
# HYPOTHESIS TESTING CODE==================================================================

# HYPOTHESIS TESTING CODE==================================================================

weight_age_0_17 <- lm(weight ~ age_0_17, data = a2)
print(summary(weight_age_0_17))

weight_age_18_64 <- lm(weight ~ age_18_64, data = a2)
print(summary(weight_age_18_64))

weight_age_65. <- lm(weight ~ age_65., data = a2)
print(summary(weight_age_65.))
# HYPOTHESIS TESTING CODE==================================================================

# HYPOTHESIS TESTING CODE==================================================================

volume_age_0_17 <- lm(volume ~ age_0_17, data = a2)
print(summary(volume_age_0_17))

volume_age_18_64 <- lm(volume ~ age_18_64, data = a2)
print(summary(volume_age_18_64))

volume_age_65. <- lm(volume ~ age_65., data = a2)
print(summary(volume_age_65.))
# HYPOTHESIS TESTING CODE==================================================================


# CORRELATION BETWEEN THE NUMERICAL DATAS (male and female VS num_transaction)=======================================

mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  cormatrix <- cor(mydataframe)
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  fm <- as.data.frame(as.table(cormatrix))
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}
model2 <- select(a2, 'male', 'female', 'num_transactions')
mosthighlycorrelated(model2, 20)

library ("ggpubr")
ggscatter(a2, x = "male", y = "num_transactions", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "male population", ylab = "number of transaction")


library ("ggpubr")
ggscatter(a2, x = "female", y = "num_transactions", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "female population", ylab = "number of transaction")

# CORRELATION BETWEEN THE NUMERICAL DATAS (num_transactions and weight VS volume vs age groups)=======================================

mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  cormatrix <- cor(mydataframe)
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  fm <- as.data.frame(as.table(cormatrix))
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}
model3 <- select(a2, 'weight', 'volume', 'age_0_17', "age_18_64" , "age_65.")
mosthighlycorrelated(model3, 20) 

library ("ggpubr")
ggscatter(a2, x = "age_0_17", y = "weight", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "population in the age group of 0-17", ylab = "weight of average food product purchased")


library ("ggpubr")
ggscatter(a2, x = "age_18_64", y = "weight", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "population in the age group of 18-64", ylab = "weight of average food product purchased")

library ("ggpubr")
ggscatter(a2, x = "age_65.",  y = "weight", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "population in the age group 65 and above", ylab = "weight of average food product purchased")

library ("ggpubr")
ggscatter(a2, x = "age_0_17", y = "volume", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "population in the age group of 0-17", ylab = "volume of average drink product purchased")


library ("ggpubr")
ggscatter(a2, x = "age_18_64", y = "volume", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "population in the age group of 18-64", ylab = "volume of average drink product purchased")

library ("ggpubr")
ggscatter(a2, x = "age_65.", y = "volume", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "population in the age group 65 and above", ylab = "volume of average drink product purchased")


#Regression male=========================================================================================================
# Create the predictor and response variable.
x <- c(a2$male)
y <- c(a2$num_transactions)
relation <- lm(y~x)


# Plot the chart.
plot(y,x,col = "blue",main = "Male population vs number of transaction",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "male",ylab = "number of transaction")

# Save the file.
dev.off()

#Regression female =================================================================================================
# Create the predictor and response variable.
x <- c(a2$female)
y <- c(a2$num_transactions)
relation <- lm(y~x)


# Plot the chart.
plot(y,x,col = "blue",main = "Female population vs number of transaction",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "female",ylab = "number of transaction")

# Save the file.
dev.off()



#Regression weight age0-17=========================================================================================================
# Create the predictor and response variable.
x <- c(a2$age_0_17)
y <- c(a2$weight)
relation <- lm(y~x)

# Plot the chart.
plot(y,x,col = "blue",main = "age_0_17 vs weight",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "age_0_17",ylab = "weight")

# Save the file.
dev.off()

#Regression weight age18-64=========================================================================================================
# Create the predictor and response variable.
x <- c(a2$age_18_64)
y <- c(a2$weight)
relation <- lm(y~x)

# Plot the chart.
plot(y,x,col = "blue",main = "age_18_64 vs weight",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "age_18_64",ylab = "weight")

# Save the file.
dev.off()


#Regression weight age65.=========================================================================================================
# Create the predictor and response variable.
x <- c(a2$age_65.)
y <- c(a2$weight)
relation <- lm(y~x)

# Plot the chart.
plot(y,x,col = "blue",main = "age_65. vs weight",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "age_65.",ylab = "weight")

# Save the file.
dev.off()


#Regression volume age0-17=========================================================================================================
# Create the predictor and response variable.
x <- c(a2$age_0_17)
y <- c(a2$volume)
relation <- lm(y~x)

# Plot the chart.
plot(y,x,col = "blue",main = "age_0_17 vs volume",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "age_0_17",ylab = "volume")

# Save the file.
dev.off()


#Regression volume age18-64=========================================================================================================
# Create the predictor and response variable.
x <- c(a2$age_18_64)
y <- c(a2$volume)
relation <- lm(y~x)

# Plot the chart.
plot(y,x,col = "blue",main = "age_18_64 vs volume",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "age_18_64",ylab = "volume")

# Save the file.
dev.off()


#Regression volume age65.=========================================================================================================
# Create the predictor and response variable.
x <- c(a2$age_65.)
y <- c(a2$volume)
relation <- lm(y~x)

# Plot the chart.
plot(y,x,col = "blue",main = "age_65. vs volume",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "age_65.",ylab = "volume")

# Save the file.
dev.off()


getwd


