install.packages("class")
install.packages(c("plyr" , "ggplot2"))
library(class)
library(plyr)
library(ggplot2)

# Age, Income, Marital
new = c(0, 0.88, 0)

R2 = c(0.25, 0.01, 1)
R3 = c(0.13, 0.19, 1)
R4 = c(0.65, 0, 1)
R5 = c(0.06, 0.92, 0)
R6 = c(0.38, 0.39, 0)
R7 = c(0.72, 0.19, 0)
R8 = c(0.75, 1, 1)
R9 = c(0.63, 0.9, 1)
R10 = c(1, 0.48, 1)

data_set = rbind(R2, R3, R4, R5, R6, R7, R8, R9, R10)
dimnames(data_set) = list(c("R2 (Bad loss)", "R3 (Bad loss)", "R4 (Bad loss)", "R5 (Bad loss)", "R6 (Good risk)", "R7 (Good risk)", "R8 (Good risk)", "R9 (Good risk)", "R10 (Good risk)")
                      , 
                      c("Age", "Income", "Marital"))

trueClass = c("Bad loss", "Bad loss", "Bad loss", "Bad loss", "Good risk", "Good risk", "Good risk", "Good risk", "Good risk")

knn(data_set, new, cl= trueClass, k = 2, prob = TRUE)
