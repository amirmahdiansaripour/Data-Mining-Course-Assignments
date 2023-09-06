install.packages(c("rpart" , "rpart.plot"));
library(rpart)
library(rpart.plot)

data_set = read.csv("C:\\Users\\amirmahdi\\Desktop\\Miningdata\\datasets\\Churn.csv", stringsAsFactors = T, header = T)

right = paste0(colnames(data_set)[-c(22, 1, 2, 4, 5, 11, 14, 17, 20)] , collapse = "+") 
#Omit state and Area.code (independent), phone and charges (correlated)

Churn_column = 22;
formula = paste0(colnames(data_set)[Churn_column] , "~" , right)
formula = as.formula(formula)
formula

## Construct a CART Tree
model3 = rpart(formula , data = data_set)
predicted = predict(model3 , type = "class" )

## Display the confusion matrix
(tab = table(data_set$Churn. , predicted))
TN = tab[1,1] ; FP = tab[1,2] ; FN = tab[2,1] ; TP = tab[2,2]

(FPP = FP / (FP + TP))
(FNP = FN / (FN + TN))
(ErrorRate = (FN + FP) / (FN + FP + TN + TP))
(Accuracy = 1 - ErrorRate)
(Sensitivity = TP / (TP + FN))
(Specifity = TN / (TN + FP))












