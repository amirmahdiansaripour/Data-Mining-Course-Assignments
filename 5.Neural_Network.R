data_set = read.csv("C:\\Users\\amirmahdi\\Desktop\\Miningdata\\datasets\\Churn.csv", header = T)

install.packages("neuralnet")

library(nnet)
library(neuralnet)

## Converting categorical variables into factors

data_set$State = factor(data_set$State)
levels(data_set$State)
data_set$Area.Code = factor(data_set$Area.Code)
levels(data_set$Area.Code)
levels(data_set$Area.Code) = c("1", "2", "3") #need to collapse
levels(data_set$Area.Code)
data_set$Int.l.Plan = factor(data_set$Int.l.Plan)
data_set$VMail.Plan = factor(data_set$VMail.Plan)
data_set$Churn. = factor(data_set$Churn.)

## One hot method for Area.Code (Attention: The categorical and 3rd columns of one-hot are not useful anymore, thus deleted)
newAreaCode = class.ind(data_set$Area.Code)
newdataSet = cbind(data_set, newAreaCode)[, -c(4, 25)]
newdataSet = newdataSet[, -2]  #Omit state attribute (independent from target)
colnames(newdataSet)[c(21, 22)] = c("AreaCode1", "AreaCode2")

## One hot method for Int.l.Plan
newIntPlan = class.ind(data_set$Int.l.Plan)
newdataSet = cbind(newdataSet, newIntPlan)[, -c(4, 23)]
colnames(newdataSet)[22] = "Int.l.Plan"

## One hot method for VMail.Plan
newVmailPlan = class.ind(data_set$VMail.Plan)
newdataSet = cbind(newdataSet, newVmailPlan)[, -c(4, 23)]
colnames(newdataSet)[22] = "Vmail.Plan"

## One hot method for Churn Plan
newChurn = class.ind(data_set$Churn.)
newdataSet = cbind(newdataSet, newChurn)[, -c(18, 23)]
colnames(newdataSet)[22] = "Churn."

newdataSet = newdataSet[, -c(1, 3)] # omit phone and X columns
newdataSet = newdataSet[, -c(16, 17)] # Omit AreaCode1 and 2

normalize = function(a){
  return ((a - min(a)) / (max(a) - min(a)));
}

## Checking whether there are any missing values
colSums(is.na(newdataSet))

# Correlation test and omitting those variables extremely correlated
(cor_res = cor.test(newdataSet$Day.Charge, newdataSet$Day.Mins, method = "pearson"))
(cor_res = cor.test(newdataSet$Eve.Charge, newdataSet$Eve.Mins, method = "pearson"))
(cor_res = cor.test(newdataSet$Night.Charge, newdataSet$Night.Mins, method = "pearson"))
(cor_res = cor.test(newdataSet$Intl.Charge, newdataSet$Intl.Mins, method = "pearson"))

newdataSet$Account.Length = normalize(newdataSet$Account.Length)
newdataSet$VMail.Message = normalize(newdataSet$VMail.Message)
newdataSet$Day.Mins = normalize(newdataSet$Day.Mins)
newdataSet$Day.Calls = normalize(newdataSet$Day.Calls)
newdataSet$Eve.Mins = normalize(newdataSet$Eve.Mins)
newdataSet$Eve.Calls = normalize(newdataSet$Eve.Calls)
newdataSet$Night.Mins = normalize(newdataSet$Night.Mins)
newdataSet$Night.Calls = normalize(newdataSet$Night.Calls)
newdataSet$Intl.Mins = normalize(newdataSet$Intl.Mins)
newdataSet$Intl.Calls = normalize(newdataSet$Intl.Calls)
newdataSet$CustServ.Calls = normalize(newdataSet$CustServ.Calls)

newdataSet = newdataSet[,-c(5, 8, 11, 14)] #Omit Correlated Variables

## Check transformed variables
str(newdataSet)

Churn_column = 14

right = paste0(colnames(newdataSet)[-Churn_column] , collapse = "+")
formula = paste0(colnames(newdataSet)[Churn_column] , "~" , right)
formula = as.formula(formula)

net.dat <- neuralnet(formula,
                     data = newdataSet,
                     rep = 1,
                     lifesign = "full",
                     hidden = 3,
                     linear.output=FALSE)

## Final MSE error
(mean((newdataSet[,18] - net.dat$net.result[[1]])^2))

net.dat$weights

plot(net.dat)

## Confusion Matrix
tb = table(newdataSet$Churn.,round(net.dat$net.result[[1]]))
tb
sum(diag(tb)/sum(tb))

