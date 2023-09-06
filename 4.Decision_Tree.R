dataset = read.csv("C:\\Users\\amirmahdi\\Desktop\\Miningdata\\datasets\\Churn.csv" , header = T);
install.packages(c("rpart" , "rpart.plot"));
install.packages("partykit");
install.packages("C50");

library("rpart"); library("rpart.plot"); library("C50");
library("partykit");

normalize = function(a){
  return ((a - min(a)) / (max(a) - min(a)));
}

names(dataset)

## Converting categorical variables into factors
dataset$State = factor(dataset$State);
levels(dataset$State);
dataset$Area.Code = factor(dataset$Area.Code);
levels(dataset$Area.Code);
levels(dataset$Area.Code) = c("A" , "B" , "C"); #Convert 408 , 415 , 510 into A , B , C
levels(dataset$Area.Code);
dataset$Int.l.Plan = factor(dataset$Int.l.Plan);
levels(dataset$Int.l.Plan); # No , Yes
dataset$VMail.Plan = factor(dataset$VMail.Plan);
levels(dataset$VMail.Plan);
dataset$Churn. = factor(dataset$Churn.);
levels(dataset$Churn.); # False, True


## Detecting variables with high correlations
(cor_res = cor.test(dataset$Day.Charge , dataset$Day.Mins, method = "pearson"));
(cor_res = cor.test(dataset$Eve.Charge , dataset$Eve.Mins, method = "pearson"));
(cor_res = cor.test(dataset$Night.Charge , dataset$Night.Mins, method = "pearson"));
(cor_res = cor.test(dataset$Intl.Charge , dataset$Intl.Mins, method = "pearson"));

## Normalizing numerical variables
dataset$Account.Length = normalize(dataset$Account.Length);
dataset$VMail.Message = normalize(dataset$VMail.Message);
dataset$Day.Mins = normalize(dataset$Day.Mins);
dataset$Day.Calls = normalize(dataset$Day.Calls);
dataset$Eve.Mins = normalize(dataset$Eve.Mins);
dataset$Eve.Calls = normalize(dataset$Eve.Calls);
dataset$Night.Mins = normalize(dataset$Night.Mins);
dataset$Night.Calls = normalize(dataset$Night.Calls);
dataset$Intl.Mins = normalize(dataset$Intl.Mins);
dataset$Intl.Calls = normalize(dataset$Intl.Calls);
dataset$CustServ.Calls = normalize(dataset$CustServ.Calls);

## Fitting CART Tree

cartfit = rpart(Churn. ~ CustServ.Calls + Intl.Calls + Intl.Mins + Night.Calls
                + Night.Mins + Eve.Calls + Eve.Mins + Day.Calls + Day.Mins + VMail.Message + 
                  Account.Length + Int.l.Plan + VMail.Plan , data = dataset , method = "class")

print(cartfit);
rpart.plot(cartfit);


## Fitting C45 Tree

x = dataset[, c(3, 6, 7, 8, 9, 10, 12, 13, 15, 16, 18, 19, 21)];
y = factor(dataset$Churn.);

C5.0Control(
  minCases = 10,
  earlyStopping = TRUE
)

c50 = C5.0(x, y, control = C5.0Control());
plot(c50);


















