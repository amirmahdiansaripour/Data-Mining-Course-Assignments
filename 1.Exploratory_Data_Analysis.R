
data_set = read.csv("C:\\Users\\amirmahdi\\Desktop\\Miningdata\\datasets\\Churn.csv", header = T)[, -1]


## Contingency table for Churn and Int.l.Plan
(t1 = table(data_set$Churn., data_set$Int.l.Plan,
            dnn = c("Churn.", "Int.l.Plan")))
(t12 = addmargins(t1, FUN = sum))

p1 = prop.table(t1, margin = 2)
(round(p1, 3))

## Barplot for Churn and Int.l.Plan
barplot(t1, col = c("lightblue", "darkred"), las = 1, ylim = c(0, 3500)
        , axis.lty = 1, main = "Int.l.Plan and Churn. barplot" , xlab = "Int.l.Plan", ylab = "Frequency")

legend("topright", legend = c("True.", "False."), col = c("darkred", "lightblue")
       , pch = 20, title = "Churn")

box(which = "plot", lty = "solid", col = "black")

## Chi-squared test for Churn and Int.l.Plan
CrossTable(y = data_set$Churn., x = data_set$Int.l.Plan,
           prop.r = FALSE, prop.t = FALSE
           , chisq = TRUE, prop.chisq = FALSE,
           dnn = c("Int.l.Plan", "Churn"))

####################################

## Multi-variate analysis

subset_churn.false = subset(data_set, data_set$Churn. == "False." & 
                              data_set$Day.Mins > 250 & data_set$Eve.Mins >= 200)
subset_churn.true = subset(data_set, data_set$Churn. == "True." & 
                             data_set$Day.Mins > 250 & data_set$Eve.Mins >= 200)

t.test(subset_churn.true$Intl.Charge, subset_churn.false$Intl.Charge)


################### 
######## new binning (low and high)

new_data_set = cbind(CustServ = data_set$CustServ.Calls, 
                     ChurnRate = data_set$Churn.)
for(i in 1 : nrow(new_data_set)){
  if(as.numeric(new_data_set[i, 1]) >= 4){
    new_data_set[i, 1] = "high";
  }
  else{
    new_data_set[i, 1] = "low";
  }
}

library(gmodels)
CrossTable(y = new_data_set[, 1], x = new_data_set[, 2],
           prop.r = FALSE, prop.t = FALSE
           , chisq = TRUE, prop.chisq = FALSE,
           dnn = c("Churn", "Cust.Serv.Calls"))

####################
## Continuous variables

ggplot(data_set, aes(data_set$Account.Length, fill = Churn.)) + 
  geom_histogram(bins = 100)

## Proportional
ggplot(data_set, aes(Intl.Charge, fill = Churn.)) + 
  geom_histogram(bins = 21, position = "fill")

################
## Joint scatter plot
x_scatter = data_set$Day.Mins;
x_l = "DayMins"
y_scatter = data_set$Intl.Calls;
y_l = "DayCharge"
base = data_set$Churn.;

plot(x_scatter[base == "True."], 
     y_scatter[base == "True."], col = "lightblue", pch = 20, 
     cex = 1, xlab = x_l, ylab = y_l)

points(x_scatter[base == "False."], 
       y_scatter[base == "False."], col = "red", pch = 20, cex = 1)

legend("topleft", legend = c("True", "False"), 
       col = c("lightblue", "red"), title = "Chrun.", pch = 20, cex = 0.7)



