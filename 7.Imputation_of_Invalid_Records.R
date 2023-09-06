
data_set = read.csv("C:\\Users\\amirmahdi\\Desktop\\Miningdata\\datasets\\Cereal.csv", header = T)
name_and_Type_and_Rating = c(1 , 3, 14);
data_set = data_set[, -name_and_Type_and_Rating]; ## omit name and type and rating
table(data_set["potass"])
table(data_set["carbo"])
table(data_set["sugars"])

data_set$sugars[58] = NA ## It is -1 , so it should be imputed.
carbo_sugar_potass = c(1, 7, 8, 9);
right = paste0(colnames(data_set)[-carbo_sugar_potass] , collapse = "+") # Variables needed to be imputed

formula = paste0(colnames(data_set)[8] , "~" , right)
formula = as.formula(formula)

model1 = lm( formula , data = data_set)
Sugars_missing_index = which(is.na(data_set)[,8] == T)

useful_columns = c(2, 3, 4, 5, 6, 10, 11, 12, 13);
Imputed_Sugars = predict(model1 , newdata = data_set[Sugars_missing_index, useful_columns])
Imputed_Sugars
data_set[Sugars_missing_index,8] = Imputed_Sugars;

## Another invalid value to be imputed
data_set$carbo[58] = NA
right =  paste0(colnames(data_set)[-c(1, 7, 9)] , collapse = "+") #Target is carbo
formula = paste0(colnames(data_set)[7] , "~" , right)
formula = as.formula(formula)
model1 = lm( formula , data = data_set)
Carbo_missing_index = which(is.na(data_set)[,7] == T)
#Carbo_missing_index
useful_columns = c(2, 3, 4, 5, 6, 8, 10, 11, 12, 13);

Imputed_Carbo = predict(model1 , newdata = data_set[Carbo_missing_index, useful_columns])
data_set[Carbo_missing_index,7] = Imputed_Carbo
