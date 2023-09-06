install.packages("cluster")
library(cluster)

data_set = read.csv("C:\\Users\\amirmahdi\\Desktop\\Miningdata\\datasets\\cereal.csv", header = T)
name_and_Rating_columns = c(1, 16);
data_set = data_set[, -name_and_Rating_columns] #Omit name & rating
data_set = na.omit(data_set)

data_set$mfr = factor(data_set$mfr);
levels(data_set$mfr)
table(data_set$mfr)
newMfr = class.ind(data_set$mfr);
last_One_Hot = 20;
data_set = cbind(data_set, newMfr)[ , -c(1, last_One_Hot)];

data_set$type = factor(data_set$type)
levels(data_set$type)
table(data_set$type) ## Obviously, The type variable causes bias (C:74 , H:3)

type_column = 1;
data_set = data_set[, -c(type_column)]; # Omit type (bias)

newdataSet = scale(data_set) ## Standardize 

kc = kmeans(newdataSet , centers = 4)
clusplot( newdataSet , kc$cluster , color = TRUE , shade = TRUE  )

kc$centers
