getwd()
## Diagnosing breast cancer with kNN algorithm
# Step 1 : Collecting the data from UCI Machine Learning Repository
df <- read.csv('wisc_bc_data.csv', stringsAsFactors = FALSE)
## now we would understand the basic structure of the data using str function
str(df)
View(df)
## the first variable ID, provides a unique indentifier number
## for each patient, thus it will be removed
df <- df[-1]
## checking the head of the dataset
head(df)
## we can see ID column is removed
# we would examone the next variable, i.e., diagnosis
# as here we are trying to predict the outcome of diagnosis
# the feature indicates whether the mass is benign or malingnant
# using table function, we can see 
## 357 masses are benign and 212 are malignant
table(df$diagnosis)
## R machine learning classifiers require the target feature
## to be coded as a factor
df$diagnosis <- factor(df$diagnosis, levels = c("B", "M"),
                       labels = c("Benign", "Malingnant"))
round(prop.table(table(df$diagnosis))*100, digits = 1)
#in the console we could see, 
# values are labeled as 'Benign' and 'Malingnant'
# remainin 30 variable are all numeric
# consist of 3 different meausrements of 10 characteristics
summary(df[c("radius_mean", "area_mean", "smoothness_mean")])
#as kNN's distance calculation is heavily 
# dependent upon measurement scale of input features
# normalisation is needed to be applied
# for normalisation, we need to create a function
# this function will take a vector of x,
# and for each value of x, subtract the minimum value of x
# and divide by the range of values of x

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#testing the normalise fuction
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))
df_n <- as.data.frame(lapply(df[2:31], normalize))
summary(df_n$area_mean)
# spilliting the data
df_train <- df_n[1:469,]
df_test <- df_n
# In the training  & test dataset, the target variable 'diagnosis'
# is excluded. for training the kNN model, 
#we will need to store these class labels in factor vectors

df_train_labels <- df[1:469, 1]
df_test_labels <- df[470:569, 1]
#install class package

#Training and classification using the knn() function 
#is performed in a single function call using four parameters

df_pred <- knn(train = df_train, test = df_test,
               cl = df_train_labels, k = 21)


# Evaluating Model Performance
install.packages('gmodels')
library(gmodels)
CrossTable(x = df_test_labels, y = df_pred,
           prop.chisq = FALSE)
#from here we could see, we have achieved 98% accuracy!
mean(df_test_labels != df_pred)*100
#elbow method
df_pred = NULL
error.rate <- NULL
for(i in 1:50){
  set.seed(101)
  df_pred = knn(train = df_train, test = df_test,
                cl = df_train_labels, k = i)
  error.rate[i] = mean(df_test_labels != df_pred)
}

library(ggplot2)
k_values <- 1:50
error.df <- data.frame(error.rate, k_values)
error.df

ggplot(error.df,aes(x=k_values,y=error.rate)) + geom_point()+ geom_line(lty="dotted",color='red')


