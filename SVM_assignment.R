
salaryData_Train <- read.csv(file.choose())
salaryData_Test <- read.csv(file.choose())

head(salaryData_Train)
head(salaryData_Test)

dim(salaryData_Train)
str(salaryData_Train)
dim(salaryData_Test)

library(Hmisc)

describe(salaryData_Train)

newdata <- rbind(salaryData_Train,salaryData_Test)

nrow(newdata)-(sum(complete.cases(newdata)))
table(newdata$Salary)
prop.table(table(newdata$Salary))

barplot(table(newdata$workclass))
barplot(table(newdata$education))
barplot(table(newdata$educationno))
table(newdata$education,newdata$educationno)
barplot(table(newdata$maritalstatus))
barplot(table(newdata$occupation))
barplot(table(newdata$relationship))
barplot(table(newdata$race))
barplot(table(newdata$sex))
barplot(table(newdata$native))
table(newdata$native)

describe(newdata$educationno)
hist(newdata$educationno,breaks=c(1,5,10,15,20))

describe(newdata$capitalgain)
barplot(table(newdata$capitalgain))

describe(newdata$capitalloss)
barplot(table(newdata$capitalloss))

barplot(table(newdata$Salary))

###Perform PCA for EDA

dim(newdata)

install.packages("PCAmixdata")
library(PCAmixdata)

split <- splitmix(newdata)

str(split)

head(split$X.quanti)
head(split$X.quali)

X1 <- split$X.quanti
X2 <- split$X.quali

pcamix_sal <- PCAmix(X.quanti=X1,X.quali = X2,rename.level = T,graph = F) 

summary(pcamix_sal)

plot(pcamix_sal,choice = "ind",coloring.ind = X2$Salary,label=F,
     posleg = "bottomright",main="Observations")

plot(pcamix_sal,choice = "cor",main= "Numerical variables")

#Corelation plot for numerical variables indicates that the Capital gain ,Capital loss,number of hours work per week, Age and education level are positively corelated with each other

plot(pcamix_sal,choice = "sqload",coloring.var = T,leg = T,posleg = "topright",main = "All variables")

#Plot shows that the features Capital gain,capital loss,salary,age,hours per week,race are linked with 1st PC


####SVM works on numerical variables. Since data contains categorical variables , we will have to create the dummy variables . So for each variable we create dummy variables of the number of levels

salaryData_Train$educationno <- NULL
head(salaryData_Train)

#Creating dummy variables for categorical variables excl numeric ones
dummies <- dummyVars(~.,data=salaryData_Train[,-c(1,9,10,11,13)])

c2 <- predict(dummies,salaryData_Train[,-c(1,9,10,11,13)])

dim(c2)

# Combining numeric and dummy variables
sal_training <- as.data.frame(cbind(salaryData_Train[,c(1,9,10,11,13)],c2))

dim(sal_training)
head(sal_training)

salaryData_Test$educationno <- NULL
head(salaryData_Test)

dummies <- dummyVars(~.,data=salaryData_Test[,-c(1,9,10,11,13)])

D2 <- predict(dummies,salaryData_Test[,-c(1,9,10,11,13)])

sal_test <- as.data.frame(cbind(salaryData_Test[,c(1,9,10,11,13)],D2))

dim(sal_test)

#build SVM algorithm
library(kernlab)

Salary_classifier <- ksvm(Salary~.,data=sal_training,kernel = "rbfdot")
salary_predictions <- predict(Salary_classifier,sal_test)

head(salary_predictions)

agreement_rbf <- salary_predictions == sal_test$Salary

table(agreement_rbf)
prop.table(table(agreement_rbf))

#SVM model gives 84% accuracy
##########################################################################################

forestfires_data <- read.csv(file.choose())

head(forestfires_data)
dim(forestfires_data)

table(forestfires_data$month)

table(forestfires_data$month,forestfires_data$size_category)
table(forestfires_data$day,forestfires_data$size_category)

table(forestfires_data$month,forestfires_data$day,forestfires_data$size_category)

table(forestfires_data$FFMC,forestfires_data$size_category)
barplot(table(forestfires_data$FFMC))

table(forestfires_data$DMC,forestfires_data$size_category)
barplot(table(forestfires_data$DMC))

table(forestfires_data$DC,forestfires_data$size_category)
barplot(table(forestfires_data$DC))

table(forestfires_data$ISI,forestfires_data$size_category)
barplot(table(forestfires_data$ISI))

table(forestfires_data$temp,forestfires_data$size_category)
barplot(table(forestfires_data$temp))

table(forestfires_data$RH,forestfires_data$size_category)
barplot(table(forestfires_data$RH))

table(forestfires_data$wind,forestfires_data$size_category)

table(forestfires_data$rain,forestfires_data$size_category)

table(forestfires_data$area,forestfires_data$size_category)

prop.table(table(forestfires_data$size_category))
#EDA shows month,rain,wind, area,temp have a strong correlation with Size category-burnt area of the forest

#Removing columns month and day since the dummy variables are created for the same

forest_newdata <- forestfires_data[,-c(1,2)]
dim(forest_newdata)

library(caret)
intraininglocal <- createDataPartition(forest_newdata$size_category,p=0.7,list = F)

forest_training <- forest_newdata[intraininglocal,]
forest_Testing <- forest_newdata[-intraininglocal,]

dim(forest_training)
dim(forest_Testing)


head(forest_training)
head(forest_Testing)

#build SVM algorithm
library(kernlab)

Forest_classifier <- ksvm(size_category~.,data=forest_training,kernel = "rbfdot")
forest_predictions <- predict(Forest_classifier,forest_Testing)

head(forest_predictions)

agreement_rbf <- forest_predictions == forest_Testing$size_category
table(agreement_rbf)
prop.table(table(agreement_rbf))

#Accuracy with SVM is 79%