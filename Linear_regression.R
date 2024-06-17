# Data Pre processing

# Importing the data set
Ins = read.csv('insurance.csv')
View(Ins)

# removing duplicates
INS=Ins[!duplicated(Ins),]

# Taking care of missing data
Ins$age = ifelse(is.na(Ins$age),ave(Ins$age, FUN = function(x) mean(x, na.rm = TRUE)),Ins$age)
Ins$sex = ifelse(is.na(Ins$sex),ave(Ins$sex, FUN = function(x) mean(x, na.rm = TRUE)),Ins$sex)
Ins$bmi = ifelse(is.na(Ins$bmi), ave(Ins$bmi, FUN = function(x) mean(x, na.rm = TRUE)),Ins$bmi)
Ins$children = ifelse(is.na(Ins$children),ave(Ins$children, FUN = function(x) mean(x, na.rm = TRUE)),Ins$children)
Ins$smoker = ifelse(is.na(Ins$smoker),ave(Ins$smoker, FUN = function(x) mean(x, na.rm = TRUE)),Ins$smoker)
Ins$region = ifelse(is.na(Ins$region),ave(Ins$region, FUN = function(x) mean(x, na.rm = TRUE)),Ins$region)
Ins$charges = ifelse(is.na(Ins$charges),ave(Ins$charges, FUN = function(x) mean(x, na.rm = TRUE)),Ins$charges)
summary(Ins)
str(Ins)

# finding null values
colSums(is.na(Ins))

# Encoding categorical data
Ins$smoker = factor(Ins$smoker,levels = c('yes','no'),labels = c(1,0))
Ins$region = factor(Ins$region,levels = c('southeast', 'southwest','northeast','northwest'),labels = c(1,2,3,4))
Ins$sex = factor(Ins$sex,levels = c('female','male'),labels = c(1,0))
View(Ins)
summary(Ins)

# removing outliers
set.seed(123)
x = rnorm(1338)
boxplot(x)
x2 = x[!x %in% boxplot.stats(x)$out]
boxplot(x2)
length(x2)

# data visualization using ggplot
unique(Ins$age)
unique(Ins$sex)
unique(Ins$bmi)
unique(Ins$children)
unique(Ins$smoker)
unique(Ins$region)
unique(Ins$charges)
library(ggplot2)
ggplot(data=Ins) + geom_boxplot(aes(y=charges, x=smoker, fill=smoker)) + xlab("smoker") + ylab("charges")+ ggtitle('smoker vs charges')
ggplot(data=Ins) + geom_boxplot(aes(y=charges, x=region, fill=region)) + xlab('region') + ylab("charges") + ggtitle('region vs charges')
ggplot(data=Ins) + geom_boxplot(aes(y=charges, x=sex, fill=sex)) + xlab("sex") + ylab("charges")+ggtitle('sex vs charges')
ggplot(data=Ins) + geom_boxplot(aes(y=charges, x=age, fill=age)) + xlab("age") + ylab("charges")+ggtitle('age vs charges')
ggplot(data=Ins) + geom_boxplot(aes(y=charges, x=children, fill=children)) + xlab("children") + ylab("charges")+ggtitle('children vs charges')

# DATA EXPLORATION
# displays internal structure of the data set
str(Ins)
# summarize charges
summary(Ins$charges)
# histogram of charges
hist(Ins$charges)
# table of region
table(Ins$region)

# EXAMINING THE RELATIONSHIP AMONG FEATURES
cor(Ins$age,Ins$charges)
cor(Ins$bmi,Ins$charges)
cor(Ins$children,Ins$charges)
cor(Ins[c('age', 'bmi', 'children', 'charges')])

# Visualizing the relationship among features
pairs(Ins[c('age', 'bmi', 'children', 'charges')])

# Splitting the data set into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(Ins$charges, SplitRatio = 0.75)
training_set = subset(Ins, split == TRUE)
test_set = subset(Ins, split == FALSE)

# Fitting Multiple Linear Regression to the Training set
initialmodel = lm(formula = charges ~ .,data = training_set)
summary(initialmodel)

# Adding a higher order age term
Ins$age2 <- Ins$age^2
summary(Ins$age2)

# Adding an indicator for BMI
Ins$bmi30 = ifelse(Ins$bmi >= 30, 1, 0)
View(Ins)

# Partition the data again with the additional columns 
new_training_set = subset(Ins, split == TRUE)
new_test_set = subset(Ins, split == FALSE)

# Creating the final model
final_model = lm(charges ~ sex + bmi + children + region + age2 + bmi30*smoker,data = new_training_set)
summary(final_model)

# Predicting the test set results with the improved 
chargesPredicted = predict(final_model, newdata = new_test_set)
cor(chargesPredicted, new_test_set$charges)
plot(chargesPredicted, new_test_set$charges)
abline(a = 0, b = 1, col = 'red', lwd = 3, lty = 2)







