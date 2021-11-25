
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

data <- read.csv(url, header=FALSE)

head(data) # We see the data but with no column names

# Column names

colnames(data) <- c("age","sex","cp","trestbps","chol","fbs","restecg",
                    "thalach","exang","oldpeak","slope","ca","thal","hd")
head(data) # Now column names are successfully added

str(data)
#structure of this data is not clean, sex is shown as a number, instead it should
#be a factor. 'cp' (chest pain) is also supposed to be a factor with 1 through 3
#showing different level of pains, and 4 meaning no pain. 
#Also any missing data should be replaced by NA

##Cleaning the data##
data[data == "?"] <- NA

## Making '0' as females and '1' as male
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"

##Converting to factors
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp) # chest pain
# 1 = typical angina,
# 2 = atypical angina,
# 3 = non-anginal pain,
# 4 = asymptomatic
data$fbs <- as.factor(data$fbs) # fasting blood sugar if less than 120 mg/dl, 1 = TRUE, 0 = FALSE
data$restecg <- as.factor(data$restecg) # resting electrocardiographic results
data$exang <- as.factor(data$exang) # exercise induced angina, 1 = yes, 0 = no
data$slope <- as.factor(data$slope) # the slope of the peak exercise ST segment

# the thal and ca columns are characters because it had a "?" in it, R is assuming it is a string
# Hence converting them to integer first and then factor

data$ca <- as.integer(data$ca)
data$ca <- as.factor(data$ca)
data$thal <- as.integer(data$thal)
data$thal <- as.factor(data$thal)

data$hd <- ifelse(test = data$hd == 0, yes = "Healthy", no = "Unhealthy")
data$hd <- as.factor(data$hd)

str(data) # Now all variables which should show factors is showing factors

##Checking how many 'NAs' we have

sum(is.na(data))

# Checking which columns have 'NA' in them
contains_any_na = sapply(data, function(x) any(is.na(x)))
contains_any_na      

# We can see that thal and ca have NAs in them
# Now we need to see if removing these rows will affect our data or not

data[is.na(data$ca) | is.na(data$thal),]

## there are 303 rows in total, and if we remove 6 rows data won't be afftected that much, hence removing NAs

data <- data[!(is.na(data$ca) | is.na(data$thal)),]
data

## Exploratory data analysis ##

## We need to check whether heart disease (hd) are affecting both male and females
## If it is only affecting males, we can remove all the females and vice versa

xtabs(~ hd + sex, data = data)

## We can clearly see both the genders are being affected by heart diseases

## checking if chest pain was reported by all the patients; healthy or unhealthy.

xtabs(~hd + cp, data = data)

#We can clearly see chest pain were reported by all the patients

## We will check all the data which are factors to see if all of them are being affected by all patients

xtabs(~hd + fbs, data = data)
xtabs(~hd + restecg, data = data)
# Here we can see that rest ecg of '1' has only 4 patients, this could not help us find the best fitting line
# For the time being leaving it, if the model is not accurate will remove it
xtabs(~hd + exang, data = data)
xtabs(~hd + slope, data = data)
xtabs(~hd + ca, data = data)
xtabs(~hd + thal, data = data)

## making a simple logistic regression model ##
logistic <- glm(hd ~ sex, data = data, family = "binomial")
summary(logistic)

# the deviance residuals are close to zeroes, which is a good sign
# the coefficient give us the following equation - heart disease = -1.0438 + 1.2737 x the sex of the patient
# if the sex is female the equation will be:
# heart disease = -1.0438 + 1.2737 x 0
# heart disease = -1.0438
# Hence the log odds of a patients being female is -1.0438

# The p-values are lower than 0.05, hence the log(odds) and log(odds ratio) are statistically significant
# AIC - Akaike Information Criterion is used to compare one model to another

# All the variables

logistic <- glm(hd ~. , data = data, family = "binomial")
summary(logistic)

# calculation McFadden Pseudo R2

ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2
(ll.null - ll.proposed)/ll.null

# Psuedo R2 is 0.55. This can be interpreted as overall effect size

# calculating p-value for the above r-squared using chi-square
1 - pchisq(2*(ll.proposed-ll.null), df =(length(logistic$coefficients)-1))

# p value is extremely tiny hence it is statistically 

## making the graph ##
library(ggplot2)
library(cowplot)

## We create a data frame which contains only the probabilities of having heart disease along
## with actual heart disease status
predict.data <- data.frame(
  probability.of.hd = logistic$fitted.values,
  hd = data$hd)

## Sorting the data frame from low probability to high probability
predict.data <- predict.data[
  order(predict.data$probability.of.hd, decreasing = FALSE),]

## Assigning rank

predict.data$rank <- 1:nrow(predict.data)

## Graph ##
ggplot(data = predict.data, aes(x = rank, y = probability.of.hd)) +
  geom_point(aes(color = hd), alpha = 1, shape = 4, stroke = 2) + 
  xlab("Index") +
  ylab("Predicted probability of heart disease")

## saving as pdf ##

ggsave("predicted heart disease.pdf")
