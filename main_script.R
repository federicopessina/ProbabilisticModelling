###############################################################

# Import dataset

data <- read.csv('heart.csv')
head(data)

###############################################################

#Pre-Processing

## general summary
dim(data)
summary(data)

library(psych)
describe(data)

## rename variable mispelled
library(data.table)
setnames(data, "ï..age", "age")
setnames(data, "cp", "chest_pain")
setnames(data, "trestbps", "blood_pressure")
setnames(data, "chol", "cholesterol")
setnames(data, "fbs", "blood_sugar")
setnames(data, "restecg", "rest_cardio")
setnames(data, "thalach", "max_cardio")
setnames(data, "exang", "angina_pain")
setnames(data, "oldpeak", "ecg_depression")
setnames(data, "ca", "num_major_vessels")
setnames(data, "thal", "thalassemia")
setnames(data, "target", "hearth_disease")


## drop the null values
colSums(is.na(data))
library(Amelia)
missmap(data)
data <- na.omit(data) # not necessary

missing_num_major_vessels_indeces <- which(data$num_major_vessels %in% 4)
missing_thal_indeces <-which(data$thalassemia %in% 0)
missing_values_indeces <- c(missing_num_major_vessels_indeces, missing_thal_indeces)
data <- data[-missing_values_indeces, ]


## transform numerical variables in categorical variable
library(ggpubr) # take a look at the distribution

ggplot(data, aes(age, colour = hearth_disease)) +
  geom_freqpoly(binwidth = 1) + 
  labs(title="Age Distribution")

ggplot(data, aes(data$sex)) +
  geom_bar() +
  theme_pubclean() +
  labs(title="Sex Distribution")

ggplot(data, aes(data$chest_pain)) +
  geom_bar() +
  theme_pubclean() +
  labs(title="Chest Pain Distribution")

ggplot(data, aes(data$blood_pressure)) +
  geom_freqpoly(binwidth = 1) +
  theme_pubclean() +
  labs(title="Blood Pressure Distribution")

ggplot(data, aes(data$rest_cardio)) +
  geom_freqpoly(binwidth = 1) +
  theme_pubclean() +
  labs(title="Rest Cardio Distribution")

ggplot(data, aes(data$max_cardio)) +
  geom_freqpoly(binwidth = 1) +
  theme_pubclean() +
  labs(title="Max Cardio Distribution")

ggplot(data, aes(data$angina_pain)) +
  geom_bar() +
  theme_pubclean() +
  labs(title="Angina Pain Distribution")


ggplot(data, aes(data$ecg_depression)) +
  geom_freqpoly(binwidth = 1) +
  theme_pubclean() +
  labs(title="ECG Depression Distribution")

ggplot(data, aes(data$num_major_vessels)) +
  geom_bar() +
  theme_pubclean() +
  labs(title="Vessel Distribution")

ggplot(data, aes(data$thalassemia)) +
  geom_bar() +
  theme_pubclean() +
  labs(title="Thalassemia Distribution")

ggplot(data, aes(data$hearth_disease)) +
  geom_bar() +
  theme_pubclean() +
  labs(title="Hearth Disease Distribution")


### variables pairs
library(GGally)
ggpairs(data,
        title = "Pairs Graph")

## give a better name to the factor values for the graphs

data$sex <- as.factor(data$sex)
data$chest_pain <- as.factor(data$chest_pain)
data$blood_sugar <- as.factor(data$blood_sugar)
data$rest_cardio <- as.factor(data$rest_cardio)
data$angina_pain <- as.factor(data$angina_pain)
data$slope <- as.factor(data$slope)
data$thalassemia <- as.factor(data$thalassemia)
data$hearth_disease <- as.factor(data$hearth_disease)

levels(data$sex) <- c("Female", "Male")
levels(data$chest_pain) <- c("Asymptomatic", "Atypical angina", "No angina", "Typical angina")
levels(data$blood_sugar) <- c("No", "Yes")
levels(data$rest_cardio) <- c("Hypertrophy", "Normal", "Abnormalities")
levels(data$angina_pain) <- c("No", "Yes")
levels(data$slope) <- c("Descending", "Flat", "Ascending")
levels(data$thalassemia) <- c("Fixed defect", "Normal flow", "Reversible defect")
levels(data$hearth_disease) <- c("Yes", "No")

## change continuous variables into cathegorical
library(dplyr)
data <- data %>% 
  mutate(age = case_when(age >= 0  & age <= 40 ~ '0-40',
                              age > 40  & age <= 50 ~ '41-50',
                              age > 50  & age <= 60 ~ '51-60',
                              age > 60  & age <= 70 ~ '61-70',
                              age > 70 ~ '71-100'))

data <- data %>% 
  mutate(blood_pressure = case_when(blood_pressure >= 0  & blood_pressure <= 100 ~ '0-100',
                              blood_pressure > 100  & blood_pressure <= 120 ~ '101-120',
                              blood_pressure > 120  & blood_pressure <= 140 ~ '121-140',
                              blood_pressure > 140  & blood_pressure <= 160 ~ '141-160',
                              blood_pressure > 160 ~ '161-200'))

data <- data %>% 
  mutate(ecg_depression = case_when(ecg_depression >= 0  & ecg_depression <= 0.5 ~ '0.00-0.50',
                              ecg_depression > 0.5  & ecg_depression <= 1 ~ '0.51-1.00',
                              ecg_depression > 1.0  & ecg_depression <= 1.5 ~ '1.01-1.50',
                              ecg_depression > 1.5  & ecg_depression <= 2 ~ '1.51-2.00',
                              ecg_depression > 2  & ecg_depression <= 2.5 ~ '2.01-2.50',
                              ecg_depression > 2.5  & ecg_depression <= 3 ~ '2.51-3.00',
                              ecg_depression > 3  & ecg_depression <= 3.5 ~ '3.00-3.50',
                              ecg_depression > 3.5  & ecg_depression <= 4 ~ '3.50-4.00',
                              ecg_depression > 4 ~ '4.00-8.00'))

data <- data %>% 
  mutate(cholesterol = case_when(cholesterol >= 0  & cholesterol <= 20 ~ '0-20',
                              cholesterol > 20  & cholesterol <= 40 ~ '20-40',
                              cholesterol > 40  & cholesterol <= 60 ~ '40-60',
                              cholesterol > 60  & cholesterol <= 80 ~ '60-80',
                              cholesterol > 80  & cholesterol <= 100 ~ '80-100',
                              cholesterol > 100  & cholesterol <= 120 ~ '100-120',
                              cholesterol > 120  & cholesterol <= 140 ~ '120-140',
                              cholesterol > 140 ~ '140-160'))

data <- data %>% 
  mutate(max_cardio = case_when(max_cardio >= 0  & max_cardio <= 100 ~ '0-100',
                              max_cardio > 100  & max_cardio <= 120 ~ '100-120',
                              max_cardio > 120  & max_cardio <= 140 ~ '120-140',
                              max_cardio > 140  & max_cardio <= 160 ~ '140-160',
                              max_cardio > 160  & max_cardio <= 180 ~ '160-180',
                              max_cardio > 180 ~ '180-220'))

## trasform into numeric

#data$age <- as.numeric(data$age)
#data$sex <- as.numeric(data$sex)
#data$chest_pain <- as.numeric(data$chest_pain)
#data$blood_pressure <- as.numeric(data$blood_pressure)
#data$cholesterol <- as.numeric(data$cholesterol)
#data$blood_sugar <- as.numeric(data$blood_sugar)
#data$rest_cardio <- as.numeric(data$rest_cardio)
#data$max_cardio <- as.numeric(data$max_cardio)
#data$angina_pain <- as.numeric(data$angina_pain)
#data$ecg_depression <- as.numeric(data$ecg_depression)
#data$slope <- as.numeric(data$slope)
#data$num_major_vessels <- as.numeric(data$num_major_vessels)
#data$thalassemia <- as.numeric(data$thalassemia)
#data$hearth_disease <- as.numeric(data$hearth_disease)

## transform categorical variable to R factors

data$age <- as.factor(data$age) #HERE
data$blood_pressure <- as.factor(data$blood_pressure)
data$ecg_depression <- as.factor(data$ecg_depression)
data$cholesterol <- as.factor(data$cholesterol)
data$max_cardio <- as.factor(data$max_cardio)
data$num_major_vessels <- as.factor(data$num_major_vessels)


###############################################################


# Train/Test Split
library(caret)

set.seed(888)

training_indeces <- createDataPartition(y = data$hearth_disease, 
                                        p = 0.7, 
                                        list = FALSE)
data.train <- data[ training_indeces,]
data.test  <- data[-training_indeces,]

prop.table(table(data.train$hearth_disease)) * 100 # check balance
prop.table(table(data.test$hearth_disease)) * 100 # check balance


## create and train
library(bnlearn)

bn_data.train <- as.data.frame(data.train)
bn_data.test <- as.data.frame(data.test)


res <- hc(bn_data.train)

plot(res)

###############################################################

# Model
library(e1071)
library(corrplot)
library(tidyverse)
library(psych)
library(reshape2)
library(ggthemes)
library(gridExtra)

###############################################################

## Cross-Validation

fitControl <- trainControl(method = "cv", # cross validation
                           number = 10)   # number of folds 

###############################################################

## Naive Bayes

model.nb <- train(form = hearth_disease ~ ., # formula
                  data = bn_data.train,
                  method = "naive_bayes",
                  trControl = fitControl) # 10 cv

model.nb # model infos

### evaluate

predict.nb <- predict(model.nb,
                   newdata = bn_data.test)

confusion_matrix.nb <- 
  confusionMatrix(predict.nb, bn_data.test$hearth_disease)

plot(model.nb)      # distribution
confusion_matrix.nb # results


var_imp.nb <- varImp(model.nb) # variable importance
plot(var_imp.nb)

confusion_matrix_tab.nb <- table(predict.nb, bn_data.test$hearth_disease)
accuracy.nb <- sum(diag(confusion_matrix_tab.nb))/sum(confusion_matrix_tab.nb)*100
###############################################################

## Tree-Augmented Naive Bayes 

explanatory_variables = c('age', 'sex', 'chest_pain', 
                          'blood_pressure', 'cholesterol',
                          'blood_sugar', 'rest_cardio',
                          'max_cardio', 'angina_pain',
                          'ecg_depression', 'slope',
                          'num_major_vessels', 'thalassemia',
                          'agegroup')

model.tan <- tree.bayes(x = bn_data.train, 
           training = 'hearth_disease', 
           explanatory = explanatory_variables, 
           whitelist = NULL, 
           blacklist = NULL,
           mi = 'mi', # alternative: 'mig' # estimator used for the mutual information coefficients
           root = NULL, 
           debug = FALSE)

model.tan

plot(model.tan)

### evaluate

predict.tan <- predict(model.tan,
                       data = bn_data.test)

confusion_matrix.tan <- 
  confusionMatrix(predict.nb, bn_data.test$hearth_disease)

plot(model.tan)      # distribution
confusion_matrix.tan # results

confusion_matrix_tab.tan <- table(predict.tan, bn_data.test$hearth_disease)
accuracy.tan <- sum(diag(confusion_matrix_tab.tan))/sum(confusion_matrix_tab.tan)*100

###############################################################

## Max-Min Hill Climbing (MMHC)

mmhc(x = bn_data.train, # dataframe w variible 
     whitelist = NULL, 
     blacklist = NULL, 
     restrict.args = list(),
     maximize.args = list(), 
     debug = FALSE)

###############################################################

## Aracne

model.aracne <- aracne(x = bn_data.train, 
                       whitelist = NULL, 
                       blacklist = NULL, 
                       mi = 'mi', 
                       debug = FALSE)

plot(model.aracne)

### evaluate

predict.tan <- predict(model.aracne,
                       data = bn_data.test)

confusion_matrix.aracne <- 
  confusionMatrix(predict.tan, bn_data.test$hearth_disease)
###############################################################

# Model Comparison

result_accuracy <- c(accuracy.nb, accuracy.tan)
result_names <- c("Naive Bayes", "Tree-Augmented Naive Bayes")
library(data.table)
result<-data.table(method = result_names,
                   mcc = result_accuracy)
library(ggplot2)
ggplot(result,aes(x = method, y = mcc)) + 
  geom_bar(stat = "identity") +
  labs(title = "Results by Accuracy values",
       y ="Algorithms")+
  coord_flip()
