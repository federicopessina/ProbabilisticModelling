###############################################################

# Import dataset
data <- read.csv('heart.csv')
head(data)

###############################################################

# Data Exploration and Pre-Processing

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

## give a better name to the factor values for the graphs
levels(data$sex) <- c("Female", "Male")
levels(data$chest_pain) <- c("Asymptomatic", "Atypical angina", "No angina", "Typical angina")
levels(data$blood_sugar) <- c("No", "Yes")
levels(data$rest_cardio) <- c("Hypertrophy", "Normal", "Abnormalities")
levels(data$angina_pain) <- c("No", "Yes")
levels(data$slope) <- c("Descending", "Flat", "Ascending")
levels(data$thalassemia) <- c("Fixed defect", "Normal flow", "Reversible defect")
levels(data$hearth_disease) <- c("Yes", "No")

library(dplyr)

data <- data %>% 
  mutate(agegroup = case_when(age >= 0  & age <= 40 ~ '0-40',
                              age > 40  & age <= 50 ~ '41-50',
                              age > 50  & age <= 60 ~ '51-60',
                              age > 60  & age <= 70 ~ '61-70',
                              age > 70 ~ '71-100'))

data <- data %>% 
  mutate(agegroup = case_when(blood_pressure >= 0  & blood_pressure <= 100 ~ '0-100',
                              blood_pressure > 100  & blood_pressure <= 120 ~ '101-120',
                              blood_pressure > 120  & blood_pressure <= 140 ~ '121-140',
                              blood_pressure > 140  & blood_pressure <= 160 ~ '141-160',
                              blood_pressure > 160 ~ '161-200'))

data <- data %>% 
  mutate(agegroup = case_when(ecg_depression >= 0  & ecg_depression <= 0.5 ~ '0.00-0.50',
                              ecg_depression > 0.5  & ecg_depression <= 1 ~ '0.51-1.00',
                              ecg_depression > 1.0  & ecg_depression <= 1.5 ~ '1.01-1.50',
                              ecg_depression > 1.5  & ecg_depression <= 2 ~ '1.51-2.00',
                              ecg_depression > 2  & ecg_depression <= 2.5 ~ '2.01-2.50',
                              ecg_depression > 2.5  & ecg_depression <= 3 ~ '2.51-3.00',
                              ecg_depression > 3  & ecg_depression <= 3.5 ~ '3.00-3.50',
                              ecg_depression > 3.5  & ecg_depression <= 4 ~ '3.50-4.00',
                              ecg_depression > 4 ~ '4.00-8.00'))

data <- data %>% 
  mutate(agegroup = case_when(cholesterol >= 0  & cholesterol <= 20 ~ '0-20',
                              cholesterol > 20  & cholesterol <= 40 ~ '20-40',
                              cholesterol > 40  & cholesterol <= 60 ~ '40-60',
                              cholesterol > 60  & cholesterol <= 80 ~ '60-80',
                              cholesterol > 80  & cholesterol <= 100 ~ '80-100',
                              cholesterol > 100  & cholesterol <= 120 ~ '100-120',
                              cholesterol > 120  & cholesterol <= 140 ~ '120-140',
                              cholesterol > 140 ~ '140-160'))

data <- data %>% 
  mutate(agegroup = case_when(max_cardio >= 0  & max_cardio <= 100 ~ '0-100',
                              max_cardio > 100  & max_cardio <= 120 ~ '100-120',
                              max_cardio > 120  & max_cardio <= 140 ~ '120-140',
                              max_cardio > 140  & max_cardio <= 160 ~ '140-160',
                              max_cardio > 160  & max_cardio <= 180 ~ '160-180',
                              max_cardio > 180 ~ '180-220'))



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

library(ggpubr) # take a look at the distribution prior

ggplot(data, aes(data$age)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

ggplot(data, aes(data$blood_pressure)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

ggplot(data, aes(data$ecg_depression)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

ggplot(data, aes(data$cholesterol)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

ggplot(data, aes(data$max_cardio)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

ggplot(data, aes(data$num_major_vessels)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

ggplot(data, aes(data$agegroup)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

ggplot(data, aes(age, colour = hearth_disease)) +
  geom_freqpoly(binwidth = 1) + labs(title="Age Distribution by Outcome")

ggplot(data, aes(x = age, fill = hearth_disease, color = hearth_disease)) +
  geom_histogram(binwidth = 1) + labs(title = "Age Distribution by Outcome")
  c + theme_bw()

library(GGally)
ggpairs(data,
        title = "Pairs Graph")

## transform categorical variable to R factors
data$sex <- as.factor(data$sex)
data$chest_pain <- as.factor(data$chest_pain)
data$blood_sugar <- as.factor(data$blood_sugar)
data$rest_cardio <- as.factor(data$rest_cardio)
data$angina_pain <- as.factor(data$angina_pain)
data$slope <- as.factor(data$slope)
data$thalassemia <- as.factor(data$thalassemia)
data$hearth_disease <- as.factor(data$hearth_disease)

data$age <- as.factor(data$age)
data$blood_pressure <- as.factor(data$blood_pressure)
data$ecg_depression <- as.factor(data$ecg_depression)
data$cholesterol <- as.factor(data$cholesterol)
data$max_cardio <- as.factor(data$max_cardio)
data$num_major_vessels <- as.factor(data$num_major_vessels)
data$agegroup <- as.factor(data$agegroup)


# Graphical Exploration

library(ggplot2)

## bar-chart number of patients with and without disease

plot1 <- ggplot(data, aes(target, fill=target)) + 
  geom_bar() +
  labs(x="Disease", y="Number of patients") +
  guides(fill=FALSE)

## bar-chart number of patients by age

plot2 <- ggplot(data, aes(age, fill=target)) + 
  geom_histogram(binwidth=1) +
  labs(fill="Disease", x="Age", y="Number of patients")

## bar-chart number of patients by sex with or without disease

plot3 <- ggplot(data, aes(sex, fill=target)) + 
  geom_bar() +
  labs(fill="Disease", x="Sex", y="Number of patients")

## bar-chart number of patients by chest pain with or without disease

plot4 <- ggplot(data, aes(cp, fill=target)) +
  geom_bar() +
  labs(fill="Disease", x="Chest pain type", y="Number of patients")

## bar-chart number of patients by blood pressure with or without disease

plot5 <- ggplot(data, aes(trestbps, fill=target)) +
  geom_histogram(binwidth=3) +
  labs(fill="Disease", x="Blood pressure (mm Hg)", y="Number of patients")

## bar-chart number of patients by sugar level with or without disease

plot6 <- ggplot(data, aes(fbs, fill=target)) +
  geom_bar() +
  labs(fill="Disease", x="Sugar level > 120 mg/dl", y="Number of patients")

## bar-chart number of patients by electrocardiogram with or without disease

plot7 <- ggplot(data, aes(restecg, fill=target)) +
  geom_bar() +
  labs(fill="Disease", x="Electrocardiogram on rest", y="Number of patients")

## bar-chart number of patients by heart rate during exercise with or without disease

plot8 <- ggplot(data, aes(thalach, fill=target)) +
  geom_histogram(binwidth=10) +
  labs(fill="Disease", x="Maximum heart rate during exercise", y="Number of patients")

## multiplot
library(ggpubr)

ggarrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8 + 
            rremove("x.text"), 
            #labels = c("A", "B"),
            ncol = 2, nrow = 4) #FIXME

### plot factors indicating hearth disease

options(repr.plot.width=14, repr.plot.height=14)

c %>% filter(name!="(Intercept)" & name!="predict") %>%  ggplot(aes(reorder(name,odds),odds)) + 
  geom_bar(stat = "identity") + 
  geom_label(aes(label=round(odds,2), size=8)) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text=element_text(size=12), plot.subtitle = element_text(size=12), plot.caption = element_text(size=12)) +
  geom_hline(yintercept = 1, color="red", linetype="dashed") +
  xlab('Age') + 
  ylab('Satisfaction Probability') + 
  labs(title = 'Factors Affecting Heart Disease (Odds Ratio)', subtitle = "factors with odds ratio grater than 1 indicate heart disease", caption = "**interpretation: a 1 unit increase in 'cp' increases the odds of heart disease by a factor of 2.36**")

### predict hearth disease probability for chest_pain, ecg and slope

options(repr.plot.width = 10, repr.plot.height = 3)

a <- ggplot(data, aes(cp, target)) + 
  stat_smooth(method="glm", method.args = list(family = "binomial"), se=F) + 
  theme_fivethirtyeight() +
  scale_y_continuous(limits=c(0,1)) +
  theme(axis.text=element_text(size=12), plot.subtitle = element_text(size=12), plot.caption = element_text(size=12)) +
  labs(title = 'Effect of CP on Heart Disease (%)', subtitle="x: cp, y: heart disease probability")

b <- ggplot(data, aes(slope, target)) + 
  stat_smooth(method="glm", method.args = list(family = "binomial"), se=F) + 
  theme_fivethirtyeight() +
  scale_y_continuous(limits=c(0,1)) +
  scale_x_continuous(breaks=seq(0, 2, 1)) +
  theme(axis.text=element_text(size=12), plot.subtitle = element_text(size=12), plot.caption = element_text(size=12)) +
  labs(title = 'Effect of Slope on Heart Disease (%)', subtitle="x: slope, y: heart disease probability")

c <- ggplot(data, aes(restecg, target)) + 
  stat_smooth(method="glm", method.args = list(family = "binomial"), se=F) + 
  theme_fivethirtyeight() +
  scale_y_continuous(limits=c(0,1)) +
  scale_x_continuous(breaks=seq(0, 2, 1)) +
  theme(axis.text=element_text(size=12), plot.subtitle = element_text(size=12), plot.caption = element_text(size=12)) +
  labs(title = 'Effect of ECG on Heart Disease (%)', subtitle="x: restecg, y: heart disease probability")

grid.arrange(a, b, c, nrow = 1) #FIXME

###############################################################
library(bnlearn)

bn_data <- as.data.frame(data)

#bn_data_example <- as.data.frame(bn_data[, c("age", "sex", "chest_pain",
#                                             "blood_pressure", "cholesterol",
#                                             "blood_sugar", "rest_cardio",
#                                             "max_cardio", "angina_pain",
#                                             "ecg_depression", "slope",
#                                             "num_major_vessels", "thalassemia",
#                                             "hearth_disease", "agegroup")])

res <- hc(bn_data)

plot(res)

###############################################################

# Train/Test Split

set.seed(888)

training_indeces <- createDataPartition(y = data$hearth_disease, 
                                        p = 0.7, 
                                        list = FALSE)
data.train <- data[ training_indeces,]
data.test  <- data[-training_indeces,]

prop.table(table(data.train$hearth_disease)) * 100 # check balance
prop.table(table(data.test$hearth_disease)) * 100

###############################################################

# Model

library(caret)
library(e1071)
library(corrplot)
library(tidyverse)
library(psych)
library(reshape2)
library(ggthemes)
library(gridExtra)

## 10 fold Cross-validation

fitControl <- trainControl(method = "cv", number = 10)

###############################################################

## Logistic Regression

set.seed(888)

fit <- glm(hearth_disease ~., data, family = "binomial")
p <- predict(fit, newdata = data, type = "response")
pred <- ifelse(p > 0.5, 1, 0)
tab <- table(Predicted = pred, Actual = data$hearth_disease)
c <- as.data.frame(fit$coefficients)
c$name <- rownames(c)
colnames(c)[1] <- "coef"
c$odds <- exp(c$coef)

###############################################################

## Naive Bayes

## create and train

model.nb <- train(form = hearth_disease ~ ., # formula
                  data = data.train,
                  method = "naive_bayes",
                  trControl = fitControl) # 10 cv

model.nb # model infos

### evaluate

predict.nb <- predict(model.nb,
                   newdata = data.test)

confusion_matrix.nb <- 
  confusionMatrix(predict.nb, data.test$hearth_disease)

plot(model.nb)      # distribution
confusion_matrix.nb # results


var_imp.nb <- varImp(model.nb) # variable importance
plot(var_imp.nb)

###############################################################

## Tree-Augmented Naive Bayes 

#FIXME

model.tan <- train(form = hearth_disease ~ ., 
                   data = data.train,
                   method = "tan",
                   metric = ifelse(is.factor(bn_data$hearth_disease), "Accuracy", "RMSE"),
                   maximize = ifelse(metric %in% c("RMSE", "logLoss", "MAE"), FALSE, TRUE),
                   trControl = fitControl,
                   na.action = na.fail)

model.tan

plot(model.tan)

library(bnlearn)

tan = tree.bayes(data.test, "A")
fitted = bn.fit(tan, learning.test, method = "bayes")
pred = predict(fitted, learning.test)
table(pred, learning.test[, "A"])
