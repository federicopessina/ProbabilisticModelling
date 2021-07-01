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

## drop the null values
colSums(is.na(data))

missing_num_major_vessels_indeces <- which(data$num_major_vessels %in% 4)
missing_thal_indeces <-which(data$thalassemia %in% 0)
missing_values_indeces <- c(missing_num_major_vessels_indeces, missing_thal_indeces)
data <- data[-missing_values_indeces, ]

## transform categorical variable to R factors
data$sex <- as.factor(data$sex)
data$chest_pain <- as.factor(data$chest_pain)
data$blood_sugar <- as.factor(data$blood_sugar)
data$rest_cardio <- as.factor(data$rest_cardio)
data$angina_pain <- as.factor(data$angina_pain)
data$slope <- as.factor(data$slope)
data$thalassemia <- as.factor(data$thalassemia)
data$hearth_disease <- as.factor(data$hearth_disease)

## give a better name to the factor values for the graphs
levels(data$sex) <- c("Female", "Male")
levels(data$chest_pain) <- c("Asymptomatic", "Atypical angina", "No angina", "Typical angina")
levels(data$blood_sugar) <- c("No", "Yes")
levels(data$rest_cardio) <- c("Hypertrophy", "Normal", "Abnormalities")
levels(data$angina_pain) <- c("No", "Yes")
levels(data$slope) <- c("Descending", "Flat", "Ascending")
levels(data$thalassemia) <- c("Fixed defect", "Normal flow", "Reversible defect")
levels(data$hearth_disease) <- c("Yes", "No")

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
            ncol = 2, nrow = 4)

###############################################################

# Train/Test Split

set.seed(110)
training_indeces <- createDataPartition(data$target, p = .7, list = FALSE)
data.train <- data[ training_indeces,]
data.test  <- data[-training_indeces,]

###############################################################

# Model

library(caret)
library(e1071)

## 10 fold Cross-validation

fitControl <- trainControl(method="cv", number=10)

## Logistic Regression
library(corrplot)
library(tidyverse)
library(psych)
library(reshape2)
library(ggthemes)
library(gridExtra)
library(caret)

set.seed(888)

fit <- glm(hearth_disease ~., data, family = "binomial")
p <- predict(fit, newdata = data, type = "response")
pred <- ifelse(p > 0.5, 1, 0)
tab <- table(Predicted = pred, Actual = data$hearth_disease)
c <- as.data.frame(fit$coefficients)
c$name <- rownames(c)
colnames(c)[1] <- "coef"
c$odds <- exp(c$coef)

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

## Naive Bayes

model.nb <- train(form = target ~ ., 
                  data = data.train,
                  method = "naive_bayes",
                  trControl = fitControl)
model.nb

plot(model.nb)

## Tree-Augmented Naive Bayes 

#TODO

#model.tan <- train(form = target ~ ., 
#                   data = data.train,
#                   method = "tan",
#                   #metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
#                   #maximize = ifelse(metric %in% c("RMSE", "logLoss", "MAE"), FALSE, TRUE),
#                   trControl = fitControl,
#                   na.action = na.fail)

#model.tan

#plot(model.tan)

library(bnlearn)

tan = tree.bayes(data.test, "A")
fitted = bn.fit(tan, learning.test, method = "bayes")
pred = predict(fitted, learning.test)
table(pred, learning.test[, "A"])
