###############################################################

# Import dataset

data <- read.csv('heart.csv')

data <- as.data.frame(data)

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

###############################################################

#Pre-Processing

## general summary
dim(data)
summary(data)

library(psych)
describe(data)

## drop the null values
colSums(is.na(data))
library(Amelia)
missmap(data)
data <- na.omit(data) # not necessary

missing_num_major_vessels_indeces <- which(data$num_major_vessels %in% 4)
missing_thal_indeces <-which(data$thalassemia %in% 0)
missing_values_indeces <- c(missing_num_major_vessels_indeces, missing_thal_indeces)
data <- data[-missing_values_indeces, ]

###############################################################

data.numeric <- data[, c('age', 'blood_pressure', 'cholesterol',
                         'max_cardio', 'ecg_depression')]

data.numeric$age <- as.numeric(data$age)
data.numeric$blood_pressure <- as.numeric(data$blood_pressure)
data.numeric$cholesterol <- as.numeric(data$cholesterol)
data.numeric$max_cardio <- as.numeric(data$max_cardio)
data.numeric$ecg_depression <- as.numeric(data$ecg_depression)


data.cathegorical <- data[, c('sex', 'chest_pain', 'blood_sugar',
                         'rest_cardio', 'angina_pain', 'slope',
                         'num_major_vessels', 'thalassemia',
                         'hearth_disease')]

###############################################################

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

###############################################################

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

###############################################################

## transform categorical variable to R factors

data$age <- as.factor(data$age) #HERE
data$blood_pressure <- as.factor(data$blood_pressure)


data$ecg_depression <- as.factor(data$ecg_depression)
data$cholesterol <- as.factor(data$cholesterol)
data$max_cardio <- as.factor(data$max_cardio)
data$num_major_vessels <- as.factor(data$num_major_vessels)

###############################################################


dag <- hc(data.train, debug = TRUE)
dag # infos bayesian network
plot(dag)


par(mfrow = c(2, 3), mar = c(4, 2, 2, 2))

# TODO change to numeric the variables to plot them
for (var in c('age', 'sex', 'chest_pain', 
              'blood_pressure', 'cholesterol',
              'blood_sugar', 'rest_cardio',
              'max_cardio', 'angina_pain',
              'ecg_depression', 'slope',
              'num_major_vessels', 'thalassemia',
              'agegroup')) {
  x = data[, var]
  hist(x, prob = TRUE, xlab = var, ylab = "", main = "", col = "ivory")
  lines(density(x), lwd = 2, col = "tomato")
  curve(dnorm(x, mean = mean(x), sd = sd(x)), from = min(x), to = max(x),
  add = TRUE, lwd = 2, col = "steelblue")
}

# use best variables
pairs(data.numeric[, setdiff(names(data.numeric), y = c('blood_pressure'))], # except
      upper.panel = function(x, y, ...) {
        points(x = x, y = y, col = "grey")
        abline(coef(lm(y ~ x)), col = "tomato", lwd = 2)
        },
      lower.panel = function(x, y, ...) {
        par(usr = c(0, 1, 0, 1))
        text(x = 0.5, y = 0.5, round(cor(x, y), 2), cex = 2)
        }
      )



library(gplots)
#diff.delta = sapply(diff[, 1:6], function(x) x / diff$dT)
rho = cor(data.frame(data.numeric))
palette.breaks = seq(0, 1, 0.1)
par(oma = c(2, 2, 2, 1))
heatmap.2(rho, scale = "none", trace = "none", revC = TRUE, breaks = palette.breaks)


ug = empty.graph(colnames(rho))
amat(ug) = (rho > 0.4) + 0L - diag(1L, nrow(rho))
graphviz.plot(ug, layout = "fdp", shape = "ellipse")


# blacklist
bl = tiers2blacklist(list("ecg_depression",
                          c('blood_sugar')))
bl = rbind(bl, c("ecg_depression", "max_cardio"))
bl

# whitelist
wl = matrix(c("chest_pain", "max_cardio"),
            ncol = 2, 
            byrow = TRUE, 
            dimnames = list(NULL, c("from", "to")))
wl




dag = hc(data.numeric, 
         whitelist = wl, 
         blacklist = bl,
         debug = TRUE)
dag

graphviz.plot(dag, shape = "ellipse", highlight = list(arcs = wl))

str.diff = boot.strength(data.numeric, 
                         R = 200, 
                         algorithm = "hc",
                         algorithm.args = list(whitelist = wl, 
                                               blacklist = bl)
                         )
head(str.diff)


attr(str.diff, "threshold")

avg.diff = averaged.network(str.diff)

strength.plot(avg.diff, 
              str.diff, 
              shape = "ellipse", 
              highlight = list(arcs = wl)
              )



par(mfrow = c(1, 2))
graphviz.compare(avg.diff, 
                 dag, 
                 shape = "ellipse", 
                 main = c("averaged DAG", "single DAG")
                 )

compare(avg.diff, dag)

compare(avg.diff, dag, arcs = TRUE)


undirected.arcs(cpdag(dag, wlbl = TRUE))

avg.diff$learning$whitelist = wl
avg.diff$learning$blacklist = bl
undirected.arcs(cpdag(avg.diff, wlbl = TRUE))

compare(cpdag(avg.diff, wlbl = TRUE), cpdag(dag, wlbl = TRUE))

plot(str.diff)
abline(v = 0.75, col = "tomato", lty = 2, lwd = 2)
abline(v = 0.85, col = "steelblue", lty = 2, lwd = 2)

nrow(str.diff[str.diff$strength > attr(str.diff, "threshold") &
                str.diff$direction > 0.5, ])

nrow(str.diff[str.diff$strength > 0.75 & str.diff$direction > 0.5, ])

nrow(str.diff[str.diff$strength > 0.85 & str.diff$direction > 0.5, ])


avg.simpler = averaged.network(str.diff, threshold = 0.85)

strength.plot(avg.simpler, 
              str.diff, 
              shape = "ellipse", 
              highlight = list(arcs = wl))

fitted.simpler = bn.fit(avg.simpler, data.numeric)
fitted.simpler

fitted.simpler$age # parameter example

summary(lm(max_cardio ~ cholesterol + blood_pressure, data = data.numeric))


# model validation

xval = bn.cv(data.numeric, bn = "hc", 
             algorithm.args = list(blacklist = bl, 
                                   whitelist = wl),
             loss = "cor-lw", 
             loss.args = list(target = "age", 
                              n = 200), 
             runs = 10)

err = numeric(10)

for (i in 1:10) {
  tt = table(unlist(sapply(xval[[i]], '[[', "observed")),
             unlist(sapply(xval[[i]], '[[', "predicted")) > 0.50)
  err[i] = (sum(tt) - sum(diag(tt))) / sum(tt)
}

summary(err)


predcor = structure(numeric(2),
                    names = c("age", "cholesterol"))



for (var in names(predcor)) {
  xval = bn.cv(data.numeric, bn = "hc", 
               algorithm.args = list(blacklist = bl, 
                                     whitelist = wl),
               loss = "cor-lw", 
               loss.args = list(target = var, 
                                n = 200), 
               runs = 10)
  predcor[var] = mean(sapply(xval, function(x) attr(x, "mean")))
}

round(predcor, digits = 3)


mean(predcor)
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
library(e1071)
library(corrplot)
library(tidyverse)
library(psych)
library(reshape2)
library(ggthemes)
library(gridExtra)

str(data)

#bn_data.train <- as.data.frame(data.train)
#bn_data.test <- as.data.frame(data.test)

fitted = bn.fit(dag, data = data)

graphviz.plot(dag, shape = "ellipse") # , highlight = list(arcs = wl

newdata = data.frame(hearth_disease = factor('Yes', levels = hearth_disease.lv))

predict(data, 
        node = "heart_disease", 
        data = bn_data.train)

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
accuracy.nb
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

model.aracne # model infos

plot(model.aracne)

predict.aracne <- predict(model.aracne,
                      newdata = bn_data.test)

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
