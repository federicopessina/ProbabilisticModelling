###############################################################

# Import dataset

data <- read.csv('heart.csv')
head(data)

###############################################################

# Data Exploration and Pre-Processing

## general summary
summary(data)

## rename variable mispelled
library(data.table)
setnames(data, "ï..age", "age")

## drop the null values
missing_ca_indeces <- which(data$ca %in% 4)
missing_thal_indeces <-which(data$thal %in% 0)
missing_values_indeces <- c(missing_ca_indeces, missing_thal_indeces)
data <- data[-missing_values_indeces, ]

## transform categorical variable to R factors
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$thal <- as.factor(data$thal)
data$target <- as.factor(data$target)

## give a better name to the factor values for the graphs
levels(data$sex) <- c("Female", "Male")
levels(data$cp) <- c("Asymptomatic", "Atypical angina", "No angina", "Typical angina")
levels(data$fbs) <- c("No", "Yes")
levels(data$restecg) <- c("Hypertrophy", "Normal", "Abnormalities")
levels(data$exang) <- c("No", "Yes")
levels(data$slope) <- c("Descending", "Flat", "Ascending")
levels(data$thal) <- c("Fixed defect", "Normal flow", "Reversible defect")
levels(data$target) <- c("Yes", "No")

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

set.seed(000)
training_indeces <- createDataPartition(data$target, p = .7, list = FALSE)
data.train <- data[ training_indeces,]
data.test  <- data[-training_indeces,]

###############################################################

