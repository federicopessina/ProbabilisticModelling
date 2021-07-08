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

## give a better name to the factor values for the graphs
levels(data$sex) <- c("Female", "Male")
levels(data$chest_pain) <- c("Asymptomatic", "Atypical angina", "No angina", "Typical angina")
levels(data$blood_sugar) <- c("No", "Yes")
levels(data$rest_cardio) <- c("Hypertrophy", "Normal", "Abnormalities")
levels(data$angina_pain) <- c("No", "Yes")
levels(data$slope) <- c("Descending", "Flat", "Ascending")
levels(data$thalassemia) <- c("Fixed defect", "Normal flow", "Reversible defect")
levels(data$hearth_disease) <- c("Yes", "No")

###############################################################

data$age <- as.numeric(data$age)
data$sex <- as.numeric(data$sex)
data$chest_pain <- as.numeric(data$chest_pain)
data$blood_pressure <- as.numeric(data$blood_pressure)
data$cholesterol <- as.numeric(data$cholesterol)
data$blood_sugar <- as.numeric(data$blood_sugar)
data$rest_cardio <- as.numeric(data$rest_cardio)
data$max_cardio <- as.numeric(data$max_cardio)
data$angina_pain <- as.numeric(data$angina_pain)
data$ecg_depression <- as.numeric(data$ecg_depression)
data$slope <- as.numeric(data$slope)
data$num_major_vessels <- as.numeric(data$num_major_vessels)
data$thalassemia <- as.numeric(data$thalassemia)
data$hearth_disease <- as.numeric(data$hearth_disease)

###############################################################
# Discretize the data
M <- as.data.frame(data)

NUMBER_BREACKS = 5

list_M <- lapply(
  X = c('interval', 'quantile', 'hartemink'),
  FUN = function(method) discretize(
    data = M,
    method = method,
    breaks = NUMBER_BREACKS,
    ordered = TRUE
  )
)

list_M

names(list_M) <- c('interval', 'quantile', 'hartemink')

lapply(X = list_M, FUN = summary)

###############################################################

# Algos

v_algorithms <- c('pc.stable', 'gs', 
                  'iamb', 'fast.iamb', 'inter.iamb', 'iamb.fdr',
                  'mmpc', 'si.hiton.pc', 'hpc', 'hc', 'mmhc',
                  'tabu', 'rsmax2', 'h2pc',
                  'aracne', 'chow.liu')

list_bnlearn <- list()

for(j in v_algorithms) for(k in names(list_M)) try({
  list_bnlearn[[j]][[k]] <- do.call(
    what = j,
    args = list(x = list_M[[k]])
  )

  M_arcs <- arcs(list_bnlearn[[j]][[k]])

  for(l in 1:nrow(M_arcs)){
    list_bnlearn[[j]][[k]] <- set.arc(
    x = list_bnlearn[[j]][[k]],
    from = M_arcs[l, 1],
    to = M_arcs[l, 2],
    check.cycles = FALSE,
    check.illegal = FALSE
  )
    
  list_bnlearn[[j]][[k]] <- choose.direction(
    x = list_bnlearn[[j]][[k]],
    arc = M_arcs[l, ],
    data = list_M[[k]]
  )}
}, silent = TRUE)

###############################################################

# Scoring the algos

M_score <- matrix(
  data = NA,
  nrow = length(v_algorithms),
  ncol = length(list_M)
)

rownames(M_score) <- v_algorithms
colnames(M_score) <- names(list_M)

for(j in v_algorithms) for(k in names(list_M)) try({
 M_score[j,k] <- score(
   x = list_bnlearn[[j]][[k]],
   data = list_M[[k]],
   type = 'bic'
 ) 
})

for (j in rownames(M_score)) M_score <- M_score[ , order(M_score[j, ])]
for (j in colnames(M_score)) M_score <- M_score[order(M_score[, j]) , ]

M_score  

graphviz.plot(
  list_bnlearn[[nrow(M_score)]][[ncol(M_score)]]
)
