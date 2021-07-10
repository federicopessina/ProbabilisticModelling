data <- read.csv('heart.csv')

data <- as.data.frame(data)

###############################################################

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
#dim(data)
#summary(data)

#library(psych)
#describe(data)

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
data.factor <- data

levels(data.factor$sex) <- c("Female", "Male")
levels(data.factor$chest_pain) <- c("Asymptomatic", "Atypical angina", "No angina", "Typical angina")
levels(data.factor$blood_sugar) <- c("No", "Yes")
levels(data.factor$rest_cardio) <- c("Hypertrophy", "Normal", "Abnormalities")
levels(data.factor$angina_pain) <- c("No", "Yes")
levels(data.factor$slope) <- c("Descending", "Flat", "Ascending")
levels(data.factor$thalassemia) <- c("Fixed defect", "Normal flow", "Reversible defect")
levels(data.factor$hearth_disease) <- c("Yes", "No")

data.factor$sex <- as.factor(data.factor$sex)
data.factor$chest_pain <- as.factor(data.factor$chest_pain)
data.factor$blood_sugar <- as.factor(data.factor$blood_sugar)
data.factor$rest_cardio <- as.factor(data.factor$rest_cardio)
data.factor$angina_pain <- as.factor(data.factor$angina_pain)
data.factor$slope <- as.factor(data.factor$slope)
data.factor$thalassemia <- as.factor(data.factor$thalassemia)
data.factor$hearth_disease <- as.factor(data.factor$hearth_disease)

## change continuous variables into cathegorical
library(dplyr)
data.factor <- data.factor %>% 
  mutate(age = 
           case_when(age >= 0  & age <= 40 ~ '0-40',
                     age > 40  & age <= 50 ~ '41-50',
                     age > 50  & age <= 60 ~ '51-60',
                     age > 60  & age <= 70 ~ '61-70',
                     age > 70 ~ '71-100'))

data.factor <- data.factor %>% 
  mutate(blood_pressure = 
           case_when(blood_pressure >= 0  & blood_pressure <= 100 ~ '0-100',
                     blood_pressure > 100  & blood_pressure <= 120 ~ '101-120',
                     blood_pressure > 120  & blood_pressure <= 140 ~ '121-140',
                     blood_pressure > 140  & blood_pressure <= 160 ~ '141-160',
                     blood_pressure > 160 ~ '161-200'))

data.factor <- data.factor %>% 
  mutate(ecg_depression = 
           case_when(ecg_depression >= 0  & ecg_depression <= 0.5 ~ '0.00-0.50',
                     ecg_depression > 0.5  & ecg_depression <= 1 ~ '0.51-1.00',
                     ecg_depression > 1.0  & ecg_depression <= 1.5 ~ '1.01-1.50',
                     ecg_depression > 1.5  & ecg_depression <= 2 ~ '1.51-2.00',
                     ecg_depression > 2  & ecg_depression <= 2.5 ~ '2.01-2.50',
                     ecg_depression > 2.5  & ecg_depression <= 3 ~ '2.51-3.00',
                     ecg_depression > 3  & ecg_depression <= 3.5 ~ '3.00-3.50',
                     ecg_depression > 3.5  & ecg_depression <= 4 ~ '3.50-4.00',
                     ecg_depression > 4 ~ '4.00-8.00'))

data.factor <- data.factor %>% 
  mutate(cholesterol = 
           case_when(cholesterol >= 0  & cholesterol <= 20 ~ '0-20',
                     cholesterol > 20  & cholesterol <= 40 ~ '20-40',
                     cholesterol > 40  & cholesterol <= 60 ~ '40-60',
                     cholesterol > 60  & cholesterol <= 80 ~ '60-80',
                     cholesterol > 80  & cholesterol <= 100 ~ '80-100',
                     cholesterol > 100  & cholesterol <= 120 ~ '100-120',
                     cholesterol > 120  & cholesterol <= 140 ~ '120-140',
                     cholesterol > 140 ~ '140-160'))

data.factor <- data.factor %>% 
  mutate(max_cardio = 
           case_when(max_cardio >= 0  & max_cardio <= 100 ~ '0-100',
                     max_cardio > 100  & max_cardio <= 120 ~ '100-120',
                     max_cardio > 120  & max_cardio <= 140 ~ '120-140',
                     max_cardio > 140  & max_cardio <= 160 ~ '140-160',
                     max_cardio > 160  & max_cardio <= 180 ~ '160-180',
                     max_cardio > 180 ~ '180-220'))

###############################################################

par(mfrow = c(2, 3), mar = c(4, 2, 2, 2))

data.numeric <- data

data.numeric$age <- as.numeric(data.numeric$age)
data.numeric$sex <- as.numeric(data.numeric$sex)
data.numeric$chest_pain <- as.numeric(data.numeric$chest_pain)
data.numeric$blood_pressure <- as.numeric(data.numeric$blood_pressure)
data.numeric$cholesterol <- as.numeric(data.numeric$cholesterol)
data.numeric$blood_sugar <- as.numeric(data.numeric$blood_sugar)
data.numeric$rest_cardio <- as.numeric(data.numeric$rest_cardio)
data.numeric$max_cardio <- as.numeric(data.numeric$max_cardio)
data.numeric$angina_pain <- as.numeric(data.numeric$angina_pain)
data.numeric$ecg_depression <- as.numeric(data.numeric$ecg_depression)
data.numeric$slope <- as.numeric(data.numeric$slope)
data.numeric$num_major_vessels <- as.numeric(data.numeric$num_major_vessels)
data.numeric$thalassemia <- as.numeric(data.numeric$thalassemia)
data.numeric$hearth_disease <- as.numeric(data.numeric$hearth_disease)

library(dplyr)
data.numeric <- data.numeric %>% 
  mutate(age = 
           case_when(age >= 0  & age <= 40 ~ 35,
                     age > 40  & age <= 50 ~ 45,
                     age > 50  & age <= 60 ~ 55,
                     age > 60  & age <= 70 ~ 65,
                     age > 70 ~ 75))

data.numeric <- data.numeric %>% 
  mutate(blood_pressure = 
           case_when(blood_pressure >= 0  & blood_pressure <= 100 ~ 90,
                     blood_pressure > 100  & blood_pressure <= 120 ~ 110,
                     blood_pressure > 120  & blood_pressure <= 140 ~ 130,
                     blood_pressure > 140  & blood_pressure <= 160 ~ 150,
                     blood_pressure > 160 ~ 160))

data.numeric <- data.numeric %>% 
  mutate(ecg_depression = 
           case_when(ecg_depression >= 0  & ecg_depression <= 0.5 ~ 0.25,
                     ecg_depression > 0.5  & ecg_depression <= 1 ~ 0.75,
                     ecg_depression > 1.0  & ecg_depression <= 1.5 ~ 1.25,
                     ecg_depression > 1.5  & ecg_depression <= 2 ~ 1.75,
                     ecg_depression > 2  & ecg_depression <= 2.5 ~ 2.25,
                     ecg_depression > 2.5  & ecg_depression <= 3 ~ 2.75,
                     ecg_depression > 3  & ecg_depression <= 3.5 ~ 3.25,
                     ecg_depression > 3.5  & ecg_depression <= 4 ~ 3.75,
                     ecg_depression > 4 ~ 4))

data.numeric <- data.numeric %>% 
  mutate(cholesterol = 
           case_when(cholesterol >= 0  & cholesterol <= 20 ~ 15,
                     cholesterol > 20  & cholesterol <= 40 ~ 30,
                     cholesterol > 40  & cholesterol <= 60 ~ 50,
                     cholesterol > 60  & cholesterol <= 80 ~ 70,
                     cholesterol > 80  & cholesterol <= 100 ~ 90,
                     cholesterol > 100  & cholesterol <= 120 ~ 110,
                     cholesterol > 120  & cholesterol <= 140 ~ 130,
                     cholesterol > 140 ~ 145))

data.numeric <- data.numeric %>% 
  mutate(max_cardio = 
           case_when(max_cardio >= 0  & max_cardio <= 100 ~ 90,
                     max_cardio > 100  & max_cardio <= 120 ~ 110,
                     max_cardio > 120  & max_cardio <= 140 ~ 130,
                     max_cardio > 140  & max_cardio <= 160 ~ 150,
                     max_cardio > 160  & max_cardio <= 180 ~ 170,
                     max_cardio > 180 ~ 190))

str(data.numeric)

###############################################################

#visualization
library(gplots)  #OK
#diff.delta = sapply(diff[, 1:6], function(x) x / diff$dT)
rho = cor(data.frame(data.numeric))
palette.breaks = seq(0, 1, 0.1)
par(oma = c(2, 2, 2, 1))
heatmap.2(rho, scale = "none", 
          trace = "none", 
          revC = TRUE, 
          breaks = palette.breaks)

library(bnlearn)

ug = empty.graph(colnames(rho))
amat(ug) = (rho > 0.4) + 0L - diag(1L, nrow(rho))
graphviz.plot(ug, layout = "fdp", shape = "ellipse") # NO

###############################################################

dag <- aracne(data.numeric, #TODO set threshold
              debug = TRUE) # OK
dag # infos bayesian network
plot(dag)

dag <- aracne(data.factor, debug = TRUE) 
dag # infos bayesian network
plot(dag)

# blacklist
bl = tiers2blacklist(list("cholesterol",
                          "blood_sugar",
                          "blood_pressure",
                          "rest_cardio",
                          c('chest_pain')))
bl = rbind(bl, c("cholesterol", "blood_sugar"))
bl

# whitelist
wl = matrix(c("num_major_vessels", "hearth_disease",
              "chest_pain", "hearth_disease",
              "angina_pain", "hearth_disease",
              "ecg_depression", "hearth_disease"),
            ncol = 2, 
            byrow = TRUE, 
            dimnames = list(NULL, c("from", "to")))
wl

###############################################################

# network aracne with wl and bl
dag = aracne(data.numeric, 
         whitelist = wl, 
         blacklist = bl,
         debug = TRUE)
dag

###############################################################

# graphical network with adjustements
graphviz.plot(dag, 
              shape = "ellipse", 
              highlight = list(arcs = wl))
# bootstrap
boot.dag = boot.strength(data.numeric, 
                         R = 200, # number of bootstrap replicates
                         algorithm = "aracne",
                         algorithm.args = list(whitelist = wl, 
                                               blacklist = bl)
)

boot.dag # TODO set threshold, forza tra le relazioni

attr(boot.dag, "threshold")

# averaged network structure.
avg.diff = averaged.network(boot.dag)

# plot strength netword
strength.plot(avg.diff, # PRINT
              boot.dag, 
              shape = "ellipse", 
              highlight = list(arcs = wl)
)

par(mfrow = c(1, 2))

# comare averaged with non averaged network
graphviz.compare(avg.diff, # PRINT
                 dag, 
                 shape = "ellipse", 
                 main = c("averaged network", "single network")
)

compare(avg.diff, dag) # PRINT

compare(avg.diff, dag, arcs = TRUE) # PRINT


undirected.arcs(cpdag(dag, wlbl = TRUE))


avg.diff$learning$whitelist = wl
avg.diff$learning$blacklist = bl
undirected.arcs(cpdag(avg.diff, wlbl = TRUE))

compare(cpdag(avg.diff, wlbl = TRUE), cpdag(dag, wlbl = TRUE))

# arc strenght
plot(boot.dag)
abline(v = 0.75, col = "tomato", lty = 2, lwd = 2)
abline(v = 0.85, col = "steelblue", lty = 2, lwd = 2)


nrow(boot.dag[str.diff$strength > attr(boot.dag, "threshold") &
                boot.dag$direction > 0.5, ])

nrow(boot.dag[boot.dag$strength > 0.75 & boot.dag$direction > 0.5, ])

nrow(boot.dag[boot.dag$strength > 0.85 & boot.dag$direction > 0.5, ])


avg.simpler = averaged.network(boot.dag, threshold = 0.85)

strength.plot(avg.simpler, 
              boot.dag, 
              shape = "ellipse", 
              highlight = list(arcs = wl))


fitted.simpler = bn.fit(avg.simpler, data.numeric)
fitted.simpler

fitted.simpler$age # parameter example

# summary of a possible linear relation
summary(lm(hearth_disease ~ num_major_vessels + 
             chest_pain +
             angina_pain, 
           data = data.numeric))


# model validation

#FIXME
xval = bn.cv(data.numeric, bn = "aracne", 
             algorithm.args = list(blacklist = bl, 
                                   whitelist = wl),
             loss = "cor-lw", 
             loss.args = list(target = "hearth_disease", 
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















