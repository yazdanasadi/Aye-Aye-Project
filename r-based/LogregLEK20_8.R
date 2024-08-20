# Laden der notwendigen Bibliotheken
library(tidyverse)
library(pROC)
library(ResourceSelection)
library(readxl)
library(MASS)
library(car)
library(glmnet)
library(corrr)

#Laden und aufbereiten des Datensatzes

BK_data <- read_excel("C:/Users/Bodenbur/Downloads/LEK_data_grouped.xlsx")
View(BK_data)

# Standardisierung der numerischen Variablen
BK_data <- BK_data %>%
  mutate(Age = scale(Age),
         Schooling.years = scale(schooling.years),
         Knowledge.non.benefit.score.max.6 = scale(Knowledge.non.benefit.score.max.6))

# Konvertieren der relevanten Variablen in Faktoren
factor_vars <- c("Clove.benefit.score", "Gender", "edu.level", "Migration.Experience",
                 "Farms.Cloves", "encountered.binary", "ID.vis.Aye", "ID.nom",
                 "Diet.Insects", "School", "Farms.Paddy.Rice", "Farms.Tavy.Rice", 
                 "Extracts.Medical.plants", "FES.provisioning", "FES.regulating", 
                 "FES.cultural", "FES.supporting", "ID.vis.Indri", "ID.vis.Microcebus", 
                 "Aye.aye.protected","Knowledge.signs", "Agegroup", "nr.month.low.supply", "DK.score.group", "Forest.cover2017", "Forest.cover1990", "Forestloss_total", "Forestloss_relative")

BK_data[factor_vars] <- lapply(BK_data[factor_vars], as.factor)
BK_data$Ethnic.group <- as.character(BK_data$Ethnic.group)
BK_data$Region <- as.character(BK_data$Region)

# Überprüfen der Struktur des Datensatzes
str(BK_data)

#Variablenselektion für logistisches Regressionsmodell

# Schrittweise Regression mit AIC zur Variablenselektion
initial_model <- glm(Clove.benefit.score ~ ., data = BK_data, family = binomial)
stepwise_model <- stepAIC(initial_model, direction = "both")
summary(stepwise_model)

# Extraktion der relevanten Variablen aus dem stepwise_model
selected_vars_stepwise <- names(coef(stepwise_model))
print(selected_vars_stepwise)

#Oder Lasso
#Vorbereitung für Lasso-Regression (Regularisierung)
x <- model.matrix(Clove.benefit.score ~ ., BK_data)[, -1]
y <- BK_data$Clove.benefit.score

# Durchführung der Lasso-Regression mit Kreuzvalidierung
cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1)
best_lambda <- cv_lasso$lambda.min
model_lasso <- glmnet(x, y, family = "binomial", alpha = 1, lambda = best_lambda)

# Extraktion der Koeffizienten der Lasso-Regression
lasso_coefs <- coef(model_lasso)
selected_vars_lasso <- rownames(lasso_coefs)[as.numeric(lasso_coefs) != 0]
print(selected_vars_lasso)

###Modellerstellung


importance <- varImp(logistic_model, scale = FALSE)
print(importance)
# Finales Modell mit den ausgewählten Variablen
final_formula <- as.formula(paste("Clove.benefit.score ~", paste(selected_vars_lasso[-1], collapse = " + ")))
final_model <- glm(final_formula, data = BK_data, family = binomial)
summary(final_model)

##Knowledge parametres #Acc 0.78
m1 <- glm(Clove.benefit.score~DK.score+Knowledge.signs+Diet.Insects+Knowledge.score+ID.vis.Aye+ID.nom+ID.vis.Indri+ID.vis.Microcebus, data = BK_data, family=binomial)
summary(m1)
varImp_m1 <- varImp(m1, scale = FALSE)
print(varImp_m1)
predictionsm1 <- predict(m1, newdata = BK_test, type = "response")
predicted_classesm1 <- ifelse(predictionsm1 > 0.5, 1, 0)
confusionMatrix(as.factor(predicted_classesm1), as.factor(BK_test$Clove.benefit.score))

##All parametres ACC 0.74
m2 <- glm(Clove.benefit.score~., data = BK_data, family=binomial)
summary(m2)
varImp_m2 <- varImp(m2, scale = FALSE)
print(varImp_m2)
predictionsm2 <- predict(m2, newdata = BK_test, type = "response")
predicted_classesm2 <- ifelse(predictionsm2 > 0.5, 1, 0)
confusionMatrix(as.factor(predicted_classesm2), as.factor(BK_test$Clove.benefit.score))


##Demographic parametres #ACC .65
m3 <- glm(Clove.benefit.score~Region+Migration.Experience+ edu.level+Ethnic.group+nr.month.low.supply+Remoteness+village.size+School+Forest.cover2017+Forest.cover1990+ Forestloss_total+	Forestloss_relative+	Farms.Paddy.Rice+	Farms.Tavy.Rice+	Extracts.Medical.plants
, data = BK_data, family=binomial)
summary(m3)
varImp_m3 <- varImp(m3, scale = FALSE)
print(varImp_m3)

predictionsm3 <- predict(m3, newdata = BK_test, type = "response")
predicted_classesm3 <- ifelse(predictionsm3 > 0.5, 1, 0)
confusionMatrix(as.factor(predicted_classesm3), as.factor(BK_test$Clove.benefit.score))

### Univariate logistische Regression
install.packages("broom")
install.packages("purrr")
install.packages("dplyr")

library(broom)
library(purrr)
library(dplyr)

# Variablen auswählen, die du untersuchen möchtest
prädiktoren <- BK_data %>% select(-Clove.benefit.score)

# Funktion zum Durchführen univariater logistischer Regression
univariate_logistic <- function(var) {
  formula <- as.formula(paste("Clove.benefit.score ~", var))
  model <- glm(formula, data = BK_data, family = binomial)
  tidy(model)
}

# Ergebnisse der univariaten logistischen Regressionen für jede Variable
ergebnisse <- map_df(names(prädiktoren), univariate_logistic, .id = "variable")

# Ergebnisse anzeigen
print(ergebnisse, n=84)

# Filtern der signifikanten Variablen
signifikante_variablen <- ergebnisse %>% filter(p.value < 0.05)
print(signifikante_variablen, n=45)

###multivariate Regression mit signifikanten Variablen statistic >3
#Gender, encountered.binary, ID.vis.Aye, Knowledge.score, DK.score, Diet.Insects, Extracts.Medical.plants, ID.vis.Indri, ID.vis.Microcebus
#ACC 0.78
m4 <- glm(Clove.benefit.score ~ Gender + encountered.binary + ID.vis.Aye + Knowledge.score  + Diet.Insects + Extracts.Medical.plants + ID.vis.Indri + ID.vis.Microcebus, 
          data = BK_data, family = binomial)
summary(m4)
varImp_m4 <- varImp(m4, scale = FALSE)
print(varImp_m4)

predictionsm4 <- predict(m4, newdata = BK_test, type = "response")
predicted_classesm4 <- ifelse(predictionsm4 > 0.5, 1, 0)
confusionMatrix(as.factor(predicted_classesm4), as.factor(BK_test$Clove.benefit.score))
library(ggplot2)

#Modellbewertung 
# Aufteilen der Daten in Trainings- und Testdaten
set.seed(123)
trainIndex <- createDataPartition(BK_data$Clove.benefit.score, p = .8, 
                                  list = FALSE, 
                                  times = 1)
BK_train <- BK_data[trainIndex,]
BK_test  <- BK_data[-trainIndex,]



# Vorhersagen und Modellbewertung auf den Testdaten
predictions <- predict(logistic_model, newdata = BK_test, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
confusionMatrix(as.factor(predicted_classes), as.factor(BK_test$Clove.benefit.score))


# Umwandeln der varImp-Ergebnisse in ein Datenrahmen
varImp_df <- as.data.frame(varImp_m4)
varImp_df$Variable <- rownames(varImp_df)

 # Erstellen eines Balkendiagramms der Variablenwichtigkeit
 ggplot(varImp_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  +     geom_bar(stat = "identity") +
  +     coord_flip() +
  +     labs(title = "Variable Importance", x = "Variables", y = "Importance Score")


library(caret) # für die Konfusionsmatrix, Datenaufteilung und varImp
library(pROC)  # für die ROC-Kurve und AUC


# Erstellen des logistischen Regressionsmodells mit den Trainingsdaten
m4 <- glm(Clove.benefit.score ~ Gender + encountered.binary + ID.vis.Aye + Knowledge.score + DK.score + Diet.Insects + Extracts.Medical.plants + ID.vis.Indri + ID.vis.Microcebus, 
          data = BK_train, family = binomial)

# Zusammenfassung des Modells
summary(m4)

# Vorhersagen mit dem Modell auf den Testdaten
test_predictions <- predict(m4, newdata = BK_test, type = "response")

# Konvertieren der Wahrscheinlichkeiten in binäre Vorhersagen (Schwellenwert 0.5)
test_predicted_classes <- ifelse(test_predictions > 0.5, 1, 0)

# Konfusionsmatrix erstellen für die Testdaten
confusion <- confusionMatrix(as.factor(test_predicted_classes), as.factor(BK_test$Clove.benefit.score))
print(confusion)



####CART Modell
library(rpart)
library(rpart.plot)
library(ggplot2)
library(caret)
C1 <- rpart(Clove.benefit.score~., data = BK_train, method = "class",control=rpart.control(minsplit=9, cp=0.03, xval = 99))
C1$cptable[which.min(C1$cptable[,"xerror"]),"CP"]
summary(C1)
rpart.plot(C1, type = 3, digits=3, fallen.leaves = TRUE)
PredictC1<- predict(C1, BK_test, type="class")
Observed.BK <-as.factor(BK_test$Clove.benefit.score)
confusionMatrix(PredictC1, Observed.BK)

C2<- rpart(Clove.benefit.score~Gender+DK.score+Knowledge.score+Diet.Insects+
             Aye.aye.protected+ID.nom+village.size, 
           data = BK_train, method = "class",control=rpart.control(minsplit=3, cp=0.036, xval = 99))
C2$cptable[which.min(C2$cptable[,"xerror"]),"CP"]
summary(C2)
rpart.plot(C2, type = 3, digits=3, fallen.leaves = TRUE)
PredictC2<- predict(C2, BK_test, type="class")
confusionMatrix(PredictC2, Observed.BK)

C3<- rpart(Clove.benefit.score~Knowledge.score+Diet.Insects+
             ID.vis.Aye+Age+Knowledge.signs+Age+edu.level+Farms.Cloves+encountered.binary+Extracts.Medical.plants, 
           data = BK_train, method = "class",control=rpart.control(minsplit=2, cp=0.036, xval = 45))
C3$cptable[which.min(C2$cptable[,"xerror"]),"CP"]
summary(C3)
rpart.plot(C3, type = 3, digits=2, fallen.leaves = TRUE)
PredictC3<- predict(C3, BK_test, type="class")
confusionMatrix(PredictC3, Observed.BK)
