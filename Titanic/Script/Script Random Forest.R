#Objetivo de identificar se a pessoa irá sobreviver ou não no naufrágio do Titanic
#Este primeiro modelo será baseado apenas nas variáveis númericas sem nenhum tratamento aprofundado
#e na variavel categórica sexo

#REMOVE TODAS INFORMAÇÕES DO AMBIENTE R E LIMPA O CONSOLE
rm(list = ls())
cat("\014")

#CARREGA OS PACOTES QUE SERÃO UTILIZADOS
require(randomForest)
require(caTools)
require(tidyverse)
require(ROSE)
require(pROC)
require(caret)

#CARREGAMENTO DOS DADOS
#NO PRIMEIRO MOMENTO SERÁ CARREGADO APENAS OS DADOS DE TREINO
#AS MODIFICAÇÕES QUE FOREM FEITAS NOS DADOS DO TREINO SERÃO FEITOS TAMBÉM NO DADO DE TESTE
setwd("C:/Kaggle/Titanic/Dataset")
list.files()
df_train <- read.csv("train.csv", stringsAsFactors = F)

#VERIFICA AS INFORMAÇÕES DO BANCO DE DADOS
str(df_train) 
summary(df_train) #temos 177 dados com valores "NA" para variável idade

#Para esse exemplo serão substituidos apenas os dados que possuem "NA" pela mediana
df_train$Age <- ifelse(is.na(df_train$Age) == "TRUE", 
                       median(df_train$Age, na.rm = T), df_train$Age)

summary(df_train) #Não constam mais valores com NA para a variável idade

#Categorizando os valores da variável Sexo
df_train$Sex <- ifelse(df_train$Sex == "male", 1, 0)

#Separação dos dados para fazer uma validação do modelo a ser criado
set.seed(123)
divisao <- sample.split(df_train$Survived, SplitRatio = 0.7)
base_treinamento <- subset(df_train, divisao == TRUE)
base_teste <- subset(df_train, divisao == FALSE)

#Gera o modelo de random forest
model_rf <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare,
                          data = base_treinamento)
model_rf

#Verifica a importância de cada variável no modelo
varImp(model_rf) %>% 
  rownames_to_column() %>% 
  arrange(desc(Overall))

previsoes <- predict(model_rf, newdata = base_teste[-1])
previsoes

#Matriz de confusão e acurácia
table(previsoes, base_teste$Survived)
cat("accuracy = ", (149+70)/268) #81,7% de acurácia



#mtry 3 ntree = (best 109) or 
#mtry = 2 ntree = (good 117,good 400)
model_rf_2 <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare,
                         mtry = 3, ntree = 109,
                         data = base_treinamento)
model_rf_2

#Verifica a importância de cada variável no modelo
varImp(model_rf_2) %>% 
  rownames_to_column() %>% 
  arrange(desc(Overall))

previsoes_2 <- predict(model_rf_2, newdata = base_teste[-1])
previsoes_2

#Matriz de confusão e acurácia
table(previsoes_2, base_teste$Survived)
cat("accuracy = ", (147+72)/268) #82,4% de acurácia



#################################BALANCED SAMPLE###################################
#Over sampling
data_balanced_over <- ovun.sample(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare,
                                  data = base_treinamento, 
                                  method = "over",
                                  N = 768, seed = 123)$data
table(data_balanced_over$Survived)



model_rf_over <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare,
                         data = data_balanced_over)
model_rf_over

#Verifica a importância de cada variável no modelo
varImp(model_rf_over) %>% 
  rownames_to_column() %>% 
  arrange(desc(Overall))

previsoes_over <- predict(model_rf_over, newdata = base_teste[-1])
previsoes_over

#Matriz de confusão e acurácia
table(previsoes_over, base_teste$Survived)
cat("accuracy = ", (144+74)/268) #81,3% de acurácia


#Under Sampling
data_balanced_under <- ovun.sample(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare,
                                  data = base_treinamento, 
                                  method = "under",
                                  N = 478,
                                  seed = 123)$data
table(data_balanced_under$Survived)


model_rf_under <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare,
                         data = data_balanced_under)
model_rf_under

#Verifica a importância de cada variável no modelo
varImp(model_rf_under) %>% 
  rownames_to_column() %>% 
  arrange(desc(Overall))

previsoes_under <- predict(model_rf_under, newdata = base_teste[-1])
previsoes_under

#Matriz de confusão e acurácia
table(previsoes_under, base_teste$Survived)
cat("accuracy = ", (137+81)/268) #80,2% de acurácia



#Both
data_balanced_both <- ovun.sample(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare,
                                   data = base_treinamento, 
                                   method = "both",
                                   p = 0.5,
                                   seed = 123)$data
table(data_balanced_both$Survived)



model_rf_both <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare,
                         data = data_balanced_both)
model_rf_both


#Verifica a importância de cada variável no modelo
varImp(model_rf_both) %>% 
  rownames_to_column() %>% 
  arrange(desc(Overall))

previsoes_both <- predict(model_rf_both, newdata = base_teste[-1])
previsoes_both

#Matriz de confusão e acurácia
table(previsoes_both, base_teste$Survived)
cat("accuracy = ", (141+72)/268) #79,8% de acurácia


#ROSE
data_balanced_rose <- ROSE(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare,
                                  data = base_treinamento,
                           seed = 123)$data

table(data_balanced_rose$Survived)


model_rf_rose <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare,
                         data = data_balanced_rose)
model_rf_rose


#Verifica a importância de cada variável no modelo
varImp(model_rf_rose) %>% 
  rownames_to_column() %>% 
  arrange(desc(Overall))

previsoes_rose <- predict(model_rf_rose, newdata = base_teste[-1])
previsoes_rose

#Matriz de confusão e acurácia
table(previsoes_rose, base_teste$Survived)
cat("accuracy = ", (145+70)/268) #80,2% de acurácia


########################CREATE RESULTS###################################


#Agora iremos aplicar esse modelo para os dados de teste que o problema nos disponibilizou
list.files()
df_test <- read.csv("test.csv", stringsAsFactors = F)
names(df_test)

#Será selecionado apenas as mesmas variaveis que foram utilizadas para construir o modelo
df_test <- df_test[,c(1,2,4,5,6,7,9)]

#Verifica se possui valores NA
summary(df_test)

#Tem 86 Na's para Age e 1 Na para Fare
#Para ambos os casos os NA'a serão substituidos pela mediana de seus respectivos valores
df_test$Age <- ifelse(is.na(df_test$Age) == "TRUE", 
                      median(df_test$Age, na.rm = T), df_test$Age)
df_test$Fare <- ifelse(is.na(df_test$Fare) == "TRUE", 
                       median(df_test$Fare, na.rm = T), df_test$Fare)

#Categorizando os valores da variável Sexo
df_test$Sex <- ifelse(df_test$Sex == "male", 1, 0)

#Prever quais as pessoas irão ou não sobreviver ao naufrágio
previsoes_final <- predict(model_rf_under, newdata = df_test[-1])
previsoes_final
previsoes_final <- data.frame(previsoes_final)

#Colocada os valores do Passenger ID e sua respectiva classificação, se irá sobreviver ou não
submission <- cbind(df_test$PassengerId, previsoes_final)
submission <- data.frame(submission)
colnames(submission) <- c("PassengerId", "Survived")

#Grava o resultado em csv
write_csv(submission, "submission_rf_under.csv")



#ROC CURVE
previsoes_prob <- predict(model_rf, newdata = base_teste[-1], type = "prob")
previsoes_prob

ROC_rf <- roc(base_teste$Survived, previsoes_prob[,2])
ROC_rf_auc <- auc(ROC_rf)
plot(ROC_rf)