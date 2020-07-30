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

#Nomes
df_train$teste <- str_extract(df_train$Name, "[ ][A-Z][a-z]+\\.")
df_train$teste <- str_trim(df_train$teste, side = c("both"))
df_train$teste <- sub(pattern = "\\.", replacement = "", x = df_train$teste)
prop.table(table(df_train$teste, df_train$Survived))

#Separação dos dados para fazer uma validação do modelo a ser criado
set.seed(123)
divisao <- sample.split(df_train$Survived, SplitRatio = 0.7)
base_treinamento <- subset(df_train, divisao == TRUE)
base_teste <- subset(df_train, divisao == FALSE)

#Gera o modelo de random forest
model_rf <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                         teste, ntree = 2000, mtry = 2, maxnode = 10, nodesize = 70,
                         data = base_treinamento)
model_rf

#Verifica a importância de cada variável no modelo
varImp(model_rf) %>% 
  rownames_to_column() %>% 
  arrange(desc(Overall))

previsoes <- predict(model_rf, newdata = base_teste[,c(3,4,5,6,7,8,10, 13)])
#previsoes

#Matriz de confusão e acurácia
table(previsoes, base_teste$Survived)
cat("accuracy = ", (table(previsoes, base_teste$Survived)[1,1]+
                      table(previsoes, base_teste$Survived)[2,2])/268) #81,7% de acurácia
