#Objetivo de identificar se a pessoa ir� sobreviver ou n�o no naufr�gio do Titanic
#Este primeiro modelo ser� baseado apenas nas vari�veis n�mericas sem nenhum tratamento aprofundado
#e na variavel categ�rica sexo

#REMOVE TODAS INFORMA��ES DO AMBIENTE R E LIMPA O CONSOLE
rm(list = ls())
cat("\014")

#CARREGA OS PACOTES QUE SER�O UTILIZADOS
require(tidyverse)
require(corrplot)
require(alluvial)

#CARREGAMENTO DOS DADOS
#NO PRIMEIRO MOMENTO SER� CARREGADO APENAS OS DADOS DE TREINO
#AS MODIFICA��ES QUE FOREM FEITAS NOS DADOS DO TREINO SER�O FEITOS TAMB�M NO DADO DE TESTE
setwd("C:/Kaggle/Titanic/Dataset")
list.files()
df_train <- read.csv("train.csv", stringsAsFactors = F)

#VERIFICA AS INFORMA��ES DO BANCO DE DADOS
str(df_train) 
summary(df_train) #temos 177 dados com valores "NA" para vari�vel idade

#Para esse exemplo ser�o substituidos apenas os dados que possuem "NA" pela mediana
df_train$Age <- ifelse(is.na(df_train$Age) == "TRUE", 
                       median(df_train$Age, na.rm = T), df_train$Age)
df_train$Age <- as.integer(df_train$Age)

summary(df_train) #N�o constam mais valores com NA para a vari�vel idade

#Categorizando os valores da vari�vel Sexo
df_train$Sex <- ifelse(df_train$Sex == "male", 1, 0)

#Propor��o de sobreviventes
round(prop.table(table(df_train$Survived)), digits = 3)*100

#Ver informa��es das variaveis
str(df_train)
#PassagerID n�o importa, porque � a identifica��o do passageiro
#como n�o vou fazer estudo "direcionado" a ele posteriormente n�o preciso analisar

#Analisar as variaveis numericas num primeiro momento
#De variaveis numericas temos: Pclass, sex, age, SibSp, Parch e Fare
numericas <- df_train[,c(2,3,5,6,7,8,10)]
corrplot(cor(numericas), method = "number")
rm(numericas)

#rela��es survived x (sex, pclass, fare) | Fare x pclass | Parch x SibSp

#rela��es survived x (sex, pclass, fare)
df_graph <- df_train[,c(2,3,10)]
df_graph2 <- df_graph %>% 
  group_by(Survived, Pclass, Fare) %>% 
  summarize(Freq = n()) %>% 
  arrange(Fare)
df_graph2$lab <- ifelse(df_graph2$Fare <= 25, "B","A")

alluvial(df_graph2[,c(1,2,5)], freq=df_graph2$Freq,
         col = ifelse(df_graph2$Survived == 0, "red", "green"),
         border = ifelse(df_graph2$Survived == 0, "red", "green"),
         hide = df_graph2$Freq == 0,
         cex = 0.7)
rm(df_graph, df_graph2)

#Pude perceber que pessoas que estavam na classe 3 e de acordo com a minha classifica��o
#e com preco da passagem B, tinham maior chance de morrer


#Verificando rel��o entre Parch x SibSp
df_graph <- df_train[,c(2,7,8)]
df_graph$family <- rowSums(df_graph[,c(2,3)])
#df_graph$SibSp <- ifelse(df_graph$SibSp >= 1, 1, df_graph$SibSp)
#df_graph$Parch <- ifelse(df_graph$Parch >= 1, 1, df_graph$Parch)
df_graph2 <- df_graph %>% 
  group_by(Survived, SibSp, Parch, family) %>% 
  summarize(Freq = n())
alluvial(df_graph2[,c(1,4)], freq=df_graph2$Freq,
         col = ifelse(df_graph2$Survived == 0, "red", "green"),
         border = ifelse(df_graph2$Survived == 0, "red", "green"),
         hide = df_graph2$Freq == 0,
         cex = 0.7)
rm(df_graph, df_graph2)


#Verificando rela��o entra a idade e se sobreviveu
h1 <- df_train %>% 
  filter(Survived == 0)
h1 <- as.numeric(h1$Age)
h2 <- df_train %>% 
  filter(Survived == 1)
h2 <- as.numeric(h2$Age)

hist(h1, col= "red", main="Age x Survived", xlab = "Age", ylim = c(0,250))
hist(h2, add=T, col=rgb(0, 1, 0, 0.5))
rm(h1, h2)
#pessoas entre 25 a 30 anos foram as que mais morreram e pessoas entre 20 e 35
#anos foram os que mais sobreviveram

#Rela��o entre o sexo e se se sobreviveu ou nao
barplot(table(df_train$Survived, df_train$Sex), col = c("red", "green"),
        legend.text = c("Morreu", "Sobreviveu"),
        ylim = c(0,600))
#a maioria das pessoas que morreram eram do sexo maculino

#verificando o embarque
df_graph <- df_train[,c(2,12)]
df_graph2 <- df_graph %>% 
  group_by(Survived, Embarked) %>% 
  summarize(Freq = n())
alluvial(df_graph2[,c(1:2)], freq=df_graph2$Freq,
         col = ifelse(df_graph2$Survived == 0, "red", "green"),
         border = ifelse(df_graph2$Survived == 0, "red", "green"),
         hide = df_graph2$Freq == 0,
         cex = 0.7)
rm(df_graph, df_graph2)
round(prop.table(table(df_train$Embarked,df_train$Survived)), digits = 2)
#pessoas que embarcaram pelo port�o S morerram mais

#verificando a cabine
prop.table(table(df_train$Cabin, df_train$Survived))
df_train$cabine <- ifelse(df_train$Cabin == "","sem cabine","com cabine")
prop.table(table(df_train$cabine, df_train$Survived))
df_train$cabine <- NULL
#pessoas que tinham informa��o de alguma cabine sobreviveram mais
#em rela��o aquelas que n�o tinham

#verificando o ticket
sort(table(df_train$Ticket), decreasing = T)
#a respeito do ticket n�o consegui encontrar nenhum padr�o

#Nome
#Primeiro vou separar apenas as informa��es de Mrs, Misses, Master...
df_train$teste <- str_extract(df_train$Name, "[ ][A-Z][a-z]+\\.")
df_train$teste <- str_trim(df_train$teste, side = c("both"))
df_train$teste <- sub(pattern = "\\.", replacement = "", x = df_train$teste)
prop.table(table(df_train$teste, df_train$Survived))
#Aqueles nomes com MR morreram em amior quantidade
table(df_train$teste)
