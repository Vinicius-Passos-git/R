#UTILIZANDO OS DADOS TITANIC DO KAGGLE, IREI EXPLORAR ALGUMAS INFORMAÇÕES E 
#POSTERIORMENTE CRIAREI MODELOS PARA VER O QUAL MELHOR SE ENCAIXA PARA 
#CONSEGUIR PREVER QUAIS OS PASSAGEIROS IRIAM OU NÃO SOBREVIVER AO
#NAUFRÁGIO

#INICIALMENTE IREI FAZER UMA ANÁLISE PREVIA DOS DADOS PARA VER QUAIS 
#INSIGHTS CONISGO TIRAR.
#SEI QUE ESSA É UMA BASE QUE MUITAS PESSOAS JÁ UTILIZARAM E 
#DEVEM TER CENTENAS DE ANALISES SOBRE ELA
#MAS AQUI TENTAREI FAZER ALGUMS SIMPLES SEM BASEAR NAS QUE JÁ FORAM FEITAS

#REMOVE TODAS INFORMAÇÕES DO AMBIENTE R E LIMPA O CONSOLE
rm(list = ls())
cat("\014")

#CARREGA OS PACOTES QUE SERÃO UTILIZADOS
require(tidyverse) #pacote para tratamento dos dados
require(caTools) #para separar os dados em teste e treino
require(caret) #biblioteca para o modelos de ml
require(vip) #importância das variaveis
require(alluvial) #utilizado para gráficos
require(randomForest) #utilizado para o modelo de Random Forest
require(ROCR) #utilizado para criar a curva ROC
require(extraTrees)

#CARREGAMENTO DOS DADOS
#NO PRIMEIRO MOMENTO SERÁ CARREGADO APENAS OS DADOS DE TREINO
#AS MODIFICAÇÕES QUE FOREM FEITAS NOS DADOS DO TREINO SERÃO FEITOS TAMBÉM NO DADO DE TESTE
setwd("C:/Kaggle/Titanic/Dataset")
list.files()
df_train <- read.csv("train.csv", stringsAsFactors = F)
#Iicialmente vemos que temos 891 observações e 12 variaveis

str(df_train) #verifica as informações das variáveis
#temos um total de 7 variáveis num/int e 5 variaveis do tipo caracter
#dentre essas, PassengerID não é relevante para o nosso estudo
#com isso járei tirar da nossa base de dados

df_train$PassengerId <- NULL
str(df_train) #verifica as informações das variáveis
summary(df_train) #retorna a sumarização das informações. Vemos que a idade possui 177 valores com NA

#Vamos olhar nesse primeiro momento a quantidade de pessoas que sobreviveram/morreram
#no naufragio
table(df_train$Survived)
round(prop.table(table(df_train$Survived)), digits = 2)
barplot(table(df_train$Survived),
        col = c("red", "green"),
        axes = F,
        names.arg =  c("not survied", "survived"),
        main = "Sobreviventes/Não sobreviventes")
box(which = "figure")

#Percebemos inicialmente que temos na nossa base de dados uma quantidade com mais de 60%
#das pessoas classificados como not survived. Vamos analisar se temos alguma informação junto das
#outras variaveis que consigamos identificar se pessoas que mais morreram
#tinha idades diferentes, sexo diferente entre outras variaveis que 
#iremos analisar

#Vamos começar pela idade. Como sabemos, temos 177 valores como NA. Vamos ver como
#iremos substituir esses valores

boxplot(df_train$Age)
hist(df_train$Age,
     breaks = 30,
     col = "grey80",
     probability = T,
     xlab = "Idade",
     main = "Histograma de idade",
     ylim = c(0, 0.04))
lines(density(df_train$Age, na.rm = T), col = "black")

#Vemos que a maioria das pessoas que estavam no navio tinha entre 20 e 40 anos e pelo
#gráfico de boxplot que não temos muitos outliers
mean(df_train$Age, na.rm = T) #calculo da média
median(df_train$Age, na.rm = T) #calculo da mediana
#Vemos que a média e mediana são próximas
#Primeiro vamos analisar se a proporção de NA para idade que morreram e sobreviveram
#não são muito discrepantes
df_train %>% 
  count(Survived, Sex, Age) %>% 
  group_by(Survived) %>% 
  filter(is.na(Age) == "TRUE")

#Vemos que 70% das pessoas que estão com NA morreram e o restante sobreviveu.
#Para não criar um vies da nossa base de teste colocando apenas a media ou mediana
#nesse primeiro momento irei gerar um valor aleatorio para os NA entre 20 e 40 anos
#isso porque vimos com o gráfico que a maioria das pessoas estão nesse intervalo
set.seed(123)
df_train$Age <- ifelse(is.na(df_train$Age) == "TRUE", sample(20:40, 177, replace = T),
                       df_train$Age)
summary(df_train$Age)
mean(df_train$Age) #calculo da média
median(df_train$Age) #calculo da mediana

#ós carescentar os dados faltantes para a idade vamos ver como é a 
#distruibuição de acordo com a idade para aqueles que sobreviveram e aqueles
#que não sobreviveram

survived <- df_train %>% 
  filter(Survived == 1)
no_survived <- df_train %>% 
  filter(Survived == 0)
p1 <- hist(survived$Age)
p2 <- hist(no_survived$Age)
plot( p1, col=rgb(0,1,0,1/4), xlim=c(0,80), ylim = c(0,140))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,80), add=T) 

rm(survived, no_survived, p1, p2)

#Vemos que pessoas com até 15 anos aparente ter mais chance de sobreviver
#com isso vamos criar uma variável em sexo como child até essa idade
df_train$Sex <- ifelse(df_train$Age <= 15, "Child", df_train$Sex)

#Vamos verificar a variavel PClass
table(df_train$Pclass)
round(prop.table(table(df_train$Pclass)), digits = 2)

#Mais de 50% das pessoas estavam no PClass igual a 3
#vamos ver a proporção de 0 e 1 nessa variavel
table(df_train$Survived, df_train$Pclass)
round(prop.table(table(df_train$Survived, df_train$Pclass)), digits = 2)

barplot(table(df_train$Survived, df_train$Pclass),
        col = c("red", "green"), axes = F)
legend("topleft", c("Not survied", 'Survived'), fill = c("red", "green"))

#No gráfico vemos que a classe 1 teve mais sobreviventes e na classe 3 mais mortos
#já na classe 2 essa diferença não perceptivel

#Agora vamos analisar a variavel sexo
table(df_train$Survived, df_train$Sex)
round(prop.table(table(df_train$Survived, df_train$Sex)), digits = 2)
barplot(table(df_train$Survived, df_train$Sex),
        col = c("red", "green"), axes = F)
legend("topleft", c("Not survied", 'Survived'), fill = c("red", "green"))

#Isso também já era esperado, a quantidade de homens que morreram foi bem maior do que
#as mulheres

#SibSp
table(df_train$Survived, df_train$SibSp)
round(prop.table(table(df_train$Survived, df_train$SibSp)), digits = 2)
barplot(table(df_train$Survived, df_train$SibSp),
        col = c("red", "green"), axes = F)
legend("topright", c("Not survied", 'Survived'), fill = c("red", "green"))

#Vemos que a maioria das pessoas viajaram sozinhos ou apenas com 1 acompanhante
barplot(table(df_train$Sex, df_train$SibSp),
        col = c("grey84","grey", "dimgrey"))
legend("topright", unique(df_train$Sex), fill = c("dimgrey", "grey", "grey84"))

#Parch
table(df_train$Survived, df_train$Parch)
round(prop.table(table(df_train$Survived, df_train$Parch)), digits = 2)
barplot(table(df_train$Survived, df_train$Parch),
        col = c("red", "green"), axes = F)
legend("topright", c("Not survied", 'Survived'), fill = c("red", "green"))
#Das pessoas que não viajaram sozinhas, elas tinham 1 ou 2 parentes no navio


#Se somarmos as duas variaveis Parch e Sibsp podemos dizer que é o tamanho
#da familia que embarcou no Titanic

df_train$Familia <- df_train[,6] + df_train[,7]

#Agora vamos verificar a relação familia e sobreviventes
barplot(table(df_train$Survived, df_train$Familia),
        col = c("red", "green"), axes = F)
legend("topright", c("Not survied", 'Survived'), fill = c("red", "green"))

#Vemos que familia de tamanho 4 ou mais tiveram morreram em uma proporção
#muito grande. Com isso vamos diminuir a quantidade de informações
#e colocar todos com 4 ou mais igual a 4
df_train$Familia <- ifelse(df_train$Familia >= 4, 4, df_train$Familia)

barplot(table(df_train$Survived, df_train$Familia),
        col = c("red", "green"), axes = F)
legend("topright", c("Not survied", 'Survived'), fill = c("red", "green"))
#Com a nova classificação ficou mais fácil verificar este fato.

#Fare
table(df_train$Survived, df_train$Fare)
round(prop.table(table(df_train$Survived, df_train$Fare)), digits = 2)
barplot(table(df_train$Survived, df_train$Fare),
        col = c("red", "green"), axes = F)
legend("topright", c("Not survied", 'Survived'), fill = c("red", "green"))
#O gráfico, que por sinal não fiz nenhum tratamento ou categorização, aparenta
#que pessoas que pagaram mais caro na passagem tiveram um percentual de
#sobreviventes maior
ggplot(df_train, aes(x=Fare, y=Age, color=factor(Survived))) +
  geom_point(size=6)
#Com o sccaterplot vemos que independente da idade as pessoas que pagaram
#mais barato na passagem morreram mais.
#Com isso irei criar uma classicação para essas passagens
df_train$classfare <- ifelse(df_train$Fare <= 50, "poor",
                             ifelse(df_train$Fare > 50 & df_train$Fare <=200,
                                    "medium", "high"))

barplot(table(df_train$Survived, df_train$classfare),
        col = c("red", "green"), axes = F)
legend("topleft", c("Not survied", 'Survived'), fill = c("red", "green"))

teste <- df_train %>% 
  group_by(Survived, classfare, Sex, Embarked) %>% 
  summarize(Freq = n())

alluvial(teste[,c(2,4,3,1)], freq=teste$Freq,
         col = ifelse(teste$Survived == 0, "red", "green"),
         border = ifelse(teste$Survived == 0, "red", "green"),
         hide = teste$Freq == 0,
         cex = 0.7)

rm(teste)
#Com esse gráfico podemos ver que com nossa nova classificação,
#pessoas que realmente pagaram pouco na passagem, morreram mais

#Agora podemos ter uma classificação melhor entre as pessoas que pagaram
#a passagem

#Embarked
table(df_train$Embarked, df_train$Survived)

barplot(table(df_train$Survived, df_train$Embarked),
        col = c("red", "green"), axes = F)
legend("topleft", c("Not survied", 'Survived'), fill = c("red", "green"))

#Temos duas pessoas que não está classificado o embarque. Como temos 
#uma quantidade maior de embarque S, os valores vazios serão substituidos por S
df_train$Embarked <- ifelse(df_train$Embarked == "",
                            "S", df_train$Embarked)

#Visto essas informações simples vamos ao modelo
#Nesse primeiro momento irei gerar um modelo de regressão logistica
#que como nossa resposta final é binária se encaixa melhor para nosso modelo.
#Depois irei testar outros modelos

#Selecionando as variaveis para o modelo final
features <- df_train[,c(1,2,4,5,6,7,9,11)]
names(features)
summary(features)

#Vamos colocar as variaveis categoricas em fatores
features$Sex <- factor(features$Sex)
features$Embarked <- ifelse(df_train$Embarked == "C", 3,
                            ifelse(df_train$Embarked == "Q",2,1))

#Classificar os valores de Fare.
#features$Fare <- ifelse(features$Fare <= 10, 0, 1)

#Para rodar o modelo vamos primeiro fazer um treinamento e posteriormente um teste
#para coseguirmos fazer uma "validação" de quanto nosso modelo está acertando

set.seed(123)
#data_split <- initial_split(features, prop = 0.7)
#data_train <- training(data_split)
#data_test <- testing(data_split)

data_split <- sample.split(features$Survived, SplitRatio = 0.7)
data_train <- subset(features, data_split == TRUE)
data_test <- subset(features, data_split == FALSE)

#Como separamos os dados em teste e treino, agora iremos aplicar o modelo de
#regressão logistica

model <- train(
  factor(Survived) ~ ., 
  data = data_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

summary(model)

#Verifica a importância das variáveis
vip(model)

#Após o modelo treinado, vamos usar os dados de teste para ver numa matriz de confusão
#o quanto nosso modelo está acertando

pred_class <- predict(model, data_test[-1])
#pred_class <- ifelse(pred_class > 0.5,1,0)

confusionMatrix(pred_class, factor(data_test$Survived))

#Criação da curva ROC
pred <- predict(model, newdata = data_test[-1], type = "prob")
pred <-ifelse(pred[,1] > 0.5, 0 ,1)

ROCpred <- prediction(pred, data_test$Survived)
ROCRperf = performance(ROCpred, "tpr", "fpr")
plot(ROCRperf)
as.numeric(performance(ROCpred, "auc")@y.values)

#rm(data_split, data_test, data_train, df_train, 
#   pred, pred_class, ROCpred, ROCRperf)


#######################Aplicar o modelo de random forest
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                        search = 'grid')
tunegrid <- expand.grid(mtry = c(1:6))
set.seed(123)                                  
model_rf <- train(factor(Survived) ~ ., data = data_train,
                  method = 'rf',
                  trControl = control,
                  tuneGrid = tunegrid)

print(model_rf)
plot(model_rf)

set.seed(123)
model_rf <- randomForest(factor(Survived) ~ ., data = data_train,
                         mtry = 2)

print(model_rf)
plot(model_rf)

pred_rf <- predict(model_rf, newdata = data_test[-1], type = "prob")
pred_rf <-ifelse(pred_rf[,1] > 0.5, 0 ,1)

confusionMatrix(factor(pred_rf), factor(data_test$Survived))

ROCpred_rf <- prediction(pred_rf, data_test$Survived)
ROCRperf_rf = performance(ROCpred_rf, "tpr", "fpr")
plot(ROCRperf_rf)
as.numeric(performance(ROCpred_rf, "auc")@y.values)



#Aplicando o XGBoost





#################GERA O RESULTADO PARA POSTAR NO KAGGLE
list.files()
df_test <- read.csv("test.csv", stringsAsFactors = F)
df <- df_test[,c(1,2,4,5,6,7,9,11)]
names(df)

df$Fare <- ifelse(is.na(df$Fare) == "TRUE", 36.627,
                  df$Fare)
#df$Fare <- ifelse(df$Fare <= 10, 0, 1)
df$Embarked <- ifelse(df$Embarked == "C", 3,
                            ifelse(df$Embarked == "Q",2,1))


summary(df)

set.seed(100)
df$Age <- ifelse(is.na(df$Age) == "TRUE", sample(20:40, 86, replace = T),
                       df$Age)


df$Sex <- ifelse(df$Age <= 15, "Child", df$Sex)
df$Sex <- factor(df$Sex)


resposta <- predict(model_rf, df[-1])

df$Survived <- resposta

submission <- df[,c(1,9)]

write_csv(submission, "submission_rf_4.csv")
