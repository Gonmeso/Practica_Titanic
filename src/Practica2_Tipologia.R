## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- include=FALSE------------------------------------------------------
library(ggplot2)
library(randomForest)
library(gridExtra)
library(lawstat)

titanic <- read.csv('Data/train.csv', stringsAsFactors = FALSE, na.strings = '')
## ------------------------------------------------------------------------
# Resumen de las variables
summary(titanic)

## ------------------------------------------------------------------------
# Estructura de las variables
str(titanic)

## ------------------------------------------------------------------------
titanic$Sex[titanic$Sex == 'male'] <- 1
titanic$Sex[titanic$Sex == 'female'] <- 0
titanic$Sex <- as.numeric(titanic$Sex)

## ------------------------------------------------------------------------
sapply(titanic, function(x) sum(is.na(x)))

## ------------------------------------------------------------------------
# Observamos el valor más común
table(titanic$Embarked)

# Se imputa a este valor los NA
titanic$Embarked[is.na(titanic$Embarked)] <- 'S'

## ------------------------------------------------------------------------
titanic$Cabin <- NULL

## ------------------------------------------------------------------------
# Utilizamos un randomForest rápido para generar un modelo para imputar la edad
set.seed(101)
modelVars <- c('Survived', 'Pclass', 'Sex', 'SibSp',
                                         'Parch')
ageModel <- randomForest(formula = Age ~ Survived + Pclass + Sex + SibSp + Parch, 
                         data = titanic[!is.na(titanic$Age), c('Age',modelVars)])
predictedAge <- predict(ageModel, titanic[is.na(titanic$Age),
                                      modelVars])

titanic$Age[is.na(titanic$Age)] <- round(predictedAge)

## ------------------------------------------------------------------------
sapply(titanic, function(x) sum(is.na(x)))

## ------------------------------------------------------------------------
sapply(titanic, function(x) sum(x == 0))

## ------------------------------------------------------------------------
titanic[titanic$Fare == 0,]

## ------------------------------------------------------------------------
pclassMedianas <- sapply(split(titanic[titanic$Fare != 0,'Fare'],
                               f = titanic$Pclass[titanic$Fare != 0]),
                         median)

titanic[titanic$Fare == 0 & titanic$Pclass == 1,'Fare'] <- pclassMedianas[1]
titanic[titanic$Fare == 0 & titanic$Pclass == 2,'Fare'] <- pclassMedianas[2]
titanic[titanic$Fare == 0 & titanic$Pclass == 3,'Fare'] <- pclassMedianas[3]


## ------------------------------------------------------------------------
varsNum <- c('PassengerId', 'Age', 'SibSp', 'Parch', 'Fare')

plotList <- vector(mode = 'list', length = length(varsNum))

for(variable in varsNum){
   p <- ggplot(titanic, aes(y = titanic[[variable]], x = variable))+
          geom_boxplot()+
          theme_bw()+
          ylab('value')+
          xlab(variable)
   plot(p)
}

## ------------------------------------------------------------------------
titanic[titanic$Fare == max(titanic$Fare),'Fare'] <- pclassMedianas[1]

## ------------------------------------------------------------------------
for(variable in varsNum){
  
  densityplot <- ggplot(titanic, aes(x = titanic[[variable]]))+
                geom_density()+
                xlab(variable)
  qq <- ggplot(titanic, aes(sample = titanic[[variable]]))+
            geom_qq()+
            geom_qq_line()
  grid.arrange(densityplot, qq, ncol=2)
  
  print(paste0("Análisis utilizando el método Shapiro-Wilk’s para la variable",
               variable))
  
  print(shapiro.test(titanic[[variable]]))
}

## ------------------------------------------------------------------------
for(variable in names(titanic[,sapply(titanic, is.numeric)])){
  
  print(paste0("Análisis de homogeneidad de variancias con el test de Levene para la variable ",
               variable))
  print(levene.test(titanic[[variable]], titanic$Survived))
  
}

## ------------------------------------------------------------------------
corrVar <- cor(titanic[,sapply(titanic, is.numeric)])
print(as.data.frame(corrVar))
heatmap(corrVar, Rowv=NA, Colv=NA)

## ------------------------------------------------------------------------
set.seed(101)
titanic$Name <- NULL
titanic$Embarked <- factor(titanic$Embarked)
titanic$Ticket <- NULL
titanic$Survived <- factor(titanic$Survived)
titanic$Pclass <- factor(titanic$Pclass)
titanic$Sex <- factor(titanic$Sex)
rf1 <- randomForest(factor(Survived) ~ ., titanic)
rf2 <- randomForest(factor(Survived) ~ Pclass + Sex + Age, titanic)
rf3 <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch, titanic)
rf4 <- randomForest(factor(Survived) ~ Pclass + Sex + Age + Fare + Embarked, titanic)

for(i in 1:4){
  print(paste("Matriz de confusión para el modelo rf", i))
  print(eval(parse(text = paste0('rf',i,'$confusion'))))
}

## ------------------------------------------------------------------------
b1 <- ggplot(titanic, aes(x = Pclass, fill = Survived))+
        geom_bar()

b2 <- ggplot(titanic, aes(x = Sex, fill = Survived))+
        geom_bar()

b3 <- ggplot(titanic, aes(x = Survived, y = Fare, fill=Survived))+
       geom_boxplot()

b4 <- ggplot(titanic, aes(x = Survived, y = Age, fill=Survived))+
      geom_boxplot()

grid.arrange(b1, b2, b3, b4, ncol=2)

write.csv(titanic, '../Data/cleanTitanic.csv')