#
# INFNET MIT Big Data - Bloco A - Trabalho de R
# 
# Título do Trabalho : Qualidade do vinhos
# Fonte de dados     : UCI - base wines
# Autor              : Fernando A J Peres
# Data               : 2017-06-09
# Arquivo            : Análise multi variada com gosto / $taste
#
#*******************************************************************************

# ******************************************************************************
# #### SETUP ####
# ******************************************************************************
## work directory path ##
wine.path = "/Users/fernandoperes/dev/r/r-wine/" # to be reused as needed
setwd(wine.path) 
## Sources
source(file =  paste(wine.path, "wines-0-utils.R", sep = ""))
## Libraries
library(party)
library(rpart)
library(rpart.plot) 
library(ipred) 
library(randomForest) 
library(C50) 


# ******************************************************************************
# #### Load prepared data files ####
# ******************************************************************************
load(file="all-wine.Rda") # load(file="red-wine.Rda") # load(file="white-wine.Rda")


# ******************************************************************************
# #### Configure and run ctree algorithm ####
# ******************************************************************************
model.config <- taste ~ fixed.acidity + 
  volatile.acidity + 
  citric.acid + 
  residual.sugar + 
  chlorides + 
  free.sulfur.dioxide +
  total.sulfur.dioxide + 
  density +             
  pH + 
  sulphates +
  alcohol

# Show the config
model.config

# define training and test set
ind <- sample(2, nrow(all.wine), replace = TRUE, prob = c(0.8, 0.2))
trainData <- all.wine[ind == 1,]
testData <- all.wine[ind == 2,]

#number of row of each set
nrow(trainData)
nrow(testData)

#*******************************************************************************
# 1. train: CTREE
#*******************************************************************************
wine.ctree <- ctree(model.config, data = trainData)

# confusion matrix (test set)
wine.ctree.Predict <- predict(wine.ctree, newdata = testData)
(wine.ctree.test.table <- table(wine.ctree.Predict, testData$taste))

# plot confusion matrix results based on test set
par.customized <- par(mfrow = c(1, 2))
plot.model.summary.by.taste(t = wine.ctree.test.table, title = "CTREE - Pelo conjunto de TESTE")

#*******************************************************************************
# 2. train: CART - CLASSIFICATION AND REGRESSION TREES 
# Faz a divisão da árvore com base nos valores que minimizam uma funçãoo de custo,
# como soma dos mínimos quadrados.
#*******************************************************************************

# run algorithm
wine.CART <- rpart(model.config, data = trainData)
summary(wine.CART)

# confusion matrix (test set)
CART.predictions <- predict(wine.CART, testData, type = "class")
wine.CART.table  <- table(CART.predictions, testData$taste)

# plot confusion matrix results based on test set
par.customized <- par(mfrow = c(1, 2))
plot.model.summary.by.taste(t = wine.CART.table, title = "CART - Por toda a base")

#*******************************************************************************
# 3. train: BAGGING CART (Training set x Test Set)
# Faz a divisão da árvore com base nos valores que minimizam uma funçãoo de custo,
# como soma dos mínimos quadrados.
#*******************************************************************************
wine.bagginCart <- bagging(model.config, data = trainData)

# confusion matrix (test set)
bagginCart.predictions <- predict(wine.bagginCart , testData, type = "class")
wine.bagginCart.table  <- table(bagginCart.predictions, testData$taste)

# confusion matrix
wine.bagginCart.table

# plot confusion matrix results based on test set
par.customized <- par(mfrow = c(1, 2))
plot.model.summary.by.taste(t = wine.bagginCart.table, title = "Bagging CART - pela base de teste")

#*******************************************************************************
# 4. train: C5.0 (Training set x Test Set)
# Faz a divisão da árvore com base nos valores que minimizam uma funçãoo de custo,
# como soma dos mínimos quadrados.
#*******************************************************************************
wine.C5.0 <- C5.0(model.config, data = trainData)

# confusion matrix (test set)
C5.0.predictions <- predict(wine.C5.0 , testData, type = "class")
wine.C5.0.table  <- table(C5.0.predictions, testData$taste)

# confusion matrix
wine.C5.0.table

# plot confusion matrix results based on test set
par.customized <- par(mfrow = c(1, 2))
plot.model.summary.by.taste(t = wine.C5.0.table, title = "C5.0- pela base de teste")


#*******************************************************************************
# 5. train: Random forrest
# Faz a divisão da árvore com base nos valores que minimizam uma funçãoo de custo,
# como soma dos mínimos quadrados.
#*******************************************************************************
wine.RF <- randomForest(model.config, data = trainData)

# confusion matrix (test set)
RF.predictions <- predict(wine.RF , testData, type = "class")
wine.RF.table  <- table(RF.predictions, testData$taste)

# confusion matrix
wine.RF.table

# plot confusion matrix results based on test set
par.customized <- par(mfrow = c(1, 2))
plot.model.summary.by.taste(t = wine.RF.table, title = "Random forest - pela base de TESTE")

