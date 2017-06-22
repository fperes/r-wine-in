#
# MIT EM BIG DATA INFNET - BLOCO A - TRABALHO FINAL DE R - PROFESSOR CÁSSIUS
# 
# Título do Trabalho : Identificação da Qualidade de vinhos
# Fonte de dados     : UCI - base wines
# Autor              : Fernando A J Peres
# Data               : 2017-06-09
# Arquivo            : Remover outliers e salvar base sem outliers
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
library(dplyr)
library(ggplot2)

# ******************************************************************************
# #### Load prepared data files ####
# ******************************************************************************
load(file="all-wine.Rda") # load(file="red-wine.Rda") # load(file="white-wine.Rda")
