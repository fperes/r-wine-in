#
# INFNET MIT Big Data - Bloco A - Trabalho de R
# 
# Título do Trabalho : Qualidade do vinhos
# Fonte de dados     : UCI - base wines
# Autor              : Fernando A J Peres
# Data               : 2017-06-09
# Arquivo            : Análise de outliers
#
#*******************************************************************************

# Análise entre variável dependente e independentes
# A T R I B U T O S:
# 01. fixed.acidity        = Acidez fixa
# 02. volatile.acidity     = Acidez volátil
# 03. citric.acid          = Ácido cítrico
# 04. residual.sugar       = Açucar residual
# 05. chlorides            = Cloretos
# 06. free.sulfur.dioxide  = Livre de dióxido de enxofre
# 07. total.sulfur.dioxide = Total de dióxido de enxofre
# 08. density              = Densidade
# 09. pH                   = pH             
# 10. sulphates            = Sulfatos
# 11. alcohol              = Teor alcólico
# 12. quality              = Qualidade
# 13. color
# 14. col.color
# 15. taste
# 16. taste.color


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


# ******************************************************************************
# #### 1. FIXED ACIDITY ####
# ******************************************************************************
# <<< Acidez fixa >>>

## initialization (decrease rework) 
x = all.wine$fixed.acidity
field.label = wine.fields.fixed.acidity
field.name  = "fixed.acidity"

#calc limits
sno.lno = wine.sno.lno(x)

# mark ouliers
table(all.wine$outlier) # before mark outliers
all.wine$outlier <- wine.mark.outlier(start = TRUE, df = all.wine, 
                                      field = field.name, sno.lno = sno.lno)
table(all.wine$outlier) # after marked outliers

# Get the subset of ALL.WINE excluding marked outliers
all.wine.non.outliers <- all.wine %>% filter(all.wine$outlier == FALSE)
x2 <- all.wine.non.outliers$fixed.acidity

# Preparing to plot charts
## distribution plot resuls
par.customized <- par(mfrow = c(1, 2))

# Plot field distribution with ALL (including outliers) 
title = paste(field.label, " - com outliers presentes", sep = "")
wine.distribution.plot(title = title, x = x, field.label = field.label,
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red")

# Plot field distribution excluding outliers) 
title = paste(field.label, " - com outliers removidos", sep = "")
wine.distribution.plot(title = title, x = x2, field.label = field.label, 
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red" )

## plot the difference all X witout outliers
par.customized <- par(mfrow = c(1, 2))

title = paste(field.label, " - status de outliers", sep = "")
wine.outliers.summary.plot(title = title, t = table(all.wine$outlier), 
                           colors = c("green", "dark red"))


# ******************************************************************************

# #### 2. VOLATILE ACIDITY ####
# ******************************************************************************
# The amount of acetic acid in wine, which at too high of levels can lead to an unpleasant, vinegar taste
# <<< Volatile acidity >>>

## initialization (decrease rework) 
x = all.wine$volatile.acidity
field.label = wine.fields.volatile.acidity
field.name  = "volatile.acidity"

#calc limits
sno.lno = wine.sno.lno(x)

# mark ouliers
table(all.wine$outlier) # before mark outliers
all.wine$outlier <- wine.mark.outlier(start = TRUE, df = all.wine, 
                                      field = field.name, sno.lno = sno.lno)
table(all.wine$outlier) # after marked outliers

# Get the subset of ALL.WINE excluding marked outliers
all.wine.non.outliers <- all.wine %>% filter(all.wine$outlier == FALSE)
x2 <- all.wine.non.outliers$volatile.acidity

# Preparing to plot charts
## distribution plot resuls
par.customized <- par(mfrow = c(1, 2))

# Plot field distribution with ALL (including outliers) 
title = paste(field.label, " - com outliers presentes", sep = "")
wine.distribution.plot(title = title, x = x, field.label = field.label,
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red")

# Plot field distribution excluding outliers) 
title = paste(field.label, " - com outliers removidos", sep = "")
wine.distribution.plot(title = title, x = x2, field.label = field.label, 
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red" )

## plot the difference all X witout outliers
par.customized <- par(mfrow = c(1, 2))

title = paste(field.label, " - status de outliers", sep = "")
wine.outliers.summary.plot(title = title, t = table(all.wine$outlier), 
                           colors = c("green", "dark red"))

# Ácido cítrico


# ******************************************************************************
# #### 3. CITRIC ACID ####
# ******************************************************************************
# Found in small quantities, citric acid can add ‘freshness’ and flavor to wines
# <<< Ácido cítrico >>>

## initialization (decrease rework) 
x = all.wine$citric.acid
field.label = wine.fields.citric.acid
field.name  = "citric.acid"

#calc limits
sno.lno = wine.sno.lno(x)

# mark ouliers
table(all.wine$outlier) # before mark outliers
all.wine$outlier <- wine.mark.outlier(start = TRUE, df = all.wine, 
                                      field = field.name, sno.lno = sno.lno)
table(all.wine$outlier) # after marked outliers

# Get the subset of ALL.WINE excluding marked outliers
all.wine.non.outliers <- all.wine %>% filter(all.wine$outlier == FALSE)
x2 <- all.wine.non.outliers$citric.acid

# Preparing to plot charts
## distribution plot resuls
par.customized <- par(mfrow = c(1, 2))

# Plot field distribution with ALL (including outliers) 
title = paste(field.label, " - com outliers presentes", sep = "")
wine.distribution.plot(title = title, x = x, field.label = field.label,
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red")

# Plot field distribution excluding outliers) 
title = paste(field.label, " - com outliers removidos", sep = "")
wine.distribution.plot(title = title, x = x2, field.label = field.label, 
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red" )

## plot the difference all X witout outliers
par.customized <- par(mfrow = c(1, 2))

title = paste(field.label, " - status de outliers", sep = "")
wine.outliers.summary.plot(title = title, t = table(all.wine$outlier), 
                           colors = c("green", "dark red"))


# ******************************************************************************
# #### 4. RESIDUAL SUGAR ####
# ******************************************************************************
# The amount of sugar remaining after fermentation stops, it’s rare to find wines with less than 1 gram/liter and wines with greater than 45 grams/liter are considered sweet
# <<< Açucar residual >>>

## initialization (decrease rework) 
x = all.wine$residual.sugar
field.label = wine.fields.residual.sugar
field.name  = "residual.sugar"

#calc limits
sno.lno = wine.sno.lno(x)

# mark ouliers
table(all.wine$outlier) # before mark outliers
all.wine$outlier <- wine.mark.outlier(start = TRUE, df = all.wine, 
                                      field = field.name, sno.lno = sno.lno)
table(all.wine$outlier) # after marked outliers

# Get the subset of ALL.WINE excluding marked outliers
all.wine.non.outliers <- all.wine %>% filter(all.wine$outlier == FALSE)
x2 <- all.wine.non.outliers$residual.sugar

# Preparing to plot charts
## distribution plot resuls
par.customized <- par(mfrow = c(1, 2))

# Plot field distribution with ALL (including outliers) 
title = paste(field.label, " - com outliers presentes", sep = "")
wine.distribution.plot(title = title, x = x, field.label = field.label,
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red")

# Plot field distribution excluding outliers) 
title = paste(field.label, " - com outliers removidos", sep = "")
wine.distribution.plot(title = title, x = x2, field.label = field.label, 
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red" )

## plot the difference all X witout outliers
par.customized <- par(mfrow = c(1, 2))

title = paste(field.label, " - status de outliers", sep = "")
wine.outliers.summary.plot(title = title, t = table(all.wine$outlier), 
                           colors = c("green", "dark red"))

# Cloretos

# ******************************************************************************
# #### 5. CHLORIDES ####
# ******************************************************************************
# The amount of salt in the wine
# <<< Cloretos >>>

## initialization (decrease rework) 
x = all.wine$chlorides
field.label = wine.fields.chlorides
field.name  = "chlorides"

#calc limits
sno.lno = wine.sno.lno(x)

# mark ouliers
table(all.wine$outlier) # before mark outliers
all.wine$outlier <- wine.mark.outlier(start = TRUE, df = all.wine, 
                                      field = field.name, sno.lno = sno.lno)
table(all.wine$outlier) # after marked outliers

# Get the subset of ALL.WINE excluding marked outliers
all.wine.non.outliers <- all.wine %>% filter(all.wine$outlier == FALSE)
x2 <- all.wine.non.outliers$chlorides

# Preparing to plot charts
## distribution plot resuls
par.customized <- par(mfrow = c(1, 2))

# Plot field distribution with ALL (including outliers) 
title = paste(field.label, " - com outliers presentes", sep = "")
wine.distribution.plot(title = title, x = x, field.label = field.label,
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red")

# Plot field distribution excluding outliers) 
title = paste(field.label, " - com outliers removidos", sep = "")
wine.distribution.plot(title = title, x = x2, field.label = field.label, 
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red" )

## plot the difference all X witout outliers
par.customized <- par(mfrow = c(1, 2))

title = paste(field.label, " - status de outliers", sep = "")
wine.outliers.summary.plot(title = title, t = table(all.wine$outlier), 
                           colors = c("green", "dark red"))

# Livre de dióxido de enxofre

# ******************************************************************************
# #### 6. FREE SULFUR DIOXIDE ####
# ******************************************************************************
# The free form of SO2 exists in equilibrium between molecular SO2 (as a dissolved gas) and bisulfite ion; it prevents microbial growth and the oxidation of wine
# <<< Livre de dioxido de enxofre >>>

## initialization (decrease rework) 
x = all.wine$free.sulfur.dioxide
field.label = wine.fields.free.sulfur.dioxide
field.name  = "free.sulfur.dioxide"

#calc limits
sno.lno = wine.sno.lno(x)

# mark ouliers
table(all.wine$outlier) # before mark outliers
all.wine$outlier <- wine.mark.outlier(start = TRUE, df = all.wine, 
                                      field = field.name, sno.lno = sno.lno)
table(all.wine$outlier) # after marked outliers

# Get the subset of ALL.WINE excluding marked outliers
all.wine.non.outliers <- all.wine %>% filter(all.wine$outlier == FALSE)
x2 <- all.wine.non.outliers$free.sulfur.dioxide

# Preparing to plot charts
## distribution plot resuls
par.customized <- par(mfrow = c(1, 2))

# Plot field distribution with ALL (including outliers) 
title = paste(field.label, " - com outliers presentes", sep = "")
wine.distribution.plot(title = title, x = x, field.label = field.label,
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red")

# Plot field distribution excluding outliers) 
title = paste(field.label, " - com outliers removidos", sep = "")
wine.distribution.plot(title = title, x = x2, field.label = field.label, 
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red" )

## plot the difference all X witout outliers
par.customized <- par(mfrow = c(1, 2))

title = paste(field.label, " - status de outliers", sep = "")
wine.outliers.summary.plot(title = title, t = table(all.wine$outlier), 
                           colors = c("green", "dark red"))

# Total de dióxido de enxofre


# ******************************************************************************
# #### 7. TOTAL SULFUR DIOXIDE ####
# ******************************************************************************
# Amount of free and bound forms of S02; in low concentrations, SO2 is mostly undetectable in wine, but at free SO2 concentrations over 50 ppm, SO2 becomes evident in the nose and taste of wine
# <<< Total de dioxido de enxofre >>>

## initialization (decrease rework) 
x = all.wine$total.sulfur.dioxide
field.label = wine.fields.total.sulfur.dioxide
field.name  = "total.sulfur.dioxide"

# calc limits
sno.lno = wine.sno.lno(x)

# mark ouliers
table(all.wine$outlier) # before mark outliers
all.wine$outlier <- wine.mark.outlier(start = TRUE, df = all.wine, 
                                      field = field.name, sno.lno = sno.lno)
table(all.wine$outlier) # after marked outliers

# Get the subset of ALL.WINE excluding marked outliers
all.wine.non.outliers <- all.wine %>% filter(all.wine$outlier == FALSE)
x2 <- all.wine.non.outliers$total.sulfur.dioxide

# Preparing to plot charts
## distribution plot resuls
par.customized <- par(mfrow = c(1, 2))

# Plot field distribution with ALL (including outliers) 
title = paste(field.label, " - com outliers presentes", sep = "")
wine.distribution.plot(title = title, x = x, field.label = field.label,
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red")

# Plot field distribution excluding outliers) 
title = paste(field.label, " - com outliers removidos", sep = "")
wine.distribution.plot(title = title, x = x2, field.label = field.label, 
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red" )

## plot the difference all X witout outliers
par.customized <- par(mfrow = c(1, 2))

title = paste(field.label, " - status de outliers", sep = "")
wine.outliers.summary.plot(title = title, t = table(all.wine$outlier), 
                           colors = c("green", "dark red"))

# Densidade

# ******************************************************************************
# #### 8. DENSITY ####
# ******************************************************************************
# Density of water is close to that of water depending on the percent alcohol and sugar content
# <<< Densidade >>>

## initialization (decrease rework) 
x = all.wine$density
field.label = wine.fields.density
field.name  = "density"

#calc limits
sno.lno = wine.sno.lno(x)

# mark ouliers
table(all.wine$outlier) # before mark outliers
all.wine$outlier <- wine.mark.outlier(start = TRUE, df = all.wine, 
                                      field = field.name, sno.lno = sno.lno)
table(all.wine$outlier) # after marked outliers

# Get the subset of ALL.WINE excluding marked outliers
all.wine.non.outliers <- all.wine %>% filter(all.wine$outlier == FALSE)
x2 <- all.wine.non.outliers$density

# Preparing to plot charts
## distribution plot resuls
par.customized <- par(mfrow = c(1, 2))

# Plot field distribution with ALL (including outliers) 
title = paste(field.label, " - com outliers presentes", sep = "")
wine.distribution.plot(title = title, x = x, field.label = field.label,
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red")

# Plot field distribution excluding outliers) 
title = paste(field.label, " - com outliers removidos", sep = "")
wine.distribution.plot(title = title, x = x2, field.label = field.label, 
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red" )

## plot the difference all X witout outliers
par.customized <- par(mfrow = c(1, 2))

title = paste(field.label, " - status de outliers", sep = "")
wine.outliers.summary.plot(title = title, t = table(all.wine$outlier), 
                           colors = c("green", "dark red"))
# pH

# ******************************************************************************
# #### 9. pH ####
# ******************************************************************************
# Describes how acidic or basic a wine is on a scale from 0 (very acidic) to 14 (very basic); most wines are between 3-4 on the pH scale
# <<< pH >>>

## initialization (decrease rework) 
x = all.wine$fixed.pH
field.label = wine.fields.pH
field.name  = "pH"
sno.lno = wine.sno.lno(x)

# mark ouliers
table(all.wine$outlier) # before mark outliers
all.wine$outlier <- wine.mark.outlier(start = TRUE, df = all.wine, 
                                      field = field.name, sno.lno = sno.lno)
table(all.wine$outlier) # after marked outliers

# Get the subset of ALL.WINE excluding marked outliers
all.wine.non.outliers <- all.wine %>% filter(all.wine$outlier == FALSE)
x2 <- all.wine.non.outliers$pH

# Preparing to plot charts
## distribution plot resuls
par.customized <- par(mfrow = c(1, 2))

# Plot field distribution with ALL (including outliers) 
title = paste(field.label, " - com outliers presentes", sep = "")
wine.distribution.plot(title = title, x = x, field.label = field.label,
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red")

# Plot field distribution excluding outliers) 
title = paste(field.label, " - com outliers removidos", sep = "")
wine.distribution.plot(title = title, x = x2, field.label = field.label, 
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red" )

## plot the difference all X witout outliers
par.customized <- par(mfrow = c(1, 2))

title = paste(field.label, " - status de outliers", sep = "")
wine.outliers.summary.plot(title = title, t = table(all.wine$outlier), 
                           colors = c("green", "dark red"))

# Sulfato



# ******************************************************************************
# #### 10. SULPHATES ####
# ******************************************************************************
# A wine additive which can contribute to sulfur dioxide gas (S02) levels, wich acts as an antimicrobial and antioxidant
# <<< Sulfato >>>

## initialization (decrease rework) 
x = all.wine$sulphates
field.label = wine.fields.sulphates
field.name  = "sulphates"
sno.lno = wine.sno.lno(x)

# mark ouliers
table(all.wine$outlier) # before mark outliers
all.wine$outlier <- wine.mark.outlier(start = TRUE, df = all.wine, 
                                      field = field.name, sno.lno = sno.lno)
table(all.wine$outlier) # after marked outliers

# Get the subset of ALL.WINE excluding marked outliers
all.wine.non.outliers <- all.wine %>% filter(all.wine$outlier == FALSE)
x2 <- all.wine.non.outliers$sulphates

# Preparing to plot charts
## distribution plot resuls
par.customized <- par(mfrow = c(1, 2))

# Plot field distribution with ALL (including outliers) 
title = paste(field.label, " - com outliers presentes", sep = "")
wine.distribution.plot(title = title, x = x, field.label = field.label,
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red")

# Plot field distribution excluding outliers) 
title = paste(field.label, " - com outliers removidos", sep = "")
wine.distribution.plot(title = title, x = x2, field.label = field.label, 
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red" )

## plot the difference all X witout outliers
par.customized <- par(mfrow = c(1, 2))

title = paste(field.label, " - status de outliers", sep = "")
wine.outliers.summary.plot(title = title, t = table(all.wine$outlier), 
                           colors = c("green", "dark red"))

# Teor Álcólico

# ******************************************************************************
# #### 11. ALCOHOL ####
# ******************************************************************************
# The percent of alcohol
# <<< Teor alcólico >>>

## initialization (decrease rework) 
x = all.wine$alcohol
field.label = wine.fields.alcohol
field.name  = "alcohol"

#calc limits
sno.lno = wine.sno.lno(x)

# mark ouliers
table(all.wine$outlier) # before mark outliers
all.wine$outlier <- wine.mark.outlier(start = TRUE, df = all.wine, 
                                      field = field.name, sno.lno = sno.lno)
table(all.wine$outlier) # after marked outliers

# Get the subset of ALL.WINE excluding marked outliers
all.wine.non.outliers <- all.wine %>% filter(all.wine$outlier == FALSE)
x2 <- all.wine.non.outliers$alcohol

# Preparing to plot charts
## distribution plot resuls
par.customized <- par(mfrow = c(1, 2))

# Plot field distribution with ALL (including outliers) 
title = paste(field.label, " - com outliers presentes", sep = "")
wine.distribution.plot(title = title, x = x, field.label = field.label,
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red")

# Plot field distribution excluding outliers) 
title = paste(field.label, " - com outliers removidos", sep = "")
wine.distribution.plot(title = title, x = x2, field.label = field.label, 
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red" )

## plot the difference all X witout outliers
par.customized <- par(mfrow = c(1, 2))

title = paste(field.label, " - status de outliers", sep = "")
wine.outliers.summary.plot(title = title, t = table(all.wine$outlier), 
                           colors = c("green", "dark red"))


# #### 12. QUALITY ####
# ******************************************************************************
# The qualty of the wine based on perception (Rate 0..10)
# <<< qualidade >>>

## initialization (decrease rework) 
x = all.wine$quality
field.label = wine.fields.quality
field.name  = "quality"

#calc limits
sno.lno = wine.sno.lno(x)

# mark ouliers
table(all.wine$outlier) # before mark outliers
all.wine$outlier <- wine.mark.outlier(start = TRUE, df = all.wine, 
                                      field = field.name, sno.lno = sno.lno)
table(all.wine$outlier) # after marked outliers

# Get the subset of ALL.WINE excluding marked outliers
all.wine.non.outliers <- all.wine %>% filter(all.wine$outlier == FALSE)
x2 <- all.wine.non.outliers$quality

# Preparing to plot charts
## distribution plot resuls
par.customized <- par(mfrow = c(1, 2))

# Plot field distribution with ALL (including outliers) 
title = paste(field.label, " - com outliers presentes", sep = "")
wine.distribution.plot(title = title, x = x, field.label = field.label,
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red")

# Plot field distribution excluding outliers) 
title = paste(field.label, " - com outliers removidos", sep = "")
wine.distribution.plot(title = title, x = x2, field.label = field.label, 
                       color =  wine.color.all, xlim = c(min(x), max(x)), 
                       sno.lno = sno.lno, line.color = "red" )

## plot the difference all X witout outliers
par.customized <- par(mfrow = c(1, 2))

title = paste(field.label, " - status de outliers", sep = "")
wine.outliers.summary.plot(title = title, t = table(all.wine$outlier), 
                           colors = c("green", "dark red"))






