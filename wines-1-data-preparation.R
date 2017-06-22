
#
# INFNET MIT Big Data - Bloco A - Trabalho de R
# 
# Título do Trabalho : Qualidade do vinhos
# Fonte de dados     : UCI - base wines
# Autor              : Fernando A J Peres
# Data               : 2017-06-09
# Arquivo            : Preparação dos dados 
#
#*******************************************************************************


# Há dois datasets neste estudo, um sobre vinho tinto e outro sobre vinho branco.
# Cojunto de Dados:
# 1. Vinho Tinto (*winequality-red.csv*) com w = 1599 observações.
# 2. Vinho Branco (*winequality-white.csv*) com w = 4898 observações.
# TOTAL DE OBSERVAÇÕES W = 6497 observações.
#
# Mais informação no documento "wines-data-preparation.Rmd" ou  "wines-data-preparation.html"
#
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
# #### LOAD DATA ####
# ******************************************************************************
white.wine = read.csv("winequality-white.csv", sep = ";", header = TRUE)
red.wine   = read.csv("winequality-red.csv", sep = ";", header = TRUE)

#Listar as colunas para ver que são as mesmas

names(white.wine)
names(red.wine)

# Etapa 3: Adicionar a coluna cor do vinho **$color** aos *data frames*

red.wine$color       = wine.red.label #"red"            # string - color of the wine 
white.wine$color     = wine.white.label #"white"          # string - color of the wine

# Etapa 4: Coluna para cor de impresão de gráficos

red.wine$col.color   = wine.color.red     # color code 
white.wine$col.color = wine.color.white   #color code 

wine.color.white
wine.color.red

# Etapa 5: Avaliar a distribuição de qualidade antes de criar as categorias qualidade

# Vinho Tinto

par.customized <- par(mfrow=c(1,2))#, family = "Lucida Console") 

# barplot(main = "Quartis", quantile(all.wine$quality), ylim = c(0,10), col = wine.color.red)

boxplot(x = red.wine$quality, xlab = "Qualidade", 
        col = wine.color.red,
        main = "[Tinto] Qualidade do Vinho", # col.main = wine.color.red,
        cex.axis = 0.75, cex.lab  = 0.75,   cex.main = 1, 
        horizontal = T, 
        frame = F)

h <- hist(x = red.wine$quality, xlab = "Qualidade", 
          main = "", col.main = wine.color.red, cex.main = 1, adj = 0,
          ylab = "Frequência", ylim = c(0, 900),
          col  = wine.color.red,
          include.lowest = TRUE,
          cex.axis = 0.75, cex.lab  = 0.75,   cex.main = 1,
          labels = TRUE) #prob = TRUE)

# curva normal
xfit <- seq(min(red.wine$quality), max(red.wine$quality), length = 40) 
yfit <- dnorm(xfit, mean = mean(red.wine$quality), sd = sd(red.wine$quality))
yfit <- yfit * diff(h$mids[1:2]) * length(red.wine$quality) 
lines(xfit, yfit, col = "blue", lwd = 2)


# Vinho branco

par.customized <- par(mfrow=c(1,2))

boxplot(x = white.wine$quality, xlab = "Qualidade", 
        col = wine.color.white,
        main = "[Branco] Qualidade do Vinho",
        cex.axis = 0.75, cex.lab  = 0.75,   cex.main = 1,
        horizontal = T, 
        frame = F)

h <- hist(main = "",
          white.wine$quality, 
          xlab = "Qualidade", 
          ylab = "Frequência", ylim = c(0, 2500),
          cex.axis = 0.75, cex.lab  = 0.75,   cex.main = 1,
          col  = wine.color.white,
          include.lowest = TRUE,
          labels = TRUE) #prob = TRUE)

# curva normal
xfit <- seq(min(white.wine$quality), max(white.wine$quality), length = 40) 
yfit <- dnorm(xfit, mean = mean(white.wine$quality), sd = sd(white.wine$quality))
yfit <- yfit * diff(h$mids[1:2]) * length(white.wine$quality) 
lines(xfit, yfit, col = "blue", lwd = 2)

# Definição das Quebras

par.customized <- par(mfrow = c (2, 1))

boxplot(x = red.wine$quality, xlab = "Qualidade", 
        col = wine.color.red,
        main = "[Tinto] Qualidade do Vinho",
        cex.axis = 0.75, cex.lab  = 0.75,   cex.main = 1, 
        horizontal = T, 
        frame = F,
        ylim = c(3,10))

boxplot(x = white.wine$quality, xlab = "Qualidade", 
        col = wine.color.white,
        main = "[Branco] Qualidade do Vinho",
        cex.axis = 0.75, cex.lab  = 0.75,   cex.main = 1,
        horizontal = T, 
        frame = F,
        ylim = c(3,10))


# Etapa 6: Criar o atributo gosto 

red.wine$taste   = ifelse(red.wine$quality < 6, "ruim", "bom")
red.wine$taste[red.wine$quality == 6]     =  "regular"
red.wine$taste   <- as.factor(red.wine$taste)

# Vinho tinto  - Como ficou o atributo **gosto / $taste**
table(red.wine$taste)

# Vinho tinto - Gráfico de Pizza para **gosto / $taste **
red.wine.tastes.summary = table(red.wine$taste)
red.wine.tastes.percent = 100 * red.wine.tastes.summary / sum(red.wine.tastes.summary)
red.wine.tastes.percent = round(red.wine.tastes.percent, digits = 2)

labs   = c(paste("Bom - ", red.wine.tastes.percent[1], "%", sep = ""), 
           paste("Regular - ", red.wine.tastes.percent[2], "%", sep = ""), 
           paste("Ruim - ", red.wine.tastes.percent[3], "%", sep = ""))

pie(x = red.wine.tastes.percent, 
    labels = labs,
    cex.axis = 0.75, cex.lab  = 0.75,   cex.main = 1, 
    main ="[Tinto] Gosto (Qualidade 0-10 categorizada)",
    col = c(wine.good.color, wine.regular.color, wine.bad.color)) #c("green", " yellow", "dark red"))

# Vinho branco - **gosto / $taste **
white.wine$taste = ifelse(white.wine$quality < 6, "ruim", "bom")
white.wine$taste[white.wine$quality == 6] =  "regular"
white.wine$taste <- as.factor(white.wine$taste)

# Vinho branco  - Como ficou o atributo **gosto / $taste**
table(white.wine$taste)


#### Vinho branco - Gráfico de Pizza para **gosto / $taste **
white.wine.tastes.summary = table(white.wine$taste)
white.wine.tastes.percent = 100 * white.wine.tastes.summary / sum(white.wine.tastes.summary)
white.wine.tastes.percent = round(white.wine.tastes.percent, digits = 2)

labs   = c(paste("Bom - ", white.wine.tastes.percent[1], "%", sep = ""), 
           paste("Regular - ", white.wine.tastes.percent[2], "%", sep = ""), 
           paste("Ruim - ", white.wine.tastes.percent[3], "%", sep = ""))

pie(x = white.wine.tastes.percent, 
    labels = labs, 
    cex.axis = 0.75, cex.lab  = 0.75,   cex.main = 1,
    main ="[Branco] Gosto (Qualidade 0-10 categorizada)",
    col = c(wine.good.color, wine.regular.color, wine.bad.color)) #c("green", " yellow", "dark red"))

## Etapa 7: Cores para o gosto (conceito) do vinho

### Red 
red.wine$taste.color <- ifelse(red.wine$taste == "bom", wine.good.color, wine.regular.color)
red.wine$taste.color[red.wine$taste == "ruim"] <- wine.bad.color
red.wine$taste.color <- as.factor(red.wine$taste.color)

# White
white.wine$taste.color <- ifelse(white.wine$taste == "bom", wine.good.color, wine.regular.color)
white.wine$taste.color[white.wine$taste  == "ruim"] <- wine.bad.color
white.wine$taste.color <- as.factor(white.wine$taste.color)

#colors()

names(red.wine)
names(white.wine)
#Etapa 8: Juntar os *data frames* de vinho tinto e branco
all.wine <- rbind(white.wine, red.wine)

# Etapa 9: Salvar os *data frames* transformados
save(red.wine, file="red-wine.Rda")
save(white.wine, file="white-wine.Rda")
save(all.wine, file="all-wine.Rda")

# Vinhos tintos
names(red.wine)   # List of fields of Red-wines

#Vinhos brancos
names(white.wine) # List of fields of White-wines

# Todos os vinhos
names(all.wine)   # List of fields of All-wines

## Etapa 10: Todos os vinhos - Como ficou o atributo **gosto / $taste**
table(all.wine$taste)
all.wine$taste.color

#### Todos os vinhos - Gráfico de Pizza para **gosto / $taste**
all.wine.tastes.summary = table(all.wine$taste)
all.wine.tastes.percent = 100 * all.wine.tastes.summary / sum(all.wine.tastes.summary)
all.wine.tastes.percent = round(all.wine.tastes.percent, digits = 2)

labs   = c(paste("Bom - ", all.wine.tastes.percent[1], "%", sep = ""), 
           paste("Regular - ", all.wine.tastes.percent[2], "%", sep = ""), 
           paste("Ruim - ", all.wine.tastes.percent[3], "%", sep = ""))

pie(x = all.wine.tastes.percent, 
    labels = labs, 
    cex.axis = 0.75, cex.lab  = 0.75,   cex.main = 1,
    main ="[Todos] Gosto (Qualidade 0-10 categorizada)",
    col = c(wine.good.color, wine.regular.color, wine.bad.color)) #c("green", " yellow", "dark red"))

# Todos os vinhos
par.customized <- par(mfrow=c(1,2))

boxplot(main = "[Todos] Qualidade do Vinho", 
        all.wine$quality, horizontal = T, col = wine.color.all)

h <- hist(main = "",
          all.wine$quality, 
          xlab = "Qualidade", 
          ylab = "Frequência", ylim = c(0, 3200),
          col  = "gray",
          include.lowest = TRUE,
          cex.axis = 0.75, cex.lab  = 0.75,   cex.main = 1,
          labels = TRUE) #prob = TRUE)

# curva normal
xfit <- seq(min(all.wine$quality), max(all.wine$quality), length = 40) 
yfit <- dnorm(xfit, mean = mean(all.wine$quality), sd = sd(all.wine$quality))
yfit <- yfit * diff(h$mids[1:2]) * length(all.wine$quality) 
lines(xfit, yfit, col = "blue", lwd = 2)

# Resumo:

# Taste
par.customized <- par(mfrow=c(3,1))

labs   = c(paste("Bom - ", all.wine.tastes.percent[1], "%", sep = ""), 
           paste("Regular - ", all.wine.tastes.percent[2], "%", sep = ""), 
           paste("Ruim - ", all.wine.tastes.percent[3], "%", sep = ""))

pie(x = all.wine.tastes.percent, 
    labels = labs, 
    main ="[Todos] Gosto",
    cex.axis = 0.75, cex.lab  = 0.75, cex.main = 1,
    c(wine.good.color, wine.regular.color, wine.bad.color)) #col = c("green", " yellow", "dark red"))

# red
labs   = c(paste("Bom - ", red.wine.tastes.percent[1], "%", sep = ""), 
           paste("Regular - ", red.wine.tastes.percent[2], "%", sep = ""), 
           paste("Ruim - ", red.wine.tastes.percent[3], "%", sep = ""))

pie(x = red.wine.tastes.percent, 
    labels = labs, 
    main ="[Tinto] Gosto",
    cex.axis = 0.75, cex.lab  = 0.75, cex.main = 1,
    c(wine.good.color, wine.regular.color, wine.bad.color)) #col = c("green", " yellow", "dark red"))

# white
labs   = c(paste("Bom - ", white.wine.tastes.percent[1], "%", sep = ""), 
           paste("Regular - ", white.wine.tastes.percent[2], "%", sep = ""), 
           paste("Ruim - ", white.wine.tastes.percent[3], "%", sep = ""))

pie(x = white.wine.tastes.percent, 
    labels = labs, 
    main ="[Branco] Gosto",
    cex.axis = 0.75, cex.lab  = 0.75, cex.main = 1,
    c(wine.good.color, wine.regular.color, wine.bad.color)) #col = c("green", " yellow", "dark red"))

# taste

par.customized <- par(mfrow = c(2, 3))

v.all   = as.vector(all.wine.tastes.percent)
v.red   = as.vector(red.wine.tastes.percent)
v.white = as.vector(white.wine.tastes.percent)

good    = c(v.all[1], v.red[1], v.white[1])
regular = c(v.all[2], v.red[2], v.white[2])
bad     = c(v.all[3], v.red[3], v.white[3])

h = F


bp = barplot( all.wine.tastes.percent,
         main = "[Todos] Qualidade do Vinho",
         col = c("green", " yellow", "dark red"),
         cex.axis = 0.85, cex.lab  = 0.85, cex.main = 1,
         ylim = c(0, 50),
         horiz = h)

text(bp, all.wine.tastes.percent, paste(all.wine.tastes.percent,"%", sep = ""), pos = 3, cex = 1)

bp = barplot( red.wine.tastes.percent,
         main = "Somente vinhos tintos",
         ylab = "Gosto (qualidade)",
         col = c("green", " yellow", "dark red"),
         cex.axis = 0.85, cex.lab  = 0.85, cex.main = 1,
         ylim = c(0, 50),
         horiz = h)

text(bp, red.wine.tastes.percent, paste(red.wine.tastes.percent,"%", sep = ""), pos = 3, cex = 1)

bp = barplot( white.wine.tastes.percent,
         main = "Somente vinhos brancos",
         ylab = "Gosto (qualidade)",
         col = c("green", " yellow", "dark red"),
         cex.axis = 0.85, cex.lab  = 0.85, cex.main = 1,
         ylim = c(0, 50),
         horiz = h)

text(bp, white.wine.tastes.percent, paste(white.wine.tastes.percent,"%", sep = ""), pos = 3, cex = 1)

#barplot( as.matrix(c(good, regular, bad)), beside = TRUE,
bp = barplot( good, 
         main = "Gosto = Bom",
         ylab = "Tipos de vinho",
         names.arg = c("Todos", "Tinto", "Branco"),
         col = c(wine.color.all, wine.color.red, wine.color.white),
         cex.axis = 0.85, cex.lab  = 0.85, cex.main = 1,
         ylim = c(0, 50),
         horiz = h, xlab = "% gosto" )

text(bp, good, paste(good,"%", sep = ""), pos = 3, cex = 1)


#barplot( as.matrix(c(good, regular, bad)), beside = TRUE,
bp = barplot( regular, 
         main = "Gosto = Regular",
         ylab = "Tipos de vinho",
         names.arg = c("Todos", "Tinto", "Branco"),
         col = c(wine.color.all, wine.color.red, wine.color.white),
         cex.axis = 0.85, cex.lab  = 0.85, cex.main = 1,
         ylim = c(0, 50),
         horiz = h, xlab = "% gosto" )

text(bp, regular, paste(regular,"%", sep = ""), pos = 3, cex = 1)

#barplot( as.matrix(c(good, regular, bad)), beside = TRUE,
bp = barplot( bad, 
         main = "Gosto = Ruim",
         ylab = "Tipos de vinho",
         names.arg = c("Todos", "Tinto", "Branco"),
         col = c(wine.color.all, wine.color.red, wine.color.white),
         cex.axis = 0.85, cex.lab  = 0.85, cex.main = 1,
         ylim = c(0, 50),
         horiz = h, xlab = "% gosto" )

text(bp, bad, paste(bad,"%", sep = ""), pos = 3, cex = 1)

# Qualidade / $quality - BoxPlot
par.customized <- par(mfrow=c(3,1))

boxplot(x = all.wine$quality, xlab = "Qualidade", 
        col = wine.color.all,
        main = "[Todos] Qualidade do Vinho",
        cex.axis = 0.75, cex.lab  = 0.75,   cex.main = 1,
        horizontal = T, 
        frame = F,
        ylim = c(3,10))

boxplot(x = red.wine$quality, xlab = "Qualidade", 
        col = wine.color.red,
        main = "[Tinto] Qualidade do Vinho",
        cex.axis = 0.75, cex.lab  = 0.75,   cex.main = 1, 
        horizontal = T, 
        frame = F,
        ylim = c(3,10))

boxplot(x = white.wine$quality, xlab = "Qualidade", 
        col = wine.color.white,
        main = "[Branco] Qualidade do Vinho",
        cex.axis = 0.75, cex.lab  = 0.75,   cex.main = 1, 
        horizontal = T, 
        frame = F,
        ylim = c(3,10))


# Etapa 9: Exemplo: Efeito das colunas (cor do vinho e cor do gosto)
# Exemplo de gráfico com cor para o gosto

library(plyr)

bom <- subset(all.wine, all.wine$taste == "bom")

bom$taste.color 

plot(x = bom$fixed.acidity, y = bom$pH, 
     #col = bom$taste.color,  
     xlab = wine.fields.fixed.acidity, ylab = wine.fields.pH,
     main = "Gosto",
     pch = 20 , frame.plot = F)

par.customized <- par(mfrow=c(1,1))
plot(x = all.wine$fixed.acidity, y = all.wine$pH,
     xlab = wine.fields.fixed.acidity, ylab = wine.fields.pH,
     main = "Pela cor do Vinho",
     col = all.wine$col.color,
     pch = 20, frame.plot = F)
legend(x = "topright", 
       legend = unique(all.wine$color), 
       col    = unique(all.wine$col.color),
       pch = 16 )

# scatter plot com ggplot
library(ggplot2)

ggplot(all.wine, aes(x = fixed.acidity, y = pH)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.pH, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle("Pelo gosto do vinho") + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))


