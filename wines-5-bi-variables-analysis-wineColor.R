#
# INFNET MIT Big Data - Bloco A - Trabalho de R
# 
# Título do Trabalho : Qualidade do vinhos
# Fonte de dados     : UCI - base wines
# Autor              : Fernando A J Peres
# Data               : 2017-06-09
# Arquivo            : Análise descritiva 1: análise bi-variada com gosto / $taste
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
library("gridExtra")
library("cowplot")

# ******************************************************************************
# #### Load prepared data files ####
# ******************************************************************************
load(file="all-wine.Rda") # load(file="red-wine.Rda") # load(file="white-wine.Rda")

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
# #### 01. fixed.acidity = Acidez fixa vs * ####
# ******************************************************************************

# 02. volatile.acidity     = Acidez volátil
title = paste(wine.fields.fixed.acidity, "X", wine.fields.volatile.acidity)
a = ggplot(all.wine, aes(x = fixed.acidity, y = volatile.acidity)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.volatile.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 03. citric.acid          = Ácido cítrico
title = paste(wine.fields.fixed.acidity, "X", wine.fields.citric.acid)
b = ggplot(all.wine, aes(x = fixed.acidity, y = citric.acid)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.citric.acid, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 04. residual.sugar       = Açucar residual
title = paste(wine.fields.fixed.acidity, "X", wine.fields.residual.sugar)
c = ggplot(all.wine, aes(x = fixed.acidity, y = residual.sugar)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.residual.sugar, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 05. chlorides            = Cloretos
title = paste(wine.fields.fixed.acidity, "X", wine.fields.chlorides)
d = ggplot(all.wine, aes(x = fixed.acidity, y = chlorides)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.chlorides, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 06. free.sulfur.dioxide  = Livre de dióxido de enxofre
title = paste(wine.fields.fixed.acidity, "X", wine.fields.free.sulfur.dioxide)
a = ggplot(all.wine, aes(x = fixed.acidity, y = free.sulfur.dioxide)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.free.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))



# 07. total.sulfur.dioxide = Total de dióxido de enxofre
title = paste(wine.fields.fixed.acidity, "X", wine.fields.total.sulfur.dioxide)
b = ggplot(all.wine, aes(x = fixed.acidity, y = total.sulfur.dioxide)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.total.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9),
        plot.background = element_rect(fill = "gray95"))

# 08. density              = Densidade
title = paste(wine.fields.fixed.acidity, "X", wine.fields.density)
c = ggplot(all.wine, aes(x = fixed.acidity, y = density)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.density, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 09. pH                   = pH
title = paste(wine.fields.fixed.acidity, "X", wine.fields.pH)
d = ggplot(all.wine, aes(x = fixed.acidity, y = pH)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.pH, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 10. sulphates            = Sulfatos
title = paste(wine.fields.fixed.acidity, "X", wine.fields.sulphates)
a = ggplot(all.wine, aes(x = fixed.acidity, y = sulphates)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.sulphates, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.fixed.acidity, "X", wine.fields.alcohol)
b = ggplot(all.wine, aes(x = fixed.acidity, y = alcohol)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.alcohol, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, ncol = 2, nrow = 2)
#****************************************


# ******************************************************************************
# #### 02. volatile.acidity = Acidez volátil ####
# ******************************************************************************

# 01. fixed.acidity        = Acidez fixa
title = paste(wine.fields.volatile.acidity, "X", wine.fields.fixed.acidity)
a = ggplot(all.wine, aes(x = volatile.acidity, y = fixed.acidity)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.fixed.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 03. citric.acid          = Ácido cítrico
title = paste(wine.fields.volatile.acidity, "X", wine.fields.citric.acid)
b = ggplot(all.wine, aes(x = volatile.acidity, y = citric.acid)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.citric.acid, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 04. residual.sugar       = Açucar residual
title = paste(wine.fields.volatile.acidity, "X", wine.fields.residual.sugar)
c = ggplot(all.wine, aes(x = volatile.acidity, y = residual.sugar)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.residual.sugar, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 05. chlorides            = Cloretos
title = paste(wine.fields.volatile.acidity, "X", wine.fields.chlorides)
d = ggplot(all.wine, aes(x = volatile.acidity, y = chlorides)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.chlorides, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 06. free.sulfur.dioxide  = Livre de dióxido de enxofre
title = paste(wine.fields.volatile.acidity, "X", wine.fields.free.sulfur.dioxide)
a = ggplot(all.wine, aes(x = volatile.acidity, y = free.sulfur.dioxide)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.free.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))



# 07. total.sulfur.dioxide = Total de dióxido de enxofre
title = paste(wine.fields.volatile.acidity, "X", wine.fields.total.sulfur.dioxide)
b = ggplot(all.wine, aes(x = volatile.acidity, y = total.sulfur.dioxide)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.total.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 08. density              = Densidade
title = paste(wine.fields.volatile.acidity, "X", wine.fields.density)
c = ggplot(all.wine, aes(x = volatile.acidity, y = density)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.density, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 09. pH                   = pH
title = paste(wine.fields.fixed.acidity, "X", wine.fields.pH)
d = ggplot(all.wine, aes(x = volatile.acidity, y = pH)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.pH, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 10. sulphates            = Sulfatos
title = paste(wine.fields.volatile.acidity, "X", wine.fields.sulphates)
a = ggplot(all.wine, aes(x = volatile.acidity, y = sulphates)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.sulphates, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.volatile.acidity, "X", wine.fields.alcohol)
b = ggplot(all.wine, aes(x = volatile.acidity, y = alcohol)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.alcohol, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, ncol = 2, nrow = 2)
#****************************************

# ******************************************************************************
# #### 03. citric.acid = Ácido cítrico ####
# ******************************************************************************
# 02. volatile.acidity     = Acidez volátil
title = paste(wine.fields.citric.acid, "X", wine.fields.fixed.acidity)
a = ggplot(all.wine, aes(x = citric.acid, y = fixed.acidity)) + 
  labs(x = wine.fields.citric.acid, y = wine.fields.fixed.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 03. citric.acid          = Ácido cítrico
title = paste(wine.fields.citric.acid, "X", wine.fields.volatile.acidity)
b = ggplot(all.wine, aes(x = citric.acid, y = volatile.acidity)) + 
  labs(x = wine.fields.citric.acid, y = wine.fields.volatile.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 04. residual.sugar       = Açucar residual
title = paste(wine.fields.citric.acid, "X", wine.fields.residual.sugar)
c = ggplot(all.wine, aes(x = citric.acid, y = residual.sugar)) + 
  labs(x = wine.fields.citric.acid, y = wine.fields.residual.sugar, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 05. chlorides            = Cloretos
title = paste(wine.fields.citric.acid, "X", wine.fields.chlorides)
d = ggplot(all.wine, aes(x = citric.acid, y = chlorides)) + 
  labs(x = wine.fields.citric.acid, y = wine.fields.chlorides, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 06. free.sulfur.dioxide  = Livre de dióxido de enxofre
title = paste(wine.fields.citric.acid, "X", wine.fields.free.sulfur.dioxide)
a = ggplot(all.wine, aes(x = citric.acid, y = free.sulfur.dioxide)) + 
  labs(x = wine.fields.citric.acid, y = wine.fields.free.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))



# 07. total.sulfur.dioxide = Total de dióxido de enxofre
title = paste(wine.fields.citric.acid, "X", wine.fields.total.sulfur.dioxide)
b = ggplot(all.wine, aes(x = citric.acid, y = total.sulfur.dioxide)) + 
  labs(x = wine.fields.citric.acid, y = wine.fields.total.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 08. density              = Densidade
title = paste(wine.fields.citric.acid, "X", wine.fields.density)
c = ggplot(all.wine, aes(x = citric.acid, y = density)) + 
  labs(x = wine.fields.citric.acid, y = wine.fields.density, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 09. pH                   = pH
title = paste(wine.fields.citric.acid, "X", wine.fields.pH)
d = ggplot(all.wine, aes(x = citric.acid, y = pH)) + 
  labs(x = wine.fields.citric.acid, y = wine.fields.pH, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 10. sulphates            = Sulfatos
title = paste(wine.fields.citric.acid, "X", wine.fields.sulphates)
a = ggplot(all.wine, aes(x = citric.acid, y = sulphates)) + 
  labs(x = wine.fields.citric.acid, y = wine.fields.sulphates, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.citric.acid, "X", wine.fields.alcohol)
b = ggplot(all.wine, aes(x = citric.acid, y = alcohol)) + 
  labs(x = wine.fields.citric.acid, y = wine.fields.alcohol, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, ncol = 2, nrow = 2)
#****************************************


# ******************************************************************************
# #### 04. residual.sugar = Açucar residual ####
# ******************************************************************************
# 01. fixed.acidity        = Acidez fixa
title = paste(wine.fields.residual.sugar, "X", wine.fields.fixed.acidity)
a = ggplot(all.wine, aes(x = residual.sugar, y = fixed.acidity)) + 
  labs(x = wine.fields.residual.sugar, y = wine.fields.fixed.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 02. volatile.acidity     = Acidez volátil
title = paste(wine.fields.residual.sugar, "X", wine.fields.volatile.acidity)
b = ggplot(all.wine, aes(x = residual.sugar, y = volatile.acidity)) + 
  labs(x = wine.fields.residual.sugar, y = wine.fields.volatile.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 03. citric.acid          = Ácido cítrico
title = paste(wine.fields.residual.sugar, "X", wine.fields.citric.acid)
c = ggplot(all.wine, aes(x = residual.sugar, y = citric.acid)) + 
  labs(x = wine.fields.residual.sugar, y = wine.fields.citric.acid, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 05. chlorides            = Cloretos
title = paste(wine.fields.residual.sugar, "X", wine.fields.chlorides)
d = ggplot(all.wine, aes(x = residual.sugar, y = chlorides)) + 
  labs(x = wine.fields.residual.sugar, y = wine.fields.chlorides, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 06. free.sulfur.dioxide  = Livre de dióxido de enxofre
title = paste(wine.fields.residual.sugar, "X", wine.fields.free.sulfur.dioxide)
a = ggplot(all.wine, aes(x = residual.sugar, y = free.sulfur.dioxide)) + 
  labs(x = wine.fields.residual.sugar, y = wine.fields.free.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))



# 07. total.sulfur.dioxide = Total de dióxido de enxofre
title = paste(wine.fields.residual.sugar, "X", wine.fields.total.sulfur.dioxide)
b = ggplot(all.wine, aes(x = residual.sugar, y = total.sulfur.dioxide)) + 
  labs(x = wine.fields.residual.sugar, y = wine.fields.total.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 08. density              = Densidade
title = paste(wine.fields.residual.sugar, "X", wine.fields.density)
c = ggplot(all.wine, aes(x = residual.sugar, y = density)) + 
  labs(x = wine.fields.residual.sugar, y = wine.fields.density, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 09. pH                   = pH
title = paste(wine.fields.residual.sugar, "X", wine.fields.pH)
d = ggplot(all.wine, aes(x = residual.sugar, y = pH)) + 
  labs(x = wine.fields.residual.sugar, y = wine.fields.pH, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 10. sulphates            = Sulfatos
title = paste(wine.fields.residual.sugar, "X", wine.fields.sulphates)
a = ggplot(all.wine, aes(x = residual.sugar, y = sulphates)) + 
  labs(x = wine.fields.residual.sugar, y = wine.fields.sulphates, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.residual.sugar, "X", wine.fields.alcohol)
b = ggplot(all.wine, aes(x = residual.sugar, y = alcohol)) + 
  labs(x = wine.fields.residual.sugar, y = wine.fields.alcohol, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, ncol = 2, nrow = 2)
#****************************************


# ******************************************************************************
# #### 05. chlorides = Cloretos ####
# ******************************************************************************
# 01. fixed.acidity        = Acidez fixa
title = paste(wine.fields.chlorides, "X", wine.fields.fixed.acidity)
a = ggplot(all.wine, aes(x = chlorides, y = fixed.acidity)) + 
  labs(x = wine.fields.chlorides, y = wine.fields.fixed.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 02. volatile.acidity     = Acidez volátil
title = paste(wine.fields.chlorides, "X", wine.fields.volatile.acidity)
b = ggplot(all.wine, aes(x = chlorides, y = volatile.acidity)) + 
  labs(x = wine.fields.chlorides, y = wine.fields.volatile.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 03. citric.acid          = Ácido cítrico
title = paste(wine.fields.chlorides, "X", wine.fields.citric.acid)
c = ggplot(all.wine, aes(x = chlorides, y = citric.acid)) + 
  labs(x = wine.fields.chlorides, y = wine.fields.citric.acid, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 04. residual.sugar       = Açucar residual
title = paste(wine.fields.chlorides, "X", wine.fields.residual.sugar)
d = ggplot(all.wine, aes(x = chlorides, y = residual.sugar)) + 
  labs(x = wine.fields.chlorides, y = wine.fields.residual.sugar, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 06. free.sulfur.dioxide  = Livre de dióxido de enxofre
title = paste(wine.fields.chlorides, "X", wine.fields.free.sulfur.dioxide)
a = ggplot(all.wine, aes(x = chlorides, y = free.sulfur.dioxide)) + 
  labs(x = wine.fields.chlorides, y = wine.fields.free.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))



# 07. total.sulfur.dioxide = Total de dióxido de enxofre
title = paste(wine.fields.chlorides, "X", wine.fields.total.sulfur.dioxide)
b = ggplot(all.wine, aes(x = chlorides, y = total.sulfur.dioxide)) + 
  labs(x = wine.fields.chlorides, y = wine.fields.total.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 08. density              = Densidade
title = paste(wine.fields.chlorides, "X", wine.fields.density)
c = ggplot(all.wine, aes(x = chlorides, y = density)) + 
  labs(x = wine.fields.chlorides, y = wine.fields.density, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 09. pH                   = pH
title = paste(wine.fields.chlorides, "X", wine.fields.pH)
d = ggplot(all.wine, aes(x = chlorides, y = pH)) + 
  labs(x = wine.fields.chlorides, y = wine.fields.pH, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 10. sulphates            = Sulfatos
title = paste(wine.fields.chlorides, "X", wine.fields.sulphates)
a = ggplot(all.wine, aes(x = chlorides, y = sulphates)) + 
  labs(x = wine.fields.chlorides, y = wine.fields.sulphates, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.chlorides, "X", wine.fields.alcohol)
b = ggplot(all.wine, aes(x = chlorides, y = alcohol)) + 
  labs(x = wine.fields.chlorides, y = wine.fields.alcohol, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, ncol = 2, nrow = 2)
#****************************************

# ******************************************************************************
# #### 06. free.sulfur.dioxide  = Livre de dióxido de enxofre ####
# ******************************************************************************
# 01. fixed.acidity        = Acidez fixa
title = paste(wine.fields.free.sulfur.dioxide, "X", wine.fields.fixed.acidity)
a = ggplot(all.wine, aes(x = free.sulfur.dioxide, y = fixed.acidity)) + 
  labs(x = wine.fields.free.sulfur.dioxide, y = wine.fields.fixed.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 02. volatile.acidity     = Acidez volátil
title = paste(wine.fields.free.sulfur.dioxide, "X", wine.fields.volatile.acidity)
b = ggplot(all.wine, aes(x = free.sulfur.dioxide, y = volatile.acidity)) + 
  labs(x = wine.fields.free.sulfur.dioxide, y = wine.fields.volatile.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 03. citric.acid          = Ácido cítrico
title = paste(wine.fields.free.sulfur.dioxide, "X", wine.fields.citric.acid)
c = ggplot(all.wine, aes(x = free.sulfur.dioxide, y = citric.acid)) + 
  labs(x = wine.fields.free.sulfur.dioxide, y = wine.fields.citric.acid, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 04. residual.sugar       = Açucar residual
title = paste(wine.fields.free.sulfur.dioxide, "X", wine.fields.residual.sugar)
d = ggplot(all.wine, aes(x = free.sulfur.dioxide, y = residual.sugar)) + 
  labs(x = wine.fields.free.sulfur.dioxide, y = wine.fields.residual.sugar, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 05. chlorides            = Cloretos
title = paste(wine.fields.free.sulfur.dioxide, "X", wine.fields.chlorides)
a = ggplot(all.wine, aes(x = free.sulfur.dioxide, y = chlorides)) + 
  labs(x = wine.fields.free.sulfur.dioxide, y = wine.fields.chlorides, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))



# 07. total.sulfur.dioxide = Total de dióxido de enxofre
title = paste(wine.fields.free.sulfur.dioxide, "X", wine.fields.total.sulfur.dioxide)
b = ggplot(all.wine, aes(x = free.sulfur.dioxide, y = total.sulfur.dioxide)) + 
  labs(x = wine.fields.free.sulfur.dioxide, y = wine.fields.total.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 08. density              = Densidade
title = paste(wine.fields.free.sulfur.dioxide, "X", wine.fields.density)
c = ggplot(all.wine, aes(x = free.sulfur.dioxide, y = density)) + 
  labs(x = wine.fields.free.sulfur.dioxide, y = wine.fields.density, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 09. pH                   = pH
title = paste(wine.fields.free.sulfur.dioxide, "X", wine.fields.pH)
d = ggplot(all.wine, aes(x = free.sulfur.dioxide, y = pH)) + 
  labs(x = wine.fields.free.sulfur.dioxide, y = wine.fields.pH, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 10. sulphates            = Sulfatos
title = paste(wine.fields.free.sulfur.dioxide, "X", wine.fields.sulphates)
a = ggplot(all.wine, aes(x = free.sulfur.dioxide, y = sulphates)) + 
  labs(x = wine.fields.free.sulfur.dioxide, y = wine.fields.sulphates, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.free.sulfur.dioxide, "X", wine.fields.alcohol)
b = ggplot(all.wine, aes(x = free.sulfur.dioxide, y = alcohol)) + 
  labs(x = wine.fields.free.sulfur.dioxide, y = wine.fields.alcohol, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, ncol = 2, nrow = 2)
#****************************************


# ******************************************************************************
# #### 07. total.sulfur.dioxide = Total de dióxido de enxofre ####
# ******************************************************************************
# 01. fixed.acidity        = Acidez fixa
title = paste(wine.fields.total.sulfur.dioxide, "X", wine.fields.fixed.acidity)
a = ggplot(all.wine, aes(x = total.sulfur.dioxide, y = fixed.acidity)) + 
  labs(x = wine.fields.total.sulfur.dioxide, y = wine.fields.fixed.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 02. volatile.acidity     = Acidez volátil
title = paste(wine.fields.total.sulfur.dioxide, "X", wine.fields.volatile.acidity)
b = ggplot(all.wine, aes(x = total.sulfur.dioxide, y = volatile.acidity)) + 
  labs(x = wine.fields.total.sulfur.dioxide, y = wine.fields.volatile.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 03. citric.acid          = Ácido cítrico
title = paste(wine.fields.total.sulfur.dioxide, "X", wine.fields.citric.acid)
c = ggplot(all.wine, aes(x = total.sulfur.dioxide, y = citric.acid)) + 
  labs(x = wine.fields.total.sulfur.dioxide, y = wine.fields.citric.acid, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 04. residual.sugar       = Açucar residual
title = paste(wine.fields.total.sulfur.dioxide, "X", wine.fields.residual.sugar)
d = ggplot(all.wine, aes(x = total.sulfur.dioxide, y = residual.sugar)) + 
  labs(x = wine.fields.total.sulfur.dioxide, y = wine.fields.residual.sugar, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 05. chlorides            = Cloretos
title = paste(wine.fields.total.sulfur.dioxide, "X", wine.fields.chlorides)
a = ggplot(all.wine, aes(x = total.sulfur.dioxide, y = chlorides)) + 
  labs(x = wine.fields.total.sulfur.dioxide, y = wine.fields.chlorides, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))



# 06. free.sulfur.dioxide  = Livre de dióxido de enxofre
title = paste(wine.fields.total.sulfur.dioxide, "X", wine.fields.free.sulfur.dioxide)
b = ggplot(all.wine, aes(x = total.sulfur.dioxide, y = free.sulfur.dioxide)) + 
  labs(x = wine.fields.total.sulfur.dioxide, y = wine.fields.free.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 08. density              = Densidade
title = paste(wine.fields.total.sulfur.dioxide, "X", wine.fields.density)
c = ggplot(all.wine, aes(x = total.sulfur.dioxide, y = density)) + 
  labs(x = wine.fields.total.sulfur.dioxide, y = wine.fields.density, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 09. pH                   = pH
title = paste(wine.fields.total.sulfur.dioxide, "X", wine.fields.pH)
d = ggplot(all.wine, aes(x = total.sulfur.dioxide, y = pH)) + 
  labs(x = wine.fields.total.sulfur.dioxide, y = wine.fields.pH, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 10. sulphates            = Sulfatos
title = paste(wine.fields.total.sulfur.dioxide, "X", wine.fields.sulphates)
a = ggplot(all.wine, aes(x = total.sulfur.dioxide, y = sulphates)) + 
  labs(x = wine.fields.total.sulfur.dioxide, y = wine.fields.sulphates, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.total.sulfur.dioxide, "X", wine.fields.alcohol)
b = ggplot(all.wine, aes(x = total.sulfur.dioxide, y = alcohol)) + 
  labs(x = wine.fields.total.sulfur.dioxide, y = wine.fields.alcohol, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, ncol = 2, nrow = 2)
#****************************************



# ******************************************************************************
# #### 08. density = Densidade ####
# ******************************************************************************
# 01. fixed.acidity        = Acidez fixa
title = paste(wine.fields.density, "X", wine.fields.fixed.acidity)
a = ggplot(all.wine, aes(x = density, y = fixed.acidity)) + 
  labs(x = wine.fields.density, y = wine.fields.fixed.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 02. volatile.acidity     = Acidez volátil
title = paste(wine.fields.density, "X", wine.fields.volatile.acidity)
b = ggplot(all.wine, aes(x = density, y = volatile.acidity)) + 
  labs(x = wine.fields.density, y = wine.fields.volatile.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 03. citric.acid          = Ácido cítrico
title = paste(wine.fields.density, "X", wine.fields.citric.acid)
c = ggplot(all.wine, aes(x = density, y = citric.acid)) + 
  labs(x = wine.fields.density, y = wine.fields.citric.acid, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 04. residual.sugar       = Açucar residual
title = paste(wine.fields.density, "X", wine.fields.residual.sugar)
d = ggplot(all.wine, aes(x = density, y = residual.sugar)) + 
  labs(x = wine.fields.density, y = wine.fields.residual.sugar, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 05. chlorides            = Cloretos
title = paste(wine.fields.density, "X", wine.fields.chlorides)
a = ggplot(all.wine, aes(x = density, y = chlorides)) + 
  labs(x = wine.fields.density, y = wine.fields.chlorides, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))



# 06. free.sulfur.dioxide  = Livre de dióxido de enxofre
title = paste(wine.fields.density, "X", wine.fields.free.sulfur.dioxide)
b = ggplot(all.wine, aes(x = density, y = free.sulfur.dioxide)) + 
  labs(x = wine.fields.density, y = wine.fields.free.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 07. total.sulfur.dioxide = Total de dióxido de enxofre
title = paste(wine.fields.density, "X", wine.fields.total.sulfur.dioxide)
c = ggplot(all.wine, aes(x = density, y = total.sulfur.dioxide)) + 
  labs(x = wine.fields.density, y = wine.fields.total.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 09. pH                   = pH
title = paste(wine.fields.density, "X", wine.fields.pH)
d = ggplot(all.wine, aes(x = density, y = pH)) + 
  labs(x = wine.fields.density, y = wine.fields.pH, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 10. sulphates            = Sulfatos
title = paste(wine.fields.density, "X", wine.fields.sulphates)
a = ggplot(all.wine, aes(x = density, y = sulphates)) + 
  labs(x = wine.fields.density, y = wine.fields.sulphates, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.density, "X", wine.fields.alcohol)
b = ggplot(all.wine, aes(x = density, y = alcohol)) + 
  labs(x = wine.fields.density, y = wine.fields.alcohol, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, ncol = 2, nrow = 2)
#****************************************



# ******************************************************************************
# #### 09. pH = pH ####
# ******************************************************************************
# 01. fixed.acidity        = Acidez fixa
title = paste(wine.fields.pH, "X", wine.fields.fixed.acidity)
a = ggplot(all.wine, aes(x = pH, y = fixed.acidity)) + 
  labs(x = wine.fields.pH, y = wine.fields.fixed.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 02. volatile.acidity     = Acidez volátil
title = paste(wine.fields.pH, "X", wine.fields.volatile.acidity)
b = ggplot(all.wine, aes(x = pH, y = volatile.acidity)) + 
  labs(x = wine.fields.pH, y = wine.fields.volatile.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 03. citric.acid          = Ácido cítrico
title = paste(wine.fields.pH, "X", wine.fields.citric.acid)
c = ggplot(all.wine, aes(x = pH, y = citric.acid)) + 
  labs(x = wine.fields.pH, y = wine.fields.citric.acid, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 04. residual.sugar       = Açucar residual
title = paste(wine.fields.pH, "X", wine.fields.residual.sugar)
d = ggplot(all.wine, aes(x = pH, y = residual.sugar)) + 
  labs(x = wine.fields.pH, y = wine.fields.residual.sugar, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 05. chlorides            = Cloretos
title = paste(wine.fields.pH, "X", wine.fields.chlorides)
a = ggplot(all.wine, aes(x = pH, y = chlorides)) + 
  labs(x = wine.fields.pH, y = wine.fields.chlorides, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 06. free.sulfur.dioxide  = Livre de dióxido de enxofre
title = paste(wine.fields.pH, "X", wine.fields.free.sulfur.dioxide)
b = ggplot(all.wine, aes(x = pH, y = free.sulfur.dioxide)) + 
  labs(x = wine.fields.pH, y = wine.fields.free.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 07. total.sulfur.dioxide = Total de dióxido de enxofre
title = paste(wine.fields.pH, "X", wine.fields.total.sulfur.dioxide)
c = ggplot(all.wine, aes(x = pH, y = total.sulfur.dioxide)) + 
  labs(x = wine.fields.pH, y = wine.fields.total.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 08. density = Densidade
title = paste(wine.fields.pH, "X", wine.fields.density)
d = ggplot(all.wine, aes(x = pH, y = density)) + 
  labs(x = wine.fields.pH, y = wine.fields.density, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 10. sulphates            = Sulfatos
title = paste(wine.fields.pH, "X", wine.fields.sulphates)
a = ggplot(all.wine, aes(x = pH, y = sulphates)) + 
  labs(x = wine.fields.pH, y = wine.fields.sulphates, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.pH, "X", wine.fields.alcohol)
b = ggplot(all.wine, aes(x = pH, y = alcohol)) + 
  labs(x = wine.fields.pH, y = wine.fields.alcohol, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, ncol = 2, nrow = 2)
#****************************************

# ******************************************************************************
# #### 10. sulphates = Sulfatos ####
# ******************************************************************************
# 01. fixed.acidity        = Acidez fixa
title = paste(wine.fields.sulphates, "X", wine.fields.fixed.acidity)
a = ggplot(all.wine, aes(x = sulphates, y = fixed.acidity)) + 
  labs(x = wine.fields.sulphates, y = wine.fields.fixed.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 02. volatile.acidity     = Acidez volátil
title = paste(wine.fields.sulphates, "X", wine.fields.volatile.acidity)
b = ggplot(all.wine, aes(x = sulphates, y = volatile.acidity)) + 
  labs(x = wine.fields.sulphates, y = wine.fields.volatile.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 03. citric.acid          = Ácido cítrico
title = paste(wine.fields.sulphates, "X", wine.fields.citric.acid)
c = ggplot(all.wine, aes(x = sulphates, y = citric.acid)) + 
  labs(x = wine.fields.sulphates, y = wine.fields.citric.acid, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 04. residual.sugar       = Açucar residual
title = paste(wine.fields.sulphates, "X", wine.fields.residual.sugar)
d = ggplot(all.wine, aes(x = sulphates, y = residual.sugar)) + 
  labs(x = wine.fields.sulphates, y = wine.fields.residual.sugar, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 05. chlorides            = Cloretos
title = paste(wine.fields.sulphates, "X", wine.fields.chlorides)
a = ggplot(all.wine, aes(x = sulphates, y = chlorides)) + 
  labs(x = wine.fields.sulphates, y = wine.fields.chlorides, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 06. free.sulfur.dioxide  = Livre de dióxido de enxofre
title = paste(wine.fields.sulphates, "X", wine.fields.free.sulfur.dioxide)
b = ggplot(all.wine, aes(x = sulphates, y = free.sulfur.dioxide)) + 
  labs(x = wine.fields.sulphates, y = wine.fields.free.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 07. total.sulfur.dioxide = Total de dióxido de enxofre
title = paste(wine.fields.sulphates, "X", wine.fields.total.sulfur.dioxide)
c = ggplot(all.wine, aes(x = sulphates, y = total.sulfur.dioxide)) + 
  labs(x = wine.fields.sulphates, y = wine.fields.total.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 08. density = Densidade
title = paste(wine.fields.sulphates, "X", wine.fields.density)
d = ggplot(all.wine, aes(x = sulphates, y = density)) + 
  labs(x = wine.fields.sulphates, y = wine.fields.density, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 09. pH = pH    
title = paste(wine.fields.sulphates, "X", wine.fields.pH)
a = ggplot(all.wine, aes(x = sulphates, y = pH)) + 
  labs(x = wine.fields.sulphates, y = wine.fields.pH, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.sulphates, "X", wine.fields.alcohol)
b = ggplot(all.wine, aes(x = sulphates, y = alcohol)) + 
  labs(x = wine.fields.sulphates, y = wine.fields.alcohol, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, ncol = 2, nrow = 2)
#****************************************

# ******************************************************************************
# #### 11. alcohol = Teor alcólico ####
# ******************************************************************************
# 01. fixed.acidity        = Acidez fixa
title = paste(wine.fields.alcohol, "X", wine.fields.fixed.acidity)
a = ggplot(all.wine, aes(x = alcohol, y = fixed.acidity)) + 
  labs(x = wine.fields.alcohol, y = wine.fields.fixed.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 02. volatile.acidity     = Acidez volátil
title = paste(wine.fields.alcohol, "X", wine.fields.volatile.acidity)
b = ggplot(all.wine, aes(x = alcohol, y = volatile.acidity)) + 
  labs(x = wine.fields.alcohol, y = wine.fields.volatile.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 03. citric.acid          = Ácido cítrico
title = paste(wine.fields.alcohol, "X", wine.fields.citric.acid)
c = ggplot(all.wine, aes(x = alcohol, y = citric.acid)) + 
  labs(x = wine.fields.alcohol, y = wine.fields.citric.acid, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 04. residual.sugar       = Açucar residual
title = paste(wine.fields.alcohol, "X", wine.fields.residual.sugar)
d = ggplot(all.wine, aes(x = alcohol, y = residual.sugar)) + 
  labs(x = wine.fields.alcohol, y = wine.fields.residual.sugar, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 05. chlorides            = Cloretos
title = paste(wine.fields.alcohol, "X", wine.fields.chlorides)
a = ggplot(all.wine, aes(x = alcohol, y = chlorides)) + 
  labs(x = wine.fields.alcohol, y = wine.fields.chlorides, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 06. free.sulfur.dioxide  = Livre de dióxido de enxofre
title = paste(wine.fields.alcohol, "X", wine.fields.free.sulfur.dioxide)
b = ggplot(all.wine, aes(x = alcohol, y = free.sulfur.dioxide)) + 
  labs(x = wine.fields.alcohol, y = wine.fields.free.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 07. total.sulfur.dioxide = Total de dióxido de enxofre
title = paste(wine.fields.alcohol, "X", wine.fields.total.sulfur.dioxide)
c = ggplot(all.wine, aes(x = alcohol, y = total.sulfur.dioxide)) + 
  labs(x = wine.fields.alcohol, y = wine.fields.total.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 08. density = Densidade
title = paste(wine.fields.alcohol, "X", wine.fields.density)
d = ggplot(all.wine, aes(x = alcohol, y = density)) + 
  labs(x = wine.fields.alcohol, y = wine.fields.density, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

#****************************************
plot_grid(a, b, c, d, ncol = 2, nrow = 2)
#****************************************

# 09. pH = pH    
title = paste(wine.fields.alcohol, "X", wine.fields.pH)
a = ggplot(all.wine, aes(x = alcohol, y = pH)) + 
  labs(x = wine.fields.alcohol, y = wine.fields.pH, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))

# 10. sulphates = Sulfatos
title = paste(wine.fields.alcohol, "X", wine.fields.sulphates)
b = ggplot(all.wine, aes(x = alcohol, y = sulphates)) + 
  labs(x = wine.fields.alcohol, y = wine.fields.sulphates, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("gold", "red"),
                     breaks = c("branco", "tinto"))+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title   = element_text(size = 9),
        legend.text  = element_text(size = 9))


#****************************************
plot_grid(a, b, ncol = 2, nrow = 2)
#****************************************










