# Big Data Na Pratica 2 - Analytics Web App Para Grandes Volumes de Dados

# Como o Pib e a Desigualdade Social influenciam no crescimento da NetFlix


# Configurando Diretório de Trabalho
setwd("C:/Users/Julia/Desktop/CienciaDeDados/1.Big-Data-Analytics-com-R-e-Microsoft-Azure-Machine-Learning/4.Linguagem-R-Graficos/BigDataNaPratica_NetFlix")
getwd()



# **************** Dashboard Analítico ****************


# Imports # install.packages("shinythemes")

library(shiny)
library(plotly)
library(shinythemes)


# Carrega o primeiro dataset limpo

dataset1 <- read.csv("datasets_limpos/dataset1.csv")

View(dataset1)


# Ajusta o tipo de dado de algumas colunas

# remove as vírgulas da coluna
dataset1$X..of.Subscribers.Q4.2021..Estimate. <- gsub(",", "", dataset1$X..of.Subscribers.Q4.2021..Estimate.) 
# converte para numérico
dataset1$X..of.Subscribers.Q4.2021..Estimate. <- as.numeric(dataset1$X..of.Subscribers.Q4.2021..Estimate.) 

# fazendo mesma coisa acima em uma unica linha
dataset1$Q4.2021.Revenue....Estimate. <- as.numeric(gsub(",", "", dataset1$Q4.2021.Revenue....Estimate.))

View(dataset1)


# Cria dataframes filtrando os outliers

# feito para criar um gráfico de dispersão (scatter plot) apenas com os países que não são os Estados Unidos.
dataset_scat_out <- filter(dataset1, Country != "United States")
View(dataset_scat_out)

# feito para criar um gráfico de barras apenas com os países que não são a Suíça. (desigualdade social na suiça é muito baixa)
dataset_bar <- filter(dataset1, Country != "Switzerland")
View(dataset_bar)

# feito para criar um gráfico de barras apenas com os países que não são a África do Sul. (desigualdade social é muito alta)
dataset_bar_out <- filter(dataset1, Country != "South Africa")
View(dataset_bar_out)



# Carrega os datasets 2, 3 e 6

genre <- read.csv("datasets_limpos/dataset2.csv")
tree <- read.csv("datasets_limpos/dataset3.csv")
countries <- read.csv("datasets_limpos/dataset6.csv")

View(genre)
View(tree)
View(countries)


# cria um novo df onde estarão apenas as linhas em que o valor da coluna parents é NA.

country_list <- filter(countries, is.na(parents))
View(country_list)


