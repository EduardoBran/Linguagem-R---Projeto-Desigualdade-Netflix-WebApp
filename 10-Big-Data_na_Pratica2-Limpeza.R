# Big Data Na Pratica 2 - Limpeza

# Como o Pib e a Desigualdade Social influenciam no crescimento da NetFlix



# Configurando Diretório de Trabalho
setwd("C:/Users/Julia/Desktop/CienciaDeDados/1.Big-Data-Analytics-com-R-e-Microsoft-Azure-Machine-Learning/4.Linguagem-R-Graficos/BigDataNaPratica_NetFlix")
getwd()



# **************** Script de Carga e Limpeza dos Dados ****************


# Carregando os pacotes

library(dplyr)
library(tidyr)
library(readxl)
library(readr)


# *** Carga dos Dados ***

# Carregando os dados da Netflix
dados_netflix <- read.csv("datasets_originais/dados_netflix_Dec_2021.csv")
View(dados_netflix)

# Carregando os dados do World Bank
dados_pib <- read.csv("datasets_originais/dados_world_bank.csv", header = FALSE) # com header =  false, ele nao transforma em nome de coluna a primeira linha e cria nomeia automaticamente o nome das colunas com V1, V2, V3...
View(dados_pib)

# Carregando os dados de desigualdade salarial
dados_salario <- read.csv("datasets_originais/dados_desigualdade_salarial_harvard.csv")
View(dados_salario)

# Carregando os dados do IMDB
dados_IMDB <- read_tsv("datasets_originais/dados_IMDB.tsv")
View(dados_IMDB)

# Carregando os dados dos top 10 shows da NetFlix por país
dados_top10 <- read_excel("datasets_originais/top_10_shows_netflix.xlsx")
View(dados_top10)

# Carregando dados de assinantes da Netflix em Julho/2021
dados_sub <- read.csv("datasets_originais/assinantes_netflix_jul_2021.csv")
View(dados_sub)

# Carregando os dados de códigos ISO dos países
countrycode <- read.csv("datasets_originais/wikipedia-iso-country-codes.csv")
View(countrycode)



# *** Limpeza e Preparação do Primeiro Dataset Combinado ***

# Cria uma coluna com a diferença de dados para o gráfico de barras (plano standard - plano basico)
dados_netflix$basic_standard_diff = (dados_netflix$Cost.Per.Month...Standard.... - dados_netflix$Cost.Per.Month...Basic....)

# Cria uma coluna com a diferença de dados para o gráfico de barras (plano premium - plano standard)
dados_netflix$standard_premium_diff = (dados_netflix$Cost.Per.Month...Premium.... - dados_netflix$Cost.Per.Month...Standard....)


# Combina os dados anteriores com dados do PIB

# renomeando para 'Country' a primeira coluna
names(dados_pib)[names(dados_pib) == 'V1'] <- 'Country' 

# a função merge() é utilizada para combinar os conjuntos de dados dados_netflix e dados_pib com base na coluna Country. Isso
# cria um novo conjunto de dados chamado dados_netflix_pib, que contém todas as informações dos dois conjuntos de dados
# originais, combinadas em uma única tabela. A combinação é feita de tal forma que apenas as linhas que possuem o mesmo valor
# na coluna Country em ambos os conjuntos de dados são mantidas.
dados_netflix_pib <- merge(dados_netflix, dados_pib, by = 'Country')
View(dados_netflix_pib)

View(dados_netflix)

# Extrai o PIB de 2020

# exclui todas as colunas de 11 a 72 e 74 e 75 e cria um novo df. Essas colunas correspondem a dados dos anos anteriores a 2020
dados_netflix_pib2020 <- dados_netflix_pib[-c(11:72, 74, 75)]

# renomeando o nome da coluna
names(dados_netflix_pib2020)[names(dados_netflix_pib2020) == 'V65'] <- '2020 GDP (World Bank)'
View(dados_netflix_pib2020)


# Limpeza do dataframe de desigualdade salarial ("%>%" simbolo para concatenar comandos)

# remove todas as colunas a partir da quarta coluna em diante e mantém apenas as três primeiras colunas do conjunto de dados.
dados_salario <- dados_salario[, c(1:3)]

# Cria um novo df onde ele utiliza o operador %>% (pipe) para direcionar a saída do objeto dados_salario para a próxima função.
# Em seguida, o group_by(country) agrupa os dados de salário pelo nome do país.
# O summarise(max = max(year, na.rm = TRUE)) resume os dados agrupados, calculando o máximo valor da variável year para cada
# país e atribuindo o resultado a uma nova coluna chamada max. O argumento na.rm = TRUE é utilizado para remover valores
# faltantes (NA) na coluna year, caso existam.
dados_salario_ano <- dados_salario %>% group_by(country) %>% summarise(max = max(year, na.rm = TRUE))

View(dados_salario_ano)
View(dados_salario)
