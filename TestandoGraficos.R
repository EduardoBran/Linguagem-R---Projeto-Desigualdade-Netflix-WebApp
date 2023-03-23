# Tentando Testar Gráficos

# Como o Pib e a Desigualdade Social influenciam no crescimento da NetFlix


# Configurando Diretório de Trabalho
setwd("C:/Users/Julia/Desktop/CienciaDeDados/1.Big-Data-Analytics-com-R-e-Microsoft-Azure-Machine-Learning/4.Linguagem-R-Graficos/BigDataNaPratica_NetFlix")
getwd()

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
dataset1_scat_out <- filter(dataset1, Country != "United States")
View(dataset_scat_out1)

# feito para criar um gráfico de barras apenas com os países que não são a Suíça. (desigualdade social na suiça é muito baixa)
dataset1_bar <- filter(dataset1, Country != "Switzerland")
View(dataset1_bar)

# feito para criar um gráfico de barras apenas com os países que não são a África do Sul. (desigualdade social é muito alta)
dataset1_bar_out <- filter(dataset1, Country != "South Africa")
View(dataset1_bar_out)



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






#Scatter Plot

# - É um gráfico de dispersão (scatter plot) gerado usando a biblioteca Plotly no R. Ele mostra a relação entre o PIB de cada país
#   em 2020 e o número de assinantes estimados para o quarto trimestre de 2021, com cada ponto representando um país. O eixo x representa
#   o PIB em dólares americanos (USD) em bilhões e o eixo y representa o número estimado de assinantes em milhões.

# - Podemos interpretar o gráfico observando que existe uma relação positiva entre o PIB e o número de assinantes, ou seja, quanto maior
#   o PIB, maior o número estimado de assinantes. No entanto, a relação não é linear e existem muitos países com uma quantidade
#   relativamente baixa de assinantes, mesmo com um PIB elevado.

# - O texto que aparece quando o cursor é colocado sobre cada ponto do gráfico (gerado pela opção "text = ~Country") indica o país
#   representado pelo ponto correspondente. Isso pode ser útil para identificar padrões ou anomalias no gráfico, bem como para comparar
#   o desempenho de diferentes países em termos de PIB e assinantes.

grafico_scatter_plot <-  fig <- plot_ly(data = dataset_scat_out, x = ~X2020.GDP..World.Bank., y = ~X..of.Subscribers.Q4.2021..Estimate.,
                   type = "scatter", mode = "markers", text = ~Country) %>%
  layout(xaxis = list(title = "GDP 2020"),
         yaxis = list(title = "Subscribers Q4 2021"))

grafico_scatter_plot



# Bar Plot

# - É um gráfico de barras empilhadas que compara o custo mensal dos planos Basic, Standard e Premium em relação à desigualdade
#   salarial (GINI) em diferentes países. Cada barra representa um valor de GINI para um determinado país e é dividida em três seções,
#   cada uma representando o custo mensal de um tipo de plano (Basic, Standard ou Premium). A altura total de cada barra representa o
#   custo mensal total médio de todos os três tipos de planos.

# - O eixo x representa o valor do índice de GINI, que é uma medida da desigualdade salarial. Quanto maior o valor do GINI, maior é a
#   desigualdade salarial. O eixo y representa o custo mensal médio dos diferentes planos em USD.

# - Através do gráfico, pode-se identificar a relação entre a desigualdade salarial e o custo mensal dos diferentes planos. Além disso,
#   a empilhamento das barras permite visualizar a participação de cada tipo de plano no custo total, para diferentes níveis de
#   desigualdade salarial.

grafico_bar_plot <- plot_ly(data = dataset1_bar_out, x = ~gini_disp, y = ~Cost.Per.Month...Basic....,
                            type = 'bar', name = 'Basic', text = ~Country)

grafico_bar_plot <- grafico_bar_plot %>% add_trace(y = ~basic_standard_diff, name = 'Standard')
grafico_bar_plot <- grafico_bar_plot %>% add_trace(y = ~standard_premium_diff, name = 'Premium')
grafico_bar_plot <- grafico_bar_plot %>% layout(yaxis = list(title = 'Custo Mensal dos Planos Basic, Standard e Premium (USD)', titlefont = list(size=10)),
                      xaxis = list(title = 'Desigualdade Salarial (GINI)'),
                      barmode = 'stack')
grafico_bar_plot


