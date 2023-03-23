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
dataset1_scat_out <- filter(dataset1, Country != "United States")
View(dataset_scat_out1)

# feito para criar um gráfico de barras apenas com os países que não são a Suíça. (desigualdade social na suiça é muito baixa)
dataset1_bar <- filter(dataset1, Country != "Switzerland")
View(dataset_bar1)

# feito para criar um gráfico de barras apenas com os países que não são a África do Sul. (desigualdade social é muito alta)
dataset1_bar_out <- filter(dataset1, Country != "South Africa")
View(dataset_bar_out1)



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



# https://rstudio.github.io/shinythemes - outros temas



############################# UI - User Interface #############################

ui <- 
  navbarPage(
    theme = shinytheme("cerulean"),
    "Big Data na Prática2",
    
    tabPanel(
      "Visão Geral",
      
      sidebarLayout(
        
        sidebarPanel(
          
          selectInput(
            "select",
            label = h4("Selecione a Variável do Eixo Y:"),
            choices = list("Faturamento da Netflix Q4-2021" = "Q4.2021.Revenue....Estimate.", 
                           "Assinaturas da Netflix Q4-2021" = "X..of.Subscribers.Q4.2021..Estimate.",
                           "Tamanho Total do Catálogo" = "Total.Library.Size", 
                           "Preço da Assinatura Basic" = "Cost.Per.Month...Basic....", 
                           "Preço da Assinatura Standard"= "Cost.Per.Month...Standard....",
                           "Preço da Assinatura Premium" = "Cost.Per.Month...Premium...."),
            selected = 1
            
          ), # end selectInput
          
          checkboxInput("outlierscatter", "Mostrar Outlier", FALSE)
          
        ), # end sidebarPanel
        
        mainPanel(plotlyOutput("scatPlot"))
        
      ) # end sidebarLayout
      
    ), # end tabPanel
    
    tabPanel(
      "Desigualdade Salarial",
      h4("Disparidade de Renda e Diferenças nos Preços da Assinatura Basic, Standard e Premium da Netflix (Mensal)"),
      
      sidebarPanel(
        checkboxInput("outlierbar", "Mostrar Outlier", FALSE)
      
      ),# end sidebarPanel
      
      mainPanel(
        plotlyOutput("barPlot")
      
      ) # end mainPanel
      
    ), # end tabPanel
    
    tabPanel(
      "Gêneros Populares",
      
      tabPanel(
        "Country",
        
        sidebarLayout(
          
          sidebarPanel(
            
            selectInput(
              "Country",
              label = h3("Selecione o País:"),
              choices = country_list$labels,
              selected = 1
              
            ) # end selecInput
            
          ), # end sidebarPanel
          
          mainPanel(
            h3("Popularidade de Gênero dos Filmes Por País"),
            h5("Com base no número de vezes que um filme/programa de TV de um determinado gênero esteve no Top 10 semanal da Netflix em um país (Dados de Junho 2021 - Março 2022)."),
            plotlyOutput("countryPlot")
            
          ) # end mainPanel
          
        ) # end sidebarLayout
        
      ) # end tabPanel
      
    ), # end tabPanel
    
    tabPanel(
      "Assinantes Netflix",
      
      sidebarLayout(
        
        sidebarPanel(
          
          selectInput(
            "select3",
            label = h3("Selecione a Escala:"),
            choices = list("Faturamento Netflix Q4-2021" = "Q4.2021.Revenue....Estimate.",
                           "Assinaturas Netflix Q4-2021" = "X..of.Subscribers.Q4.2021..Estimate."),
            selected = 1
            
          ), # end selectInput
          
          checkboxInput("outliermap", "Mostrar Outlier", FALSE)
          
        ), # end sidebarPanel
        
        mainPanel(
          plotlyOutput("mapPlot"),
          h3("Faturamento x Assinaturas"),
          plotlyOutput("mapscatPlot")
          
        ) # end mainPanel
        
      ) # end sidebarLayoyt
      
    ) # end tabPanel
    
  ) # end navbarPage




############################# Server - Lógica do Servidor #############################


server <- 
  
  function(input, output) {
    
    #Scatter Plot
    output$scatPlot <- 
      
      renderPlotly({
        
        if(input$outlierscatter){
          dfs <- dataset1
        }
        else{
          dfs <- dataset1_scat_out
        }
        
        fig <- plot_ly(data = dfs, x = ~X2020.GDP..World.Bank., y = ~get(input$select),
                       type = "scatter", mode = "markers", text = ~Country)
        
        fig <- fig %>% layout(yaxis = list(title = 'Variável Selecionada'), xaxis = list(title = 'PIB (USD)'))
        
        fig
        
      }) # end renderPlotly
    
    
    #Bar Plot
    output$barPlot <- 
      
      renderPlotly({
        
        if(input$outlierbar) {
          dfb <- dataset1_bar
        }
        else {
          dfb <- dataset1_bar_out
        }
        
        fig <- plot_ly(data = dfb, x = ~gini_disp, y = ~Cost.Per.Month...Basic....,
                       type = 'bar', name = 'Basic', text = ~Country)
        
        fig <- fig %>% add_trace(y = ~basic_standard_diff, name = 'Standard')
        fig <- fig %>% add_trace(y = ~standard_premium_diff, name = 'Premium')
        fig <- fig %>% layout(yaxis = list(title = 'Custo Mensal dos Planos Basic, Standard e Premium (USD)', titlefont = list(size=10)),
                              xaxis = list(title = 'Desigualdade Salarial (GINI)'),
                              barmode = 'stack')
        fig
        
      }) # end renderPlotly
    
    
    # Country Plot
    output$countryPlot <- 
      
      renderPlotly({
        
        country <- filter(countries, parents == input$Country)
        country <- rbind(filter(countries, labels == input$Country), country)
        
        fig <- plot_ly(data = country, ids = ~id, labels = ~labels,
                       parents = ~parents, values = ~n, type = 'treemap',
                       branchvalues = 'total', pathbar = list(visible = TRUE))
        fig
      
      }) # end renderPlotly
    
    
    # Treemap Plot (não é chamado no grafico interativo)
    output$treePlot <-
      
      renderPlotly({
        
        fig <- plot_ly(data = tree, ids = ~id, labels = ~labels,
                       parents = ~parent, values = ~n, type = 'treemap',
                       branchvalues = 'total', pathbar = list(visible = TRUE))
        
        fig
        
      }) # end renderPlotly
    
    
    # Mapa
    output$mapPlot <- 
      
      renderPlotly({
        
        if(input$outliermap) {
          dfm <- dataset1
        }
        else {
          dfm <- dataset1_scat_out
        }
        
        l <- list(color = toRGB("grey"), width = 0.5)
        
        # https://en.wikipedia.org/wiki/List_of_map_projections
        
        g <- list(
          showframe = FALSE,
          showcoastlines = FALSE,
          projection = list(type = 'Miller')
        )
        
        fig <- plot_geo(dfm)
        
        fig <- fig %>% add_trace(z = ~get(input$select3),
                                 color = ~get(input$select3),
                                 colorscale = 'Purples',
                                 text = ~Country,
                                 locations = ~Alpha.3.code,
                                 marker = list(line = l))
        
        fig <- fig %>% colorbar(title = 'Escala')
        fig <- fig %>% layout(title = 'Mapa Global da Netflix em Q4-2021')
        
        fig
        
      }) # end renderPlotly
    
    
    # Scatter Plot
    
    output$mapscatPlot <- 
      
      renderPlotly({
        
        if(input$outliermap){
          dfms <- dataset1
        }
        else {
          dfms <- dataset1_scat_out
        }
        
        fig <- plot_ly(data = dfms, x = ~X..of.Subscribers.Q4.2021..Estimate., y = ~Q4.2021.Revenue....Estimate.,
                       type = "scatter", mode = "markers", text = ~Country)
        
        fig <- fig %>% layout(yaxis = list(title = 'Faturamento da Netflix em Q4-2021'),
                              xaxis = list(title = 'Assinantes da Netflix em Q4-2021'))
        
        fig
        
      })# end renderPlotly

  } # end function


# Executa o server

shinyApp(ui, server)








# Explicando o Country Plot

# - É um treemap plot em que a área dos retângulos é proporcional à quantidade de observações em cada categoria. A função renderPlotly()
#   do shiny é utilizada para gerar o gráfico dinamicamente com base nos inputs do usuário.

# - A primeira parte do código filtra a tabela countries para obter somente as observações do país selecionado no input Country.
#   A segunda parte cria o treemap com a função plot_ly(). Os argumentos data, ids, labels, parents e values são utilizados para
#   especificar os dados e a hierarquia do treemap.

# - O argumento type especifica que o gráfico deve ser do tipo treemap, e branchvalues especifica que os valores exibidos devem ser o
#   valor total da categoria e não o valor absoluto de cada observação. O argumento pathbar é utilizado para adicionar barras de
#   navegação para o usuário se deslocar na hierarquia do treemap.
                 
