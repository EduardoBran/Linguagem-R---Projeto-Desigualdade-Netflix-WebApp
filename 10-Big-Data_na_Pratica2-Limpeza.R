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

# Carregando dados de assinantes da Netflix em Julho/2021
dados_sub <- read.csv("datasets_originais/assinantes_netflix_jul_2021.csv")
View(dados_sub)

# Carregando os dados de códigos ISO dos países
countrycode <- read.csv("datasets_originais/wikipedia-iso-country-codes.csv")
View(countrycode)


# Carregando os dados dos top 10 shows da NetFlix por país
dados_top10 <- read_excel("datasets_originais/top_10_shows_netflix.xlsx")
View(dados_top10)


# *** Análise exploratória (não foi feita para esse exemplo) ***





# *************** Limpeza e Preparação do Primeiro Dataset Combinado ***************


# Cria uma coluna com a diferença de dados do para o gráfico de barras (plano standard - plano basico)
dados_netflix$basic_standard_diff = (dados_netflix$Cost.Per.Month...Standard.... - dados_netflix$Cost.Per.Month...Basic....)

# Cria uma coluna com a diferença de dados para o gráfico de barras (plano premium - plano standard)
dados_netflix$standard_premium_diff = (dados_netflix$Cost.Per.Month...Premium.... - dados_netflix$Cost.Per.Month...Standard....)

View(dados_netflix)


# Combina os dados anteriores com dados do PIB

# renomeando para 'Country' a primeira coluna de 'dados_pib'
names(dados_pib)[names(dados_pib) == 'V1'] <- 'Country' 

# A função merge() é utilizada para combinar os conjuntos de dados 'dados_netflix' e 'dados_pib' com base na coluna Country.
# Isso cria um novo conjunto de dados chamado dados_netflix_pib, que contém todas as informações dos dois conjuntos de dados
# originais, combinadas em uma única tabela. A combinação é feita de tal forma que apenas as linhas que possuem o mesmo valor
# na coluna Country em ambos os conjuntos de dados são mantidas.
dados_netflix_pib <- merge(dados_netflix, dados_pib, by = 'Country')

View(dados_netflix_pib)


# Extrai o PIB de 2020

# exclui todas as colunas de 11 a 72 e 74 e 75 de 'dados_netflix_pib' e cria um novo df. Essas colunas
# correspondiam aos dados dos anos anteriores a 2020
dados_netflix_pib2020 <- dados_netflix_pib[-c(11:72, 74, 75)]

# renomeando o nome da coluna
names(dados_netflix_pib2020)[names(dados_netflix_pib2020) == 'V64'] <- '2020 GDP (World Bank)'
View(dados_netflix_pib2020)


# Limpeza do dataframe de desigualdade salarial ( "%>%" simbolo para concatenar comandos )

# remove todas as colunas a partir da quarta coluna em diante e mantém apenas as três primeiras colunas do conjunto de dados.
dados_salario <- dados_salario[, c(1:3)]

View(dados_salario)

# Cria um novo df onde ele utiliza o operador %>% (pipe) para direcionar a saída do objeto dados_salario para a próxima função.
# Em seguida, o group_by(country) agrupa os dados de salário pelo nome do país.
# O summarise(max = max(year, na.rm = TRUE)) resume os dados agrupados, calculando o máximo valor da variável year para cada
# país e atribuindo o resultado a uma nova coluna chamada max. O argumento na.rm = TRUE é utilizado para remover valores
# faltantes (NA) na coluna year, caso existam. O código criou um conjunto de dados que lista cada país e o ano mais recente disponível em que foram registrados dados de desigualdade salarial

dados_salario_ano <- dados_salario %>% group_by(country) %>% summarise(max = max(year, na.rm = TRUE))

View(dados_salario_ano)


# Combinando os dataframes

# - Combinando os dois df dados_salario e dados_salario_ano usando o país usando a funcao merge().
# - O argumento by.x especifica quais colunas da tabela dados_salario devem ser usadas como chave de junção. Nesse caso, as
#   colunas são country e year. O argumento by.y especifica quais colunas da tabela dados_salario_ano devem ser usadas como
#   chave de junção. Nesse caso, as colunas são country e max.
# - Assim, a tabela resultante dados_salario2 contém informações sobre o índice de desigualdade de renda (gini_disp) para cada
#   país e ano presente na tabela dados_salario, juntamente com o ano máximo presente na tabela dados_salario_ano.
# - Caso haja países com anos diferentes entre as tabelas, os registros desses países serão descartados na junção. Já se
#   houver anos diferentes para um mesmo país nas tabelas, os registros serão mantidos e apresentados na tabela final.
# - O resultado final é um novo conjunto de dados chamado "dados_salario2", que inclui todas as colunas de 
#   "dados_salario" e mais uma coluna "max", que contém o ano mais recente para cada país.

dados_salario2 <- merge(dados_salario, dados_salario_ano, by.x = c('country', 'year'), by.y = c('country', 'max'))
View(dados_salario2)

# - Realiza um merge() entre dois data frames: dados_netflix_pib2020 e dados_salario2, baseado na colunas Country e country
#   em dados_netflix_pib2020 e na coluna country em dados_salario2.

dados_netflix_pib_salario2020 <- merge(dados_netflix_pib2020, dados_salario2, by.x = c('Country'), by.y = c('country'))
View(dados_netflix_pib_salario2020)


# Limpa o dataset de faturamento e subscrição (deleta as colunas indicadas)
dados_sub2 <- dados_sub[, c(1, 23, 24)]
View(dados_sub2)

# Com a função merge() combina os conjuntos de dados 'dados_netflix_pib_salario2020' e 'dados_sub2' com base na coluna Country.
complete <- merge(dados_netflix_pib_salario2020, dados_sub2, by = c('Country'))
View(complete)

# Faz um merge do countrycode para o complete

# Deletando colunas de countrycode
countrycode2 <- countrycode[, c(1, 3)]
View(countrycode2)

# Realiza um merge() entre dois data frames: complete e countrycode2, baseado na colunas Country e English.short.name.lower.case
# Adiciona a coluna com Codigo de Cada País
complete <- merge(complete, countrycode2, by.x = c('Country'), by.y = c('English.short.name.lower.case'))
View(complete)


# Salvando o dataframe
write.csv(complete, 'datasets_limpos/dataset1.csv', row.names = FALSE)

# carregando o dataframe
complete <- read_csv("datasets_limpos/dataset1.csv")

# Colunas do df acima

# País | Código do País | Total Catálogo | Nº Shows | Nº Filmes | Custo Básico | Custo Standard | Custo Premium |

# Diferença Valor Plano Básico - Standard  | Diferença Valor Plano Premium - Standard | PIB

# Ano | Índice Desigualdade | Nº Assinantes 4 Trimestre 2021 | Faturamento do Período | Código do País 





# *************** Limpeza e Preparação do Segundo Dataset Combinado ***************


# Limpa e filtra o dataframe IMDB
# - antes da vírgula é linha, ta vazio então quero todas as linhas
# - depois da virgula é coluna, eu quero tudo menos as colunas escolhidas (-c)

genero <- dados_IMDB[, -c(1, 4:8)] # genero <- dados_IMDB[, c(2, 3, 9)]
View(genero)

# renomeando coluna
names(genero)[names(genero) == 'primaryTitle'] <- 'show_title'
View(genero)


# associa o dados_top10 com df genero (cria um novo df com todas as colunas de dados_top10 e genero onde 'show_title' sao iguais)
topgenero <- merge(dados_top10, genero, by = "show_title")
View(topgenero)


# Limpando o dataframe anterior para manter apenas 1 entrada para cada top 10


# Em topgenero2 ficará apenas os filmes que atenderem a filtragem abaixo
# - somente quando a coluna category for 'Filmes e a coluna titleType for 'movie'
# - ou somente quando a coluna category for 'TV e a coluna titleType for 'tvSeries'

topgenero2 <- topgenero[(topgenero$category == "Films" & topgenero$titleType == "movie") |
                        (topgenero$category == "TV" & topgenero$titleType == "tvSeries"), ]
View(topgenero2)

# Em topgenero3 foi usada a funcao distinct() para remover as linhas duplicadas do dataframe topgenero2, mantendo apenas uma
# ocorrência de cada combinação única de valores das colunas show_title, week, country_name, category, titleType e cumulative_weeks_in_top_10.
# A função distinct do pacote dplyr é usada para remover as linhas duplicadas. O argumento .keep_all = TRUE é usado para manter
# todas as colunas no dataframe após a remoção das linhas duplicadas.

topgenero3 <- distinct(topgenero2, show_title, week, country_name, category, titleType, cumulative_weeks_in_top_10, .keep_all = TRUE)
View(topgenero3)


# Mantém apenas a informação de gênero de filmes por país (removendo colunas)

topgeneropaises <- topgenero3[, -c(1, 3:9)]
View(topgeneropaises)


# Pivot do dataframe 

# Utilizando a função separate() do pacote tidyr para separar a coluna genres do data frame topgeneropaises em três 
# colunas diferentes: genero1, genero2 e genero3. (antes tinhamos varios generos em apenas em 1 coluna)

topgeneropaisesPiv <- separate(topgeneropaises, c('genres'), c('genero1', 'genero2', 'genero3'), sep = ',')
View(topgeneropaisesPiv)

# Usando a função pivot_longer() do pacote tidyr para converter as colunas genero1, genero2 e genero3 em uma única
# coluna: genero123, e os valores dessas colunas foram colocados em uma nova coluna chamada genres. Isso foi feito para que
# a análise dos dados seja mais fácil, já que antes os gêneros estavam distribuídos em três colunas diferentes.

topgeneropaisesPivot <- pivot_longer(topgeneropaisesPiv, c('genero1', 'genero2', 'genero3'),
                                     names_to = 'genero123', values_to = 'genres')
View(topgeneropaisesPivot)

# colocando em ordem alfabética de acordo com o nome dos países (coluna country_name)

topgeneropaisesPivot_ord <- topgeneropaisesPivot %>% 
  arrange(country_name)
View(topgeneropaisesPivot_ord)


# Conta o número de gêneros

# contou o número de ocorrências de cada gênero para cada país criando um novo dataframe chamado generoCount que possui três
# colunas: country_name (nome do país), genres (gênero do filme ou série) e n (número de ocorrências do gênero para o país).
generoCount <- count(topgeneropaisesPivot, country_name, genres)
View(generoCount)

# remove todas as linhas que contêm valores ausentes (NA) do dataframe 
generoCount <- na.omit(generoCount)
View(generoCount)

# removeu todas as linhas em que o valor da coluna "genres" é igual a "\N" (para indicar uma única barra invertida na expressão, é necessário usá-la duas vezes)
generoCount <- subset(generoCount, genres!="\\N")
View(generoCount)

# converte a coluna "n" do data frame "generoCount" para o tipo numérico (numeric)
generoCount$n <- as.numeric(generoCount$n)
View(generoCount)


# Salva em disco (row.names FALSE é usado para que a primeira coluna do arquivo CSV gerado não contenha os nomes das linhas do dataframe, apenas os dados. Isso é útil quando se deseja importar os dados para outra ferramenta ou linguagem, sem precisar remover a primeira coluna manualmente.)
write.csv(generoCount, "datasets_limpos/dataset2.csv", row.names = FALSE)





# *************** Limpeza e Preparação do Terceiro Dataset Combinado ***************


# Renomeia a coluna 'country_name' do dataframe generoCount para 'label'
sunburst <- rename(generoCount, label = country_name)
View(sunburst)

# Remove os traços
sunburst$genres = sub("-", " ", sunburst$genres)
View(sunburst)

# Cria uma coluna nova chamada 'parent' e depois adiciona conteúdo nela
sunburst$parent = c("total - ")
sunburst$parent <- paste(sunburst$parent, sunburst$genres) 

# Cria uma coluna nova chamada 'id' e depois adiciona conteúdo nela
sunburst$id = c(' - ')
sunburst$id <- paste(sunburst$parent, sunburst$id)
sunburst$id <- paste(sunburst$id, sunburst$label)

# converte a coluna "n" do data frame "generoCount" para o tipo numérico (numeric)
sunburst$n <- as.numeric(sunburst$n)

View(sunburst)


# Agregação

# Utilizando a função "aggregate", que recebe como argumentos: a coluna "n" que se deseja agregar, a lista com as variáveis
# usadas para agrupar (neste caso, "genres") e a função "sum" que será aplicada para agregar os valores da coluna "n" por
# grupo. O resultado é um novo data frame chamado "added", contendo a soma dos valores de "n" para cada gênero.

added <- aggregate(sunburst$n, list(sunburst$genres), FUN=sum)
View(added)

# renomeando colunas
added <- rename(added, label = Group.1)
added <- rename(added, n = x)

# converte a coluna "n" do data frame "generoCount" para o tipo numérico (numeric)
added$n <- as.numeric(added$n)

# criando novas colunas ('genres', 'parent', 'id')
added$genres <- c(NA)
added$parent <- c('total')
added$id <- c(' - ')

# adicionando conteudo para a coluna 'id'
added$id <- paste(added$parent, added$id)
added$id <- paste(added$id, added$label)

View(added)

# Calcula a soma da coluna 'n'
total = sum(added$n)


# Combina tudo para o dataframe final

# uniu (concatenou) as duas tabelas added e sunburst em uma nova tabela chamada sunburstFinal. A função rbind() foi usada para
# concatenar as tabelas pela sequência das linhas, ou seja, as linhas da tabela added foram adicionadas ao final da tabela sunburst.
sunburstFinal <- rbind(added, sunburst)

# adicionou uma nova linha no inicio com as informacoes para cada coluna dentro do rbind()
sunburstFinal2 <- rbind(c("total", total, NA, NA, "total"), sunburstFinal)

# excluindo coluna
sunburstFinal2 <- sunburstFinal2[, -c(3)]

# convertendo para numerico
sunburstFinal2$n <- as.numeric(sunburstFinal2$n)

View(sunburstFinal2)

# salvando em disco
write.csv(sunburstFinal2, "datasets_limpos/dataset3.csv", row.names = FALSE)





# *************** Limpeza e Preparação do Quarto Dataset Combinado ***************


# Vamos trabalhar com top 10 para evitar problemas de performance nos gráficos (exibir somente top 10)

# apaga as primeiras 27 linhas e mantem todas as colunas
top10sunburst <- sunburstFinal2[-c(1:28),]

# transforma coluna em numerico
top10sunburst$n <- as.numeric(top10sunburst$n)

View(top10sunburst)


# Top 10 gêneros por país

# A função group_by é utilizada para agrupar as linhas do objeto top10sunburst pela coluna "label". Isso significa que todas
# as linhas com a mesma categoria em "label" serão agrupadas juntas para as próximas operações;
# A função top_n é utilizada para selecionar as top 10 linhas com o maior valor na coluna "n" para cada grupo definido pela
# coluna "label". Ou seja, para cada categoria em "label", serão selecionadas as 10 linhas com os maiores valores em "n".

top10sunburst2 <- top10sunburst %>% 
  group_by(label) %>% 
  top_n(10, n)

View(top10sunburst2)


# Recalcula os totais, ajusta e combina o dataframe

# agregacao para gerar um novo df chamado "top10add", contendo a soma dos valores de "n" para cada 'parent'.
top10add <- aggregate(top10sunburst2$n, list(top10sunburst2$parent), FUN = sum)
View(top10add)

# renomeando colunas
top10add <- rename(top10add, id = Group.1)
top10add <- rename(top10add, n = x)

# cria uma nova coluna no data frame top10add chamada label, onde os valores dessa coluna são os mesmos da coluna id, mas
# com a string "total - " removida.
top10add$label = sub("total - ", "", top10add$id)

# cria uma nova coluna chamada 'parten' onde todas as linhas tem escrito 'total'
top10add$parent = c('total')

# converte a coluna "n" do data frame "generoCount" para o tipo numérico (numeric)
top10add$n <- as.numeric(top10add$n)

total = sum(top10add$n)
total

# adiciono ao df top10sunburst2 todas as 16 linhas de top10add (as colunas possuem o mesmo nome)
top10sunburst2 <- rbind(top10add, top10sunburst2)
View(top10sunburst2)

# adiciona uma nova linha onde cada informacao dentro do vetor vai para uma coluna
top10sunburst2 <- rbind(c("total", total, NA, NA), top10sunburst2)
View(top10sunburst2)


# salva em disco
write.csv(top10sunburst2, "datasets_limpos/dataset4.csv", row.names = FALSE)




# *************** Limpeza e Preparação do Quinto Dataset Combinado ***************


# Filtra o dataframe anterior e cria um novo

# Elimina a primeira linha
nototal <- sunburstFinal2[-c(1),]
View(nototal)

# edita a coluna 'parent' onde os valores dessa coluna são os mesmos da coluna 'parent', mas com a string "total - " removida.
nototal$parent = sub('total - ', '', nototal$parent)

# edita a coluna 'parent' onde os valores dessa coluna são os mesmos da coluna 'parent', mas onde tinha apenas 'total' ficar NA
nototal$parent = sub('total', NA, nototal$parent)

# edita a coluna id onde os valores dessa coluna são os mesmos da coluna 'id', mas mas com a string "total  -  " removida.
nototal$id = sub('total  -  ', '', nototal$id)

View(nototal)


# salva em disco
write.csv(top10sunburst2, "datasets_limpos/dataset5.csv", row.names = FALSE)









topgenero <- read_csv("datasets_limpos/topgenero.csv")
