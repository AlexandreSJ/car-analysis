install.packages("googlesheets4")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("scales")

library(scales)
library(googlesheets4)
library(ggplot2)
library(dplyr)


#Carregando os dados e tranformando em tabela
url <- "https://docs.google.com/spreadsheets/d/1g21oIfZTaNqYwNxpaaIpgMrhODsDK1DYo_YNx4DPlno/edit?usp=sharing"
sheet_properties(url)

data <- read_sheet(url, sheet = 1)
print(data)

#Obtendo o percentual de CHURN
table(data$Churn)
churn <- (table(data$Churn)[2] / (table(data$Churn)[2] + table(data$Churn)[1]))
print(churn * 100)
#NOTA:Nesse bloco podemos analisar a perda e o impacto sobre a carteira de clientes da empresa.
#Essa perda é representada por 20,38% do clientes(Churn).

#obtendo o percentual de churn dos clientes que não são Ativos(frequentes).
ClienteAtivo <- (sum(data$Churn == "1" & data$ClienteAtivo == 0)) / sum(data$Churn == "1")
print(ClienteAtivo * 100)
#NOTA: Com esse resultado, podemos ver que temos uma tendencia de perder aproximadamente 63,94% dos clientes que não são ativos(frequencia)

#Exportando a planilha como PNG (Pt. 1/2)
png(filename="./analysis.png")

#Obtendo a tendencia por idade
cut(data$Idade,5)
cut(data$Idade,seq(17,97,10))
filtroChurn <- subset(data, data$Churn == "1")
barplot(table(cut(filtroChurn$Idade,seq(17,97,10))))

GrafIdadeChurn <- ggplot(data = filtroChurn, mapping = aes(x = cut(filtroChurn$Idade,seq(17,97,10)))) + 
  geom_bar() +  
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  xlab("Intervalo de Idade") +  
  ylab("Frequência")
print(GrafIdadeChurn)
#NOTA: Neste grafico podemos ver onde está a concentração de perda de clientes(churn). Portanto os top 3 perdas é:
#37 à 47 - 787 
#47 à 57 - 583
#27 à 37 - 389

GrafPercIdadeChurn <- ggplot(data = filtroChurn, mapping = aes(x = cut(filtroChurn$Idade,seq(17,97,10)))) + 
  geom_bar() +  
  geom_text(stat = 'count', aes(label = percent(..count../sum(..count..))), vjust = -0.5) +
  xlab("Intervalo de Idade") +  
  ylab("Frequência")
print(GrafPercIdadeChurn)
#NOTA: Neste grafico podemos ver onde está a concentração de perda de clientes(churn). Portanto os top 3 perdas é:
#37 à 47 - 38.6%
#47 à 57 - 28.6%
#27 à 37 - 19.1%

#Obtendo a tendencia por variedade de produtos
GrafProdutoChurn <- ggplot(data = filtroChurn, mapping = aes(x = QtdeProdutos)) + 
  geom_bar() +  
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  xlab("Intervalo de Idade") +  
  ylab("Frequência")
print(GrafProdutoChurn)
#NOTA: Podemos notar que clientes que possuem apenas um produto tem tendencia de se tornar Churn. 

GrafPercProdutoChurn <- ggplot(data = filtroChurn, mapping = aes(x = QtdeProdutos)) + 
  geom_bar() +  
  geom_text(stat = 'count', aes(label = percent(..count../sum(..count..))), vjust = -0.5) +
  xlab("Intervalo de Idade") +  
  ylab("Frequência")
print(GrafPercProdutoChurn)
#NOTA: Podemos notar 69,1% dos clientes perdidos(churn) possuem apenas um produto.

#Obtendo a tendencia por tempo de casa(por anos)
GrafPercTempoChurn <- ggplot(data = filtroChurn, mapping = aes(x = TempoCliente)) + 
  geom_bar() +  
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  xlab("Ano") +  
  ylab("Quantidade")
print(GrafPercTempoChurn)
#NOTA: Podemos notar que os dados estão bem distribuido.

#Exportando a planilha como PNG (Pt. 2/2)
dev.off()