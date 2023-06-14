# print(getwd())
# setwd(".")
# print(getwd())

data <- read.csv("input/report.csv")

print(noquote("Dados: "))
if(is.data.frame(data)){
    print(data)
    print(noquote("Número de colunas: "))
    print(ncol(data))
    print(noquote("Número de linhas: "))
    print(nrow(data))
}