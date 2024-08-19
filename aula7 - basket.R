# Basket Analysis
#install.packages("arules")
library(arules)

# Dados de Compras em Mercearias

data(Groceries)
# Vamos ver o formato dos dados
inspect(Groceries[1:10])
str(Groceries)
# note que está no formato "transactions"

# Vamos selecionar as regras de associação com
# Suporte >= 0.001 e Confiança >= 0.8

rules <- apriori(Groceries, parameter = list(sup = 0.001, conf = 0.8))

# Obtivemos 410 regras de associação. Vamos inspecionar as 10 primeiras:
inspect(rules[1:10])

# Ou as com maior confiança e lift:

inspect(head(rules, n = 20, by = c("lift")))

# Vamos supor que tenhamos interesse em saber o que é comprado juntamente com cerveja:

target <- subset(rules, subset = rhs %in% "bottled beer")

inspect(target)

# Assim, dado que um consumidor adquiriu bebidas destiladas e vinho tinto ou rose, a probabilidade condicional de que adquira cerveja engarrafada ´e de 90,5%. A regra de associacao encontrada é relevante, uma vez que seu lift e de 11.2.
#install.packages("arulesViz")
library(arulesViz)
top10 <- head(rules, n = 10, by = c("lift"))
plot(top10, method = "graph",  engine = "htmlwidget")

