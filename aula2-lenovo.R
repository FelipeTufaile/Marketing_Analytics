
# Bibliotecas -------------------------------------------------------------
library(readxl)
library(tidyverse)

# Base de dados -----------------------------------------------------------

lenovo <- read_excel("lenovo.xlsx")


# Pergunta 1 -------------------------------------------------------------

# Nossos clientes estão satisfeitos? (q4)

lenovo %>% 
  mutate(q4 = factor(q4,
                     labels = c("Muito Satisfeito",
                                    "Satisfeito",
                                    "Instasisfeito", 
                                    "Muito Insatisfeito"))) %>% 
  ggplot(aes(q4))+
  geom_bar(fill = "lightblue")+
  theme_minimal()+
  labs(x = "",
       y = "Contagem",
       title = "Grau de satisfação com a Lenovo (n = 372)")

# Recomendariam nosso produto? q5
lenovo %>% 
  mutate(q5 = factor(q5,
                     labels = c("definitivamente recomendaria",
                                 "provavelmente",
                                 "talvez sim ou talvez não",
                                 "provavelmente não",
                                 "definitivamente não recomendaria"))) %>% 
  ggplot(aes(q5))+
  geom_bar(fill = "lightgreen")+
  theme_minimal()+
  labs(x = "",
       y = "Contagem",
       title = "Recomendação Lenovo (n = 372)")+
  coord_flip()

# Recomprariam nosso produto? q6

lenovo %>% 
  mutate(q6 = factor(q6,
                     labels = c("definitivamente recompraria",
                                "provavelmente",
                                "talvez sim ou talvez não",
                                "provavelmente não"))) %>% 
  ggplot(aes(q6))+
  geom_bar(fill = "lightpink")+
  theme_minimal()+
  labs(x = "",
       y = "Contagem",
       title = "Intenção de Recompra Lenovo (n = 372)")+
  coord_flip()

# Associação entre as variáveis -------------------------
# Será que existe associação entre essas variáveis?

# Satisfação x Recomendação
lenovo <- lenovo %>% 
  mutate(q4 = factor(q4,
                     labels = c("Muito Satisfeito",
                                "Satisfeito",
                                "Instasisfeito", 
                                "Muito Insatisfeito")),
         q5 = factor(q5,
                     labels = c("definitivamente recomendaria",
                                "provavelmente",
                                "talvez sim ou talvez não",
                                "provavelmente não",
                                "definitivamente não recomendaria")),
         q6 = factor(q6,
                     labels = c("definitivamente recompraria",
                                "provavelmente",
                                "talvez sim ou talvez não",
                                "provavelmente não")))

# Satisfação e Recomendação 
prop.table(table(lenovo$q4))
View(cbind(prop.table(table(lenovo$q4,lenovo$q5), margin = 2)))
# só olhando para a tabela, parece ter relação entre satisfação e recomendação?
# Teste Qui-quadrado de associação entre variáveis categóricas
print(chisq.test(table(lenovo$q4,lenovo$q5)))
# p-valor bem pequeno > rejeita a H0 > conclui que há associação



# Satisfação e Recompra 
prop.table(table(lenovo$q4))
View(cbind(prop.table(table(lenovo$q4,lenovo$q6), margin = 2)))
# só olhando para a tabela, parece ter relação entre satisfação e recomendação?
# Teste Qui-quadrado de associação entre variáveis categóricas
print(chisq.test(table(lenovo$q4,lenovo$q6)))
# p-valor bem pequeno > rejeita a H0 > conclui que há associação


# Recomendação e Recompra
prop.table(table(lenovo$q5))
View(cbind(prop.table(table(lenovo$q5,lenovo$q6), margin = 2)))
# só olhando para a tabela, parece ter relação entre satisfação e recomendação?
# Teste Qui-quadrado de associação entre variáveis categóricas
print(chisq.test(table(lenovo$q5,lenovo$q6)))
# p-valor bem pequeno > rejeita a H0 > conclui que há associação

