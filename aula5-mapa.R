# Renegade em sala
library(readxl)
library(devtools)
install_github("kassambara/factoextra")
library(factoextra)
library(ggthemes)

library("devtools")
install_github("kassambara/factoextra")

Renegade_19 <- read_excel("Renegade 19.xlsx",sheet = "BD1")
df <- as.data.frame(Renegade_19)
rownames(df) <- df[,1]
df <- df[,-1]
View(df)
library(ggrepel)

pc.cr <- df %>% select(atrativo:interessante) %>% prcomp(cor = TRUE)

#dimensoes
summary(pc.cr)
#loadings
pc.cr$x

#Mapa Perceptual

fviz_pca_biplot(pc.cr, repel = TRUE)+
  labs(title = "Proximidade de atributos entre as marcas(MultiDimensional Scaling)",
       caption='Fonte: Marketing Analytics - Insper') +
  theme_few()+
  theme(panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust=0,
                                    vjust=-0.5,size=8))
#Com segmentos
pc.cr <- df %>% prcomp(cor = TRUE)

fviz_pca_biplot(pc.cr, repel = TRUE)+
  labs(title = "Proximidade de atributos entre as marcas(MultiDimensional Scaling)",
       caption='Fonte: Marketing Analytics - Insper') +
  theme_few()+
  theme(panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust=0,
                                    vjust=-0.5,size=8))
