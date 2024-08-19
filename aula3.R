library(tidyverse)
library(ggthemes)

anova <- read.csv("Aula3-anova.csv", sep = ";")

# Efeito de desconto nas vendas

# Visualmente:

anova %>% 
  group_by(desconto) %>%
  summarize(media_vendas = mean(vendas)) %>% 
  mutate(desconto = recode(desconto,"1" = "Sim", "2" = "Não")) %>% 
  ggplot(aes(x = desconto, y = media_vendas, fill = desconto)) + 
  geom_col(show.legend = FALSE)+
  geom_text(aes(label = round(media_vendas,2)),vjust = -0.5)+
  labs(title = "Efeito de Desconto nas vendas", y = "Vendas (média)", x = "")+
  theme_minimal()

# Essa diferença é estatisticamente signficante?

# Teste t de médias
t.test(vendas~desconto, alternative = "less", data = anova %>% 
         mutate(desconto = recode(desconto,"1" = "Sim", "2" = "Não")))
?t.test
#H0: media_nao - media_sim = 0
#HA: media_nao - media_sim < 0

# One-way anova

modelo1 <-aov(vendas~desconto, data = anova %>% 
               mutate(desconto = recode(desconto,"1" = "Sim", "2" = "Não")))
anova(modelo1)

#Error or within-group variance
anova(modelo1)["Residuals", "Mean Sq"]

#Treatment or between-group variance:
anova(modelo1)["desconto", "Mean Sq"]

# Efeito de Trade nas Vendas

# Visualmente:

anova %>% 
  group_by(trade) %>%
  summarize(media_vendas = mean(vendas)) %>% 
  mutate(trade = recode(trade, "1" = "Gondola", "2" = "Tabloide", "3" = "Display")) %>% 
  ggplot(aes(x = trade, y = media_vendas, fill = trade)) + 
  geom_col(show.legend = FALSE)+
  geom_text(aes(label = round(media_vendas,2)),position = position_dodge(width = 1),
            vjust = -0.5)+
  labs(title = "Efeito de Desconto nas vendas", y = "Vendas (média)", x = "")+
  theme_minimal()+
  theme(axis.line=element_blank(), panel.border =element_blank(), panel.grid.major.x =element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(hjust = 0, vjust = 7,size = 8))+
  scale_fill_brewer(palette = "Pastel1")

# Essa diferença é estatisticamente signficante?

# One-way anova

modelo3 <-aov(vendas~trade, data = anova %>% 
                mutate(trade = recode(trade, "1" = "Gondola", "2" = "Tabloide", "3" = "Display")))
anova(modelo3)

#Error or within-group variance
anova(modelo3)["Residuals", "Mean Sq"]

#Treatment or between-group variance:
anova(modelo3)["trade", "Mean Sq"]

# Quais são as diferenças?


attach(anova %>%   
         mutate(trade = recode(trade, "1" = "Gondola", "2" = "Tabloide", "3" = "Display")))

pairwise.t.test(vendas, trade)
detach()

# Efeito de Desconto e Trade nas Vendas (Two-way Anova)

# Visualização das diferenças nas médias

anova %>%   
  mutate(desconto = recode(desconto,"1" = "Sim", "2" = "Não"),
                   trade = recode(trade, "1" = "Gondola", "2" = "Tabloide", "3" = "Display")) %>% 
  group_by(trade, desconto) %>%
  summarize(media_vendas = mean(vendas)) %>% 
  ggplot(aes(x = trade, y = media_vendas, fill = desconto)) + 
  geom_col(position = "dodge")+
  geom_text(aes(label = round(media_vendas,2)),position = position_dodge(width = 1),
            vjust = -0.5)+
  labs(title = "Efeito de Desconto nas vendas", y = "Vendas (média)", x = "")+
  theme_minimal()+
  theme(axis.line=element_blank(), panel.border =element_blank(), panel.grid.major.x =element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(hjust = 0, vjust = 7,size = 8))+
  scale_fill_brewer(palette = "Pastel1")




#Tabela descritiva
aggregate(vendas ~ desconto*trade, data = anova %>%   
            mutate(desconto = recode(desconto,"1" = "Sim", "2" = "Não"),
                   trade = recode(trade, "1" = "Gondola", "2" = "Tabloide", "3" = "Display")),
            FUN = function(x) c(media = mean(x), dp = sd(x), freq = length(x)))


# Two-way Anova (duas variáveis para agrupamento)
#H0: The means are equal for both variables (i.e., factor variable)
#H1: The means are different for both variables

modelo2 <-aov(vendas~desconto*trade, data = anova %>%   
               mutate(desconto = recode(desconto,"1" = "Sim", "2" = "Não"),
                      trade = recode(trade, "1" = "Gondola", "2" = "Tabloide", "3" = "Display")))
anova(modelo2)

# Pairwise 
attach(anova %>%   
         mutate(desconto = recode(desconto,"1" = "Sim", "2" = "Não"),
                trade = recode(trade, "1" = "Gondola", "2" = "Tabloide", "3" = "Display"),
                inter = paste0(desconto,"x",trade)))

pairwise.t.test(vendas, inter,p.adj = "bonf")
detach()

t.test(x= CRED_T1, y= CRED_T2, alternativa = "two.sided",data = experimento_mat)

