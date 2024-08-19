

library(dplyr)

df <- experimento_mat %>%
    as_tibble()

## STUDY LEVEL

test01 <- df %>%
    select(IV_MATERIA_PATR, ESCOLARIDADE) %>%
    na.omit()

## Looking at prob table
View(cbind(prop.table(table(test01$IV_MATERIA_PATR, test01$ESCOLARIDADE), margin = 1)))

## Looking at the chi-sq test
print(chisq.test(table(test01$IV_MATERIA_PATR, test01$ESCOLARIDADE)))
## p-value low -> reject H0 -> there is association

## INCOME

test02 <- df %>%
  select(IV_MATERIA_PATR, RENDA) %>%
  na.omit()

## Looking at prob table
View(cbind(prop.table(table(test02$IV_MATERIA_PATR, test02$RENDA), margin = 1)))

## Looking at the chi-sq test
print(chisq.test(table(test02$IV_MATERIA_PATR, test02$RENDA)))
## p-value low -> reject H0 -> there is association

## MARITAL STATUS

test03 <- df %>%
  select(IV_MATERIA_PATR, EST_CIVIL) %>%
  na.omit()

## Looking at prob table
View(cbind(prop.table(table(test03$IV_MATERIA_PATR, test03$EST_CIVIL), margin = 1)))

## Looking at the chi-sq test
print(chisq.test(table(test03$IV_MATERIA_PATR, test03$EST_CIVIL)))
## p-value low -> reject H0 -> there is association between the variables


## CALCULATING DIFFERENCE BETWEEN T1 AND T2
df <- df %>%
    mutate(diffC = CRED_T2 - CRED_T1)

## Separting groups C  
control_C <- df$diffC[df$IV_MATERIA_PATR==0]
impact_C <- df$diffC[df$IV_MATERIA_PATR==1]

## Variance test in order to understand whether or not the variances are different
var.test(control_C, impact_C, ratio = 1, alternative = "two.sided")
## p-value low -> reject H0 -> true ratio of variances is not equal to 1


## Average test using t-test
t.test(impact_C, control_C, alternative = "less", paired = FALSE, var.equal = FALSE, conf.level = 0.95)
## p-value low -> reject H0 -> Mean of impact_C is less than control_C

## Using paired tests
t.test(df$CRED_T2[df$IV_MATERIA_PATR==0],
       df$CRED_T1[df$IV_MATERIA_PATR==0], 
       alternative = "less", 
       paired = TRUE,
       conf.level = 0.95)

## Using paired tests
t.test(df$CRED_T2[df$IV_MATERIA_PATR==1],
       df$CRED_T1[df$IV_MATERIA_PATR==1], 
       alternative = "less", 
       paired = TRUE,
       conf.level = 0.95)
