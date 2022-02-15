# Turquia
library(readxl)
dados <- read_excel("D:/ATUALIZA_PASTA_d/A Nova pasta/Turquia/Pasta1.xlsx", 
                     col_types = c("text", "numeric", "numeric", 
                                   "numeric", "skip", "numeric", "numeric", "numeric"))


summary(dados)
table(as.factor(dados$CHP_govern_provinc_in2014))
table(as.factor(dados$kurdist_strong))

# partido era o mais forte, golpe 1980 - excluido - permitido o mesmo nome (supostamente mesma ideologia)
# 1995 
# volta a ter força (segundo mais forte na década de 2010)
cor.test(dados$CHP_1977, dados$CHP_1995)
cor.test(dados$CHP_1977, dados$CHP_2018)
cor.test(dados$CHP_1995, dados$CHP_2018)
reg1 <- lm(CHP_2018 ~ CHP_1977, data= dados)
summary(reg1)
reg2 <- lm(CHP_2018 ~ CHP_1977 + CHP_1995, data= dados)
summary(reg2)
library(coefplot)
coefplot(reg2, intercept=FALSE)

# com um controle
reg3 <- lm(CHP_2018 ~ CHP_1977 + CHP_1995 + kurdist_strong, data= dados)
summary(reg3)
coefplot(reg3, intercept=FALSE)
# beta no final

# voltando
cor.test(dados$kurdist_strong, dados$size) #não
reg4 <- lm(CHP_2018 ~ CHP_1977 + CHP_1995 + kurdist_strong + size, data= dados)
summary(reg4)
coefplot(reg4, intercept=FALSE)



library(huxtable)
huxreg(reg1, reg2, reg3, reg4)



#diagnóstico
plot(reg4)


#install.packages("olsrr")
library(olsrr)
ols_vif_tol(reg4)
ols_eigen_cindex(reg4)

# visual
ggplot(data = dados, aes(x = CHP_1977, y = CHP_2018)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

# mais um
cor.test(dados$kurdist_strong, dados$CHP_govern_provinc_in2014) #não
cor.test(dados$CHP_govern_provinc_in2014, dados$size) #não
cor.test(dados$CHP_2018, dados$CHP_govern_provinc_in2014)#não
reg5 <- lm(CHP_2018 ~ CHP_1977 + CHP_1995 + kurdist_strong + size + CHP_govern_provinc_in2014
           , data= dados)
summary(reg5)
coefplot(reg5, intercept=FALSE)
huxreg(reg5)
huxreg(reg1, reg2, reg3, reg4, reg5)
#diag
#diagnóstico
plot(reg5)
ols_vif_tol(reg5)
ols_eigen_cindex(reg5)
#
library(lm.beta)
reg5x <- lm.beta(reg5)
library(sjPlot)
lista.modelo <- list(reg5, reg5x)
tab_model(lista.modelo, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")
#muito importante

# os dados de 1977 explicam mais que qualquer coisa