# Turquia
library(readxl)
dados <- read_excel("D:/ATUALIZA_PASTA_d/A Nova pasta/Turquia/Pasta1.xlsx", 
                     col_types = c("text", "numeric", "numeric", 
                                   "numeric", "skip", "numeric", "numeric", "numeric"))


summary(dados)
table(as.factor(dados$CHP_govern_provinc_in2014))# 13 das 65
table(as.factor(dados$kurdist_strong)) # em 7 das 65

# partido era o mais forte, golpe 1980 - excluido - permitido o mesmo nome (supostamente mesma ideologia)


# toda a a análise #############

#1995 
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

# mais um -   O MAIS IMPORTANTE
library(dplyr)

dados$size<- log(dados$size)

summary(dados)
cor.test(dados$kurdist_strong, dados$CHP_govern_provinc_in2014) #não
cor.test(dados$CHP_govern_provinc_in2014, dados$size) #não
cor.test(dados$CHP_2018, dados$CHP_govern_provinc_in2014)#não
dados$kurdist_strong <- as.factor(dados$kurdist_strong)
dados$CHP_govern_provinc_in2014 <- as.factor(dados$CHP_govern_provinc_in2014)
reg5 <- lm(CHP_2018 ~ CHP_1977 + CHP_1995 + kurdist_strong + size + CHP_govern_provinc_in2014
           , data= dados)
summary(reg5)
coefplot(reg5, intercept=FALSE)
library(huxtable)
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
tab_model(reg5x, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")
#muito importante

coefplot(reg5x, intercept=FALSE)

# os dados de 1977 explicam mais que qualquer coisa


##### análise dos resíduos , modelo completo

base <- dados

base$predicted <- predict(reg5)   # Save the predicted values
base$residuals <- residuals(reg5) # Save the residual values

h <- ggplot(base, aes(x = CHP_1977, y = CHP_2018)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  geom_text(label=base$Province)+       
  geom_segment(aes(xend = CHP_1977, yend = predicted), alpha = 20) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals)), shape=3) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()
h

#
huxreg(reg5)
library(marginaleffects)
plot_cap(reg5, condition = c("CHP_1977"))#boa
summary(dados$CHP_1977)
66.3-12.3 # intervalo
54/9 # da numero inteiro
predictions(reg5, newdata = datagrid(CHP_1977 = seq(12.30, 66.30, 9)))#boa prections 1977

plot_cap(reg5, condition = c("CHP_1977","CHP_govern_provinc_in2014"))#boa
plot_cap(reg5, condition = c("CHP_1995","CHP_govern_provinc_in2014"))#nem tanto
plot_cap(reg5, condition = c("CHP_1977","kurdist_strong"))#boa

# predictions outras
summary(dados)

# terceiro quartil CHP77 e 95 e kurdis =não, CHP2014=sim
predictions(reg5, newdata = datagrid(CHP_1977 = 45.20, CHP_1995=11.86, kurdist_strong=0,CHP_govern_provinc_in2014=1))

# primeiro quartil CHP77 e 95 e kurdis =sim, CHP2014=nao
predictions(reg5, newdata = datagrid(CHP_1977 = 30.60, CHP_1995=5.88, kurdist_strong=1,CHP_govern_provinc_in2014=0))

# há uma diferença notória e estatisticamente significativa.



coefplot(reg5, intercept=FALSE, interactive=TRUE)# dar zoom CHP 1995 e CHP 1977

# só pra confirmra
plot(dados$CHP_1995, dados$CHP_1977)
cor(dados$CHP_1995, dados$CHP_1977)

plot(dados$CHP_1995, dados$CHP_2018)
plot(dados$CHP_1977, dados$CHP_2018)
# só pra confirmar


# os gráficos e tabelas a serem utilzados regressão linear ####
mfx <- marginaleffects(reg5)
head(mfx, 4)
summary(mfx)# da pra ver os intervalos
library(ggeffects)
plot_cap(reg5, condition = "CHP_govern_provinc_in2014")
p1 <- ggpredict(reg5, c("CHP_govern_provinc_in2014"))# numeros
p1
plot_cap(reg5, condition = "CHP_1977")
p2<- ggpredict(reg5, c("CHP_1977"))# numeros
p2
plot_cap(reg5, condition = "CHP_1995")
plot_cap(reg5, condition = "size")
plot_cap(reg5, condition = "kurdist_strong")
p3<- ggpredict(reg5, c("kurdist_strong"))# numeros
p3


library(jtools)
reg6 <- lm(CHP_2018 ~ CHP_1977 +  kurdist_strong + size + CHP_govern_provinc_in2014
           , data= dados)

reg6x <- lm.beta(reg6)
plotg <- plot_coefs(reg5x,reg6x, 
                     model.names = c("CHP in 2018(modelo1)","CHP in 2018(modelo2)"),
                     legend.title = "Anos",
                     inner_ci_level = .9,
                     point.shape = FALSE,
                     rescale.distributions=TRUE)
plotg#explicar
tab_model(reg5, reg6, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")# apêndice

# modelo multinível ####
library(lavaan)

#CHP_2018 ~ CHP_1977 +  kurdist_strong + size + CHP_govern_provinc_in2014(dado por CHP1977 e CHP 1995 tambem) + CHP_1995(dado por CHP1977 tambem)
df <- dados[,2:7]
df$kurdist_strong <- as.numeric(df$kurdist_strong)
df$CHP_govern_provinc_in2014 <- as.numeric(df$CHP_govern_provinc_in2014)
modelmulti1<-'
#equation where (endo) is predicted by (exogeneou)
CHP_1995~CHP_1977
CHP_govern_provinc_in2014~CHP_1977
CHP_govern_provinc_in2014~CHP_1995
#equation where bolso is predicted by sexo(exo) e puni
CHP_2018~CHP_1977+kurdist_strong+size+CHP_govern_provinc_in2014+CHP_1995
#estimtating the variances of the exogenous variables 
CHP_1977~~CHP_1977
CHP_1995~~CHP_1995
#estimtating the covariances of the exogenous variables
size~~size
kurdist_strong~~kurdist_strong
CHP_govern_provinc_in2014~~CHP_govern_provinc_in2014
CHP_2018~~CHP_2018'


fit<-lavaan(modelmulti1,data=df)
fitMeasures(fit)#
parameterEstimates(fit)#estimações


## gráficos modelos multinível #####
library(lavaanPlot)
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = 
                                              "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE,covs=
             TRUE,stars = c("regress"))
