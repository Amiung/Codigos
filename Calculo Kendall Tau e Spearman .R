#
###--- PACOTES
#
install.packages("readxl")
library(readxl)
library(stats)
install.packages("Kendall")
library(Kendall)
############################
#
#---- Diretorio Leitura dos dados
setwd('C:/Users/amiun/Desktop/ProjetoGit/Doutorado/Download_Dados')
#
#---- Leitura e preparação dos dados
rm(list = ls())
dados_se <- read_excel("ENA_2018_2022.xlsx", sheet = "Sudeste")
dados_s <- read_excel("ENA_2018_2022.xlsx", sheet = "Sul")
dados_ne <- read_excel("ENA_2018_2022.xlsx", sheet = "Nordeste")
dados_n <- read_excel("ENA_2018_2022.xlsx", sheet = "Norte")
dados_sol_MG <- read_excel("Rad_Janaúba.xlsx", sheet = "Sheet 1")
dados_sol_BA <- read_excel("Rad_Juazeiro.xlsx", sheet = "Sheet 1")
dados_sol_PI <- read_excel("São Gonçalo_PI_Rad.xlsx", sheet = "Sheet 1")
dados_sol <- read_excel("ALEX_I_Valores.xlsx", sheet = "ALEX I")
head(dados_se)
head(dados_ne)
head(dados_s)
head(dados_n)
head(dados_sol_MG
head(dados_sol_BA)
head(dados_sol_PI)
head(dados_eol)
Ena_se <- dados_se[1:60,3]
Ena_s <- dados_s[1:60,3]
Ena_ne <- dados_ne[1:60,3]
Ena_n <- dados_n[1:60,3]
Candiba_Ver <- dados_eol[1:60,3]
Candiba_Sim <- dados_eol[1:60,4]
AlexI_Sim <- dados_sol[1:60,3]
K_tau_se_eol <- cor(Ena_se,Candiba_Sim, method = "kendall")
K_tau_ne_eol <- cor(Ena_ne,Candiba_Sim, method = "kendall")
K_tau_s_eol <- cor(Ena_s,Candiba_Sim, method = "kendall")
K_tau_n_eol <- cor(Ena_n,Candiba_Sim, method = "kendall")
#
K_tau_se_sol <- cor(Ena_se,AlexI_Sim, method = "kendall")
K_tau_ne_sol <- cor(Ena_ne,AlexI_Sim, method = "kendall")
K_tau_s_sol <- cor(Ena_s,AlexI_Sim, method = "kendall")
K_tau_n_sol <- cor(Ena_n,AlexI_Sim, method = "kendall")
#
K_tau_eol_sol <- cor(Candiba_Sim,AlexI_Sim, method = "kendall")
#
K_tau_se_ne <- cor(Ena_se,Ena_ne, method = "kendall")
K_tau_se_s <- cor(Ena_se,Ena_s, method = "kendall")
K_tau_se_n <- cor(Ena_se,Ena_n, method = "kendall")
K_tau_ne_s <- cor(Ena_s,Ena_ne, method = "kendall")
K_tau_s_n <- cor(Ena_s,Ena_n, method = "kendall")
K_tau_ne_n <- cor(Ena_ne,Ena_n, method = "kendall")
#
#Data <- make_date('1980') + months(0:467)
#Data <- make_date('1981') + months(0:455)
#Data <- yearmonth(Data)
#dados <- dados %>%
#  mutate(Data)
#head(dados)
#dados <- as_tsibble(dados, index = Data)
#dados_ts <- ts(dados, start = c(1980,1), frequency = 12)
##dados_ts <- ts(dados, start = c(1981,1), frequency = 12)
#head(dados_ts)
#
# Leitura dos cen?rios de ENA
cenarios = readxl::read_xlsx("Cenarios_ENA_REE_SE.xlsx")
head(cenarios)
#
# Cen?rios
cenarios <- as.matrix(cenarios)
Numero_cenario <- 2000
ncen <- Numero_cenario
n.step <- 12 # horizonte de simula??o
#
Avg_sample_Eol = Avg_sample_Ena = NULL #M?dias Mensais
Sd_sample_Eol = Sd_sample_Ena = NULL #Desvios Mensais
Min_sample_Eol = Min_sample_Ena = NULL #M?nimos Mensais
Max_sample_Eol = Max_sample_Ena = NULL #M?ximos Mensais
eol_norm = ena_norm = NULL
Temp_avg_Eol = Temp_avg_Ena = NULL
Hist_ena = Hist_eol =matrix(NA, nrow = nrow(dados)/12, ncol=12)#Matriz com dados hist?ricos
for(mes in 1:12){
  linhas = which(cycle(dados_ts) == mes)
  Avg_sample_Ena[mes] = mean(dados_ts[linhas,2])
  Avg_sample_Eol[mes] = mean(dados_ts[linhas,1])
  Sd_sample_Ena[mes] = sd(dados_ts[linhas,2])
  Sd_sample_Eol[mes] = sd(dados_ts[linhas,1])
  Min_sample_Ena[mes] = min(dados_ts[linhas,2])
  Min_sample_Eol[mes] = min(dados_ts[linhas,1])
  Max_sample_Ena[mes] = max(dados_ts[linhas,2])
  Max_sample_Eol[mes] = max(dados_ts[linhas,1])
  Temp_avg_Ena[linhas] = mean(dados_ts[linhas,2])
  Temp_avg_Eol[linhas] = mean(dados_ts[linhas,1])
  temp <-  cbind(dados_ts[linhas,1])
  Hist_eol[,mes] = dados_ts[linhas,1]
  temp <-  cbind(dados_ts[linhas,2])
  Hist_ena[,mes] = dados_ts[linhas,2]
  eol_norm[linhas] = (dados_ts[linhas,1] - Avg_sample_Eol[mes])/Sd_sample_Eol[mes]
  ena_norm[linhas] = (dados_ts[linhas,2] - Avg_sample_Ena[mes])/Sd_sample_Ena[mes]
}
Avg_sample_Ena_pu <- Avg_sample_Ena/mean(Avg_sample_Ena) # Ena m?dia mensal em PU da m?dia anual
Avg_sample_Eol_pu <- Avg_sample_Eol/mean(Avg_sample_Eol) # Eol m?dia mensal em PU da m?dia anual

dados <- dados %>%
  mutate(Ena_Month_avg = Temp_avg_Ena, Eol_Month_avg = Temp_avg_Eol,Eol_norm=eol_norm, Ena_norm=ena_norm)
head(dados)
#
#---- An?lises Gr?fica

#1) S?ries Individuais
dados %>%
  pivot_longer(c(Eol, Eol_Month_avg), names_to="S?ries") %>%
  autoplot(value)+labs(y='MWmed', x='Per?odo')

acf(dados$Eol, lag.max = 36, ylab="ACF", xlab="Lag", lwd=2, col=2, main="S?rie de Gera??o E?lica")
pacf(dados$Eol, lag.max = 36, ylab="PACF", xlab="Lag", lwd=2, col=2, main="S?rie de Gera??o E?lica")
hist(dados$Eol,main="Ger??o E?lica",cex.main=.8, col='lightblue', xlab = '', freq = F)+
  curve(dnorm(x,mean = mean(dados$Eol), sd=sd(dados$Eol)), add = T, col='red')

dados %>%
  pivot_longer(c(Ena, Ena_Month_avg), names_to="S?ries") %>%
  autoplot(value)+labs(y='MWmed', x='Per?odo')

acf(dados$Ena, lag.max = 36, ylab="ACF", xlab="Lag", lwd=2, col=2, main='S?rie de Energia Natural Afluente')
pacf(dados$Ena, lag.max = 36, ylab="PACF", xlab="Lag", lwd=2, col=2, main='S?rie de Energia Natural Afluente')
hist(dados$Ena,main="Energia Natural Afluente",cex.main=.8, col='lightblue', xlab = '', freq = F)+
  curve(dnorm(x,mean = mean(dados$Ena), sd=sd(dados$Ena)), add = T, col='red')
dados %>%
  pivot_longer(c(Ena, Eol),
               names_to = "var", values_to = "value") %>%
  ggplot(aes(x = Data, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  labs(title = "Hist?rico de ENA e Gera??o E?lica",
       y = "MWmed", x="Per?odo")

# Teste de estacionariedade
x = ur.df(dados$Eol, type = "drift", selectlags = "AIC")
summary(x)

y = ur.df(dados$Ena, type = "drift", selectlags = "AIC")
summary(y)

#2) An?lise da Correla??o

plot(Avg_sample_Ena_pu, type='l', col='darkblue', xlab = 'Meses', ylab = '',
     main= "Perfil - Energia Natural Afluente - SE x Gera??o E?lica NE") # Gr?fico m?dia mensal
lines(Avg_sample_Eol_pu, col="red")
legend("topright",bty="n",col=c("darkblue",'red'),
       legend=c("ENA - SE","Gera??o E?lica"),lty=c(1,1), cex=.7)
dados %>%
  ggplot(aes(x = Ena, y =Eol)) +
  labs(y = "Gera??o E?lica",
       x = "Energia Natural Afluente (REE-SE)") +
  geom_point() +
  geom_smooth(method = "auto", se = FALSE)
  #stat_smooth()

Coef_correlacao <- cor(dados$Ena, dados$Eol ,method = "spearm")

#
#--- Regress?o Din?mica (Aplica??o FPP3)

# 1) Linear

Model_linear <- dados %>%
  #model(ARIMA(Eol ~ Ena))
   model(ARIMA(Eol ~ Ena + pdq(1,0,1) + PDQ(1,0,0)))
Model_linear %>% gg_tsresiduals()
report(Model_linear)
res_linear <- resid(Model_linear) # s?rie de erros
acf(res_linear$.resid, ylab="f.a.c", xlab="M?s", lwd=2, col=2, main=NA)
pacf(res_linear$.resid, lag.max = 48, ylab="f.a.c", xlab="M?s", lwd=2, col=2, main=NA)

# 2) Log

Model_log <- dados %>%
  model(ARIMA(Eol ~ Ena + log(Ena)))
Model_log %>% gg_tsresiduals()
report(Model_log)
res_log <- resid(Model_log) # s?rie de erros
acf(res_log$.resid, ylab="f.a.c", xlab="M?s", lwd=2, col=2, main=NA)
pacf(res_log$.resid, lag.max = 48, ylab="f.a.c", xlab="M?s", lwd=2, col=2, main=NA)



# 3) Quadr?tica

Model_qua <- dados %>%
  model(ARIMA(Eol ~ Ena + I(Ena^2)))
Model_qua %>% gg_tsresiduals()
report(Model_qua)
res_qua <- resid(Model_qua) # s?rie de erros
acf(res_qua$.resid, ylab="FAC", xlab="M?s", lwd=2, col=2, main=NA)
pacf(res_qua$.resid, lag.max = 48, ylab="PACF", xlab="M?s", lwd=2, col=2, main=NA)

# 4) C?bica

Model_cub <- dados %>%
  model(ARIMA(Eol ~ Ena + I(Ena^3)))
Model_cub %>% gg_tsresiduals()
report(Model_cub)
res_cub <- resid(Model_cub) # s?rie de erros
acf(res_cub$.resid, ylab="FAC", xlab="M?s", lwd=2, col=2, main=NA)
pacf(res_cub$.resid, lag.max = 48, ylab="PACF", xlab="M?s", lwd=2, col=2, main=NA)

#
#--- Simula??o melhor modelo ajustado - Cen?rios de ENA NEWAVE (Aplica??o FPP3)

ncen <- 2000 # n?mero de cen?rio de ENA - simula??o NEWAVE
n.step <- 12 # horizonte dos cen?rios
eol_cenarios = matrix(NA, nrow = ncen, ncol = n.step)
fit <- Model_log # recebe dados do modelo ajustado para simula??o
for (i in 1:ncen) {
  cen_ena <- new_data(dados,12) %>%
    mutate(Ena = cenarios[i,2:13])
  fcast <- forecast(fit, new_data = cen_ena)
  eol_cenarios[i,] <- fcast$.mean
}
media_eol_cen=NULL
for (i in 1:12) {
  media_eol_cen[i] = eol_cenarios[,i]
}

# Gr?fico Simula??o
#
# 1) ENAs para simula??o
plot(Hist_ena[1,],type="l",ylim=range(dados$Ena),col="grey90",
     ylab = "MWmed",xlab="Meses", main="Hist?rico de ENA - REE SE (1980-2018)", cex.axis=0.7, cex.lab=.8, cex.main=0.9)
for(i in 2:nrow(dados)/12){
  lines(Hist_ena[i,],col="grey90")
}
lines(Avg_sample_Ena,col=2,lty=2)
lines(Min_sample_Ena,col="darkgreen", lty=2)
lines(Max_sample_Ena,col="black", lty=2)
legend("topright",bty="n",col=c("grey90",2,4, 'darkgreen', 'black'),
       legend=c("Hist?rico","M?dia Hist?rica", 'M?nima historico', 'M?xima hist?rico'),lty=c(1,2,1,2,2), cex=.7)
#
# 2) Cen?rios Gera??o E?lica
plot(eol_cenarios[1,],type="l",ylim=range(eol_cenarios),col="grey90",
     ylab = "Gera??o E?lica em MWmed",xlab="Passos a frente", main="Simula?ao de Gera??o - Complexo E?lico Alto Sert?o - BA", cex.axis=0.7, cex.lab=.8, cex.main=0.9)
for(i in 2:ncen){
  lines(eol_cenarios[i,],col="grey90")
}

lines(Avg_sample_Eol,col=2,lty=2)
lines(media_eol_cen,col=4)
lines(Min_sample_Eol,col="darkgreen", lty=2)
lines(Max_sample_Eol,col="black", lty=2)
legend("topleft",bty="n",col=c("grey90",2,4, 'darkgreen', 'black'),
       legend=c("Simula??o","M?dia Hist?rica","M?dia Simula??o", 'M?nima historico', 'M?xima hist?rico'),lty=c(1,2,1,2,2), cex=.7)

#
#--- Regress?o Din?mica [Com PAR(p)]

# 1) Recuperar os erros do modelo de Regress?o (Ni)

fit <- dados %>%
  model(tslm=TSLM(Eol ~ Ena + log(Ena) ))
report(fit)#Recuperar Beta 0
beta_0 <- fit %>% coef()
beta_0 <- beta_0$estimate[1]
betas <- Model_log %>% coef()
beta_1 <- betas$estimate[5]
beta_2 <- betas$estimate[6]
Regressao = NULL
Ni_t = NULL
for (i in 1:nrow(dados)) {
  Regressao[i] <- beta_0+beta_1*dados$Ena[i]+beta_2*log(dados$Ena[i])
  Ni_t[i] <- Regressao - dados$Eol[i]
}
dados <- dados %>%
  mutate(Ni = Ni_t)

# 2) Ajuste do modelo PAR(p) para os ru?dos da regress?o

dados %>%
  pivot_longer(c(Ni), names_to="S?ries") %>%
  autoplot(value)+labs(y='', x='Per?odo')

hist(dados$Ni,main="Histograma Res?duos Regress?o", col='lightgrey', xlab = '', freq = F)+
  curve(dnorm(x,mean = mean(dados$Ni), sd=sd(dados$Ni)), add = T, col='red')
pacf(dados$Ni, main= "PACF - Ru?dos da Regress?o")

Ni_ts <- ts(dados$Ni, start = c(1980,1), frequency = 12)
lag <- stats::lag
Model_PARp <- pear(Ni_ts, m= c(1,1,1,1,1,1,1,1,1,1,1,1))
Ruido_PARp <- Model_PARp$residuals
pacf(Ruido_PARp)
Coef_PARp <-Model_PARp$phi

#--- Simula??o com modelo PARp - Cen?rios de ENA NEWAVE (Aplica??o FPP3)

ncen <- 2000 # n?mero de cen?rio de ENA - simula??o NEWAVE
n.step <- 12 # horizonte dos cen?rios
eol_cenarios_2 = matrix(NA, nrow = ncen, ncol = n.step) # Cen?rios de gera??o com modelo PARp

#
# Previs?o 12 passos ? frente com o PARp
ordem_PARp <- 1 # Ordem igual para todos os meses
Num_cases <- nrow(dados)
Fcast.PARp = matrix(NA, ncol = 1, nrow = 2*n.step)
Fcast.PARp[1:12] <- Ni_t[(Num_cases-11):Num_cases]
Avg_Ni_t = NULL # M?dia dos erros da regress?o
Sd_Ni_t = NULL # Desvio Padr?o dos erros da regress?o
for(mes in 1:12){
  linhas = which(cycle(Ni_t) == mes)
  Avg_Ni_t[mes] = mean(Ni_t[linhas]) 
  Sd_Ni_t[mes] = sd(Ni_t[linhas]) 
}
ciclo <- cycle(Ni_ts)
for(cases in 13:(2*n.step)){
  mes <- ciclo[cases]
  z_chapeu = NULL
  temp_calc <- 0
  for (j in 1:ordem_PARp) {
    if(mes-j <= 0){
      mes_aux <- (mes - j + 12)
    } else{
      mes_aux <- (mes - j)
    }
    z_chapeu[j] = Coef_PARp[mes, j] * ((Fcast.PARp[(cases-j)]-Avg_Ni_t[mes_aux]))/Sd_Ni_t[mes_aux]
    temp_calc <-  Avg_Ni_t[mes]+z_chapeu[j]*Sd_Ni_t[mes]+temp_calc
  }
  Fcast.PARp[cases] <- temp_calc
}

#
# C?lculo dos 2000 cen?rios

for (i in 1:ncen) {
  for (j in 1:12) {
    eol_cenarios_2[i,j] = beta_0 + beta_1*cenarios[i,j+1]+beta_2*log(cenarios[i,j+1])+Fcast.PARp[j]
  }
  eol_cenarios_2[i]
}


# Gr?fico Simula??o
#
# 1) Cen?rios Gera??o E?lica
plot(eol_cenarios_2[1,],type="l",ylim=range(eol_cenarios_2),col="grey90",
     ylab = "Gera??o E?lica em MWmed",xlab="Passos a frente", main="Simula?ao de Gera??o - Complexo E?lico Alto Sert?o - BA", cex.axis=0.7, cex.lab=.8, cex.main=0.9)
for(i in 2:ncen){
  lines(eol_cenarios_2[i,],col="grey90")
}

lines(Avg_sample_Eol,col=2,lty=2)
lines(media_eol_cen,col=4)
lines(Min_sample_Eol,col="darkgreen", lty=2)
lines(Max_sample_Eol,col="black", lty=2)
#lines(c(valida_vazao_mensal),col=5)
legend("bottom",bty="n",col=c("grey90",2,4, 'darkgreen', 'black'),
       legend=c("Simula??o","M?dia Hist?rica","M?dia Simula??o", 'M?nima historico', 'M?xima hist?rico'),lty=c(1,2,1,2,2), cex=.7)

#--- Simula??o melhor modelo ajustado - Cen?rios de ENA NEWAVE (Aplica??o FPP3)

ncen <- 2000 # n?mero de cen?rio de ENA - simula??o NEWAVE
n.step <- 12 # horizonte dos cen?rios
eol_cenarios = matrix(NA, nrow = ncen, ncol = n.step)
fit <- Model_log # recebe dados do modelo ajustado para simula??o
for (i in 1:ncen) {
  cen_ena <- new_data(dados,12) %>%
    mutate(Ena = cenarios[i,2:13])
  fcast <- forecast(fit, new_data = cen_ena)
  eol_cenarios[i,] <- fcast$.mean
}
media_eol_cen=NULL
for (i in 1:12) {
  media_eol_cen[i] = eol_cenarios[,i]
}


#################################################################
#################################################################
#################################################################
#################################################################
#Testes
fit <- dados %>%
  model(tslm=TSLM(Eol ~ Ena))
report(fit)
erros_l <- resid(fit)
erros <- erros_l$.resid
pacf(erros)
mean(erros)
fit <- auto.arima(erros)
fit$coef
fit2 <- fit %>%model(ARIMA(.resid))

teste <- auto.arima(Ni_t)
teste$coef

dados_3 <- dados %>% 
  mutate(Ni = Ni_t)
head(dados_3)


fit_3 <- dados_3 %>%
  model(ARIMA(Ni ~ Ena))
report(fit_3)

ARIMA
