#
###--- PACOTES
#
install.packages("readxl")
library(readxl)
library(stats)
install.packages("Kendall")
library(Kendall)
install.packages("tidyverse")
library(tidyverse)
install.packages("copula")
library(copula)
library(MASS)
install.packages("xlsx")
library(xlsx)
library(ggplot2)
install.packages("ggpmisc")
library(ggpmisc)
############################
#
#---- Diretorio Leitura dos dados
#setwd('C:/Users/frog-/OneDrive/Área de Trabalho/Anderson_Frog/Dados')
setwd('C:/Users/amiun/Desktop/ProjetoGit/Doutorado/Download_Dados')

#---- Leitura e preparação dos dados
rm(list = ls())
dados_se <- read_excel("ENA_2018_2022.xlsx", sheet = "Sudeste")
dados_s <- read_excel("ENA_2018_2022.xlsx", sheet = "Sul")
dados_ne <- read_excel("ENA_2018_2022.xlsx", sheet = "Nordeste")
dados_n <- read_excel("ENA_2018_2022.xlsx", sheet = "Norte")
dados_sol_MG <- read_excel("Rad_Janaúba.xlsx", sheet = "Sheet 1")
dados_sol_BA <- read_excel("Rad_Juazeiro.xlsx", sheet = "Sheet 1")
dados_sol_PI <- read_excel("São Gonçalo_PI_Rad.xlsx", sheet = "Sheet 1")
dados_eol_BA <- read_excel("Santo Se_Veloc_Vento_BA.xlsx", sheet = "Sheet 1")
dados_eol_RN <- read_excel("Serra do Mel_Veloc_Vento_RN.xlsx", sheet = "Sheet 1")
dados_eol_PI <- read_excel("Dom Inocêncio_Vel.xlsx", sheet = "Sheet 1")
#
head(dados_se)
head(dados_ne)
head(dados_s)
head(dados_n)
head(dados_sol_MG)
head(dados_sol_BA)
head(dados_sol_PI)
head(dados_eol_BA)
head(dados_eol_RN)
head(dados_eol_PI)
#
Ena_se <- data.matrix(dados_se[1:60,3])
Ena_s <- data.matrix(dados_s[1:60,3])
Ena_ne <- data.matrix(dados_ne[1:60,3])
Ena_n <- data.matrix(dados_n[1:60,3])
Rad_BA <- data.matrix(dados_sol_BA[1:60,2])
Rad_MG <- data.matrix(dados_sol_MG[1:60,2])
Rad_PI <- data.matrix(dados_sol_PI[1:60,2])
Vel_BA <- data.matrix(dados_eol_BA[1:60,2])
Vel_RN <- data.matrix(dados_eol_RN[1:60,2])
Vel_PI <- data.matrix(dados_eol_PI[1:60,2])
#

#-----Análise descritiva dos dados
#


#-----Calculo da Dependência entre as variáveis (Kendall Tau)
#
# ENA x Radiação 
K_tau_se_sol_BA <- cor(Ena_se,Rad_BA, method = "kendall")
K_tau_ne_sol_BA <- cor(Ena_ne,Rad_BA, method = "kendall")
K_tau_s_sol_BA <- cor(Ena_s,Rad_BA, method = "kendall")
K_tau_n_sol_BA <- cor(Ena_n,Rad_BA, method = "kendall")
#
K_tau_se_sol_MG <- cor(Ena_se,Rad_MG, method = "kendall")
K_tau_ne_sol_MG <- cor(Ena_ne,Rad_MG, method = "kendall")
K_tau_s_sol_MG <- cor(Ena_s,Rad_MG, method = "kendall")
K_tau_n_sol_MG <- cor(Ena_n,Rad_MG, method = "kendall")
#
K_tau_se_sol_PI <- cor(Ena_se,Rad_PI, method = "kendall")
K_tau_ne_sol_PI <- cor(Ena_ne,Rad_PI, method = "kendall")
K_tau_s_sol_PI <- cor(Ena_s,Rad_PI, method = "kendall")
K_tau_n_sol_PI <- cor(Ena_n,Rad_PI, method = "kendall")
#
# ENA x Velocidade do vento
K_tau_se_eol_BA <- cor(Ena_se,Vel_BA, method = "kendall")
K_tau_ne_eol_BA <- cor(Ena_ne,Vel_BA, method = "kendall")
K_tau_s_eol_BA <- cor(Ena_s,Vel_BA, method = "kendall")
K_tau_n_eol_BA <- cor(Ena_n,Vel_BA, method = "kendall")
#
K_tau_se_eol_RN <- cor(Ena_se,Vel_RN, method = "kendall")
K_tau_ne_eol_RN <- cor(Ena_ne,Vel_RN, method = "kendall")
K_tau_s_eol_RN <- cor(Ena_s,Vel_RN, method = "kendall")
K_tau_n_eol_RN <- cor(Ena_n,Vel_RN, method = "kendall")
#
K_tau_se_eol_PI <- cor(Ena_se,Vel_PI, method = "kendall")
K_tau_ne_eol_PI <- cor(Ena_ne,Vel_PI, method = "kendall")
K_tau_s_eol_PI <- cor(Ena_s,Vel_PI, method = "kendall")
K_tau_n_eol_PI <- cor(Ena_n,Vel_PI, method = "kendall")
#
# Radiação x Velocidade do vento
k_tau_eol_BA_sol_BA <- cor(Vel_BA,Rad_BA, method = "kendall")
k_tau_eol_PI_sol_PI <- cor(Vel_PI,Rad_PI, method = "kendall")
k_tau_eol_PI_sol_MG <- cor(Vel_PI,Rad_MG, method = "kendall")
k_tau_eol_BA_sol_MG <- cor(Vel_BA,Rad_MG, method = "kendall")
k_tau_eol_BA_sol_PI <- cor(Vel_BA,Rad_PI, method = "kendall")
k_tau_eol_PI_sol_BA <- cor(Vel_PI,Rad_BA, method = "kendall")
k_tau_eol_RN_sol_BA <- cor(Vel_RN,Rad_BA, method = "kendall")
k_tau_eol_RN_sol_MG <- cor(Vel_RN,Rad_MG, method = "kendall")
k_tau_eol_RN_sol_PI <- cor(Vel_RN,Rad_PI, method = "kendall")
#
# Radiação x Radiação (entre Estados)
K_tau_sol_MG_sol_PI <- cor(Rad_MG,Rad_PI, method = "kendall")
K_tau_sol_BA_sol_PI <- cor(Rad_BA,Rad_PI, method = "kendall")
K_tau_sol_MG_sol_BA <- cor(Rad_MG,Rad_BA, method = "kendall")
#
# Tabelas com resultados
#
# ENA x Radiação
col_1 <- c("Ena_SE", "ENA_NE", "ENA_S", "ENA_N")
col_2 <- c(K_tau_se_sol_MG, K_tau_ne_sol_MG, K_tau_s_sol_MG, K_tau_n_sol_MG)
col_3 <- c(K_tau_se_sol_BA, K_tau_ne_sol_BA, K_tau_s_sol_BA, K_tau_n_sol_BA)
col_4 <- c(K_tau_se_sol_PI, K_tau_ne_sol_PI, K_tau_s_sol_PI, K_tau_n_sol_PI)
Ena_Rad <- data_frame(Kendall_Tau = col_1, Radiação_MG = col_2, Radiação_BA = col_3, Radiação_PI = col_4)
#View(Ena_Rad)
#
# ENA x Velocidade do Vento
col_1 <- c("Ena_SE", "ENA_NE", "ENA_S", "ENA_N")
col_2 <- c(K_tau_se_eol_RN, K_tau_ne_eol_RN, K_tau_s_eol_RN, K_tau_n_eol_RN)
col_3 <- c(K_tau_se_eol_BA, K_tau_ne_eol_BA, K_tau_s_eol_BA, K_tau_n_eol_BA)
col_4 <- c(K_tau_se_eol_PI, K_tau_ne_eol_PI, K_tau_s_eol_PI, K_tau_n_eol_PI)
Ena_Vel <- data_frame(Kendall_Tau = col_1, Vel_Vento_RN = col_2, Vel_Vento_BA = col_3, Vel_Vento_PI = col_4)
#View(Ena_Vel)
#
# Radiaçao x Velocidade do Vento
col_1 <- c("Radiação_MG", "Radiação_BA", "Radiação_PI")
col_2 <- c(k_tau_eol_PI_sol_MG, K_tau_ne_eol_RN, k_tau_eol_PI_sol_PI)
col_3 <- c(k_tau_eol_BA_sol_MG, k_tau_eol_BA_sol_BA, k_tau_eol_BA_sol_PI)
col_4 <- c(k_tau_eol_RN_sol_MG, k_tau_eol_RN_sol_BA, k_tau_eol_RN_sol_PI)
Rad_Vel <- data_frame(Kendall_Tau = col_1, Vel_Vento_PI = col_2, Vel_Vento_BA = col_3, Vel_Vento_RN = col_4)
#View(Rad_Vel)
#
# Gráficos de Dispersão
#
#Ena x Radiação

dados <- data.frame(Ena_se,Rad_BA)#1
ggplot(dados,aes(y=Ena_se, x=Radiação_BA))+geom_point(pch=21,col="red",fill="red")

dados <- data.frame(Ena_se,Rad_PI)#2
ggplot(dados,aes(y=Ena_se, x=Radiação_PI))+geom_point(pch=21,col="red",fill="blue")

dados <- data.frame(Ena_se,Rad_MG)#3
ggplot(dados,aes(y=Ena_se, x=Radiação_MG))+geom_point(pch=21,col="red",fill="green")

#Ena x Velocidade do Vento

dados <- data.frame(Ena_se,Vel_BA)#1
ggplot(dados,aes(y=Ena_se, x=Vel_Vento_BA))+geom_point(pch=21,col="red",fill="red")

dados <- data.frame(Ena_se,Vel_PI)#2
ggplot(dados,aes(y=Ena_se, x=Vel_Vento_PI))+geom_point(pch=21,col="red",fill="blue")

dados <- data.frame(Ena_se,Vel_RN)#3
ggplot(dados,aes(y=Ena_se, x=Vel_Vento_RN))+geom_point(pch=21,col="red",fill="green")

#Radiação x Velocidade do Vento

dados <- data.frame(Rad_BA,Vel_BA)#1
ggplot(dados,aes(y=Radiação_BA, x=Vel_Vento_BA))+geom_point(pch=21,col="red",fill="red")

dados <- data.frame(Rad_PI,Vel_PI)#2
ggplot(dados,aes(y=Radiação_PI, x=Vel_Vento_PI))+geom_point(pch=21,col="red",fill="blue")

dados <- data.frame(Rad_MG,Vel_RN)#3
ggplot(dados,aes(y=Radiação_MG, x=Vel_Vento_RN))+geom_point(pch=21,col="red",fill="green")

dados <- data.frame(Rad_BA,Vel_PI)#4
ggplot(dados,aes(y=Radiação_BA, x=Vel_Vento_PI))+geom_point(pch=21,col="red",fill="red")

dados <- data.frame(Rad_BA,Vel_RN)#5
ggplot(dados,aes(y=Radiação_BA, x=Vel_Vento_RN))+geom_point(pch=21,col="red",fill="red")

dados <- data.frame(Rad_MG,Vel_BA)#6
ggplot(dados,aes(y=Radiação_MG, x=Vel_Vento_BA))+geom_point(pch=21,col="red",fill="red")

dados <- data.frame(Rad_MG,Vel_PI)#7
ggplot(dados,aes(y=Radiação_MG, x=Vel_Vento_PI))+geom_point(pch=21,col="red",fill="red")

dados <- data.frame(Rad_PI,Vel_BA)#8
ggplot(dados,aes(y=Radiação_PI, x=Vel_Vento_BA))+geom_point(pch=21,col="red",fill="red")

dados <- data.frame(Rad_PI,Vel_RN)#9
ggplot(dados,aes(y=Radiação_PI, x=Vel_Vento_RN))+geom_point(pch=21,col="red",fill="red")

#
#Radiação x Radiação

dados <- data.frame(Rad_MG,Rad_BA)#1
ggplot(dados,aes(y=Radiação_MG, x=Radiação_BA))+geom_point(pch=21,col="red",fill="red")

dados <- data.frame(Rad_MG,Rad_PI)#2
ggplot(dados,aes(y=Radiação_MG, x=Radiação_PI))+geom_point(pch=21,col="red",fill="blue")

dados <- data.frame(Rad_BA,Rad_PI)#3
ggplot(dados,aes(y=Radiação_BA, x=Radiação_PI))+geom_point(pch=21,col="red",fill="green")

#
#Velocidade do Vento x Velecidade do Vento

dados <- data.frame(Vel_PI,Vel_BA)#1
ggplot(dados,aes(y=Vel_Vento_PI_MG, x=Vel_Vento_BA))+geom_point(pch=21,col="red",fill="red")

dados <- data.frame(Vel_PI,Vel_RN)#2
ggplot(dados,aes(y=Vel_Vento_PI, x=Vel_Vento_RN))+geom_point(pch=21,col="red",fill="blue")

dados <- data.frame(Vel_BA,Vel_RN)#3
ggplot(dados,aes(y=Vel_Vento_BA, x=Vel_Vento_RN))+geom_point(pch=21,col="red",fill="green")
#
#---- Criando funções de distribuição empirica para os dados
#
# Conveter os dados em uma distribuição uniforme

# Ena Sudeste
Ena_se_u <- Ena_se
Ena_se_u <- rank(Ena_se,ties.method = "random")/(length(Ena_se)+1)
Ena_se_u <- data.matrix(Ena_se_u)
#view(Ena_se_u)
#
# Velocidade do Vento RN
Vel_RN_u <- Vel_RN
Vel_RN_u <- rank(Vel_RN,ties.method = "random")/(length(Vel_RN)+1)
Vel_RN_u <- data.matrix(Vel_RN_u)
#view(Vel_RN_u)
#
# Radiação MG
Rad_MG_u <- Rad_MG
Rad_MG_u <- rank(Rad_MG,ties.method = "random")/(length(Rad_MG)+1)
Rad_MG_u <- data.matrix(Rad_MG_u)
#
# Fitting Archimedean Copula
clayton_object <- claytonCopula(dim =3)
gaussian_object <- normalCopula(dim=3)
copula_data <- matrix(,nrow = 60,ncol = 3)
copula_data[,1] <- Ena_se_u
copula_data[,2] <- Vel_RN_u
copula_data[,3] <- Rad_MG_u
#view(copula_data)
fitted_clayton_itau <- fitCopula(clayton_object,copula_data[,1:3],method = "itau")
fitted_clayton_itau
summary(fitted_clayton_itau)
gaussian_object <- normalCopula(dim=3)
fitted_gaussian_itau <- fitCopula(gaussian_object,copula_data[,1:3],method = "itau")
fitted_gaussian_itau
summary(fitted_gaussian_itau)
