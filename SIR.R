# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)
# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(EpiDynamics)
library(shinySIR)
library(R0)
library(forecast)
# Importação das Bases --------------------------------------------------------------------

covid <- read_csv("dados/covid_preditos.csv")
# A mediana do início dos sintomas até a notificação é de 4 dias. Por isso, o início do sintoma será truncado em Sys.Date - 4
covid <- subset(covid, covid$INICIO_SINTOMAS <= Sys.Date() - 4)
covid_preditos <- subset(covid, covid$DADOS == "Totais")
covid_preditos <- covid_preditos %>% dplyr::select(INICIO_SINTOMAS, CASOS)
covid_atuais <- subset(covid, covid$DADOS == "Atuais")
covid_atuais <- covid_atuais %>% dplyr::select(INICIO_SINTOMAS, CASOS)


# Estimação dos parâmetros para SIR ------------------------------------------------

#Calculou-se o número de pessoas diagnosticadas esperado se todos tivessem feito o exame(nowcasting) , por dia
#I = Calculou-se o número de pessoas com SAR_CoV_2 esperado na população geral, a partir do número de diagnosticadosm por dia (Ver referência) 
#R = Calculou-se o número de pessoas com mais de (14 ou 21 dias) após a infecção + número de mortes
#S = Calcuou-se o número de sucetíveis subtraindo-se I e R da população total de Florianóplis (500.000), para cada dia


## Atuais
covid_atuais$INFECTADOS <- covid_atuais$CASOS * 5 #80% da população sem diagnóstico
covid_atuais$CASOS <- NULL
#Projetando Infectados
PROJ_INFECTADOS <- ts(covid_atuais$INFECTADOS)
fit_atual <- auto.arima(PROJ_INFECTADOS)
PROJ_INFECTADOS <- forecast(fit_atual,h=11)$mean[1:11]
plot(forecast(fit_atual,h=11)) #4 dias que foram truncados + 7 dias de predição
INICIO_SINTOMAS <- c((max(covid_atuais$INICIO_SINTOMAS)+1):(max(covid_atuais$INICIO_SINTOMAS)+11))
INICIO_SINTOMAS <- as.Date(INICIO_SINTOMAS, origin = "1970-01-01")
proj_atuais <- data.frame("INICIO_SINTOMAS" = INICIO_SINTOMAS,"INFECTADOS" = PROJ_INFECTADOS)
covid_atuais <- rbind(covid_atuais, proj_atuais) %>% as.data.frame()
covid_atuais$DT_REMOV <- as.Date(covid_atuais$INICIO_SINTOMAS, format = "%Y-%m-%d")+14
REMOVIDOS <- covid_atuais[,names(covid_atuais) %in% c("INFECTADOS", "DT_REMOV")] #Precisa incluir os óbitos
names(REMOVIDOS) <- c("REMOVIDOS", "INICIO_SINTOMAS") 
covid_atuais <- merge(covid_atuais, REMOVIDOS, by = "INICIO_SINTOMAS", all.x = T)
covid_atuais$REMOVIDOS <- ifelse(is.na(covid_atuais$REMOVIDOS),0,covid_atuais$REMOVIDOS)
covid_atuais$NAO_SUSCEPTIVEL <- covid_atuais$INFECTADOS + covid_atuais$REMOVIDOS
covid_atuais$CUM_NAO_SUSCEPIVEL <- cumsum(covid_atuais$NAO_SUSCEPTIVEL)
covid_atuais$POPULACAO <- 500973
covid_atuais$SUSCEPTIVEIS <- covid_atuais$POPULACAO - covid_atuais$CUM_NAO_SUSCEPIVEL
covid_atuais$INICIO_SINTOMAS <- as.Date(covid_atuais$INICIO_SINTOMAS, format = "%Y-%m-%d")
# covid_atuais <- subset(covid_atuais, covid_atuais$INICIO_SINTOMAS > as.Date("2020-02-01", format = "%Y-%m-%d") &
# 		    	covid_atuais$INICIO_SINTOMAS < Sys.Date())
covid_atuais_sir <- covid_atuais %>% dplyr::select(INICIO_SINTOMAS, INFECTADOS, REMOVIDOS, SUSCEPTIVEIS)


## Preditos
covid_preditos$INFECTADOS <- covid_preditos$CASOS * 5 #80% da população sem diagnóstico
covid_preditos$CASOS <- NULL
#Projetando Infectados
PROJ_INFECTADOS <- ts(covid_preditos$INFECTADOS)
fit_atual <- auto.arima(PROJ_INFECTADOS)
PROJ_INFECTADOS <- forecast(fit_atual,h=11)$mean[1:11]
plot(forecast(fit_atual,h=11)) #4 dias que foram truncados + 7 dias de predição
INICIO_SINTOMAS <- c((max(covid_preditos$INICIO_SINTOMAS)+1):(max(covid_preditos$INICIO_SINTOMAS)+11))
INICIO_SINTOMAS <- as.Date(INICIO_SINTOMAS, origin = "1970-01-01")
proj_preditos <- data.frame("INICIO_SINTOMAS" = INICIO_SINTOMAS,"INFECTADOS" = PROJ_INFECTADOS)
covid_preditos <- rbind(covid_preditos, proj_preditos) %>% as.data.frame()
covid_preditos$DT_REMOV <- as.Date(covid_preditos$INICIO_SINTOMAS, format = "%Y-%m-%d")+14
REMOVIDOS <- covid_preditos[,names(covid_preditos) %in% c("INFECTADOS", "DT_REMOV")] #Precisa incluir os óbitos
names(REMOVIDOS) <- c("REMOVIDOS", "INICIO_SINTOMAS") 
covid_preditos <- merge(covid_preditos, REMOVIDOS, by = "INICIO_SINTOMAS", all.x = T)
covid_preditos$REMOVIDOS <- ifelse(is.na(covid_preditos$REMOVIDOS),0,covid_preditos$REMOVIDOS)
covid_preditos$NAO_SUSCEPTIVEL <- covid_preditos$INFECTADOS + covid_preditos$REMOVIDOS
covid_preditos$CUM_NAO_SUSCEPIVEL <- cumsum(covid_preditos$NAO_SUSCEPTIVEL)
covid_preditos$POPULACAO <- 500973
covid_preditos$SUSCEPTIVEIS <- covid_preditos$POPULACAO - covid_preditos$CUM_NAO_SUSCEPIVEL
covid_preditos$INICIO_SINTOMAS <- as.Date(covid_preditos$INICIO_SINTOMAS, format = "%Y-%m-%d")
# covid_preditos <- subset(covid_preditos, covid_preditos$INICIO_SINTOMAS > as.Date("2020-02-01", format = "%Y-%m-%d") &
# 		    	covid_preditos$INICIO_SINTOMAS < Sys.Date())
covid_preditos_sir <- covid_preditos %>% dplyr::select(INICIO_SINTOMAS, INFECTADOS, REMOVIDOS, SUSCEPTIVEIS)
	


# Plot do nowcasting ------------------------------------------------------
atuais_sir_melt <- melt(covid_atuais_sir, id.vars = "INICIO_SINTOMAS")
atuais_sir_melt <- subset(atuais_sir_melt, atuais_sir_melt$variable != "SUSCEPTIVEIS")
ggplot(atuais_sir_melt, aes(INICIO_SINTOMAS, value, group = variable, color = variable))+
	geom_line()+
	theme_bw()+
	labs(title = "Dados atuais",
	     x = "Data de Início dos Sintomas", 
	     y = "Número de pessoas")


preditos_sir_melt <- melt(covid_preditos_sir, id.vars = "INICIO_SINTOMAS")
preditos_sir_melt <- subset(preditos_sir_melt, preditos_sir_melt$variable != "SUSCEPTIVEIS")
ggplot(preditos_sir_melt, aes(INICIO_SINTOMAS, value, group = variable, color = variable))+
	geom_line()+
	theme_bw()+
	labs(title = "Dados preditos",
	     x = "Data de Início dos Sintomas", 
	     y = "Número de pessoas")
 


# Análise do R ------------------------------------------------------------
# Criano generation time : distribuição gamma, com média 5.84 unidade de tempo e desvio padrão de 1 unidade de tempo  
GT.covid <- generation.time("gamma", c(4.89 ,1.48)) 

#############################################################################
#Atual
#############################################################################
# loads and transform dataset 
r0_base_atuais <- covid_atuais$INFECTADOS
names(r0_base_atuais) <- covid_atuais$INICIO_SINTOMAS # a base precisa ser um vetor nomeado
# applies methods EG, ML, SB, TD to the dataset 
res.R_atuais <- estimate.R(r0_base_atuais, GT=GT.covid, methods=c("TD"), 
			     begin = min(covid_atuais$INICIO_SINTOMAS), end = (max(covid_atuais$INICIO_SINTOMAS)-1)) # outros métodos "EG","ML","SB"
r_atuais <- res.R_atuais$estimates$TD$R %>% as.data.frame()
ic_atuais <- res.R_atuais$estimates$TD$conf.int %>% as.data.frame()
r_ic_atuais <- cbind(r_atuais, ic_atuais) %>% as.data.frame()
names(r_ic_atuais) <- c("R", "IC 05", "IC 95")
r_ic_atuais$DATA <- row.names(r_ic_atuais)
ggplot(r_ic_atuais, aes(x = DATA))+
	geom_line(aes(y = R, group = 1), fill = "red")+
	geom_hline(yintercept = 1, size = 2)+
	theme_bw()+
	labs(title = "Sem Nowcasting",
	     x = "Data", 
	     y = "Rt")+
  	theme(axis.text.x = element_text(angle=90, hjust = 1))


# diplays results
plot(res.R_atuais) 
# displays fit to the epidemic curve 
plotfit(res.R_atuais) 


#############################################################################
#Predito
#############################################################################
# loads and transform dataset 
r0_base_preditos <- covid_preditos$INFECTADOS
names(r0_base_preditos) <- covid_preditos$INICIO_SINTOMAS # a base precisa ser um vetor nomeado
# applies methods EG, ML, SB, TD to the dataset 
res.R_preditos <- estimate.R(r0_base_preditos, GT=GT.covid, methods=c("TD"), 
			     begin = min(covid_preditos$INICIO_SINTOMAS), end = (max(covid_preditos$INICIO_SINTOMAS)-1)) # outros métodos "EG","ML","SB"
r_preditos <- res.R_preditos$estimates$TD$R %>% as.data.frame()
ic_preditos <- res.R_preditos$estimates$TD$conf.int %>% as.data.frame()
r_ic_preditos <- cbind(r_preditos, ic_preditos) %>% as.data.frame()
names(r_ic_preditos) <- c("R", "IC 05", "IC 95")
r_ic_preditos$DATA <- row.names(r_ic_preditos)
ggplot(r_ic_preditos, aes(x = DATA))+
	geom_line(aes(y = R, group = 1), fill = "red")+
	geom_hline(yintercept = 1, size = 2)+
	theme_bw()+
	labs(title = "Com Nowcasting",
	     x = "Data", 
	     y = "Rt")+
  	theme(axis.text.x = element_text(angle=90, hjust = 1))



# diplays results
plot(res.R_preditos)
# displays fit to the epidemic curve 
plotfit(res.R_preditos) 



# sensitivity analysis according to choice of time window for exponential growth 
sensitivity.analysis(r0_base_preditos, GT.covid, begin = as.Date("2020-02-24"), end = as.Date("2020-05-01"), est.method="ML", sa.type="time")  
# sensitivity analysis according to generation time > 
sensitivity.analysis(r0_base, GT.type="gamma", GT.mean=seq(1,33,1), GT.sd.range=1, begin=1, end=33, est.method="EG", sa.type="GT")





