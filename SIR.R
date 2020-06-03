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
library(deSolve)
library(reshape2)

# Importação das Bases --------------------------------------------------------------------

covid <- read_csv("dados/covid_preditos.csv")
# A mediana do início dos sintomas até a notificação é de 4 dias e para exame de 3 dias. Por isso, o início do sintoma será truncado em Sys.Date - 7
covid <- subset(covid, covid$INICIO_SINTOMAS <= Sys.Date() - 7)
covid_nowcasted <- subset(covid, covid$DADOS == "Totais")
covid_nowcasted <- covid_nowcasted %>% dplyr::select(INICIO_SINTOMAS, CASOS)
covid_medidos <- subset(covid, covid$DADOS == "Atuais")
covid_medidos <- covid_medidos %>% dplyr::select(INICIO_SINTOMAS, CASOS)


# Estimação dos parâmetros para SIR ------------------------------------------------

#Calculou-se o número de pessoas diagnosticadas esperado se todos tivessem feito o exame(nowcasting) , por dia
#I = Calculou-se o número de pessoas com SAR_CoV_2 esperado na população geral, a partir do número de diagnosticadosm por dia (Ver referência) 
#R = Calculou-se o número de pessoas com mais de (14 ou 21 dias) após a infecção + número de mortes
#S = Calcuou-se o número de sucetíveis subtraindo-se I e R da população total de Florianóplis (500.000), para cada dia


## medidos
covid_medidos$INFECTADOS <- covid_medidos$CASOS * 5 #80% da população sem diagnóstico
covid_medidos$CASOS <- NULL
#Projetando Infectados
PROJ_INFECTADOS <- ts(covid_medidos$INFECTADOS)
fit_atual <- auto.arima(PROJ_INFECTADOS)
PROJ_INFECTADOS <- forecast(fit_atual,h=14)$mean[1:14] #7 dias que foram truncados + 7 dias de predição
plot(forecast(fit_atual,h=14)) #7 dias que foram truncados + 7 dias de predição
INICIO_SINTOMAS <- c((max(covid_medidos$INICIO_SINTOMAS)+1):(max(covid_medidos$INICIO_SINTOMAS)+14))
INICIO_SINTOMAS <- as.Date(INICIO_SINTOMAS, origin = "1970-01-01")
proj_medidos <- data.frame("INICIO_SINTOMAS" = INICIO_SINTOMAS,"INFECTADOS" = PROJ_INFECTADOS)
covid_medidos <- rbind(covid_medidos, proj_medidos) %>% as.data.frame()
covid_medidos$DT_REMOV <- as.Date(covid_medidos$INICIO_SINTOMAS, format = "%Y-%m-%d")+13
REMOVIDOS <- covid_medidos[,names(covid_medidos) %in% c("INFECTADOS", "DT_REMOV")] #Precisa incluir os óbitos
names(REMOVIDOS) <- c("REMOVIDOS", "INICIO_SINTOMAS") 
covid_medidos <- merge(covid_medidos, REMOVIDOS, by = "INICIO_SINTOMAS", all.x = T)
covid_medidos$REMOVIDOS <- ifelse(is.na(covid_medidos$REMOVIDOS),0,covid_medidos$REMOVIDOS)
covid_medidos$NAO_SUSCEPTIVEL <- covid_medidos$INFECTADOS + covid_medidos$REMOVIDOS
covid_medidos$CUM_NAO_SUSCEPIVEL <- cumsum(covid_medidos$NAO_SUSCEPTIVEL)
covid_medidos$POPULACAO <- 500973
covid_medidos$SUSCEPTIVEIS <- covid_medidos$POPULACAO - covid_medidos$CUM_NAO_SUSCEPIVEL
covid_medidos$INICIO_SINTOMAS <- as.Date(covid_medidos$INICIO_SINTOMAS, format = "%Y-%m-%d")
# covid_medidos <- subset(covid_medidos, covid_medidos$INICIO_SINTOMAS > as.Date("2020-02-01", format = "%Y-%m-%d") &
# 		    	covid_medidos$INICIO_SINTOMAS < Sys.Date())
covid_medidos_sir <- covid_medidos %>% dplyr::select(INICIO_SINTOMAS, INFECTADOS, REMOVIDOS, SUSCEPTIVEIS)


## nowcasted
covid_nowcasted$INFECTADOS <- covid_nowcasted$CASOS * 5 #80% da população sem diagnóstico
covid_nowcasted$CASOS <- NULL
#Projetando Infectados
PROJ_INFECTADOS <- ts(covid_nowcasted$INFECTADOS)
fit_atual <- auto.arima(PROJ_INFECTADOS)
PROJ_INFECTADOS <- forecast(fit_atual,h=14)$mean[1:14]
plot(forecast(fit_atual,h=14)) #7 dias que foram truncados + 7 dias de predição
INICIO_SINTOMAS <- c((max(covid_nowcasted$INICIO_SINTOMAS)+1):(max(covid_nowcasted$INICIO_SINTOMAS)+14))
INICIO_SINTOMAS <- as.Date(INICIO_SINTOMAS, origin = "1970-01-01")
proj_nowcasted <- data.frame("INICIO_SINTOMAS" = INICIO_SINTOMAS,"INFECTADOS" = PROJ_INFECTADOS)
covid_nowcasted <- rbind(covid_nowcasted, proj_nowcasted) %>% as.data.frame()
covid_nowcasted$DT_REMOV <- as.Date(covid_nowcasted$INICIO_SINTOMAS, format = "%Y-%m-%d")+13
REMOVIDOS <- covid_nowcasted[,names(covid_nowcasted) %in% c("INFECTADOS", "DT_REMOV")] #Precisa incluir os óbitos
names(REMOVIDOS) <- c("REMOVIDOS", "INICIO_SINTOMAS") 
covid_nowcasted <- merge(covid_nowcasted, REMOVIDOS, by = "INICIO_SINTOMAS", all.x = T)
covid_nowcasted$REMOVIDOS <- ifelse(is.na(covid_nowcasted$REMOVIDOS),0,covid_nowcasted$REMOVIDOS)
covid_nowcasted$NAO_SUSCEPTIVEL <- covid_nowcasted$INFECTADOS + covid_nowcasted$REMOVIDOS
covid_nowcasted$CUM_NAO_SUSCEPIVEL <- cumsum(covid_nowcasted$NAO_SUSCEPTIVEL)
covid_nowcasted$POPULACAO <- 500973
covid_nowcasted$SUSCEPTIVEIS <- covid_nowcasted$POPULACAO - covid_nowcasted$CUM_NAO_SUSCEPIVEL
covid_nowcasted$INICIO_SINTOMAS <- as.Date(covid_nowcasted$INICIO_SINTOMAS, format = "%Y-%m-%d")
# covid_nowcasted <- subset(covid_nowcasted, covid_nowcasted$INICIO_SINTOMAS > as.Date("2020-02-01", format = "%Y-%m-%d") &
# 		    	covid_nowcasted$INICIO_SINTOMAS < Sys.Date())
covid_nowcasted_sir <- covid_nowcasted %>% dplyr::select(INICIO_SINTOMAS, INFECTADOS, REMOVIDOS, SUSCEPTIVEIS)
	


# Plot do nowcasting ------------------------------------------------------
medidos_sir_melt <- melt(covid_medidos_sir, id.vars = "INICIO_SINTOMAS")
medidos_sir_melt <- subset(medidos_sir_melt, medidos_sir_melt$variable != "SUSCEPTIVEIS")
ggplot(medidos_sir_melt, aes(INICIO_SINTOMAS, value, group = variable, color = variable))+
	geom_line()+
	theme_bw()+
	labs(title = "Dados medidos",
	     x = "Data de Início dos Sintomas", 
	     y = "Número de pessoas")


nowcasted_sir_melt <- melt(covid_nowcasted_sir, id.vars = "INICIO_SINTOMAS")
nowcasted_sir_melt <- subset(nowcasted_sir_melt, nowcasted_sir_melt$variable != "SUSCEPTIVEIS")
ggplot(nowcasted_sir_melt, aes(INICIO_SINTOMAS, value, group = variable, color = variable))+
	geom_line()+
	theme_bw()+
	labs(title = "Dados nowcasted",
	     x = "Data de Início dos Sintomas", 
	     y = "Número de pessoas")
 


# Análise do R ------------------------------------------------------------
# Criano generation time : distribuição gamma, com média 5.84 unidade de tempo e desvio padrão de 1 unidade de tempo  
GT.covid <- generation.time("gamma", c(4.89 ,1.48)) 

#############################################################################
#Medido
#############################################################################
# loads and transform dataset 
r0_base_medidos <- covid_medidos$INFECTADOS
names(r0_base_medidos) <- covid_medidos$INICIO_SINTOMAS # a base precisa ser um vetor nomeado
# applies methods EG, ML, SB, TD to the dataset 
res.R_medidos <- estimate.R(r0_base_medidos, GT=GT.covid, methods=c("TD"), 
			     begin = min(covid_medidos$INICIO_SINTOMAS), end = (max(covid_medidos$INICIO_SINTOMAS)-1)) # outros métodos "EG","ML","SB"
r_medidos <- res.R_medidos$estimates$TD$R %>% as.data.frame()
ic_medidos <- res.R_medidos$estimates$TD$conf.int %>% as.data.frame()
r_ic_medidos <- cbind(r_medidos, ic_medidos) %>% as.data.frame()
names(r_ic_medidos) <- c("R", "IC 05", "IC 95")
r_ic_medidos$DATA <- row.names(r_ic_medidos)
ggplot(r_ic_medidos, aes(x = DATA))+
	geom_line(aes(y = R, group = 1), fill = "red")+
	geom_hline(yintercept = 1, size = 2)+
	theme_bw()+
	labs(title = "Sem Nowcasting",
	     x = "Data", 
	     y = "Rt")+
  	theme(axis.text.x = element_text(angle=90, hjust = 1))


# diplays results
plot(res.R_medidos) 
# displays fit to the epidemic curve 
plotfit(res.R_medidos) 


#############################################################################
#Nowcastad
#############################################################################
# loads and transform dataset 
r0_base_nowcasted <- covid_nowcasted$INFECTADOS
names(r0_base_nowcasted) <- covid_nowcasted$INICIO_SINTOMAS # a base precisa ser um vetor nomeado
# applies methods EG, ML, SB, TD to the dataset 
res.R_nowcasted <- estimate.R(r0_base_nowcasted, GT=GT.covid, methods=c("TD"), 
			     begin = min(covid_nowcasted$INICIO_SINTOMAS), end = (max(covid_nowcasted$INICIO_SINTOMAS)-1)) # outros métodos "EG","ML","SB"
r_nowcasted <- res.R_nowcasted$estimates$TD$R %>% as.data.frame()
ic_nowcasted <- res.R_nowcasted$estimates$TD$conf.int %>% as.data.frame()
r_ic_nowcasted <- cbind(r_nowcasted, ic_nowcasted) %>% as.data.frame()
names(r_ic_nowcasted) <- c("R", "IC 05", "IC 95")
r_ic_nowcasted$DATA <- row.names(r_ic_nowcasted)
ggplot(r_ic_nowcasted, aes(x = DATA))+
	geom_line(aes(y = R, group = 1), fill = "red")+
	geom_hline(yintercept = 1, size = 2)+
	theme_bw()+
	labs(title = "Com Nowcasting",
	     x = "Data", 
	     y = "Rt")+
  	theme(axis.text.x = element_text(angle=90, hjust = 1))



# diplays results
plot(res.R_nowcasted)
# displays fit to the epidemic curve 
plotfit(res.R_nowcasted) 



# sensitivity analysis according to choice of time window for exponential growth 
#sensitivity.analysis(r0_base_nowcasted, GT.covid, begin = as.Date("2020-02-24"), end = as.Date("2020-05-01"), est.method="ML", sa.type="time")  
# sensitivity analysis according to generation time > 
#sensitivity.analysis(r0_base, GT.type="gamma", GT.mean=seq(1,33,1), GT.sd.range=1, begin=1, end=33, est.method="EG", sa.type="GT")




# Inputs do modelo:
## Valores inicial no tempo 0
valores_estado_incial_medidos <- c(S = tail(covid_medidos_sir$SUSCEPTIVEIS,1),   
                          	  I = tail(covid_medidos_sir$INFECTADOS,1),           
                          	  R = tail(covid_medidos_sir$REMOVIDOS,1))  

valores_estado_incial_nowcasted <- c(S = tail(covid_nowcasted_sir$SUSCEPTIVEIS,1),   
                          	    I = tail(covid_nowcasted_sir$INFECTADOS,1),           
                          	    R = tail(covid_nowcasted_sir$REMOVIDOS,1))           

## Vetor com os parâmentros descrevendo as taxas de transição em unidades de dia^-1
parametros <- c(beta = 1.5/14,      # Taxa de infecção nos susceptíveis
                gamma = 1/14)     # Taxa de recuperação nos infectados. Será utilizado 14 dias

# Tempo:
## Vetor com a sequencia temporal para o modelo
tempo <- seq(from = 0, to = 2000, by = 1)   # de 0 a 100 dias em interválos diários

# Função do modelo SIR: 
## A função do modelo utiliza os inputs na seguinte ordem: tempo, estado, parametros
sir_model <- function(tempo, estado, parametros) {  

    with(as.list(c(estado, parametros)), {  # tell R to unpack variable names from the state and parameters inputs    
        
    # Calculando o tamanho total da população N (soma do número de pessoas em cada compartimento)
      N <- S+I+R
      
    # Definindo lambda como função de beta (taxa de infectividade) e I:
      lambda <- beta * I/N 
        
    # A equação diferencial
      dS <- -lambda * S               # as pessoas saem (-) do compartimento S a uma taxa lambda (força da infecção)
      dI <- lambda * S - gamma * I    # as pessoas entram (+) no compartimento I vindas de S a uma taxa lambda, 
                                      # e saem (-) do compartimento I para o compartimento R a uma taxa gamma (taxa de recuperação)
      dR <- gamma * I                 # as pessoas entram (+) entram no compartimento R a partir do comparimento I à uma tax gamma
      
    # Retorna o n~umero de pessoas em S, I e R em cada período de tempo 
    return(list(c(dS, dI, dR))) 
    })
  
}

# Resultado do modelo (resolvendo as equações diferenciais):
output_medidos <- as.data.frame(ode(y = valores_estado_incial_medidos, 
                            	   times = tempo, 
                            	   func = sir_model,
                            	   parms = parametros))
output_medidos[which(output_medidos$I == max(output_medidos$I)),]

taxa_uti <- 4/tail(covid_medidos_sir$INFECTADOS,1)
leitos_vagos_uti <- 79
leitos_vagos_uti_20_perc <- leitos_vagos_uti*0.2
casos_para_lotar_uti <- leitos_vagos_uti_20_perc/taxa_uti
output_medidos[min(which(output_medidos$I > casos_para_lotar_uti)),]




output_nowcasted <- as.data.frame(ode(y = valores_estado_incial_nowcasted, 
                            	   times = tempo, 
                            	   func = sir_model,
                            	   parms = parametros))
output_nowcasted[which(output_nowcasted$I == max(output_nowcasted$I)),]

taxa_uti <- 4/tail(covid_nowcasted_sir$INFECTADOS,1)
leitos_vagos_uti <- 79
casos_para_lotar_uti <- leitos_vagos_uti/taxa_uti
output_nowcasted[min(which(output_nowcasted$I > casos_para_lotar_uti)),]


library(data.table)     
dt <- data.table(output_nowcasted, val = "I") 
setkey(dt, "I")
sel <- dt[casos_para_lotar_uti, roll = "nearest"]

output_nowcasted[which(round(output_nowcasted$I,0) == sel),]




comparacao <- merge(output_medidos, output_nowcasted, by = "time", all = T)
names(comparacao) <- c("time", "S_medido", "I_medido", "R_medido", "S_nowcasted", "I_nowcasted", "R_nowcasted")
comparacao_long <- melt(as.data.frame(comparacao), id = "time")
ggplot(data = comparacao_long,                                               
       aes(x = time, y = value, colour = variable, group = variable)) +  
  geom_line() +                                                          
  xlab("Time (days)")+                                                   
  ylab("Proportion of the population") +                                 
  labs(colour = "Compartment",                                            
       title = "Proportion susceptible, infected and recovered over time") +                                                               
  theme(legend.position = "bottom")                                      

comparacao_long$proportion <- comparacao_long$value/sum(valores_estado_incial_medidos)
ggplot(data = comparacao_long,                                               
       aes(x = time, y = proportion, colour = variable, group = variable)) +  
  geom_line() +                                                          
  xlab("Time (days)")+                                                   
  ylab("Proportion of the population") +                                 
  labs(colour = "Compartment",                                            
       title = "Proportion susceptible, infected and recovered over time") +                                                               
  theme(legend.position = "bottom")                                      



#Transformando em data frame para plotagem
output_medidos_long <- melt(as.data.frame(output_medidos), id = "time")                  

output_nowcasted_long <- melt(as.data.frame(output_nowcasted), id = "time")                  

# Gráfico da evolução do número de pessoas em S, I and R 
ggplot(data = output_medidos_long,                                               
       aes(x = time, y = value, colour = variable, group = variable)) +  
  geom_line() +                                                          
  xlab("Time (days)")+                                                   
  ylab("Proportion of the population") +                                 
  labs(colour = "Compartment",                                            
       title = "Proportion susceptible, infected and recovered over time") +                                                               
  theme(legend.position = "bottom")                                      

ggplot(data = output_nowcasted_long,                                               
       aes(x = time, y = value, colour = variable, group = variable)) +  
  geom_line() +                                                          
  xlab("Time (days)")+                                                   
  ylab("Proportion of the population") +                                 
  labs(colour = "Compartment",                                            
       title = "Proportion susceptible, infected and recovered over time") +                                                               
  theme(legend.position = "bottom")                                      


# Calculating the proportion in each compartment as a column in the long-format output
output_medidos_long$proportion <- output_medidos_long$value/sum(valores_estado_incial_medidos)

output_nowcasted_long$proportion <- output_nowcasted_long$value/sum(valores_estado_incial_nowcasted)

# Plot the proportion of people in the S, I and R compartments over time
ggplot(data = output_medidos_long,                                              
       aes(x = time, y = proportion, colour = variable, group = variable)) +  
  geom_line() +                                                          
  xlab("Time (days)")+                                                   
  ylab("Proportion of the population") +                                 
  labs(colour = "Compartment",                                           
       title = "Proportion susceptible, infected and recovered over time") +                                                               
  theme(legend.position = "bottom")     


ggplot(data = output_nowcasted_long,                                              
       aes(x = time, y = proportion, colour = variable, group = variable)) +  
  geom_line() +                                                          
  xlab("Time (days)")+                                                   
  ylab("Proportion of the population") +                                 
  labs(colour = "Compartment",                                           
       title = "Proportion susceptible, infected and recovered over time") +                                                               
  theme(legend.position = "bottom")                                      


# Calculating the effective reproduction number in a new column
output_medidos$reff <- parametros["beta"]/parametros["gamma"] *                         # R0 = beta/gamma
                output_medidos$S/(output_medidos$S+output_medidos$I+output_medidos$R)   # multiplicando R0 pela proporção de susceptíveis
											# em cada período de tempo para cada linha
library(data.table)     
dt <- data.table(output_medidos, val = "reff") 
setkey(dt, "reff")
x <- 1
dt[J(x), roll = "nearest", which = TRUE]

output_nowcasted$reff <- parametros["beta"]/parametros["gamma"] *                         	# R0 = beta/gamma
                output_nowcasted$S/(output_nowcasted$S+output_nowcasted$I+output_nowcasted$R)   # multiplicando R0 pela proporção de susceptíveis
                                                                         			# em cada período de tempo para cada linha
dt <- data.table(output_nowcasted, val = "reff") 
setkey(dt, "reff")
x <- 1
dt[J(x), roll = "nearest", which = TRUE]



# Plot Reff
ggplot(data = output_medidos,                                                    
       aes(x = time, y = reff)) +                                        
  geom_line() +                                                          
  xlab("Time (days)")+                                                   
  ylab("Reff") +                                                         
  labs(title = "Effective reproduction number over time")                

ggplot(data = output_nowcasted,                                                    
       aes(x = time, y = reff)) +                                        
  geom_line() +                                                          
  xlab("Time (days)")+                                                   
  ylab("Reff") +                                                         
  labs(title = "Effective reproduction number over time")     




library(EpiDynamics)
parameters <- c(mu = 2.546 / (450000 * 365), beta = 2/14,
sigma = 1 / 6.4, gamma = 1 / 7)
initials <- c(S = 0.1, E = 1e-04, I = 1e-04, R = 1 - 0.1 - 1e-4 - 1e-4)
# Solve and plot.
seir <- SEIR(pars = parameters, init = initials, time = 0:(60 * 365))
PlotMods(seir)
