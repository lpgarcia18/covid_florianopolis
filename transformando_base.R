# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)

# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(reshape2)

# Importanto bases --------------------------------------------------------
covid <- read_csv("dados/covid_anonimizado.csv", 
    locale = locale(encoding = "WINDOWS-1252"))

# Transformando base ------------------------------------------------------

# Excluindo entradas que não possuem data de início dos sintomas, pois essa informação é fundamental para se analisar a incidência
covid <- subset(covid, !is.na(covid$`Data do início dos sintomas`))

# Convertendo, ajustando e criando variáveis
covid <- sapply(covid, as.character) %>% as.data.frame()

##Convertendo idade em categoria
covid$IDADE <- as.numeric(as.character(covid$IDADE)) 
covid[which(covid$IDADE < 0.001), colnames(covid) == "IDADE"] <- NA #Excluindo dados de idade incorretos
covid$IDADE <- ifelse(covid$IDADE < 10, "10 menos", 
			    ifelse(covid$IDADE < 20, "10 a 20",
			           ifelse(covid$IDADE < 40, "20 a 40",
			                  ifelse(covid$IDADE < 60, "40 a 60",
			                         ifelse(covid$IDADE < 80, "60 a 80", "80 mais")
			                         )
			                  )
			           )
			)

## Criando a variável Triagem. O primeiro modelo de triagem para classificação de casos suspeitos para COVID-19 
## em Florianópolis foi utilizado até 2020-03-24, 
## o segundo modelo até 2020-04-06 e o terceiro a partir de então.
covid$`Data do início dos sintomas` <- as.Date(covid$`Data do início dos sintomas`, format = "%Y-%m-%d")
covid$TRIAGEM <- ifelse(covid$`Data do início dos sintomas` < as.Date("2020-03-24", format = "%Y-%m-%d"), "modelo 1",
			ifelse(covid$`Data do início dos sintomas` < as.Date("2020-04-06", format = "%Y-%m-%d"), "modelo 2", "modelo 3"))

## Transformando outros municípios na categoria "outros", pois o número é pequeno
covid$`Municipio de residencia` <- as.character(covid$`Municipio de residencia`)
covid$`Municipio de residencia` <- ifelse(covid$`Municipio de residencia` == "florianopolis", "florianopolis", "outro")

## Ajustando a variável Bairro para corrigir erros causasdos por caracteres especiais ou falha na digitação.
## Os bairros foram aproximados para coincidirem com as áreas dos Centros de Saúde
## Bairros de outros municípios foram categorizados como outros
covid$Bairro <- as.character(covid$Bairro)
covid[which(covid$Bairro == "agrona´mica"),names(covid) == "Bairro"]<- "agronomica"
covid[which(covid$Bairro == "armaa§a£o"),names(covid) == "Bairro"]<- "armacao"
covid[which(covid$Bairro == "armaa§ao"),names(covid) == "Bairro"]<- "armacao"
covid[which(covid$Bairro == "balnea¡rio"),names(covid) == "Bairro"]<- "balneario"
covid[which(covid$Bairro == "biguacu"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "ca³rrego grande"),names(covid) == "Bairro"]<- "corrego grande"
covid[which(covid$Bairro == "caieira da barra sul"),names(covid) == "Bairro"]<- "caieira da barra do sul"
covid[which(covid$Bairro == "canto (coninente)"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "canto (continente)"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "canto(continente)"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "canto"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "costeira"),names(covid) == "Bairro"]<- "costeira do pirajubae"
covid[which(covid$Bairro == "costeira de pirajubae"),names(covid) == "Bairro"]<- "costeira do pirajubae"
covid[which(covid$Bairro == "costeira do pirajube"),names(covid) == "Bairro"]<- "costeira do pirajubae"
covid[which(covid$Bairro == "cs rio vermelho"),names(covid) == "Bairro"]<- "rio vermelho"
covid[which(covid$Bairro == "cs trindade"),names(covid) == "Bairro"]<- "trindade"
covid[which(covid$Bairro == "estacio"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "forquilinhas"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "fzenda do rio tavares"),names(covid) == "Bairro"]<- "fazenda do rio tavares"
covid[which(covid$Bairro == "igleses"),names(covid) == "Bairro"]<- "ingleses"
covid[which(covid$Bairro == "ingleses do rio vermelho"),names(covid) == "Bairro"]<- "ingleses"
covid[which(covid$Bairro == "inglses"),names(covid) == "Bairro"]<- "ingleses"
covid[which(covid$Bairro == "itaguacu"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "jardim cidade de florianopolis"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "jardim janaina"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "jose mendes"),names(covid) == "Bairro"]<- "prainha"
covid[which(covid$Bairro == "jurere internacional"),names(covid) == "Bairro"]<- "jurere"
covid[which(covid$Bairro == "jurere tradicional"),names(covid) == "Bairro"]<- "jurere"
covid[which(covid$Bairro == "lagoa"),names(covid) == "Bairro"]<- "lagoa da conceicao"
covid[which(covid$Bairro == "lagoa da conceia§a£o"),names(covid) == "Bairro"]<- "lagoa da conceicao"
covid[which(covid$Bairro == "joao mendes"),names(covid) == "Bairro"]<- "prainha"
covid[which(covid$Bairro == "monte verde / barra do sambaqui"),names(covid) == "Bairro"]<- 
covid[which(covid$Bairro == "monte de cristo"),names(covid) == "Bairro"]<- "monte cristo"
covid[which(covid$Bairro == "monte verde"),names(covid) == "Bairro"]<- "saco grande"
covid[which(covid$Bairro == "pantano sul"),names(covid) == "Bairro"]<- "pantano do sul" 
covid[which(covid$Bairro == "real parque"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "ribeira£o da ilha"),names(covid) == "Bairro"]<- "ribeirao da ilha"
covid[which(covid$Bairro == "ribeirao"),names(covid) == "Bairro"]<- "ribeirao da ilha"
covid[which(covid$Bairro == "ribeirao da  ilha"),names(covid) == "Bairro"]<- "ribeirao da ilha"
covid[which(covid$Bairro == "ribeirao da lha"),names(covid) == "Bairro"]<- "ribeirao da ilha"
covid[which(covid$Bairro == "ribeiriao da ilha"),names(covid) == "Bairro"]<- "ribeirao da ilha"
covid[which(covid$Bairro == "sa£o joa£ do rio vermelho"),names(covid) == "Bairro"]<- "rio vermelho"
covid[which(covid$Bairro == "sa£o joa£o do rio vermelho"),names(covid) == "Bairro"]<- "rio vermelho"
covid[which(covid$Bairro == "sa£o joao do rio vermelho"),names(covid) == "Bairro"]<- "rio vermelho"
covid[which(covid$Bairro == "saco de limaµes"),names(covid) == "Bairro"]<- "saco dos limoes"
covid[which(covid$Bairro == "saco dos limaµes"),names(covid) == "Bairro"]<- "saco dos limoes"
covid[which(covid$Bairro == "sao joao do rio vermelho" ),names(covid) == "Bairro"]<- "rio vermelho"
covid[which(covid$Bairro == "sao joao do rio vermelho ap 5" ),names(covid) == "Bairro"]<- "rio vermelho"
covid[which(covid$Bairro == "serraria"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "tepera"),names(covid) == "Bairro"]<- "tapera"
covid[which(covid$Bairro == "trimdade"),names(covid) == "Bairro"]<- "trindade"
covid[which(covid$Bairro == "rocado"),names(covid) == "Bairro"]<- "rio vermelho"
covid[which(covid$Bairro == "varge, grande"),names(covid) == "Bairro"]<- "vargem grande"
covid[which(covid$Bairro == "rocado"),names(covid) == "Bairro"]<- "rio vermelho"
covid[which(covid$Bairro == "varge, grande"),names(covid) == "Bairro"]<- "vargem grande"
covid[which(covid$Bairro == "vargem do bom jesus"),names(covid) == "Bairro"] <- "canasvieiras"
covid[which(covid$Bairro == "vila cachoeira"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "sambaqui"),names(covid) == "Bairro"]<- "saco grande"
covid[which(covid$Bairro == "santa monica"),names(covid) == "Bairro"]<- "itacorubi"
covid[which(covid$Bairro == "caeira da barra do sul"),names(covid) == "Bairro"]<- "caieira da barra do sul"
covid[which(covid$Bairro == "caeira do saco dos limoes"),names(covid) == "Bairro"]<- "saco dos limoes"
covid[which(covid$Bairro == "costa de dentro"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "bela vista"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "acores"),names(covid) == "Bairro"]<- "pantano do sul"
covid[which(covid$Bairro == "jureraª"),names(covid) == "Bairro"]<- "pantano do sul"
covid[which(covid$Bairro == "cacupa©"),names(covid) == "Bairro"]<- "pantano do sul"
covid[which(covid$Bairro == "joa£o paulo"),names(covid) == "Bairro"]<- "pantano do sul"
covid[which(covid$Bairro == "sto anta´nio de lisboa"),names(covid) == "Bairro"]<- "pantano do sul"
covid[which(covid$Bairro == "abraa£o"),names(covid) == "Bairro"]<- "pantano do sul"
covid[which(covid$Bairro == "ipiranga"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "fundos"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "armaa§a£o do pa¢ntano do sul"),names(covid) == "Bairro"]<- "pantano do sul"
covid[which(covid$Bairro == "forquilhas"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "areias"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "praia brava"),names(covid) == "Bairro"]<- "ponta das canas"
covid[which(covid$Bairro == "pagani"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "bom abrigo"),names(covid) == "Bairro"]<- "coqueiros"
covid[which(covid$Bairro == "ponte do imaruim"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "joaƒo paulo"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "parque sao jorge"),names(covid) == "Bairro"]<- "itacorubi"
covid[which(covid$Bairro == "daniela"),names(covid) == "Bairro"]<- "jurere"
covid[which(covid$Bairro == "parque sa£o jorge"),names(covid) == "Bairro"]<- "corrego grande"
covid[which(covid$Bairro == "nao identificado"),names(covid) == "Bairro"]<- NA
covid[which(covid$Bairro == "nossa senhora do rosa¡rio"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "santa ma´nica"),names(covid) == "Bairro"]<- "itacorubi"
covid[which(covid$Bairro == "cachoeira"),names(covid) == "Bairro"]<- "cachoeira do bom jesus"
covid[which(covid$Bairro == "caiera- scao dos limoes"),names(covid) == "Bairro"]<- "saco dos limoes"
covid[which(covid$Bairro == "sao sebastiao"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "sem denominacao"),names(covid) == "Bairro"]<- NA
covid[which(covid$Bairro == "armaa‡aƒo do pantano do sul"),names(covid) == "Bairro"]<- "pantano do sul"
covid[which(covid$Bairro == "saco dos  limaµes"),names(covid) == "Bairro"]<- "saco dos limoes"
covid[which(covid$Bairro == "porto da lagoa"),names(covid) == "Bairro"]<- "canto da lagoa"
covid[which(covid$Bairro == "jardim cidade florianopolis"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "jardim atla¢ntico"),names(covid) == "Bairro"]<- "jardim atlantico"
covid[which(covid$Bairro == "ingleses-rio vermelho"),names(covid) == "Bairro"]<- "rio vermelho"
covid[which(covid$Bairro == "itaguaa§u"),names(covid) == "Bairro"]<- "coqueiros"
covid[which(covid$Bairro == "josa© mendes"),names(covid) == "Bairro"]<- "prainha"
covid[which(covid$Bairro == "kobrasol"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "vargem grande/de fora"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "sto antonio de lisboa"),names(covid) == "Bairro"]<- "santo antonio de lisboa"
covid[which(covid$Bairro == "bosques das mansoes"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "ingleses do ri vermelho"),names(covid) == "Bairro"]<- "ingleses"
covid[which(covid$Bairro == "agronomia"),names(covid) == "Bairro"]<- "agronomica"
covid[which(covid$Bairro == "cachoeira de bom jesus"),names(covid) == "Bairro"]<- "cachoeira do bom jesus"
covid[which(covid$Bairro == "canto da lago"),names(covid) == "Bairro"]<- "canto da lagoa"
covid[which(covid$Bairro == "capoieras"),names(covid) == "Bairro"]<- "capoeiras"
covid[which(covid$Bairro == "costeira do pirajubaa©"),names(covid) == "Bairro"]<- "costeira do pirajubae"
covid[which(covid$Bairro == "lagoa de conceicao"),names(covid) == "Bairro"]<- "lagoa da conceicao"
covid[which(covid$Bairro == "pedra branca"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "saco de limoes"),names(covid) == "Bairro"]<- "saco dos limoes"
covid[which(covid$Bairro == "ponta das canasvieras"),names(covid) == "Bairro"]<- "ponta das canas"

unique(covid$Bairro)

## Alguns algoritmos como o Random Forest não trabalham com variáveis com grande númer de categorias, por isso, selecionaram-se
## Bairros com mais de 100 notificações, os demais bairros foram transformados em "outro" 
covid$Bairro <- as.character(covid$Bairro)
bairros <- c("agronomica", "campeche","capoeiras", "centro", "ingleses" , "itacorubi", 
	     "rio vermelho", "saco grande","tapera","trindade")#bairro com mais de 100 notificacoes
covid$Bairro <- ifelse(covid$Bairro %in% bairros,covid$Bairro, "outro")


## Mantendo apenas resultados "confirmado" ou "descartado" ou "não detectado" (essas duas últimas categorias são a mesma coisa). 
## Os pacientes aguardando resultado ou com resultado inconclusivo foram classificados como missing
covid$`Resultado do teste` <- ifelse(covid$`Resultado do teste` == "confirmado", "confirmado",
			  		ifelse(covid$`Resultado do teste` == "descartado", "descartado",
			  			ifelse(covid$`Resultado do teste` == "não detectado", "descartado",
			  	       		"missing")))

#Selecionando as variáveis de trabalho
covid <- covid %>%
	dplyr::select(ID,Sexo, `Municipio de residencia`, Bairro, IDADE, TRIAGEM, `Data do início dos sintomas`,
		      `Resultado do teste`)

## Substituindo NA por "missing" nas variáveis que serão utilizadas como categógicas.
covid[,!(names(covid) %in% c("ID", "Data do início dos sintomas"))] <- sapply(covid[,!(names(covid) %in% c("ID", "Data do início dos sintomas"))], as.factor) %>% as.data.frame()
covid$`Resultado do teste` <-fct_explicit_na(covid$`Resultado do teste`, "missing")
covid$`Municipio de residencia` <-fct_explicit_na(covid$`Municipio de residencia`, "missing")
covid$Sexo <-fct_explicit_na(covid$Sexo, "missing")
covid$Bairro <-fct_explicit_na(covid$Bairro, "missing")
covid$TRIAGEM <-fct_explicit_na(covid$TRIAGEM, "missing")
covid$IDADE <-fct_explicit_na(covid$IDADE, "missing")



## Com a quantidade de missim em Sexo e Idade é pequeno esses dados foram retirados para que não haja problemas na paralelização
covid <- subset(covid, covid$Sexo != "missing")
covid <- subset(covid, covid$IDADE != "missing")

## Ajustando nome da base
names(covid) <- c("ID", "Sexo", "Municipio", "Bairro", "Idade", "Triagem", "Inicio", "Resultado")
summary(covid)


# Exportando base ---------------------------------------------------------
write.csv(covid, "dados/covid_ajustado.csv", row.names = F)
