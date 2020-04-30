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
covid$FAIXA_ETARIA <- ifelse(covid$IDADE < 10, "10 menos", 
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

## Ajustando a variável território (Áreas dos centros de saúde) e subterritório (áreas das equipes de saúde da família)
## Florianópolis possui projeções demográficos (população, escolaridade, renda, sexo, idade), por território.
covid <- covid %>% rename(Territorio = `Unidade de referência`,
			 Subterritorio = `Equipe de referência`)

## Alguns algoritmos como o Random Forest não trabalham com variáveis com grande númer de categorias, por isso, selecionaram-se
## território com mais de 50 notificações e subterritórios com mais de 25 notificações, 
## os demais foram transformados em "outro"
table_territorio <- table(covid$Territorio) %>% as.data.frame()
table_territorio <- subset(table_territorio, table_territorio$Freq > 50)
covid$Territorio <- as.character(covid$Territorio)
covid$Territorio <- ifelse(covid$Territorio %in% table_territorio$Var1,covid$Territorio, "outro")
table_subterritorio <- table(covid$Subterritorio) %>% as.data.frame()
table_subterritorio <- subset(table_subterritorio, table_subterritorio$Freq > 25)
covid$Subterritorio <- as.character(covid$Subterritorio)
covid$Subterritorio <- ifelse(covid$Subterritorio %in% table_subterritorio$Var1,covid$Subterritorio, "outro")
unique(covid$Subterritorio)

## Mantendo apenas resultados "confirmado" ou "descartado" ou "não detectado" (essas duas últimas categorias são a mesma coisa). 
## Os pacientes aguardando resultado ou com resultado inconclusivo foram classificados como missing
covid$`Resultado do teste` <- ifelse(covid$`Resultado do teste` == "confirmado", "confirmado",
			  		ifelse(covid$`Resultado do teste` == "descartado", "descartado",
			  			ifelse(covid$`Resultado do teste` == "não detectado", "descartado",
			  	       		"missing")))

#Selecionando as variáveis de trabalho
covid <- covid %>%
	dplyr::select(ID,Sexo, `Municipio de residencia`, Territorio, Subterritorio, FAIXA_ETARIA, IDADE, TRIAGEM, `Data do início dos sintomas`,
		      `Resultado do teste`)

## Substituindo NA por "missing" nas variáveis que serão utilizadas como categógicas.
covid[,!(names(covid) %in% c("ID", "Data do início dos sintomas", "IDADE"))] <- sapply(covid[,!(names(covid) %in% c("ID", "Data do início dos sintomas", "IDADE"))], as.factor) %>% as.data.frame()
covid$`Resultado do teste` <-fct_explicit_na(covid$`Resultado do teste`, "missing")
covid$`Municipio de residencia` <-fct_explicit_na(covid$`Municipio de residencia`, "missing")
covid$Sexo <-fct_explicit_na(covid$Sexo, "missing")
covid$Territorio <-fct_explicit_na(covid$Territorio, "missing")
covid$Subterritorio <-fct_explicit_na(covid$Subterritorio, "missing")
covid$TRIAGEM <-fct_explicit_na(covid$TRIAGEM, "missing")
covid$FAIXA_ETARIA <-fct_explicit_na(covid$FAIXA_ETARIA, "missing")



## Com a quantidade de missim em Sexo e Idade é pequeno esses dados foram retirados para que não haja problemas na paralelização
covid <- subset(covid, covid$Sexo != "missing")
covid <- subset(covid, covid$IDADE != "missing")

## Ajustando nome da base
names(covid) <- c("ID", "SEXO", "MUNICIPIO", "TERRITORIO", "SUBTERRITORIO","FAIXA_ETARIA", "IDADE", "TRIAGEM", "INICIO_SINTOMAS", "RESULTADO")
summary(covid)


# Exportando base ---------------------------------------------------------
write.csv(covid, "dados/covid_ajustado.csv", row.names = F)
