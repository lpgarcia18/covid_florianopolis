# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)

# Pacotes -----------------------------------------------------------------
library(readr)
library(readxl)
library(tidyverse)
library(reshape2)
library(purrr)


# Funcões -----------------------------------------------------------------
## Ler as tabelas dentro da planilha de excel
read_excel_allsheets <- function(filename, tibble = FALSE) {
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    if(!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
    x
}

#Retirar acentos
rm_accent <- function(str,pattern="all") {
	  # Rotinas e funções úteis V 1.0
	  # rm.accent - REMOVE ACENTOS DE PALAVRAS
	  # Função que tira todos os acentos e pontuações de um vetor de strings.
	  # Parâmetros:
	  # str - vetor de strings que terão seus acentos retirados.
	  # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
	  #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
	  #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
	  #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
	  if(!is.character(str))
	    str <- as.character(str)
	
	  pattern <- unique(pattern)
	
	  if(any(pattern=="Ç"))
	    pattern[pattern=="Ç"] <- "ç"
	
	  symbols <- c(
	    acute = "áéíóúÁÉÍÓÚýÝ",
	    grave = "àèìòùÀÈÌÒÙ",
	    circunflex = "âêîôûÂÊÎÔÛ",
	    tilde = "ãõÃÕñÑ",
	    umlaut = "äëïöüÄËÏÖÜÿ",
	    cedil = "çÇ"
	  )
	
	  nudeSymbols <- c(
	    acute = "aeiouAEIOUyY",
	    grave = "aeiouAEIOU",
	    circunflex = "aeiouAEIOU",
	    tilde = "aoAOnN",
	    umlaut = "aeiouAEIOUy",
	    cedil = "cC"
	  )
	
	  accentTypes <- c("´","`","^","~","¨","ç")
	
	  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
	    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
	
	  for(i in which(accentTypes%in%pattern))
	    str <- chartr(symbols[i],nudeSymbols[i], str)
	
	  return(str)
}

## Ajutar títulos das bases
ajustar_nomes <- function(x){
	 x%>%
	  stringr::str_trim() %>%                        #Remove espaços em branco sobrando
	  stringr::str_to_lower() %>%                    #Converte todas as strings para minusculo
	  rm_accent() %>%                                #Remove os acentos com a funcao criada acima
	  stringr::str_replace_all("[/' '.()]", "_") %>% #Substitui os caracteres especiais por "_"
	  stringr::str_replace_all("_+", "_") %>%        #Substitui os caracteres especiais por "_"   
	  stringr::str_replace("_$", "")                 #Substitui o caracter especiais por "_"
	}


# Importanto bases --------------------------------------------------------
## Dados de casos suspeitos
covid <- read_csv("dados/covid_anonimizado.csv")
## Dados demográficos
populacao <- read_excel_allsheets("dados/demografia/Estima_Pop_Genero_2000_a_2023/Estima_Pop_Genero_2000_a_2023_ULS.xlsx")
populacao <- map(populacao,`[`,"44075")#extraindo coluna de 2020
titulos_pop <- c("populacao", "homens", "mulheres")
titulos_pop <- paste("populacao", titulos_pop)
populacao <- do.call(cbind,populacao)
names(populacao) <- titulos_pop

escolaridade <- read_excel_allsheets("dados/demografia/Estima_Escolaridade_2000_a_2023/Estima_Escolaridade_Pessoas_2000_2030_Unidade_Saude.xlsx")
escolaridade <- map(escolaridade,`[`,"44075")#extraindo coluna de 2020
titulos_esc <- names(escolaridade)
titulos_esc <- paste("escolaridade", titulos_esc)
titulos_esc <- ajustar_nomes(titulos_esc)
escolaridade <- do.call(cbind,escolaridade)
names(escolaridade) <- titulos_esc

cor_pele <- read_excel_allsheets("dados/demografia/Estima_Raça_Cor_2000_a_2023/Eatima_Raca_Cor_2000_a_2023_ULS.xlsx")
cor_pele <- map(cor_pele,`[`,"44075")#extraindo coluna de 2020
titulos_cp <- names(cor_pele)
titulos_cp <-  paste("cor_pele", titulos_cp)
titulos_cp <- ajustar_nomes(titulos_cp)
cor_pele <- do.call(cbind,cor_pele)
names(cor_pele) <- titulos_cp
cor_pele <- cor_pele[-54,] #retirando o total

renda <- read_excel_allsheets("dados/demografia/Estima_Renda_2000_a_2023/Estima_Renda_ULS.xlsx")
renda <- map(renda,`[`,"44075")#extraindo coluna de 2020
titulos_renda <- names(renda)
titulos_renda <- ajustar_nomes(titulos_renda)
renda <- do.call(cbind,renda)
names(renda) <- titulos_renda
renda <- renda[-54,] #retirando o total

demografia <- cbind(populacao,escolaridade, cor_pele, renda)
demografia <- demografia[-c(1:4),]#retirando linhas sem informação

idade <- read_excel_allsheets("dados/demografia/Estima_Idade_2000_a_2023/Estima_Idade_0_a_100_ou_mais_de_2000_a_2030_ULS.xlsx")
nome_cs <- map(idade,`[`,5)#extraindo o nome dos cs
nome_cs <- do.call(cbind,nome_cs)
nome_cs <- names(nome_cs)
nome_cs <- ajustar_nomes(nome_cs)
nome_fe <- map(idade,`[`,4)#extraindo o nome das faixas estárias
nome_fe <- unlist(nome_fe[1]) %>% as.data.frame()
nome_fe <- nome_fe[-c(1:3),]
nome_fe <- as.character(nome_fe)
nome_fe <- paste0("idade_", nome_fe)
nome_fe[which(is.na(nome_fe))] <- "Total"
nome_fe <- ajustar_nomes(nome_fe)
idade <- map(idade,`[`,"...25")#extraindo coluna de 2020
idade <- do.call(cbind,idade)
names(idade) <- nome_cs
idade <- idade[-c(1:3),]
idade <- idade %>% t() %>% as.data.frame()
names(idade) <- nome_fe
idade$total <- NULL

demografia <- cbind(idade, demografia)
demografia$territorio <- row.names(demografia)

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


## Mantendo apenas resultados "confirmado" ou "descartado" ou "não detectado" (essas duas últimas categorias são a mesma coisa). 
## Os pacientes aguardando resultado ou com resultado inconclusivo foram classificados como missing
covid$`Resultado do teste` <- ifelse(covid$`Resultado do teste` == "confirmado", "confirmado",
			  		ifelse(covid$`Resultado do teste` == "descartado", "descartado",
			  			ifelse(covid$`Resultado do teste` == "não detectado", "descartado",
			  	       		"missing")))

## Ajustando a variável território (Áreas dos centros de saúde) e subterritório (áreas das equipes de saúde da família)
## Florianópolis possui projeções demográficos (população, escolaridade, renda, sexo, idade), por território.
covid <- covid %>% rename(Territorio = `Unidade de referência`,
			 Subterritorio = `Equipe de referência`)
covid$Territorio <- ajustar_nomes(covid$Territorio)
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





# Merge das bases ---------------------------------------------------------
covid$Territorio <- as.character(covid$Territorio)
demografia$territorio <- as.character(demografia$territorio)
covid <- merge(covid,demografia, by.x = "Territorio",by.y = "territorio", all.y = T) #Retirando dados de outros municípios

## Ajustando nome da base
names(covid)[c(1:10)] <- c("TERRITORIO", "ID", "SEXO", "MUNICIPIO", "SUBTERRITORIO","FAIXA_ETARIA", "IDADE", "TRIAGEM", "INICIO_SINTOMAS", "RESULTADO")
summary(covid)


## Alguns algoritmos como o Random Forest não trabalham com variáveis com grande númer de categorias, por isso, selecionaram-se
## território com mais de 50 notificações e subterritórios com mais de 25 notificações, 
## os demais foram transformados em "outro"
# table_territorio <- table(covid$Territorio) %>% as.data.frame()
# table_territorio <- subset(table_territorio, table_territorio$Freq > 50)
# covid$Territorio <- as.character(covid$Territorio)
# covid$Territorio <- ifelse(covid$Territorio %in% table_territorio$Var1,covid$Territorio, "outro")
# covid$Territorio <- ajustar_nomes(covid$Territorio)
table_subterritorio <- table(covid$SUBTERRITORIO) %>% as.data.frame()
table_subterritorio <- subset(table_subterritorio, table_subterritorio$Freq > 25)
covid$SUBTERRITORIO <- as.character(covid$SUBTERRITORIO)
covid$SUBTERRITORIO <- ifelse(covid$SUBTERRITORIO %in% table_subterritorio$Var1,covid$SUBTERRITORIO, "outro")
# Exportando base ---------------------------------------------------------
write.csv(covid, "dados/covid_ajustado.csv", row.names = F, fileEncoding = "UTF-8")
