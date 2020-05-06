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
library(googlesheets4)


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
	  stringr::str_replace_all("-", "_") %>%        #Substitui os caracteres especiais por "_"   
	  stringr::str_replace("_$", "")                 #Substitui o caracter especiais por "_"
	}


# Importanto bases --------------------------------------------------------
## Dados demográficos
populacao <- read_excel_allsheets("dados/demografia/Estima_Pop_Genero_2000_a_2023/Estima_Pop_Genero_2000_a_2023_ULS.xlsx")
populacao <- map(populacao,`[`,"44075")#extraindo coluna de 2020
titulos_pop <- c("populacao", "homens", "mulheres")
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
titulos_cp <- sub(x = titulos_cp,pattern = "-",replacement = " ")
titulos_cp <- sub(x = titulos_cp,pattern = " ",replacement = "")
titulos_cp <- sub(x = titulos_cp,pattern = " ",replacement = "")
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
## Dados de casos suspeitos
covid <- read_csv("dados/covid_anonimizado.csv")


# Transformando base ------------------------------------------------------

# Excluindo entradas que não possuem data de início dos sintomas, pois essa informação é fundamental para se analisar a incidência
covid <- subset(covid, !is.na(covid$`Data do início dos sintomas`))

# Convertendo, ajustando e criando variáveis
covid <- sapply(covid, as.character) %>% as.data.frame()



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
covid[which(covid$Bairro == "monte verde / barra do sambaqui"),names(covid) == "Bairro"]<- "saco grande"
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
covid[which(covid$Bairro == "caminho novo"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "barreiros"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "cacupe"),names(covid) == "Bairro"]<- "santo antonio de lisboa"
covid[which(covid$Bairro == "carvoeira"),names(covid) == "Bairro"]<- "pantanal"


covid$Bairro <- ifelse(is.na(covid$Bairro) | covid$Bairro == "outro", covid$Bairro, paste0("cs ",covid$Bairro))
covid$Bairro <- ajustar_nomes(covid$Bairro)
sort(unique(covid$Bairro))


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


## Utilizando data de notificação se data de primeiro sintoma for nula
covid$`Data do início dos sintomas` <- ifelse(is.na(covid$`Data do início dos sintomas`), covid$`Data da notificação` ,covid$`Data do início dos sintomas`)

## Mantendo apenas resultados "confirmado" ou "descartado" ou "não detectado" (essas duas últimas categorias são a mesma coisa). 
## Os pacientes aguardando resultado ou com resultado inconclusivo foram classificados como missing
covid$`Resultado do teste` <- ifelse(covid$`Resultado do teste` == "confirmado", "confirmado",
			  		ifelse(covid$`Resultado do teste` == "descartado", "descartado",
			  			ifelse(covid$`Resultado do teste` == "não detectado", "descartado",
			  	       		"missing")))

## Ajustando a variável território (Áreas dos centros de saúde) e subterritório (áreas das equipes de saúde da família)
## Florianópolis possui projeções demográficos (população, escolaridade, renda, sexo, idade), por território.
## Quando o dado de CS de referência não estava disponível, utilizou-se o endereço para se inferir o terrotório
covid <- covid %>% rename(Territorio = `Unidade de referência`,
			 Subterritorio = `Equipe de referência`)
covid$Territorio <- ajustar_nomes(covid$Territorio)
covid$Territorio <- ifelse(is.na(covid$Territorio), covid$Bairro, covid$Territorio)

sort(unique(covid$Territorio))

## Construindo variável Quantidade de Infectados (pessoas com até 14 dias de início de sintomas) por território
covid$`Data do início dos sintomas` <- as.Date(covid$`Data do início dos sintomas`, format = "%Y-%m-%d", origin = "1970-01-01")
infec <- covid
infec <- subset(infec, infec$`Resultado do teste` == "confirmado")
infec$INFECTADO <-infec$`Data do início dos sintomas` 
INFECTADO <- table(infec$Territorio, infec$INFECTADO)
infec$INFECTADO_1 <-infec$`Data do início dos sintomas` + 1
INFECTADO_1 <- table(infec$Territorio, infec$INFECTADO_1)
infec$INFECTADO_2 <-infec$`Data do início dos sintomas` + 2
INFECTADO_2 <- table(infec$Territorio, infec$INFECTADO_2)
infec$INFECTADO_3 <-infec$`Data do início dos sintomas` + 3
INFECTADO_3 <- table(infec$Territorio, infec$INFECTADO_3)
infec$INFECTADO_4 <-infec$`Data do início dos sintomas` + 4
INFECTADO_4 <- table(infec$Territorio, infec$INFECTADO_4)
infec$INFECTADO_5 <-infec$`Data do início dos sintomas` + 5
INFECTADO_5 <- table(infec$Territorio, infec$INFECTADO_5)
infec$INFECTADO_6 <-infec$`Data do início dos sintomas` + 6
INFECTADO_6 <- table(infec$Territorio, infec$INFECTADO_6)
infec$INFECTADO_7 <-infec$`Data do início dos sintomas` + 7
INFECTADO_7 <- table(infec$Territorio, infec$INFECTADO_7)
infec$INFECTADO_8 <-infec$`Data do início dos sintomas` + 8
INFECTADO_8 <- table(infec$Territorio, infec$INFECTADO_8)
infec$INFECTADO_9 <-infec$`Data do início dos sintomas` + 9
INFECTADO_9 <- table(infec$Territorio, infec$INFECTADO_9)
infec$INFECTADO_10 <-infec$`Data do início dos sintomas` + 10
INFECTADO_10 <- table(infec$Territorio, infec$INFECTADO_10)
infec$INFECTADO_11 <-infec$`Data do início dos sintomas` + 11
INFECTADO_11 <- table(infec$Territorio, infec$INFECTADO_11)
infec$INFECTADO_12 <-infec$`Data do início dos sintomas` + 12
INFECTADO_12 <- table(infec$Territorio, infec$INFECTADO_12)
infec$INFECTADO_13 <-infec$`Data do início dos sintomas` + 13
INFECTADO_13 <- table(infec$Territorio, infec$INFECTADO_13)
INFECTADO <- melt(INFECTADO)
INFECTADO_1 <- melt(INFECTADO_1)
INFECTADO_2 <- melt(INFECTADO_2)
INFECTADO_3 <- melt(INFECTADO_3)
INFECTADO_4 <- melt(INFECTADO_4)
INFECTADO_5 <- melt(INFECTADO_5)
INFECTADO_6 <- melt(INFECTADO_6)
INFECTADO_7 <- melt(INFECTADO_7)
INFECTADO_8 <- melt(INFECTADO_8)
INFECTADO_9 <- melt(INFECTADO_9)
INFECTADO_10 <- melt(INFECTADO_10)
INFECTADO_11 <- melt(INFECTADO_11)
INFECTADO_12 <- melt(INFECTADO_12)
INFECTADO_13 <- melt(INFECTADO_13)

infec <- Reduce(function(x,y) merge(x = x, y = y, by = c("Var1", "Var2")), 
       		list(INFECTADO,
			INFECTADO_1,
			INFECTADO_2,
			INFECTADO_3,
			INFECTADO_4,
			INFECTADO_5,
			INFECTADO_6,
			INFECTADO_7,
			INFECTADO_8,
			INFECTADO_9,
			INFECTADO_10,
			INFECTADO_11,
			INFECTADO_12,
			INFECTADO_13
)) %>% as.data.frame()

infec$INFECTADOS_TERRITORIO <- rowSums(infec[,c(3:16)], na.rm = T)
infec <- infec[,c(1,2,17)]
names(infec) <- c("Territorio", "Data do início dos sintomas", "INFECTADOS_TERRITORIO")
	
covid <- merge(covid, infec, by = c("Territorio", "Data do início dos sintomas"), all.x = T)

#Selecionando as variáveis de trabalho
covid <- covid %>%
	dplyr::select(ID,Sexo, `Municipio de residencia`, Territorio, Subterritorio, FAIXA_ETARIA, IDADE, TRIAGEM, `Data do início dos sintomas`,
		      `Resultado do teste`, `Raça/Cor`, INFECTADOS_TERRITORIO)


## Substituindo NA por "missing" nas variáveis que serão utilizadas como categógicas.
covid[,!(names(covid) %in% c("ID", "Data do início dos sintomas", "IDADE"))] <- sapply(covid[,!(names(covid) %in% c("ID", "Data do início dos sintomas", "IDADE"))], as.factor) %>% as.data.frame()
covid$`Resultado do teste` <-fct_explicit_na(covid$`Resultado do teste`, "missing")
covid$`Municipio de residencia` <-fct_explicit_na(covid$`Municipio de residencia`, "missing")
covid$Sexo <-fct_explicit_na(covid$Sexo, "missing")
covid$Territorio <-fct_explicit_na(covid$Territorio, "missing")
covid$Subterritorio <-fct_explicit_na(covid$Subterritorio, "missing")
covid$TRIAGEM <-fct_explicit_na(covid$TRIAGEM, "missing")
covid$FAIXA_ETARIA <-fct_explicit_na(covid$FAIXA_ETARIA, "missing")
covid$`Raça/Cor` <-fct_explicit_na(covid$`Raça/Cor`, "missing")




## Com a quantidade de missim em Sexo e Idade é pequeno esses dados foram retirados para que não haja problemas na paralelização
covid <- subset(covid, covid$Sexo != "missing")
covid <- subset(covid, covid$IDADE != "missing")





# Merge das bases ---------------------------------------------------------
covid$Territorio <- as.character(covid$Territorio)
demografia$territorio <- as.character(demografia$territorio)
covid <- merge(covid,demografia, by.x = "Territorio",by.y = "territorio", all.y = T) #Retirando dados de outros municípios

## Calculando a taxa de infectados por território
covid$TX_INFECTADOS_TERRITORIO <- as.numeric(covid$INFECTADOS_TERRITORIO)/as.numeric(covid$populacao)*100000



## Ajustando nome da base
names(covid)[c(1:12,152)] <- c("TERRITORIO", "ID", "SEXO", "MUNICIPIO", "SUBTERRITORIO","FAIXA_ETARIA", "IDADE", "TRIAGEM", "INICIO_SINTOMAS", "RESULTADO", "RACA_COR", "INFECTADOS_TERRITORIO", "TX_INFECTADOS_TERRITORIO")
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

covid$INFECTADOS_TERRITORIO <- as.numeric(covid$INFECTADOS_TERRITORIO)
covid$INFECTADOS_TERRITORIO <- if_else(is.na(covid$INFECTADOS_TERRITORIO),0,covid$INFECTADOS_TERRITORIO)
covid$TX_INFECTADOS_TERRITORIO <- as.numeric(covid$TX_INFECTADOS_TERRITORIO)
covid$TX_INFECTADOS_TERRITORIO <- if_else(is.na(covid$TX_INFECTADOS_TERRITORIO),0,covid$TX_INFECTADOS_TERRITORIO)


# Dados de trânsito -------------------------------------------------------
## Utilizou-se o deslocamento por carro como proxy de contato social
## O tráfego é mensurado em 4 avenidas da cidade. Utilizou-se a média destas 4 aveindas
## Trabalhou-se com a hipótese de que a chance de transimissão pode variar de acordo com o fluxo de carros em 
## diferentes períodos. Por isso, utilizaram-se a mobilidade 14 períodos (Periodo atual e 13 dias anteriores à data de primeiros sintomas)
id_transito <- "1lfiVlAcIyMB2lBA3GXEwbN3qQJw0XdvoiNMJfxh1tPI"
transito <- read_sheet(id_transito,"fluxo_automoveis",skip = 0,col_names = T) %>% as.data.frame()
transito <- transito[,c(1,5)]
transito <- na.omit(transito)
names(transito) <- c("INICIO_SINTOMAS", "MEDIA_TRANSITO")
transito$INICIO_SINTOMAS <- as.Date(transito$INICIO_SINTOMAS, format = "%d/%m/%Y")
transito_lag1 <- transito
transito_lag1$INICIO_SINTOMAS <- transito_lag1$INICIO_SINTOMAS - 1
transito_lag2 <- transito
transito_lag2$INICIO_SINTOMAS <- transito_lag2$INICIO_SINTOMAS - 2
transito_lag3 <- transito
transito_lag3$INICIO_SINTOMAS <- transito_lag3$INICIO_SINTOMAS - 3
transito_lag4 <- transito
transito_lag4$INICIO_SINTOMAS <- transito_lag4$INICIO_SINTOMAS - 4
transito_lag5 <- transito
transito_lag5$INICIO_SINTOMAS <- transito_lag5$INICIO_SINTOMAS - 5
transito_lag6 <- transito
transito_lag6$INICIO_SINTOMAS <- transito_lag6$INICIO_SINTOMAS - 6
transito_lag7 <- transito
transito_lag7$INICIO_SINTOMAS <- transito_lag7$INICIO_SINTOMAS - 7
transito_lag8 <- transito
transito_lag8$INICIO_SINTOMAS <- transito_lag8$INICIO_SINTOMAS - 8
transito_lag9 <- transito
transito_lag9$INICIO_SINTOMAS <- transito_lag9$INICIO_SINTOMAS - 9
transito_lag10 <- transito
transito_lag10$INICIO_SINTOMAS <- transito_lag10$INICIO_SINTOMAS - 10
transito_lag11 <- transito
transito_lag11$INICIO_SINTOMAS <- transito_lag11$INICIO_SINTOMAS - 11
transito_lag12 <- transito
transito_lag12$INICIO_SINTOMAS <- transito_lag12$INICIO_SINTOMAS - 12
transito_lag13 <- transito
transito_lag13$INICIO_SINTOMAS <- transito_lag13$INICIO_SINTOMAS - 13
transito <- Reduce(function(x,y) merge(x = x, y = y, by = c("INICIO_SINTOMAS"), all = T), 
       		list(transito,
       		     transito_lag1,
       		     transito_lag2,
       		     transito_lag3,
       		     transito_lag4,
       		     transito_lag5,
       		     transito_lag6,
       		     transito_lag7,
       		     transito_lag8,
       		     transito_lag9,
       		     transito_lag10,
       		     transito_lag11,
       		     transito_lag12,
       		     transito_lag13
)) %>% as.data.frame()

names(transito) <- c("INICIO_SINTOMAS",
		     "MEDIA_TRANSITO",
		     "MEDIA_TRANSITO_LAG1",
		     "MEDIA_TRANSITO_LAG2",
		     "MEDIA_TRANSITO_LAG3",
		     "MEDIA_TRANSITO_LAG4",
		     "MEDIA_TRANSITO_LAG5",
		     "MEDIA_TRANSITO_LAG6",
		     "MEDIA_TRANSITO_LAG7",
		     "MEDIA_TRANSITO_LAG8",
		     "MEDIA_TRANSITO_LAG9",
		     "MEDIA_TRANSITO_LAG10",
		     "MEDIA_TRANSITO_LAG11",
		     "MEDIA_TRANSITO_LAG12",
		     "MEDIA_TRANSITO_LAG13")

covid <- merge(covid, transito, by = "INICIO_SINTOMAS", all = T)

## Extraindo datas de início de sintomas que foram digitadas erradas
covid <- subset(covid, covid$INICIO_SINTOMAS > as.Date("2020-02-01", format = "%Y-%m-%d"))
covid <- subset(covid, covid$INICIO_SINTOMAS < (Sys.Date()+1))

## Extraindo dados missing que foram inseridos com a base de transito
covid <- na.omit(covid)

## Proporcao de masculino
covid$homens <- as.numeric(as.character(covid$homens))
covid$mulheres <- as.numeric(as.character(covid$mulheres))
covid$PROP_MASC <- covid$homens/covid$mulheres

## Percentual de pessoas com 60 anos ou mais
covid[,c(12:112)] <- apply(covid[,c(12:112)], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))
covid$PERC_60_MAIS <- rowSums(covid[,c(72:112)])/rowSums(covid[,c(12:112)])


## Percentual de pessoas NÃO brancas
covid[,c(139:143)] <- apply(covid[,c(139:143)], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))
covid$PERC_NAO_BRANCA <- covid[,c(139)]/rowSums(covid[,c(139:143)])

## Percentual de pessoas 10 anos ou menos 
covid[,c(117:136,138)] <- apply(covid[,c(117:136,138)], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))
covid$PERC_ESC_10_MENOS <- rowSums(covid[,c(117:129)])/rowSums(covid[,c(117:136,138)])


# Exportando base ---------------------------------------------------------
write.csv(covid, "dados/covid_ajustado.csv", row.names = F, fileEncoding = "UTF-8")
