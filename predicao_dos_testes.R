# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)
options(java.parameters = "-Xmx10g") #Evitar que o java tenha problemas de memória

# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(caret)
library(mlr)
library(forecast)
library(doParallel)
library(parallelMap)
library(tigerstats)
library(matrixStats)

# Importanto bases ---------------------------------------------------------------
## Dados de suspeitos 
covid <- read_csv("dados/covid_ajustado.csv")

# Transformando base ------------------------------------------------------
covid$ID <- as.factor(covid$ID)
covid$PROF_SAUDE <-  as.factor(covid$PROF_SAUDE)
covid$RESULTADO <- as.factor(covid$RESULTADO)
covid$TERRITORIO <-as.factor(covid$TERRITORIO)
covid$SEXO <-as.factor(covid$SEXO)
covid$TRIAGEM <-as.factor(covid$TRIAGEM)
covid$RACA_COR <-as.factor(covid$RACA_COR)
covid$FAIXA_ETARIA <-as.factor(covid$FAIXA_ETARIA)
covid$INICIO_SINTOMAS <-  as.numeric(covid$INICIO_SINTOMAS)#Transformando em número, pois o learner do mlr não trabalha com data
covid$DATA_NOTIFICACAO <- as.numeric(covid$DATA_NOTIFICACAO)#Transformando em número, pois o learner do mlr não trabalha com data


# Formação das bases de treino, teste e predição --------------------------
train_test_base <- subset(covid, covid$RESULTADO == "descartado" |
			  	covid$RESULTADO == "confirmado")
train_test_base$RESULTADO <- factor(train_test_base$RESULTADO, levels = c("confirmado", "descartado"))
summary(train_test_base)

#Base para predição
predic_base <- subset(covid, !(covid$RESULTADO %in% c("confirmado", "descartado")))
summary(predic_base)



#Iniciando paralelização
parallelStartSocket(12)

estimativa <- function(train_test_base, predic_base, seed){
	
	# Separando a base de traino_validacao da base de teste
	indice <- createDataPartition(train_test_base$RESULTADO, p = 0.7, list=FALSE)
	
	#Base de treino
	train_base <- train_test_base[indice,]
	summary(train_base)
	
	#Base de teste
	test_base <- train_test_base[-indice,]
	summary(test_base)
	
	# Setando a tarefa
	mod1_task <- makeClassifTask(data = train_base[,!(names(train_base) %in% c("ID"))], target = "RESULTADO")#Diversos algoritmos não trabalham com variáveis com muitas categorias e com datas por isso Início foi retirado
	confirmados <- sum(train_base$RESULTADO == "confirmado")
	descartados <- sum(train_base$RESULTADO == "descartado")
	## Como há muitos mais casos descartados que confirmados utilizou-se under sampling para balanceamento
	mod1_task_under <- undersample(mod1_task, rate = confirmados/descartados)


	## Filtro das features e hiperparâmetros
	#listFilterMethods()
	rf <- makeFilterWrapper(learner = 'classif.ranger', fw.method = "ranger_permutation")
	ps_rf <- makeParamSet( 
		makeIntegerLearnerParam(id = "num.trees", lower = 100L, upper = 2000L),
		makeIntegerLearnerParam(id = "mtry", lower = 1L,upper = 50L),
		makeIntegerLearnerParam(id = "min.node.size", lower = 1L, upper = 10L),
		makeNumericLearnerParam(id = "sample.fraction", lower = 0L, upper = 1L),
		makeDiscreteParam("fw.perc", values = seq(0.2, 1, 0.05))
	)
	## Estratégia de hiperparametrização - random search
	ctrl <- makeTuneControlRandom(maxit = 10L)
	## Estratégia de ressampling do inner loop - validação cruzada com estratificação dos resultados balanceados entre as folds
	folds <- 5
	inner <- makeResampleDesc("CV", iter = folds, stratify = TRUE)
	#measures https://mlr.mlr-org.com/articles/tutorial/measures.html
	## learner ajustados para filtragem e tuning
	lrn_rf <- makeTuneWrapper(learner = rf, resampling = inner, par.set = ps_rf, control = ctrl,
				  show.info = T, measure = acc)
	
	## Estratégia de ressampling do outer loop - validação cruzada com estratificação dos resultados balanceados entre as folds
	outer <- makeResampleDesc("CV", iter = folds, predict = "both", stratify = TRUE)
	
	# Retornar aviso e não erro - para não parar processo de tunning
	configureMlr(on.learner.error = "warn")
	
	## Tunning 
	mod_train <- mlr::resample(learner = lrn_rf, task = mod1_task_under, resampling = outer, models = TRUE, show.info = FALSE, 
				   measure = list(acc, tpr, tnr), extract = getTuneResult)
	predicoes <- mod_train$pred$data
	predicoes_treino <- subset(predicoes, predicoes$set == "train")
	predicoes_validacao <- subset(predicoes, predicoes$set == "test")
	#Métricas de treino
	result_train <- mod_train$measures.train 
	result_train <- colMeans(result_train) %>% t() %>% as.data.frame()
	result_train$iter <- NULL
	
	#Métricas de validação
	result_valid <- mod_train$measures.test
	result_valid <- colMeans(result_valid) %>% t() %>% as.data.frame()
	result_valid$iter <- NULL
	
	## Exração de hiperparâmentros
	mmce_resultados <- list()
	for(i in 1:length(mod_train$extract)){ 
		mmce_resultados[[i]] <- mod_train$extract[[i]]$y 
	}
	
	mmce_resultados <- do.call(rbind, mmce_resultados) %>% as.data.frame()
	mmce_resultados$iteracao <- c(1:nrow(mmce_resultados))
	iteracao <- mmce_resultados[which(mmce_resultados$acc.test.mean == min(mmce_resultados$acc.test.mean, na.rm = T)) , 2]
	
	tunned_model <- mod_train$models[[iteracao]] #modelo tunado
	
	tuned_par <- mod_train$extract[[iteracao]]$x #para ver hiperparâmetros
	
	## Predição na base de teste
	mod2_task <- makeClassifTask(data = test_base[,!(names(test_base) %in% c("ID"))], target = "RESULTADO")
	PREDITO <- predict(tunned_model, mod2_task)
	result_test <- calculateConfusionMatrix(pred = PREDITO,sums = F,relative = F)
	result_test <- result_test[[1]][c(1,2,4,5)] 
	result_test <- data_frame(acc = (sum(result_test[c(1,4)])/ sum(result_test)), #1=verdadeiro positivo, 2 = falso positivo,  3 = falso negativo, 4 = verdadeiro negativo
				  tpr = (sum(result_test[c(1)])/ sum(result_test[c(1,3)])), # Sensibilidade
				  tnr = (sum(result_test[c(4)])/ sum(result_test[c(2,4)]))) # Especificidade

	## Predição dos dados faltantes
	mod3_task <- makeClassifTask(data = predic_base[,!(names(predic_base) %in% c("ID"))], target = "RESULTADO")
	predic_base$RESULTADO <- predict(tunned_model, mod3_task)$data[,3]
	predic_base$RESULTADO <- as.character(predic_base$RESULTADO)
	
	
	#Dados para grãfico
	covid$INICIO_SINTOMAS <- as.Date(covid$INICIO_SINTOMAS, origin = "1970-01-01")
	covid_id <- covid %>% dplyr::select(ID, INICIO_SINTOMAS)
	train_test_base$RESULTADO <- as.character(train_test_base$RESULTADO)
	train_test_base$INICIO_SINTOMAS <- as.Date(train_test_base$INICIO_SINTOMAS, origin = "1970-01-01")
	cum_train <- merge(train_test_base, covid_id, by = c("ID", "INICIO_SINTOMAS"), all = T)
	cum_train <- subset(cum_train, cum_train$RESULTADO == "confirmado")
	cum_train$NUMERO <- 1
	cum_train$INICIO_SINTOMAS <- as.Date(cum_train$INICIO_SINTOMAS, format = "%Y-%m-%d")
	cum_train <- cum_train %>%
		group_by(INICIO_SINTOMAS) %>%
		summarise(CASOS = sum(NUMERO, na.rm = T))
	cum_train$CUM_CASOS <- cumsum(cum_train$CASOS) 
	cum_train$DADOS <- "Atuais"
	predic_base$DADOS <- "Preditos"
	train_test_base$DADOS <- "Atuais"
	predic_base$INICIO_SINTOMAS <- as.Date(predic_base$INICIO_SINTOMAS, origin = "1970-01-01")
	train_test_base$INICIO_SINTOMAS <- as.Date(train_test_base$INICIO_SINTOMAS, origin = "1970-01-01")
	base_final <- rbind(predic_base, train_test_base) %>% as.data.frame()
	base_final <- merge(base_final, covid_id, by = c("ID", "INICIO_SINTOMAS"), all = T)
	cum_base <- subset(base_final, base_final$RESULTADO == "confirmado")
	cum_base$NUMERO <- 1
	cum_base$INICIO_SINTOMAS <- as.Date(cum_base$INICIO_SINTOMAS, format = "%Y-%m-%d")
	cum_base <- cum_base %>%
		group_by(INICIO_SINTOMAS) %>%
		summarise(CASOS = sum(NUMERO, na.rm = T))
	cum_base$CUM_CASOS <- cumsum(cum_base$CASOS) 
	cum_base$DADOS <- "Totais" #Dados totais = atuiais + preditos
	cum_base <- rbind(cum_train, cum_base) %>% as.data.frame()
	
	return(list(cum_base, result_train, result_valid, result_test))

}

n_boot <- 5

boot_base <- list()
for(i in 1:n_boot){
	boot_base[[i]] <- estimativa(train_test_base, predic_base, 1)
}

cum_base <- list()
for(i in 1:n_boot){
	cum_base[[i]] <- boot_base[[i]][[1]][c(1,2,4)] %>% as.data.frame()	
}
cum_base <- Reduce(function(x,y) merge(x , y, by = c("INICIO_SINTOMAS", "DADOS"), all = T), cum_base) %>% as.data.frame()
cum_base_mat <- as.matrix(cum_base[,c(3:ncol(cum_base))])
cum_base$MEDIANA_CASOS <- rowMedians(cum_base_mat, na.rm = T)


train_result <- list()
for(i in 1:n_boot){
	train_result[[i]] <- boot_base[[i]][2] %>% as.data.frame()	
}
train_result <- do.call(rbind,train_result)

valid_result <- list()
for(i in 1:n_boot){
	valid_result[[i]] <- boot_base[[i]][3] %>% as.data.frame()	
}
valid_result <- do.call(rbind,valid_result)

test_result <- list()
for(i in 1:n_boot){
	test_result[[i]] <- boot_base[[i]][4] %>% as.data.frame()	
}
test_result <- do.call(rbind,test_result)

#Parando paralelização
parallelStop()



cum_base1 <- cum_base[,c(1,2,ncol(cum_base))]

# Plotando RESULTADOs -----------------------------------------------------
ggplot(cum_base, aes(as.Date(INICIO_SINTOMAS), CUM_CASOS, group = DADOS, color = DADOS))+
	geom_line()+
	theme_bw()+
	labs(y = "Número de Casos", 
	     x = "Data dos Primeiros Sintomas")

ggplot(cum_base1, aes(as.Date(INICIO_SINTOMAS), MEDIANA_CASOS, group = as.factor(DADOS), color = DADOS))+
	geom_line()+
	theme_bw()+
	labs(y = "Número de Casos", 
	     x = "Data dos Primeiros Sintomas")


# Exportando base ---------------------------------------------------------
write.csv(cum_base, "dados/covid_preditos.csv", row.names = F, fileEncoding = "UTF-8")


