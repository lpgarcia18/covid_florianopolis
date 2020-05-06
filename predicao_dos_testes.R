# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)
options(java.parameters = "-Xmx8g") #Evitar que o java tenha problemas de memória

# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(caret)
library(mlr)
library(forecast)
library(doParallel)
library(parallelMap)

# Importanto bases ---------------------------------------------------------------
## Dados de suspeitos 
covid <- read_csv("dados/covid_ajustado.csv")



# Transformando base ------------------------------------------------------
covid$TERRITORIO <- as.factor(covid$TERRITORIO)
covid$SEXO <- as.factor(covid$SEXO)
covid$MUNICIPIO <- as.factor(covid$MUNICIPIO)
covid$SUBTERRITORIO <- as.factor(covid$SUBTERRITORIO)
covid$TRIAGEM <- as.factor(covid$TRIAGEM)
covid$RESULTADO <- as.factor(covid$RESULTADO)
covid$RACA_COR <- as.factor(covid$RACA_COR)
covid$INICIO_SINTOMAS <- as.numeric(covid$INICIO_SINTOMAS)#Transformando em número, pois o learner do mlr não trabalha com data
covid$INFECTADOS_TERRITORIO <- as.numeric(covid$INFECTADOS_TERRITORIO)


# Formação das bases de treino, teste e predição --------------------------
train_test_base <- subset(covid, covid$RESULTADO == "descartado" |
	       	      covid$RESULTADO == "confirmado")
train_test_base$RESULTADO <- factor(train_test_base$RESULTADO, levels = c("confirmado", "descartado"))
set.seed(1)
indice <- createDataPartition(train_test_base$RESULTADO, p = 0.7, list=FALSE)

#Base de treino
train_base <- train_test_base[indice,]
summary(train_base)

#Base de teste
test_base <- train_test_base[-indice,]
summary(test_base)

#Base para predição
predic_base <- subset(covid, !(covid$RESULTADO %in% c("confirmado", "descartado")))
predic_base$RESULTADO <- NULL
summary(predic_base)

train_base <- train_base[,c(1:100)]
# Realizando benchmarking de algoritmos de classificação ------------------
parallelStartSocket(4)

mod1_task <- makeClassifTask(data = train_base[,!(names(train_base) %in% c("ID"))], target = "RESULTADO")#Diversos algoritmos não trabalham com variáveis com muitas categorias e com datas por isso Início foi retirado
confirmados <- sum(train_base$RESULTADO == "confirmado")
descartados <- sum(train_base$RESULTADO == "descartado")

## Como há muitos mais casos descartados que confirmados, oversampling, undersampling e smote foram testados
mod1_task_over <- oversample(mod1_task, rate = descartados/confirmados) 
mod1_task_under <- undersample(mod1_task, rate = confirmados/descartados)
mod1_task_smote <- smote(mod1_task, rate = descartados/confirmados, nn = 5)

lrns_type <- c(#'classif.adaboostm1',
		#'classif.bartMachine',
		#'classif.boosting',
		#'classif.gamboost',
		#'classif.gbm',
		#'classif.glmboost',
		#'classif.glmnet',
		#'classif.h2o.deeplearning',
		#'classif.h2o.gbm',
		#'classif.h2o.glm',
		'classif.h2o.randomForest',
		#'classif.naiveBayes',
		'classif.randomForest',
		'classif.randomForestSRC',
		'classif.ranger'
		#,'classif.svm'
		)


lrns <- list()
for(i in seq_along(lrns_type)){
	lrns[[i]] <- lrns_type[i]
}

cross_val <- makeResampleDesc("CV", iter = 5)

## Escolheu-se a acurárcia como métrica de maximização
#measures https://mlr.mlr-org.com/articles/tutorial/measures.html
benc <-  benchmark(tasks = mod1_task, learners = lrns, resampling = cross_val, 
		   measures = list(acc), show.info = FALSE, models = TRUE)
benc_under <-  benchmark(tasks = mod1_task_under, learners = lrns, resampling = cross_val, 
			 measures = list(acc), show.info = FALSE, models = TRUE)
benc_over <-  benchmark(tasks = mod1_task_over, learners = lrns, resampling = cross_val, 
			measures = list(acc), show.info = FALSE, models = TRUE)
benc_smote <-  benchmark(tasks = mod1_task_smote, learners = lrns, resampling = cross_val, 
			 measures = list(acc), show.info = FALSE, models = TRUE)

plotBMRBoxplots(benc, measure = acc)
plotBMRBoxplots(benc_under, measure = acc)
plotBMRBoxplots(benc_over, measure = acc)
plotBMRBoxplots(benc_smote, measure = acc)

# Predizendo o RESULTADO dos testes ---------------------------------------
## Escolheu-se o algoritmo e o learner que produziram a maior mediana de acurácia e possuiam a menor variação 
## na análise de benchmarking

set.seed(1)
result_train <- resample(learner = 'classif.randomForestSRC', task = mod1_task_over, resampling = cross_val, show.info = FALSE)
confusionMatrix(data = result_train$pred$data$response, reference = result_train$pred$data$truth)
confusionMatrix(data = result_train$pred$data$response, reference = result_train$pred$data$truth, mode = "prec_recall")

set.seed(1)
mod_pred <- mlr::train(mod1_task_over, learner = 'classif.randomForestSRC')
test_base$PREDICAO <- predict(mod_pred, newdata = test_base[, names(test_base) != "RESULTADO"])$data[,1]
confusionMatrix(data = test_base$PREDICAO, reference = test_base$RESULTADO)
confusionMatrix(data = test_base$PREDICAO, reference = test_base$RESULTADO, mode = "prec_recall")

predic_base$RESULTADO <- predict(mod_pred, newdata = predic_base[,names(predic_base) != "ID"])$data[,1]
predic_base$RESULTADO <- as.character(predic_base$RESULTADO)

parallelStop()

sum(predic_base$RESULTADO == "confirmado")

# Plotando predição -------------------------------------------------------
#Dados atuais
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
cum_train <- subset(cum_train, cum_train$INICIO_SINTOMAS > as.Date("2020-02-01", format = "%Y-%m-%d") &
		    	cum_train$INICIO_SINTOMAS < Sys.Date())



#Dados totais = atuiais + preditos
predic_base$DADOS <- "Preditos"
train_base$DADOS <- "Atuais"
test_base$DADOS <- "Atuais"
predic_base$INICIO_SINTOMAS <- as.Date(predic_base$INICIO_SINTOMAS, origin = "1970-01-01")
train_base$INICIO_SINTOMAS <- as.Date(train_base$INICIO_SINTOMAS, origin = "1970-01-01")
test_base$INICIO_SINTOMAS <- as.Date(test_base$INICIO_SINTOMAS, origin = "1970-01-01")
base_final <- rbind(predic_base, train_base) %>% as.data.frame()
base_final <- rbind(base_final, test_base[,names(test_base) != "PREDICAO"]) %>% as.data.frame()
base_final <- merge(base_final, covid_id, by = c("ID", "INICIO_SINTOMAS"), all = T)
cum_base <- subset(base_final, base_final$RESULTADO == "confirmado")
cum_base$NUMERO <- 1
cum_base$INICIO_SINTOMAS <- as.Date(cum_base$INICIO_SINTOMAS, format = "%Y-%m-%d")
cum_base <- cum_base %>%
	group_by(INICIO_SINTOMAS) %>%
	summarise(CASOS = sum(NUMERO, na.rm = T))
cum_base$CUM_CASOS <- cumsum(cum_base$CASOS) 
cum_base$DADOS <- "Totais"
cum_base <- subset(cum_base, cum_base$INICIO_SINTOMAS > as.Date("2020-02-01", format = "%Y-%m-%d") &
		    	cum_base$INICIO_SINTOMAS < Sys.Date())

cum_base <- rbind(cum_train, cum_base) %>% as.data.frame()

cum_base <- subset(cum_base, !is.na(cum_base$INICIO_SINTOMAS))


# Plotando RESULTADOs -----------------------------------------------------
ggplot(cum_base, aes(as.Date(INICIO_SINTOMAS), CUM_CASOS, group = DADOS, color = DADOS))+
	geom_line()+
	theme_bw()+
	labs(y = "Número de Casos", 
	     x = "Data dos Primeiros Sintomas")+
	theme(axis.text.x = element_text(angle = 90, hjust = 1))+
	scale_x_date(date_breaks = "1 day",   date_labels = "%d/%m/%Y")+
  	theme(axis.text.x = element_text(angle=45, hjust = 1))

ggplot(cum_base, aes(as.Date(INICIO_SINTOMAS), CASOS, group = DADOS, color = DADOS))+
	geom_line()+
	theme_bw()+
	labs(y = "Número de Casos", 
	     x = "Data dos Primeiros Sintomas")+
	theme(axis.text.x = element_text(angle = 90, hjust = 1))+
	scale_x_date(date_breaks = "1 day",   date_labels = "%d/%m/%Y")+
  	theme(axis.text.x = element_text(angle=45, hjust = 1))


# Exportando base ---------------------------------------------------------
write.csv(cum_base, "dados/covid_atuais_preditos.csv", row.names = F, fileEncoding = "UTF-8")
cum_pred <- subset(cum_base, cum_base$DADOS == "Totais")
write.csv(cum_base, "dados/covid_preditos.csv", row.names = F, fileEncoding = "UTF-8")
	


