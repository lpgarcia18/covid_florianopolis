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
covid <- read_csv("dados/covid_ajustado.csv")

# Transformando base ------------------------------------------------------
covid[,!(names(covid) %in% c("ID"))] <- sapply(covid[,!(names(covid) %in% c("ID"))], as.factor) %>% as.data.frame()

# Formação das bases de treino, teste e predição --------------------------
train_test_base <- subset(covid, covid$Resultado == "descartado" |
	       	      covid$Resultado == "confirmado")
train_test_base$Resultado <- factor(train_test_base$Resultado, levels = c("confirmado", "descartado"))
set.seed(1)
indice <- createDataPartition(train_test_base$Resultado, p = 0.7, list=FALSE)

#Base de treino
train_base <- train_test_base[indice,]
summary(train_base)

#Base de teste
test_base <- train_test_base[-indice,]
summary(test_base)

#Base para predição
predic_base <- subset(covid, !(covid$Resultado %in% c("confirmado", "descartado")))
predic_base$Resultado <- NULL
summary(predic_base)


# Realizando benchmarking de algoritmos de classificação ------------------
parallelStartSocket(4)

mod1_task <- makeClassifTask(data = train_base[,!(names(train_base) %in% c("ID", "Inicio"))], target = "Resultado")#Diversos algoritmos não trabalham com variáveis com muitas categorias e com datas por isso Início foi retirado
confirmados <- sum(train_base$Resultado == "confirmado")
descartados <- sum(train_base$Resultado == "descartado")

## Como há muitos mais casos descartados que confirmados, oversampling, undersampling e smote foram testados
mod1_task_over <- oversample(mod1_task, rate = descartados/confirmados) 
mod1_task_under <- undersample(mod1_task, rate = confirmados/descartados)
mod1_task_smote <- smote(mod1_task, rate = descartados/confirmados, nn = 5)

lrns_type <- c('classif.adaboostm1',
		#'classif.bartMachine',
		'classif.boosting',
		'classif.gamboost',
		'classif.gbm',
		'classif.glmboost',
		'classif.glmnet',
		'classif.h2o.deeplearning',
		'classif.h2o.gbm',
		'classif.h2o.glm',
		'classif.h2o.randomForest',
		'classif.naiveBayes',
		'classif.randomForest',
		'classif.randomForestSRC',
		'classif.ranger',
		'classif.svm')


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

# Predizendo o resultado dos testes ---------------------------------------
## Escolheu-se o algoritmo e o learner que produziram a maior mediana de acurácia e possuiam a menor variação 
## na análise de benchmarking

set.seed(1)
result_train <- resample(learner = 'classif.randomForestSRC', task = mod1_task_smote, resampling = cross_val, show.info = FALSE)
confusionMatrix(data = result_train$pred$data$response, reference = result_train$pred$data$truth)
confusionMatrix(data = result_train$pred$data$response, reference = result_train$pred$data$truth, mode = "prec_recall")

set.seed(1)
mod_pred <- mlr::train(mod1_task_smote, learner = 'classif.randomForestSRC')
test_base$PREDICAO <- predict(mod_pred, newdata = test_base[, names(test_base) != "Resultado"])$data[,1]
confusionMatrix(data = test_base$PREDICAO, reference = test_base$Resultado)
confusionMatrix(data = test_base$PREDICAO, reference = test_base$Resultado, mode = "prec_recall")

predic_base$Resultado <- predict(mod_pred, newdata = predic_base[,names(predic_base) != "ID"])$data[,1]
predic_base$Resultado <- as.character(predic_base$Resultado)

parallelStop()



# Plotando predição -------------------------------------------------------
#Dados atuais
covid_id <- covid %>% dplyr::select(ID, Inicio)
train_test_base$Resultado <- as.character(train_test_base$Resultado)
cum_train <- merge(train_test_base, covid_id, by = c("ID", "Inicio"), all = T)
cum_train <- subset(cum_train, cum_train$Resultado == "confirmado")
cum_train$NUMERO <- 1
cum_train$Inicio <- as.Date(cum_train$Inicio, format = "%Y-%m-%d")
cum_train <- cum_train %>%
	group_by(Inicio) %>%
	summarise(CASOS = sum(NUMERO, na.rm = T))
cum_train$CUM_CASOS <- cumsum(cum_train$CASOS) 
cum_train$DADOS <- "Atuais"
cum_train <- subset(cum_train, cum_train$Inicio > as.Date("2020-02-01", format = "%Y-%m-%d") &
		    	cum_train$Inicio < Sys.Date())



#Dados totais = atuiais + preditos
predic_base$DADOS <- "Preditos"
train_base$DADOS <- "Atuais"
test_base$DADOS <- "Atuais"
base_final <- rbind(predic_base, train_base) %>% as.data.frame()
base_final <- rbind(base_final, test_base[,names(test_base) != "PREDICAO"]) %>% as.data.frame()
base_final <- merge(base_final, covid_id, by = c("ID", "Inicio"), all = T)
cum_base <- subset(base_final, base_final$Resultado == "confirmado")
cum_base$NUMERO <- 1
cum_base$Inicio <- as.Date(cum_base$Inicio, format = "%Y-%m-%d")
cum_base <- cum_base %>%
	group_by(Inicio) %>%
	summarise(CASOS = sum(NUMERO, na.rm = T))
cum_base$CUM_CASOS <- cumsum(cum_base$CASOS) 
cum_base$DADOS <- "Preditos"
cum_base <- subset(cum_base, cum_base$Inicio > as.Date("2020-02-01", format = "%Y-%m-%d") &
		    	cum_base$Inicio < Sys.Date())

cum_base <- rbind(cum_train, cum_base) %>% as.data.frame()

cum_base <- subset(cum_base, !is.na(cum_base$Inicio))

ggplot(cum_base, aes(as.Date(Inicio), CUM_CASOS, group = DADOS, color = DADOS))+
	geom_line()+
	theme_bw()+
	labs(y = "Número de Casos", 
	     x = "Data dos Primeiros Sintomas")+
	theme(axis.text.x = element_text(angle = 90, hjust = 1))+
	scale_x_date(date_breaks = "1 day",   date_labels = "%d/%m/%Y")+
  	theme(axis.text.x = element_text(angle=45, hjust = 1))

ggplot(cum_base, aes(as.Date(Inicio), CASOS, group = DADOS, color = DADOS))+
	geom_line()+
	theme_bw()+
	labs(y = "Número de Casos", 
	     x = "Data dos Primeiros Sintomas")+
	theme(axis.text.x = element_text(angle = 90, hjust = 1))+
	scale_x_date(date_breaks = "1 day",   date_labels = "%d/%m/%Y")+
  	theme(axis.text.x = element_text(angle=45, hjust = 1))

write.csv(cum_base, "dados/covid_atuais_preditos_sir.csv", row.names = F)

cum_pred <- subset(cum_base, cum_base$DADOS == "Preditos")
write.csv(cum_base, "dados/covid_preditos_sir.csv", row.names = F)
	


