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


# Seleção das variáveis ------------------------------------------------
# covid <- covid %>% 
# 	dplyr::select(ID,
# 		      RESULTADO,
# 		      TERRITORIO,
# 		      SEXO,
# 		      MUNICIPIO,
# 		      SUBTERRITORIO,
# 		      TRIAGEM,
# 		      RACA_COR,
# 		      INICIO_SINTOMAS,
# 		      TX_INFECTADOS_TERRITORIO,
# 		      MEDIA_TRANSITO_LAG13,
# 		      PROP_MASC,
# 		      PERC_60_MAIS,
# 		      PERC_NAO_BRANCA,
# 		      PERC_ESC_10_MENOS)



# Transformando base ------------------------------------------------------
covid$ID <- as.character(covid$ID)
covid$RESULTADO <- as.factor(covid$RESULTADO)
covid$TERRITORIO <- as.factor(covid$TERRITORIO)
covid$SEXO <- as.factor(covid$SEXO)
covid$MUNICIPIO <- as.factor(covid$MUNICIPIO)
covid$SUBTERRITORIO <- as.factor(covid$SUBTERRITORIO)
covid$TRIAGEM <- as.factor(covid$TRIAGEM)
covid$RACA_COR <- as.factor(covid$RACA_COR)
covid$INICIO_SINTOMAS <- as.numeric(covid$INICIO_SINTOMAS)#Transformando em número, pois o learner do mlr não trabalha com data
covid$TX_INFECTADOS_TERRITORIO <- as.numeric(covid$TX_INFECTADOS_TERRITORIO)
covid$MEDIA_TRANSITO_LAG13 <- as.numeric(covid$MEDIA_TRANSITO_LAG13)
covid$PROP_MASC <- as.numeric(covid$PROP_MASC)
covid$PERC_60_MAIS <- as.numeric(covid$PERC_60_MAIS)
covid$PERC_NAO_BRANCA <- as.numeric(covid$PERC_NAO_BRANCA)
covid$PERC_ESC_10_MENOS <- as.numeric(covid$PERC_ESC_10_MENOS)
covid$FAIXA_ETARIA <- as.factor(covid$FAIXA_ETARIA)


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

# Realizando benchmarking de algoritmos de classificação ------------------
parallelStartSocket(4)

mod1_task <- makeClassifTask(data = train_base[,!(names(train_base) %in% c("ID"))], target = "RESULTADO")#Diversos algoritmos não trabalham com variáveis com muitas categorias e com datas por isso Início foi retirado
confirmados <- sum(train_base$RESULTADO == "confirmado")
descartados <- sum(train_base$RESULTADO == "descartado")
## Como há muitos mais casos descartados que confirmados utilizou-se smote para balanceamento
mod1_task_smote <- smote(mod1_task, rate = descartados/confirmados, nn = 5)

## Três algorítmos foram comparados gbm, rf e svm
## Escolheu-se a acurárcia balanceada como métrica de maximização
# Filtragem de features com  tuning no inner resampling loop
## Filtro das features
#listFilterMethods()
gbm <- makeFilterWrapper(learner = 'classif.gbm', fw.method = "FSelector_gain.ratio") 
rf <- makeFilterWrapper(learner = 'classif.ranger', fw.method = "FSelector_gain.ratio")
svm <- makeFilterWrapper(learner = 'classif.ksvm', fw.method = "FSelector_gain.ratio")

## Paramentros a serem tunados
ps_gbm <- makeParamSet(      
		makeIntegerLearnerParam(id = "n.trees", lower = 1L, upper = 2000L),
	        makeIntegerLearnerParam(id = "interaction.depth", lower = 1L, upper = 5L),
	        makeIntegerLearnerParam(id = "n.minobsinnode", lower = 1L, upper = 3L),
	        makeNumericLearnerParam(id = "shrinkage", lower = 0, upper = 0.5),
	        makeNumericLearnerParam(id = "bag.fraction", lower = 0, upper = 1),
		makeDiscreteParam("fw.perc", values = seq(0.2, 1, 0.05))
	)
ps_rf <- makeParamSet( 
	      makeIntegerLearnerParam(id = "num.trees", lower = 1L, upper = 2000L),
	      makeIntegerLearnerParam(id = "mtry", lower = 1L,upper = 10L),
	      makeIntegerLearnerParam(id = "min.node.size", lower = 1L, upper = 10L),
	      makeNumericLearnerParam(id = "sample.fraction", lower = 0L, upper = 1L),
	      makeDiscreteParam("fw.perc", values = seq(0.2, 1, 0.05))
	 )
ps_svm <- makeParamSet(
		  makeNumericParam("C", lower = -10, upper = 10, trafo = function(x) 10^x),
  		  makeNumericParam("sigma", lower = -10, upper = 10, trafo = function(x) 10^x),
		  makeDiscreteParam("fw.perc", values = seq(0.2, 1, 0.05))
  	  )
## Estratégia de hyperparametrização - grid search
ctrl <- makeTuneControlRandom(maxit = 100L)
## Estratégia de ressampling do inner loop - validação cruzada com estratificação dos resultados balanceados entre as folds
inner <- makeResampleDesc("CV", iter = 5, stratify = TRUE)
#measures https://mlr.mlr-org.com/articles/tutorial/measures.html
## learnes ajustados para filtragem e tuning
lrn_gbm <- makeTuneWrapper(learner = gbm, resampling = inner, par.set = ps_gbm, control = ctrl,
  show.info = FALSE, measures = bac)
lrn_rf <- makeTuneWrapper(learner = rf, resampling = inner, par.set = ps_rf, control = ctrl,
  show.info = FALSE, measures = bac)
lrn_svm <- makeTuneWrapper(learner = svm, resampling = inner, par.set = ps_svm, control = ctrl,
  show.info = FALSE, measures = bac)
# Lista de Learners
lrns <- list(lrn_gbm, lrn_rf, lrn_svm)

## Estratégia de ressampling do outer loop - validação cruzada com estratificação dos resultados balanceados entre as folds
outer <- makeResampleDesc("CV", iter = 5, stratify = TRUE)

## Rodando o benchmark
res <- benchmark(tasks = mod1_task_smote, learners = lrns, resampling = outer, show.info = T, models = TRUE, measures = bac)
getBMRAggrPerformances(res, as.df = T)
getBMRPerformances(res, as.df = T)
plotBMRBoxplots(res, measure = bac)

## Tunning com algoritmo selecionado na base de treino
set.seed(1)
tune_train <- tuneParams(learner = rf, task = mod1_task_smote, resampling = outer, show.info = FALSE, 
		       measure = bac, par.set = ps_rf, control = ctrl)

## Resultado na base de treino
lrn_train <- setHyperPars(makeFilterWrapper(learner = "classif.ranger", 
					   fw.method = "FSelector_gain.ratio"), par.vals = tune_train$x)

mod_train <- resample(learner = lrn_train, task = mod1_task_smote, resampling = outer, models = TRUE, show.info = FALSE, 
		       measure = bac)
confusionMatrix(data = mod_train$pred$data$response, reference = mod_train$pred$data$truth)
confusionMatrix(data = mod_train$pred$data$response, reference = mod_train$pred$data$truth, mode = "prec_recall")

## Resultado na base de teste - utilizando o mesmo threashold para feature selection e os hyperparamentros tunados na base de traino
set.seed(1)
mod2_task <- makeClassifTask(data = test_base[,!(names(test_base) %in% c("ID"))], target = "RESULTADO")
confirmados <- sum(test_base$RESULTADO == "confirmado")
descartados <- sum(test_base$RESULTADO == "descartado")
mod2_task_smote <- smote(mod2_task, rate = descartados/confirmados, nn = 5)
mod_test <- train(lrn_train, mod2_task)
test_base$PREDICAO <- predict(mod_test, newdata = test_base[,names(test_base) != c("ID", "RESULTADO")])$data[,1]
confusionMatrix(data = test_base$PREDICAO, reference = test_base$RESULTADO)
confusionMatrix(data = test_base$PREDICAO, reference = test_base$RESULTADO, mode = "prec_recall")

## Predição dos dados faltantes
predic_base$RESULTADO <- predict(mod_test, newdata = predic_base[,names(predic_base) != "ID"])$data[,1]
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
	


