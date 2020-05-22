# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)
options(java.parameters = "-Xmx16g") #Evitar que o java tenha problemas de memória

# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(caret)
library(mlr)
library(forecast)
library(doParallel)
library(parallelMap)
library(tigerstats)

# Importanto bases ---------------------------------------------------------------
## Dados de suspeitos 
covid <- read_csv("dados/covid_ajustado.csv")

# Transformando base ------------------------------------------------------
covid$ID <- as.factor(covid$ID)
covid$PROF_SAUDE <-  as.factor(covid$PROF_SAUDE)
covid$DOR_GARGANTA <- as.factor(covid$DOR_GARGANTA)
covid$DISPINEIA <- as.factor(covid$DISPINEIA)
covid$FEBRE <- as.factor(covid$FEBRE)
covid$TOSSE <- as.factor(covid$TOSSE)
covid$TIPO_EXAME <-as.factor(covid$TIPO_EXAME)
covid$RESULTADO <- as.factor(covid$RESULTADO)
covid$TERRITORIO <-as.factor(covid$TERRITORIO)
covid$SEXO <-as.factor(covid$SEXO)
covid$SUBTERRITORIO <-as.factor(covid$SUBTERRITORIO)
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

#Analisando distribuição das variáveis entre a base de treino_teste e a base de predição
#Variáveis com muita diferença serão excluídas, pois não hã suporte na base de predição
train_test_base$TIPO <- "train_test"
predic_base$TIPO <- "predict"

base_comp <- rbind(train_test_base, predic_base) %>% as.data.frame()
rowPerc(table(base_comp$TIPO, base_comp$TERRITORIO))
rowPerc(table(base_comp$TIPO, base_comp$PROF_SAUDE)) # Retirar por estar muito desbalanceado entre as duas bases
rowPerc(table(base_comp$TIPO, base_comp$SEXO))
rowPerc(table(base_comp$TIPO, base_comp$DATA_NOTIFICACAO))
rowPerc(table(base_comp$TIPO, base_comp$DOR_GARGANTA)) # Retirar por estar muito desbalanceado entre as duas bases
rowPerc(table(base_comp$TIPO, base_comp$DISPINEIA)) # Retirar por estar muito desbalanceado entre as duas bases
rowPerc(table(base_comp$TIPO, base_comp$FEBRE)) # Retirar por estar muito desbalanceado entre as duas bases
rowPerc(table(base_comp$TIPO, base_comp$TOSSE)) # Retirar por estar muito desbalanceado entre as duas bases
rowPerc(table(base_comp$TIPO, base_comp$RESULTADO))
rowPerc(table(base_comp$TIPO, base_comp$TIPO_EXAME)) # Retirar por estar muito desbalanceado entre as duas bases
rowPerc(table(base_comp$TIPO, base_comp$IDADE))
rowPerc(table(base_comp$TIPO, base_comp$FAIXA_ETARIA))
rowPerc(table(base_comp$TIPO, base_comp$TRIAGEM))
rowPerc(table(base_comp$TIPO, base_comp$INFECTADOS_TERRITORIO))
rowPerc(table(base_comp$TIPO, base_comp$TX_INFECTADOS_TERRITORIO))


train_test_base$TIPO <- NULL
train_test_base$PROF_SAUDE <- NULL
train_test_base$DOR_GARGANTA <- NULL
train_test_base$DISPINEIA <- NULL
train_test_base$FEBRE <- NULL
train_test_base$TOSSE <- NULL
train_test_base$TIPO_EXAME <- NULL

predic_base$TIPO <- NULL
predic_base$PROF_SAUDE <- NULL
predic_base$DOR_GARGANTA <- NULL
predic_base$DISPINEIA <- NULL
predic_base$FEBRE <- NULL
predic_base$TOSSE <- NULL
predic_base$TIPO_EXAME <- NULL


# Separando a base de traino_validacao da base de teste
indice <- createDataPartition(train_test_base$RESULTADO, p = 0.7, list=FALSE)

#Base de treino
train_base <- train_test_base[indice,]
summary(train_base)

#Base de teste
test_base <- train_test_base[-indice,]
summary(test_base)



# Realizando benchmarking de algoritmos de classificação ------------------
mod1_task <- makeClassifTask(data = train_base[,!(names(train_base) %in% c("ID"))], target = "RESULTADO")#Diversos algoritmos não trabalham com variáveis com muitas categorias e com datas por isso Início foi retirado
confirmados <- sum(train_base$RESULTADO == "confirmado")
descartados <- sum(train_base$RESULTADO == "descartado")
## Como há muitos mais casos descartados que confirmados utilizou-se smote para balanceamento
mod1_task_smote <- smote(mod1_task, rate = descartados/confirmados, nn = 5)
mod1_task_under <- undersample(mod1_task, rate = confirmados/descartados)

## Três algorítmos foram comparados gbm, rf e svm
## Escolheu-se a acurárcia balanceada como métrica de maximização
# Filtragem de features com  tuning no inner resampling loop
## Filtro das features
#listFilterMethods()
rf <- makeFilterWrapper(learner = 'classif.ranger', fw.method = "ranger_permutation")
ps_rf <- makeParamSet( 
	      makeIntegerLearnerParam(id = "num.trees", lower = 1L, upper = 2000L),
	      makeIntegerLearnerParam(id = "mtry", lower = 1L,upper = 10L),
	      makeIntegerLearnerParam(id = "min.node.size", lower = 1L, upper = 10L),
	      makeNumericLearnerParam(id = "sample.fraction", lower = 0L, upper = 1L),
	      makeDiscreteParam("fw.perc", values = seq(0.2, 1, 0.05))
	 )
## Estratégia de hyperparametrização - grid search
ctrl <- makeTuneControlRandom(maxit = 10L)
## Estratégia de ressampling do inner loop - validação cruzada com estratificação dos resultados balanceados entre as folds
folds <- 5
inner <- makeResampleDesc("CV", iter = folds, stratify = TRUE)
#measures https://mlr.mlr-org.com/articles/tutorial/measures.html
## learner ajustados para filtragem e tuning
lrn_rf <- makeTuneWrapper(learner = rf, resampling = inner, par.set = ps_rf, control = ctrl,
  show.info = T)

## Estratégia de ressampling do outer loop - validação cruzada com estratificação dos resultados balanceados entre as folds
outer <- makeResampleDesc("CV", iter = folds, predict = "both", stratify = TRUE)

#Iniciando paralelização
parallelStartSocket(4)

## Tunning com algoritmo selecionado 
set.seed(1)
mod_train <- mlr::resample(learner = lrn_rf, task = mod1_task_under, resampling = outer, models = TRUE, show.info = FALSE, 
		      measure = fpr, extract = getTuneResult)
predicoes <- mod_train$pred$data
predicoes_treino <- subset(predicoes, predicoes$set == "train")
predicoes_validacao <- subset(predicoes, predicoes$set == "test")

#Predições treino
confusionMatrix(data = predicoes_treino$response, reference = predicoes_treino$truth)
#confusionMatrix(data = mod_train$pred$data$response, reference = mod_train$pred$data$truth, mode = "prec_recall")

#Predições validação
confusionMatrix(data = predicoes_validacao$response, reference = predicoes_validacao$truth)
#confusionMatrix(data = predicoes_validacao$response, reference = predicoes_validacao$truth, mode = "prec_recall")


## Exração de hiperparâmentros
mmce_resultados <- list()
for(i in 1:folds){ 
	mmce_resultados[[i]] <- mod_train$extract[[i]]$y 
}

mmce_resultados <- do.call(rbind, mmce_resultados) %>% as.data.frame()
mmce_resultados$iteracao <- c(1:nrow(mmce_resultados))
iteracao <- mmce_resultados[which(mmce_resultados$mmce.test.mean == min(mmce_resultados$mmce.test.mean)) , 2]

tunned_model <- mod_train$models[[iteracao]] #modelo tunado

tuned_par <- mod_train$extract[[iteracao]]$x #para ver hiperparâmetros

## Predição dos dados faltantes
mod2_task <- makeClassifTask(data = test_base[,!(names(test_base) %in% c("ID"))], target = "RESULTADO")
set.seed(1)
test_base$PREDITO <- predict(tunned_model, mod2_task)$data[,3]
predic_base$RESULTADO <- as.character(predic_base$RESULTADO)
confusionMatrix(data = test_base$PREDITO, reference = test_base$RESULTADO)
#confusionMatrix(data = test_base$PREDITO, reference = test_base$RESULTADO, mode = "prec_recall")


## Predição dos dados faltantes
mod3_task <- makeClassifTask(data = predic_base[,!(names(predic_base) %in% c("ID"))], target = "RESULTADO")
set.seed(1)
predic_base$RESULTADO <- predict(tunned_model, mod3_task)$data[,3]
predic_base$RESULTADO <- as.character(predic_base$RESULTADO)

#Parando paralelização
parallelStop()

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
	


