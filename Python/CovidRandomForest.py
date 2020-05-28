"""
Created on Mon Apr 20 16:48:01 2020

@author: andre-goncalves
@version:
    Implementação Validação Cruzada Aninhada
    Retirada do Permutation Selection
"""

import pandas as pd
import numpy as np
from imblearn.over_sampling import SMOTE, BorderlineSMOTE
from imblearn.combine import SMOTEENN, SMOTETomek
from imblearn.over_sampling import RandomOverSampler
from sklearn.preprocessing import MinMaxScaler
from category_encoders import OneHotEncoder
from sklearn.impute import SimpleImputer

from sklearn.model_selection import train_test_split
from sklearn.model_selection import StratifiedKFold

from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_selection import RFECV
from sklearn.model_selection import GridSearchCV, RandomizedSearchCV

from sklearn.metrics import confusion_matrix
from sklearn.metrics import classification_report

#Faz a leitura da base
df = pd.read_csv("Dados/novo_covid_ajustado_semsintomas.csv")
df['RESULTADO'].value_counts()

#Separa a base que está aguardando resultado para predição
dfFuturo = df.query('RESULTADO == 2')
dfFuturo.shape

#Deixa na base de processamento apenas os registros com resultado
df = df.query('RESULTADO == 0 or RESULTADO == 1')
df.shape

#Definindo x, y
features = df.columns.difference(['RESULTADO'])
x = df[features]
y = df['RESULTADO']

#Normalização da base
normalizador = MinMaxScaler(feature_range=(0, 1))
normalizador.fit(x)
x.values[:] = normalizador.transform(x)

#Definição do ramdom_state
ramdomState=14659

#Separa base treinamento e teste
xTreino, xTeste, yTreino, yTeste = train_test_split(x, y, train_size=0.7, stratify=y, shuffle=True, random_state=ramdomState)

#Balanceamento
treino = xTreino.join(yTreino)
qtdeDescartados = treino['RESULTADO'].value_counts()[0]
qtdeConfirmados = treino['RESULTADO'].value_counts()[1]
dfDescartados = treino[treino['RESULTADO'] == 0] #Separa a base de descartados
dfConfirmados = treino[treino['RESULTADO'] == 1] #Separa a base de confirmados

#Under sampling
#print('\nUnder Sampling')
#dfDescartadosUnder = dfDescartados.sample(qtdeConfirmados)
#dfUnder = pd.concat([dfDescartadosUnder, dfConfirmados], axis=0)
#xTreino = dfUnder[features].values
#yTreino = dfUnder['RESULTADO'].values

#Over sampling
#print('\nOver Sampling')
#dfConfirmadosOver = dfConfirmados.sample(qtdeDescartados, replace=True, random_state=ramdomState)
#dfOver = pd.concat([dfDescartados, dfConfirmadosOver], axis=0)
#xTreino = dfOver[features].values
#yTreino = dfOver['RESULTADO'].values

#Random Over sampling
#print('\nRandom Over Sampling')
#ros = RandomOverSampler(random_state=ramdomState)
#xTreino, yTreino = ros.fit_resample(treino[features], treino['RESULTADO'])
#xTreino = xTreino.values
#yTreino = yTreino.values

#Smote sampling
#print('\nSmote Sampling')
#smote = SMOTE(random_state=ramdomState)
#xTreino, yTreino = smote.fit_resample(treino[features], treino['RESULTADO'])
#xTreino = xTreino.values
#yTreino = yTreino.values

#Borderline Smote sampling
print('\nBorderline Smote Sampling')
borderlineSmote = BorderlineSMOTE(random_state=ramdomState)
xTreino, yTreino = borderlineSmote.fit_resample(treino[features], treino['RESULTADO'])
xTreino = xTreino.values
yTreino = yTreino.values

#SmoteEEN sampling
#print('\nSMOTEENN Sampling')
#smoteENN = SMOTEENN(random_state=ramdomState)
#xTreino, yTreino = smoteENN.fit_resample(treino[features], treino['RESULTADO'])
#xTreino = xTreino.values
#yTreino = yTreino.values

#SmoteTomek sampling
#print('\nSmoteTomek Sampling')
#smoteTomek = SMOTETomek(random_state=ramdomState)
#xTreino, yTreino = smoteTomek.fit_resample(treino[features], treino['RESULTADO'])
#xTreino = xTreino.values
#yTreino = yTreino.values

#K-fold
print('\n========== VALIDAÇÃO CRUZADA ANINHADA ==========')
#Define o score
score = 'recall'

#Validação cruzada com embaralhamento
outerCV = StratifiedKFold(n_splits=5, shuffle=True, random_state=1)
innerCV = StratifiedKFold(n_splits=5, shuffle=True, random_state=1)

paramGrid = {
        'estimator__criterion': ['entropy', 'gini'],
        'estimator__n_estimators': [3, 25, 50, 100],
        'estimator__max_depth': [None, 3, 5],
        'estimator__min_samples_split': [2, 5],
        'estimator__min_samples_leaf': [1, 3, 5],
        'estimator__min_weight_fraction_leaf': [0, 0.5],
        'estimator__max_features': ['auto', 0.1, 0.2, 0.5],
        'estimator__bootstrap': [False, True],
        }

arrayGridSearch = []
arrayMetrica = []

arrayYReal = []
arrayYPrediction = []
arrayConfusion = np.array([[0, 0], [0, 0]])

cv_iter = outerCV.split(xTreino, yTreino)
for treino, teste in cv_iter:
    #Feature Selection nas mesmas condições de classificador e folders
    rfecv = RFECV(estimator=RandomForestClassifier(class_weight="balanced", random_state=ramdomState), step=0.05, min_features_to_select=2, cv=innerCV, scoring=score)
    
    gridSearch = RandomizedSearchCV(rfecv, param_distributions=paramGrid, cv=innerCV, scoring=score, n_iter=20, n_jobs=-1)
    gridSearch.fit(xTreino[treino], yTreino[treino])
    arrayGridSearch = np.append(arrayGridSearch, gridSearch)

    classifier = gridSearch.best_estimator_
    arrayMetrica = np.append(arrayMetrica, gridSearch.best_score_)

    #Etapa de predição
    yPrediction = classifier.predict(xTreino[teste])

    arrayYReal = np.append(arrayYReal, yTreino[teste])
    arrayYPrediction = np.append(arrayYPrediction, yPrediction)    
    arrayConfusion += confusion_matrix(yTreino[teste], yPrediction, labels=[0, 1])

#Escolhe o melhor classificador
melhorGridSearch = arrayGridSearch[np.argsort(arrayMetrica)[::-1][0]]

print('\nClassificador:', melhorGridSearch.best_estimator_.estimator.__class__)
print('Score:', score)
print('\nMelhor parametrização: %s' % melhorGridSearch.best_params_)
print('Melhor pontuação: %.2f' % melhorGridSearch.best_score_)

print("\n", pd.DataFrame(arrayConfusion, index=['real:descartado', 'real:confirmado'], 
                         columns=['pred:descartado', 'pred:confirmado']))

print("\n(TN, FP, FN, TP): %s \n" % arrayConfusion.ravel())
print(classification_report(arrayYReal, arrayYPrediction, labels=[0, 1]))

#Validação 
print('\n========== TESTE ==========')
estimator = melhorGridSearch.best_estimator_

#Faz o corte nas features, deixando apenas as mais importantes
indFeatures = estimator.get_support(1)
xTreino = xTreino[:, indFeatures]
xTeste = xTeste[xTeste.columns[indFeatures]]

#Etapa de treinamento
classifier = estimator.estimator
classifier.fit(xTreino, yTreino)

#Etapa de predição
yPrediction = classifier.predict(xTeste)

cm = confusion_matrix(yTeste, yPrediction, labels=[0, 1])

print(pd.DataFrame(cm, index=['real:descartado', 'real:confirmado'], 
                   columns=['pred:descartado', 'pred:confirmado']))

print("\n(TN, FP, FN, TP): %s \n" % cm.ravel())

print(classification_report(yTeste, yPrediction, labels=[0, 1]))

print('\n========== RESUMO RFECV ==========')
#Mostra a quantidade de features selecionadas
print('Qtde features selecionadas: ', estimator.n_features_)

##Exibe as features selecionadas
for contador in range(len(indFeatures)):
    print(xTeste.columns[contador], np.absolute(classifier.feature_importances_[contador]))