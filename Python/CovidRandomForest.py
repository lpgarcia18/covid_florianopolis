# -*- coding: utf-8 -*-
"""
Created on Mon Apr 20 16:48:01 2020

@author: andre-goncalves
"""

import pandas as pd
import numpy as np
from matplotlib import pyplot as plt
from imblearn.over_sampling import SMOTE
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_selection import RFECV
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import train_test_split
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import confusion_matrix
from sklearn.metrics import classification_report
from sklearn.metrics import accuracy_score

#Faz a leitura da base
df = pd.read_csv("Dados/novo_covid_ajustado.csv")

#Definindo x, y
features = df.columns.difference(['RESULTADO'])
x = df[features]
y = df['RESULTADO']

#Separa base treinamento e teste
xTreino, xTeste, yTreino, yTeste = train_test_split(x, y, stratify=y, train_size=0.7, random_state=1986)

#Balanceamento
treino = xTreino.join(yTreino)
qtdeDescartados = treino['RESULTADO'].value_counts()[0]
qtdeConfirmados = treino['RESULTADO'].value_counts()[1]
dfDescartados = treino[treino['RESULTADO'] == 0] #Separa a base de descartados
dfConfirmados = treino[treino['RESULTADO'] == 1] #Separa a base de confirmados

#Under sampling
print('\nUnder Sampling')
dfDescartadosUnder = dfDescartados.sample(qtdeConfirmados)
dfUnder = pd.concat([dfDescartadosUnder, dfConfirmados], axis=0)
xTreino = dfUnder[features].values
yTreino = dfUnder['RESULTADO'].values

#Over sampling
print('\nOver Sampling')
dfConfirmadosOver = dfConfirmados.sample(qtdeDescartados, replace=True)
dfOver = pd.concat([dfDescartados, dfConfirmadosOver], axis=0)
xTreino = dfOver[features].values
yTreino = dfOver['RESULTADO'].values

#Smote sampling
print('\nSmote Sampling')
oversample = SMOTE()
xTreino, yTreino = oversample.fit_resample(treino[features], treino['RESULTADO'])
xTreino = xTreino.values
yTreino = yTreino.values

#K-fold
print('\n========== VALIDAÇÃO MÉTODO K-FOLD ==========')
arrayYReal = []
arrayYPrediction = []
arrayAcuracia = []
arrayConfusion = np.array([[0, 0], [0, 0]])

kfold = StratifiedKFold(n_splits=5, shuffle=True, random_state=1986)

#Grid Search
paramGrid = {
        'estimator__n_estimators': [3, 10, 25],
        'estimator__criterion': ['entropy', 'gini'],
        'estimator__max_depth': ['none', 3, 5],
        #'estimator__min_samples_split': [2, 5],
        #'estimator__min_samples_leaf': [1, 3, 5],
        #'#estimator__min_weight_fraction_leaf': [0, 2, 5],
        #'estimator__max_features': ['auto', 0.1, 0.2, 0.5],
        #'estimator__bootstrap': [False, True]
        }

classifier = RandomForestClassifier(class_weight="balanced", random_state=1986)
rfecv = RFECV(estimator=classifier, step=1, cv=kfold, scoring='balanced_accuracy')
gridSearch = GridSearchCV(rfecv, paramGrid, scoring='balanced_accuracy', n_jobs=3)
gridSearch.fit(xTreino, yTreino)

classifier = gridSearch.best_estimator_
indFeatures = np.where(classifier.support_ == True)[0]

print('Melhor parametrização: %s' % gridSearch.best_params_)
print('Melhor pontuação: %.2f' % gridSearch.best_score_)
print('Qtde features selecionadas: ', len(indFeatures))

#Validação 
print('\n========== TESTE ==========')
xTreino = xTreino[:, indFeatures]
xTeste = xTeste[xTeste.columns[indFeatures]]

#Etapa de treinamento
classifier.fit(xTreino, yTreino)

#Etapa de predição
yPrediction = classifier.predict(xTeste)

cm = confusion_matrix(yTeste, yPrediction, labels=[0, 1])

print(pd.DataFrame(cm, index=['real:descartado', 'real:confirmado'], 
                   columns=['pred:descartado', 'pred:confirmado']))

print("\n(TN, FP, FN, TP): %s \n" % cm.ravel())

print(classification_report(yTeste, yPrediction, labels=[0, 1]))