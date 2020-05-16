# -*- coding: utf-8 -*-
"""
Created on Mon Apr 20 16:48:01 2020

@author: andre-goncalves
"""

import pandas as pd
import numpy as np
from imblearn.over_sampling import SMOTE
from sklearn.svm import SVC
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier, AdaBoostClassifier
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

#Define o score
score = 'average_precision'

#Grid Search
paramGrid = [
    {
         'estimator': [RandomForestClassifier(class_weight='balanced', random_state=1986)],
         'estimator__criterion': ['entropy', 'gini'],
         'estimator__n_estimators': [3, 10],
         #'estimator__max_depth': [None, 3, 5],
         #'estimator__bootstrap': [False, True],
    },
    {
         'estimator': [AdaBoostClassifier(random_state=1986)],
         'estimator__n_estimators': [3, 10],
    },
    {
         'estimator': [GradientBoostingClassifier(random_state=1986)],
         'estimator__criterion': ['friedman_mse', 'mse', 'mae'], 
         'estimator__n_estimators': [3, 10],
         #'estimator__max_depth': [None, 3, 5],
         #'estimator__loss': ['deviance', 'exponential'],
    },
    {
        'estimator': [SVC(kernel="linear", C=0.025, random_state=1986)],
    }]

#Feature Selection nas mesmas condições de classificador e folders
rfecv = RFECV(estimator=None, step=1, cv=kfold, scoring=score)

#Faz o processamento de treinamento com Tuning e Feature Selection
gridSearch = GridSearchCV(rfecv, paramGrid, scoring=score, n_jobs=3, verbose=25)
gridSearch.fit(xTreino, yTreino)

classifier = gridSearch.best_estimator_
indFeatures = np.where(classifier.support_ == True)[0]

print('\nMelhor estimador: %s' % gridSearch.best_estimator_)
print('Melhor parametrização: %s' % gridSearch.best_params_)
print('Melhor pontuação: %.2f' % gridSearch.best_score_)
print('Qtde features selecionadas: ', len(indFeatures))

#K-fold
print('========== VALIDAÇÃO MÉTODO K-FOLD ==========')
xTreino = xTreino[:, indFeatures]

arrayYReal = []
arrayYPrediction = []
arrayAcuracia = []
arrayConfusion = np.array([[0, 0], [0, 0]])
cv_iter = kfold.split(xTreino, yTreino)
for treino, teste in cv_iter:
    #Etapa de treinamento
    classifier.fit(xTreino[treino], yTreino[treino])
    
    #Etapa de predição
    yPrediction = classifier.predict(xTreino[teste])
    
    arrayYReal = np.append(arrayYReal, yTreino[teste])
    arrayYPrediction = np.append(arrayYPrediction, yPrediction)
    
    arrayConfusion += confusion_matrix(yTreino[teste], yPrediction, labels=[0, 1])
    arrayAcuracia.append(accuracy_score(yTreino[teste], yPrediction))

print(pd.DataFrame(arrayConfusion, index=['real:descartado', 'real:confirmado'], 
                   columns=['pred:descartado', 'pred:confirmado']))

print("\n(TN, FP, FN, TP): %s \n" % arrayConfusion.ravel())
print(classification_report(arrayYReal, arrayYPrediction, labels=[0, 1]))

#Validação 
print('\n========== TESTE ==========')
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