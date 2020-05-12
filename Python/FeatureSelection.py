# -*- coding: utf-8 -*-
"""
Created on Mon Apr 20 16:48:01 2020

@author: andre-goncalves
"""

import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.inspection import permutation_importance
from imblearn.over_sampling import SMOTE

#Faz a leitura da base
df = pd.read_csv("Dados/novo_covid_ajustado.csv")

#Definindo x, y
features = df.columns.difference(['RESULTADO'])
x = df[features]
y = df['RESULTADO']

#Separa base treinamento e teste
xTreino, xTeste, yTreino, yTeste = train_test_split(x, y, random_state=1986)

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

#Classificador Randon Forest
classifierRF = RandomForestClassifier(random_state=1986, criterion='gini', max_depth=10, n_estimators=50, n_jobs=-1)

#Treina com todos registros
classifierRF.fit(xTreino, yTreino) 

#Feature Selection
print('\nFeature Selection')
featuresSelection = zip(classifierRF.feature_importances_, features)
for importance, feature in sorted(featuresSelection, reverse=True)[:30]:
    print('%s: %f%%' % (feature, importance*100))

#Permutation Importance
print('\nPermutation Importance')
pi = permutation_importance(classifierRF, xTreino, yTreino, n_repeats=10, random_state=1986, n_jobs=4)
for i in pi.importances_mean.argsort()[::-1][:30]:
    print('%s: %.2f' % (features[i], pi.importances_mean[i]*1000))