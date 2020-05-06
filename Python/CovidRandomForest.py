#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 20 16:48:01 2020

@author: andre-goncalves
"""

import pandas as pd
import numpy as np
from matplotlib import pyplot as plt
from sklearn.ensemble import RandomForestClassifier
from imblearn.over_sampling import SMOTE
from sklearn.model_selection import train_test_split
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import StratifiedKFold

#Faz a leitura da base
df = pd.read_csv("Dados/novo_covid_ajustado.csv")

#Definindo x, y
features = df.columns.difference(['RESULTADO'])
x = df[features].values
y = df['RESULTADO'].values

#Balanceamento
qtdeDescartados, qtdeConfirmados = df['RESULTADO'].value_counts().values
dfDescartados = df[df['RESULTADO'] == 'descartado'] #Separa a base de descartados
dfConfirmados = df[df['RESULTADO'] == 'confirmado'] #Separa a base de confirmados

#Under sampling
print('\nUnder Sampling')
dfDescartadosUnder = dfDescartados.sample(qtdeConfirmados)
dfUnder = pd.concat([dfDescartadosUnder, dfConfirmados], axis=0)
x = dfUnder[features].values
y = dfUnder['RESULTADO'].values

#Over sampling
print('\nOver Sampling')
dfConfirmadosOver = dfConfirmados.sample(qtdeDescartados, replace=True)
dfOver = pd.concat([dfDescartados, dfConfirmadosOver], axis=0)
x = dfOver[features].values
y = dfOver['RESULTADO'].values

#Smote sampling
print('\nOver Sampling')
oversample = SMOTE()
x, y = oversample.fit_resample(x, y)

#Separa base treinamento e teste
xTreino, xTeste, yTreino, yTeste = train_test_split(x, y, random_state=1986)

#Classificador Randon Forest
classifierRF = RandomForestClassifier(random_state=1986, criterion='gini', max_depth=10, n_estimators=50, n_jobs=-1)

#Feature Selection
print('\nFeature Selection')
classifierRF.fit(x, y) #Treina com todos registros
featuresSelection = zip(classifierRF.feature_importances_, features)
for importance, feature in sorted(featuresSelection, reverse=True):
    print('%s: %f%%' % (feature, importance*100))

#Etapa de treinamento
classifierRF.fit(xTreino, yTreino)

#Etapa de predição
prediction = classifierRF.predict(xTeste)

#Estima três registros
print('\nTrês primeiros resultados')
print(prediction[0], prediction[1], prediction[3])
print(yTeste[0], yTeste[1], yTeste[3])

#Validação Simples
print('\nValidação Simples')
a = pd.DataFrame(yTeste, columns=['teste'])
b = pd.DataFrame(prediction, columns=['prediction'])
c = a.join(b)

print('Acurácia: %f%%' % (np.equal(yTeste, prediction).sum() / len(prediction)))
print('Sensibilidade: %f%%' %
      (len(c.query('prediction == "confirmado" and teste == "confirmado"')) /
      len(c.query('teste == "confirmado"'))))
print('Especificidade: %f%%' %
      (len(c.query('prediction == "descartado" and teste == "descartado"')) /
      len(c.query('teste == "descartado"'))))

#Validação cruzada
print('\nValidação Cruzada')
cv = StratifiedKFold(n_splits=5, shuffle=True, random_state=1986)

#Base Treino
scores = cross_val_score(classifierRF, xTreino, yTreino, scoring='accuracy', cv=cv)
print('Acurácia: %0.2f (+/- %0.2f)' % (scores.mean(), scores.std() * 2))

scoresDf = pd.DataFrame(scores)
print(scoresDf.describe())
scoresDf.boxplot()
plt.show()

#Base de Teste
scores = cross_val_score(classifierRF, xTeste, yTeste, scoring='accuracy', cv=cv)
print('Acurácia: %0.2f (+/- %0.2f)' % (scores.mean(), scores.std() * 2))

scoresDf = pd.DataFrame(scores)
print(scoresDf.describe())
scoresDf.boxplot()
plt.show()