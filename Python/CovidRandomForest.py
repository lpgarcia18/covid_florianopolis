#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 20 16:48:01 2020

@author: andre-goncalves
"""

import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split
from sklearn.model_selection import cross_val_score

df = pd.read_csv("Dados/covid_ajustado.csv")

#Balanceamento
qtdeDescartados, qtdeConfirmados = df['Resultado_do_teste'].value_counts().values
dfDescartados = df[df['Resultado_do_teste'] == 'descartado'] #Separa a base de descartados
dfConfirmados = df[df['Resultado_do_teste'] == 'confirmado'] #Separa a base de confirmados

#Under sampling
print('\nUnder Sampling')
dfDescartadosUnder = dfDescartados.sample(qtdeConfirmados)
dfUnder = pd.concat([dfDescartadosUnder, dfConfirmados], axis=0)

#Over sampling
#print('\nOver Sampling')
#dfConfirmadosOver = dfConfirmados.sample(qtdeDescartados, replace=True)
#dfOver = pd.concat([dfDescartados, dfConfirmadosOver], axis=0)

#Definindo x, y
features = dfUnder.columns.difference(['Resultado_do_teste'])
x = dfUnder[features].values
y = dfUnder['Resultado_do_teste'].values

#Separa base treinamento e teste
xTreino, xTeste, yTreino, yTeste = train_test_split(x, y)

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
scoresDf = cross_val_score(classifierRF, xTreino, yTreino, scoring='accuracy', cv=5)
print('Acurácia: %f%%' % (scoresDf.mean()))