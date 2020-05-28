# -*- coding: utf-8 -*-
"""
Created on Mon Apr 20 16:48:01 2020

@author: andre-goncalves
"""

import pandas as pd
from imblearn.over_sampling import SMOTE, BorderlineSMOTE
from imblearn.combine import SMOTEENN, SMOTETomek
from imblearn.over_sampling import RandomOverSampler

from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MinMaxScaler

from sklearn.ensemble import RandomForestClassifier
from sklearn.inspection import permutation_importance

#Faz a leitura da base
df = pd.read_csv("Dados/novo_covid_ajustado.csv")

#Definindo x, y
features = df.columns.difference(['RESULTADO'])
x = df[features]
y = df['RESULTADO']

#Normalização da base
normalizador = MinMaxScaler(feature_range=(0, 1))
normalizador.fit(x)
x.values[:] = normalizador.transform(x)

#Separa base treinamento e teste
xTreino, xTeste, yTreino, yTeste = train_test_split(x, y, random_state=1)

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
#dfConfirmadosOver = dfConfirmados.sample(qtdeDescartados, replace=True, random_state=1)
#dfOver = pd.concat([dfDescartados, dfConfirmadosOver], axis=0)
#xTreino = dfOver[features].values
#yTreino = dfOver['RESULTADO'].values

#Random Over sampling
#print('\nRandom Over Sampling')
#ros = RandomOverSampler(random_state=1)
#xTreino, yTreino = ros.fit_resample(treino[features], treino['RESULTADO'])
#xTreino = xTreino.values
#yTreino = yTreino.values

#Smote sampling
#print('\nSmote Sampling')
#smote = SMOTE(random_state=1)
#xTreino, yTreino = smote.fit_resample(treino[features], treino['RESULTADO'])
#xTreino = xTreino.values
#yTreino = yTreino.values

#Borderline Smote sampling
print('\nBorderline Smote Sampling')
borderlineSmote = BorderlineSMOTE(random_state=1)
xTreino, yTreino = borderlineSmote.fit_resample(treino[features], treino['RESULTADO'])
xTreino = xTreino.values
yTreino = yTreino.values

#SmoteEEN sampling
#print('\nSMOTEENN Sampling')
#smoteENN = SMOTEENN(random_state=1)
#xTreino, yTreino = smoteENN.fit_resample(treino[features], treino['RESULTADO'])
#xTreino = xTreino.values
#yTreino = yTreino.values

#SmoteTomek sampling
#print('\nSmoteTomek Sampling')
#smoteTomek = SMOTETomek(random_state=1)
#xTreino, yTreino = smoteTomek.fit_resample(treino[features], treino['RESULTADO'])
#xTreino = xTreino.values
#yTreino = yTreino.values

#Classificador Randon Forest
classifierRF = RandomForestClassifier(criterion='gini', max_depth=10, n_estimators=50, n_jobs=-1, random_state=1)

#Treina com todos registros
classifierRF.fit(xTreino, yTreino) 

#Feature Selection
print('\nFeature Selection')
featuresSelection = zip(classifierRF.feature_importances_, features)
for importance, feature in sorted(featuresSelection, reverse=True)[:30]:
    print('%s: %f%%' % (feature, importance*100))

#Permutation Importance
print('\nPermutation Importance')
pi = permutation_importance(classifierRF, xTreino, yTreino, n_repeats=10, n_jobs=4, random_state=1)
for i in pi.importances_mean.argsort()[::-1][:50]:
    print('%s: %.2f' % (features[i], pi.importances_mean[i]*1000))