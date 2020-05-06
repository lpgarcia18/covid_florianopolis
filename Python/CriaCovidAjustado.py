# -*- coding: utf-8 -*-
"""CovidAnonimizado.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1CE5wGf7XNRVEPST8-uSLpxh2CKuL3Snu
"""

import pandas as pd
import time
from sklearn import preprocessing
from sklearn.feature_extraction import FeatureHasher

#Faz a leitura do arquivo
dados = pd.read_csv('/home/andre-goncalves/MEGA.DR/Pesquisa/Covid-19/Dados/covid_ajustado.csv')
dados.shape

#Seleciona as colunas que contém informações
print(dados.isnull().sum().where(lambda x: x < len(dados)).dropna().index)

#Altera o NaN para missing
meusDados = dados.fillna('missing')
meusDados.head()

#Totaliza os dados da coluna Resultado
meusDados.groupby("RESULTADO").size()

#Conta qtos Territorios a base possui
len(meusDados.groupby("TERRITORIO").size())

#Faz um sumário dos dados de todas colunas
for coluna in meusDados.columns:
  print('Coluna: ', coluna)
  print(meusDados[coluna].value_counts())
  print('')

#Totaliza os dados da coluna Resultado por Território
meusDados.query('RESULTADO == "confirmado"')['TERRITORIO'].value_counts()

#Mostra uma breve descrição dos dados do Bairro
meusDados['TERRITORIO'].describe()

#Transforma str para date
meusDados['INICIO_SINTOMAS'] = pd.to_datetime(meusDados['INICIO_SINTOMAS'], errors='coerce')

#Exclui casos que não conseguiu converter a data
meusDados = meusDados.dropna(subset=['INICIO_SINTOMAS'])
meusDados.shape

###Faz a totalização dos casos confirmados por Território ###
confirmadosPorTerritorio = meusDados[['TERRITORIO', 'INICIO_SINTOMAS', 'RESULTADO']].query('RESULTADO == "confirmado"')
confirmadosPorTerritorio = pd.merge(meusDados, confirmadosPorTerritorio, how='left', on=['TERRITORIO', 'TERRITORIO'], suffixes=('_t1', '_t2'))

print(confirmadosPorTerritorio.shape)
confirmadosPorTerritorio.head()

confirmadosPorTerritorio['Diferenca_dias_sintomas'] = (confirmadosPorTerritorio['INICIO_SINTOMAS_t1'] - confirmadosPorTerritorio['INICIO_SINTOMAS_t2']).dt.days
confirmadosPorTerritorio = confirmadosPorTerritorio[(confirmadosPorTerritorio.Diferenca_dias_sintomas >= 0) & (confirmadosPorTerritorio.Diferenca_dias_sintomas <= 14)]

confirmadosPorTerritorio.head()

confirmados14Dias = confirmadosPorTerritorio.groupby(['ID']).size()
confirmados14Dias

###Faz a totalização dos casos descartados por Bairro ###
descartadosPorTerritorio = meusDados[['TERRITORIO', 'INICIO_SINTOMAS', 'RESULTADO']].query('RESULTADO == "descartado"')
descartadosPorTerritorio = pd.merge(meusDados, descartadosPorTerritorio, how='left', on=['TERRITORIO', 'TERRITORIO'], suffixes=('_t1', '_t2'))

print(descartadosPorTerritorio.shape)
descartadosPorTerritorio.head()

descartadosPorTerritorio['Diferenca_dias_sintomas'] = (descartadosPorTerritorio['INICIO_SINTOMAS_t1'] - descartadosPorTerritorio['INICIO_SINTOMAS_t2']).dt.days
descartadosPorTerritorio = descartadosPorTerritorio[(descartadosPorTerritorio.Diferenca_dias_sintomas >= 0) & (descartadosPorTerritorio.Diferenca_dias_sintomas <= 14)]

descartadosPorTerritorio.head()

descartados14Dias = descartadosPorTerritorio.groupby(['ID']).size()
descartados14Dias

###Junta os dados na tabela original ###
df = pd.merge(meusDados, confirmados14Dias.to_frame(), how='left', on=['ID', 'ID'])
df = pd.merge(df, descartados14Dias.to_frame(), how='left', on=['ID', 'ID'])
df.head()
df.shape

df = df.rename(columns={'0_x': 'Confirmados_territorio_14dias'})
df = df.rename(columns={'0_y': 'Descartados_territorio_14dias'})
df.query('TERRITORIO == "cs_centro"').sort_values(by=['INICIO_SINTOMAS']).head(5)
    
#Define pra zero os registros com valor nulo
df.loc[df[df.Confirmados_territorio_14dias.isnull()].index, ['Confirmados_territorio_14dias']] = 0
df.loc[df[df.Descartados_territorio_14dias.isnull()].index, ['Descartados_territorio_14dias']] = 0

#Deixa na base apenas os registros com Resultado igual a 'confirmado' ou 'descartado'
df = df.drop(df.query('RESULTADO != "confirmado" and RESULTADO != "descartado"').index)

#Reset no índice para dar append nas colunas
df = df.reset_index()

#Transforma 'Data dos sintomas' em timestamp
df['INICIO_SINTOMAS'].isnull().sum()
df['INICIO_SINTOMAS'] = df['INICIO_SINTOMAS'].apply(lambda x: int(time.mktime(x.timetuple())))

# Formata a Idade com duas casas decimais
df['IDADE'] = df['IDADE'].map('{:,.2f}'.format)

#Exclui registros com Sexo 'missing'
df = df.drop(df[df.SEXO == 'missing'].index)

#Transforma Sexo em código
df['SEXO'] = df['SEXO'].apply(lambda x: '0' if (x == 'f') else '1')

#Transforma Territorio em hash
len(df.groupby('TERRITORIO').size())
fh = FeatureHasher(n_features=10, input_type='string')
hashTerritorio = fh.fit_transform(df['TERRITORIO'])
dfTerritorio = pd.DataFrame(fh.fit_transform(df['TERRITORIO']).toarray(), columns=['hf0', 'hf1', 'hf2', 'hf3', 'hf4', 'hf5', 'hf6', 'hf7', 'hf8', 'hf9'])
df[dfTerritorio.columns] = dfTerritorio

#Transforma Raça em código
len(df.groupby('RACA_COR').size())
df['RACA_COR'].value_counts()
enc = preprocessing.OneHotEncoder()
dfRacaCor = pd.DataFrame(preprocessing.OneHotEncoder().fit_transform(df['RACA_COR'].to_frame()).toarray(), columns=['rc0', 'rc1', 'rc2', 'rc3', 'rc4'])
df[dfRacaCor.columns] = dfRacaCor

#Transforma Triagem em código
len(df.groupby('TRIAGEM').size())
df.groupby('TRIAGEM').size()
dfTriagem = pd.DataFrame(preprocessing.OneHotEncoder().fit_transform(df['TRIAGEM'].to_frame()).toarray(), columns=['tr0', 'tr1', 'tr2'])
df[dfTriagem.columns] = dfTriagem

#Faz o filtro de colunas
colunas = ['SEXO','IDADE','INICIO_SINTOMAS','RESULTADO','INFECTADOS_TERRITORIO','idade_0_anos',
           'idade_1_anos','idade_2_anos','idade_3_anos','idade_4_anos','idade_5_anos',
           'idade_6_anos','idade_7_anos','idade_8_anos','idade_9_anos','idade_10_anos',
           'idade_11_anos','idade_12_anos','idade_13_anos','idade_14_anos','idade_15_anos',
           'idade_16_anos','idade_17_anos','idade_18_anos','idade_19_anos','idade_20_anos',
           'idade_21_anos','idade_22_anos','idade_23_anos','idade_24_anos','idade_25_anos',
           'idade_26_anos','idade_27_anos','idade_28_anos','idade_29_anos','idade_30_anos',
           'idade_31_anos','idade_32_anos','idade_33_anos','idade_34_anos','idade_35_anos',
           'idade_36_anos','idade_37_anos','idade_38_anos','idade_39_anos','idade_40_anos',
           'idade_41_anos','idade_42_anos','idade_43_anos','idade_44_anos','idade_45_anos',
           'idade_46_anos','idade_47_anos','idade_48_anos','idade_49_anos','idade_50_anos',
           'idade_51_anos','idade_52_anos','idade_53_anos','idade_54_anos','idade_55_anos',
           'idade_56_anos','idade_57_anos','idade_58_anos','idade_59_anos','idade_60_anos',
           'idade_61_anos','idade_62_anos','idade_63_anos','idade_64_anos','idade_65_anos',
           'idade_66_anos','idade_67_anos','idade_68_anos','idade_69_anos','idade_70_anos',
           'idade_71_anos','idade_72_anos','idade_73_anos','idade_74_anos','idade_75_anos',
           'idade_76_anos','idade_77_anos','idade_78_anos','idade_79_anos','idade_80_anos',
           'idade_81_anos','idade_82_anos','idade_83_anos','idade_84_anos','idade_85_anos',
           'idade_86_anos','idade_87_anos','idade_88_anos','idade_89_anos','idade_90_anos',
           'idade_91_anos','idade_92_anos','idade_93_anos','idade_94_anos','idade_95_anos',
           'idade_96_anos','idade_97_anos','idade_98_anos','idade_99_anos','idade_100_anos_ou_mais',
           'idade_na','populacao populacao','populacao homens','populacao mulheres',
           'escolaridade_alfabetizados','escolaridade_nao_alfabetizados',
           'escolaridade_s_instr_ou_-1_ano','escolaridade_01_ano','escolaridade_02_anos',
           'escolaridade_03_anos','escolaridade_04_anos','escolaridade_05_anos',
           'escolaridade_06_anos','escolaridade_07_anos','escolaridade_08_anos',
           'escolaridade_09_anos','escolaridade_10_anos','escolaridade_11_anos',
           'escolaridade_12_anos','escolaridade_13_anos','escolaridade_14_anos',
           'escolaridade_15_anos','escolaridade_16_anos','escolaridade_17_anos_ou_mais',
           'escolaridade_nao_determinado','escolaridade_alfabetizacao_de_adultos',
           'cor_pele_1branca','cor_pele_2preta','cor_pele_3amarela','cor_pele_4parda',
           'cor_pele_5indigena','cor_pele_9ignorado','renda_tot_domic','renda_med_por_domic',
           'renda_tot_resp','renda_med_resp','renda_tot_pess','renda_med_pess', 
           'Confirmados_territorio_14dias', 'Descartados_territorio_14dias', 'hf0', 'hf1', 'hf2', 
           'hf3', 'hf4', 'hf5', 'hf6', 'hf7', 'hf8', 'hf9', 'rc0', 'rc1', 'rc2', 'rc3', 'rc4', 
           'tr0', 'tr1', 'tr2']

colunas = ['SEXO','IDADE','INICIO_SINTOMAS','RESULTADO',
           'escolaridade_alfabetizados','escolaridade_nao_alfabetizados',
           'escolaridade_s_instr_ou_-1_ano','escolaridade_01_ano','escolaridade_02_anos',
           'escolaridade_03_anos','escolaridade_04_anos','escolaridade_05_anos',
           'escolaridade_06_anos','escolaridade_07_anos','escolaridade_08_anos',
           'escolaridade_09_anos','escolaridade_10_anos','escolaridade_11_anos',
           'escolaridade_12_anos','escolaridade_13_anos','escolaridade_14_anos',
           'escolaridade_15_anos','escolaridade_16_anos','escolaridade_17_anos_ou_mais',
           'escolaridade_nao_determinado','escolaridade_alfabetizacao_de_adultos',
           'renda_tot_domic','renda_med_por_domic',
           'renda_tot_resp','renda_med_resp','renda_tot_pess','renda_med_pess', 
           'Confirmados_territorio_14dias', 'Descartados_territorio_14dias', 'hf0', 'hf1', 'hf2', 
           'hf3', 'hf4', 'hf5', 'hf6', 'hf7', 'hf8', 'hf9', 'rc0', 'rc1', 'rc2', 'rc3', 'rc4']

df = df[colunas]
df.head()

df.to_csv('Dados/novo_covid_ajustado.csv', index=False, header=True)