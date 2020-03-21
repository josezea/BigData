# -*- coding: utf-8 -*-
"""
Created on Sat Mar 21 09:48:04 2020

@author: Home
"""

import os
import pandas as pd
import numpy as np 
os.chdir(r'C:\Users\Home\Documents\Laboral2020\Konrad Lorenz\BigData\Clase5')
forbes = pd.read_excel('forbes2000.xlsx')

# Verbos dplyr:

dply: pandas
# filter: query
# arrange: sort_values       # Ordenar
# mutate: assign
# group_by: group_by
# summarise: agg
# select: select (iloc, loc, [])  

# Filtrar las empresas colombianas y que además valor de mercado mayor a 2300
forbes.columns
consulta1 = forbes.query("Country == 'Colombia' & valor_mercado > 2300")

filtro1 = (forbes['Country'] == 'Colombia') & (forbes['valor_mercado'] > 2300)
# filtro1 = (forbes.Country == 'Colombia') & (forbes.valor_mercado > 2300)

consulta1a = forbes.loc[filtro1,]
forbes.iloc[[1,2,5],[1,3]]

# Ordenar de mayor a menor las empresas colombinas con mayor utilidad
consulta2=forbes.query("Country == 'Colombia'").\
sort_values(['utilidades'],ascending=False)

# Crear una columna que sea el valor de mercado dividio los acrivos.
forbes = forbes.assign(ratio_VmUtil = forbes.valor_mercado / forbes.activos).\
sort_values(['ratio_VmUtil'],ascending=False)

forbes = forbes.assign(log_activos = np.log(forbes.utilidades))


# Como construir agregaciones

forbes.valor_mercado.mean()
forbes.agg({'valor_mercado': 'mean'})
forbes.agg({'valor_mercado': 'count'})
forbes.agg({'valor_mercado': np.mean})  
forbes.agg({'valor_mercado': 'count'})
forbes.agg({'valor_mercado': 'min'})
forbes.agg({'valor_mercado': 'max'})
forbes.agg({'valor_mercado': 'median'})



# Coeficiente de variación: 
#forbes.valor_mercado.cv

def cv(x):
    return(np.std(x) / np.mean(x) * 100)


#forbes.agg({'valor_mercado': cv})

forbes.groupby('Country').agg({'valor_mercado': cv})

def p25(x):
    return(np.quantile(x,0.25))
    
p25(forbes.valor_mercado.values)

# agg 

cv(forbes.valor_mercado)
forbes[['valor_mercado', 'utilidades']].apply(cv,axis=0)

forbes.agg({'valor_mercado': ['mean','median', np.std, np.min, np.max]})

forbes.agg({'valor_mercado': ['mean','median', np.std, np.min, np.max],
            'utilidades':['mean','median', np.std, np.min, np.max]
            })


 # Recodificar Colombia, Perú, CHile y México en Alianza Pacífico, 
 #  Demas países en otros
   
lista=['Colombia','Mexico','Chile','Peru']
 
bool_pacifico = forbes.Country.isin(lista)   
bool_pacifico.value_counts()     

forbes.loc[bool_pacifico,'Region']='Alianza Pacifica'
forbes.loc[~bool_pacifico,'Region']='Otros'
# R:forbes$Region <- NA; forbes[logica,]="Alizana Pacifica";forbes[!logica,]="Otros"

# Cuartiles de los activos
#min-p25: 1
#p25-m: 2
#m_p75:3
#p75-max: 4

cuartiles=forbes.activos.quantile([0,0.25, 0.5, 0.75,1])
 
forbes['cuartiles_activos'] = pd.cut(forbes['activos'], 
    bins=cuartiles)
forbes['cuartiles_activos'].value_counts()

#prueba = forbes.assign(cuartiles_activos = pd.cut(forbes['activos'], 
#    bins=cuartiles))


# righr: es cerrado a la derecha
forbes['cuartiles_activos'] = pd.cut(forbes['activos'], 
    bins=cuartiles, right=False, include_lowest = True)
forbes['cuartiles_activos'].value_counts()


cuartiles=forbes.activos.quantile([0,0.25, 0.5, 0.75,1])
cuartiles[4]=np.inf
cuartiles 
forbes['cuartiles_activos'] = pd.cut(forbes['activos'], 
    bins=cuartiles, right=False, include_lowest = True)
forbes['cuartiles_activos'].value_counts()


#data['cuartiles_activos'] = data['cuartiles_activos'].
#replace({'[1500.0, 12050.0) ': 1, 2: 'Bajo', 3: 'Medio', 4: 'Medio', 5: 'Alto', 6: 'Alto'})

