# -*- coding: utf-8 -*-
"""
Created on Wed Jan 31 21:56:50 2024

@author: thfar
"""

from gurobipy import *
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from math import sqrt

####Optimize one portfolio
data_stock=pd.read_csv('C:/Users/thfar/OneDrive/Documents/IAA/IAA - AA503/Optimization/Optimization Final Project/stocks.csv')
stocks = data_stock[['AAPL_wr', 'GOOG_wr', 'IBM_wr', 'MARA_wr', 'NVDA_wr']].columns
num_stocks=len(stocks)

### Get all of the expected returns
stock_return = data_stock[['AAPL_wr', 'GOOG_wr', 'IBM_wr', 'MARA_wr', 'NVDA_wr']][1:].mean()
### Get covariance matrix
cov_mat=data_stock[['AAPL_wr', 'GOOG_wr', 'IBM_wr', 'MARA_wr', 'NVDA_wr']][1:].cov()
# Create an empty model
m = Model('portfolio')

# Add a variable for each stock
vars = pd.Series(m.addVars(stocks,lb=0), index=stocks)  

##Set objective
portfolio_risk = cov_mat.dot(vars).dot(vars)
m.setObjective(portfolio_risk, GRB.MINIMIZE)

## constraints
m.addConstr(vars.sum() == 1, 'budget')
m.addConstr(stock_return.dot(vars) >= 0.9,'return')

m.optimize()


print('Minimum Risk Portfolio:\n')

for v in vars:
    if v.x > 0:
        print('\t%s\t: %g' % (v.varname, v.x))
# 	C0	: 0.229713
# 	C1	: 0.0918229
# 	C2	: 0.538814
# 	C3	: 0.0173418
# 	C4	: 0.122308

#objective val solution
print(m.objVal) #5.614834698132613



##########################################################################
############# Efficient Frontier 
##########################################################################

stocks = data_stock[['AAPL_wr', 'GOOG_wr', 'IBM_wr', 'MARA_wr', 'NVDA_wr']].columns
num_stocks=len(stocks)


# Calculate basic summary statistics for individual stocks
stock_volatility = data_stock[['AAPL_wr', 'GOOG_wr', 'IBM_wr', 'MARA_wr', 'NVDA_wr']].std()
stock_return = data_stock[['AAPL_wr', 'GOOG_wr', 'IBM_wr', 'MARA_wr', 'NVDA_wr']].mean()
cov_mat=data_stock[['AAPL_wr', 'GOOG_wr', 'IBM_wr', 'MARA_wr', 'NVDA_wr']].cov()


returns = np.linspace( 0.6, 1.4, 100 )
ret_list = []
risks = []
props = []

for ret in returns:
    m.reset(0)
    m = Model('Portfolio_Optimization')
    m.setParam('OutputFlag', 0)
    vars=pd.Series(m.addVars(stocks,lb=0), index=stocks) 
    portfolio_risk = cov_mat.dot(vars).dot(vars)
    m.setObjective(portfolio_risk, GRB.MINIMIZE)
    m.addConstr(vars.sum() == 1, name = 'budget' )
    m.addConstr(stock_return.dot(vars) == ret , name = 'return_sim')
  
    m.optimize()  
    risks.append(np.sqrt(m.ObjVal))
    ret_list.append(stock_return.dot(m.x) )
    props.append(m.x)
    

plt.rcParams.update({'font.size': 22})
plt.plot( risks, returns ) 
plt.xlabel( 'Risk' )
plt.ylabel( 'Return' )
plt.title( 'Efficient Frontier' )
plt.plot()
plt.show()

