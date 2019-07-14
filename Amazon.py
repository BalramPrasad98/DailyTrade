#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov 16 14:48:59 2017

@author: balramprasad
"""
import glob
import pandas as pd

filelist = glob.glob('*.csv') # 'glob.glob' -- hee hee
filelist

for f in filelist:
    df = pd.read_csv(f)
    print(df,"\n")
df

stocks1 = pd.DataFrame()
for f in filelist:
    newdf = pd.read_csv(f)
    newdf['Stock'] = f.split('.')[0]
    stocks1 = pd.concat([stocks1,newdf])
stocks1

#November 3rd 2003 (54.799999) to July 11th 2016 (750.000000)
#If we invested $10,000 in the market on 11/3/2003, we would have $136,861
#Increased by $126,861. But is this the most that we can make???
a = 10000
shares = 10000/54.799999

Amazon = stocks1[(stocks1['Stock'] == 'AMZN')]
Amazon.to_csv('Amazon.csv', encoding='utf-8', index=False)
Amazon['Change'] = ((Amazon['High'] - Amazon['Open'])/Amazon['Open'])*100
Amazon['.005Gain'] = (Amazon['Open'] + (.005*Amazon['Open']))
Gain_Amazon = Amazon[Amazon['.005Gain'] <= Amazon['High']]
Gain_Amazon['Gain'] = Gain_Amazon['.005Gain']-Gain_Amazon['Open']
TotalG = sum(Gain_Amazon['Gain'])

Loss_Amazon = Amazon[Amazon['.001Gain'] > Amazon['High']]
Loss_Amazon['Loss'] = Loss_Amazon['Close']-Loss_Amazon['Open']
TotalL = sum(Loss_Amazon['Loss'])

(TotalG + TotalL) * shares


#Higher price at end of the day than the start of the day
Close_g_Open = Amazon[(Amazon['Open'] < Amazon['Close'])]
#What percentage did these stocks increase by?
Percent_ch_Op_Cl = (((Close_g_Open['Close'] - Close_g_Open['Open']
)/(Close_g_Open['Open']))*100)
Percent_ch_Op_Cl.mean()
#What's the maximum that the increased during the day
Percent_ch_High_Open = (((Close_g_Open['High'] - Close_g_Open['Open'])/(
        Close_g_Open['Open']))*100)
Percent_ch_High_Open.mean()

#Days in which Open price is greater than close price
Close_l_Open = Amazon[(Amazon['Open'] > Amazon['Close'])]

#High Price is greater than open price - there are 1488/1534 days in which 
#the price went up during the day, despite ending at a lower price than it 
#opened
HighGOpen = Close_l_Open[(Close_l_Open['High'] > Close_l_Open['Open'])]
len(HighGOpen)/len(Close_l_Open)
#These prices ended up going up by an average of .7362
P_ch_Clo_HO = (((HighGOpen['High'] - HighGOpen['Open'])/(HighGOpen['Open']))*100)
P_ch_Clo_HO.mean()

#Price went down for almost half of the days traded 48.71% of the days
len(Close_l_Open)/len(Amazon) * 100

Amazon['Change'] = ((Amazon['High'] - Amazon['Open'])/Amazon['Open'])*100
