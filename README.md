# DailyTrade with a Gains Cap versus a Hold Strategy

This project looks at the effectiveness of a Daily Trade strategy with a set gains cap versus a Hold strategy in two increasing blue-chip technology stocks, Amazon and Facebook. The Amazon dataset includes all days the market was open between November 3rd, 2003 and July 11th, 2016. The Facebook dataset includes all days the market was open between May 18th, 2012 and July 1st, 2015.

The Daily Trade Strategy is as follows:
1. A gains cap is set at a certain percentage higher than the opening price - in this case it was 6% (NOTE: Only 1% of trading days had a high price that was 6% greater than the opening price. This mark was set with the idea of reducing the loss of potential gains.)
2. If the gains cap is reached, the all shares are sold.
3. If the gains cap is not reached at any point during the day then all shares are sold at the closing price.
4. The process is repeated the next day.

Note: Capital Gains tax was not taken into account and fractional share investing was employed.

The analysis identified Gap as being a major predictor of the effect of each of these strategies. This repository contains the full report, R file used for analysis, full datasets, and abridged datasets with the relevant statistics. 
