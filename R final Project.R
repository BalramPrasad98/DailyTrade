install.packages("ggplot2")
library(ggplot2)

##############################################################################################
################################# Amazon #####################################################
##############################################################################################
Amazon <- read.csv('Amazon.csv')
Amazon <- Amazon[rev(rownames(Amazon)),]

quantile(Amazon$Close, probs=0.95)
a <- round(95*length(Amazon$Close))
g <- sort(Amazon$Close)

Facebook$Date[(length(Facebook$Date))]
#Higher price at end of the day than the start of the day
Close_greater_Open = Amazon[Amazon$Open < Amazon$Close,]
#What percentage did these stocks increase by from Open to Close
Close_greater_Open$P_Ch_Op_Cl <- (((Close_greater_Open$Close - Close_greater_Open$Open)/
                                                  (Close_greater_Open$Open))*100)
mean(Close_greater_Open$P_Ch_Op_Cl) ##1.562084

#Using resampling, this number is verified.
samplesize <- 100
replications <- 20
Change_Day_Price <- replicate(replications, sum(sample(
  Close_greater_Open$P_Ch_Op_Cl,samplesize,replace = TRUE))/samplesize)
mean(Change_Day_Price) ##1.562084

#What's the maximum that the price increased during the day (Open to High)?
Close_greater_Open$P_Ch_Op_H <- (((Close_greater_Open$High - Close_greater_Open$Open)/
                                               (Close_greater_Open$Open))*100)
mean(Close_greater_Open$P_Ch_Op_H) ##2.306873

#Using resampling, this number is verified.
samplesize <- 100
replications <- 20
Change_Day_Price <- replicate(replications, sum(sample(
  Close_greater_Open$P_Ch_Op_H,samplesize,replace = TRUE))/samplesize)
mean(Change_Day_Price) ##2.33531

#Days in which Open price is greater than close price
Close_l_Open = Amazon[Amazon$Open > Amazon$Close,]
#High Price is greater than open price - there are 1488/1534 (97.0%) days in which 
#the price went up during the day, despite ending at a lower price than it 
#opened
HighGOpen = Close_l_Open[Close_l_Open$High > Close_l_Open$Open,]
length(HighGOpen$Date)/length(Close_l_Open$Date) ##0.970013
#These prices ended up going up by an average of .7362
HighGOpen$P_ch_Op_H = (((HighGOpen$High - HighGOpen$Open)/(HighGOpen$Open))*100)
mean(HighGOpen$P_ch_Op_H)

#Using resampling, this number is verified.
samplesize <- 100
replications <- 20
Change_Day_Price <- replicate(replications, sum(sample(
  HighGOpen$P_ch_Op_H,samplesize,replace = TRUE))/samplesize)
mean(Change_Day_Price) ##0.7593876

#Price went down for almost half of the days traded 48.71% of the days
length(Close_l_Open$Date)/length(Amazon$Date) * 100

#Change from opening price to close price
Amazon$DayChange <- Amazon$Close - Amazon$Open
#Using resampling, this number is verified.
samplesize <- 100
replications <- 20
Change_Day_Price <- replicate(replications, sum(sample(
  Amazon$DayChange,samplesize,replace = TRUE) > 0)/samplesize)
mean(Change_Day_Price) ##0.502

#Formula for Amazon buying everyday at the opening price and selling if stock 
#appreciates 5%, otherwise at the closing price
Amazon$Change <- ((Amazon$High - Amazon$Open)/Amazon$Open)*100
PercentageCap <- 6

#New column with the selling price for each day
Amazon$SellPrice <- NULL
for (i in 1:length(Amazon$Change)){
  if (Amazon$Change[i] > PercentageCap){
  Amazon$SellPrice[i] <- Amazon$Open[i]+((PercentageCap/100)*Amazon$Open[i])
  } 
  else{
    Amazon$SellPrice[i] <- Amazon$Close[i]
  }
}

#Gain or loss for the day based on selling price 
Amazon$Gain_Loss <- Amazon$SellPrice/Amazon$Open
#Total cash value after each day of sales using day sale strategy
x <- 1000
Amazon$Value[1] <- x*Amazon$Gain_Loss[1]
for (i in 2:length(Amazon$Change)){
  Amazon$Value[i] <- Amazon$Value[i-1]*Amazon$Gain_Loss[i]
}
#Cash value of stocks purchased on day 1 after each day assuming stock was held the 
#entire time 
Amazon$Hold <- (x/Amazon$Open[1])*(Amazon$Close) 

#Difference between value of daily trading and holding if stock were sold at the end
#of each day
Amazon$Diff <- Amazon$Value - Amazon$Hold

#Value of cash after last day of daily trading
DailyTrade <- Amazon$Value[length(Amazon$Value)]
#Value of cash after last day of assuming stock was held
Hold <- (x/Amazon$Open[1])*(Amazon$Close[length(Amazon$Close)])
#Gains made from going with daily trade strategy rather than hold strategy
Diff <- DailyTrade - Hold

#Creating the Amazon Graph plotting daily trade versus holding strategy
Amazon$index <- 1:length(Amazon$Date)
Amazon_Graph <- ggplot(Amazon, aes(index)) + 
  geom_line(aes(y = Value, colour = "Daily Trade (6% cap)")) + 
  geom_line(aes(y = Hold, colour = "Holding")) + 
  labs(title="Amazon Holding versus Daily Trading",x="Days since first investment", 
       y="Total Cash (Initial Investment $1,000)", colour="Type of Trade")

#Calculates the gap for each day 
Amazon$Gap[1] <- Amazon$Close[2]-Amazon$Close[1]
for (i in 2:length(Amazon$Close)){
  Amazon$Gap[i] <- Amazon$Open[i+1]-Amazon$Close[i]
}
Gap_Amazon_Graph <- ggplot(Amazon, aes(x=index, y=Gap)) + geom_line() + 
  labs(title="Amazon Gap over time", x = "Days since initial investment")

Amazon$Gap[1] <- Amazon$Close[2]-Amazon$Close[1]
for (i in 2:length(Amazon$Close)){
  Amazon$Gap[i] <- Amazon$Open[i+1]-Amazon$Close[i]
}
Amazon$Gap

#Change in the day to day cash value using hold strategy
Amazon$HoldChange[1] <- Amazon$Hold[2]-Amazon$Hold[1]
for (i in 2:length(Amazon$Close)){
  Amazon$HoldChange[i] <- Amazon$Hold[i+1]-Amazon$Hold[i]
}

#Change in the day to day cash value using the daily trade strategy
Amazon$ValueChange[1] <- Amazon$Value[2]-Amazon$Value[1]
for (i in 2:length(Amazon$Close)){
  Amazon$ValueChange[i] <- Amazon$Value[i+1]-Amazon$Value[i]
}


#Plotting the relation between gap and the day change. This is necessary to see if a positive
#gap correlates to a strong days performance
Gap_V_DayChange_Graph <- ggplot(Amazon, aes(x=Gap, y=DayChange)) + geom_point(size = .25) + 
  labs(title="Amazon Gap versus Change in Price for following day", y = "Days change")

Gap_V_DayChange <- lm(Amazon$DayChange ~ Amazon$Gap, data=Amazon)
cor.test(Amazon$Gap, Amazon$DayChange, method = "pearson", alternative = "greater")
##p-value = 1
summary(Gap_V_DayChange)$r.squared ##0.006162331

#Graph comparing the Gap and the daily change in value from holding
Hold_Amazon_Graph <- ggplot(Amazon, aes(x=Gap, y=HoldChange)) + geom_point(size=.5) + 
  geom_smooth(method=lm) + labs(title="Amazon Gap versus daily change in total cash using 
                                Hold Strategy", y = "Change in cash assuming held")
cor.test(Amazon$Gap, Amazon$HoldChange, method = "pearson", alternative = "greater")
##p-value = 2.2e-16
Gap_V_HoldChange <- lm(Amazon$HoldChange ~ Amazon$Gap, data=Amazon)
summary(Gap_V_HoldChange)$r.squared ##0.4476325

#Graph comparing the Gap and daily change in value from daily trade
Daily_Amazon_Graph <- ggplot(Amazon, aes(x=Gap, y=ValueChange)) + geom_point(size=.5) + 
  geom_smooth(method=lm) + labs(title="Amazon Gap versus daily change in total cash using 
                                Daily Trade Strategy", y = "Change in cash assuming daily trade")
cor.test(Amazon$Gap, Amazon$ValueChange, method = "pearson", alternative = "greater")
##p-value = 0.9999
#Generating a test statistic to see if there is a linear relationship in the graph
Gap_V_ValueChange <- lm(Amazon$ValueChange ~ Amazon$Gap, data=Amazon)
summary(Gap_V_ValueChange)$r.squared ##0.004359926

##############################################################################################
################################# Facebook ###################################################
##############################################################################################
Facebook <- read.csv('Facebook.csv')
Facebook <- Facebook[rev(rownames(Facebook)),]

#Higher price at end of the day than the start of the day
Close_greater_Open = Facebook[Facebook$Open < Facebook$Close,]
#What percentage did these stocks increase by from Open to Close
Close_greater_Open$P_Ch_Op_Cl <- (((Close_greater_Open$Close - Close_greater_Open$Open)/
                                     (Close_greater_Open$Open))*100)
mean(Close_greater_Open$P_Ch_Op_Cl) ##1.541439

#What's the maximum that the priceincreased during the day (Open to High)?
Close_greater_Open$P_Ch_Op_H <- (((Close_greater_Open$High - Close_greater_Open$Open)/
                                    (Close_greater_Open$Open))*100)
mean(Close_greater_Open$P_Ch_Op_H) ##2.29488

#Days in which Open price is greater than close price
Close_l_Open = Facebook[Facebook$Open > Facebook$Close,]
#High Price is greater than open price - there are 1488/1534 (97.0%) days in which 
#the price went up during the day, despite ending at a lower price than it 
#opened
HighGOpen = Close_l_Open[Close_l_Open$High > Close_l_Open$Open,]
length(HighGOpen$Date)/length(Close_l_Open$Date) ##0.970013
#These prices ended up going up by an average of .7362
HighGOpen$P_ch_Op_H = (((HighGOpen$High - HighGOpen$Open)/(HighGOpen$Open))*100)
mean(HighGOpen$P_ch_Op_H)

#Price went down for almost half of the days traded 48.71% of the days
length(Close_l_Open$Date)/length(Facebook$Date) * 100

#Change from opening price to close price
Facebook$DayChange <- Facebook$Close - Facebook$Open
#Using resampling, this number is verified.
samplesize <- 100
replications <- 20
Change_Day_Price <- replicate(replications, sum(sample(
  Facebook$DayChange,samplesize,replace = TRUE) > 0)/samplesize)
mean(Change_Day_Price)

#Formula for Facebook buying everyday at the opening price and selling if stock 
#appreciates 5%, otherwise at the closing price
Facebook$Change <- ((Facebook$High - Facebook$Open)/Facebook$Open)*100
PercentageCap <- 6

#New column with the selling price for each day
Facebook$SellPrice <- NULL
for (i in 1:length(Facebook$Change)){
  if (Facebook$Change[i] > PercentageCap){
    Facebook$SellPrice[i] <- Facebook$Open[i]+((PercentageCap/100)*Facebook$Open[i])
  } 
  else{
    Facebook$SellPrice[i] <- Facebook$Close[i]
  }
}

#Gain or loss for the day based on selling price 
Facebook$Gain_Loss <- Facebook$SellPrice/Facebook$Open
#Total cash value after each day of sales using day sale strategy
x <- 1000
Facebook$Value[1] <- x*Facebook$Gain_Loss[1]
for (i in 2:length(Facebook$Change)){
  Facebook$Value[i] <- Facebook$Value[i-1]*Facebook$Gain_Loss[i]
}
#Cash value of stocks purchased on day 1 after each day assuming stock was held the 
#entire time 
Facebook$Hold <- (x/Facebook$Open[1])*(Facebook$Close) 

#Difference between value of daily trading and holding if stock were sold at the end
#of each day
Facebook$Diff <- Facebook$Value - Facebook$Hold

#Value of cash after last day of daily trading
DailyTrade <- Facebook$Value[length(Facebook$Value)]
#Value of cash after last day of assuming stock was held
Hold <- (x/Facebook$Open[1])*(Facebook$Close[length(Facebook$Close)])
#Gains made from going with daily trade strategy rather than hold strategy
Diff <- DailyTrade - Hold

#Number of days when daily trading strategy was more effective than the hold strategy
sum(Facebook$Diff > 0)/length(Facebook$Diff)

#Creating the Facebook Graph plotting daily trade versus holding strategy
Facebook$index <- 1:length(Facebook$Date)
Facebook_Graph <- ggplot(Facebook, aes(index)) + 
  geom_line(aes(y = Value, colour = "Daily Trade (6% cap)")) + 
  geom_line(aes(y = Hold, colour = "Hold")) + 
  labs(title="Facebook Holding versus Daily Trading",x="Days since first investment", 
       y="Total Cash (Initial Investment $1,000)", colour="Type of Trade")

#Calculates the gap for each day 
Facebook$Gap[1] <- Facebook$Close[2]-Facebook$Close[1]
for (i in 2:length(Facebook$Close)){
  Facebook$Gap[i] <- Facebook$Open[i+1]-Facebook$Close[i]
}
Gap_Facebook_Graph <- ggplot(Facebook, aes(x=index, y=Gap)) + geom_line() + 
  labs(title="Facebook Gap over time", x = "Days since initial investment")

#Change in the day to day cash value using hold strategy
Facebook$HoldChange[1] <- Facebook$Hold[2]-Facebook$Hold[1]
for (i in 2:length(Facebook$Close)){
  Facebook$HoldChange[i] <- Facebook$Hold[i+1]-Facebook$Hold[i]
}

#Change in the day to day cash value using the daily trade strategy
Facebook$ValueChange[1] <- Facebook$Value[2]-Facebook$Value[1]
for (i in 2:length(Facebook$Close)){
  Facebook$ValueChange[i] <- Facebook$Value[i+1]-Facebook$Value[i]
}

#Change from opening price to close price
Facebook$DayChange <- Facebook$Close - Facebook$Open

#Plotting the relation between gap and the day change. This is necessary to see if a positive
#gap correlates to a strong days performance
Gap_V_DayChange_Graph <- ggplot(Facebook, aes(x=Gap, y=DayChange)) + geom_point(size = .25) + 
  labs(title="Facebook Gap versus Change in Price for following day", y = "Days change")
Gap_V_DayChange <- lm(Facebook$DayChange ~ Facebook$Gap, data=Facebook)
cor.test(Facebook$Gap, Facebook$DayChange, method = "pearson", alternative = "greater")
summary(Gap_V_DayChange)$r.squared


#Graph comparing the Gap and the daily change in value from holding
Hold_Facebook_Graph <- ggplot(Facebook, aes(x=Gap, y=HoldChange)) + geom_point(size=.5) + 
  geom_smooth(method=lm) + labs(title="Facebook Gap versus daily change in total cash using 
                                Hold Strategy", y = "Change in cash assuming held")
cor.test(Facebook$Gap, Facebook$HoldChange, method = "pearson", alternative = "greater") 
##p-value = 2.2e-16
Gap_V_HoldChange <- lm(Facebook$HoldChange ~ Facebook$Gap, data=Facebook)
summary(Gap_V_HoldChange)$adj.r.squared ##0.3294795

#Graph comparing the Gap and daily change in value from daily trade
Daily_Facebook_Graph <- ggplot(Facebook, aes(x=Gap, y=ValueChange)) + geom_point(size=.5) + 
  geom_smooth(method=lm) + labs(title="Facebook Gap versus daily change in total cash using 
                                Daily Trade Strategy", y = "Change in cash assuming daily trade")
cor.test(Facebook$Gap, Facebook$ValueChange, method = "pearson", alternative = "greater")
##p-value = 0.1296
#Generating a test statistic to see if there is a linear relationship in the graph
Gap_V_ValueChange <- lm(Facebook$ValueChange ~ Facebook$Gap, data=Facebook)
summary(Gap_V_ValueChange)$r.squared ##0.0003509692


