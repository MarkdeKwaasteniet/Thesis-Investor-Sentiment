rm(list=ls())  

install.packages("stargazer")
install.packages("tsDyn")
install.packages("vars")
install.packages("xtable")
library(tsDyn)
library(vars)
library(stargazer)
library(urca)
library(forecast)
library(tidyverse)
library(xtable)
install.packages("gridExtra")
library(gridExtra)
install.packages("cowplot")
library(cowplot)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

setwd("~/Documents/Documents/Bedrijfskunde jaar 3/Bachelor Thesis/NLP/Data Analyse")
source("ReadFunction.R")
source("VARmodel.R")

####################### DAX data ##############################################
### DAX Sentiment on Return ####
# Reading in the data set and computing the lag for every VAR model based on the AIC
setwd("~/Documents/Documents/Bedrijfskunde jaar 3/Bachelor Thesis/NLP/Data Analyse/DAX Data")
df_Return_Sentiment_daily_DAX <- return_sentiment_read_csv('df_return_sentiment_daily_DAX.csv', 10)
df_Return_Sentiment_30min_DAX <- return_sentiment_read_csv('df_Return_Sentiment_30min_DAX.csv', 60) 
df_Return_Sentiment_60min_DAX <- return_sentiment_read_csv('df_Return_Sentiment_60min_DAX.csv', 30) 
# DAX_daily_return_sentiment: 4 lags
# DAX_30min_return_sentiment: 5 lags
# DAX_60min_return_sentiment: 5 lags
lagdaily = 4
lag30min = 5
lag60min = 5

# Creating the VAR models based on the lags selected above - Incl. Testing the residuals 
VAR_model_df_Return_Sentiment_daily_DAX <- VAR_model(df_Return_Sentiment_daily_DAX, nlags = lagdaily, 10)
VAR_model_df_Return_Sentiment_30min_DAX <- VAR_model(df_Return_Sentiment_30min_DAX, nlags = lag30min, 60)
VAR_model_df_Return_Sentiment_60min_DAX <- VAR_model(df_Return_Sentiment_60min_DAX, nlags = lag60min, 60)

# Perform causality tests
causality_test(VAR_model_df_Return_Sentiment_daily_DAX, "Log_Returns", "Sentiment_Score", "HC3")
causality_test(VAR_model_df_Return_Sentiment_30min_DAX, "Log_Returns", "Sentiment_Score", "HC3")
causality_test(VAR_model_df_Return_Sentiment_60min_DAX, "Log_Returns", "Sentiment_Score", "HC3")

# Perform IRF 
irfplot(df_Return_Sentiment_daily_DAX, df_Return_Sentiment_30min_DAX, df_Return_Sentiment_60min_DAX, "Log_Returns", "Sentiment_Score", 10, lagdaily, lag30min, lag60min)

# Output VAR model
stargazer(VAR_model_df_Return_Sentiment_daily_DAX$varresult, VAR_model_df_Return_Sentiment_30min_DAX$varresult, VAR_model_df_Return_Sentiment_60min_DAX$varresult)

### DAX Tweet Volume on Return ####
# Reading in the data set and computing the lag for every VAR model based on the AIC
df_Return_Tweetvol_daily_DAX <- return_tweetvol_read_csv('df_Return_Tweetvol_daily_DAX.csv', 5) 
df_Return_Tweetvol_30min_DAX <- return_tweetvol_read_csv('df_Return_Tweetvol_30min_DAX.csv', 40) 
df_Return_Tweetvol_60min_DAX <- return_tweetvol_read_csv('df_Return_Tweetvol_60min_DAX.csv', 20) 
# DAX_daily_return_tweetvol: 3 lags
# DAX_30min_return_tweetvol: 36 lags
# DAX_60min_return_tweetvol: 18 lags
lagdaily = 3
lag30min = 36
lag60min = 18

# Creating the VAR models based on the lags selected above - Incl. Testing the residuals 
VAR_model_df_Return_Tweetvol_daily_DAX <- VAR_model(df_Return_Tweetvol_daily_DAX, nlags = 3, 10)
VAR_model_df_Return_Tweetvol_30min_DAX <- VAR_model(df_Return_Tweetvol_30min_DAX, nlags = 36, 72)
VAR_model_df_Return_Tweetvol_60min_DAX <- VAR_model(df_Return_Tweetvol_60min_DAX, nlags = 18, 36)
# Note all the residuals of 30min and 60min data sets are auto correlated, heteroskedastic and non normal.

# Perform causality tests
causality_test(VAR_model_df_Return_Tweetvol_daily_DAX, "Log_Returns", "Tweet_Volume", "HC3")
causality_test(VAR_model_df_Return_Tweetvol_30min_DAX, "Log_Returns", "Tweet_Volume", "HC3")
causality_test(VAR_model_df_Return_Tweetvol_60min_DAX, "Log_Returns", "Tweet_Volume", "HC3")

# Perform IRF 
irfplot(df_Return_Tweetvol_daily_DAX, df_Return_Tweetvol_30min_DAX, df_Return_Tweetvol_60min_DAX, "Log_Returns", "Tweet_Volume", 10, lagdaily, lag30min, lag60min)

# Output VAR models
stargazer(VAR_model_df_Return_Tweetvol_daily_DAX$varresult, VAR_model_df_Return_Tweetvol_30min_DAX$varresult, VAR_model_df_Return_Tweetvol_60min_DAX$varresult)

### DAX Tweetvolume and Sentiment on Trading Volume ####
# Reading in the data set and computing the lag for every VAR model based on the AIC
df_Volume_Sentiment_daily_DAX <- volume_sentiment_read_csv('df_Volume_Sentiment_daily_DAX.csv', 20)
df_Volume_Sentiment_daily_DAX[,1] = df_Volume_Sentiment_daily_DAX[,1]/1000000
df_Volume_Tweetvol_daily_DAX <- volume_tweetvol_read_csv('df_Volume_Tweetvol_daily_DAX.csv', 20)
df_Volume_Tweetvol_daily_DAX[,1] = df_Volume_Tweetvol_daily_DAX[,1]/1000000
# DAX_daily_Volume_Sentiment: 4 lags
# DAX_daily_Volume_tweetvol: 2 lags
lagdaily = 4
lagdaily1 = 2

VAR_model_df_Volume_Sentiment_daily_DAX <- VAR(df_Volume_Sentiment_daily_DAX, p = 4, type = "const", season = NULL, exogen = NULL)
VAR_model_df_Volume_Tweetvol_daily_DAX <- VAR(df_Volume_Tweetvol_daily_DAX, p = 2, type = "const", season = NULL, exogen = NULL)

# Creating the VAR models based on the lags selected above - Incl. Testing the residuals 
VAR_model_df_Volume_Sentiment_daily_DAX <- VAR_model(df_Volume_Sentiment_daily_DAX, nlags = 4, 20)
VAR_model_df_Volume_Tweetvol_daily_DAX <- VAR_model(df_Volume_Tweetvol_daily_DAX, nlags = 2, 20)
# Note all the residuals of 30min and 60min data sets are auto correlated, heteroskedastic and non normal.

# Perform causality tests
causality_test(VAR_model_df_Volume_Sentiment_daily_DAX, "Volume", "Sentiment_Score", "HC3")
causality_test(VAR_model_df_Volume_Tweetvol_daily_DAX, "Volume", "Tweet_Volume", "HC3")

# Perform IRF 
irfplot_volume(df_Volume_Tweetvol_daily_DAX, df_Volume_Sentiment_daily_DAX, "Volume", "Tweet_Volume", "Sentiment_Score", 10, lagdaily, lagdaily1)

# Output VAR models
stargazer(VAR_model_df_Volume_Sentiment_daily_DAX$varresult, VAR_model_df_Volume_Tweetvol_daily_DAX$varresult)


####################### FTSE data #############################################
### FTSE Sentiment on Returns ####
# Reading in the data set and computing the lag for every VAR model based on the AIC
setwd("~/Documents/Documents/Bedrijfskunde jaar 3/Bachelor Thesis/NLP/Data Analyse/FTSE100 Data")
df_Return_Sentiment_daily_FTSE <- return_sentiment_read_csv('df_return_sentiment_daily_FTSE.csv', 20)
df_Return_Sentiment_30min_FTSE <- return_sentiment_read_csv('df_Return_Sentiment_30min_FTSE.csv', 40) 
df_Return_Sentiment_60min_FTSE <- return_sentiment_read_csv('df_Return_Sentiment_60min_FTSE.csv', 40)
lagdaily = 1
lag30min = 5
lag60min = 3
# FTSE_daily_return_sentiment: 1 lags
# FTSE_30min_return_sentiment: 2 lags --> 5 for no autocorrelation
# FTSE_60min_return_sentiment: 3 lags

# Creating the VAR models based on the lags selected above - Incl. Testing the residuals 
VAR_model_df_Return_Sentiment_daily_FTSE <- VAR_model(df_Return_Sentiment_daily_FTSE, nlags = lagdaily, 10)
VAR_model_df_Return_Sentiment_30min_FTSE <- VAR_model(df_Return_Sentiment_30min_FTSE, nlags = lag30min, 25)
VAR_model_df_Return_Sentiment_60min_FTSE <- VAR_model(df_Return_Sentiment_60min_FTSE, nlags = lag60min, 50)

# Perform causality tests
causality_test(VAR_model_df_Return_Sentiment_daily_FTSE, "Log_Returns", "Sentiment_Score", "HC3")
causality_test(VAR_model_df_Return_Sentiment_30min_FTSE, "Log_Returns", "Sentiment_Score", "HC3")
causality_test(VAR_model_df_Return_Sentiment_60min_FTSE, "Log_Returns", "Sentiment_Score", "HC3")

# Perform IRF 
irf_plot_1lags(df_Return_Sentiment_daily_FTSE, "Log_Returns", "Sentiment_Score", 10)
irf_plot_2lags(df_Return_Sentiment_30min_FTSE, "Log_Returns", "Sentiment_Score", 10)
irf_plot_3lags(df_Return_Sentiment_60min_FTSE, "Log_Returns", "Sentiment_Score", 10)

irfplot(df_Return_Sentiment_daily_FTSE, df_Return_Sentiment_30min_FTSE, df_Return_Sentiment_60min_FTSE, "Log_Returns", "Sentiment_Score", 10, lagdaily, lag30min, lag60min)

### FTSE TweetVolume on Returns ####
# Reading in the data set and computing the lag for every VAR model based on the AIC
df_Return_Tweetvol_daily_FTSE <- return_tweetvol_read_csv('df_Return_Tweetvol_daily_FTSE.csv', 20) 
df_Return_Tweetvol_30min_FTSE <- return_tweetvol_read_csv('df_Return_Tweetvol_30min_FTSE.csv', 20) 
df_Return_Tweetvol_60min_FTSE <- return_tweetvol_read_csv('df_Return_Tweetvol_60min_FTSE.csv', 20)
lagdaily = 5
lag30min = 95  # 2 days
lag60min = 45 # 2 days
# FTSE_daily_return_tweetvo: 3 lags
# FTSE_30min_return_tweetvo: 18 lags
# FTSE_60min_return_tweetvo: 18 lags

# Creating the VAR models based on the lags selected above - Incl. Testing the residuals 
VAR_model_df_Return_Tweetvol_daily_FTSE <- VAR_model(df_Return_Tweetvol_daily_FTSE, nlags = lagdaily, 10)
VAR_model_df_Return_Tweetvol_30min_FTSE <- VAR_model(df_Return_Tweetvol_30min_FTSE, nlags = lag30min, 95)
VAR_model_df_Return_Tweetvol_60min_FTSE <- VAR_model(df_Return_Tweetvol_60min_FTSE, nlags = lag60min, 50)

# Perform causality tests
causality_test(VAR_model_df_Return_Tweetvol_daily_FTSE, "Log_Returns", "Tweet_Volume", "HC3")
causality_test(VAR_model_df_Return_Tweetvol_30min_FTSE, "Log_Returns", "Tweet_Volume", "HC3")
causality_test(VAR_model_df_Return_Tweetvol_60min_FTSE, "Log_Returns", "Tweet_Volume", "HC3")

# Perform IRF 
irfplot(df_Return_Tweetvol_daily_FTSE, df_Return_Tweetvol_30min_FTSE, df_Return_Tweetvol_60min_FTSE, "Log_Returns", "Tweet_Volume", 10, lagdaily, lag30min, lag60min)


### FTSE TweetVolume and Sentiment on Trading Volume ####
# Reading in the data set and computing the lag for every VAR model based on the AIC
df_Volume_Sentiment_daily_FTSE <- volume_sentiment_read_csv('df_Volume_Sentiment_daily_FTSE.csv', 20)
df_Volume_Sentiment_daily_FTSE[,1] = df_Volume_Sentiment_daily_FTSE[,1]/1000000
df_Volume_Tweetvol_daily_FTSE <- volume_tweetvol_read_csv('df_Volume_Tweetvol_daily_FTSE.csv', 20)
df_Volume_Tweetvol_daily_FTSE[,1] = df_Volume_Tweetvol_daily_FTSE[,1]/1000000
# FTSE_daily_Volume_Sentiment: 1 lags
# FTSE_daily_Volume_tweetvol: 2 lags
lagdaily = 1
lagdaily1 = 2

# Creating the VAR models based on the lags selected above - Incl. Testing the residuals 
VAR_model_df_Volume_Sentiment_daily_FTSE <- VAR_model(df_Volume_Sentiment_daily_FTSE, nlags = lagdaily, 10)
VAR_model_df_Volume_Tweetvol_daily_FTSE <- VAR_model(df_Volume_Tweetvol_daily_FTSE, nlags = lagdaily1, 10)
# Note all the residuals of 30min and 60min data sets are auto correlated, heteroskedastic and non normal.

# Perform causality tests
causality_test(VAR_model_df_Volume_Sentiment_daily_FTSE, "Volume", "Sentiment_Score", "HC3")
causality_test(VAR_model_df_Volume_Tweetvol_daily_FTSE, "Volume", "Tweet_Volume", "HC3")

# Perform IRF 
irfplot_volume(df_Volume_Tweetvol_daily_FTSE, df_Volume_Sentiment_daily_FTSE, "Volume", "Tweet_Volume", "Sentiment_Score", 10, lagdaily, lagdaily1)


####################### CAC data ##############################################
### CAC Sentiment on Return ####
# Reading in the data set and computing the lag for every VAR model based on the AIC
setwd("~/Documents/Documents/Bedrijfskunde jaar 3/Bachelor Thesis/NLP/Data Analyse/CAC40 Data")
df_Return_Sentiment_daily_CAC <- return_sentiment_read_csv('df_return_sentiment_daily_CAC.csv', 20)
df_Return_Sentiment_30min_CAC <- return_sentiment_read_csv('df_Return_Sentiment_30min_CAC.csv', 40) 
df_Return_Sentiment_60min_CAC <- return_sentiment_read_csv('df_Return_Sentiment_60min_CAC.csv', 40) 
# CAC_daily_return_sentiment: 2 lags
# CAC_30min_return_sentiment: 18 lags
# CAC_60min_return_sentiment: 9 lags
lagdaily = 2
lag30min = 18
lag60min = 9

# Creating the VAR models based on the lags selected above - Incl. Testing the residuals 
VAR_model_df_Return_Sentiment_daily_CAC <- VAR_model(df_Return_Sentiment_daily_CAC, nlags = lagdaily, 10)
VAR_model_df_Return_Sentiment_30min_CAC <- VAR_model(df_Return_Sentiment_30min_CAC, nlags = lag30min, 25)
VAR_model_df_Return_Sentiment_60min_CAC <- VAR_model(df_Return_Sentiment_60min_CAC, nlags = lag60min, 60)

# Perform causality tests
causality_test(VAR_model_df_Return_Sentiment_daily_CAC, "Log_Returns", "Sentiment_Score", "HC3")
causality_test(VAR_model_df_Return_Sentiment_30min_CAC, "Log_Returns", "Sentiment_Score", "HC3")
causality_test(VAR_model_df_Return_Sentiment_60min_CAC, "Log_Returns", "Sentiment_Score", "HC3")

# Perform IRF 
irfplot(df_Return_Sentiment_daily_CAC, df_Return_Sentiment_30min_CAC, df_Return_Sentiment_60min_CAC, "Log_Returns", "Sentiment_Score", 10, lagdaily, lag30min, lag60min)


### CAC Tweet volume on Return ####
# Reading in the data set and computing the lag for every VAR model based on the AIC
df_Return_Tweetvol_daily_CAC <- return_tweetvol_read_csv('df_Return_Tweetvol_daily_CAC.csv', 20) 
df_Return_Tweetvol_30min_CAC <- return_tweetvol_read_csv('df_Return_Tweetvol_30min_CAC.csv', 20) 
df_Return_Tweetvol_60min_CAC <- return_tweetvol_read_csv('df_Return_Tweetvol_60min_CAC.csv', 20) 
# CAC_daily_return_tweetvo: 5 lags
# CAC_30min_return_tweetvo: 20 lags
# CAC_60min_return_tweetvo: 18 lags
lagdaily = 5
lag30min = 90
lag60min = 40

# Creating the VAR models based on the lags selected above - Incl. Testing the residuals 
VAR_model_df_Return_Tweetvol_daily_CAC<- VAR_model(df_Return_Tweetvol_daily_CAC, nlags = lagdaily, 10)
VAR_model_df_Return_Tweetvol_30min_CAC <- VAR_model(df_Return_Tweetvol_30min_CAC, nlags = lag30min, 100)
VAR_model_df_Return_Tweetvol_60min_CAC <- VAR_model(df_Return_Tweetvol_60min_CAC, nlags = lag60min, 40)

# Perform causality tests
causality_test(VAR_model_df_Return_Tweetvol_daily_CAC, "Log_Returns", "Tweet_Volume", "HC3")
causality_test(VAR_model_df_Return_Tweetvol_30min_CAC, "Log_Returns", "Tweet_Volume", "HC3")
causality_test(VAR_model_df_Return_Tweetvol_60min_CAC, "Log_Returns", "Tweet_Volume", "HC3")

# Perform IRF 
irfplot(df_Return_Tweetvol_daily_CAC, df_Return_Tweetvol_30min_CAC, df_Return_Tweetvol_60min_CAC, "Log_Returns", "Tweet_Volume", 10, lagdaily, lag30min, lag60min)

### CAC Sentiment and Tweet Volume on Trading Volume ####
# Reading in the data set and computing the lag for every VAR model based on the AIC
df_Volume_Sentiment_daily_CAC <- volume_sentiment_read_csv('df_Volume_Sentiment_daily_CAC.csv', 20) # 4 lags
df_Volume_Sentiment_daily_CAC[,1] = df_Volume_Sentiment_daily_CAC[,1]/1000000
df_Volume_Tweetvol_daily_CAC <- volume_tweetvol_read_csv('df_Volume_Tweetvol_daily_CAC.csv', 20) # 2 lags
df_Volume_Tweetvol_daily_CAC[,1] = df_Volume_Tweetvol_daily_CAC[,1]/1000000
# CAC_daily_Volume_Sentiment: 3 lags
# CAC_daily_Volume_tweetvol: 5 lags
lagdaily = 3
lagdaily1 = 5

# Creating the VAR models based on the lags selected above - Incl. Testing the residuals 
VAR_model_df_Volume_Sentiment_daily_CAC <- VAR_model(df_Volume_Sentiment_daily_CAC, nlags = lagdaily, 20)
VAR_model_df_Volume_Tweetvol_daily_CAC <- VAR_model(df_Volume_Tweetvol_daily_CAC, nlags = lagdaily1, 20)
# Note all the residuals of 30min and 60min data sets are auto correlated, heteroskedastic and non normal.

# Perform causality tests
causality_test(VAR_model_df_Volume_Sentiment_daily_CAC, "Volume", "Sentiment_Score", "HC3")
causality_test(VAR_model_df_Volume_Tweetvol_daily_CAC, "Volume", "Tweet_Volume", "HC3")

# Perform IRF 
irfplot_volume(df_Volume_Tweetvol_daily_CAC, df_Volume_Sentiment_daily_CAC, "Volume", "Tweet_Volume", "Sentiment_Score", 10, lagdaily, lagdaily1)




### additional ####
VAR_model_daily <- summary(VAR_model_df_Return_Sentiment_daily_DAX)
VAR_model_30min <- summary(VAR_model_df_Return_Sentiment_30min_DAX)
xtable(VAR_model_daily$varresult$Sentiment_Score)

par(mfrow=c(2,1),mar=c(4.1,4.1,1.1,2.1))
res <- VAR_model_daily$varresult$Sentiment_Score$residuals
res2 <- VAR_model_30min$varresult$Sentiment_Score$residuals
plot(res, ylab="Residuals", xlab="Time")
lines(VAR_model_daily$varresult$Sentiment_Score$residuals, col="blue")
plot(res2, ylab="Residuals", xlab="Time")
lines(VAR_model_30min$varresult$Sentiment_Score$residuals, col="blue")

