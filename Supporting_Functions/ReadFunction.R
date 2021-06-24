install.packages("tseries")
library(tseries)

# Read in the CSV files and turn into time series
return_sentiment_read_csv <- function(CSV, nlags) {
  df <- read.csv(CSV)
  Return <- ts(df$Log_Returns, start = c(2018,1,2), frequency = 1)
  Sentiment <- ts(df$Sentiment_Score, start = c(2018,1,2), frequency = 1)
  df <- cbind(Return,Sentiment)
  colnames(df) <- cbind("Log_Returns", "Sentiment_Score")
  lagselect <- VARselect(df, lag.max = nlags, type = "const")
  print(lagselect$selection)
  return(df)
}

return_tweetvol_read_csv <- function(CSV, nlags) {
  df <- read.csv(CSV)
  Return <- ts(df$Log_Returns, start = c(2018,1,2), frequency = 1)
  Tweet_Volume <- ts(df$Tweet_Volume, start = c(2018,1,2), frequency = 1)
  df <- cbind(Return,Tweet_Volume)
  colnames(df) <- cbind("Log_Returns", "Tweet_Volume")
  lagselect <- VARselect(df, lag.max = nlags, type = "const")
  print(lagselect$selection)
  return(df)
}

volume_sentiment_read_csv <- function(CSV, nlags) {
  df <- read.csv(CSV)
  Volume <- ts(df$Volume, start = c(2018,1,2), frequency = 1)
  Sentiment <- ts(df$Sentiment_Score, start = c(2018,1,2), frequency = 1)
  df <- cbind(Volume,Sentiment)
  colnames(df) <- cbind("Volume", "Sentiment_Score")
  lagselect <- VARselect(df, lag.max = nlags, type = "const")
  print(lagselect$selection)
  return(df)
}

volume_tweetvol_read_csv <- function(CSV, nlags) {
  df <- read.csv(CSV)
  Volume <- ts(df$Volume, start = c(2018,1,2), frequency = 1)
  Tweet_Volume <- ts(df$Tweet_Volume, start = c(2018,1,2), frequency = 1)
  df <- cbind(Volume,Tweet_Volume)
  colnames(df) <- cbind("Volume", "Tweet_Volume")
  lagselect <- VARselect(df, lag.max = nlags, type = "const")
  print(lagselect$selection)
  return(df)
}
