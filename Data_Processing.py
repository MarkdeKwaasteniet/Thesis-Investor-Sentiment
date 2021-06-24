from Tweet_Analyzer import tweet_analyzer
from Stock_Analyzer import T30_stock_analyzer
from Stock_Analyzer import T60_stock_analyzer
from Stock_Analyzer import day_stock_analyzer
import numpy as np

# Insert index to analyze
Index = "FTSE"
Ticker = "^FTSE"

# Create a clean dataset of tweets
df_tweets = tweet_analyzer(Index + "_tweets_final.csv")  # Insert the index file of tweets

# Create three different datasets. 30min, 60min and daily
df_30min_tweets = df_tweets.resample('30T').agg({'Sentiment_Score': np.mean, 'Tweet_Volume': np.sum})
df_60min_tweets = df_tweets.resample('60T').agg({'Sentiment_Score': np.mean, 'Tweet_Volume': np.sum})
df_day_tweets = df_tweets.resample('D').agg({'Sentiment_Score': np.mean, 'Tweet_Volume': np.sum})

df_stock_data_30min = T30_stock_analyzer("30_minute_" + Index + ".csv")
df_stock_data_60min = T60_stock_analyzer("60_minute_" + Index + ".csv")
df_stock_data_day = day_stock_analyzer(Ticker)

# Join the twitter and stock data
df_complete_30min = df_30min_tweets.join(df_stock_data_30min, how='outer')
df_complete_60min = df_60min_tweets.join(df_stock_data_60min, how='outer')
df_complete_day = df_day_tweets.join(df_stock_data_day, how='outer')

# Keep only values when there are stock market values
df_complete_30min = df_complete_30min[df_complete_30min['Open'].notna()]
df_complete_60min = df_complete_60min[df_complete_60min['Open'].notna()]
df_complete_day = df_complete_day[df_complete_day['Open'].notna()]

# Count the number of missing values
print(df_complete_30min.isna().sum())  # Sentiment: 204 for DAX - 73 for FTSE - 907 for CAC - 3000+ for AEX
print(df_complete_60min.isna().sum())  # Sentiment: 8 for DAX - 1 for FTSE - 166 for CAC - 1000+ for AEX
print(df_complete_day.isna().sum())  # 0

# Fill the NaN with 0 sentiment
df_complete_30min = df_complete_30min.fillna(0)
df_complete_60min = df_complete_60min.fillna(0)
df_complete_day = df_complete_day.fillna(0)

# Export to CSV for further analysis
df_complete_30min.to_csv('df_' + Index + '_30min.csv')
df_complete_60min.to_csv('df_' + Index + '_60min.csv')
df_complete_day.to_csv('df_' + Index + '_day.csv')
