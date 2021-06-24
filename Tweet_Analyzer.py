from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
from datetime import datetime
import pandas as pd
import re


def tweet_analyzer(tweet_data):
    # Read the Twitter dataset given in the function
    df_tweets = pd.read_csv(tweet_data)

    # Keep only English language tweets
    df_tweets = df_tweets[df_tweets['language'].str.contains("en")]

    # Keep certain columns of interest
    columns_to_keep = ['id', 'tweet', 'date', 'time', 'timezone', 'retweets_count']
    df_tweets = df_tweets.copy()[columns_to_keep]

    # Clean the tweets of #,$,@ and URL
    df_tweets['tweet'] = df_tweets['tweet'].apply(clean_text, args=(clean_config,))

    # Drop duplicated tweets based on id number
    # df_tweets.drop_duplicates(subset="id", keep=False, inplace=True)

    # Create new column for sentiment score
    df_tweets['Sentiment_Score'] = df_tweets['tweet'].apply(get_sentiment)

    # Set the date time index
    df_tweets['date'] = pd.to_datetime(df_tweets['date'], format='%d/%m/%Y').dt.date
    df_tweets['time'] = pd.to_datetime(df_tweets['time'], format='%H:%M:%S').dt.time
    df_tweets['Date'] = df_tweets.apply(lambda df_tweets: datetime.combine(df_tweets['date'], df_tweets['time']), 1)
    df_tweets.set_index('Date', inplace=True, drop=True)

    # Copy the dataset to count the number of tweets later
    df_tweets_count = df_tweets.copy()
    df_tweets_count['Tweet_Volume'] = 1

    # Keep only sentiment selecting the mean sentiment score per minute
    df_tweets = df_tweets[['Sentiment_Score']].groupby(pd.Grouper(freq='T'), as_index=True).mean()

    # Count the number of tweets using the copied dataset and add to the sentiment dataset
    df_tweets['Tweet_Volume'] = df_tweets_count[['Tweet_Volume']].groupby(pd.Grouper(freq='T'), as_index=True).sum()

    return df_tweets


def clean_text(tweet, options):
    if options['remove_url']:
        tweet = re.sub(r"http\S+", "", tweet, flags=re.MULTILINE)

    if options['remove_mentions']:
        tweet = tweet.replace("@", " ")  # tweet = re.sub(r"@(\w+)", '', tweet, flags=re.MULTILINE)

    if options['remove_hashtags']:
        tweet = tweet.replace("#", "")  # re.sub(r"#(\w+)", '', tweet, flags=re.MULTILINE)

    if options['remove_cashtags']:
        tweet = tweet.replace("$", "")  # tweet = re.sub(r"$(\w+)", '', tweet, flags=re.MULTILINE)

    return tweet


clean_config = {
    'remove_url': True,
    'remove_mentions': True,
    'remove_hashtags': True,
    'remove_cashtags': True,
}

analyzer = SentimentIntensityAnalyzer()


def get_sentiment(tweet):
    return analyzer.polarity_scores(tweet)['compound']
