import matplotlib.pyplot as plt
from scipy import stats
import pandas as pd
from statsmodels.tsa.stattools import adfuller


def price_volume_plotter(df, index):
    # Create a plot of daily index prices and trading volume
    top_plt = plt.subplot2grid((5, 4), (0, 0), rowspan=3, colspan=4)
    top_plt.plot(df.index, df["Close"], color='darkblue')
    plt.title('Historical index prices of ' + index + ' [01-01-2018 to 01-01-2019]')
    bottom_plt = plt.subplot2grid((5, 4), (3, 0), rowspan=1, colspan=4)
    bottom_plt.bar(df.index, df['Volume'], color='#008000')
    plt.title(index + ' Trading Volume', y=-0.60)
    plt.gcf().set_size_inches(15, 10)
    plt.savefig('Daily Stockprices ' + index + '.png')
    plt.close()


def returns_tweet_volume_plotter(df, index):
    # Create a plot of daily index returns and tweet volume
    top_plt = plt.subplot2grid((5, 4), (0, 0), rowspan=3, colspan=4)
    top_plt.plot(df.index, df["Log_Returns"], color='darkblue')
    plt.title('Historical index returns of ' + index + ' [01-01-2018 to 01-01-2019]')
    bottom_plt = plt.subplot2grid((5, 4), (3, 0), rowspan=1, colspan=4)
    bottom_plt.bar(df.index, df['Tweet_Volume'], color='#008000')
    plt.title(index + ' Tweet Volume', y=-0.60)
    plt.gcf().set_size_inches(15, 10)
    plt.savefig('Daily Returns and Tweet Volume ' + index + '.png')
    plt.close()


def rolling_average_plotter(df, index, window_sz):
    sentiment = pd.Series(stats.zscore(df['Sentiment_Score']))
    sentiment_gaussian = sentiment.rolling(window_sz, win_type='triang').mean()
    price_change = pd.Series(stats.zscore(df['Log_Returns']))
    price_change_gaussian = price_change.rolling(window_sz, win_type='triang').mean()
    plt.plot(df.index, sentiment_gaussian, label="Sentiment Score")
    plt.plot(df.index, price_change_gaussian, label="Log Return")
    plt.xlabel('Time')
    plt.ylabel('Z scores')
    plt.legend()
    plt.title('Movement of Log Returns and Sentiment Score of ' + index + ' - ' + str(window_sz) + ' day window')
    plt.gcf().set_size_inches(15, 10)
    plt.savefig('Movement of Log Returns and Sentiment Score of ' + index + '.png')
    plt.close()


def normality_probability_plotter(df, variable, interval):
    # Probability plot
    plt.figure(figsize=(14, 6))
    plt.subplot(1, 2, 1)
    df[variable].hist(bins=50)
    plt.title('Normality plot of ' + variable + ' using ' + interval + ' data')
    plt.subplot(1, 2, 2)
    stats.probplot(df[variable], plot=plt)
    plt.title('Probability plot of ' + variable + ' using ' + interval + ' data')
    plt.savefig('Normality and Probability plot of ' + variable + interval + '.png')
    plt.close()


def normality_tester(df, df1, df2):
    result = {}
    for col in df:
        result[col] = stats.normaltest(df[col])
    df_result = pd.DataFrame()
    print("\n Jarque-Bera Normality test")
    for k, v in result.items():
        df_result = df_result.append(pd.DataFrame(
            data={'Statistic': round(v[0], 3), 'p-value': round(v[1], 3)},
            index=[k]))
    df_result.index.name = 'Data Set Variables'
    df_result1 = df_result
    for col in df1:
        result[col] = stats.normaltest(df1[col])
    df_result = pd.DataFrame()
    print("\n Jarque-Bera Normality test")
    for k, v in result.items():
        df_result = df_result.append(pd.DataFrame(
            data={'Statistic': round(v[0], 3), 'p-value': round(v[1], 3)},
            index=[k]))
    df_result.index.name = 'Data Set Variables'
    df_result = df_result.append(df_result1)
    df_result1 = df_result
    for col in df2:
        result[col] = stats.normaltest(df2[col])
    df_result = pd.DataFrame()
    print("\n Jarque-Bera Normality test")
    for k, v in result.items():
        df_result = df_result.append(pd.DataFrame(
            data={'Statistic': round(v[0], 3), 'p-value': round(v[1], 3)},
            index=[k]))
    df_result.index.name = 'Data Set Variables'
    df_result = df_result.append(df_result1)
    print(df_result)


def stationary_tester(df):
    result = {}
    for col in df:
        result[col] = adfuller(df[col], maxlag=None, autolag='AIC', regression='c')
    df_result = pd.DataFrame()
    print("\n ADF test - AIC lag based")
    for k, v in result.items():
        df_result = df_result.append(pd.DataFrame(
            data={'Number of obs': v[3], 't-test': round(v[0], 3), 'p-value': round(v[1], 3),
                  '1%': round(v[4]['1%'], 3), '5%': round(v[4]['5%'], 3),
                  '10%': round(v[4]['10%'], 3)},
            index=[k]))
    df_result.index.name = 'Data Set Variables'
    print(df_result.to_latex(index=True))


def data_frame_maker(df, interval, index):
    columns_to_keep_Return_Sentiment = ['Sentiment_Score', 'Log_Returns']
    df_Return_Sentiment = df.copy()[columns_to_keep_Return_Sentiment]
    df_Return_Sentiment.to_csv('df_Return_Sentiment_' + interval + '_' + index + '.csv')

    columns_to_keep_Return_TweetVol = ['Tweet_Volume', 'Log_Returns']
    df_Return_Tweetvol = df.copy()[columns_to_keep_Return_TweetVol]
    df_Return_Tweetvol.to_csv('df_Return_Tweetvol_' + interval + '_' + index + '.csv')

    if "Volume" in df:
        columns_to_keep_Volume_Sentiment = ['Sentiment_Score', 'Volume']
        df_Volume_Sentiment = df.copy()[columns_to_keep_Volume_Sentiment]
        df_Volume_Sentiment.to_csv('df_Volume_Sentiment_' + interval + '_' + index + '.csv')

        columns_to_keep_Volume_TweetVol = ['Tweet_Volume', 'Volume']
        df_Volume_Tweetvol = df.copy()[columns_to_keep_Volume_TweetVol]
        df_Volume_Tweetvol.to_csv('df_Volume_Tweetvol_' + interval + '_' + index + '.csv')

def price_plotter(df, index):
    # Create a plot of daily index prices and trading volume
    top_plt = plt.subplot2grid((5, 4), (0, 0), rowspan=3, colspan=4)
    top_plt.plot(df.index, df["Close"], color='darkblue')
    plt.title('Historical index prices of ' + index + ' [01-01-2018 to 01-01-2019]')
    plt.gcf().set_size_inches(15, 10)
    plt.savefig('Daily Stock prices only ' + index + '.png')
    plt.close()

