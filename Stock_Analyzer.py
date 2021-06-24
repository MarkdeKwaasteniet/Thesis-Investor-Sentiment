import pandas as pd
import numpy as np
import yfinance as yf

def T30_stock_analyzer(stock_data):
    #importing stock data
    df_stock_data_30min = pd.read_csv(stock_data)

    #Creating Log returns
    df_stock_data_30min['Log_Returns'] = np.log(df_stock_data_30min['Close'].shift(-1)/df_stock_data_30min['Close'])
    df_stock_data_30min = df_stock_data_30min[df_stock_data_30min['Log_Returns'].notna()]


    #Create index for the data
    df_stock_data_30min['Date'] = pd.to_datetime(df_stock_data_30min['Date'], format='%d/%m/%Y %H:%M')
    df_stock_data_30min.set_index('Date', inplace=True, drop=True)

    return df_stock_data_30min

def T60_stock_analyzer(stock_data):
    # importing stock data
    df_stock_data_60min = pd.read_csv(stock_data)

    # Creating Log Returns
    df_stock_data_60min['Log_Returns'] = np.log(df_stock_data_60min['Close'].shift(-1)/df_stock_data_60min['Close'])
    df_stock_data_60min = df_stock_data_60min[df_stock_data_60min['Log_Returns'].notna()]

    # Create index for the data
    df_stock_data_60min['Date'] = pd.to_datetime(df_stock_data_60min['Date'], format='%d/%m/%Y %H:%M')
    df_stock_data_60min.set_index('Date', inplace=True, drop=True)

    return df_stock_data_60min

def day_stock_analyzer(stock_ticker):
    df_stock_data_day = yf.download(stock_ticker, start='2018-01-01', end='2019-01-03', progress=False)

    # Create log returns
    df_stock_data_day['Log_Returns'] = np.log(df_stock_data_day['Close'].shift(-1)/df_stock_data_day['Close'])
    df_stock_data_day = df_stock_data_day[df_stock_data_day['Log_Returns'].notna()]

    return df_stock_data_day