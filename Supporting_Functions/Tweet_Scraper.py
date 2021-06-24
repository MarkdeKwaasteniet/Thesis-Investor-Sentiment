import aiohttp
import twint
import time

def downloadtweets(searchtweets):
    dt = twint.Config()
    dt.Search = str(searchtweets)
    dt.Since = '2017-12-21 00:00:00'
    dt.Until = '2019-01-04 00:00:00'
    dt.Store_csv = True
    dt.Resume = "./data/" + str(searchtweets) + "MDAX resume tweets.csv"
    dt.Output = "./data/" + str(searchtweets) + "MDAX tweets.csv"

    while True:
        try:
            twint.run.Search(dt)
            break
        except aiohttp.ClientConnectorError:
            time.sleep(10)
            print('Client error. Restarting...')

downloadtweets("$MDAX OR $mdax OR #MDAX OR #mdax")

# Command used for DAX: "$DAX OR $dax OR $GDAXI OR $gdaxi OR #GDAXI OR #gdaxi OR #DAX30 OR #dax30"
# Command used for FTSE: "$UKX OR $ukx OR #UKX OR #ukx OR $FTSE OR $ftse OR #FTSE OR #ftse OR $FTSE100 OR $ftse100 OR #FTSE100 OR #ftse100"
# Command used for CAC: "$CAC OR $cac OR #CAC40 OR #cac40 OR $CAC40 OR $cac40"
# Command used for ESTX 50: "$ESTX50 OR $estx50 OR $SX5E OR $sx5e OR $STOXX50E OR $stoxx50e OR #STOXX50E OR #stoxx50e OR #SX5E OR #sx5e OR #ESTX50 OR #estx50 OR #EUROSTOXX50  OR #eurostoxx50"
# Command used for AEX: "$AEX OR $aex OR $AEX-INDEX OR $aex-index OR #AEX OR #aex OR #aex-index OR #AEX-INDEX"


# Formula used in excel for stock data = (time + 0.25)

