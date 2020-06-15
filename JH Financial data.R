library(quantmod) # Package to pull data from FRED to your R Studio
getSymbols('GDPC1',src='FRED') # Pulling the US Real GDP (quarterly)
str(GDPC1) # Returns the structure of your data set
# It's a list with two objects: (1) the source: Fred, and
# (2) the quarterly time series of US Real GDP from 1/1/1947 to 1/1/2020
head(GDPC1) # The first 6 values
tail(GDPC1) # The last 6 values
plot(GDPC1, main="USA Real GDP (2012 $ billions")

rm(list=ls())

library(dplyr)
library(ggplot2)
library(BatchGetSymbols)
first.date <- Sys.Date() - 3650
last.date <- Sys.Date()
freq.data <- 'daily'
# set tickers
tickers <- c('FB','GOOG','MMM', '^GSPC')
l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         type.return = 'arit',
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) # cache in tempdir()
str(l.out)

curl::has_internet()

p <- ggplot(l.out$df.tickers, aes(x = ref.date, y = price.adjusted))
p <- p + geom_line()
p <- p + facet_wrap(~ticker, scales = 'free_y') 
print(p)