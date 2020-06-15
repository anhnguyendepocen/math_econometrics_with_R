# Julio Huato
# CAPM
# 5/13/2020

rm(list=ls())

library(BatchGetSymbols)
first.date <- Sys.Date() - 830
last.date <- Sys.Date()
freq.data <- 'daily'
tickers <- c('PG', 'GOOG', 'MMM', '^GSPC')

l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) # cache in tempdir()
str(l.out)

library(ggplot2)

p <- ggplot(l.out$df.tickers, aes(x = ref.date, y = price.close))
p <- p + geom_line()
p <- p + facet_wrap(~ticker, scales = 'free_y') 
print(p)

p2 <- ggplot(l.out$df.tickers, aes(x = ref.date, y = ret.adjusted.prices))
p2 <- p2 + geom_line()
p2 <- p2 + facet_wrap(~ticker, scales = 'free_y') 
print(p2)

str(l.out$df.tickers$ret.adjusted.prices)
str(l.out$df.tickers)

rGSPC <- l.out$df.tickers[ which(l.out$df.tickers$ticker =='^GSPC'), ]
str(rGSPC$ret.adjusted.prices)

rPG <- l.out$df.tickers[ which(l.out$df.tickers$ticker =='PG'), ]
plot(rGSPC$ret.adjusted.prices, rPG$ret.adjusted.prices)
