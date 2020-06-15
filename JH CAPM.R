# JH
# CAPM
# 5/13/2020

rm(list=ls())

# Beta estimation for FB, MMM, PG, GM, and GOOG
library(BatchGetSymbols)
# set dates
first.date <- Sys.Date() - 3650
last.date <- Sys.Date()
freq.data <- 'daily'
# set tickers
tickers <- c('FB','MMM', 'PG', 'GM', 'GOOG','^GSPC')

l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         type.return = 'log',
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) # cache in tempdir()
str(l.out)

library(curl)
has_internet()


data1 <- l.out$df.tickers
str(data1)
data <- data.frame(data1$ticker, data1$ref.date, data1$ret.adjusted.prices)
tail(data1$ref.date)
str(data)
library(tidyverse)

returns <- data %>% 
  group_by(data1.ticker) %>% 
  select(data1.price.adjusted) %>% 
  mutate( return_r = log(data1.price.adjusted) - log(lag(data1.price.adjusted)) ) 

str(returns)

returnSP <- returns %>% 
  filter( data1.ticker == '^GSPC' )
returnSP <- as.data.frame(returnSP[,-2])
str(returnSP)
returnSP <- returnSP[,2]

# GOOG
returnGOOG <- returns %>% 
  filter( data1.ticker == 'GOOG' )
returnGOOG <- as.data.frame(returnGOOG[,-2])
str(returnGOOG)
returnGOOG <- returnGOOG[,2]
regGOOG <- lm(returnGOOG ~ returnSP) 
summary(regGOOG)
beta1GOOG <- coef( lm(returnGOOG ~ returnSP ) )[2]
beta1GOOG
plot(returnSP, returnGOOG, pch=16)
abline(regGOOG)
abline(h=0, lty=2)
abline(v=0, lty=2)
mtext(bquote("GOOG" ~ beta  == .(round(beta1GOOG, 4)) ))

# MMM
returnMMM <- returns %>% 
  filter( data1.ticker == 'MMM' )
returnMMM <- as.data.frame(returnMMM[,-2])
str(returnMMM)
returnMMM <- returnMMM[,2]
regMMM <- lm(returnMMM ~ returnSP) 
summary(regMMM)
beta1MMM <- coef( lm(returnMMM ~ returnSP ) )[2]
beta1MMM
plot(returnSP, returnMMM, pch=16)
abline(regMMM)
abline(h=0, lty=2)
abline(v=0, lty=2)
mtext(bquote("MMM" ~ beta  == .(round(beta1MMM, 4)) ))

# HERE


print(l.out$df.control)

library(ggplot2)

p <- ggplot(l.out$df.tickers, aes(x = ref.date, y = price.adjusted))
p <- p + geom_line()
p <- p + facet_wrap(~ticker, scales = 'free_y') 
print(p)

q <- ggplot(l.out$df.tickers, aes(x = ref.date, y = volume))
q <- q + geom_line()
q <- q + facet_wrap(~ticker, scales = 'free_y') 
print(q)

# CAPM equation

capm_fn <- function(rsafe, rmarket, ibeta) {
  mriskp <- (rmarket - rsafe)
  iriskp <- ibeta*mriskp
  irr = rsafe + iriskp 
  print(paste("The market risk premium is", mriskp*100, "% p.a."))
  print(paste("Asset i's risk premium is", iriskp*100, "% p.a."))
  print(paste("Asset i's required return rate is", irr*100, "% p.a."))
}

# Example: 
# Safe return rate (3m T bill yield)= .01. 
# Market return rate (SP500)=.12.
# Asset i's beta=1.2.
capm_fn(.01, .1, 1.2)



# LOOSE ENDS
# I don't need this now
# All the tickers in the SP500
# df.SP500 <- GetSP500Stocks()
# tickers <- df.SP500$Tickers

l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date)

print(l.out$df.control)
print(l.out$df.tickers)
