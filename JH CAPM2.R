# JH
# CAPM
# 5/13/2020

rm(list=ls())
library(quantmod)
Symbols <- c('MMM', 'PG', 'GOOG','^GSPC')
getSymbols(Symbols, return.class ='xts',
                 index.class  ='Date',
                 from = "2005-01-01",
                 to = Sys.Date(),
                 periodicity = "daily",
                 curl.options = list(), src = 'yahoo')

MMM <- data.frame(date=index(MMM), coredata(MMM))
PG <- data.frame(date=index(PG), coredata(PG))
GOOG <- data.frame(date=index(GOOG), coredata(GOOG))
GSPC <- data.frame(date=index(GSPC), coredata(GSPC))

dt <- data.frame(GSPC$date, MMM$MMM.Adjusted, PG$PG.Adjusted, GOOG$GOOG.Adjusted, GSPC$GSPC.Adjusted)
names(dt) <- c("date", "MMM", "PG", "GOOG", "GSPC")
str(dt)

library(tidyverse)
returns <- dt %>% 
  group_by(group) %>% 
  mutate( return_r = log(dt2$cases) - log(lag(dt2$cases)) ) 
str(returns)

returns <- dt2 %>% 
  group_by(group) %>% 
  select(dt2$cases) %>% 
  mutate( return_r = log(dt2$) - log(lag(data1.price.adjusted)) ) 


for (i in c(FB,GM,GOOG,PG,MMM,GSPC)){
  plot()
  }

FB <- data.frame(date=index(FB), coredata(FB))
str(dt)

FB <- as.vector(FB$FB.Adjusted)
GM <- as.vector(GM$GM.Adjusted)
GOOG <-  as.vector(GOOG$GOOG.Adjusted)
PG <- as.vector(PG$PG.Adjusted)
MMM <-  as.vector(MMM$MMM.Adjusted)
GSPC <-  as.vector(GSPC$GSPC.Adjusted)

library(tidyverse)
returns <- dt %>% 
  group_by(colnames()) %>% 
  mutate( return_r = log() - log(lag()) ) 
str(returns)

dt <- as.data.frame(cbind(FB, GM, GOOG, PG, MMM, GSPC))
str(dt)
plot(dt$FB, type="l")

library(tidyverse)
returns <- dt %>% 
  group_by(colnames()) %>% 
  mutate( return_r = log() - log(lag()) ) 
str(returns)


plot(dt$FB, type="l")

# l.out <- BatchGetSymbols(tickers = tickers, 
#                         first.date = first.date,
#                         last.date = last.date, 
#                         freq.data = freq.data,
#                         cache.folder = file.path(tempdir(), 
#                                                  'BGS_Cache') ) # cache in tempdir()
# str(l.out)

data1 <- l.out$df.tickers
str(data1)
data <- data.frame(data1$ticker, data1$ref.date, data1$price.adjusted)
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
