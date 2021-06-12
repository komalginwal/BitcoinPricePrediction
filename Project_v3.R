#install.packages(c("Rcpp","tidyverse"))
#install.packages("GGally")
#install.packages("anytime")
#install.packages("coinmarketcapr")
#install.packages("lubridate")
#install.packages("magrittr") # package installations are only needed the first time you use it
#install.packages("psych")
#install.packages("Quandl")
#install.packages("PerformanceAnalytics")
#install.packages("stringr")
#install.packages("lessR")
#install.packages("forecast")

library(anytime)
library(ggplot2)
library(plotly)
library(stringr)
library(tidyr)
library(dplyr)
library(lessR)
library(knitr)
library(reshape2)
library(corrplot)
library(PerformanceAnalytics)
library(forecast)

#Setting the working directory
setwd("/Users/komal/Documents/FE582/Project_data/")

perl_path <- "C:/Strawberry/perl/bin/perl5.32.1.exe"

#Loading the historical data of different cryptocurrencies 
Aave <- read.csv("coin_Aave.csv")
Aave <- Aave[ , -which(names(Aave) %in% c("SNo"))]

BinanceCoin <- read.csv("coin_BinanceCoin.csv")
BinanceCoin <- BinanceCoin[ , -which(names(BinanceCoin) %in% c("SNo"))]

Bitcoin <- read.csv("coin_Bitcoin.csv")
Bitcoin <- Bitcoin[ , -which(names(Bitcoin) %in% c("SNo"))]

Cardano <- read.csv("coin_Cardano.csv")
Cardano <- Cardano[ , -which(names(Cardano) %in% c("SNo"))]

ChainLink <- read.csv("coin_ChainLink.csv")
ChainLink <- ChainLink[ , -which(names(ChainLink) %in% c("SNo"))]

Cosmos <- read.csv("coin_Cosmos.csv")
Cosmos <- Cosmos[ , -which(names(Cosmos) %in% c("SNo"))]

Cryptocom <- read.csv("coin_CryptocomCoin.csv")
Cryptocom <- Cryptocom[ , -which(names(Cryptocom) %in% c("SNo"))]

Dogecoin <- read.csv("coin_Dogecoin.csv")
Dogecoin <- Dogecoin[ , -which(names(Dogecoin) %in% c("SNo"))]

EOS <- read.csv("coin_EOS.csv")
EOS <- EOS[ , -which(names(EOS) %in% c("SNo"))]

Ethereum <- read.csv("coin_Ethereum.csv")
Ethereum <- Ethereum[ , -which(names(Ethereum) %in% c("SNo"))]

Iota <- read.csv("coin_Iota.csv")
Iota <- Iota[ , -which(names(Iota) %in% c("SNo"))]

Litecoin <- read.csv("coin_Litecoin.csv")
Litecoin <- Litecoin[ , -which(names(Litecoin) %in% c("SNo"))]

Monero <- read.csv("coin_Monero.csv")
Monero <- Monero[ , -which(names(Monero) %in% c("SNo"))]

NEM <- read.csv("coin_NEM.csv")
NEM <- NEM[ , -which(names(NEM) %in% c("SNo"))]

Polkadot <- read.csv("coin_Polkadot.csv")
Polkadot <- Polkadot[ , -which(names(Polkadot) %in% c("SNo"))]

Solana <- read.csv("coin_Solana.csv")
Solana <- Solana[ , -which(names(Solana) %in% c("SNo"))]

Stellar <- read.csv("coin_Stellar.csv")
Stellar <- Stellar[ , -which(names(Stellar) %in% c("SNo"))]

Tether <- read.csv("coin_Tether.csv")
Tether <- Tether[ , -which(names(Tether) %in% c("SNo"))]

Tron <- read.csv("coin_Tron.csv")
Tron <- Tron[ , -which(names(Tron) %in% c("SNo"))]

Uniswap <- read.csv("coin_Uniswap.csv")
Uniswap <- Uniswap[ , -which(names(Uniswap) %in% c("SNo"))]

USDCoin <- read.csv("coin_USDCoin.csv")
USDCoin <- USDCoin[ , -which(names(USDCoin) %in% c("SNo"))]

WrappedBitcoin <- read.csv("coin_WrappedBitcoin.csv")
WrappedBitcoin <- WrappedBitcoin[ , -which(names(WrappedBitcoin) %in% c("SNo"))]

XRP <- read.csv("coin_XRP.csv")
XRP <- XRP[ , -which(names(XRP) %in% c("SNo"))]

price_data <- rbind(Aave, BinanceCoin,Bitcoin,Cardano,ChainLink,Cosmos, Cryptocom,Dogecoin,EOS,Ethereum,Iota,
              Litecoin,Monero,NEM,Polkadot,Solana,Stellar,Tether,Tron,Uniswap,
              USDCoin,WrappedBitcoin,XRP)

#Checking for variable type
sapply(price_data, class)

#Checking for NAs
count(is.na(price_data))

#Summary of Bitcoin
summary(price_data)
head(price_data,5)

#Changing the date variable to date format
Aave$Date<- anydate(Aave$Date)
BinanceCoin$Date<- anydate(BinanceCoin$Date)
Bitcoin$Date <- anydate(Bitcoin$Date)
Bitcoin$Volume <- as.numeric(Bitcoin$Volume)
Cardano$Date <- anydate(Cardano$Date)
ChainLink$Date <- anydate(ChainLink$Date)
Cosmos$Date <- anydate(Cosmos$Date)
Cryptocom$Date <- anydate(Cryptocom$Date)
Dogecoin$Date <- anydate(Dogecoin$Date)
Ethereum$Date <- anydate(Ethereum$Date)
EOS$Date <- anydate(EOS$Date)
Iota$Date <- anydate(Iota$Date)
Litecoin$Date <- anydate(Litecoin$Date)
Monero$Date <- anydate(Monero$Date)
NEM$Date <- anydate(NEM$Date)
Polkadot$Date <- anydate(Polkadot$Date)
Solana$Date <- anydate(Solana$Date)
Stellar$Date <- anydate(Stellar$Date)
Tether$Date <- anydate(Tether$Date)
Tron$Date <- anydate(Tron$Date)
Uniswap$Date <- anydate(Uniswap$Date)
USDCoin$Date <- anydate(USDCoin$Date)
WrappedBitcoin$Date <- anydate(WrappedBitcoin$Date)
XRP$Date <- anydate(XRP$Date)
price_data$Date <- anydate(price_data$Date)


#Timeseries analysis of currencies with respect to closing price over the years 
ggplot() +
  geom_point() +
  geom_line(data = Aave, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = BinanceCoin, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = Bitcoin, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = Cardano, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = ChainLink, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = Cosmos, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = Cryptocom, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = Dogecoin, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = EOS, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = Ethereum, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = Iota, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = Litecoin, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = Monero, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = NEM, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = Polkadot, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = Solana, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = Stellar, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = Tron, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = Uniswap, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = USDCoin, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = WrappedBitcoin, mapping = aes(x = Date, y = Close, colour = Name))+
  geom_line(data = XRP, mapping = aes(x = Date, y = Close, colour = Name))
ggplotly()

ggplot() +
  geom_point() +
  geom_line(data = Aave, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = BinanceCoin, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = Bitcoin, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = Cardano, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = ChainLink, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = Cosmos, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = Cryptocom, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = Dogecoin, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = EOS, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = Ethereum, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = Iota, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = Litecoin, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = Monero, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = NEM, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = Polkadot, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = Solana, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = Stellar, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = Tron, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = Uniswap, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = USDCoin, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = WrappedBitcoin, mapping = aes(x = Date, y = Marketcap, colour = Name))+
  geom_line(data = XRP, mapping = aes(x = Date, y = Marketcap, colour = Name))
ggplotly()


#Correlation and clustering
##Change data frame with row=date and col=close.raw
close_raw <- reshape(price_data[c(1,3,7)], timevar= "Name", idvar = "Date", direction = "wide")
close_raw[,"Close.Name"] <- NULL
close_raw

##Change values into numeric and get it as the dataframe style
close <- data.frame(sapply(close_raw, function(z){as.numeric(as.character(z))}))
#Change names of cols
colnames(close) <- sub("Close.", "", colnames(close))

dim(close)

#close.180 <- close[,colSums(!is.na(close)) >= 180]

corr <- cor(close[,-1], use = "pairwise.complete")
corr

corrplot(corr, order="hclust", diag = FALSE, tl.col = "black", tl.cex =0.6,
         title = "Correlation matrix (ordered by hierarchical clustering)")

#Make correlation matrix between bitcoin and all of alt coins
corr_bitcoin <- corr[1:ncol(corr),"Bitcoin", drop=FALSE]
corrplot(t(corr_bitcoin), diag = FALSE, tl.col = "black", tl.cex = 0.7)

corr_bitcoin_dec_order <- corr_bitcoin[order(corr_bitcoin, decreasing=T),,drop=F]
data.frame(name=corr_bitcoin_dec_order[2:6,0], cor=corr_bitcoin_dec_order[2:6,1])

corr_bitcoin_inc_order <- corr_bitcoin[order(corr_bitcoin, decreasing=F),,drop=F]
data.frame(name=corr_bitcoin_inc_order[1:5,0], cor=corr_bitcoin_inc_order[1:5,1])

#Analysis of marketcap
marketcap <- price_data[c(1,3,9)]
#Change data frame with row=date and col=marketcap
marketcap_raw <- reshape(marketcap, timevar= "Name", idvar = "Date", direction = "wide")
marketcap_raw[,"Marketcap.Name"] <- NULL

#Change values into numeric
marketcap <- sapply(marketcap_raw, function(z){as.numeric(gsub(",","", z))})
#Change names of cols
colnames(marketcap) <- sub("Marketcap.", "", colnames(marketcap))

mean_cap <- data.frame(mean_cap=colMeans(marketcap, na.rm = T))
mean_cap_10_name <- rownames(mean_cap[order(mean_cap$mean_cap, decreasing = T),,drop=F])[1:10]
mean_cap_10_value <- mean_cap[order(mean_cap$mean_cap, decreasing = T),,drop=F][1:10,]
mean_cap_10 <- data.frame(name=mean_cap_10_name, mean_marketcap=mean_cap_10_value)
mean_cap_10

ggplot(mean_cap_10, aes(reorder(name,-mean_marketcap),mean_marketcap)) +
  geom_bar(stat = "summary", fun.y = "mean")  + 
  xlab('Currency name') + ylab('Average Marketcap')


#Analysing Volatility of top 5 cryptocurrencies: Bitcoin, Ethereum, Ripple, Binance Coin, Cardano
#subsetting data for a year from 27th Feb 2020 to 27th Feb 2021 
BTC_2020_21 <- Bitcoin[Bitcoin$Date >= "2020-02-27",]
ETH_2020_21 <- Ethereum[Ethereum$Date >= "2020-02-27",]
BNB_2020_21 <- BinanceCoin[BinanceCoin$Date >= "2020-02-27",]
ADA_2020_21 <- Cardano[Cardano$Date >= "2020-02-27",]
XRP_2020_21 <- XRP[XRP$Date >= "2020-02-27",]

#Selecting the required columns
Date <- BTC_2020_21[,"Date"]
BTC_close <- BTC_2020_21[, "Close"]
ETH_close <- ETH_2020_21[, "Close"]
BNB_close <- BNB_2020_21[, "Close"]
ADA_close <- ADA_2020_21[, "Close"]
XRP_close <- XRP_2020_21[, "Close"]

#Creating a single dataframe with the required columns
combined_df <- data.frame(Date, BTC_close,ETH_close,BNB_close,ADA_close,XRP_close)

#Simple Returns
#Calculating Percentage change based on closing price - simple daily returns. 
for (col in names(combined_df)[-1]) {
  symbol <- str_sub(col, 1, -7)
  new_col_name <- paste(symbol, "simple.returns.pct")
  col_values <- combined_df[[col]]
  combined_df[[new_col_name]] <- 100*(col_values - lag(col_values))/lag(col_values)
}

combined_df <- na.omit(combined_df)

combined_df <- select(combined_df, Date, `BTC simple.returns.pct`:`XRP simple.returns.pct`)
names(combined_df)[-1] <- str_sub(names(combined_df)[-1], 1, -20)
combined_df <- gather(combined_df, key = "Symbol", value = "Simple Returns %", BTC:XRP)

ggplot(data = combined_df) +
  geom_line(aes(x = Date, y = `Simple Returns %`)) +
  xlab("Year: Feb'20-Feb'21") +
  facet_wrap("Symbol", nrow  = 2) +
  scale_x_date(date_labels = "%b")

#Cumulative returns using prices 
cum_return_pct <- 100*((tail(combined_df[-1],n=1) - head(combined_df[-1],n=1))/ head(combined_df[-1],n=1))
cum_return_pct <- as.data.frame(t(cum_return_pct))
cum_return_pct$Currency <- rownames(cum_return_pct)
cum_return_pct      <- melt(cum_return_pct, id.vars=c("Currency"))
cum_return_pct <- pivot_wider(cum_return_pct, names_from = variable, values_from = value)

ggplot(cum_return_pct, aes(str_sub(Currency,1,-7),cumulative_return)) +
  geom_bar(stat = "summary", fun.y = "mean")  + 
  geom_text(aes(label=round(cumulative_return,4)), vjust=-0.25)+
  xlab('Currency') + ylab('Cumulative Return %')

#Daily Volatility
top5_volatilities<- combined_df %>%
  group_by(Symbol) %>%
  summarise(`Daily Volatility` = sd(`Simple Returns %`))%>%
  arrange(desc(`Daily Volatility`))
top5_volatilities$Name <- c("Ripple","Binance Coin","Cardano","Ethereum","Bitcoin")

ggplot(top5_volatilities, aes(Name,`Daily Volatility`)) +
  geom_bar(stat = "summary", fun.y = "mean")  + 
  geom_text(aes(label=`Daily Volatility`), vjust=-0.25)+
  xlab('Currency Name') + ylab('Daily Volatility')

#Bitcoin Analysis
#Bitcoin Opening Price
ggplot(Bitcoin, aes(Bitcoin$Date, Bitcoin$Open)) + 
  geom_line(color='green') + scale_x_date("Year")+ ylim(0,20000) + ylab("Opening Price")+
  ggtitle("Bitcoin Opening Price")

#Bitcoin Closing Price
ggplot(Bitcoin, aes(Bitcoin$Date, Bitcoin$Close)) + 
  geom_line(color='red') + scale_x_date("Year")+ ylim(0,20000) + ylab("Closing Price")+
  ggtitle("Bitcoin Closing Price")


#Boxplot and Hist of Closing Price
breaks<-c(50,1000,3000,5000,10000,15000,20000)
labels<-c("50-1000","1000-3000","3000-5000","5000-10000","10000-15000","15000-20000")
bins <- cut(Bitcoin$Close,breaks,include.lowest = T,right = F,labels = labels)
plot(bins,col=2, main="Bitcoin Closing Price", xlab="Prices", ylab="Frequency")

boxplot(Bitcoin$Close,col=4, main="Boxplot of Bitcoin CP", xlab="Closing Price",ylab="Prices" )

#Correlation among variables
pairs(data=Bitcoin,
      ~ Volume + Marketcap + High + Low + Open + Close)


#Comparison of Volume and Marketcap for the month of Feb in 2021
ggplot() +
  geom_point() +
  geom_line(data = Bitcoin[(Bitcoin$Date > "2021-02-01"),], mapping = aes(x = Date, y = Volume,colour = Name ))+
  geom_line(data = Ethereum[(Ethereum$Date > "2021-02-01"),], mapping = aes(x = Date, y = Volume,colour = Name))+
  geom_line(data = BinanceCoin[(BinanceCoin$Date > "2021-02-01"),], mapping = aes(x = Date, y = Volume,colour = Name))+
  geom_line(data = Tether[(Tether$Date > "2021-02-01"),], mapping = aes(x = Date, y = Volume,colour = Name))+
  geom_line(data = Dogecoin[(Dogecoin$Date > "2021-02-01"),], mapping = aes(x = Date, y = Volume,colour = Name))+
  geom_line(data = XRP[(XRP$Date > "2021-02-01"),], mapping = aes(x = Date, y = Volume,colour = Name))+
  geom_line(data = Litecoin[(Litecoin$Date > "2021-02-01"),], mapping = aes(x = Date, y = Volume,colour = Name))+
  ggtitle("Volume") + ylab("Prices")
ggplotly()

ggplot() +
  geom_point() +
  geom_line(data = Bitcoin[(Bitcoin$Date > "2021-02-01"),], mapping = aes(x = Date, y = Marketcap,colour = Name ))+
  geom_line(data = Ethereum[(Ethereum$Date > "2021-02-01"),], mapping = aes(x = Date, y = Marketcap,colour = Name))+
  geom_line(data = BinanceCoin[(BinanceCoin$Date > "2021-02-01"),], mapping = aes(x = Date, y = Marketcap,colour = Name))+
  geom_line(data = Tether[(Tether$Date > "2021-02-01"),], mapping = aes(x = Date, y = Marketcap,colour = Name))+
  geom_line(data = Dogecoin[(Dogecoin$Date > "2021-02-01"),], mapping = aes(x = Date, y = Marketcap,colour = Name))+
  geom_line(data = XRP[(XRP$Date > "2021-02-01"),], mapping = aes(x = Date, y = Marketcap,colour = Name))+
  geom_line(data = Litecoin[(Litecoin$Date > "2021-02-01"),], mapping = aes(x = Date, y = Marketcap,colour = Name))+
  ggtitle("Marketcap") + ylab("Prices")
ggplotly()

#Regression Model 1 for learning relations within the Bitcoin dataset
reg_mod_btc <- lm(Close~Open+High+Low+Volume+Marketcap, data = Bitcoin)
summary(reg_mod_btc)

reg_mod_btc %>%
  ggplot(aes(x = reg_mod_btc$fitted.values, y = reg_mod_btc$residuals)) +
  geom_point() + 
  ggtitle("Residuals vs Fitted values") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Fitted Values", y = "Residuals")

#Prediction Model
set.seed(1)
train.index<-sample(1:nrow(Bitcoin),0.70*nrow(Bitcoin), replace=FALSE)
train <- Bitcoin[train.index, ]
test  <- Bitcoin[-train.index,]

ggplot(train, aes(train$Date, train$Close)) +
  geom_line(color = 'blue') + scale_x_date("Year")+ ylim(0,20000) + ylab("Closing Price") + 
  ggtitle('Train Dataset')

ggplot(test, aes(test$Date, test$Close)) +
  geom_line(color = 'red') + scale_x_date("Year")+ ylim(0,20000) + ylab("Closing Price") + 
  ggtitle('Test Dataset')


##Volume as predictor
model1 <- lm(Close~Volume, data=train)
summary(model1)
prediction <- predict(model1,test)
head(p1)
error1 <- p1 - test[["Close"]]
sqrt(mean(error1^2))
plot(p1)

##Volume,High,Low,Open,Marketcap as predictors
model2 <- lm(Close~Volume+High+Low+Open+Marketcap, data=train)
summary(model2)
p2 <- predict(model2,test)
head(p2)
error2 <- p2 - test[["Close"]]
sqrt(mean(error2^2))
plot(p2)


