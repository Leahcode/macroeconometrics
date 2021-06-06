### Macroeconomics Project ###

rm(list = ls())

# read data
oil_prices <- read_csv("Downloads/oil-import-prices-all-countries.csv")

# drop all countries, keep Austria
temp_oil_prices_aut <- oil_prices[which(oil_prices$LOCATION=='AUT'),]

# delete unnecessary columns
temp <- c("TIME", "Value")
oil_prices_aut <- temp_oil_prices_aut[temp]
rm(temp_oil_prices_aut)

# plot 1st order difference
plot(oil_prices_aut[1:40], oil_prices_aut[2:41], 'l') #does this work? 
plot(oil_prices_aut$Value[1:40], oil_prices_aut$Value[2:41],)

# info
mean(oil_prices_aut$Value)
sd(oil_prices_aut$Value)

#AR1 process
Y <- oil_prices_aut
ar1 <- Y$Value
for(ii in 2:41){ar1[ii] <- 0.8*ar1[ii-1]}
# better to use: for(ii in 2:41){ar1[ii] <- 0.8*ar1[ii-1]+eps[ii]} but I couldn't find an error term 
acf(ar1,lwd=3)
lines(0:30, 0.8^(0:30), col = 'red', lwd = 2)
plot (ar1[1:40],ar1[2:41])

#MA1 process
ma1 <- Y$Value[2:41]+0.8*Y$Value[1:40]
plot(1:40, ma1, col = 'blue', type = 'l')
acf(ma1, col = 'red', lwd = 3)
mean(ma1); var(ma1)
