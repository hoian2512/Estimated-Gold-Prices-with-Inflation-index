#PACKAGES
library(boot)
library(MASS)
library(rcompanion)
library(MCMCpack)
library(Rcpp)
library(rstanarm)
library(BAS)
library(e1071)
library(ggplot2)
library(ggpubr)

#NHAP DU LIEU
gold_data = read.csv("Gold Prices VN 2021.csv", header = TRUE)
CPI_Data = read.csv("CPI_Data.csv", header = TRUE)
GMD = read.csv("GP_modified.csv", header = TRUE)

gold_prices_sell = gold_data$GI..B.N[0:84]
gold_prices_buy = gold_data$GI..MUA[0:84]
INFL = CPI_Data$Inflation

#THONG KE MO TA
summary(gold_prices_buy)
summary(gold_prices_sell)
summary(INFL)
sd(gold_prices_buy)
sd(gold_prices_sell)
sd(INFL)

#BIEN DOI BO DU LIEU 
GPB_norm = GMD$Buy[0:84]
GPS_norm = GMD$Sell[0:84]
INF_norm = log10(INFL)

summary(GPB_norm)
summary(GPS_norm)
summary(INF_norm)
sd(GPB_norm)
sd(GPS_norm)
sd(INF_norm)

plotNormalHistogram(GPB_norm, main = "Return of VN Gold Buy Prices 2021")
plotNormalHistogram(GPS_norm, main = "Return of VN Gold Sell Prices 2021")
plotNormalHistogram(INF_norm, main = "Return of Inflation from 2015 to 2021")

#DO TINH CHUAN
skewness(GPB_norm)
skewness(GPS_norm)
skewness(INF_norm)
kurtosis(GPB_norm)
kurtosis(GPS_norm)
kurtosis(INF_norm)

#KET QUA DU BAO
Data_all = data.frame(Buy = GPB_norm[0:84],
                      Sell = GPS_norm[0:84],
                      Infla = INF_norm[0:84])
Data_all
Test1 = stan_glm(Buy ~ Infla, data = Data_all)
summary(Test1)
prior_summary(Test1)
posterior.r2 = bayes_R2(Test1)
summary(posterior.r2)

posterior.r2 = data.frame(posterior.r2)
ggplot(data = posterior.r2, aes(x=posterior.r2))+
geom_histogram(col="White", fill="Blue")

posterior = MCMCregress(Buy ~ Infla, data = Data_all, b0 = c(0, 0), B0 = c(0.01, 0.01))
plot(posterior)
summary(posterior)

Test2 = stan_glm(Sell ~ Infla, data = Data_all)
summary(Test2)
prior_summary(Test2)
posterior1.r2 = bayes_R2(Test2)
summary(posterior1.r2)

posterior1.r2 = data.frame(posterior1.r2)
ggplot(data = posterior1.r2, aes(x=posterior1.r2))+
  geom_histogram(col="White", fill="Blue")

posterior1 = MCMCregress(Sell ~ Infla, data = Data_all, b0 = c(0, 0), B0 = c(0.01, 0.01))
plot(posterior1)
summary(posterior1)

