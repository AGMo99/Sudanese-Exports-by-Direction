#Import Librarie That We Will Use
library(readxl)
library(AER)
library(lmtest)
library(tseries)
library(dynlm)
library(ggplot2)
library(dplyr)

#Load The Pure Data

SDoE <- read.csv('C:/Users/AG/Documents/Sudan Direction of Export Data.csv')

Net_foreign_Trade <- read.csv('C:/Users/AG/Documents/Net Foreign Trade.csv')
View(SDoE)
#Select The Data That We Will Work on It

SDoE <- SDoE[, 2:33]

Net_foreign_Trade <- Net_foreign_Trade[, 2]

DataUse <- data.frame(SDoE, Net_foreign_Trade)

#Plot The Data

matplot(Net_foreign_Trade, type = 'l')

#West Eourpe
matplot(SDoE[, 1:8], type = 'l', ylim = c(0, 60))
legend('topleft', legend = c('UK', 'France', 'Germany', 'Italy', 'Holland', 'Belguim', 'Sweden', 'Spain'), bty = 'n')
#East Europe
matplot(SDoE[, 10:13], type = 'l')
legend('topleft', legend = c('Russia', 'Romania', 'Bulgaria'), bty = 'n')
#Asian
matplot(SDoE[, 14:19], type = 'l')
legend('topleft', legend = c('Thailand', 'Japan', 'China', 'Bangladish', 'South Korea'), bty = 'n')
#African
matplot(SDoE[, 20:21], type = 'l')
legend('topleft', legend = c('Comesa'), bty = 'n')
#westren Hemisphere
matplot(SDoE[, 22:24], type = 'l')
legend('topleft', legend = c('United States', 'Brazil'), bty = 'n')
#Arab
matplot(SDoE[, 25:31], type = 'l')
legend('topleft', legend = c('Saudi Arabia', 'Yamen', 'UAE', 'Jordan', 'Syria', 'Iraq'), bty = 'n')
#Net
matplot(SDoE[, 32], type = 'l')
legend('topleft', legend = c('Net'), bty = 'n')

#Correlation Matrix For Each Column

CorrMat <- cor(DataUse)
View(CorrMat > 0.5)
#UK with Spain and Thailand
#France With S.Korea, Romania, Bulgaria, US, Saudi Arabia and Germany
#Germany With France, Thailand, US, Iraq and Comesa
#Italy With Japan and China
#Holland With Bangladish and Yamen
#Belguim With Other.Weatern.Hemisphere and Syria
#Sweden With UAE
#Spain With US, UK, Thailand
#Romania With France, Bulgaria, Other.East.Europe.Countries, South.Korea, Other.Asian.Countries, Comesa, Saudi.Arabia, Jordan and Other.Arab.Countries
#Bulgaria With France, Other weast eourope, Other east eourope, Romania, S.Korea, Brazil and Jordan
#Yemen with Sweden, China and Bangladesh


#Check For Stationary

adftest <- lapply(DataUse, adf.test, k=0)

#Check For ACF and PACF

acftset <- lapply(DataUse, acf)

pacftest <- lapply(DataUse, pacf)

#Clean nThe Data

DataClean <- lapply(DataUse, diff, lag=1)

#Chack ADF Agian

adftest2 <- lapply(DataClean, adf.test, k=0)

#Check For ACF and PACF Agian

acftset2 <- lapply(DataClean, acf)

pacftest2 <- lapply(DataClean, pacf)

#Choose the X's

cor(as.data.frame(DataClean), DataClean$Net_foreign_Trade)

#The Model

Model <- dynlm(Net_foreign_Trade ~ China, data = DataClean, weights = 1/sqrt(abs(DataClean$Net_foreign_Trade)))
res <- resid(Model)^2
ARCH <- dynlm(res ~ L(res))

summary(ARCH)

#Check The Model

#Heteroscedasitcity

white.test(DataClean$Net_foreign_Trade, DataClean$China)

#Autocorrelation

bgtest(Model)
