########################### Trabalho de Econometria II ##########################################
##### Packages
library('eia')
library('ggplot2')
library('ggfortify')
library('tidyr')
library('dplyr')
library('tseries')
library('forecast')
library('stargazer')
library('vars')


######### Coletando e observando os dados de consumo de Eletricidade em Homestead, Florida
##
# Utilizando a chave do API da EIA (US Energy Information Administration)
eia_set_key('f66f49cb811a2f2a0c6f7bf340b71bd8')


# ID da série com demanda horária por eletricidade em Homestead, Florida
seriesidelechomestead <- 'EBA.HST-ALL.D.H'


# Importando todos os dados da série
serieeiahomestead <- eia_series(seriesidelechomestead, n = 52560)


# Criando um Data Frame com os dados relevantes, e removendo os irrelevantes
dadoseia <- data.frame(serieeiahomestead$data)
dadoseia <- data.frame(Tempo = dadoseia$date , Megawatts = dadoseia$value)
rm(seriesidelechomestead)


## Salvando um arquivo com os dados obtidos acima, pois os dados são atualizados em tempo real
#save(dadoseia, file = '/SSD2/Estudo/Trabalhos/Econometria II/Dados/dadoseia.Rda')
load(file = '/SSD2/Estudo/Trabalhos/Econometria II/Dados/dadoseia.Rda')


# Conferindo se há NAs nos dados e substituindo pela média da série
numerodenas <- sum(is.na(dadoseia$Megawatts))
meanseries <- mean(dadoseia$Megawatts, na.rm = TRUE)


# Trocando os valores NAs pelo valor de média da amostra
dadoseia[is.na(dadoseia)] <- meanseries
numerodenas <- sum(is.na(dadoseia$Megawatts))
numerodezeros <- sum(dadoseia$Megawatts == 0)


# Fazendo um gráfico para observar o comportamento da série original
graficopuroeia <- ggplot(dadoseia) +
  labs(title = 'Histórico de Consumo de Energia Elétrica', 
       x = 'Tempo' , y = 'Megawatts',
       caption = 'Criado com dados da
       U.S Energy Information Administration') +
  geom_line(aes(x=Tempo, y=Megawatts, color='Consumo')) +
#  geom_point(aes(x=t, y=Galinhas, color='Galinhas')) +
#  geom_line(aes(x=t, y=Ovos, color='Ovos')) +
#  geom_point(aes(x=t, y=Ovos, color='Ovos')) +
  scale_color_manual(values = c('#ff7518'), name = 'Legenda')

graficopuroeia


# Para testar quantos valores abaixo de um determinado valor existem na série
test <- dadoseia$Megawatts < 10
sum(test == TRUE)


# defininfo uma série de tempo para os dados de energia
eiahomestead <- ts(data = dadoseia$Megawatts, start = 2016, end = 2021, frequency = 365*24)


# Testando para presençca de raiz unitária
adftesteiahtestclass48 <- adf.test(eiahomestead, k = 48)
adftesteiahtestclass24 <- adf.test(eiahomestead, k = 24)
adftesteiahtestclass10 <- adf.test(eiahomestead, k = 10)
adftesteiahtestclass9 <- adf.test(eiahomestead, k = 9)
adftesteiahtestclass8 <- adf.test(eiahomestead, k = 8)
adftesteiahtestclass7 <- adf.test(eiahomestead, k = 7)
adftesteiahtestclass6 <- adf.test(eiahomestead, k = 6)
adftesteiahtestclass5 <- adf.test(eiahomestead, k = 5)
adftesteiahtestclass4 <- adf.test(eiahomestead, k = 4)
adftesteiahtestclass3 <- adf.test(eiahomestead, k = 3)
adftesteiahtestclass2 <- adf.test(eiahomestead, k = 2)
adftesteiahtestclass1 <- adf.test(eiahomestead, k = 1)


# Os dados são estacionários no ADF test. Abaixo são removidos os testes e objetos
# desnecessários
rm(adftesteiahtestclass48)
rm(adftesteiahtestclass24)
rm(adftesteiahtestclass10)
rm(adftesteiahtestclass9)
rm(adftesteiahtestclass8)
rm(adftesteiahtestclass7)
rm(adftesteiahtestclass6)
rm(adftesteiahtestclass5)
rm(adftesteiahtestclass4)
rm(adftesteiahtestclass3)
rm(adftesteiahtestclass2)
rm(adftesteiahtestclass1)
rm(eiahomestead)


## Transformando os dados em dados diários
# Removendo algumas linhas para tornar os horários de meia noite a meia noite
dadoseia <- dadoseia[-c(52556:52560),]
dadoseia <- dadoseia[-c(1:18),]
dadoseia <- data.frame(Tempo = dadoseia$Tempo, Megawatts = dadoseia$Megawatts)


# Removendo mais linhas para estar no mesmo tempo que os dados da força aérea
dadoseia <- dadoseia[-c(51554:52537),]
dadoseia <- dadoseia[-c(1:72),]
dadoseia <- data.frame(Tempo = dadoseia$Tempo, Megawatts = dadoseia$Megawatts)


# Colocando o Data Frame em ordem crescente de tempo
dadoseia <- dadoseia[nrow(dadoseia):1,]
dadoseia <- data.frame(Tempo = dadoseia$Tempo, Megawatts = dadoseia$Megawatts)


# Criando um vetor com os valores diários de gasto de energia
valores <- dadoseia$Megawatts
medias <- c(1:2147)
for(i in c(1:2147)){medias[i] = valores[24*i] + valores[24*i-1] + valores[24*i-2]
                        + valores[24*i-3] + valores[24*i-4] + valores[24*i-5]
                        + valores[24*i-6] + valores[24*i-7] + valores[24*i-8]
                        + valores[24*i-9] + valores[24*i-10] + valores[24*i-11]
                        + valores[24*i-12] + valores[24*i-13] + valores[24*i-14]
                        + valores[24*i-15] + valores[24*i-16] + valores[24*i-17]
                        + valores[24*i-18] + valores[24*i-19] + valores[24*i-20]
                        + valores[24*i-21] + valores[24*i-22] + valores[24*i-23]}
mediasmean <- mean(na.omit(medias))
medias[2146] <- 242
medias[2147] <- 242

##
#########


################################################################################


######### Coletando e observando os dados de temperatura em Homestead, Florida
##
# Importando os dados (2016-01-01 a 2021-11-16)
HFLAirForce <- read.csv('/SSD2/Dados/Homestead Climate/HomesteadDailyNOAA.csv')


# Aparentemente temos datas faltando nos dados da Air Force
d <- HFLAirForce$DATE
d <- as.Date(d)
date_range <- seq(min(d), max(d), by =1)
datasfaltantes <- date_range[!date_range %in% d]


# Vamos criar um data Frame com os dados relevantes
AirForceTEMP <- data.frame(Tempo = HFLAirForce$DATE,
                           Temperatura = HFLAirForce$TOBS,
                           TMAX = HFLAirForce$TMAX,
                           TMIN = HFLAirForce$TMIN)
rm(HFLAirForce)


# Adicionando linhas nas datas faltantes nos dados e preenchendo-os
AirForceTEMP <- AirForceTEMP %>%
  mutate(Tempo = as.Date(Tempo)) %>%
  complete(Tempo = seq.Date(min(Tempo), max(Tempo), by='day')) %>%
  fill(Temperatura) %>%
  fill(TMIN) %>%
  fill(TMAX) 


# Criando um data frame com os dados relevantes
df <- data.frame(Tempo = as.Date(AirForceTEMP$Tempo),
                 Temperatura = AirForceTEMP$Temperatura,
                 TMIN = AirForceTEMP$TMIN,
                 TMAX = AirForceTEMP$TMAX,
                 Megawatts = medias)


# Limpando o environment
rm(AirForceTEMP)
rm(dadoseia)
rm(serieeiahomestead)
rm(d)
rm(datasfaltantes)
rm(date_range)
rm(i)
rm(meanseries)
rm(numerodenas)
rm(numerodezeros)
rm(test)
rm(valores)
rm(medias)


# Efetuando um gráfico com temperatura e consumo de energia
graficoconselec <- ggplot(df) +
  labs(title = 'Consumo de Energia Elétrica e Temperatura', 
       x = 'Tempo' , y = 'Fahrenheit, Megawatts',
       caption = 'Criado com dados da
       Global Historical Climatology Network e
       U.S Energy Information Administration') +
  geom_line(aes(x = Tempo, y = Megawatts, color='Consumo')) +
  geom_line(aes(x = Tempo, y = Temperatura, color='Temperatura')) +
  scale_color_manual(values = c('#ff7518','#2e8b57'), name = 'Legenda')

graficoconselec


# Efetuando os testes de raiz unitária
adftestairtemphtestclass48 <- adf.test(df$Temperatura, alternative = 'stationary', k = 48)
adftestairtemphtestclass24 <- adf.test(df$Temperatura, alternative = 'stationary', k = 24)
adftestairtemphtestclass10 <- adf.test(df$Temperatura, alternative = 'stationary', k = 10)
adftestairtemphtestclass9 <- adf.test(df$Temperatura, alternative = 'stationary', k = 9)
adftestairtemphtestclass8 <- adf.test(df$Temperatura, alternative = 'stationary', k = 8)
adftestairtemphtestclass7 <- adf.test(df$Temperatura, alternative = 'stationary', k = 7)
adftestairtemphtestclass6 <- adf.test(df$Temperatura, alternative = 'stationary', k = 6)
adftestairtemphtestclass5 <- adf.test(df$Temperatura, alternative = 'stationary', k = 5)
adftestairtemphtestclass4 <- adf.test(df$Temperatura, alternative = 'stationary', k = 4)
adftestairtemphtestclass3 <- adf.test(df$Temperatura, alternative = 'stationary', k = 3)
adftestairtemphtestclass2 <- adf.test(df$Temperatura, alternative = 'stationary', k = 2)
adftestairtemphtestclass1 <- adf.test(df$Temperatura, alternative = 'stationary', k = 1)


# Os dados são estacionários no caso do ADF test
rm(adftestairtemphtestclass48)
rm(adftestairtemphtestclass24)
rm(adftestairtemphtestclass10)
rm(adftestairtemphtestclass9)
rm(adftestairtemphtestclass8)
rm(adftestairtemphtestclass7)
rm(adftestairtemphtestclass6)
rm(adftestairtemphtestclass5)
rm(adftestairtemphtestclass4)
rm(adftestairtemphtestclass3)
rm(adftestairtemphtestclass2)
rm(adftestairtemphtestclass1)
rm(mediasmean)


# Fazendo os correlogramas
ACorrelogramamwatts <- acf(df$Megawatts, lag.max = 750)
ACorrelogramatemperatura <- acf(df$Temperatura, lag.max = 750)
PACorrelogramamwatts <- pacf(df$Megawatts, lag.max = 750)
PACorrelogramatemperatura <- pacf(df$Temperatura, lag.max = 750)


# Fazendo os gráficos dos correlogramas
GraficoACFMegawatts <- autoplot(ACorrelogramamwatts, ci= 0.95) +
  labs(title = 'Consumo de Energia Elétrica', 
       x = 'Lags' , y = 'Autocorrelação Estimada',
       caption = 'Criado com dados da
       U.S Energy Information Administration')

GraficoACFTemperatura <- autoplot(ACorrelogramatemperatura, ci = 0.95) + 
  labs(title = 'Temperatura em Homestead', 
       x = 'Lags' , y = 'Autocorrelação Estimada',
       caption = 'Criado com dados da
       Global Historical Climatology Network')

GraficoPACFMegawatts <- autoplot(PACorrelogramamwatts, ci= 0.95) + 
  labs(title = 'Consumo de Energia Elétrica', 
       x = 'Lags' , y = 'A. Parcial Estimada', 
       caption = 'Criado com dados da
       U.S Energy Information Administration')

GraficoPACFTemperatura <- autoplot(PACorrelogramatemperatura, ci = 0.95) + 
  labs(title = 'Temperatura em Homestead', 
       x = 'Lags' , y = 'A. Parcial Estimada',
       caption = 'Criado com dados da
       Global Historical Climatology Network')

GraficoACFTemperatura
GraficoACFMegawatts
GraficoPACFTemperatura
GraficoPACFMegawatts


# Limpando o Environment
rm(ACorrelogramamwatts)
rm(ACorrelogramatemperatura)
rm(PACorrelogramamwatts)
rm(PACorrelogramatemperatura)


##
#########


################################################################################


######### Demonstrando que deve ser utilizada Primeira diferença
##

# Definindo as séries de tempo
tsconsumo <- ts(df$Megawatts, start = 2016, end = 2021, frequency = 365)
tstemperatura <- ts(df$Temperatura, start = 2016, end = 2021, frequency = 365)


# "Decompondo" os dados
consumodecompostoadditive <- decompose(tsconsumo, type = 'additive')
consumodecompostomultiplicative <- decompose(tsconsumo, type = 'multiplicative')
temperaturadecompostoadditive <- decompose(tstemperatura, type = 'additive')
temperaturadecompostomultiplicative <- decompose(tstemperatura, type = 'multiplicative')

graficodecomposicaoconsumoA <- autoplot(consumodecompostoadditive)
graficodecomposicaoconsumoM <- autoplot(consumodecompostomultiplicative)
graficodecomposicaotemperaturaA <- autoplot(temperaturadecompostoadditive)
graficodecomposicaotemperaturaM <- autoplot(temperaturadecompostomultiplicative)


# Para conferir novemente se há estacionariedade ou não
testekpssconsumo <- kpss.test(tsconsumo)
testekpsstemperatura <- kpss.test(tstemperatura)


# Tirando as diferenças até encontrar estacionariedade
consumo1D <- diff(df$Megawatts, differences = 1)
consumo2D <- diff(df$Megawatts, differences = 2)
consumo3D <- diff(df$Megawatts, differences = 3)
consumo4D <- diff(df$Megawatts, differences = 4)
consumo5D <- diff(df$Megawatts, differences = 5)
Temperatura1D <- diff(df$Temperatura, differences = 1)
Temperatura2D <- diff(df$Temperatura, differences = 2)
Temperatura3D <- diff(df$Temperatura, differences = 3)
Temperatura4D <- diff(df$Temperatura, differences = 4)
Temperatura5D <- diff(df$Temperatura, differences = 5)


# Efetuando o teste kpss para cada diferença
testekpssconsumod1 <- kpss.test(consumo1D)
testekpssconsumod2 <- kpss.test(consumo2D)
testekpssconsumod3 <- kpss.test(consumo3D)
testekpssconsumod4 <- kpss.test(consumo4D)
testekpssconsumod5 <- kpss.test(consumo5D)
testekpsstemperaturad1 <- kpss.test(Temperatura1D)
testekpsstemperaturad2 <- kpss.test(Temperatura2D)
testekpsstemperaturad3 <- kpss.test(Temperatura3D)
testekpsstemperaturad4 <- kpss.test(Temperatura4D)
testekpsstemperaturad5 <- kpss.test(Temperatura5D)


# Efetuando o gráfico de autocorrelação para cada caso
plot(acf(tsconsumo))
plot(acf(consumo1D))
plot(acf(consumo2D))
plot(acf(consumo3D))
plot(acf(consumo4D))
plot(acf(consumo5D))
plot(acf(tstemperatura))
plot(acf(Temperatura1D))
plot(acf(Temperatura2D))
plot(acf(Temperatura3D))
plot(acf(Temperatura4D))
plot(acf(Temperatura5D))


# Efetuando o teste ADF para estacionariedade nas primeiras diferenças
adf.test(consumo1D)
adf.test(Temperatura1D)


# Como o teste ADF indica estacionariedade nos dados puros e nos dados em
#primeira diferença, e, além disso na primeira diferença temos o correlograma
#típico de um processo estacionário, vamos assumir que a primeira diferença 
#está relacionada a um processo estacionário, apesar do teste kpss.


# Abaixo foi definido um data frame com a primeira diferença
df <- df[-c(1),]
df <- data.frame(Tempo = as.Date(df$Tempo), Megawatts = df$Megawatts, Temperatura = df$Temperatura, DeltaTemperatura = Temperatura1D, DeltaMegawatts = consumo1D)


# Aqui foram feitos os gráficos de autocorrelação
graficoACFconselec1D <- autoplot(acf(consumo1D, lag.max = 750)) +
  labs(title = 'Consumo (1° Diferença)', 
       x = 'Lags' , y = 'Autocorrelação Estimada', 
       caption = 'Criado com dados da
       U.S Energy Information Administration')
  
graficoPACFconselec1D <- autoplot(pacf(consumo1D, lag.max = 750)) +
  labs(title = 'Consumo (1° Diferença)', 
       x = 'Lags' , y = 'A. Parcial Estimada', 
       caption = 'Criado com dados da
       U.S Energy Information Administration')
  
graficoACFTemperatura1D <- autoplot(acf(Temperatura1D, lag.max = 750)) +
  labs(title = 'Temperatura (1° Diferença)', 
       x = 'Lags' , y = 'Autocorrelação Estimada',
       caption = 'Criado com dados da
       Global Historical Climatology Network')

graficoPACFtemperatura1D <- autoplot(pacf(Temperatura1D, lag.max = 750)) +
  labs(title = 'Temperatura (1° Diferença)', 
       x = 'Lags' , y = 'A. Parcial Estimada',
       caption = 'Criado com dados da
       Global Historical Climatology Network')

graficoACFconselec1D
graficoPACFconselec1D
graficoACFTemperatura1D
graficoPACFtemperatura1D


# Limpando o environment
rm(consumodecompostoadditive)
rm(consumodecompostomultiplicative)
rm(testekpssconsumo)
rm(testekpssconsumod1)
rm(testekpssconsumod2)
rm(testekpssconsumod3)
rm(testekpssconsumod4)
rm(testekpssconsumod5)
rm(testekpsstemperatura)
rm(testekpsstemperaturad1)
rm(testekpsstemperaturad2)
rm(testekpsstemperaturad3)
rm(testekpsstemperaturad4)
rm(testekpsstemperaturad5)
rm(consumo1D)
rm(consumo2D)
rm(consumo3D)
rm(consumo4D)
rm(consumo5D)
rm(Temperatura1D)
rm(Temperatura2D)
rm(Temperatura3D)
rm(Temperatura4D)
rm(Temperatura5D)
rm(tsconsumo)
rm(tstemperatura)


# Definindo as séries de tempo relevantes para estimativas
tsConsumo <- ts(data = df$Megawatts, start = 2016, end = 2021, frequency = 365)
tsTemperatura <- ts(data = df$Temperatura, start = 2016, end = 2021, frequency = 365)


# Vamos efetuar fits assumindo modelos ARIMA's sazonais (consumo)

# Para lidar com grandes dados
mstsConsumo <- msts(tsConsumo, start = 2016, seasonal.periods = c(365, 365*2, 365*3, 365*4, 365*5, 365*6))
mstsConsumo <- msts(tsTemperatura, start = 2016, seasonal.periods = c(365, 365*2, 365*3, 365*4, 365*5, 365*6))


# Utilizando a função auto arima com K=1
fourierC1 <- fourier(tsConsumo, K=1)
Cautoarima1 <- auto.arima(tsConsumo, xreg = as.matrix(fourierC1), stepwise = FALSE)
summary(Cautoarima1)


# Utilizando a função auto arima com K=2
fourierC2 <- fourier(tsConsumo, K=2)
Cautoarima2 <- auto.arima(tsConsumo, xreg = as.matrix(fourierC2), stepwise = FALSE)
summary(Cautoarima2)


# Utilizando a função auto arima com K=3
fourierC3 <- fourier(tsConsumo, K=3)
Cautoarima3 <- auto.arima(tsConsumo, xreg = as.matrix(fourierC3), stepwise = FALSE)
summary(Cautoarima3)


# Utilizando a função auto arima com K=4
fourierC4 <- fourier(tsConsumo, K=4)
Cautoarima4 <- auto.arima(tsConsumo, xreg = as.matrix(fourierC4), stepwise = FALSE)
summary(Cautoarima4)


# Utilizando a função auto arima com K=5
fourierC5 <- fourier(tsConsumo, K=5)
Cautoarima5 <- auto.arima(tsConsumo, xreg = as.matrix(fourierC5), stepwise = FALSE)
summary(Cautoarima5)


# Utilizando a função auto arima com K=6
fourierC6 <- fourier(tsConsumo, K=6)
Cautoarima6 <- auto.arima(tsConsumo, xreg = as.matrix(fourierC6), stepwise = FALSE)
summary(Cautoarima6)


# Vamos efetuar fits assumindo modelos ARIMA's sazonais (consumo) com SPETWISE

# Para lidar com grandes dados
mstsConsumo <- msts(tsConsumo, start = 2016, seasonal.periods = c(365, 365*2, 365*3, 365*4, 365*5, 365*6))
mstsConsumo <- msts(tsTemperatura, start = 2016, seasonal.periods = c(365, 365*2, 365*3, 365*4, 365*5, 365*6))


# Utilizando a função auto arima com K=1
SfourierC1 <- fourier(tsConsumo, K=1)
SCautoarima1 <- auto.arima(tsConsumo, xreg = as.matrix(SfourierC1), stepwise = TRUE)
summary(SCautoarima1)


# Utilizando a função auto arima com K=2
SfourierC2 <- fourier(tsConsumo, K=2)
SCautoarima2 <- auto.arima(tsConsumo, xreg = as.matrix(SfourierC2), stepwise = TRUE)
summary(SCautoarima2)


# Utilizando a função auto arima com K=3
SfourierC3 <- fourier(tsConsumo, K=3)
SCautoarima3 <- auto.arima(tsConsumo, xreg = as.matrix(SfourierC3), stepwise = TRUE)
summary(SCautoarima3)


# Utilizando a função auto arima com K=4
SfourierC4 <- fourier(tsConsumo, K=4)
SCautoarima4 <- auto.arima(tsConsumo, xreg = as.matrix(SfourierC4), stepwise = TRUE)
summary(SCautoarima4)


# Utilizando a função auto arima com K=5
SfourierC5 <- fourier(tsConsumo, K=5)
SCautoarima5 <- auto.arima(tsConsumo, xreg = as.matrix(SfourierC5), stepwise = TRUE)
summary(SCautoarima5)


# Utilizando a função auto arima com K=6
SfourierC6 <- fourier(tsConsumo, K=6)
SCautoarima6 <- auto.arima(tsConsumo, xreg = as.matrix(SfourierC6), stepwise = TRUE)
summary(SCautoarima6)


# Os resultados derivados acima podem ser relavantes. Abaixo é limpado o
#environment
rm(fourierC1)
rm(fourierC2)
rm(fourierC3)
rm(fourierC4)
rm(fourierC5)
rm(fourierC6)
rm(SfourierC1)
rm(SfourierC2)
rm(SfourierC3)
rm(SfourierC4)
rm(SfourierC5)
rm(SfourierC6)
rm(Cautoarima1)
rm(Cautoarima2)
rm(Cautoarima3)
rm(Cautoarima4)
rm(Cautoarima5)
rm(Cautoarima6)
rm(SCautoarima1)
rm(SCautoarima2)
rm(SCautoarima3)
rm(SCautoarima4)
rm(SCautoarima5)
rm(SCautoarima6)



##
#########


################################################################################


######### Efetuando a previsão com SARIMA
##


# Definindo uma série de tempo de teste e performance
testetsConsumo <- ts(df$Megawatts, start =2016, end=2019, frequency=365)
performancetsConsumo <- ts(df$Megawatts, start =2019, end=2021, frequency=365)


# Efetuando os códigos para obter os ARIMAS na base de teste (sem stepwise)

# Utilizando a função auto arima com K=1
fouriertesteC1 <- fourier(testetsConsumo, K=1)
Cautoarimateste1 <- auto.arima(testetsConsumo, xreg = as.matrix(fouriertesteC1), stepwise = FALSE)
summary(Cautoarimateste1)


# Utilizando a função auto arima com K=2
fouriertesteC2 <- fourier(testetsConsumo, K=2)
Cautoarimateste2 <- auto.arima(testetsConsumo, xreg = as.matrix(fouriertesteC2), stepwise = FALSE)
summary(Cautoarimateste2)


# Utilizando a função auto arima com K=3
fouriertesteC3 <- fourier(testetsConsumo, K=3)
Cautoarimateste3 <- auto.arima(testetsConsumo, xreg = as.matrix(fouriertesteC3), stepwise = FALSE)
summary(Cautoarimateste3)


# Utilizando a função auto arima com K=4
fouriertesteC4 <- fourier(testetsConsumo, K=4)
Cautoarimateste4 <- auto.arima(testetsConsumo, xreg = as.matrix(fouriertesteC4), stepwise = FALSE)
summary(Cautoarimateste4)


# Utilizando a função auto arima com K=5
fouriertesteC5 <- fourier(testetsConsumo, K=5)
Cautoarimateste5 <- auto.arima(testetsConsumo, xreg = as.matrix(fouriertesteC5), stepwise = FALSE)
summary(Cautoarimateste5)


# Utilizando a função auto arima com K=6
fouriertesteC6 <- fourier(testetsConsumo, K=6)
Cautoarimateste6 <- auto.arima(testetsConsumo, xreg = as.matrix(fouriertesteC6), stepwise = FALSE)
summary(Cautoarimateste6)


# Efetuando os códigos para obter os ARIMAS na base de teste (com stepwise)

# Utilizando a função auto arima com K=1
SfouriertesteC1 <- fourier(testetsConsumo, K=1)
SCautoarimateste1 <- auto.arima(testetsConsumo, xreg = as.matrix(SfouriertesteC1), stepwise = TRUE)
summary(SCautoarimateste1)


# Utilizando a função auto arima com K=2
SfouriertesteC2 <- fourier(testetsConsumo, K=2)
SCautoarimateste2 <- auto.arima(testetsConsumo, xreg = as.matrix(SfouriertesteC2), stepwise = TRUE)
summary(SCautoarimateste2)


# Utilizando a função auto arima com K=3
SfouriertesteC3 <- fourier(testetsConsumo, K=3)
SCautoarimateste3 <- auto.arima(testetsConsumo, xreg = as.matrix(SfouriertesteC3), stepwise = TRUE)
summary(SCautoarimateste3)


# Utilizando a função auto arima com K=4
SfouriertesteC4 <- fourier(testetsConsumo, K=4)
SCautoarimateste4 <- auto.arima(testetsConsumo, xreg = as.matrix(SfouriertesteC4), stepwise = TRUE)
summary(SCautoarimateste4)


# Utilizando a função auto arima com K=5
SfouriertesteC5 <- fourier(testetsConsumo, K=5)
SCautoarimateste5 <- auto.arima(testetsConsumo, xreg = as.matrix(SfouriertesteC5), stepwise = TRUE)
summary(SCautoarimateste5)


# Utilizando a função auto arima com K=6
SfouriertesteC6 <- fourier(testetsConsumo, K=6)
SCautoarimateste6 <- auto.arima(testetsConsumo, xreg = as.matrix(SfouriertesteC6), stepwise = TRUE)
summary(SCautoarimateste6)


## Utilizando o comando forecast para efetuar as previsões do ARIMA

# forecast arima com k=1 sem stepwise
arimaforecastk1 <- forecast(Cautoarimateste1, h=731, xreg = Cautoarimateste1$xreg)
graficoarimaforecastk1 <- autoplot(arimaforecastk1) +
  labs(title = 'Previsão com Modelo SARIMA(0,1,4)(1,1) ', 
       x = 'Tempo' , y = 'Megawatts',
       caption = 'Criado com dados da
       U.S Energy Information Administration')
graficoarimaforecastk1


# forecast arima com k=1 com stepwise
arimaforecastSk1 <- forecast(SCautoarimateste1, h=731, xreg = SCautoarimateste1$xreg)
graficoarimaforecastSk1 <- autoplot(arimaforecastSk1) +
  labs(title = 'Previsão com Modelo SARIMA(5,1,0)(1,1) ', 
       x = 'Tempo' , y = 'Megawatts',
       caption = 'Criado com dados da
       U.S Energy Information Administration')
graficoarimaforecastSk1


# Descobrindo a diferença, em quadrado, entre valores reais e previsão

# SARIMA(0,1,4)(1,1)
Cforecastvectork1 <- arimaforecastk1$mean
forecastdiferencesk1 <- performancetsConsumo - Cforecastvectork1
somadoquadradodasdiferençask1 <- sum(forecastdiferencesk1*forecastdiferencesk1)


# SARIMA(5,1,0)(1,1)
CforecastvectorSk1 <- arimaforecastSk1$mean
forecastdiferencesSk1 <- performancetsConsumo - CforecastvectorSk1
somadoquadradodasdiferençasSk1 <- sum(forecastdiferencesSk1*forecastdiferencesSk1)

# Valor de razão para comparação
razaocomparacaok1sk1 <- somadoquadradodasdiferençask1 / somadoquadradodasdiferençasSk1 

# Limpando o Environment
rm(arimaforecastk1)
rm(arimaforecastSk1)
rm(Cautoarimateste1)
rm(Cautoarimateste2)
rm(Cautoarimateste3)
rm(Cautoarimateste4)
rm(Cautoarimateste5)
rm(Cautoarimateste6)
rm(SCautoarimateste1)
rm(SCautoarimateste2)
rm(SCautoarimateste3)
rm(SCautoarimateste4)
rm(SCautoarimateste5)
rm(SCautoarimateste6)
rm(fouriertesteC1)
rm(fouriertesteC2)
rm(fouriertesteC3)
rm(fouriertesteC4)
rm(fouriertesteC5)
rm(fouriertesteC6)
rm(SfouriertesteC1)
rm(SfouriertesteC2)
rm(SfouriertesteC3)
rm(SfouriertesteC4)
rm(SfouriertesteC5)
rm(SfouriertesteC6)
rm(mstsConsumo)
rm(Cforecastvectork1)
rm(CforecastvectorSk1)


##
#########


################################################################################


######### Efetuando a análise com VAR
##

# Definindo a base de dados de treinamento
testetsTemperatura <- ts(df$Temperatura, start =2016, end=2019, frequency=365)
performancetsTemperatura <- ts(df$Temperatura, start =2019, end=2021, frequency=365)
traintsvar <- ts.union(testetsConsumo, testetsTemperatura)

# efetuando os vares com lag 1
VARbothp1 <- VAR(traintsvar, type = c('both'), season = 365)
summary(VARbothp1)
VARtrendp1 <- VAR(traintsvar, type = c('trend'), season = 365)
summary(VARtrendp1)
VARconstantp1 <- VAR(traintsvar, type = c('const'), season = 365)
summary(VARconstantp1)


# dos modelos anteriores o que rendeu estimativas com maior R^2 foi VARtrend

# efetuando os vares com lag 2
VARbothp2 <- VAR(traintsvar, type = c('both'), p=2, season = 365)
summary(VARbothp2)
VARtrendp2 <- VAR(traintsvar, type = c('trend'), p=2, season = 365)
summary(VARtrendp2)
VARconstantp2 <- VAR(traintsvar, type = c('const'), p=2, season = 365)
summary(VARconstantp2)


# Deixando o R escolher o lag com AIC
VARbothAIC <- VAR(traintsvar, type = c('both'), season = 365, lag.max = 365)
summary(VARbothAIC)
VARtrendAIC <- VAR(traintsvar, type = c('trend'), season = 365, lag.max = 365)
summary(VARtrendAIC)
VARconstantAIC <- VAR(traintsvar, type = c('const'), season = 365, lag.max = 365)
summary(VARconstantAIC)


# O modelo que contêm apenas a tendência é o que contêm o melhor R^2.
# Utilizando forecast para fazer uma previsão

forecastVARbothp1 <- forecast(VARbothp1, h = 731)
plot(forecastVARbothp1)

forecastVARtrendp1 <- forecast(VARtrendp1, h = 731)
plot(forecastVARtrendp1)

forecastVARconstantp1 <- forecast(VARconstantp1, h = 731)
plot(forecastVARconstantp1)

forecastVARbothp2 <- forecast(VARbothp2, h = 731)
plot(forecastVARbothp2)

forecastVARtrendp2 <- forecast(VARtrendp2, h = 731)
plot(forecastVARtrendp2)

forecastVARconstantp2 <- forecast(VARconstantp2, h = 731)
plot(forecastVARconstantp2)

forecastVARbothp2 <- forecast(VARbothp2, h = 731)
plot(forecastVARbothp2)

forecastVARtrendp2 <- forecast(VARtrendp2, h = 731)
plot(forecastVARtrendp2)

forecastVARconstantp2 <- forecast(VARconstantp2, h = 731)
plot(forecastVARconstantp2)

forecastVARbothAIC <- forecast(VARbothAIC, h = 731)
plot(forecastVARbothAIC)

forecastVARtrendAIC <- forecast(VARtrendAIC, h = 731)
plot(forecastVARtrendAIC)

forecastVARconstantAIC <- forecast(VARconstantAIC, h = 731)
plot(forecastVARconstantAIC)

# Determinando qual dos fits efetua o previsão melhor

## Modelos selecionados por interpretação econômica
# VAR sazonal com 1 lags constante e tendencia
varforecastvectorbothp1 <- forecastVARbothp1$forecast$testetsConsumo$mean
varforecastdiferencesbothp1 <- performancetsConsumo - varforecastvectorbothp1
varsomadoquadradodasdiferençasbothp1 <- sum(varforecastdiferencesbothp1*varforecastdiferencesbothp1)

# VAR sazonal com 1 lags tendência
varforecastvectortrendp1 <- forecastVARtrendp1$forecast$testetsConsumo$mean
varforecastdiferencestrendp1 <- performancetsConsumo - varforecastvectortrendp1
varsomadoquadradodasdiferençastrendp1 <- sum(varforecastdiferencestrendp1*varforecastdiferencestrendp1)

# VAR sazonal com 1 lags constante
varforecastvectorconstantp1 <- forecastVARconstantp1$forecast$testetsConsumo$mean
varforecastdiferencesconstantp1 <- performancetsConsumo - varforecastvectorconstantp1
varsomadoquadradodasdiferençasconstantp1 <- sum(varforecastdiferencesconstantp1*varforecastdiferencesconstantp1)

# VAR sazonal com 2 lags constante e tendencia
varforecastvectorbothp2 <- forecastVARbothp2$forecast$testetsConsumo$mean
varforecastdiferencesbothp2 <- performancetsConsumo - varforecastvectorbothp2
varsomadoquadradodasdiferençasbothp2 <- sum(varforecastdiferencesbothp2*varforecastdiferencesbothp2)

# VAR sazonal com 2 lags tendência
varforecastvectortrendp2 <- forecastVARtrendp2$forecast$testetsConsumo$mean
varforecastdiferencestrendp2 <- performancetsConsumo - varforecastvectortrendp2
varsomadoquadradodasdiferençastrendp2 <- sum(varforecastdiferencestrendp2*varforecastdiferencestrendp2)

# VAR sazonal com 2 lags constante
varforecastvectorconstantp2 <- forecastVARconstantp2$forecast$testetsConsumo$mean
varforecastdiferencesconstantp2 <- performancetsConsumo - varforecastvectorconstantp2
varsomadoquadradodasdiferençasconstantp2 <- sum(varforecastdiferencesconstantp2*varforecastdiferencesconstantp2)
##

## Modelos selecionados pelo comando que utiliza AIC
# VAR sazonal com 1 lags constante e tendencia
varforecastvectorbothAIC <- forecastVARbothAIC$forecast$testetsConsumo$mean
varforecastdiferencesbothAIC <- performancetsConsumo - varforecastvectorbothAIC
varsomadoquadradodasdiferençasbothAIC <- sum(varforecastdiferencesbothAIC*varforecastdiferencesbothAIC)

# VAR sazonal com 1 lags tendência
varforecastvectortrendAIC <- forecastVARtrendAIC$forecast$testetsConsumo$mean
varforecastdiferencestrendAIC <- performancetsConsumo - varforecastvectortrendAIC
varsomadoquadradodasdiferençastrendAIC <- sum(varforecastdiferencestrendAIC*varforecastdiferencestrendAIC)

# VAR sazonal com 1 lags constante
varforecastvectorconstantAIC <- forecastVARconstantAIC$forecast$testetsConsumo$mean
varforecastdiferencesconstantAIC <- performancetsConsumo - varforecastvectorconstantAIC
varsomadoquadradodasdiferençasconstantAIC <- sum(varforecastdiferencesconstantAIC*varforecastdiferencesconstantAIC)

## O melhor modelo dentre os VARES é o de apenas constante com um lag,
# modelo do seguinte plot
plot(forecastVARconstantp1)

# É o que tem a interpretação econômica também.

# Fazendo o gráficos da previsão melhor (SVAR com constante e lag 1, sazonal 365)
graficoSVAR1constantforecast <- autoplot(forecastVARconstantp1) +
  labs(title = 'Previsão com Modelo VAR Sazonal e com Constante', 
       x = 'Tempo' , y = 'Fahrenheit, Megawatts',
       caption = 'Criado com dados da
       U.S Energy Information Administration e
       Global Historical Climatology Network')
graficoSVAR1constantforecast


# Fazendo o gráfico que demonstra que R^2 pode não ser tão relevante para previsão
# SVAR com 1 lag, tendência e sazonalidadade
graficoSVAR1trendforecast <- autoplot(forecastVARtrendp1) +
  labs(title = 'Previsão com Modelo VAR Sazonal e com Tendência', 
       x = 'Tempo' , y = 'Fahrenheit, Megawatts',
       caption = 'Criado com dados da
       U.S Energy Information Administration e
       Global Historical Climatology Network')
graficoSVAR1trendforecast



# Graficos

graficoACFconselec1D
graficoACFTemperatura1D
graficoarimaforecastk1
graficoarimaforecastSk1
graficoconselec
graficodecomposicaoconsumoA
graficodecomposicaoconsumoM
graficodecomposicaotemperaturaA
graficodecomposicaotemperaturaM
graficoPACFconselec1D
graficoPACFtemperatura1D
graficopuroeia
graficoSVAR1constantforecast
graficoSVAR1trendforecast
GraficoACFTemperatura
GraficoACFMegawatts
GraficoPACFMegawatts
GraficoPACFTemperatura


################################################################################
##
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
################################################################################
# utilizando a função multiplot para misturar gráficos

multiACFPACFtemperatura <- multiplot(GraficoACFTemperatura,
                                     graficoACFTemperatura1D,
                                     GraficoPACFTemperatura,
                                     graficoPACFtemperatura1D,
                                     cols = 2)


multiACFPACFconsumo <- multiplot(GraficoACFMegawatts,
                                 graficoACFconselec1D,
                                 GraficoPACFMegawatts,
                                 graficoPACFconselec1D,
                                 cols = 2)


multiforecastarima <- multiplot(graficoarimaforecastk1,
                                graficoarimaforecastSk1,
                                cols = 1)


graficodados <- graficoconselec
graficodados

# teste de cointegração
Jcointtest2 <- ca.jo(data.frame(Temperatura = df$Temperatura, Consumo = df$Megawatts), K = 2, season = 365, ecdet = 'const')
Jcointtest3 <- ca.jo(data.frame(Temperatura = df$Temperatura, Consumo = df$Megawatts), K = 3, season = 365, ecdet = 'const')

summary(Jcointtest2)
summary(Jcointtest3)

# Sim, as variáveis cointegram.


# razões de performance

melhorARIMAsobremelhorVAR <- somadoquadradodasdiferençask1 / varsomadoquadradodasdiferençasconstantp1

# Fazendo um gráfico da cointegrada estacionária
# Para k=3
est1 <- data.frame( Tempo = df$Tempo, X = (df$Temperatura - (0.1234823)*(df$Megawatts)-40.0794384))
est2 <- data.frame( Tempo = df$Tempo, X = (df$Temperatura + (0.2533503)*(df$Megawatts)-131.3402391))
est3 <- data.frame( Tempo = df$Tempo, X = (df$Temperatura + (0.1416354)*(df$Megawatts)+1254.9888425))
plot(est1)
plot(est2)
plot(est3)

#Para k=2
est1k <- data.frame( Tempo = df$Tempo, X = (df$Temperatura - (0.116288)*(df$Megawatts)-41.815599))
est2k <- data.frame( Tempo = df$Tempo, X = (df$Temperatura + (0.284114)*(df$Megawatts)-138.773058))
est3k <- data.frame( Tempo = df$Tempo, X = (df$Temperatura + (0.1416354)*(df$Megawatts)+9631.1430934))
plot(est1k)
plot(est2k)
plot(est3k)


graficoest <- ggplot(est) +
  labs(title = 'Processo Estacionário por Cointegração', 
       x = 'Tempo' , y = 'Fahrenheit + Megawatts',
       caption = 'Criado com dados da
       Global Historical Climatology Network e
       U.S Energy Information Administration') +
  geom_line(aes(x = Tempo, y = X, color='Processo
Cointegrado')) +
  scale_color_manual(values = c('purple'), name = 'Legenda')
  
graficoest

aictestvarp1trend <- (364+3)*2 - 2*7835.203
aictestvarp1constant <- (364+3)*2 - 2*7742.721
aictestsarimak1 <- 6*2 - 2*8457.64

summary(VARconstantp1)
summary(VARtrendp1)

r2sarimap1nonstepwise <- (sum(Cautoarima1$residuals^2))/(sum(((testetsConsumo)-mean(testetsConsumo))^2))
r2sarimap1stepwise <- (sum(SCautoarima1$residuals^2))/(sum(((testetsConsumo)-mean(testetsConsumo))^2))

summary(SCautoarimateste1)
summary(Cautoarimateste1)

kpss.test(ts(data = est3k$X, start = 2016, end = 2022, frequency = 365), null = 'Trend')
kpss.test(ts(data = est2k$X, start = 2016, end = 2022, frequency = 365), null = 'Trend')
kpss.test(ts(data = est1k$X, start = 2016, end = 2022, frequency = 365), null = 'Trend')
kpss.test(ts(data = est3k$X, start = 2016, end = 2022, frequency = 365), null = 'Level')
kpss.test(ts(data = est2k$X, start = 2016, end = 2022, frequency = 365), null = 'Level')
kpss.test(ts(data = est1k$X, start = 2016, end = 2022, frequency = 365), null = 'Level')
adf.test(ts(data = est1k$X, start = 2016, end = 2022, frequency = 365))

