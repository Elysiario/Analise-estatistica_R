# Projeto de Métodos de Regressão e Previsão

# Alunos: Ariana dos Reis 2220131, Elysiario dos Santos 2220153

###### Tarefa 1 #######
## Import do dataset
dados_Crime <- read.csv("Crime.csv")
View(dados_Crime)

#criação variável para ver apenas os condados de 3 a 11

dados_Crime2 <- subset(dados_Crime, county %in% c(3, 5, 7, 9, 11))
attach(dados_Crime2)

#Conferência se o filtro está correto

View(dados_Crime2)
table(dados_Crime2$county, dados_Crime2$year)

# Verificar se existem dados nulos

sum(is.na(dados_Crime2)) 
colSums(is.na(dados_Crime2))
# Existem 40 observações omissas, no entanto são de variáveis que não serão utilizadas no estudo
# pelo que será ignorado.

# 1 (a) Represente graficamente a taxa de crimes cometidos per capita ao longo dos 7 anos
# analisados, distinguindo os diferentes condados. Comente.

#Gráfico taxa de crime cometidos per capita ao longo dos 7 anos	
plot(year[county==3], crmrte[county==3], type="l", 
     xlab="Ano", ylab="Taxa de crimes per capita",
     main="Crimes per capita dos counties 3 a 11", col = 2,
     ylim = c(min(crmrte), max(crmrte)))
for (i in c(5,7,9,11)){
  lines(year[county==i], crmrte[county==i], col = i)
}
legend("topleft",
       legend = c("County 3", "County 5", "County 7", "County 9", "County 11"),
       lty = 1, col = c(2,5,7,9,11), bg = "transparent", border = "transparent", 
       text.width = 0.4, cex = 0.7)


# Através da análise da evolução per capita dos crimes ano a ano, podemos notar que o "County 7" é o mais crítico,
# pois tem um número de crimes maior que os demais condados (3, 5, 9 e 11) em todos os anos, exceto 1982.
# Para auxiliar na interpretação dos dados, foi criada uma tabela com as médias de crime per capita de cada
# condado, o que confirma a percepção que o condado 7 é o mais violento (0.02304530 por habitante), 
# e traz a informação do condado 9, que possui a menor média por habitante (0.01137764).
         
media_crm_county <- aggregate(crmrte ~ county, data = dados_Crime2, FUN = mean)
print(media_crm_county)

# 1 (b) - Estime os modelos de regressão linear que colocam a taxa de crimes a depender das
#restantes variáveis, usando o “pooled model”, o modelo de efeitos fixos e o de efeitos
#aleatórios. Comente os modelos obtidos e compare-os usando os métodos que considerar
#adequados.

# Baixar a biblioteca necessária para a análises dos modelos
library(plm)

# Mínimos quadrados ordinários (MQO)

PooledMod = plm(crmrte ~ avgsen + polpc + density, model="pooling", data=dados_Crime2)
summary(PooledMod)

#Coefficients:
#Estimate  Std. Error t-value Pr(>|t|)   
#(Intercept)  0.01259898  0.00399330  3.1550 0.003556 **
#avgsen      -0.00026562  0.00038752 -0.6854 0.498172   
#polpc        3.22280403  1.56462349  2.0598 0.047899 * 
#density      0.00194411  0.00407619  0.4769 0.636749   

# crmrte^ = 0.01259898 - 0.00026562*avgsen + 3.22280403*polpc + 0.00194411*density

# Extra:
mqo <- lm(crmrte ~ avgsen + polpc + density, data = dados_Crime2)
summary(mqo)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  0.0125990  0.0039933   3.155  0.00356 **
#avgsen      -0.0002656  0.0003875  -0.685  0.49817   
#polpc        3.2228040  1.5646235   2.060  0.04790 * 
#density      0.0019441  0.0040762   0.477  0.63675  

# Como esperávamos, tanto o PooledMod quanto o lm apresentam valores parececidos.

# Modelo de Efeitos Fixos
DF = pdata.frame(dados_Crime2, index=c("county","year"))
FEMod = plm(crmrte ~ avgsen + polpc + density, model="within", data=DF)
coefficients(FEMod)
cfixef(FEMod)

#crmrte_3^ = 0.050684 + 0.0001134622*avgsen - 1.5296611614*polpc - 0.0350730575*density 
#crmrte_5^ = 0.028213 + 0.0001134622*avgsen - 1.5296611614*polpc - 0.0350730575*density 
#crmrte_7^ = 0.041587 + 0.0001134622*avgsen - 1.5296611614*polpc - 0.0350730575*density 
#crmrte_9^ = 0.030718 + 0.0001134622*avgsen - 1.5296611614*polpc - 0.0350730575*density 
#crmrte_11^ = 0.040702 + 0.0001134622*avgsen - 1.5296611614*polpc - 0.0350730575*density 

# Modelo de Efeitos Aleatórios
REMod = plm(crmrte ~ avgsen + polpc + density, model="random", random.method = "walhus",
            data=DF)
summary(REMod)

# crmrte^ = 1.7935e-02 + 5.5037e-05*avgsen - 4.4502e-01*polpc - 3.2284e-03*density

# Comparação dos modelos

# Teste F:
# H0: c1=c2=...=c5 --> os efeitos fixos são todos iguais
# H1: existem i != j tal que ci != cj --> existem diferentes efeitos fixos
pFtest(FEMod,PooledMod)
# p-value = 4.069e-08 < alpha = 0.05 => Rejeita-se H0, pois
# existe evidência estatistica que nos permite afirmar que existem diferenças significativas entre os efeitos fixos no modelo.
# Portanto, o Modelo de Efeitos Fixos (MEF), é preferível ao modelo de dados empilhados (MQO).

# Teste de Breusch-Pagan:
# H0: var(desvios aleatórios) = 0
# H1: var(desvios aleatórios) != 0
# ou
# H0: não existem desvios aleatórios
# H1: existem != desvios aleatórios
plmtest(PooledMod, type="bp")
# Como o teste de hipóteses de Breusch-Pagan apresentou p-value = NA será desconsiderado. Os demais testes estão a funcionar bem,
# o que nos dá segurança para não utilizá-lo, dado que aparentemente o problema não está nos dados, mas sim na impossibilidade de
# calcular o teste corretamente.

# Teste de Hausman:
# H0: MEF e MEA são consistentes
# H1: MEF é consistente e o MEA não

phtest(FEMod, REMod)
# p-value = 0.07421 > alpha = 0.05 => Não rejeitar H0, existe evidência estatística para afirmar que tanto MEF
# e MEA são consistentes. 

# Comentários finais das análises dos modelos:

# Após análise dos modelos concluímos que no teste (pFtest), o valor p é extremamente baixo (4.069e-08), indicando uma forte 
# evidência para rejeitar a hipótese nula de que não há efeitos significativos no modelo. Isso sugere que o modelo 
# de Efeitos Fixos (MEF) é estatisticamente superior, já que os efeitos individuais têm um impacto significativo na 
# variável dependente.
# Em contrapartida o teste de Hausman (phtest) compara a consistência dos modelos de Efeitos Fixos (FEMod) 
# e Efeitos Aleatórios (REMod). O valor p (0.07421) é maior do que o valor de significância comumente usado (alpha = 0,05).
# Portanto, não há evidências suficientes para rejeitar a hipótese nula de que os dois modelos são consistentes. 
# Isso sugere que ambos os modelos podem ser considerados adequados (MQO e MEA) em relação à consistência.
#Com base nesses resultados, o modelo de Efeitos Fixos (FEMod) parece ser a melhor escolha, pois apresenta efeitos individuais significativos
# e foi considerado melhor do que o modelo dos mínimos ordinários. 

###### Tarefa 2 #######

# Baixar as bibliotecas necessárias
library(tidyverse)
library(lubridate)
library(fpp3)
library(tsibble)
library(forecast)
#Carregar dados (usar read csv2, devido a base não estar separada apenas por vírgula)
dados_air <- read.csv2("AirQualityUCI.csv")
attach(dados_air)
View(dados_air)

# Fitrar os dados de acordo com o período escolhido.
dias <- paste0(seq(21,27), "/03/2004")

dados_air <- dados_air %>%
  filter(Date %in% dias)

# Verificar o tipo de cada variável

glimpse(dados_air)

# Verificar se existem dados nulos
sum(is.na(dados_air)) 
colSums(is.na(dados_air))

# Notamos que não existem dados nulos nas variáveis mais relevantes para a análise (Date, Time e T)

# Juntar data e hora

dados_air$horario <- paste(dados_air$Time, dados_air$Date, sep = ".")

# Confirmar se a junção deu certo

dados_air$horario

# Converter a string para o formato data e hora

dia <- gsub("/", "-", dados_air$Date)
time <- gsub("\\.", ":", dados_air$Time)
data <- paste(dia, time)
dados_air$horario <- dmy_hms(data)

# Confirmar se a conversão deu certo

glimpse(dados_air)

# Gráfico

dados_air %>%
  select(horario, `T`) %>%
  ggplot(aes(horario, `T`)) +
  geom_line() +
  labs(x = "Dia", y = "Temperatura", title = "Variação de temperatura entre 21/03/2004 e 27/03/2004")

#Sazonalidade:
# Através do gráfico de linhas do período estipulado, podemos notar sazonalidade no comportamento, dado que a temperatura inicia o dia,
# em seguida cai e, após isso caminha em direção ao pico e volta a cair, conforme se aproxima o final do dia. Este padrão é observado
# em todos dos dias analisados.

# Modelagem da série

serie <- dados_air %>%
  select(horario, `T`)

serie <- tsibble(serie)

acf(serie, main = "Função de Autocorrelação", xlab = "Lags", ylab = "Autocorrelação")

#Redução das instâncias apenas para melhorar a percepção dos dados

acf(serie, 100) 

# Visível autocorrelação de um ponto em relação ao ponto anterior com queda lenta. Nota-se também valores significativos 
#fora da faixa de confiança o que confirma a autocorrelação.

pacf(serie, main = "Função de Autocorrelação Parcial", xlab = "Lags", ylab = "Autocorrelação Parcial")

#Redução das instâncias apenas para melhorar a percepção dos dados
pacf(serie, 100)

# Nota-se de imediato a presença de valores significativos fora da faixa de confiança. Também de observa o decaimento rápido
# dos valores e valores significativos nos primeiros lags o que pode ser benéfico para o modelo.

fit <- auto.arima(serie, seasonal = TRUE)
fit
summary(fit)
# ARIMA(0,1,2)(0,1,2)[24]  

# Análise dos residuais

acf(fit$residuals, main = "Função de Autocorrelação", xlab = "Lags", ylab = "Autocorrelação")

# Nos resíduos nota-se uma queda acentuada com alguns poucos valores fora do intervalo de confiança

pacf(fit$residuals, main = "Função de Autocorrelação Parcial", xlab = "Lags", ylab = "Autocorrelação Parcial")

# Podemos ver no gráfico a presença de correlação parcial negativa significativa e ausência de correlação parcial positiva.

# Box test

Box.test(fit$residuals, lag = 48, type = "Ljung-Box")
#Box-Ljung test
#data:  fit$residuals
#X-squared = 40.556, df = 48, p-value = 0.7685

# H0: Não há autocorrelação entre os resídios. x H1: há autocorrelação entre os resídios

# p-value é 0.7685 > alpha, o que sugere que não há evidências suficientes para rejeitar a hipótese nula 
#de não autocorrelação dos resíduos significativa nos resíduos do modelo. 

# Gráficos para analisar os residuais

hist(fit$residuals, main = "Histograma dos Resíduos", xlab = "Resíduos", ylab = "Frequência")

#Histograma confirma a percepção e mostra maior concentração dos residuais perto de 0 e simetria da distribuição
# quase perfeita o que também pode ser confirmado no gráfico de dispersão abaixo:

plot(serie$horario, fit$residuals, main = "Dispersão dos Resíduos", xlab = "Horário", ylab = "Resíduos")


########### FORMA FEITA NAS AULAS ###########
#############################################
TemTS = ts(dados_air$T, start=c(21, 0), end=c(27, 23), frequency=24)
TemTS

plot.ts(TemTS, main = "Temperaturas registadas", xlab= "Dias", ylab = "Temperatura")
# Analisar:
# Sazonalidade:
Acf(TemTS,  main = "Gráfico da função de autocorrelação")
Pacf(TemTS)

# Tendência:
# Não existe uma tendência.

# Componente cíclica:
# Não existe componente ciclica, pois não existem temperaturas registados, 
# num periodo de tempo com alguma dimensão, que se desviem consideravelmente dos restantes

# Usando o modelo de decomposição aditiva
TemTScomponentsAdd = decompose(TemTS,type="additive") 
plot(TemTScomponentsAdd)

# Usando o modelo de decomposição multiplicativa
TemTScomponentsMul = decompose(TemTS,type="multiplicative") 
plot(TemTScomponentsMul)

# Ajustar um modelo à série das temperaturas 
# Estimação do modelo ARIMA:
fit.TemTS.ARIMA = auto.arima(TemTS,seasonal = FALSE)
fit.TemTS.ARIMA
# O modelo obtido é o ARIMA(0,1,1)

# Estimação do modelo SARIMA:
fit.TemTS.SARIMA <- auto.arima(TemTS, seasonal = TRUE)
fit.TemTS.SARIMA
# O modelo obtido é o ARIMA(0,1,2)(0,1,2)[24]

# Comentar a qualidade de ajustamento do modelos SARIMA
accuracy(fit.TemTS.ARIMA)
#                       ME     RMSE       MAE        MPE     MAPE      MASE       ACF1
# Training set -0.02178891 1.190554 0.8758544 -0.4702305 6.965393 0.3543777 0.01987813

accuracy(fit.TemTS.SARIMA)
#                       ME     RMSE       MAE        MPE     MAPE      MASE       ACF1
# Training set 0.008502047 1.032199 0.7065044 -0.1475059 5.732362 0.2858574 0.00536585

# As medidas de erro mais usadas para analisar o ajustamento são RMSE, MAE e MAPE

# Utilizar o modelo ajustado para estimar a temperatura nas 24 horas do dia seguinte
# à semana analisada (28 de março de 2004)
PrevTemTS.SARIMA = forecast(fit.TemTS.SARIMA, 24, level=0.95)
PrevTemTS.SARIMA

plot.ts(TemTS, xlim=c(21, 29), ylim=c(0, 26), main = "Temperaturas registadas vs Modelo SARIMA", xlab= "Dias", ylab = "Temperatura")
lines(PrevTemTS.SARIMA$fitted, col="red")
lines(PrevTemTS.SARIMA$mean, col="blue")
lines(PrevTemTS.SARIMA$lower, col="blue", lty=2)
lines(PrevTemTS.SARIMA$upper, col="blue", lty=2)

PrevTemTS.ARIMA = forecast(fit.TemTS.ARIMA, 24, level=0.95)
PrevTemTS.ARIMA

plot.ts(TemTS, xlim=c(21, 29), ylim=c(0, 26), main = "Temperaturas registadas vs Modelo ARIMA", xlab= "Dias", ylab = "Temperatura")
lines(PrevTemTS.ARIMA$fitted, col="red")
lines(PrevTemTS.ARIMA$mean, col="blue")
lines(PrevTemTS.ARIMA$lower, col="blue", lty=2)
lines(PrevTemTS.ARIMA$upper, col="blue", lty=2)




