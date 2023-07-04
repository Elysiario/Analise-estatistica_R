#Trabalho de Análise Exploratória de dados - Mestrado em Ciência de Dados
#Alunos Nélio Mourato (2220119) e Elysiario Santos (2220153)

#Neste projeto realizaremos a análise exploratória da base de "fusoes" que trata da composição química em amostras que serão usadas
# em testes de resistência de material.

# Análise preliminar dos dados:

#O trabalho deve incluir as quatro seguintes análises estatísticas.

#1. A análise exploratória geral da base de dados, com a descrição mais pormenorizada de uma
#variável nominal, de uma variável ordinal e de duas variáveis quantitativas (variáveis escolhidas pelo grupo entre as variáveis da base de dados). 
#Em particular, deve incluir:
  
#a) a manipulação dos dados (preparação da base de dados para o tratamento estatístico);

View(Fusoes)
names(Fusoes)
Fusoes$QUALIDADE
head(Fusoes,3)
tail(Fusoes, 3)
dim(Fusoes)

# Na importação da base nota-se que a coluna "QUALIDADE" está descrita como character e como não pretendemos usá-la para cálculos a mesma 
# deverá continuar com essa formatação.
# As variáveis quantitativas, que vão da coluna 5 a 16 já foram importadas corretamente, no formato "Double"
# A base de dados contém 16 colunas e 6383 linhas, e contém uma variável Nominal, uma ordinal e 12 quantitativas, 
# sendo suficiente para a elaboração do projeto proposto.

# b) a análise à existência de observações omissas;

sum(is.na(Fusoes))

# Confirmado que não existem valores omissos no DF, contudo temos incidência de valores ==0, vamos explorá-los:

Cont <-(Fusoes[, 5:16]==0)
table(Cont)
#Total de valores quantitativos na base == 76596
#Total de valores == 0 na base == 1613 ou 2,1% do total de dados quantitativos

# Criação de barplot para entender a concentração de volumes de dados == 0

barplot((Cont), main=" ", xlab=" ", ylab=" ", col= 5:16) 

# Nota-se uma maior concentração de valores ==0 nas seguintes variáveis:

# CO

Cont_CO <-(Fusoes[, 13]==0)
table(Cont_CO)
#380 ocorrências (5,95% da Variável "CO")

# TI
Cont_TI <-(Fusoes[, 16]==0)
table(Cont_TI)
#566 ocorrências (8,86% da Variável "TI")

# c) a apresentação de tabelas, gráficos e medidas adequadas ao resumo da informação das 4 variáveis escolhidas para análise (uma nominal, uma ordinal e duas quantitativas);

# Variáveis a serem exploradas:
# Variável nominal: Familia
# variável ordinal: 'Nickel content'
# Variavel quantitativa 1: C
# Variavel quantitativa 2: NI

# Explorando a variável Familia (Nominal)

Fusoes$Familia

niveis_f <-factor(Fusoes$Familia)
table(niveis_f)
barplot(table(niveis_f))

# De acordo com a formação da tabela e criação do gráfico (barplot), podemos concluir que a família "Austenitico" tem concentração
# consideravelmente superior que as demais (Duplex, Mastensitico e Mastensitico PHD)

# Explorando a variável `Nickel Content`(Ordinal)

Fusoes$`Nickel Content`

niveis_n <-factor(Fusoes$`Nickel Content`, levels = c("no Ni Alloy", "low Ni", "median Ni", "High Ni"), 
                  labels = c("no Ni Alloy", "low Ni", "median Ni", "High Ni"))
table(niveis_n)
niveis_n
class(niveis_n)
barplot(table(niveis_n))

# A variável ordinal foi separada em níveis para uma melhor categorização dos dados. Feito isso, conclue-se que a maior concentração
# dos dados dessa variável são "High Ni", ou seja, com alta adição de Níquel na composição, o que se associa com a variável família, anteriormente analisada.

# Explorando a Variavel quantitativa 1: "C"

library(psych)
describe(Fusoes$C)

# vars n    mean    sd    median  trimmed   mad   min  max    range skew    kurtosis  se
  #  1 6383 0.04    0.07  0.02    0.03      0.01  0    1.07   1.07  6.83    59.53     0

hist(Fusoes$C)

# Notamos um desvio padrão pequeno, devido a baixa amplitude dos dados (1.07). A média e a mediana estão bem próximas com 0.04 e 0.02, respectivamente.
# Histograma mostra os dados mais concentrados no intervalo entre 0.0 E 0.1, confirmando a informação acima.

# Explorando a variavel quantitativa 2: "NI"

library(psych)
describe(Fusoes$NI)

#vars n     mean    sd    median  trimmed   mad   min max     range   skew      kurtosis  se
#  1  6383  7.73    3.03  8.16    8.16      0.76   0  20.35   20.35   -0.87     2.52      0.04

hist(Fusoes$NI)

#Histograma mostra os dados mais concentrados no intervalo entre 8.0 E 10.0, justamente onde temos o 1º , 3º quartil e a média.
# Devido a maior amplitude, o desvio padrão é bem maior que da variável "C", anteriormente analisada.
# Interessante notar que essa é uma confirmação dos exercícios anteriores que mostra uma maior incidência de Níquel (NI) nas amostras.

# d) a apresentação de intervalos com 95% de confiança para a percentagem de indivíduos em cada categoria da variável nominal;

#Austenitico

Austenitico <-factor(Fusoes$Familia, 
                     levels = c("Austenitico", "Duplex", "Martensitico", "Martensitico PHD"), labels = c(1, 0, 0, 0))
Austenitico
table(Austenitico)

binom.test(table(Austenitico),
           conf.level= 0.95)

# number of successes = 5179, number of trials = 6383, p-value < 2.2e-16
#alternative hypothesis: true probability of success is not equal to 0.5
#95 percent confidence interval:
  #0.8015569 0.8209056
#sample estimates:
  #probability of success 0.811374  
# A probabilidade de sucesso nesse caso é de 81.13%, variando entre 80.15% e 82.09% com intervalo de confiança de 95%.

# Duplex
Duplex <-factor(Fusoes$Familia, 
                     levels = c("Duplex", "Austenitico", "Martensitico", "Martensitico PHD"), labels = c(1, 0, 0, 0))
Duplex
table(Duplex)

binom.test(table(Duplex),
           conf.level= 0.95)

# number of successes = 44, number of trials = 6383, p-value < 2.2e-16
#alternative hypothesis: true probability of success is not equal to 0.5
#95 percent confidence interval:
  #0.005013028 0.009243015
#sample estimates:
  #probability of success 0.00689331 
# A probabilidade de sucesso nesse caso é de 0.69%, variando entre 0.50 % e 0.92% com intervalo de confiança de 95%.

#Martensitico

Martensitico <-factor(Fusoes$Familia, 
                levels = c("Martensitico","Duplex", "Austenitico", "Martensitico PHD"), labels = c(1, 0, 0, 0))
Martensitico
table(Martensitico)


binom.test(table(Martensitico),
           conf.level= 0.95)

#number of successes = 606, number of trials = 6383, p-value < 2.2e-16
#alternative hypothesis: true probability of success is not equal to 0.5
#95 percent confidence interval:
  #0.08785647 0.10239543
#sample estimates:
 #probability of success 0.09493968 
# A probabilidade de sucesso nesse caso é de 9.4%, variando entre 8.78 % e 10.23% com intervalo de confiança de 95%.

#Martensitico PHD

Martensitico_PHD <-factor(Fusoes$Familia, 
                      levels = c("Martensitico PHD", "Martensitico", "Duplex", "Austenitico"), labels = c(1, 0, 0, 0))
Martensitico_PHD
table(Martensitico_PHD)

binom.test(table(Martensitico_PHD),
           conf.level= 0.95)

#number of successes = 554, number of trials = 6383, p-value < 2.2e-16
#alternative hypothesis: true probability of success is not equal to 0.5
#95 percent confidence interval:
  #0.07999809 0.09396824
#sample estimates:
  #probability of success 0.08679304 
# A probabilidade de sucesso nesse caso é de 8.67%, variando entre 7.99 % e 9.39% com intervalo de confiança de 95%.

# e) a análise se alguma das duas variáveis quantitativas escolhidas pode ser caracterizada por uma distribuição normal;

# Análise de distribuição normal "C"

# H0 dados provêm de uma distribuição normal
# H1 não provém de uma distribuição normal

set.seed(23) # Determinando que a amostra coletada será sempre a mesma. Todas as amostras aleatórias gerarão o mesmo número
amostra_C <- sample(Fusoes$C, 5000)
shapiro.test(amostra_C)
hist(amostra_C)

# H0 dados provêm de uma distribuição normal
# H1 não provém de uma distribuição normal

#p-value < 2.2e-16, rejeitar H0, dado que o p-value é menor que 0,05.
# Histograma evidencia que a amostra não provêm de distribuição normal, devido a maior concentração de dados entre 0 e 0.2

# Análise de distribuição normal "NI"
set.seed(23)
amostra_NI <- sample(Fusoes$NI, 5000)
shapiro.test(amostra_NI)
hist(amostra_NI)
median(amostra_NI)


#p-value < 2.2e-16, rejeitar H0, dado que o p-value é menor que 0,05
# Mesmo com concentração de dados significante próximo a mediana, podemos dizer que a amostra não tem distribuição normal,
# Devido aos dados não seguirem de forma decrescente, partindo da mediana.

# f) a comparação dos valores observados nas duas variáveis quantitativas nas diferentes categorias da variável ordinal, 
# incluindo uma comparação da sua variabilidade em cada categoria, bem como do seu valor médio;

library(tidyverse)

#Analise das variáveis quantitativas em relação a variável ordinal

Fusoes %>%
  group_by(`Nickel Content`) %>% summarise(Média_C =mean(C), Media_NI=mean(NI), 
                                           Range_C = range(C), Range_NI = range(NI))

ggplot(Fusoes, aes(x=`Nickel Content`, y=C)) + geom_boxplot()
ggplot(Fusoes, aes(x=`Nickel Content`, y=NI)) + geom_boxplot()

#Notamos uma pequena variação de High NI e median NI em relação a C. As demais componentes Low Ni e no ni alloy oscilam
# um pouco mais, porém sem amplitude significativa
#Já para NI a oscilação é maior e os dados de High NI estão com maior concentração entre 8 e 10, onde se encontra a média(8.97).
# Tanto o resumo estatístico quanto os gráficos evidenciam uma variabilidade bem maior de NI em relação a C, sempre tendo a variável
#`Nickel Content`como referência.

# g) a análise da independência/associação entre as duas variáveis qualitativas (as variáveis nominal e ordinal escolhidas);

# H0 - as duas variáveis são independentes
# H1 - as duas variáveis não são independentes

fisher.test(table(Fusoes$Familia, Fusoes$`Nickel Content`), simulate.p.value = TRUE)

# p-value = 0.0004998 - De acordo com o teste exato de fisher devemos rejeitar H0, dado que o ressultado está abaixo 
# de 0,05 ou 5%.
  
# h) a análise da correlação entre as duas variáveis quantitativas.

# H0: Correlação == 0, ou seja, não existe correlação entre as variáveis quantitativas
# H1: Correlação != 0, indicando correlação entre as variáveis

cor(Fusoes$C, Fusoes$NI)

# Correlação -0.5600347, classificada como negativa moderada. Portanto deve-se rejeitar H0 

#2. [4 val.] A aplicação de técnicas de análise fatorial em componentes principais a todas as variáveis quantitativas
# da base de dados, de forma investigar uma possível redução da dimensão dos dados, nomeadamente:

# a) a análise da correlação entre as variáveis e do índice KMO (Kaiser-Meyer-Olkin);

# Correlação geral:

cor(Fusoes[, 5:16])

library(corrplot); 
corrplot(cor(Fusoes[,5:16]), 
         type = "lower", 
         addCoef.col = TRUE,
         diag = FALSE,
         order = "hclust")
# Não houve correlação forte para nenhum par de variáveis. A maior correlação foi entre NI e CR com valor 0.74

library(psych); 
KMO(Fusoes[, 5:16])
# Overall MSA =  0.66 nível de variância razoável
# MSA for each item = 
  #C   SI   MN    P    S   CR   NI   MO   CO   CU    N   TI 
#0.77 0.38 0.74 0.90 0.40 0.64 0.69 0.48 0.86 0.57 0.69 0.59 
# SI, S e MO <50 
# Cabe destacar a variável "P" com índice de variância 0.90 (muito bom)

# Correlação por família: Austenitico 

Familia_Austenitico<-subset(Fusoes,Familia=="Austenitico")
View(Familia_Austenitico)
cor(Familia_Austenitico[, 5:16])

library(corrplot); 
corrplot(cor(Familia_Austenitico[,5:16]), 
         type = "lower", 
         addCoef.col = TRUE,
         diag = FALSE,
         order = "hclust")

# Correlação média para SI e NI e S e C, para as demais a correlação não é significante

KMO(Familia_Austenitico[, 5:16])

#Kaiser-Meyer-Olkin factor adequacy
#Overall MSA =  0.41 (baixa)
#MSA for each item = 
  #C   SI   MN    P    S   CR   NI   MO   CO   CU    N   TI 
#0.55 0.49 0.72 0.41 0.39 0.29 0.33 0.33 0.64 0.59 0.88 0.35 
#Atenção para MN e N que são médio e boa, respectivamente

#Correlação por família: Duplex

Familia_Duplex<-subset(Fusoes,Familia=="Duplex")
View(Familia_Duplex)
cor(Familia_Duplex[, 5:16])

corrplot(cor(Familia_Duplex[,5:16]), 
         type = "lower", 
         addCoef.col = TRUE,
         diag = FALSE,
         order = "hclust")

# Em Duplex temos altas correlações positivas (CR X NI, CR x MO, NI x MO, MN X CU, MN X SI ),
# e também negativas (CU X NI, CU X MO, SI X CR, SI X NI, MN X CR, MN X NI, MN X MO)

KMO(Familia_Duplex[, 5:16])

#Kaiser-Meyer-Olkin factor adequacy
#Call: KMO(r = Familia_Duplex[, 5:16])
#Overall MSA =  0.64
#MSA for each item = 
  #C   SI   MN    P    S   CR   NI   MO   CO   CU    N   TI 
#0.52 0.75 0.60 0.19 0.13 0.69 0.59 0.59 0.80 0.96 0.67 0.38 
# Atenção para CO e CU que tem variância alta e muito alta, respectivamente

#Correlação por família: Martensitico

Familia_Martensitico<-subset(Fusoes,Familia=="Martensitico")
View(Familia_Martensitico)
cor(Familia_Martensitico[, 5:16])

corrplot(cor(Familia_Martensitico[,5:16]), 
         type = "lower", 
         addCoef.col = TRUE,
         diag = FALSE,
         order = "hclust")

# Correlação alta apenas entre S e MN a maioria das demais correlações é insignificante

KMO(Familia_Martensitico[, 5:16])

#Kaiser-Meyer-Olkin factor adequacy
#Overall MSA =  0.57 (médio)
#MSA for each item = 
  #C   SI   MN    P    S   CR   NI   MO   CO   CU    N   TI 
#0.49 0.81 0.55 0.67 0.50 0.61 0.34 0.56 0.41 0.38 0.80 0.50 
#Variancia boa para SI e N (0.80)

#Correlação por família: Martensitico PHD

Familia_Martensitico_PHD<-subset(Fusoes,Familia=="Martensitico PHD")
View(Familia_Martensitico_PHD)
cor(Familia_Martensitico_PHD[, 5:16])

corrplot(cor(Familia_Martensitico_PHD[,5:16]), 
         type = "lower", 
         addCoef.col = TRUE,
         diag = FALSE,
         order = "hclust")

# A única correlação média para esta familia é entre S e MN (0.78) as demais são, em sua maioria, insiginicantes

KMO(Familia_Martensitico_PHD[, 5:16])

#Kaiser-Meyer-Olkin factor adequacy
#Overall MSA =  0.76 (Médio)
#MSA for each item = 
  #C   SI   MN    P    S   CR   NI   MO   CO   CU    N   TI 
#0.70 0.78 0.81 0.92 0.78 0.69 0.80 0.72 0.88 0.55 0.72 0.41 
# Variância média para "C", "SI", "S", "MO" e "N", boa para "MN", "NI"e "CO" e muito boa para "P"

# b) a fundamentação do número de componentes utilizadas, com referência à variância explicada;

# Seguindo os critérios de relevância de KMO e correlação, sugere-se que sejam desconsideradas as Seguintes variáveis:

# FUSAOBHP e QUALIDADE (Não utilizadas até o momento)
# SI, S e MO com baixa correlação em relação as demais variáveis quantitativas 
# e variância insignificante.

# c) a apresentação de gráficos que ilustrem as conclusões, avaliando se as principais componentes permitem distinguir 
# os indivíduos das diferentes categorias das variáveis qualitativas utilizadas na primeira questão.

# Ao analisar previamente os dados ficou a impressão que existe uma relação entre nível de níquel e a família (Austenitico), julgamos por 
# bem conferir através dos estudos abaixo:

table(Fusoes$Familia, Fusoes$`Nickel Content`)
library(ggplot2)

ggplot(Fusoes, aes(Fusoes$Familia, fill=Fusoes$`Nickel Content`)) +
  geom_bar(position = "fill") +
  labs(title = "Relação entre as variáveis Família e Nickel Content",
       x = Fusoes$Familia,
       y = Fusoes$`Nickel Content`) +
  scale_fill_manual(values=rainbow(4))

# A tabela e o gráfico acima evidenciam que a família Austenitico tem alta concentração de níquel na ampla maioria 
# das vezes, como suspeitávamos.

pie(table(Fusoes$Familia))

# Gráfico (Pie) evidencia maior concentração de dados na categoria "Austenitico"

pie(table(Fusoes$`Nickel Content`))

# Gráfico (Pie) evidencia maior concentração de dados na categoria "High Ni"

#3. [6 val.] A aplicação de técnicas de análise de clusters, em particular:

STDFusoes2 <- as.data.frame(Fusoes[,5:16])
STDFusoes <- (as.data.frame(scale((Fusoes[,5:16]))))
set.seed(500)
amostra <- sample(1:nrow(Fusoes), 500)
STDFusoes <- as.data.frame(scale((Fusoes[amostra,5:16])))


Teste1 <- as.data.frame(Fusoes[amostra,])
table(Teste1$`Nickel Content`)


sapply(STDFusoes,mean)
sapply(STDFusoes,sd)
boxplot(STDFusoes)


boxplot(STDFusoes2)

boxplot(Fusoes[Fusoes$Familia == "Austenitico",5:16])
boxplot(Fusoes[Fusoes$Familia == "Martensitico",5:16])
boxplot(Fusoes[Fusoes$Familia == "Martensitico PHD",5:16])
boxplot(Fusoes[Fusoes$Familia == "Duplex",5:16])
table(Fusoes$`Nickel Content`)
boxplot(Fusoes[Fusoes$`Nickel Content` == "High Ni",5:16],main="Composição quimica Ligas 'High Ni", xlab="Componente Quimico", ylab="%Quimica",ylim=c(0,25))
boxplot(Fusoes[Fusoes$`Nickel Content` == "median Ni",5:16],main="Composição quimica Ligas 'Median Ni'", xlab="Componente Quimico", ylab="%Quimica",ylim=c(0,25))
boxplot(Fusoes[Fusoes$`Nickel Content` == "no Ni Alloy",5:16],main="Composição quimica Ligas 'No Ni '", xlab="Componente Quimico", ylab="%Quimica",ylim=c(0,25))
boxplot(Fusoes[Fusoes$`Nickel Content` == "\tlow Ni",5:16],main="Composição quimica Ligas 'Low Ni'", xlab="Componente Quimico", ylab="%Quimica",ylim=c(0,25))


# a) a aplicação de um método hierárquico (apresentando o respetivo dendrograma);


d <- dist(STDFusoes, "minkowski")
as.matrix(d)[1:11,1:11]
hc = hclust(d, method="ward.D2")
#Centroid, complete, average,single, esta fora de questão aparentam ter
# uma variavel continua / apenas o Ward.D2 Apresenta valores aceitaveis
plot(hc, hang=-1); 

rect.hclust(hc, 2, border="blue")
rect.hclust(hc, 3, border="red")
rect.hclust(hc, 4, border="green")


d1 <- dist(STDFusoes, "euclidean")
as.matrix(d1)[1:11,1:11]
hc1 = hclust(d1, method="ward.D2")
plot(hc1, hang=-1); 
rect.hclust(hc1, 2, border="blue")
rect.hclust(hc1, 3, border="red")
rect.hclust(hc1, 4, border="magenta")

d2 <- dist(STDFusoes, "manhattan")
as.matrix(d2)[1:11,1:11]
hc2 = hclust(d2, method="ward.D2")
plot(hc2, hang=-1); 
rect.hclust(hc2, 2, border="blue")
rect.hclust(hc2, 3, border="red")
rect.hclust(hc2, 4, border="green")

# Aplicando os metodos hierarquicos, o Dendrogama apresenta ter 2 ou 3 clusters 

library(dendextend);
library(factoextra);
fviz_dend(hc1,3)

dend <- as.dendrogram(hc1);
labels_colors(dend) <- as.integer(Fusoes$`Nickel Content`[hc1$order]);
plot(dend, cex.lab=1.3, xlab=" ", ylab=" ", sub=" ", main="Dendograma ");
rect.hclust(hc1,2, border="blue")

rect.hclust(hc1, 2, border="blue")
rect.hclust(hc1, 3, border="red")
rect.hclust(hc1, 4, border="green")


fviz_dend(hc1, 2, ylab=" ", main = "Dendograma", 
          k_colors = c("blue", "darkorange2","magenta"),
          label_cols = as.integer(Fusoes$`Nickel Content`[hc1$order]), cex = 0.6)
fviz_dend(hc1, 3, ylab=" ", main = "Dendograma", 
          k_colors = c("blue", "red","magenta"),
          label_cols = as.integer(Fusoes$`Nickel Content`[hc1$order]), cex = 0.6)

fviz_dend(hc1, 4, ylab=" ", main = "Dendograma", 
          k_colors = c("blue", "darkorange2","magenta","green"),
          label_cols = as.integer(Fusoes$`Nickel Content`[hc1$order]), cex = 0.6)


rect.hclust(hc1, 2, border="blue")
rect.hclust(hc1, 3, border="red")

plot(hc, hang=-1)
plot(hc1, hang=-1) 
plot(hc2, hang=-1);


# b) a aplicação de um método não hierárquico (k-means);

library(factoextra); 
fviz_nbclust(STDFusoes, kmeans, method = "wss")
# O método sugere que devem ser utilizados k=3 clusters

fviz_nbclust(STDFusoes, kmeans, method = "silhouette")
# O valor máximo é atingido com k=3 clusters
# nas respostas seguintes vamos seguir com 2 ou 3  cluster

# c) a justificação do número de clusters escolhido (em cada método);
set.seed(500)
kmF3 <- kmeans(STDFusoes, 3) 
print(kmF3)

# intra-clusters 

# 61452.2182 primeiro cluster
# 47227.873  segundo cluster
# 6442.842 Terceiro Cluster
# between_SS / total_SS =  16.6 %

kmF3$cluster
# Vetor com a classificação de cada indivíduo

kmF3$centers
# Centróides das variáveis estandardizadas

kmF3$totss
# Soma total dos quadrados é 76584

kmF3$withinss
# 61452.2182 primeiro cluster
# 2047.1062  segundo cluster
# 390.7694 Terceiro Cluster
kmF3$tot.withinss
# totalizando  63890.09

kmF3$betweenss
# variação inter clusters  12693.91
kmF3$size
# dimensão dos clusters:
# 6016 no cluster 1 
# 281 no cluster 2
# 86 no cluster 3

# Centróides com 3 clusters:
sapply(Fusoes[kmF3$cluster == 1, 5:16], mean)
sapply(Fusoes[kmF3$cluster == 2, 5:16], mean)
sapply(Fusoes[kmF3$cluster == 3, 5:16], mean)

set.seed(500)
kmF2 <- kmeans(STDFusoes, 2) 
print(kmF2)
#[1] 12866.04 43963.61
kmF2$cluster
# Vetor com a classificação de cada indivíduo

kmF2$centers
# Centróides das variáveis estandardizadas

kmF2$totss
# Soma total dos quadrados é 76584
kmF2$withinss
# 12866.04 primeiro cluster
# 43963.61  segundo cluster
kmF2$tot.withinss

# totalizando  52829.65
kmF2$betweenss
# variação inter clusters 19754.35
kmF2$size
# dimensão dos clusters:
# 1164 no cluster 1 
# 5219 no cluster 2


library(clusterCrit); 
# Calinski_Harabasz", "Silhouette
intCriteria(as.matrix(STDFusoes), kmF3$cluster,"Calinski_Harabasz")
intCriteria(as.matrix(STDFusoes), kmF3$cluster,"Silhouette")
#[1] 633.8003 /$silhouette [1] 0.5102118

intCriteria(as.matrix(STDFusoes), kmF2$cluster,"Calinski_Harabasz")
intCriteria(as.matrix(STDFusoes), kmF2$cluster,"Silhouette")
#[1] 2218.076 / $silhouette [1] 0.4166462


#menor o numero entao com3  cluster é melhor que com 2
#o silhouette define 2 clusters o Calinski 3 


library(cluster); 

fviz_silhouette(silhouette(kmF2$cluster, dist(STDFusoes)))
fviz_silhouette(silhouette(kmF3$cluster, dist(STDFusoes)))
#confirma-se que pelo silhouette 2 clusters são melhores que 3

# Representar graficamente os clusters obtidos.

fviz_cluster(kmF2, STDFusoes, main=" ", ellipse.type="norm")
fviz_cluster(kmF2, STDFusoes, main=" ")

fviz_cluster(kmF3, STDFusoes, main=" ", ellipse.type="norm")
fviz_cluster(kmF3, STDFusoes, main=" ")

# a analise de fviz_cluster evidencia 2 clusters 

#Compare os clusters obtidos com as espécies de Aço 
#    utilizando medidas de validação externa.


# d) a comparação dos grupos obtidos com os grupos definidos pelas variáveis qualitativas utilizadas na primeira questão.

table(Fusoes$`Nickel Content`, kmF2$cluster)

#               1    2
#High Ni        1 5174
#low Ni        56    8
#median Ni    552   37
#no Ni Alloy  555    0

table(Teste1$`Nickel Content`, kmF2$cluster)

table(Teste1$`Nickel Content`, kmF3$cluster)
kmF4 <- kmeans(STDFusoes, 4) 
print(kmF)
table(Fusoes$`Nickel Content`, kmF3$cluster)
table(Fusoes$Familia, kmF4$cluster)

concordance(as.integer(Fusoes$`Nickel Content`), kmF3$cluster)

#       y       n
#y 13690596   26046
#n   602641 6048870

library(clusterCrit); 
extCriteria(as.integer(Fusoes$`Nickel Content`), kmF2$cluster, "Precision")
extCriteria(as.integer(Fusoes$`Nickel Content`), kmF2$cluster, "recall")
extCriteria(as.integer(Fusoes$`Nickel Content`), kmF2$cluster, "Folkes_Mallows")
extCriteria(as.integer(Fusoes$`Nickel Content`), kmF2$cluster, "Rand")
#$precision #[1] 0.9578373
#$recall [1] 0.9981011
#$folkes_mallows [1] 0.977762
#$rand [1] 0.9691339
library(aricode); 
ARI(Fusoes$`Nickel Content`, kmF2$cluster)
#[1] 0.9282216


#4. [4 val.] A aplicação do algoritmo de classificação naive Bayes, em particular:

library(MASS)

# a)  a aplicação do algoritmo para classificar as variáveis qualitativas
#utilizadas na primeira questão;
View(Fusoes)
names(Fusoes)

table(Fusoes$`Nickel Content`)
table(Fusoes$Familia)


Fusoes$`Nickel Content` <- ordered(Fusoes$`Nickel Content`,labels=c("High Ni","	low Ni","median Ni","no Ni Alloy"))

#Fusoes$Familia <- ordered(Fusoes$Familia,labels=c("Austenitico","Duplex","Martensitico","Martensitico PHD"))
#prop.table(table(Fusoes$Familia))
#pie(Fusoes$Familia)

Fusoes2 <- as.data.frame(scale(Fusoes[,5:16]))

pairs(Fusoes[,8:14],col=Fusoes$`Nickel Content`)

#pairs(Fusoes[,5:16],col=Fusoes$Familia)
?raw
library(e1071);
NBC <- naiveBayes(Fusoes[,8:14], Fusoes$`Nickel Content`)
NBC.prob <- predict(NBC, Fusoes[,5:16], type="raw")
NBC.class <- predict(NBC, Fusoes[,5:16])
head(NBC.prob)
NBC.class


# b) a análise da fiabilidade das classificações obtidas utilizando a matriz de confusão e medidas associadas.


library(caret);
confusionMatrix(NBC.class, Fusoes$`Nickel Content`,mode="prec_recall")

#Statistics by Class:

#  Class: no Ni Alloy Class: \tlow Ni Class: median Ni Class: High Ni
#Sensitivity                      0.9990         0.93750          0.94058        0.99279
#Specificity                      0.9702         0.99953          0.99931        0.99914
#Pos Pred Value                   0.9931         0.95238          0.99283        0.99101
#Neg Pred Value                   0.9958         0.99937          0.99399        0.99931
#Prevalence                       0.8107         0.01003          0.09228        0.08695
#Detection Rate                   0.8100         0.00940          0.08679        0.08632
#Detection Prevalence             0.8156         0.00987          0.08742        0.08711
#Balanced Accuracy                0.9846         0.96851          0.96994        0.99597




View(final_dataset)
max(final_dataset$km_driven)
