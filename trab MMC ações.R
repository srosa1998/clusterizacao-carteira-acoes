# 1. Pré-processamento: limpeza e codificação

# Carregando as bibliotecas
library(tidyverse)
library(gdata)
library(caret)
library(readxl)
library(DT)
library(ggplot2)
library(ggthemes)
library(scales)
library(gridExtra)
library(proxy)
library(factoextra)
library(solitude)
library(rattle)
library(rpart)
library(e1071)
library(corrplot)

# Setando o diretório 

setwd("C:\\Users\\samantha.rosa\\OneDrive - Órama DTVM\\Docs pessoais\\R")

dados = readxl::read_xlsx("indicadores minha carteira - 30.12.20 a 20.05.21.xlsx",na = c("","N/A", "N/D", "NULL"))

# 2. Análise exploratória

dados %>% DT::datatable()

summary(dados)

# colocando alguns dados em % para ficar melhor de plotar
dados$`Máximo Draw Up (em %)` = dados$`Máximo Draw Up`*100
dados$`Máximo Drawdown (em %)` = dados$`Máximo Drawdown`*100
dados$`Volatilidade (em %)`= dados$Volatilidade*100
dados$`Alfa - IGP-M/IQT Ações Ativo (em %)` = dados$`Alfa - IGP-M/IQT Ações Ativo`*100
dados$`Beta - IGP-M/IQT Ações Ativo (em %)` = dados$`Beta - IGP-M/IQT Ações Ativo`*100
dados$`Dividend Yield (Cotação Final - em %)`=dados$`Dividend Yield (Cotação Final)`*100
dados$`Retorno do Maior Período de Ganhos (em %)`= dados$`Retorno do Maior Período de Ganhos`*100
dados$`Retorno do Maior Período de Perdas (em %)` = dados$`Retorno do Maior Período de Perdas`*100

dados$`Máximo Draw Up` = NULL
dados$`Máximo Drawdown` = NULL
dados$Volatilidade = NULL
dados$`Alfa - IGP-M/IQT Ações Ativo` = NULL
dados$`Beta - IGP-M/IQT Ações Ativo` = NULL
dados$`Dividend Yield (Cotação Final - em %)` = NULL
dados$`Retorno do Maior Período de Ganhos` = NULL
dados$`Retorno do Maior Período de Perdas`= NULL

dados %>% DT::datatable()

# Verificando % de missing data no dataset

n = nrow(dados) # total de linhas
colSums(is.na(dados))*100/n # porcentagem de NA por coluna ==> Não há valores nulos

# Verificando a presença de outliers

  # Máximo draw up
boxplot_drawUP = ggplot(data = dados, aes(x=`Máximo Draw Up (em %)`))+
  geom_boxplot(col="#228B22", 
               fill="green")+
  labs(title="Boxplot - Máximo Draw Up (em %)")+
  theme_clean()

transf_eixo = number_format(scale = 1, accuracy = .1, decimal.mark = ",")

densidade_drawUP = ggplot(data = dados, aes(x=`Máximo Draw Up (em %)`)) + 
  geom_density(col="#228B22", 
                 fill="green",
               alpha = 0.5)+
  scale_y_continuous(labels = transf_eixo) +
  labs(title="Histograma - Máximo Draw Up (em %)")+
  theme_clean()

gridExtra::grid.arrange(boxplot_drawUP,densidade_drawUP)

 # Máximo draw down
boxplot_drawdown = ggplot(data = dados, aes(x=`Máximo Drawdown (em %)`))+
  geom_boxplot(col="#228B22", 
               fill="green")+
  labs(title="Boxplot - Máximo Drawdown (em %)")+
  theme_clean()

transf_eixo = number_format(scale = 1, accuracy = .1, decimal.mark = ",")

densidade_drawdown = ggplot(data = dados, aes(x=`Máximo Drawdown (em %)`)) + 
  geom_density(col="#228B22", 
               fill="green",
               alpha = 0.5)+
  scale_y_continuous(labels = transf_eixo) +
  labs(title="Histograma - Máximo Drawdown (em %)")+
  theme_clean()

gridExtra::grid.arrange(boxplot_drawdown,densidade_drawdown)

 #  Observações no Máximo Draw Up
boxplot_OBSdrawup = ggplot(data = dados, aes(x=`Observações no Máximo Draw Up`))+
  geom_boxplot(col="#228B22", 
               fill="green")+
  labs(title="Boxplot - Observações no Máximo Draw Up")+
  theme_clean()

transf_eixo = number_format(scale = 1, accuracy = .1, decimal.mark = ",")

densidade_OBSdrawup = ggplot(data = dados, aes(x=`Observações no Máximo Draw Up`)) + 
  geom_density(col="#228B22", 
               fill="green",
               alpha = 0.5)+
  scale_y_continuous(labels = transf_eixo) +
  labs(title="Histograma - Observações no Máximo Draw Up")+
  theme_clean()

gridExtra::grid.arrange(boxplot_OBSdrawup,densidade_OBSdrawup)

 # Observações no Máximo Drawdown
boxplot_OBSdrawdown = ggplot(data = dados, aes(x=`Observações no Máximo Drawdown`))+
  geom_boxplot(col="#228B22", 
               fill="green")+
  labs(title="Boxplot - Observações no Máximo Drawdown")+
  theme_clean()

transf_eixo = number_format(scale = 1, accuracy = .1, decimal.mark = ",")

densidade_OBSdrawdown = ggplot(data = dados, aes(x=`Observações no Máximo Drawdown`)) + 
  geom_density(col="#228B22", 
               fill="green",
               alpha = 0.5)+
  scale_y_continuous(labels = transf_eixo) +
  labs(title="Histograma - Observações no Máximo Drawdown")+
  theme_clean()

gridExtra::grid.arrange(boxplot_OBSdrawdown,densidade_OBSdrawdown)

 # Número de Vezes Acima do Benchmark - IQT Ações Ativo
boxplot_VezAcima = ggplot(data = dados, aes(x=`Número de Vezes Acima do Benchmark - IQT Ações Ativo`))+
  geom_boxplot(col="#228B22", 
               fill="green")+
  labs(title="Boxplot - Número de Vezes Acima do Benchmark - IQT Ações Ativo")+
  theme_clean()

transf_eixo = number_format(scale = 1, accuracy = .1, decimal.mark = ",")

densidade_VezAcima = ggplot(data = dados, aes(x=`Número de Vezes Acima do Benchmark - IQT Ações Ativo`)) + 
  geom_density(col="#228B22", 
               fill="green",
               alpha = 0.5)+
  scale_y_continuous(labels = transf_eixo) +
  labs(title="Histograma - Número de Vezes Acima do Benchmark - IQT Ações Ativo")+
  theme_clean()

gridExtra::grid.arrange(boxplot_VezAcima,densidade_VezAcima)

 # % de Vezes Abaixo do Benchmark - IQT Ações Ativo
boxplot_VezAbaixo = ggplot(data = dados, aes(x=`% de Vezes Abaixo do Benchmark - IQT Ações Ativo`))+
  geom_boxplot(col="#228B22", 
               fill="green")+
  labs(title="Boxplot - % de Vezes Abaixo do Benchmark - IQT Ações Ativo")+
  theme_clean()

transf_eixo = number_format(scale = 1, accuracy = .1, decimal.mark = ",")

densidade_VezAbaixo = ggplot(data = dados, aes(x=`% de Vezes Abaixo do Benchmark - IQT Ações Ativo`)) + 
  geom_density(col="#228B22", 
               fill="green",
               alpha = 0.5)+
  scale_y_continuous(labels = transf_eixo) +
  labs(title="Histograma - % de Vezes Abaixo do Benchmark - IQT Ações Ativo")+
  theme_clean()

gridExtra::grid.arrange(boxplot_VezAbaixo,densidade_VezAbaixo)

 # Dividend Yield (Cotação Final)
boxplot_DividYield = ggplot(data = dados, aes(x=`Dividend Yield (Cotação Final)`))+
  geom_boxplot(col="#228B22", 
               fill="green")+
  labs(title="Boxplot - Dividend Yield (Cotação Final)")+
  theme_clean()

transf_eixo = number_format(scale = 1, accuracy = .1, decimal.mark = ",")

densidade_DividYield  = ggplot(data = dados, aes(x=`Dividend Yield (Cotação Final)`)) + 
  geom_density(col="#228B22", 
               fill="green",
               alpha = 0.5)+
  scale_y_continuous(labels = transf_eixo) +
  labs(title="Histograma - Dividend Yield (Cotação Final)")+
  theme_clean()

gridExtra::grid.arrange(boxplot_DividYield ,densidade_DividYield)

# Volatilidade (em %)
boxplot_vol = ggplot(data = dados, aes(x=`Volatilidade (em %)`))+
  geom_boxplot(col="#228B22", 
               fill="green")+
  labs(title="Boxplot - Volatilidade (em %)")+
  theme_clean()

transf_eixo = number_format(scale = 1, accuracy = .1, decimal.mark = ",")

densidade_vol  = ggplot(data = dados, aes(x=`Volatilidade (em %)`)) + 
  geom_density(col="#228B22", 
               fill="green",
               alpha = 0.5)+
  scale_y_continuous(labels = transf_eixo) +
  labs(title="Histograma - Volatilidade (em %)")+
  theme_clean()

gridExtra::grid.arrange(boxplot_vol ,densidade_vol)

# Alfa - IGP-M/IQT Ações Ativo (em %)

boxplot_alfa = ggplot(data = dados, aes(x=`Alfa - IGP-M/IQT Ações Ativo (em %)`))+
  geom_boxplot(col="#228B22", 
               fill="green")+
  labs(title="Boxplot - Alfa - IGP-M/IQT Ações Ativo (em %)")+
  theme_clean()

transf_eixo = number_format(scale = 1, accuracy = .1, decimal.mark = ",")

densidade_alfa  = ggplot(data = dados, aes(x=`Alfa - IGP-M/IQT Ações Ativo (em %)`)) + 
  geom_density(col="#228B22", 
               fill="green",
               alpha = 0.5)+
  scale_y_continuous(labels = transf_eixo) +
  labs(title="Histograma - Alfa - IGP-M/IQT Ações Ativo (em %)")+
  theme_clean()

gridExtra::grid.arrange(boxplot_alfa, densidade_alfa)

# Beta - IGP-M/IQT Ações Ativo (em %)

boxplot_beta = ggplot(data = dados, aes(x=`Beta - IGP-M/IQT Ações Ativo (em %)`))+
  geom_boxplot(col="#228B22", 
               fill="green")+
  labs(title="Boxplot - Beta - IGP-M/IQT Ações Ativo (em %)")+
  theme_clean()

transf_eixo = number_format(scale = 1, accuracy = .1, decimal.mark = ",")

densidade_beta  = ggplot(data = dados, aes(x=`Beta - IGP-M/IQT Ações Ativo (em %)`)) + 
  geom_density(col="#228B22", 
               fill="green",
               alpha = 0.5)+
  scale_y_continuous(labels = transf_eixo) +
  labs(title="Histograma - Beta - IGP-M/IQT Ações Ativo (em %)")+
  theme_clean()

gridExtra::grid.arrange(boxplot_beta, densidade_beta)

# Retorno do Maior Período de Ganhos (em %)

boxplot_RetGanhos = ggplot(data = dados, aes(x=`Retorno do Maior Período de Ganhos (em %)`))+
  geom_boxplot(col="#228B22", 
               fill="green")+
  labs(title="Boxplot - Retorno do Maior Período de Ganhos (em %)")+
  theme_clean()

transf_eixo = number_format(scale = 1, accuracy = .1, decimal.mark = ",")

densidade_RetGanhos  = ggplot(data = dados, aes(x=`Retorno do Maior Período de Ganhos (em %)`)) + 
  geom_density(col="#228B22", 
               fill="green",
               alpha = 0.5)+
  scale_y_continuous(labels = transf_eixo) +
  labs(title="Histograma - Retorno do Maior Período de Ganhos (em %)")+
  theme_clean()

gridExtra::grid.arrange(boxplot_RetGanhos, densidade_RetGanhos)

# Retorno do Maior Período de Perdas (em %)

boxplot_RetPerdas = ggplot(data = dados, aes(x=`Retorno do Maior Período de Perdas (em %)`))+
  geom_boxplot(col="#228B22", 
               fill="green")+
  labs(title="Boxplot - Retorno do Maior Período de Perdas (em %)")+
  theme_clean()

transf_eixo = number_format(scale = 1, accuracy = .1, decimal.mark = ",")

densidade_RetPerdas  = ggplot(data = dados, aes(x=`Retorno do Maior Período de Perdas (em %)`)) + 
  geom_density(col="#228B22", 
               fill="green",
               alpha = 0.5)+
  scale_y_continuous(labels = transf_eixo) +
  labs(title="Histograma - Retorno do Maior Período de Perdas (em %)")+
  theme_clean()

gridExtra::grid.arrange(boxplot_RetPerdas, densidade_RetPerdas)

 #==> Vemos a maioria dos atributos com distribuições assimétricas, vamos investigar mais a presença de outliers com o método isolation forest abaixo

# Isolation Forest (detecção de outliers)

iforest = isolationForest$new(sample_size=22)
iforest$fit(dados)
scores_train = iforest$predict(dados)

dados$pred_outlier = scores_train[order(anomaly_score, decreasing = TRUE)]
dados_com_id_e_data$pred_outlier = scores_train[order(anomaly_score, decreasing = TRUE)]

plot(dados$pred_outlier$anomaly_score) # vemos que a partir de um score de 0,56, a anomalia fica estável. Vamos considerar como outliers as empresas acima desse score, mas não vamos retirá-las da base, pois talvez essas empresas possam formar um cluster de destaque, positivo ou negativo, para comparar com relação às demais.

dados$is_outlier = data.frame(ifelse(dados$pred_outlier$anomaly_score >= 0.56, "sim", "não")) # a moda para o score de anomalia é de 0,57 aproximadamente

outliers = dados %>% filter(is_outlier=="sim") # guardando os outilers
count(outliers) 

# 3. Redução de dimensionalidade


# O intuito é ver se cada atributo aqui adiciona informação nova sobre as ações ou se são redundantes. Caso sejam, podemos resumir nosso dataset combinando alguns atributos em um número menor de dimensões.

 # Colocando dados em uma mesma escala centrada em 0

dados_scale = scale(dados[,2:13])
rownames(dados_scale) = dados$Empresa

summary(dados_scale)

dados_scale['AMBEV S/A ON - ABEV3',] # Vemos que a primeira empresa da nossa carteira tem um desempenho acima da média no período analisado em relação ao número de dias na máxima (Observações no Máximo Draw Up), por exemplo; e um desempenho abaixo da média com relação ao rendimento dos seus dividendos (Dividend Yield (Cotação Final)), por exemplo

 # Correlações

corrplot(cor(dados_scale), order = "hclust")
pairs.panels(dados_scale, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
) # parece que temos atributos bastante correlacionados (uma análise mais formal, estatisticamente falando, faria testes de significância de correlação), provavelmente conseguiremos resumir este dataset em uma combinação com menos dimensões

 # PCA
dados_scale = data.frame(dados_scale)

pca = prcomp(~ .,dados_scale)
pca_df = data.frame(x=pca$x[,"PC1"], y=pca$x[,"PC2"])

 # Plotando as direções de maior variância (importância para análise)

ggplot(data = pca_df, aes(x,y)) + 
  geom_point(col="#228B22", 
             fill="green") + 
  xlab("PC1") + 
  ylab("PC2") +
  theme_clean()

 # resultado do PCA

summary(pca) # vemos que no PC3 ficamos mais perto dos 80% da variância (regra de ouro)


fviz_eig(pca, # Plotando a contribuição (proporção da variância) dos 3 PCs (variáveis originais resumidas em 3 componentes principais)
         ncp = 3, 
         addlabels=TRUE, 
         barfill = "#228B22", 
         barcolor = "green", 
         ggtheme = theme_clean(), 
         main = "% de variância explicado por cada dimensão (PC) adicionada")

# contribuições das variáveis um dataset reduzido de 12 variáveis para 3 PCs (ou seja, com o resumo dessas 12 variáveis originais em 3 componentes principais, a contribuição de cada variável original para a variância é esta apresentada no gráfico)

fviz_contrib(pca, choice = "var", axes =3, top = 12) # a linha vermelha representa a contribuição média de cada uma das 12 variáveis originais para os 3 PCs

#==> De fato, como prevemos vendo as correlações, havia uma redundância nas variáveis originais e que foi bastante reduzida com a técnica PCA (apenas 3 das 12 dimensões já explicam quase 80% da variância dos dados)

####### dataset reduzido #######

df_pca_3 = pca$x[,1:3]

 # Vamos ver como ficaram as correlações pós redução de dimensionalidade



corrplot(cor(df_pca_3), order = "hclust")
pairs.panels(df_pca_3, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
) # Perfeito, agora não temos redundância nos dados (uma correlação alta entre os atributos não significa uma relação causal entre eles mas sim que a informação trazida por eles é redundante). Além disso, com esta técnica, estou retirando do meu dado qualquer ruído de medição, ou seja, se há alguma especificidade na minha amostra, eu retiro e fico apenas com as características gerais dos meus dados.


# Escolhendo o número de clusters (método do cotovelo + silhueta)

fviz_nbclust(df_pca_3, kmeans, method = "wss") # kmeans usa dist euclidiana # método within sum of squares ("cotovelo")
 # 6 clusters por este método

# Escolhendo o número de clusters pela silhueta
fviz_nbclust(df_pca_3, kmeans, method = "silhouette")
 # 2 clusters por este método (?? sempre dá 2...ver melhor)

# silhueta
set.seed(123)
kClusters = 3
km_res = eclust(df_pca_3, "kmeans", k=kClusters, graph = FALSE, stand=FALSE, iter.max = 100, 
                nstart = 100) # Visualize k-means clusters

fviz_silhouette(km_res, palette = "jco", ggtheme = theme_clean())

# 3 clusters é o número que me dá a maior área: 0,45

#==> Independente do critério utilizado para a escolha do número de clusters, temos principalmente que avaliar o que seria aprendido por meio desses agrupamentos. Então, vamos visualizar quais empresas ficariam no mesmo grupo usando 3 e 6 clusters


dendo = df_pca_3 %>% dist %>% hclust
plot(dendo)
rect.hclust(dendo, k = 3, border = "green")
rect.hclust(dendo, k = 6, border = "red")

# pelo dendograma (clusterização hierárquica?), ficaríamos com 6 clusters (assim como pelo método do cotovelo)


# 4. Interpreção dos clusters - com 6 clusters

set.seed(123)
kClusters = 6
km_res = eclust(df_pca_3, "kmeans", k=kClusters,graph = FALSE, stand=FALSE, iter.max = 100, 
                nstart = 100) # Visualize k-means clusters

# Utilizando árvore nos dados originais

dados['label'] = factor(km_res$cluster)
#rpart.control(minsplit = 20, minbucket = round(minsplit/3), cp = 0.01, 
#              maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
#              surrogatestyle = 0, maxdepth = 30, .)
c = rpart.control(minsplit = 1, minbucket=1)
model = rpart(formula = label ~`Observações no Máximo Draw Up`+
                `Observações no Máximo Drawdown`+
                `Número de Vezes Acima do Benchmark - IQT Ações Ativo`+
                `Dividend Yield (Cotação Final)`+
                `Máximo Draw Up (em %)`+
                `Máximo Drawdown (em %)`+
                `Volatilidade (em %)`+
                `Alfa - IGP-M/IQT Ações Ativo (em %)`+
                `Beta - IGP-M/IQT Ações Ativo (em %)`+
                `Retorno do Maior Período de Ganhos (em %)`+
                `Retorno do Maior Período de Perdas (em %)`+
                `% de Vezes Abaixo do Benchmark - IQT Ações Ativo`
                , data = dados, #method='class', 
              control=c)
fancyRpartPlot(model,sub="Modelo Decision Tree")

# Interpretando a distribuição
# histogramas dos atributos

transf_eixo = number_format(scale = 1, accuracy = .1, decimal.mark = ",")

dados %>%
  gather(dados, value, 2:13) %>%
  ggplot(aes(x=value, fill=label, color=label)) +
  #geom_histogram(binwidth=1, alpha=.5, position="identity") +
  geom_boxplot(alpha=.5) +
  facet_wrap(~dados, scales="free") +
  labs(x="Values", y="Frequency")+
  scale_x_continuous(labels = transf_eixo)+
  theme_clean()

dados %>%
  gather(dados, value, 2:13) %>%
  ggplot(aes(x=value, fill=label, color=label)) +
  #geom_histogram(binwidth=1, alpha=.5, position="identity") +
  geom_density(alpha=.5) +
  facet_wrap(~dados, scales="free") +
  labs(x="Values", y="Frequency")+
  scale_x_continuous(labels = transf_eixo)+
  theme_clean()

fviz_cluster(km_res, data=df_pca_3,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#E46726", "#6D9EC1"),
             #ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE)
