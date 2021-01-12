# Projetos com feedback - 02
# Prevendo demanda de estoque com base em vendas

# Pacotes necessários

library(kableExtra)
library(data.table)
library(stringr)
library(dplyr)
library(naniar)
library(ggplot2)
library(gridExtra)
library(psych)
library(ggcorrplot)
library(randomForest)
library(RColorBrewer)

# obs: paleta de cores usada:
display.brewer.pal(11, 'BrBG')
brewer.pal(11, 'BrBG')

# Baixando datasets e fazendo cópias dos originais (caso julgue necessário - atente-se apenas para o tamanho do dataset e a capacidade de processamento da sua máquina)
# train.csv = principal
# cliente_tabla.csv = join Cliente_ID
# producto_tabla.csv = join Producto_ID
# town_state.csv = join Agencia_ID

df <- fread('train.csv')
#df_original <- fread('train.csv')

cliente <- read.csv('cliente_tabla.csv', sep = ',', header = T, stringsAsFactors = F)
#cliente_original <- read.csv('cliente_tabla.csv', sep = ',', header = T, stringsAsFactors = F)

produto <- read.csv('producto_tabla.csv', sep = ',', header = T, stringsAsFactors = F)
#produto_original <- read.csv('producto_tabla.csv', sep = ',', header = T, stringsAsFactors = F)

local <- read.csv('town_state.csv', sep = ',', header = T, stringsAsFactors = F)
#local_original <- read.csv('town_state.csv', sep = ',', header = T, stringsAsFactors = F)

# Pré-processamento de dados
# entendendo a estrutura dos datasets
# dataset principal
str(df)

# alterando tipos de colunas
# categóricas
df <- as.data.frame(df)
fatores <- c('Agencia_ID', 'Canal_ID', 'Ruta_SAK', 'Cliente_ID', 'Producto_ID')
for (i in colnames(df)) {
    if (i %in% fatores) {
        df[,i] <- as.factor(df[,i])
    }
}

# datas - criando coluna com nome e transformando em fator
df$Semana_Ext <- factor(df$Semana, labels = c("quinta", "sexta", "sábado", "domingo",
                                              "segunda", "terça", "quarta"))
df$Semana <- as.factor(df$Semana)

# dataste cliente
str(cliente)

cliente$Cliente_ID <- as.factor(cliente$Cliente_ID)

# dataset produto
str(produto)

produto$Producto_ID <- as.factor(produto$Producto_ID)

# dataset localidade
str(local)

local$Agencia_ID <- as.factor(local$Agencia_ID)
local$State <- str_replace_all(string = local$State, pattern = 'Ã‰', replacement = 'é')
local$State <- as.factor(local$State)

# Redução do dataset
# Tendo em vista a lentidão nas etapas de pré-processamento proporcionada pelo tamanho do dataset, optei por trabalhar uma amostra do mesmo para que não ocorram demoras e/ou problemas na análise exploratória e no treinamento do algoritmo

df_amostra <- df[sample(1:nrow(df), 100000),]

# obs: disponibilizei na pasta de datasets o conjunto de dados já reduzido

# unindo datasets
# df_geral <- df_amostra %>%
inner_join(cliente, by = 'Cliente_ID') %>%
    inner_join(produto, by = 'Producto_ID') %>%
    inner_join(local, by = 'Agencia_ID')

# data missing
gg_miss_which(df_geral) + 
    labs(title = 'Missing Data - Bimbo Inventory Demand',
         caption = 'Black = No missing Data') +
    theme(plot.title = element_text(hjust = 0.5))

# estrutura dataset final
str(df_geral)

# Análise exploratória

# histogramas de vendas e devoluções - Qual é a qtd de itens vendidos e devolvidos mais recorrente?

ven_hist <- df_geral %>%
    filter(Venta_uni_hoy >= 0)  %>%
    ggplot(aes(Venta_uni_hoy)) + geom_histogram(fill = '#543005') +
    scale_x_continuous(limits = c(0, 100),
                       breaks = seq(0, 100, 10)) +
    xlab('itens vendidos') + ylab(NULL)

dev_hist <- df_geral %>%
    filter(Dev_uni_proxima >= 0)  %>%
    ggplot(aes(Venta_uni_hoy)) + geom_histogram(fill = '#543005') +
    scale_x_continuous(limits = c(0, 100),
                       breaks = seq(0, 100, 10)) +
    xlab('itens devolvidos') + ylab(NULL)

grid.arrange(ven_hist, dev_hist, ncol = 1, nrow = 2, top = 'Histogramas de quantidades de vendas e devoluções')

# qtd vendas e devoluções por top10 agencia (gráfico de colunas)

ven_agencia <- df_geral %>%
    group_by(Agencia_ID) %>%
    summarise(qtd = sum(Venta_uni_hoy)) %>%
    slice_max(qtd, n = 10) %>%
    ggplot(aes(x = reorder(Agencia_ID, -qtd), y = qtd)) +
    geom_col(fill = '#8C510A') +
    labs(subtitle = 'vendas') +
    xlab(NULL) + ylab(NULL)

dev_agencia <- df_geral %>%
    group_by(Agencia_ID) %>%
    summarise(qtd = sum(Dev_uni_proxima)) %>%
    slice_max(qtd, n = 10) %>%
    ggplot(aes(x = reorder(Agencia_ID, -qtd), y = qtd)) +
    geom_col(fill = '#8C510A') +
    labs(subtitle = 'devoluções') +
    xlab(NULL) + ylab(NULL)

grid.arrange(ven_agencia, dev_agencia, ncol = 1, nrow = 2, top = 'Top 10 - Agências que mais venderam e mais devolveram')

# valor de vendas e devoluções por canais (quadros)
df_geral %>%
    group_by(Canal_ID) %>%
    summarise(valor = sum(Venta_hoy)) %>%
    slice_max(valor, n = 5) %>%
    rename('R$' = valor)

df_geral %>%
    group_by(Canal_ID) %>%
    summarise(valor = sum(Dev_proxima)) %>%
    slice_max(valor, n = 5) %>%
    rename('R$' = valor)

# demanda por dia da semana (boxplot)
df_geral %>%
    ggplot(aes(x = Semana_Ext, y = Demanda_uni_equil)) +
    geom_boxplot(outlier.colour = '#BF812D', outlier.alpha = .5) +
    xlab(NULL) + ylab('Demanda') + ggtitle('Demanda por dia da semana') +
    scale_y_continuous(limits = c(0, 1000),
                       breaks = seq(0, 1000, 200)) +
    theme(plot.title = element_text(hjust = 0.5))

# gráfico de correlação comum

df_cor_temp <- df_geral

df_cor_temp$Semana <- as.numeric(df_cor_temp$Semana)
df_cor_temp$Agencia_ID <- as.numeric(df_cor_temp$Agencia_ID)
df_cor_temp$Canal_ID <- as.numeric(df_cor_temp$Canal_ID)
df_cor_temp$Producto_ID <- as.numeric(df_cor_temp$Producto_ID)

df_cor <- round(cor(df_cor_temp[,c(1:3, 6:11)]), 1)

ggcorrplot(df_cor, title = 'Correlação de Variáveis', legend.title = NULL,
           colors = c("#543005", "#C7EAE5", "#003C30")) +
    theme(plot.title = element_text(hjust = 0.5))

# Processo de Machine Learning

# Separação em treino e teste
# o dataset em trabalho já é o de treino, baixando o de teste para previsções
teste <- fread('test.csv')

# Treinamento do algoritmo
# Modelo 1 - Regressão linear
# Conforme visto, além das variáveis numéricas, a única categórica que parece ter alguma relação e/ou influência no conjunto de dados é a Canal_ID
# Por isso, além das numéricas, vamos utilizá-la na primeira versão do nosso modelo

modelo1 <- lm(Demanda_uni_equil ~ Venta_uni_hoy + Venta_hoy + Dev_uni_proxima + Dev_proxima + Canal_ID, data = df_geral)

# Previsões e teste
previsao1 <- predict(modelo1, data = teste)

# Análises

# Resumo do modelo

summary(modelo1)

# variáveis com mts asterísticos - a análise exploratória foi boa e ajudou a pré selecionar variáveis relevantes para o modelo como, por exemplo: citar
# Multiple R-Squared altíssimo - muito próximo de 1, indicando alto nível de precisão
# p-value baixo, indicando a alta probabilidade das variáveis serem relvantes para o modelo

# Score

score1 <- data.frame(atual = df_geral$Demanda_uni_equil,
                     previsao = previsao1)
ggplot(score1, aes(x = atual, y = previsao1)) + 
    geom_point(color = '#80CDC1') +
    geom_smooth(method = 'lm', color = '#543005') +
    labs(title = 'Linear Model - Atual x Previsão',
         x = 'Valores atuais',
         y = 'Valores Previstos') +
    theme(plot.title = element_text(hjust = 0.5))

# Modelo 2 - Random Forest

modelo2 <- randomForest(Demanda_uni_equil ~ Venta_uni_hoy + Venta_hoy + Dev_uni_proxima + Dev_proxima + Canal_ID, data = df_geral)

# Previsões e teste

previsao2 <- predict(modelo2, data = teste)

# Análises

# Resumo do Modelo

print(modelo2)

# Score
score2 <- data.frame(dia_da_semana = df_geral$Semana_Ext,
                     atual = df_geral$Demanda_uni_equil,
                     previsao = previsao2)

# análise temporal atual x previsão
# linhas atual x previsão divergem um pouco mas não tanto
score2 %>%
    group_by(dia_da_semana) %>%
    summarise(qtd1 = sum(atual),
              qtd2 = sum(previsao)) %>%
    ungroup() %>%
    ggplot() + 
    geom_line(aes(x = dia_da_semana, y = qtd1, group = 1), color = '#BF812D') +
    geom_line(aes(x = dia_da_semana, y = qtd2, group = 1), color = '#35978F') +
    labs(title = 'Random Forest - Atual x Previsão',
         x = NULL,
         y = NULL) +
    theme(plot.title = element_text(hjust = 0.5))

# distribuição de resíduos
# se aproxima da distribuição normal, resíduos próximo de zero
score2 %>%
    mutate(residuos = atual - previsao) %>%
    ggplot(aes(x = residuos)) +
    geom_histogram(binwidth = 1, fill = "#543005", color = "#8C510A") +
    scale_x_continuous(limits = c(-200, 400),
                       breaks = seq(-200, 400, 200)) +
    labs(title = 'Distribuição de Resíduos',
         x = 'Resíduos',
         y = NULL) +
    theme(plot.title = element_text(hjust = 0.5))