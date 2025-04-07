# REGRESSAO DE DADOS: VIOLÊNCIA E DESIGUALDADE
install.packages("corrplot")
install.packages("magrittr")
install.packages("data.table")
install.packages("car")
library(car)
library(dplyr)
library(corrplot)
library(magrittr)
library(tidyverse)
library(data.table)
dados<-read.csv2("C://Users//Cliente//Downloads//Dados de violência por municipios de PE - PE.csv", T,sep=",")
dados
########################################################
#mineração
dados<-as.data.frame(dados)
dados_1<-dados %>%
  dplyr :: select(vitimas, pib_2022, remun_real_media_2022,ibge_matricula_em_2022, população_2022, 
                  ibge_conect_rede_de_esgoto_2022, pop_graduada_2022)

summary(dados_1)

dados_1 <- dados_1 %>%
  dplyr :: filter(vitimas != "N/A" & pib_2022 != "N/A" &  remun_real_media_2022 != "N/A" & ibge_matricula_em_2022 != "N/A" & 
                    população_2022 != "N/A" & ibge_conect_rede_de_esgoto_2022 != "N/A" & pop_graduada_2022)

dados_1 <- dados_1 %>% 
  mutate(ibge_conect_rede_de_esgoto_2022 = as.numeric(gsub("%", "", ibge_conect_rede_de_esgoto_2022)),)
dados_1 <- dados_1 %>% mutate(across(everything(), as.numeric))


########################################################
#Deixando os dados proporcionais 

dados_1 <- dados_1 %>% mutate(vitimas = vitimas * 1000/ população_2022)
dados_1 <- dados_1 %>% mutate(pib_2022 = pib_2022 * 1000/ população_2022)
dados_1 <- dados_1 %>% mutate(ibge_matricula_em_2022 = ibge_matricula_em_2022/ população_2022)
dados_1 <- dados_1 %>% mutate(pop_graduada_2022 = pop_graduada_2022/ população_2022)

########################################################
#Escalonando os dados
dados_scaled <- scale(dados_1)
dados_scaled <- as.data.frame(dados_scaled)

#######################################################
#Ver a correlação das variáveis pelo coorplot
dim(dados_1)
x<-dados_1 
correlations<-cor(x)
correlations
corrplot(correlations, method = "color", type = "lower" )
######################################################
#Plotando o VIF para analisar Multicolinearidade entre as variáveis

vif(modelo)
#Plotando para vizualizar o VIF
valores_vif<- vif(modelo)
barplot(valores_vif, 
        main = "VIF para Variáveis Independentes", 
        ylab = "VIF", 
        col = "lightblue", 
        las = 2,  # Rotaciona os rótulos das variáveis
        cex.names = 0.8)  # Tamanho dos rótulos
abline(h = 5, col = "red", lty = 2)  # Linha de referência para VIF = 5
abline(h = 10, col = "red", lty = 2) # Linha de referência para VIF = 10
#######################################################
#####################################################
# PLOTS
summary(dados_1)
#Plots simples para ver as variáveis (não foi muito utilizada por nós)
plot(as.factor(dados_1$vitimas), xlab="Vitimas")
plot(as.factor(dados_1$pib_2022))
plot(as.factor(dados_1$remun_real_media_2022))
plot(as.factor(dados_1$ibge_matricula_em_2022))
plot(as.factor(dados_1$população_2022))
plot(as.factor(dados_1$ibge_conect_rede_de_esgoto_2022))
plot(as.factor(dados_1$pop_graduada_2022))

#Boxplot para ver as variáveis (um pouco mais utilizada pelo grupo)

boxplot(dados_1$vitimas)
boxplot.stats(dados_1$vitimas)

boxplot(dados_1$pop_graduada_2022)
boxplot(dados_1$pop_graduada_2022, log = "y")
boxplot.stats(dados_1$pop_graduada_2022)


boxplot(dados_1$pop_graduada_2022)
boxplot(dados_1$pop_graduada_2022, xlab="pop_graduada_2022 ")

boxplot(dados_scaled$pop_graduada_2022)
boxplot(dados_scaled$pop_graduada_2022, xlab="pop_graduada_2022")

########################################################
#Entendendo melhor os dados (o codigo foi replicado com diversas variáveis)

#Fazendo os quartis para explorar os dados
quartis <- quantile(dados_1$população_2022, probs = c(0.25, 0.5, 0.75))
dados_1 <- dados_1 %>%dplyr :: filter(população_2022 >= quartis[2])

#Filtrar os 25% maiores valores
quartis <- quantile(dados_1$vitimas, probs = c(0.5, 0.75))

df_25_maiores <- dados_1 %>%  filter(vitimas >= quartis[1])
df_25_menores <- dados_1 %>%  filter(pop_graduada_2022 <= quartis[1])

df_25_maiores <- df_25_maiores %>% mutate(grupo = "50% Maiores")
df_25_menores <- df_25_menores %>% mutate(grupo = "25% Menores")
dados_1<-dados_combinados <- bind_rows(df_25_maiores, df_25_menores)

#foi pra entender pq as observações aumentaram em ddos combinados
print(names(df_25_maiores))
print(names(df_25_menores))
print(nrow(df_25_maiores))
print(nrow(df_25_menores))
################################################################

remover_outliers <- function(x, multiplicador = 1.5) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  limite_inferior <- Q1 - multiplicador * IQR
  limite_superior <- Q3 + multiplicador * IQR
  x >= limite_inferior & x <= limite_superior
}
# Filtrar outliers dos dados combinados
dados_sem_outliers <- dados_combinados %>%
  group_by(grupo) %>%
  filter(remover_outliers(remun_real_media_2022, multiplicador = 2)) %>%  # Ajuste o multiplicador para remover mais outliers
  ungroup()

# Gerar o boxplot sem outliers
ggplot(dados_sem_outliers, aes(x = grupo, y = remun_real_media_2022, fill = grupo)) +
  geom_boxplot(outlier.shape = NA) +  theme_minimal()

######Boxplot de remuneração########
dados_sem_outliers <- dados_combinados %>%
  group_by(grupo) %>%
  filter(remover_outliers(remun_real_media_2022, multiplicador = 2)) %>%  
  ungroup()
ggplot(dados_sem_outliers, aes(x = grupo, y = remun_real_media_2022, fill = grupo)) +
  geom_boxplot(outlier.shape = NA) +  theme_minimal()

################################################################
#Rodando o PCA novame
pca_result <- prcomp(dados_scaled, center = TRUE, scale. = TRUE)
summary(pca_result)

################################################################
#O grafico de dispersão foi utilizado com muitas variáveis combinadas
# Plotar o gráfico de dispersão
plot(dados_1$vitimas, dados_1$pop_graduada_2022,
     main = "Relação entre Vítimas e Conexão pop_graduada_2022",
     xlab = "Vítimas",
     ylab = "Conexão à pop_graduada_2022 (%)",
     pch = 19, col = "blue")

# Adicionar a linha de regressão
abline(modelo, col = "red")

plot(dados_1$vitimas, dados_1$remun_real_media_2022,
     main = "Relação entre Vítimas e Conexão remun_real_media_2022",
     xlab = "Vítimas",
     ylab = "Conexão à remun_real_media_2022 (%)",
     pch = 19, col = "blue")
abline(modelo, col = "red")

#######################################################
#Usando o pca novamente

#combinação de variaveis que vai gerar novas variáveis com baixa multicolinearidade
pca<-prcomp(x, center = TRUE, scale. = TRUE)
summary(pca)

#analisando a matriz de pesos
pca$rotation
#usar o numero de componentes principais que expliquem pelo menos 95% da variancia
explained_variance <- summary(pca)$importance[3, ]
num_components <- which(cumsum(explained_variance) >= 0.95)[1]
explained_variance
num_components

#obter os componentes principais

X_pca <- pca$x[, 1:num_components]

#criar um data frame com os componentes principais para usar no modelo

X_pca_df <- as.data.frame(X_pca)
cor_melted <- melt(correlations)

# Gráfico da variância explicada
plot(summary(pca)$importance[2, ], type = "b", 
     xlab = "Componentes Principais", 
     ylab = "Variância Explicada", 
     main = "Variância Explicada por Componente Principal")
abline(v = num_components, col = "red", lty = 2)

###############################################################
#Fazendo a modelagem por LM
modelo <-lm(formula = vitimas ~ -1 + ibge_matricula_em_2022 + população_2022 + 
              pop_graduada_2022, data = dados_scaled)
summary(modelo)
#############################################################
#Plots de correlaçaõ com ggplot
ggplot(dados_1, aes(x = pop_graduada_2022, y = vitimas)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "red") +
  labs(title = "Relação entre pop_graduada_2022 e Vítimas (com termo quadrático)",
       x = "pop_graduada_2022 Média 2022",
       y = "Vítimas") +
  theme_minimal()

ggplot(dados_1, aes(x = ibge_matricula_em_2022, y = vitimas)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "red") +
  labs(title = "Relação entre ibge_matricula_em_2022 e Vítimas (com termo quadrático)",
       x = "ibge_matricula_em_2022 Média 2022",
       y = "Vítimas") +
  theme_minimal()

ggplot(dados_1, aes(x = pib_2022, y = vitimas)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "red") +
  labs(title = "Relação entre pib e Vítimas (com termo quadrático)",
       x = "pib Média 2022",
       y = "Vítimas") +
  theme_minimal()

ggplot(dados_1, aes(x = ibge_conect_rede_de_esgoto_2022, y = vitimas)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "red") +
  labs(title = "Relação entre ibge_conect_rede_de_esgoto_2022 e Vítimas (com termo quadrático)",
       x = "ibge_conect_rede_de_esgoto_2022 Média 2022",
       y = "Vítimas") +
  theme_minimal()

ggplot(dados_1, aes(x = população_2022, y = vitimas)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "red") +
  labs(title = "Relação entre população_2022 e Vítimas (com termo quadrático)",
       x = "população_2022 Média 2022",
       y = "Vítimas") +
  theme_minimal()

################################################################