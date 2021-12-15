setwd("/home/heitor/Área de Trabalho/R Projects/Análise Macro/Lab 5")
library(tidyverse)
library(class)
library(gmodels)
library(gt)

#######   Importando os Dados

dt <- read_csv("wisc_bc_data.csv") %>% as_tibble()

#######   Tratando os Dados

dt$id <- NULL
dt$diagnosis <- factor(dt$diagnosis,
                       levels = c('B', 'M'),
                       labels = c('Benigno', 'Maligno'))
glimpse(dt)

#######   Aplicando um Escalonamento Positivo

scale1 <- function(x){ return( (x-min(x)) / (max(x)-min(x)) ) }
dt1 <- as.data.frame(lapply(dt[2:31],scale1)) %>% as_tibble()
dt1$diagn <- dt$diagnosis

#######   Separando entre Treino e Teste
set.seed(777)
train <- dt1 %>% sample_frac(.,0.8)
sid   <- as.numeric(rownames(train))
test  <- dt1[-sid,]
remove(sid)

#######   Alpicando o Modelo K-NN e Acurácia
# as amostras de treino e teste devem ser SOMENTE numéricos, a variável-fator do treino deve ser colocada em 'cl'. Devemos então retirar a variável fator, q é a trigésima primeira:

knn0 <- knn(train = train[1:30],
            test = test[1:30],
            cl = train$diagn,
            k = 21)

rslt0 <- data.frame(Original = c(test$diagn),
                     Predito = c(knn0))

t0 <- table(rslt0$Original, rslt0$Predito)

Acc0 <- sum(diag(t0))/sum(t0)
F_Ben0 <- sum(t0[2,1])/sum(t0[,1])
F_Mal0 <- sum(t0[1,2])/sum(t0[,2])

CrossTable(x = test$diagn, 
           y = knn0, 
           prop.chisq = F)

#######   Comparando Diferentes K:[1,30]

# Criando o Data Frame que será armazenado os resultados

Resultados <- data.frame( K = 21,
                          Acurácia = Acc0,
                          Falso_Benigno = F_Ben0,
                          Falso_Maligno = F_Mal0)

# Loop

Ki <- 1:30

# este primeiro loop é didático, o problema dele é que cria 4 objetos diferentes a cada loop. Não quero isso, mas serve pra entender o loop execultável abaixo:
#for (i in Ki) {
#  nome <- paste('k', i, 'nn', sep = '')
#  assign(nome, knn(train = train[1:30],
#                   test = test[1:30],
#                   cl = train$diagn,
#                   k = i))
#  rslt <- data.frame(Original = c(test$diagn),
#                      Predito = c(get(paste('k', i, 'nn', sep = ''))))
#  tab <- table(rslt$Original, rslt$Predito)
#  nome1 <- paste('Acc', i, sep = '')
#  nome2 <- paste('F_Ben', i, sep = '')
#  nome3 <- paste('F_Mal', i, sep = '')
#  assign(nome1, sum(diag(tab))/sum(tab))
#  assign(nome2, sum(tab[2,1])/sum(tab[,1]))
#  assign(nome3, sum(tab[1,2])/sum(tab[,2]))
#  nova_linha <- data.frame(i, get(nome1), get(nome2), get(nome3))
#  names(nova_linha) <- c('K', 'Acurácia', 'Falso_Benigno', 'Falso_Maligno')
#  Resultados <- rbind(Resultados, nova_linha)
#}

for (i in Ki) {
  assign('modelo_KNN', knn(train = train[1:30],
                   test = test[1:30],
                   cl = train$diagn,
                   k = i))
  rslt <- data.frame(Original = c(test$diagn),
                     Predito = c(modelo_KNN))
  tab <- table(rslt$Original, rslt$Predito)
  assign('Acc', sum(diag(tab))/sum(tab))
  assign('F_Ben', sum(tab[2,1])/sum(tab[,1]))
  assign('F_Mal', sum(tab[1,2])/sum(tab[,2]))
  nova_linha <- data.frame(i, Acc, F_Ben, F_Mal)
  names(nova_linha) <- c('K', 'Acurácia', 'Falso_Benigno', 'Falso_Maligno')
  Resultados <- rbind(Resultados, nova_linha)
}

Resultados %>% round(4) %>% View()
Resultados %>% slice_max(Resultados$Acurácia) %>% View()

#library(plotly, rgl)
#g1 <- plot_ly(x = ~dt1$dimension_mean,
#              y = ~dt1$concavity_mean,
#              z = ~dt1$texture_mean,
#              color = ~dt1$diagn,
#              size = 0.5)
#g1



