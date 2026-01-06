# LIBRERIE ----

library(dplyr)
library(ranger)
library(zoo)
library(caret)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(grid)

# PREPARAZIONE DATASET E ANALISI FEATURE ----

# Invece di percorsi assoluti
dataset <- read_csv("data/LDC2025_training_demo.csv", col_names = FALSE)

# Trasformazione feature
features <- dataset %>% select(V1:V112)

features <- apply(features, 2, function(x) rollmean(x, k = 15000, fill = "extend", align = "center")) # media mobile per smussare i dati, a 15k termini corrispondente a circa il 3.62% dei dati

# Separazione target e feature
target <- dataset %>% select(V113:V115)
features <- as.matrix(features)
target <- as.matrix(target) %*% c(0,1,2) %>% as.factor() 
# target a 1 colonna (0 = stanza vuota, 1 = persona ferma, 2 = in movimento)

# breve analisi esplorativa e grafica
summary(features[, 1])
summary(features[, 2])
summary(features[, 3])
summary(features[, 4])
summary(features[, 111])
summary(features[, 112])

# grafici training set 
{
  
  ### Andamento della variabile V1 e V2 del training set originale ###
  par(mfrow=c(2, 1), mar = rep(2, 4))
  dataset %>% select(V1) %>% mutate(V1.line = V1) %>% pull(V1.line) %>% as.ts() %>% plot(main="Segnale WiFi nel tempo (V1)", ylab="Signal Strength", xlab="Time")
  dataset %>% select(V1, V113) %>% mutate(V1.line = V1*V113) %>% select(V1.line) %>% as.ts() %>% lines(., col = "red")
  dataset %>% select(V1, V115) %>% mutate(V1.line = V1*V115) %>% select(V1.line) %>% as.ts() %>% lines(., col = "green")
  
  dataset %>% select(V2) %>% mutate(V2.line = V2) %>% pull(V2.line) %>% as.ts() %>% plot(main="Segnale WiFi nel tempo (V2)", ylab="Signal Strength", xlab="Time")
  dataset %>% select(V2, V113) %>% mutate(V2.line = V2*V113) %>% select(V2.line) %>% as.ts() %>% lines(., col = "red")
  dataset %>% select(V2, V115) %>% mutate(V2.line = V2*V115) %>% select(V2.line) %>% as.ts() %>% lines(., col = "green")
  par(mfrow=c(1, 1))
  
  ### Confronto feature dispari training set trasformato con rollmean###
  
  # Creiamo dataframe in formato lungo per ggplot
  # Per V1
  df_v1 <- features %>% 
    as.data.frame() %>% 
    select(V1) %>% 
    mutate(time = row_number()) %>% 
    rename(value = V1) %>% 
    mutate(variable = "V1")
  
  # Per V3
  df_v3 <- features %>% 
    as.data.frame() %>% 
    select(V3) %>% 
    mutate(time = row_number()) %>% 
    rename(value = V3) %>% 
    mutate(variable = "V3")
  
  # Per target
  df_target <- dataset %>% 
    mutate(label = 1*V114 + 2*V115) %>% 
    select(label) %>% 
    rename(value = label) %>% 
    mutate(time = row_number()) %>% 
    mutate(variable = "target")
  
  # Creiamo i tre grafici
  p1 <- ggplot(df_v1, aes(x = time, y = value)) +
    geom_line(color = "dodgerblue", size = 1.2) +
    theme_minimal() +
    labs(title = "V1", x = "Tempo", y = "Segnali Wifi") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
  
  p2 <- ggplot(df_v3, aes(x = time, y = value)) +
    geom_line(color = "dodgerblue", size = 1.2) +
    theme_minimal() +
    labs(title = "V3", x = "Tempo", y = "Segnali Wifi") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
  
  p3 <- ggplot(df_target, aes(x = time, y = value)) +
    geom_line(color = "dodgerblue", size = 1.2) +
    theme_minimal() +
    labs(title = "target", x = "Tempo", y = "Segnali Wifi") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
  
  # Combiniamo i grafici verticalmente usando gridExtra
  train_combined_plot_dispari <- grid.arrange(p1, p2, p3, ncol = 1, 
                                              top = textGrob("training set: V1 - V3 - target", gp = gpar(fontsize = 14, font = 3)))
  
  
  ### Confronto feature pari training set trasformato con rollmean###
  
  # Creiamo dataframe in formato lungo per ggplot
  # Per V2
  df_v2 <- features %>% 
    as.data.frame() %>% 
    select(V2) %>% 
    mutate(time = row_number()) %>% 
    rename(value = V2) %>% 
    mutate(variable = "V2")
  
  # Per V4
  df_v4 <- features %>% 
    as.data.frame() %>% 
    select(V4) %>% 
    mutate(time = row_number()) %>% 
    rename(value = V4) %>% 
    mutate(variable = "V4")
  
  # Per target
  df_target <- dataset %>% 
    mutate(label = 1*V114 + 2*V115) %>% 
    select(label) %>% 
    rename(value = label) %>% 
    mutate(time = row_number()) %>% 
    mutate(variable = "target")
  
  # Creiamo i tre grafici
  p4 <- ggplot(df_v2, aes(x = time, y = value)) +
    geom_line(color = "dodgerblue", size = 1.2) +
    theme_minimal() +
    labs(title = "V2", x = "Tempo", y = "Segnali Wifi") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
  
  p5 <- ggplot(df_v4, aes(x = time, y = value)) +
    geom_line(color = "dodgerblue", size = 1.2) +
    theme_minimal() +
    labs(title = "V4", x = "Tempo", y = "Segnali Wifi") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
  
  p6 <- ggplot(df_target, aes(x = time, y = value)) +
    geom_line(color = "dodgerblue", size = 1.2) +
    theme_minimal() +
    labs(title = "target", x = "Tempo", y = "Segnali Wifi") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
  
  # Combiniamo i grafici verticalmente usando gridExtra
  train_combined_plot_pari <- grid.arrange(p4, p5, p6, ncol = 1, 
                                           top = textGrob("training set: V2 - V4 - target", gp = gpar(fontsize = 14, font = 3)))
  
}

# Data split campionando casualmente (70% training set, 30% validation set)
set.seed(123)
train_idx <- 1:dim(features)[1] %>% 
  sample(., size = round(dim(features)[1]) * .85)
valid_idx <- subset(1:dim(features)[1], !(1:dim(features)[1] %in% train_idx))
X_train <- features[train_idx,]
Y_train <- target[train_idx]
X_valid <- features[valid_idx,]
Y_valid <- target[valid_idx]


# RF con feature a dimensionalità ridotta (10 dim PCA) ----

# PCA
pca <- princomp(X_train, cor = T)
plot(pca, type = "l")
# considero le prime 10 componenti
X_scores <- pca$scores[, 1:10]

set.seed(123)

# RF training
fit <- ranger(x = X_scores, 
              y = Y_train,
              num.trees = 500,
              max.depth = 50, 
              mtry = 10,
              importance = "impurity",
              prob = F) 

fit$confusion.matrix

# prevedo le prime 10 componenti per comporre le features del validation set
P_scores <- predict(pca, X_valid)[, 1:10]

# previsione sul validation set
P_valid <- predict(fit, P_scores, type = "response")$predictions

# validation confusion matrix e accuracy
confusionMatrix(Y_valid, P_valid)$table
confusionMatrix(Y_valid, P_valid)$overall[1]

# importanza delle features
importance <- fit$variable.importance
barplot(importance, main = "Importanza delle componenti principali", col = "dodgerblue")


# Test set

# importazione dei dati
features.test <- read_csv("data/LDC2025_test_input_demo.csv", col_names = FALSE)

# data transformation
X_test <- apply(features.test, 2, function(x) rollmean(x, k = 4026, fill = "extend", align = "center"))

# breve analisi descrittiva e grafica

summary(features.test[, 1])
summary(features.test[, 2])
summary(features.test[, 3])
summary(features.test[, 4])
summary(features.test[, 111])
summary(features.test[, 112])

# grafici test set
{
  
  ### Andamento della variabile V1 e V2 del test set originale ###
  par(mfrow=c(2, 1), mar = rep(2, 4))
  features.test %>% select(V1) %>% mutate(V1.line = V1) %>% pull(V1.line)%>% as.ts() %>% plot(main="Segnale WiFi nel tempo (V1) test set", ylab="Signal Strength", xlab="Time")
  
  features.test %>% select(V2) %>% mutate(V2.line = V2) %>% pull(V2.line) %>% as.ts() %>% plot(main="Segnale WiFi nel tempo (V2) test set", ylab="Signal Strength", xlab="Time")
  par(mfrow=c(1,1))
  
  
  ###Confronto feature dispari test set trasformato con rollmean###
  
  # Creiamo dataframe in formato lungo per ggplot
  # Per V1
  test_v1 <- X_test %>% 
    as.data.frame() %>% 
    select(V1) %>% 
    mutate(time = row_number()) %>% 
    rename(value = V1) %>% 
    mutate(variable = "V1")
  
  # Per V3
  test_v3 <- X_test %>% 
    as.data.frame() %>% 
    select(V3) %>% 
    mutate(time = row_number()) %>% 
    rename(value = V3) %>% 
    mutate(variable = "V3")
  
  # Creiamo i due grafici
  p7 <- ggplot(test_v1, aes(x = time, y = value)) +
    geom_line(color = "dodgerblue", size = 1.2) +
    theme_minimal() +
    labs(title = "V1", x = "Tempo", y = "Segnali Wifi") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
  
  p8 <- ggplot(test_v3, aes(x = time, y = value)) +
    geom_line(color = "dodgerblue", size = 1.2) +
    theme_minimal() +
    labs(title = "V3", x = "Tempo", y = "Segnali Wifi") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
  
  # Combiniamo i grafici verticalmente usando gridExtra
  test_combined_plot_dispari <- grid.arrange(p7, p8, ncol = 1, 
                                             top = textGrob("test set: V1 - V3", gp = gpar(fontsize = 14, font = 3)))
  
  
  ### Confronto feature pari test set trasformato con rollmean ###
  
  # Creiamo dataframe in formato lungo per ggplot
  # Per V1
  test_v2 <- X_test %>% 
    as.data.frame() %>% 
    select(V2) %>% 
    mutate(time = row_number()) %>% 
    rename(value = V2) %>% 
    mutate(variable = "V2")
  
  # Per V4
  test_v4 <- X_test %>% 
    as.data.frame() %>% 
    select(V4) %>% 
    mutate(time = row_number()) %>% 
    rename(value = V4) %>% 
    mutate(variable = "V4")
  
  # Creiamo i due grafici
  p9 <- ggplot(test_v2, aes(x = time, y = value)) +
    geom_line(color = "dodgerblue", size = 1.2) +
    theme_minimal() +
    labs(title = "V2", x = "Tempo", y = "Segnali Wifi") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
  
  p10 <- ggplot(test_v4, aes(x = time, y = value)) +
    geom_line(color = "dodgerblue", size = 1.2) +
    theme_minimal() +
    labs(title = "V4", x = "Tempo", y = "Segnali Wifi") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
  
  # Combiniamo i grafici verticalmente usando gridExtra
  test_combined_plot_pari <- grid.arrange(p9, p10, ncol = 1, 
                                             top = textGrob("test set: V2 - V4", gp = gpar(fontsize = 14, font = 3)))
  
  
}

# Prevedo le prime 10 componenti principali
T_scores <- predict(pca, X_test)[, 1:10] 

# previsione sulla base delle prime 10 componenti del test set
P_test <- predict(fit, T_scores, type = "response")$predictions %>% as.matrix() 

# test set export
test_rf <- data.frame(P_test) %>% 
  mutate(
    V113 = ifelse(P_test == 0, 1, 0),
    V114 = ifelse(P_test == 1, 1, 0),
    V115 = ifelse(P_test == 2, 1, 0)
  ) %>% 
  select(-P_test)

write.csv(test_rf, "gruppo3_rf.csv", row.names = F)
