### Modello  definitivo###


library(tidyverse)
library(keras)
library(readr)
library(caret)
library(zoo)
library(signal)
library(TTR)

dataset <- read_csv("data/LDC2025_training_demo.csv", col_names = FALSE)
test <- read_csv("data/LDC2025_test_input_demo.csv", col_names = FALSE)
classe_true <- read_csv("data/wifiminers_demo.csv")

#### trasformazione dati ###
features <- dataset %>% select(X1:X112)
fusione <- bind_rows(features, test)
fusione <- apply(fusione, 2, function(col) diff(col, differences = 1))
as.data.frame(fusione) %>% select(X2) %>% na.omit() %>% as.ts() %>% plot()

features <- as.matrix(fusione[1:nrow(features), ])
nrow(features)==nrow(dataset)

test_set<- as.matrix(fusione[(nrow(features)+1):nrow(fusione), ])
nrow(test_set)==nrow(test)

#### analisi grafica ####
{
  
  dataset <- read_csv("/Users/li/Desktop/Lab data challenge/LDC2025_training.csv", col_names = FALSE)
  test <- read_csv("Desktop/Lab data challenge/LDC2025_test_input.csv", col_names = FALSE)
  
  ### ANALISI ESPLORATIVA TUTTO IL TRAINING SET###
  dataset %>% select(X1, X2, X3, X4, X111, X112) %>% summary()
  
  features <- dataset %>% select(X1:X112)
  features.pari <- features[,rep(c(FALSE, TRUE), 56)]
  features.pari <- apply(features.pari, 2, function(x) rollmean(x, k = 100, fill = "extend", align = "center"))
  features <- features[,rep(c(TRUE, FALSE), 56)]
  features <- apply(features, 2, function(x) rollmean(x, k = 100, fill = "extend", align = "center"))
  
  #ANALISI GRAFICA SU TUTTO IL TRAINING SET###
  # Andamento delle singole variabili X1 e X2
  par(mfrow=c(3, 1), mar = rep(2, 4))
  features %>% as.data.frame() %>% pull(X1) %>% as.ts() %>% plot()
  features.pari %>% as.data.frame() %>% pull(X112) %>% as.ts() %>% plot()
  dataset %>% mutate(label = 1*X114 + 2*X115) %>% pull(label) %>% as.ts() %>% plot()
  par(mfrow=c(1,1))
  
  #oppure
  par(mfrow=c(2, 1), mar = rep(2, 4))
  dataset %>% select(X1) %>% mutate(X1.line = X1) %>% pull(X1.line) %>% as.ts() %>% plot(main="Segnale WiFi nel tempo (X1)", ylab="Signal Strength", xlab="Time")
  dataset %>% select(X1, X113) %>% mutate(X1.line = X1*X113) %>% select(X1.line) %>% as.ts() %>% lines(., col = "red")
  dataset %>% select(X1, X115) %>% mutate(X1.line = X1*X115) %>% select(X1.line) %>% as.ts() %>% lines(., col = "green")
  
  dataset %>% select(X2) %>% mutate(X2.line = X2) %>% pull(X2.line) %>% as.ts() %>% plot(main="Segnale WiFi nel tempo (X2)", ylab="Signal Strength", xlab="Time")
  dataset %>% select(X2, X113) %>% mutate(X2.line = X2*X113) %>% select(X2.line) %>% as.ts() %>% lines(., col = "red")
  dataset %>% select(X2, X115) %>% mutate(X2.line = X2*X115) %>% select(X2.line) %>% as.ts() %>% lines(., col = "green")
  legend("topleft", legend = c("vuoto", "fermo", "movimento"), fill = c("red", "black", "green"))
  par(mfrow=c(1, 1))
  
  ### ANALISI ESPLORATIVA SU UNA PARTE DEL TRAINING SET ###
  dataset <- dataset[250000:nrow(dataset), ]
  n <- nrow(dataset)
  dataset %>% select(X1, X2, X3, X4, X111, X112) %>% summary()
  
  #suddivido le variabili pari e dispari
  features <- dataset %>% select(X1:X112)
  features.pari <- features[,rep(c(FALSE, TRUE), 56)]
  features.pari <- apply(features.pari, 2, function(x) rollmean(x, k = 100, fill = "extend", align = "center"))
  features <- features[,rep(c(TRUE, FALSE), 56)]
  features <- apply(features, 2, function(x) rollmean(x, k = 100, fill = "extend", align = "center"))
  
  
  ### ANALISI GRAFICA SU UNA PARTE DEL TRAINING SET###
  # Andamento delle singole variabili X1 e X2
  par(mfrow=c(3, 1), mar = rep(2, 4))
  features %>% as.data.frame() %>% pull(X1) %>% as.ts() %>% plot()
  features.pari %>% as.data.frame() %>% pull(X112) %>% as.ts() %>% plot()
  dataset %>% mutate(label = 1*X114 + 2*X115) %>% pull(label) %>% as.ts() %>% plot()
  par(mfrow=c(1,1))
  
  #oppure
  par(mfrow=c(2, 1), mar = rep(2, 4))
  dataset %>% select(X1) %>% mutate(X1.line = X1) %>% pull(X1.line) %>% as.ts() %>% plot(main="Segnale WiFi nel tempo (X1)", ylab="Signal Strength", xlab="Time")
  dataset %>% select(X1, X113) %>% mutate(X1.line = X1*X113) %>% select(X1.line) %>% as.ts() %>% lines(., col = "red")
  dataset %>% select(X1, X115) %>% mutate(X1.line = X1*X115) %>% select(X1.line) %>% as.ts() %>% lines(., col = "green")
  
  dataset %>% select(X2) %>% mutate(X2.line = X2) %>% pull(X2.line) %>% as.ts() %>% plot(main="Segnale WiFi nel tempo (X2)", ylab="Signal Strength", xlab="Time")
  dataset %>% select(X2, X113) %>% mutate(X2.line = X2*X113) %>% select(X2.line) %>% as.ts() %>% lines(., col = "red")
  dataset %>% select(X2, X115) %>% mutate(X2.line = X2*X115) %>% select(X2.line) %>% as.ts() %>% lines(., col = "green")
  par(mfrow=c(1, 1))
  
  # Andamento della variabile X1 e X2 del test set
  par(mfrow=c(2, 1), mar = rep(2, 4))
  test %>% select(X1) %>% mutate(X1.line = X1) %>% pull(X1.line)%>% as.ts() %>% plot(main="Segnale WiFi nel tempo (X1) test set", ylab="Signal Strength", xlab="Time")
  
  test %>% select(X2) %>% mutate(X2.line = X2) %>% pull(X2.line) %>% as.ts() %>% plot(main="Segnale WiFi nel tempo (X2) test set", ylab="Signal Strength", xlab="Time")
  par(mfrow=c(1,1))
  
  # Andamento della variabile X1 training + X1 test set
  # il training parte dall'osservazione 250000
  
  fusione <- rbind(dataset[, 1], test[, 1])
  
  
  fusione %>% slice(1:nrow(fusione)) %>% select(X1) %>% mutate(X1.line = X1) %>% pull(X1.line) %>% as.ts() %>% plot(main="Segnale WiFi nel tempo (X1), training + test", ylab="Signal Strength", xlab="Time")
}

# TRAINING #
# Pre-elaborazione dei dati
feat <- na.omit(features)
p <- ncol(feat)
n <- nrow(feat)
target <- dataset %>% select(X113:X115)
feat <- as.matrix(feat)
target <- as.matrix(target)

# Verifica che il numero di righe di feat e target sia lo stesso
if (nrow(feat) != nrow(target)) {
  stop("Il numero di righe di feat e target non corrisponde!")
}

# Calcolo del numero di sequenze
SEQ_LEN <- 500
num_samples <- n %/% SEQ_LEN

# Tronco i dati a multiplo esatto di SEQ_LEN
usable_rows <- num_samples * SEQ_LEN
feat <- feat[1:usable_rows, ]
target <- target[1:usable_rows, ]

# Creazione delle sequenze X e Y
X <- array(dim = c(num_samples, SEQ_LEN, ncol(feat)))
Y <- array(dim = c(num_samples, SEQ_LEN, 3))

# Genera sequenze per X e Y
for (i in 1:num_samples) {
  start_idx <- (i - 1) * SEQ_LEN + 1
  end_idx <- i * SEQ_LEN
  X[i,,] <- feat[start_idx:end_idx, ]
  Y[i,,] <- target[start_idx:end_idx, ]
}

# Normalizzazione dei dati
X_train <- X
X_train_reshaped <- reticulate::array_reshape(X_train, dim = c(dim(X_train)[1]*dim(X_train)[2],dim(X_train)[3]))
train_means <- colMeans(X_train_reshaped)
train_sds <- apply(X_train_reshaped, 2, sd)

# Con questo approccio più diretto
X_train_scaled <- array(dim = dim(X_train))
for (i in 1:dim(X_train)[1]) {
  for (j in 1:dim(X_train)[2]) {
    X_train_scaled[i,j,] <- (X_train[i,j,] - train_means) / train_sds
  }
}
# Creazione degli indici per train e validation
train_idx <- 1:round((0.7 * num_samples))
valid_idx <- (round((0.7 * num_samples)) + 1):num_samples

X_train <- X_train_scaled[train_idx,,]
Y_train <- Y[train_idx,,]

X_valid <- X_train_scaled[valid_idx,,]
Y_valid <- Y[valid_idx,,]

## ADDESTRAMENTO MODELLO ##
set.seed(123)

model <- keras_model_sequential() %>%
  layer_lstm(units = 128, return_sequences = TRUE, recurrent_dropout = 0.3) %>%
  layer_dropout(rate = 0.3) %>%
  layer_lstm(units = 64, return_sequences = TRUE, recurrent_dropout = 0.3) %>%
  layer_dropout(rate = 0.3) %>%
  #layer_lstm(units = 64, return_sequences = TRUE, recurrent_dropout = 0.3) %>%
  layer_dense(units = 3, activation = "softmax")

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "rmsprop",
  metrics = c("accuracy")
)

history <- model %>% fit(
  X_train, Y_train,
  epochs = 10,  #50
  batch_size = 32,  #16
  validation_data = list(X_valid, Y_valid),
  shuffle = FALSE
)


### TEST SET ###

num_samples_test <- nrow(test_set) %/% SEQ_LEN
X.test <- array(dim = c(num_samples_test, SEQ_LEN, ncol(test_set)))

for (i in 1:dim(X.test)[1]) {
  X.test[i,,] <- test_set[SEQ_LEN * (i - 1) + (1:SEQ_LEN), ]
}

X_test_reshaped <- reticulate::array_reshape(X.test, dim = c(dim(X.test)[1]*dim(X.test)[2],dim(X.test)[3]))


X_test_scaled <- apply(X.test, 1, scale, center = train_means, scale = train_sds, simplify = F)
X_test_scaled <- simplify2array(X_test_scaled)
X_test_scaled <- reticulate::array_reshape(X_test_scaled, dim = c(dim(X_test_scaled)[3], dim(X_test_scaled)[1:2]))

# MODELLO FINALE #
# Usa tutti i dati disponibili per l'addestramento
X_full_train <- X_train_scaled  # Usa tutto il dataset scalato
Y_full_train <- Y  # Usa tutte le etichette


final_model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer,
  metrics = c("accuracy")
)

# Addestra il modello su tutti i dati
final_history <- model %>% fit(
  X_full_train, Y_full_train,
  epochs = 15,  # Aumenta il numero di epoche per il modello finale
  batch_size = 32,
  shuffle = FALSE
)

# FORECASTING #

predictions <- predict(final_model, X_test_scaled)

# CONVERSIONE #
predictions_flat <- matrix(0, nrow(test), ncol = 3)

for (i in 1:num_samples_test) {
  row_indices <- ((i - 1) * SEQ_LEN + 1):(i * SEQ_LEN)
  if (max(row_indices) > nrow(predictions_flat)) next
  predictions_flat[row_indices, ] <- predictions[i, , ]
}

#conversione probabilità in classi
predicted_classes <- max.col(predictions_flat)

#conversione delle classi in 3 variabili
results_probabilities <- as.data.frame(predictions_flat)
colnames(results_probabilities) <- c("X113", "X144", "X115")

results_classes <- data.frame(
  X113 = as.numeric(predicted_classes == 1),
  X114 = as.numeric(predicted_classes == 2),
  X115 = as.numeric(predicted_classes == 3)
)


table(predicted_classes)

#controllare se ho fatto tutto giusto
length(predicted_classes) == nrow(test)

#stesse dimensioni tra i primi due elementi
dim(X_train_scaled); dim(Y)

classe_true <- as.data.frame(classe_true)[, 1:3]
classe_true_vec <- max.col(classe_true)
classe_pred_vec <- max.col(results_classes)

# CONFUSION MATRIX #
confusionMatrix(as.factor(classe_pred_vec), as.factor(classe_true_vec))

# ESPORTAZIONE #
write_csv(results_classes, "test_gruppo_3.csv", col_names = T)
