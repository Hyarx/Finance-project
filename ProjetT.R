  # Réinitialiser l'environnement
  rm(list = ls())
  gc()

  # Charger les bibliothèques nécessaires
  library(ggplot2)
  library(dplyr)
  library(corrplot)
  library(MASS)
  library(knitr)
  library(kableExtra)
  library(FactoMineR)
  library(factoextra)
  library(readxl)
  library(glmnet)
  library(mice)
  library(randomForest)
  library(ROCR)
  library(caret)
  # Définir le répertoire de travail
  #setwd("/Users/nathan/Documents/Master_2/ASM/Projects")
  setwd("C:/Users/alexa/Documents/Université/2024-2025/Q1/Advanced Statistical Methods in Finance/Projet/Projet")
  
  
  # Charger les fichiers
  Output <- read_excel("Output and NpLs.xlsx")
  NPL_D <- read_excel("NPL _Dynamics.xlsx")
  my_data <- read_excel("data (1).xlsx")  # Renommer pour éviter les conflits
  
  summary(NPL_D)

  mean <- mean(NPL_D$`Peak NPLs 3/`, na.rm = TRUE)
  
  barplot(NPL_D$`Peak NPLs 3/`, 
          names.arg = NPL_D$Country, 
          las = 2, 
          cex.names = 0.7,  
          col = "blue", 
          main = "Peak NPLs by Country")
  
  abline(h = mean, col = "red", lwd = 2, lty = 2)  
  
  
  variables_for_x <- c(
    "rgdpe", "rgdpo", "pop", "emp", "avh", "hc", "ccon", "cda", "cgdpe", 
    "cgdpo", "cn", "ck", "ctfp", "cwtfp", "rgdpna", "rconna", "rdana", 
    "rnna", "rkna", "rtfpna", "rwtfpna", "labsh", "irr", "delta", "xr", 
    "pl_con", "pl_da", "pl_gdpo", "csh_c", "csh_i", 
    "csh_g", "csh_x", "csh_m", "csh_r"
  )
  my_data_cleaned <- na.omit(my_data)
  summary(my_data_cleaned)

  barplot(NPL_D$`Peak NPLs 3/`, 
          names.arg = NPL_D$Country, 
          las = 2, 
          cex.names = 0.7,  
          col = "blue", 
          main = "Peak NPLs by Country")
  
  abline(h = mean, col = "red", lwd = 2, lty = 2)  
  
  
  # Filtrer les données pour inclure uniquement les variables explicatives
  data_filtered <- my_data_cleaned[, variables_for_x, drop = FALSE]
  x <- as.matrix(data_filtered)
  
  # Récupérer la variable cible séparément (par exemple, `NPL`)
  y <- my_data_cleaned$'Delta NPL'
  
  # Ajuster le modèle LASSO
  cv_lasso <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
  best_lambda <- cv_lasso$lambda.min
  plot(cv_lasso)
  
  
  # Extraire les coefficients pour le lambda optimal
  coefficients <- coef(cv_lasso, s = "lambda.min")
  print("Coefficients extraits :")
  print(coefficients)
  
  # Convertir les coefficients en data frame et filtrer ceux qui sont non nuls
  coefficients_df <- as.data.frame(as.matrix(coefficients))
  colnames(coefficients_df) <- c("Coefficient")
  coefficients_df <- coefficients_df %>% filter(Coefficient != 0)
  print("Coefficients non nuls :")
  print(coefficients_df)
  
  # Lister les variables sélectionnées
  selected_vars <- rownames(coefficients)[which(coefficients != 0)]
  cat("Variables sélectionnées par LASSO :", selected_vars, "\n")
  

  
  # Extraire les noms des variables sélectionnées à partir de coefficients_df
  selected_vars <- rownames(coefficients_df)  # Récupérer les noms des variables
  selected_vars <- selected_vars[selected_vars != "(Intercept)"]  # Exclure l'intercept
  
  # Construire la formule pour la régression linéaire
  formula <- as.formula(paste("NPL ~", paste(selected_vars, collapse = " + ")))
  
  # Ajuster le modèle de régression linéaire
  linear_model <- lm(formula, data = my_data_cleaned)
  
  # Résumé des résultats
  summary(linear_model)
  
  
  y <- my_data_cleaned$'Bin_delta'
  cv_lasso <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
  best_lambda <- cv_lasso$lambda.min
  plot(cv_lasso)
  help("cv.glmnet")
  
  # Extraire les coefficients pour le lambda optimal
  coefficients <- coef(cv_lasso, s = "lambda.min")
  print("Coefficients extraits :")
  print(coefficients)
  
  # Convertir les coefficients en data frame et filtrer ceux qui sont non nuls
  coefficients_df <- as.data.frame(as.matrix(coefficients))
  colnames(coefficients_df) <- c("Coefficient")
  coefficients_df <- coefficients_df %>% filter(Coefficient != 0)
  print("Coefficients non nuls :")
  print(coefficients_df)
  
  # Extraire les coefficients des prédicteurs sélectionnés
  coeff_matrix <- as.matrix(coefficients_df)
  
  # Identifiez les prédicteurs ayant des coefficients non nuls (exclure l'intercept)
  selected_predictors <- rownames(coeff_matrix)[coeff_matrix != 0 & rownames(coeff_matrix) != "(Intercept)"]
  predictor_data <- my_data_cleaned[, selected_predictors, drop = FALSE]
  
  # Calculer la matrice de corrélation pour les variables sélectionnées
  cor_matrix <- cor(predictor_data)
  
  # Afficher la matrice de corrélation
  print("Matrice de corrélation :")
  print(cor_matrix)
  
  
  # Filtrer les colonnes de X pour ne conserver que les prédicteurs sélectionnés
  X_selected <- x[, selected_predictors, drop = FALSE]
  print(cor(X_selected))
  print(table(y))
  X_selected_scaled <- scale(X_selected)
  
  # Vérifier les nouvelles variances (elles devraient être toutes égales à 1)
  print(apply(X_selected_scaled, 2, var))
  
  # --- Ajuster le modèle logit avec validation croisée ---
  set.seed(123)
  
  # Préparer les données pour logit
  data_logit <- data.frame(Y = factor(y, levels = c(0, 1), labels = c("Class0", "Class1")),
                           X_selected_scaled)
  
  # Définir la validation croisée
  train_control <- trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
  
  # Ajuster le modèle logit
  logit_cv_model <- train(
    Y ~ .,
    data = data_logit,
    method = "glm",
    family = binomial(link = "logit"),
    metric = "ROC",
    trControl = train_control
  )
  
  # Résultats du modèle logit
  print("Résumé de la validation croisée pour le modèle logit :")
  print(logit_cv_model)
  print(logit_cv_model$results)
  
  # Courbe ROC et AUC
  predicted_probabilities <- predict(logit_cv_model, data_logit, type = "prob")[, 2]
  pred_obj <- prediction(predicted_probabilities, data_logit$Y)
  roc_performance <- performance(pred_obj, measure = "tpr", x.measure = "fpr")
  plot(roc_performance, colorize = TRUE, main = "Courbe ROC - Logit (Validation croisée)")
  abline(a = 0, b = 1, lty = 2, col = "gray")
  
  auc_performance <- performance(pred_obj, measure = "auc")
  auc_value <- auc_performance@y.values[[1]]
  cat("AUC du modèle logit (Validation croisée) :", auc_value, "\n")
  
  # Confusion Matrix
  cutoff <- 0.5
  predicted_classes <- ifelse(predicted_probabilities >= cutoff, "Class1", "Class0")
  predicted_classes <- factor(predicted_classes, levels = c("Class0", "Class1"))
  conf_matrix <- confusionMatrix(data = predicted_classes, reference = data_logit$Y, positive = "Class1")
  print("Matrice de confusion :")
  print(conf_matrix)
  
  ####### Optimal Cutoff 
  cutoffs <- seq(0, 1, by = 0.01)  # Séquence de cutoffs entre 0 et 1
  accuracy_values <- numeric(length(cutoffs))  # Stockage des accuracy
  
  # Étape 2 : Calculer l'accuracy pour chaque cutoff
  for (i in seq_along(cutoffs)) {
    cutoff <- cutoffs[i]
    
    # Prédiction des classes avec le cutoff courant
    predicted_classes <- ifelse(predicted_probabilities >= cutoff, 1, 0)
    predicted_classes <- factor(predicted_classes, levels = c(0, 1))  # Convertir en facteur
    
    # Observed classes
    observed_classes <- factor(my_data_cleaned$Bin_delta, levels = c(0, 1))  # Convertir en facteur
    
    # Matrice de confusion
    conf_matrix <- confusionMatrix(data = predicted_classes, reference = observed_classes, positive = "1")
    
    # Calcul de l'accuracy
    accuracy_values[i] <- conf_matrix$overall["Accuracy"]
  }
  
  # Étape 3 : Identifier le cutoff optimal
  optimal_cutoff <- cutoffs[which.max(accuracy_values)]
  max_accuracy <- max(accuracy_values)
  
  print(paste("Cutoff optimal :", optimal_cutoff))
  print(paste("Accuracy maximale :", max_accuracy))
  
  # Étape 4 : Matrice de confusion avec le cutoff optimal
  predicted_classes_optimal <- ifelse(predicted_probabilities >= optimal_cutoff, 1, 0)
  predicted_classes_optimal <- factor(predicted_classes_optimal, levels = c(0, 1))
  
  conf_matrix_optimal <- confusionMatrix(data = predicted_classes_optimal, reference = observed_classes)
  print(conf_matrix_optimal)
  
  
  # Ajuster le modèle logit sur toutes les données
  logit_model <- glm(Y ~ ., family = binomial(link = "logit"), data = data_logit)
  summary(logit_model)
  log_likelihood <- logLik(logit_model)
  cat("Log-vraisemblance du modèle logit :", as.numeric(log_likelihood), "\n")
  
  
  # Fixer une graine pour la reproductibilité
  set.seed(123)
  
  # Créer un dataframe avec la variable cible et les prédicteurs sélectionnés
  data_rf <- data.frame(Y = y, x)
  
  # Vérifier que data_rf a bien été créé
  print(head(data_rf))
  
  # Modifier les niveaux de Y pour qu'ils soient des noms valides
  data_rf$Y <- factor(data_rf$Y, levels = c(0, 1), labels = c("Class0", "Class1"))
  table(data_rf$Y)
  
  # Vérifier que Y est bien un facteur
  print(class(data_rf$Y))
  print(table(data_rf$Y))
  
  # Définir la stratégie de validation croisée avec probabilités de classe
  train_control <- trainControl(
    method = "cv",                     # Cross-validation classique
    number = 10,                        # Nombre de folds
    verboseIter = TRUE,                # Afficher les étapes
    classProbs = TRUE,                 # Activer les probabilités de classe
    summaryFunction = twoClassSummary  # Utiliser l'AUC comme métrique
  )
  
  # Ajuster le modèle Random Forest avec validation croisée
  set.seed(123)
  rf_cv_model <- train(
    Y ~ ., 
    data = data_rf,
    method = "rf",
    metric = "ROC",
    trControl = train_control,
    tuneGrid = expand.grid(.mtry = c(2, 3)),
    ntree = 100,      # Réduire le nombre d'arbres
    maxnodes = 10     # Limiter la profondeur des arbres
  )
  
  
  # Résumé du modèle ajusté
  print(rf_cv_model)
  
  # Afficher les performances pour chaque valeur de mtry
  print(rf_cv_model$results)
  
  # Meilleure combinaison de paramètres
  print(rf_cv_model$bestTune)
  
  # Importance des variables
  feature_importances <- varImp(rf_cv_model, scale = FALSE)
  print("Importance des variables :")
  print(feature_importances)
  
  # Visualiser l'importance des variables
  feature_importances_df <- as.data.frame(feature_importances$importance)
  feature_importances_df$Feature <- rownames(feature_importances_df)
  
  ggplot(feature_importances_df, aes(x = reorder(Feature, Overall), y = Overall)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Importance des variables (Random Forest)",
      x = "Variables",
      y = "Importance"
    ) +
    theme_minimal()
  
  # Prédictions des probabilités sur l'ensemble d'entraînement
  predicted_rf_prob <- predict(rf_cv_model, data_rf, type = "prob")[, 2]
  
  # Courbe ROC et AUC
  pred_obj_rf <- ROCR::prediction(predicted_rf_prob, data_rf$Y)
  roc_rf <- performance(pred_obj_rf, measure = "tpr", x.measure = "fpr")
  
  # Tracer la courbe ROC
  plot(roc_rf, colorize = TRUE, main = "Courbe ROC - Random Forest (Validation croisée)")
  abline(a = 0, b = 1, lty = 2, col = "gray")
  
  # Calculer l'AUC
  auc_rf <- performance(pred_obj_rf, measure = "auc")
  cat("AUC de la Random Forest (Validation croisée) :", auc_rf@y.values[[1]], "\n")
  
  # Définir un seuil pour classifier les probabilités
  cutoff <- 0.5  # Ajustez le seuil si nécessaire
  
  # Convertir les probabilités en classes prédictes
  predicted_rf_classes <- ifelse(predicted_rf_prob >= cutoff, "Class1", "Class0")
  predicted_rf_classes <- factor(predicted_rf_classes, levels = c("Class0", "Class1"))
  
  # Créer la matrice de confusion
  conf_matrix_rf <- confusionMatrix(data = predicted_rf_classes, reference = data_rf$Y,positive = "Class1" )
  
  # Afficher la matrice de confusion
  print("Matrice de confusion pour le modèle Random Forest :")
  print(conf_matrix_rf)
  
  