resample_lda <- function(data, n_iterations = 500, seed = 4210) {
    nbcol <- ncol(data)
    error_rates <- numeric(n_iterations)
    precisions <- matrix(0, nrow = n_iterations, ncol = length(unique(data[, nbcol])))
    sensitivities <- matrix(0, nrow = n_iterations, ncol = length(unique(data[, nbcol])))
    specificities <- matrix(0, nrow = n_iterations, ncol = length(unique(data[, nbcol])))
    colnames(precisions) <- unique(data[, nbcol])
    colnames(sensitivities) <- unique(data[, nbcol])
    colnames(specificities) <- unique(data[, nbcol])
    selected_variables <- vector("list", n_iterations)

    # Nouvelle structure pour stocker l'importance des variables d'origine
    original_vars <- unique(gsub("_.*", "", colnames(data)[1:(nbcol - 1)]))
    feature_importances <- matrix(NA, nrow = n_iterations, ncol = length(original_vars))
    colnames(feature_importances) <- original_vars

    set.seed(seed)
    pb <- txtProgressBar(min = 0, max = n_iterations, style = 3)

    for (i in 1:n_iterations) {
        setTxtProgressBar(pb, i)

        ind_train <- sample(1:nrow(data), 0.7 * nrow(data))
        X_train <- data[ind_train, 1:nbcol - 1]
        y_train <- data[ind_train, nbcol]
        X_test <- data[-ind_train, 1:nbcol - 1]
        y_test <- data[-ind_train, nbcol]

        lda_model <- lda(x = X_train, grouping = y_train)
        predictions <- predict(lda_model, X_test)$class
        error_rate_original <- mean(predictions != y_test)

        # Calcul de la précision, sensibilité et spécificité pour chaque classe
        for (j in unique(y_test)) {
            TP <- sum(predictions == j & y_test == j) # Vrais positifs
            FP <- sum(predictions == j & y_test != j) # Faux positifs
            TN <- sum(predictions != j & y_test != j) # Vrais négatifs
            FN <- sum(predictions != j & y_test == j) # Faux négatifs

            precisions[i, j] <- TP / (TP + FP) # Précision
            sensitivities[i, j] <- TP / (TP + FN) # Sensibilité (rappel)
            specificities[i, j] <- TN / (TN + FP) # Spécificité
        }

        selected_variables[[i]] <- names(lda_model$scaling)

        # Identifier les colonnes associées à chaque variable d'origine
        for (var in original_vars) {
            var_cols <- grep(paste0("^", var, "_"), colnames(X_test))
            if (length(var_cols) > 0) {
                # Brouillage groupé pour les variables encodées en one-hot
                X_test_brouille <- X_test
                for (col in var_cols) {
                    X_test_brouille[, col] <- sample(X_test_brouille[, col], replace = TRUE)
                }
                predictions_brouille <- predict(lda_model, X_test_brouille)$class
                error_rate_brouille <- mean(predictions_brouille != y_test)
                feature_importances[i, var] <- error_rate_brouille - error_rate_original
            } else {
                # Brouillage individuel pour les variables non encodées
                X_test_brouille <- X_test
                X_test_brouille[, var] <- sample(X_test_brouille[, var], replace = TRUE)
                predictions_brouille <- predict(lda_model, X_test_brouille)$class
                error_rate_brouille <- mean(predictions_brouille != y_test)
                feature_importances[i, var] <- error_rate_brouille - error_rate_original
            }
        }

        error_rates[i] <- error_rate_original
    }

    close(pb)

    return(list(
        error_rates = error_rates,
        precisions = precisions,
        sensitivities = sensitivities,
        specificities = specificities,
        selected_variables = selected_variables,
        feature_importances = feature_importances
    ))
}


resample_knn <- function(data, k_values, n_iterations = 500, seed = 4210) {
    nbcol <- ncol(data)
    optimal_ks <- numeric(n_iterations)
    error_rates <- numeric(n_iterations)
    precisions <- matrix(0, nrow = n_iterations, ncol = length(unique(data[, nbcol])))
    sensitivities <- matrix(0, nrow = n_iterations, ncol = length(unique(data[, nbcol])))
    specificities <- matrix(0, nrow = n_iterations, ncol = length(unique(data[, nbcol])))
    colnames(precisions) <- unique(data[, nbcol])
    colnames(sensitivities) <- unique(data[, nbcol])
    colnames(specificities) <- unique(data[, nbcol])

    # Nouvelle structure pour stocker l'importance des variables d'origine
    original_vars <- unique(gsub("_.*", "", colnames(data)[1:(nbcol - 1)]))
    feature_importances <- matrix(NA, nrow = n_iterations, ncol = length(original_vars))
    colnames(feature_importances) <- original_vars

    set.seed(seed)
    pb <- txtProgressBar(min = 0, max = n_iterations, style = 3)

    for (i in 1:n_iterations) {
        setTxtProgressBar(pb, i)

        ind_train <- sample(1:nrow(data), 0.7 * nrow(data))
        X_train <- data[ind_train, 1:nbcol - 1]
        y_train <- data[ind_train, nbcol]
        X_test <- data[-ind_train, 1:nbcol - 1]
        y_test <- data[-ind_train, nbcol]

        errors <- sapply(k_values, function(k) {
            cv_predictions <- knn.cv(train = X_train, cl = y_train, k = k)
            mean(cv_predictions != y_train)
        })
        optimal_k <- k_values[which.min(errors)]
        optimal_ks[i] <- optimal_k
        predictions <- knn(train = X_train, test = X_test, cl = y_train, k = optimal_k)
        error_rate_original <- mean(predictions != y_test)

        # Calcul de la précision, sensibilité et spécificité pour chaque classe
        for (j in unique(y_test)) {
            TP <- sum(predictions == j & y_test == j) # Vrais positifs
            FP <- sum(predictions == j & y_test != j) # Faux positifs
            TN <- sum(predictions != j & y_test != j) # Vrais négatifs
            FN <- sum(predictions != j & y_test == j) # Faux négatifs

            precisions[i, j] <- TP / (TP + FP) # Précision
            sensitivities[i, j] <- TP / (TP + FN) # Sensibilité (rappel)
            specificities[i, j] <- TN / (TN + FP) # Spécificité
        }

        # Identifier les colonnes associées à chaque variable d'origine
        for (var in original_vars) {
            var_cols <- grep(paste0("^", var, "_"), colnames(X_test))
            if (length(var_cols) > 0) {
                # Brouillage groupé pour les variables encodées en one-hot
                X_test_brouille <- X_test
                for (col in var_cols) {
                    X_test_brouille[, col] <- sample(X_test_brouille[, col], replace = TRUE)
                }
                predictions_brouille <- knn(train = X_train, test = X_test_brouille, cl = y_train, k = optimal_k)
                error_rate_brouille <- mean(predictions_brouille != y_test)
                feature_importances[i, var] <- error_rate_brouille - error_rate_original
            } else {
                # Brouillage individuel pour les variables non encodées
                X_test_brouille <- X_test
                X_test_brouille[, var] <- sample(X_test_brouille[, var], replace = TRUE)
                predictions_brouille <- knn(train = X_train, test = X_test_brouille, cl = y_train, k = optimal_k)
                error_rate_brouille <- mean(predictions_brouille != y_test)
                feature_importances[i, var] <- error_rate_brouille - error_rate_original
            }
        }

        error_rates[i] <- error_rate_original
    }

    close(pb)

    return(list(
        optimal_ks = optimal_ks,
        error_rates = error_rates,
        precisions = precisions,
        sensitivities = sensitivities,
        specificities = specificities,
        feature_importances = feature_importances
    ))
}

resample_knn_parallel <- function(data, k_values, n_iterations = 500, seed = 4210) {
    library(parallel)

    nbcol <- ncol(data)
    optimal_ks <- numeric(n_iterations)
    error_rates <- numeric(n_iterations)
    precisions <- matrix(0, nrow = n_iterations, ncol = length(unique(data[, nbcol])))
    sensitivities <- matrix(0, nrow = n_iterations, ncol = length(unique(data[, nbcol])))
    specificities <- matrix(0, nrow = n_iterations, ncol = length(unique(data[, nbcol])))
    colnames(precisions) <- unique(data[, nbcol])
    colnames(sensitivities) <- unique(data[, nbcol])
    colnames(specificities) <- unique(data[, nbcol])

    # Nouvelle structure pour stocker l'importance des variables d'origine
    original_vars <- unique(gsub("_.*", "", colnames(data)[1:(nbcol - 1)]))
    feature_importances <- matrix(NA, nrow = n_iterations, ncol = length(original_vars))
    colnames(feature_importances) <- original_vars

    set.seed(seed)
    pb <- txtProgressBar(min = 0, max = n_iterations, style = 3)

    # Fonction interne pour une itération de KNN
    knn_iteration <- function(i) {
        set.seed(seed + i)
        setTxtProgressBar(pb, i)

        ind_train <- sample(1:nrow(data), 0.7 * nrow(data))
        X_train <- data[ind_train, 1:nbcol - 1]
        y_train <- data[ind_train, nbcol]
        X_test <- data[-ind_train, 1:nbcol - 1]
        y_test <- data[-ind_train, nbcol]

        errors <- sapply(k_values, function(k) {
            cv_predictions <- knn.cv(train = X_train, cl = y_train, k = k)
            mean(cv_predictions != y_train)
        })
        optimal_k <- k_values[which.min(errors)]
        predictions <- knn(train = X_train, test = X_test, cl = y_train, k = optimal_k)
        error_rate_original <- mean(predictions != y_test)

        # Calcul de la précision, sensibilité et spécificité pour chaque classe
        precisions_i <- sapply(unique(y_test), function(j) {
            TP <- sum(predictions == j & y_test == j)
            FP <- sum(predictions == j & y_test != j)
            TP / (TP + FP)
        })
        sensitivities_i <- sapply(unique(y_test), function(j) {
            TP <- sum(predictions == j & y_test == j)
            FN <- sum(predictions != j & y_test == j)
            TP / (TP + FN)
        })
        specificities_i <- sapply(unique(y_test), function(j) {
            TN <- sum(predictions != j & y_test != j)
            FP <- sum(predictions == j & y_test != j)
            TN / (TN + FP)
        })

        # Identifier les colonnes associées à chaque variable d'origine
        feature_importances_i <- numeric(length(original_vars))
        for (var_idx in seq_along(original_vars)) {
            var <- original_vars[var_idx]
            var_cols <- grep(paste0("^", var, "_"), colnames(X_test))
            if (length(var_cols) > 0) {
                # Brouillage groupé pour les variables encodées en one-hot
                X_test_brouille <- X_test
                for (col in var_cols) {
                    X_test_brouille[, col] <- sample(X_test_brouille[, col], replace = TRUE)
                }
                predictions_brouille <- knn(train = X_train, test = X_test_brouille, cl = y_train, k = optimal_k)
                error_rate_brouille <- mean(predictions_brouille != y_test)
                feature_importances_i[var_idx] <- error_rate_brouille - error_rate_original
            } else {
                # Brouillage individuel pour les variables non encodées
                X_test_brouille <- X_test
                X_test_brouille[, var] <- sample(X_test_brouille[, var], replace = TRUE)
                predictions_brouille <- knn(train = X_train, test = X_test_brouille, cl = y_train, k = optimal_k)
                error_rate_brouille <- mean(predictions_brouille != y_test)
                feature_importances_i[var_idx] <- error_rate_brouille - error_rate_original
            }
        }

        return(list(
            optimal_k = optimal_k,
            error_rate = error_rate_original,
            precisions = precisions_i,
            sensitivities = sensitivities_i,
            specificities = specificities_i,
            feature_importances = feature_importances_i
        ))
    }

    # Utilisation de mclapply pour paralléliser les itérations
    results <- mclapply(1:n_iterations, knn_iteration, mc.cores = detectCores() - 1)

    # Récupération des résultats
    for (i in 1:n_iterations) {
        optimal_ks[i] <- results[[i]]$optimal_k
        error_rates[i] <- results[[i]]$error_rate
        precisions[i, ] <- results[[i]]$precisions
        sensitivities[i, ] <- results[[i]]$sensitivities
        specificities[i, ] <- results[[i]]$specificities
        feature_importances[i, ] <- results[[i]]$feature_importances
    }

    close(pb)

    return(list(
        optimal_ks = optimal_ks,
        error_rates = error_rates,
        precisions = precisions,
        sensitivities = sensitivities,
        specificities = specificities,
        feature_importances = feature_importances
    ))
}

resample_tree <- function(data, n_iterations = 500, seed = 4210) {
    nbcol <- ncol(data)
    cp_values <- numeric(n_iterations)
    error_rates <- numeric(n_iterations)
    precisions <- matrix(0, nrow = n_iterations, ncol = length(unique(data[, nbcol])))
    sensitivities <- matrix(0, nrow = n_iterations, ncol = length(unique(data[, nbcol])))
    specificities <- matrix(0, nrow = n_iterations, ncol = length(unique(data[, nbcol])))
    colnames(precisions) <- unique(data[, nbcol])
    colnames(sensitivities) <- unique(data[, nbcol])
    colnames(specificities) <- unique(data[, nbcol])

    # Nouvelle structure pour stocker l'importance des variables d'origine
    original_vars <- unique(gsub("_.*", "", colnames(data)[1:(nbcol - 1)]))
    feature_importances <- matrix(NA, nrow = n_iterations, ncol = length(original_vars))
    colnames(feature_importances) <- original_vars

    set.seed(seed)
    pb <- txtProgressBar(min = 0, max = n_iterations, style = 3)

    for (i in 1:n_iterations) {
        setTxtProgressBar(pb, i)
        ind_train <- sample(1:nrow(data), 0.7 * nrow(data))
        X_train <- data[ind_train, 1:nbcol - 1]
        y_train <- data[ind_train, nbcol]
        X_test <- data[-ind_train, 1:nbcol - 1]
        y_test <- data[-ind_train, nbcol]

        train_data <- data.frame(X_train, y_train = y_train)
        tree_model <- rpart(as.formula(paste(names(data)[nbcol], "~.")),
            data = data[ind_train, ],
            method = "class",
            model = TRUE
        )
        predictions <- predict(tree_model, newdata = as.data.frame(X_test), type = "class")
        error_rate_original <- mean(predictions != y_test)

        # Calcul de la précision, sensibilité et spécificité pour chaque classe
        for (j in unique(y_test)) {
            TP <- sum(predictions == j & y_test == j) # Vrais positifs
            FP <- sum(predictions == j & y_test != j) # Faux positifs
            TN <- sum(predictions != j & y_test != j) # Vrais négatifs
            FN <- sum(predictions != j & y_test == j) # Faux négatifs

            precisions[i, j] <- TP / (TP + FP) # Précision
            sensitivities[i, j] <- TP / (TP + FN) # Sensibilité (rappel)
            specificities[i, j] <- TN / (TN + FP) # Spécificité
        }

        cp_values[i] <- tree_model$cptable[which.min(tree_model$cptable[, "xerror"]), "CP"]
        arbre_post <- prune(tree_model, cp = cp_values[i])

        # Identifier les colonnes associées à chaque variable d'origine
        for (var in original_vars) {
            var_cols <- grep(paste0("^", var, "_"), colnames(X_test))
            if (length(var_cols) > 0) {
                # Brouillage groupé pour les variables encodées en one-hot
                X_test_brouille <- X_test
                for (col in var_cols) {
                    X_test_brouille[, col] <- sample(X_test_brouille[, col], replace = TRUE)
                }
                predictions_brouille <- predict(arbre_post, newdata = as.data.frame(X_test_brouille), type = "class")
                error_rate_brouille <- mean(predictions_brouille != y_test)
                feature_importances[i, var] <- error_rate_brouille - error_rate_original
            } else {
                # Brouillage individuel pour les variables non encodées
                X_test_brouille <- X_test
                X_test_brouille[, var] <- sample(X_test_brouille[, var], replace = TRUE)
                predictions_brouille <- predict(arbre_post, newdata = as.data.frame(X_test_brouille), type = "class")
                error_rate_brouille <- mean(predictions_brouille != y_test)
                feature_importances[i, var] <- error_rate_brouille - error_rate_original
            }
        }

        error_rates[i] <- error_rate_original
    }

    close(pb)

    return(list(
        cp_values = cp_values,
        error_rates = error_rates,
        precisions = precisions,
        sensitivities = sensitivities,
        specificities = specificities,
        feature_importances = feature_importances,
        optimal_tree = arbre_post
    ))
}
