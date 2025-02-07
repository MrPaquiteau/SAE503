preprocess_dummy <- function(data) {
    target <- data$diabetes
    features <- data[, !names(data) %in% "diabetes"]

    # Identifier les colonnes numériques et catégorielles
    numeric_features <- sapply(features, is.numeric)
    categorical_features <- !numeric_features

    # Commencer avec les colonnes numériques
    encoded_features <- features[, numeric_features, drop = FALSE]

    # Encodage one-hot pour les colonnes catégorielles
    for (col in names(features)[categorical_features]) {
        formula_str <- paste0("~", col, "-1")
        dummies <- model.matrix(as.formula(formula_str), data = features)

        if (ncol(dummies) > 1) {
            dummies <- dummies[, -ncol(dummies), drop = FALSE]
        }

        if (is.matrix(dummies)) {
            colnames(dummies) <- paste0(col, "_", colnames(dummies))
        } else {
            dummies <- as.matrix(dummies)
            colnames(dummies) <- paste0(col, "_", levels(as.factor(features[, col]))[1])
        }

        # Ajouter les colonnes dummy normalisées
        encoded_features <- cbind(encoded_features, dummies)
        encoded_features <- scale(encoded_features)
    }

    # Combiner avec la cible
    final_data <- data.frame(encoded_features, target)

    return(final_data)
}

only_numeric <- function(data) {
    X <- data %>% select(-diabetes)
    y <- data$diabetes
    numeric_data <- X[, sapply(X, is.numeric), drop = FALSE]

    # Exclure les variables binaires hypertension et heart_disease
    numeric_data <- numeric_data[, !names(numeric_data) %in% c("hypertension", "heart_disease"), drop = FALSE]

    # numeric_data <- scale(numeric_data)
    numeric_data <- as.data.frame(numeric_data)
    numeric_data$diabetes <- y
    return(numeric_data)
}

apply_acm <- function(data) {
    # Séparation des variables explicatives et de la cible
    X <- data %>% select(-diabetes)
    y <- data$diabetes

    # Appliquer l'ACM sur les colonnes catégorielles
    categorical_cols <- c("smoking_history", "gender")
    X_cat <- X[, categorical_cols]
    X_num <- X %>% select(-all_of(categorical_cols))

    # ACM
    mca <- MCA(X_cat, ncp = 5, graph = FALSE) # 5 composantes
    X_mca <- as.data.frame(mca$ind$coord) # Coordonnées des individus

    # Renommer les colonnes de l'ACM
    colnames(X_mca) <- paste0("component_", 0:4)

    # Combiner les variables numériques et les composantes de l'ACM
    X_combined <- cbind(X_num, X_mca)

    # Normalisation des données
    X_scaled <- scale(X_combined)

    # Ajouter la variable cible
    X_scaled <- as.data.frame(X_scaled)
    X_scaled$diabetes <- y
    return(X_scaled)
}
