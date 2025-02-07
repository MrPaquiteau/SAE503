plot_results <- function(result, method) {
    # Page 1: Taux d'erreur
    par(mfrow = c(1, 1))
    boxplot(result$error_rates,
        main = paste("Taux d'Erreurs (", method, ")"),
        ylab = "Taux d'erreurs"
    )

    # Page 2: Précisions
    par(mfrow = c(1, 2))
    for (class in colnames(result$precisions)) {
        boxplot(result$precisions[, class],
            main = paste("Précisions pour la classe", class, "(", method, ")"),
            ylab = "Précision"
        )
    }

    # Page 3: Importance des prédicteurs
    par(mfrow = c(1, 1))
    boxplot(result$feature_importances,
        main = paste("Importance des prédicteurs (", method, ")"),
        ylab = "Importance",
        las = 2
    )

    # Page 4: Sensibilité et spécificité
    par(mfrow = c(1, 2))
    for (class in colnames(result$sensitivities)) {
        combined_data <- data.frame(
            Sensibilité = result$sensitivities[, class],
            Spécificité = result$specificities[, class]
        )
        boxplot(combined_data,
            main = paste(class, "(", method, ")"),
            ylab = "Valeur",
            names = c("Sensibilité", "Spécificité")
        )
    }

    # Page 5: Paramètres spécifiques à la méthode
    if (method == "knn") {
        par(mfrow = c(1, 1))
        k_freq <- table(result$optimal_ks)
        barplot(k_freq,
            main = "Fréquence des valeurs de k optimal",
            xlab = "Valeur de k",
            ylab = "Fréquence"
        )
    } else if (method == "tree") {
        # Page 5: Cp optimal
        par(mfrow = c(1, 1))
        cp_freq <- table(result$cp_values)
        barplot(cp_freq,
            main = "Fréquence des valeurs de cp optimal",
            xlab = "Valeur de cp",
            ylab = "Fréquence"
        )

        # Page 6: Arbre post-élagage
        par(mfrow = c(1, 1))
        library(rpart.plot)
        rpart.plot(result$optimal_tree, main = "Arbre Optimal Post-Élagage")
    }
}

compare_method <- function(lda, knn, tree, class_labels) {
    combined_results <- rbind(
        data.frame(
            Method = "LDA",
            ErrorRate = lda$error_rates,
            Precision = rowMeans(lda$precisions)
        ),
        data.frame(
            Method = "KNN",
            ErrorRate = knn$error_rates,
            Precision = rowMeans(knn$precisions)
        ),
        data.frame(
            Method = "Tree",
            ErrorRate = tree$error_rates,
            Precision = rowMeans(tree$precisions)
        )
    )

    precisions <- rbind(
        data.frame(
            Method = "LDA", Class = rep(class_labels, each = nrow(lda$precisions)),
            Precision = as.vector(lda$precisions)
        ),
        data.frame(
            Method = "KNN", Class = rep(class_labels, each = nrow(knn$precisions)),
            Precision = as.vector(knn$precisions)
        ),
        data.frame(
            Method = "Tree", Class = rep(class_labels, each = nrow(tree$precisions)),
            Precision = as.vector(tree$precisions)
        )
    )

    error_rate_plot <- ggplot(combined_results, aes(x = Method, y = ErrorRate, fill = Method)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "Comparaison des taux d'erreurs", y = "Taux d'erreur")

    precision_by_class_plot <- ggplot(precisions, aes(x = Class, y = Precision, fill = Method)) +
        geom_boxplot(position = position_dodge(width = 0.8)) +
        theme_minimal() +
        labs(
            title = "Comparaison des précisions par classe et méthode",
            x = "Classe",
            y = "Précision",
            fill = "Méthode"
        )

    precision_by_method_plot <- ggplot(precisions, aes(x = Method, y = Precision, fill = Class)) +
        geom_boxplot(position = position_dodge(width = 0.8)) +
        theme_minimal() +
        labs(
            title = "Comparaison des précisions par méthode et classe",
            x = "Méthode",
            y = "Précision",
            fill = "Classe"
        )

    print(error_rate_plot)
    print(precision_by_class_plot)
    print(precision_by_method_plot)
}
