# 🚀 SAE 5.03 - Projet de Datamining - Prédiction du Diabète

## 📌 Description du projet

Ce projet s'inscrit dans le cadre du cours BUT3 - SD - S5-03 Mise en œuvre d’un processus de Datamining. L'objectif est de comparer trois méthodes de classification supervisée pour prédire si un patient est diabétique en fonction de divers attributs de santé. Les trois méthodes utilisées sont :

- k-Nearest Neighbors (k-NN)  
- Analyse Discriminante Linéaire (LDA)  
- Arbres de Décision  

Le jeu de données utilisé provient de **Kaggle** et concerne la **prédiction du diabète** à partir de caractéristiques médicales des patients.

## 📊 Données utilisées

📍 **Source des données** : [Diabetes Prediction Dataset - Kaggle](https://www.kaggle.com/datasets/iammustafatz/diabetes-prediction-dataset)  

📌 **Variables principales** :  
- `age` : Âge du patient  
- `bmi` : Indice de Masse Corporelle (IMC)  
- `HbA1c level` : Niveau d’hémoglobine glyquée  
- `blood glucose level` : Niveau de glucose sanguin  
- `gender`, `hypertension`, `heart disease`, `smoking history`, etc.  

🛠 **Prétraitement réalisé** :  
✅ Nettoyage des données et gestion des valeurs manquantes  
✅ Encodage des variables catégorielles  
✅ Échantillonnage équilibré pour éviter le déséquilibre des classes  

## 🔬 Méthodologie

1️⃣ **Analyse avec uniquement les prédicteurs quantitatifs**  
2️⃣ **Extension à tous les prédicteurs (quantitatifs + qualitatifs)**  
3️⃣ **Comparaison des performances des modèles**  
4️⃣ **Analyse de l’importance des prédicteurs**  

Les modèles ont été évalués selon leur **taux d’erreurs**, **précision**, **sensibilité** et **spécificité**.

## 📈 Résultats et conclusions

📌 **Meilleur modèle** : L’**arbre de décision** a offert la meilleure performance avec un taux d’erreur plus faible et une meilleure identification des patients diabétiques.  

📌 **Prédicteurs les plus importants** :  
🔹 **HbA1c level** et **blood glucose level** sont les plus influents dans la classification.  
🔹 **L’âge et l’IMC** jouent aussi un rôle mais restent secondaires.  

## 🛠 Technologies utilisées

- **Langage** : R  
- **Librairies principales** : `tidyverse`, `class`, `MASS`, `rpart`, `ggplot2`  

## 📂 Structure du projet
```
📦SAE503
 ┣ 📂src
 ┃ ┣ 📜encoding.r
 ┃ ┣ 📜ml_models.r
 ┃ ┣ 📜plots.r
 ┃ ┗ 📜diabetes_prediction_dataset.csv
 ┣ 📜main.Rmd
 ┣ 📜Rapport_TROILLARD.pdf
 ┣ 📜Sujet.pdf
 ┗ 📜README.md
```

## 🚀 Cloner le dépôt

Pour cloner ce dépôt, utilisez la commande suivante :

```bash
git clone https://github.com/MrPaquiteau/SAE503.git
```

## Author
Romain TROILLARD | [MrPaquiteau](https://github.com/MrPaquiteau)
