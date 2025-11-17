
 
# Analisis de resultados modelos ML

require(openxlsx)
require(dplyr)
require(ggplot2)

setwd('C:\\Users\\gonza\\Desktop\\Doctorado\\Papers\\Felix_bulnes')
list.files()

df <- read.xlsx('Resultados Felix Bulnes - Modelos ML.xlsx', 
                sheet = 'Resultados ajustados')

# Mejores modelos

# Decision tree
df %>% filter(Modelo == 'Decision Tree') %>%
  group_by(criterion, splitter, max_depth,
           min_samples_split, min_samples_leaf, max_features) %>%
  summarise(accuracy = mean(accuracy),
            precision = mean(precision),
            accuracy_adjusted = mean(accuracy_adjusted),
            precision_adjusted = mean(precision_adjusted),
            auc = mean(auc)) %>%
  arrange(desc(precision))

# Regresión logística
df %>% filter(Modelo == 'Logistic Regression') %>%
  group_by(penalty, C, class_weight) %>%
  summarise(accuracy = mean(accuracy),
            precision = mean(precision),
            accuracy_adjusted = mean(accuracy_adjusted),
            precision_adjusted = mean(precision_adjusted),
            auc = mean(auc)) %>%
  arrange(desc(precision))

# Random Forest
df %>% filter(Modelo == 'Random Forest') %>%
  group_by(criterion, max_depth, min_samples_split,
           min_samples_leaf, max_features, n_estimators) %>%
  summarise(accuracy = mean(accuracy),
            precision = mean(precision),
            accuracy_adjusted = mean(accuracy_adjusted),
            precision_adjusted = mean(precision_adjusted),
            auc = mean(auc)) %>%
  arrange(desc(precision))

# Extreme gradient boosting
df %>% filter(Modelo == 'XGBoost') %>%
  group_by(max_depth,n_estimators,learning_rate,subsample,colsample_bytree) %>%
  summarise(accuracy = mean(accuracy),
            precision = mean(precision),
            accuracy_adjusted = mean(accuracy_adjusted),
            precision_adjusted = mean(precision_adjusted),
            auc = mean(auc)) %>%
  arrange(desc(precision))

# Support vector machines
df %>% filter(Modelo == 'Support Vector Machines') %>%
  group_by(kernel,C) %>%
  summarise(accuracy = mean(accuracy),
            precision = mean(precision),
            accuracy_adjusted = mean(accuracy_adjusted),
            precision_adjusted = mean(precision_adjusted),
            auc = mean(auc)) %>%
  arrange(desc(precision))

# Mejores modelos considerando la precision

# ======== Decision tree ===========
# criterion: entropy
# splitter: best
# max_depth: 5,
# min_samples_split: 20
# min_samples_leaf: 1
# max_features: sqrt
# ======== Logistic regression ===========
# penalty: l1
# C: 1.01
# class_weight: None
# ======== Random Forest ===========
# criterion: gini
# max_depth: 10
# min_samples_split: 5
# min_samples_leaf: 10
# max_features: log2
# n_estimators: 10
# ======== Extreme gradient boosting ===========
# max_depth: 1
# n_estimators: 10
# learning_rate: 0.61
# subsample: 0.8
# colsample_bytree: 0.2
# ======== Support Vector Machines ===========
# kernel: linear
# C: 2.51

best_models <- rbind(df %>%
  filter(Modelo == 'Decision Tree',
         criterion == 'gini',
         splitter == 'random',
         max_depth == 5,
         min_samples_split == 20,
         min_samples_leaf == 5,
         max_features == 'sqrt') %>%
  select(Modelo, accuracy,auc,precision,precision_adjusted,accuracy_adjusted),
df %>%
  filter(Modelo == 'Logistic Regression',
         penalty == 'l1',
         C == 1.01,
         class_weight == 'None') %>%
  select(Modelo, accuracy,auc,precision,precision_adjusted,accuracy_adjusted),
df %>%
  filter(Modelo == 'Random Forest',
         criterion == 'log_loss',
         max_depth == 5,
         min_samples_split == 5,
         min_samples_leaf == 1,
         max_features == 'sqrt',
         n_estimators == 200) %>%
  select(Modelo, accuracy,auc,precision,precision_adjusted,accuracy_adjusted),
df %>%
  filter(Modelo == 'XGBoost',
         max_depth == 1,
         n_estimators == 10,
         learning_rate == 0.41,
         subsample == 0.2,
         colsample_bytree == 0.2) %>%
  select(Modelo, accuracy,auc,precision,precision_adjusted,accuracy_adjusted),
df %>%
  filter(Modelo == 'Support Vector Machines',
         kernel == 'linear',
         C == 62.51) %>%
  select(Modelo,accuracy,auc,precision,precision_adjusted,accuracy_adjusted))

# Precision
best_models %>%
  ggplot(aes(x = Modelo, y = precision)) +
  geom_boxplot(col = 'black', fill = 'gray') +
  theme_classic()

best_models %>%
  ggplot(aes(x = Modelo, y = precision_adjusted)) +
  geom_boxplot(col = 'black', fill = 'gray') +
  theme_classic()

best_models %>%
  ggplot(aes(x = Modelo, y = accuracy)) +
  geom_boxplot(col = 'black', fill = 'gray') +
  theme_classic()

best_models %>%
  ggplot(aes(x = Modelo, y = accuracy_adjusted)) +
  geom_boxplot(col = 'black', fill = 'gray') +
  theme_classic()

best_models %>%
  ggplot(aes(x = Modelo, y = auc)) +
  geom_boxplot(col = 'black', fill = 'gray') +
  theme_classic()

kruskal.test(precision ~ Modelo, data = best_models)
kruskal.test(accuracy ~ Modelo, data = best_models)
kruskal.test(auc ~ Modelo, data = best_models)

dunn.test::dunn.test(best_models$precision, best_models$Modelo, alpha = 0.12)
dunn.test::dunn.test(best_models$auc, best_models$Modelo, alpha = 0.1)

