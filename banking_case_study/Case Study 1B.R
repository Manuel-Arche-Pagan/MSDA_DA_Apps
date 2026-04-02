# Case study 1.2

# initializing data + environment
packages <- c('car', 'tidyverse', 'data.table', 'caret', 'pROC', 'rpart', 'rpart.plot', 'randomForest', 'broom')
lapply(packages, library, character.only = TRUE)

df <- fread('bank-additional-full.csv') %>%
  as.tibble()

glimpse(df)
set.seed(1)

#####################################
# Data Pre-Processing
# character data types are converted to factors
df2 <- df %>%
  mutate(across(where(is.character), as.factor))
attach(df2)

# ensuring ordinal variables contain ordered levels
levels(education)
is.ordered(education)
# re-leveling ordinal variable 'education'
education <- factor(
  education,
  levels = c(
    'illiterate',
    'basic.4y',
    'basic.6y',
    'basic.9y',
    'high.school',
    'professional.course',
    'university.degree',
    'unknown'),
  ordered = TRUE
)
is.ordered(df$education)

# day of week
is.ordered(day_of_week)
levels(day_of_week)
day_of_week <- factor(
  day_of_week,
  levels = c(
    'mon',
    'tue',
    'wed',
    'thu',
    'fri'),
  ordered = TRUE
)
is.ordered(day_of_week)

# month
is.ordered(month)
levels(month)
month <- factor(
  month,
  levels = c(
    'mar',
    'apr',
    'may',
    'jun',
    'jul',
    'aug',
    'sep',
    'oct',
    'nov',
    'dec'),
  ordered = TRUE
  )
is.ordered(month)


# visual inspection of data
#par(mfrow = c(3,3))
barplot(table(y)) # dataset is very imbalanced
hist(pdays) # given that no contact is assigned a value of 999, this dataset appears sparse. therefor pdays should be omitted from train/test data
hist(age)
barplot(table(poutcome)) # most individuals were not contacted previously
barplot(table(education))
barplot(table(housing))
barplot(table(month))
barplot(table(loan))
barplot(table(day_of_week))
barplot(table(default))
#par(mfrow = c(1, 1))

# creating two new variables from poutcome: prev_contacted and prev_outcome
# if customer was contacted, prev_contacted = TRUE
df3 <- df2 %>%
  mutate(
    prev_contacted = ifelse(
    poutcome != 'nonexistent', TRUE, FALSE
  ),
  prev_outcome = factor(case_when(
    poutcome == 'nonexistent' ~ 'not_contacted',
    poutcome == 'success' ~ 'success',
    poutcome == 'failure' ~ 'failure'),
    levels = c('not_contacted', 'failure', 'success')
  )
  )

# REMOVE DURATION because it is a near-perfect predictor variable
# REMOVE PDAYS because it is very sparse
# REMOVE POUTCOME because the creation of prev_contacted and prev_outcome make it redundant (likely multicollinearity issues)

df4 <- df3 %>% dplyr::select(-duration, -pdays, -poutcome)

# train/test splits
df4_yes = dplyr::filter(df4, y == 'yes')
df4_no = dplyr::filter(df4, y == 'no')
df4_no_random = sample_n(df4_no, dim(df4_yes)[1])
df4_bal = rbind(df4_yes, df4_no_random)

# suffix "imb" = "imbalanced"; "bal" = "balanced"
train_index <- createDataPartition(df4$y, p = 0.7, list = FALSE)
train_index_bal <- createDataPartition(df4_bal$y, p = 0.7, list = FALSE)
train_imb <- df4[train_index, ]
test_imb <- df4[-train_index, ]
train_bal <- df4_bal[train_index_bal, ]
test_bal  <- df4_bal[-train_index_bal, ]


###### alias fixing attempt
# dummy variables "loan-unknown' and 'prev_outcome' were perfectly collinear with other variables; 
# filtering out loan "unknown" and omitting prev_outcome minimally impacts dataset

df5 <- df4 %>%
  filter(loan != 'unknown') %>%
  select(-prev_outcome)

# train/test splits
df5_yes = dplyr::filter(df5, y == 'yes')
df5_no = dplyr::filter(df5, y == 'no')
df5_no_random = sample_n(df5_no, dim(df5_yes)[1])
df5_bal = rbind(df5_yes, df5_no_random)

# suffix "imb" = "imbalanced"; "bal" = "balanced"
train_index5 <- createDataPartition(df5$y, p = 0.7, list = FALSE)
train_index_bal5 <- createDataPartition(df5_bal$y, p = 0.7, list = FALSE)
train_imb5 <- df5[train_index, ]
test_imb5 <- df5[-train_index, ]
train_bal5 <- df5_bal[train_index_bal, ]
test_bal5  <- df5_bal[-train_index_bal, ]

detach(df4)
attach(df5)











#####################################
# Logistic Regression

formula_lr <- y ~ .

lr_model_imb5 <- glm(formula_lr, data = train_imb5, family = binomial)
lr_model_bal5 <- glm(formula_lr, data = train_bal5, family = binomial)
summary(lr_model_imb5)
summary(lr_model_bal5)


# avoiding multicollinearity issues: purging variables with vif>5 (one at a time)
vif(lr_model_bal5)
formula_lr2 <- y~. -emp.var.rate
lr_model_bal6 <- glm(formula_lr2, data = train_bal5, family = binomial)

vif(lr_model_bal6)


formula_lr3 <- y~. -emp.var.rate -euribor3m
lr_model_bal7 <- glm(formula_lr3, data = train_bal5, family = binomial)

vif(lr_model_bal7)
# all variables in balanced model have vif<5

# purgin variables with vif>5 in imbalanced dataset
vif(lr_model_imb5)
lr_model_imb6 <- glm(formula_lr2, data = train_imb5, family = binomial)
vif(lr_model_imb6)
lr_model_imb7 <- glm(formula_lr3, data = train_imb5, family = binomial)
vif(lr_model_imb7)
#all variables in imbalanced model have vif<5
# side note: the same variables were eliminated in the same order from both the balanced and imbalanced models


# creates new columns 'prob_lr' and 'pred_lr' in R object 'test'
# 'prob_lr' is the probability of 'yes'; 'pred_lr' is the prediction for y
# prob_lr threshold is 0.5 for now
test_bal5$prob_lr <- predict(lr_model_bal5, newdata = test_bal5, type = "response")
test_bal5$pred_lr <- ifelse(test_bal5$prob_lr > 0.5, "yes","no") %>% factor(levels=c("no","yes"))

test_imb5$prob_lr <- predict(lr_model_imb5, newdata = test_imb5, type = 'response')
test_imb5$pred_lr <- ifelse(test_imb5$prob_lr > 0.5, 'yes', 'no') %>% factor(levels = c('no', 'yes'))

# ROC curve, AUC calculation, confusion matrix
roc_lr_bal <- roc(test_bal5$y, test_bal5$prob_lr)
auc_lr_bal <- roc_lr_bal$auc
print(confusionMatrix(test_bal5$pred_lr, test_bal5$y, positive = "yes"))

roc_lr_imb <- roc(test_imb5$y, test_imb5$prob_lr)
auc_lr_imb <- roc_lr_imb$auc
print(confusionMatrix(test_imb5$pred_lr, test_imb5$y, positive = 'yes'))

print(paste("Logistic Regression AUC (balanced):", auc_lr_bal))
print(paste("Logistic Regression AUC (imbalanced): ", auc_lr_imb))










#####################################
# Decision Tree

tree_ml <- formula_lr
tree_model_bal <- rpart(tree_ml, data = train_bal5, method = 'class')
tree_model_imb <- rpart(tree_ml, data = train_imb5, method = 'class')

rpart.plot(tree_model_bal, main = 'Decision Tree')
rpart.plot(tree_model_imb, main = 'Decision Tree')

test_bal5$prob_tree <- predict(tree_model_bal, newdata = test_bal5)[,"yes"]
test_bal5$pred_tree <- ifelse(test_bal5$prob_tree > 0.5,"yes","no") %>% factor(levels=c("no","yes"))
test_imb5$prob_tree <- predict(tree_model_imb, newdata = test_imb5)[,'yes']
test_imb5$pred_tree <- ifelse(test_imb5$prob_tree > 0.5,'yes','no') %>% factor(levels = c('no', 'yes'))

roc_tree_bal <- roc(test_bal5$y, test_bal5$prob_lr)
auc_tree_bal <- roc_tree_bal$auc
print(confusionMatrix(test_bal5$pred_tree, test_bal5$y, positive = 'yes'))

roc_tree_imb <- roc(test_imb5$y, test_imb5$prob_lr)
auc_tree_imb <- roc_tree_imb$auc
print(confusionMatrix(test_imb5$pred_tree, test_imb5$y, positive = 'yes'))

print(paste("Decision Tree AUC (balanced): ", auc_tree_bal))
print(paste("Decision Tree AUC (imbalanced): ", auc_tree_imb))





####################################
# Random Forest

# because model.matrix() will drop any rows with any number of 'NA' values, randomForest() will throw an error if the lengths of arguments x and y are not identical. The two lines below are for use in the randomForest() call to alleviate this issue
mf_bal <- model.frame(formula_lr3, data = train_bal5, na.action = na.omit)
x_bal <- model.matrix(formula_lr3, data = mf_bal)[, -1, drop = FALSE]
y <- mf_bal$y

rf_model_bal <- randomForest(x = x_bal,
                         y = y,
                         ntree = 150,
                         mtry = floor(sqrt(length(all.vars(formula_lr3)))),
                         nodesize = 50,
                         importance = TRUE)
varImpPlot(rf_model_bal, main = 'Random Forest Importance (balanced)')

mf_imb <- model.frame(formula_lr3, data = train_imb5, na.action = na.omit)
x_imb <- model.matrix(formula_lr3, data = mf_imb)[, -1, drop = FALSE]
y <- mf_imb$y

rf_model_imb <- randomForest(x = x_imb,
                             y = y,
                             ntree = 150,
                             nodesize = 50,
                             importance = TRUE)
varImpPlot(rf_model_imb, main = 'Random Forest Importance (imbalanced)')



# creating test prob and test predict
mf_test_bal <- model.frame(formula_lr3, data = test_bal5, na.action = na.omit)
test_mm_bal <- model.matrix(formula_lr3, data = mf_test_bal)[,-1, drop = FALSE]

mf_test_imb <- model.frame(formula_lr3, data = test_imb5, na.action = na.omit)
test_mm_imb <- model.matrix(formula_lr3, data = mf_test_imb)[,-1, drop = FALSE]

# this line below doesn't work because test_bal contains 12 rows with at least one na value. instead of assigning prob_rf to test_bal, I'll assign it to mf_test, which is the same as test_bal but without na values
#test_bal$prob_rf <- predict(rf_model, newdata = test_mm, type = 'prob')[,'yes']

mf_test_bal$prob_rf <- predict(rf_model_bal, newdata = test_mm_bal, type = "prob")[, "yes"]
mf_test_bal$pred_rf <- ifelse(mf_test_bal$prob_rf > 0.5, 'yes', 'no') %>%
  factor(levels = c('yes', 'no'))

mf_test_imb$prob_rf <- predict(rf_model_imb, newdata = test_mm_imb, type = 'prob')[, 'yes']
mf_test_imb$pred_rf <- ifelse(mf_test_imb$prob_rf > 0.5, 'yes', 'no') %>%
  factor(levels = c('yes', 'no'))

# ROC and AUC metrics

roc_rf_bal <- roc(mf_test_bal$y, mf_test_bal$prob_rf)
auc_rf_bal <- roc_rf_bal$auc
roc_rf_imb <- roc(mf_test_imb$y, mf_test_imb$prob_rf)
auc_rf_imb <- roc_rf_imb$auc

print(confusionMatrix(mf_test_bal$pred_rf, mf_test_bal$y, positive = 'yes'))
print(confusionMatrix(mf_test_imb$pred_rf, mf_test_imb$y, positive = 'yes'))

print(paste("Random Forest AUC (balanced): ", auc_rf_bal))
print(paste("Random Forest AUC (imbalanced): ", auc_rf_imb))



############################
# LDA

# loaded this library later in the file to avoid issues with tidyverse package.

library("MASS")

# lda.model_bal <- lda(y ~ ., data=train_bal5)
# lda.model_imb <- lda(y ~., data = train_imb5)
# above code does not work because one or more variables have zero within-class variance, which lda is unable to process
# need to identify which variables are causing the issue

# lda.model_bal <- lda(y ~ age + job + marital + education + loan, data = train_bal5)
lda.model_bal <- lda(y ~ . -housing - loan, data = train_bal5)

# housing, loan are the problem variables. 
lda.model_imb <- lda(y ~ . - housing - loan, data = train_imb5)

# predictions
lda_bal_preds <- predict(lda.model_bal, test_bal5)
lda_imb_preds <- predict(lda.model_imb, test_imb5)




# confusion matrices
caret::confusionMatrix(as.factor(lda_bal_preds$class), as.factor(test_bal5$y))
caret::confusionMatrix(as.factor(lda_imb_preds$class), as.factor(test_imb5$y))

test_bal5 <- test_bal5 %>%
  mutate(lda_bal_preds = lda_bal_preds$class)

test_imb5 <- test_imb5 %>%
  mutate(lda_imb_preds = lda_imb_preds$class)

# LDA produces a list of coordinates from the space it creates to 
# classify the response variable
# This space is divided by the linear line that splits it into two spaces
# for classification.

# This code is to add the probabilities of the "yes" classification as a column
# in our df in order to use it for the ROI function.

test_bal5$probs_lda <- lda_bal_preds$posterior[, 2]
test_imb5$probs_lda <- lda_imb_preds$posterior[, 2]

###################
# ROI Simulation

cost_per_contact <- 5
revenue_per_conversion <- 250

# Fix this up to match our purposes:

simulate_roi <- function(df_probs, prob_col = "prob"){
  n_total <- nrow(df_probs)
  
  # Mass marketing
  mass_contacts <- n_total
  mass_conversions <- sum(as.numeric(as.character(df_probs$y_num)))
  mass_cost <- mass_contacts * cost_per_contact
  mass_revenue <- mass_conversions * revenue_per_conversion
  mass_roi <- (mass_revenue - mass_cost) / mass_cost
  
  # Top 20%
  top20_n <- ceiling(0.20 * n_total)
  top20 <- df_probs %>% arrange(desc(.data[[prob_col]])) %>% slice(1:top20_n)
  top20_conversions <- sum(as.numeric(as.character(top20$y_num)))
  top20_cost <- top20_n * cost_per_contact
  top20_revenue <- top20_conversions * revenue_per_conversion
  top20_roi <- (top20_revenue - top20_cost) / top20_cost
  
  # Top 10%
  top10_n <- ceiling(0.10 * n_total)
  top10 <- df_probs %>% arrange(desc(.data[[prob_col]])) %>% slice(1:top10_n)
  top10_conversions <- sum(as.numeric(as.character(top10$y_num)))
  top10_cost <- top10_n * cost_per_contact
  top10_revenue <- top10_conversions * revenue_per_conversion
  top10_roi <- (top10_revenue - top10_cost) / top10_cost
  
  tibble(
    scenario = c("Mass (all)", "Target top 20%", "Target top 10%"),
    contacts = c(mass_contacts, top20_n, top10_n),
    conversions = c(mass_conversions, top20_conversions, top10_conversions),
    cost = c(mass_cost, top20_cost, top10_cost),
    revenue = c(mass_revenue, top20_revenue, top10_revenue),
    ROI = c(as.numeric(mass_roi), as.numeric(top20_roi), as.numeric(top10_roi))
  )
}



# actual simulations for the three models

# This portion is to convert the response variable into a 
# character so that it can be properly converted into an integer for the ROI 
# function i.e. "yes" -> "1" and "no" -> "0".  
# ROI function will turn "1" and "0" into their respective numerical values


test_imb6 <- test_imb5 |> 
  mutate(
    y_num = factor(case_when(y == 'yes' ~  '1',
                      y == 'no' ~ '0')
  ))

test_bal6 <- test_bal5 |> 
  mutate(
    y_num = factor(case_when(y == 'yes' ~  '1',
                             y == 'no' ~ '0')
    ))

mf_test_imb_2 <- mf_test_imb |> 
  mutate(
    y_num = factor(case_when(y == 'yes' ~  '1',
                             y == 'no' ~ '0')
    ))

mf_test_bal_2 <- mf_test_bal |> 
  mutate(
    y_num = factor(case_when(y == 'yes' ~  '1',
                             y == 'no' ~ '0')
    ))

#################################
# ROI Function Calls and Output

roi_lr <- simulate_roi(test_imb6 %>% rename(prob = prob_lr))
roi_lr_bal <- simulate_roi(test_bal6 %>% rename(prob = prob_lr))

roi_lda <- simulate_roi(test_imb6 %>% rename(prob = probs_lda))
roi_lda_bal <- simulate_roi(test_bal6 %>% rename(prob = probs_lda))

roi_tree <- simulate_roi(test_imb6 %>% rename(prob = prob_tree))
roi_tree_bal <- simulate_roi(test_bal6 %>% rename(prob = prob_tree))

roi_rf <- simulate_roi(mf_test_imb_2 %>% rename(prob = prob_rf))
roi_rf_bal <- simulate_roi(mf_test_bal_2 %>% rename(prob = prob_rf))



print(list(Logistic = roi_lr, Balanced_Logistic = roi_lr_bal,
           LDA = roi_lda, Balanced_LDA = roi_lda_bal,
           Tree = roi_tree, Balanced_Tree = roi_tree_bal,
           RF = roi_rf, Balanced_RF = roi_rf_bal))
