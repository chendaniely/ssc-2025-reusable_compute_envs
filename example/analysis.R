# Load required packages
library(tidyverse)
library(palmerpenguins)
library(ggplot2)
library(GGally)
library(tidymodels)
library(broom)

# Load and clean data
penguins <- penguins %>%
  drop_na()

# Dimensions
dim(penguins)

# Pairplot
penguins %>%
  ggpairs(columns = c("bill_depth_mm", "body_mass_g", "flipper_length_mm"),
          ggplot2::aes(color = species))

# Recode target variable
penguins <- penguins %>%
  mutate(is_adelie = if_else(species == "Adelie", 1, 0))

# Logistic regression using tidymodels
logistic_recipe <- recipe(is_adelie ~ bill_depth_mm + body_mass_g + flipper_length_mm, data = penguins)

logistic_model <- logistic_reg(mode = "classification") %>%
  set_engine("glm")

logistic_workflow <- workflow() %>%
  add_model(logistic_model) %>%
  add_recipe(logistic_recipe)

logistic_fit <- logistic_workflow %>%
  fit(data = penguins)

# Coefficient summary
tidy(logistic_fit)

# Add predicted probabilities
penguins <- penguins %>%
  bind_cols(
    predict(logistic_fit, type = "prob") %>%
      rename(pred_prob = .pred_1)
  )

penguins %>%
  select(species, pred_prob) %>%
  head()

# Boxplot of predicted probabilities by species
ggplot(penguins, aes(x = species, y = pred_prob)) +
  geom_boxplot(fill = "#56B4E9") +
  labs(title = "Predicted Probability of Adelie by Species",
       y = "Predicted Probability", x = "Species") +
  theme_minimal()

# ROC curve
roc_data <- roc_curve(penguins, truth = factor(is_adelie), .pred_1) %>%
  as_tibble()

ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal()

# Coefficient plot with 95% CI
coefs <- tidy(logistic_fit, conf.int = TRUE) %>%
  filter(term != "(Intercept)")

ggplot(coefs, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Model Coefficients (95% CI)", y = "Estimate", x = "Term") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Predicted probability density by class
ggplot(penguins, aes(x = pred_prob, fill = factor(is_adelie))) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), name = "Is Adelie") +
  labs(title = "Predicted Probability Distribution", x = "Predicted Probability") +
  theme_minimal()
