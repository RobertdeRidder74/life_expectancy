################################################################################
# Load library
################################################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org")
if(!require(naniar)) install.packages("naniar", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(missForestPredict)) install.packages("missForestPredict", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(ggplot2)
library(ggthemes)
library(knitr)
library(kableExtra)
library(lubridate)
library(stringr)
library(readxl)
library(randomForest)
library(zoo)
library(naniar)
library(corrplot)
library(missForestPredict)
library(broom)
library(knitr)

################################################################################
# Load data
################################################################################

# Note: this process could take a couple of minutes

options(timeout = 500)

# -----------------------
# Our World in Data URLs
# -----------------------

life_url <-
  "https://ourworldindata.org/grapher/life-expectancy-at-different-ages.csv?v=1&csvType=full&useColumnShortNames=true"

gdp_url <-
  "https://ourworldindata.org/grapher/gdp-per-capita-penn-world-table.csv?v=1&csvType=full&useColumnShortNames=true"

healthcare_url <-
  "https://ourworldindata.org/grapher/public-health-expenditure-share-gdp.csv?v=1&csvType=full&useColumnShortNames=true"

schooling_url <-
  "https://ourworldindata.org/grapher/years-of-schooling.csv?v=1&csvType=full&useColumnShortNames=true&metric_type=average_years_schooling&level=all&sex=both"

vaccination_url <-
  "https://ourworldindata.org/grapher/vaccination-coverage-who-unicef.csv?v=1&csvType=full&useColumnShortNames=true&metric=coverage&antigen=comparison"

education_spending_url <-
  "https://ourworldindata.org/grapher/education-spending.csv?v=1&csvType=full&useColumnShortNames=true&spending_type=gdp_share&level=all"

obesity_url <-
  "https://ourworldindata.org/grapher/share-of-adults-defined-as-obese.csv?v=1&csvType=full&useColumnShortNames=true"

fertility_url <-
  "https://ourworldindata.org/grapher/children-born-per-woman.csv?v=1&csvType=full&useColumnShortNames=true"

urbanization_url <-
  "https://ourworldindata.org/grapher/share-of-people-living-in-urban-and-rural-areas-degurba.csv?v=1&csvType=full&useColumnShortNames=true"

renewables_url <-
  "https://ourworldindata.org/grapher/energy-mix.csv?v=1&csvType=full&useColumnShortNames=true&source=renewables&metric=share"

democracy_url <-
  "https://ourworldindata.org/explorers/democracy.csv?v=1&csvType=full&useColumnShortNames=true&Dataset=Varieties+of+Democracy&Metric=Electoral+democracy&Sub-metric=Main+index"


# -----------------------
# Import datasets
# -----------------------

life_at_birth <- read.csv(life_url)                    # life expectancy
gdp_per_capita <- read.csv(gdp_url)                    # GDP per capita ($)
spending_on_education <- read.csv(education_spending_url) # share of gdp
healthcare_spending <- read.csv(healthcare_url)        # healthcare spending (% GDP)
years_of_schooling <- read.csv(schooling_url)          # average years of schooling
vaccination_coverage <- read.csv(vaccination_url)      # vaccination coverage
share_of_adults_defined_as_obese <- read.csv(obesity_url)
fertility_rate <- read.csv(fertility_url)              # children per woman
urbanization <- read.csv(urbanization_url)             # urban/rural population
renewables <- read.csv(renewables_url)                 # renewable energy share
democracy <- read.csv(democracy_url)                   # 0 to 1. 1 = most demacratic

################################################################################
# select columns of interest and rename
################################################################################

life_at_birth <- life_at_birth |> select(entity, year, life_expectancy_0) |>
  rename(life_expectancy = life_expectancy_0)
#############################################
gdp_per_capita <- gdp_per_capita |> select(entity, year, rgdpo_pc) |> 
  rename( gdp_pc = rgdpo_pc)
#############################################
spending_on_education <- spending_on_education |> 
  select(entity, year, combined_expenditure_share_gdp) |>
  rename(spending_edu = combined_expenditure_share_gdp)
#############################################
healthcare_spending <- healthcare_spending |> 
  select(entity, year, share_gdp) |>
  rename(spending_healthcare = share_gdp)
#############################################
years_of_schooling <- years_of_schooling |>
  select(entity, year, mys__sex_total) |>
  rename(years_in_school = mys__sex_total)
#############################################
vaccination_coverage <- vaccination_coverage |>
  select(-code)
#############################################
share_of_adults_defined_as_obese <- share_of_adults_defined_as_obese |>
  select(entity, year, obesity_among_adults__bmi__gt__30_kg_m2__crude_estimate__pct__sex_both_sexes__age_group_18plus__years_of_age) |>
  rename(share_obesity = obesity_among_adults__bmi__gt__30_kg_m2__crude_estimate__pct__sex_both_sexes__age_group_18plus__years_of_age)
#############################################
fertility_rate <- fertility_rate |>
  select(entity, year, fertility_rate_hist) |>
  rename(children_per_woman = fertility_rate_hist)
#############################################
urbanization <- urbanization |> 
  select(entity, year, value__metric_popshare__location_type_rural_total__data_type_estimates,
         value__metric_popshare__location_type_urban_total__data_type_estimates) |>
  rename(move_from_city = value__metric_popshare__location_type_rural_total__data_type_estimates,
         move_to_city = value__metric_popshare__location_type_urban_total__data_type_estimates)

urbanization <- urbanization |> # take all years below 2025 and fill the caps between the 5 year interval
  select(entity, year, move_from_city,
         move_to_city) |>
  filter(year < 2025)

urbanization <- urbanization %>%
  arrange(entity, year) %>%
  group_by(entity) %>%
  complete(year = min(year):max(year)) %>%
  mutate(
    move_from_city =
      na.approx(
        move_from_city,
        x = year,
        na.rm = FALSE
      ),
    move_to_city =
      na.approx(
        move_to_city,
        x = year,
        na.rm = FALSE
      )
  )
#############################################
renewables <- renewables |>
  select(entity, year, renewables_share_pct)
#############################################
democracy <- democracy |>
  select(entity, year, electdem_vdem__estimate_best) |>
  rename(elect_dem_index = electdem_vdem__estimate_best)

################################################################################
# create new dataset
################################################################################

raw_data <- healthcare_spending %>% # join by healthcare to avoid alot of na"s
  left_join(life_at_birth, by = c("entity", "year")) %>%
  left_join(gdp_per_capita, by = c("entity", "year")) |>
  left_join(spending_on_education, by = c("entity", "year")) |>
  left_join(years_of_schooling, by = c("entity", "year")) |>
  left_join(vaccination_coverage, by = c("entity", "year")) |>
  left_join(share_of_adults_defined_as_obese, by = c("entity", "year")) |>
  left_join(fertility_rate, by = c("entity", "year")) |>
  left_join(urbanization, by = c("entity", "year")) |>
  left_join(renewables, by = c("entity", "year")) |>
  left_join(democracy, by = c("entity", "year"))

# create table with missing values

na_summary <- sort(colMeans(is.na(raw_data)), decreasing = TRUE)

na_table <- data.frame(
  Variable = names(na_summary),
  Missing_Percentage = round(na_summary * 100, 1)
) %>%
  filter(Missing_Percentage > 0)

kable(
  na_table,
  caption = "Variables with missing values"
)

################################################################################
# remove columns with more than 50% na's
################################################################################

raw_data <- raw_data |>
  select(-coverage__antigen_rotac, -coverage__antigen_ipv1, -coverage__antigen_pcv3,
         -coverage__antigen_hepb3)

raw_data <- raw_data[!is.na(raw_data$life_expectancy), ] # remove na's out of target

################################################################################
# split data in work and final test set
################################################################################

set.seed(123)

train_index <- createDataPartition(raw_data$life_expectancy,
                                   p = 0.85,
                                   list = FALSE)

work_set <- raw_data[train_index, ]
final_test_set  <- raw_data[-train_index, ]

################################################################################
# split work_set in train and validation
################################################################################

set.seed(123)

work_index <- createDataPartition(work_set$life_expectancy,
                                  p = 0.8,
                                  list = FALSE)

train <- work_set[work_index, ]
validate  <- work_set[-work_index, ]


################################################################################
# deal with missing values on train, test and final test set
################################################################################

################################################################################
# Remove identifiers
################################################################################

train <- train |> 
  select(-entity, -year)

validate <- validate |> 
  select(-entity, -year)

final_test_set <- final_test_set |> 
  select(-entity, -year)


################################################################################
# imputation
# Fit preprocessing only on training data
################################################################################

# Separate target from predictors
x_train <- train |> 
  select(-life_expectancy)

x_validate <- validate |> 
  select(-life_expectancy)

x_final_test <- final_test_set |> 
  select(-life_expectancy)


# Fit imputation model on training data only
set.seed(123)

imputation_model <- missForestPredict::missForest(
  x_train,
  maxiter = 10,
  initialization = "median/mode",
  save_models = TRUE,
  num.threads = 2,
  verbose = FALSE
)


# Apply the same preprocessing to all partitions
x_train_imp <- imputation_model$ximp

x_validate_imp <- missForestPredict::missForestPredict(
  imputation_model,
  newdata = x_validate
)

x_final_test_imp <- missForestPredict::missForestPredict(
  imputation_model,
  newdata = x_final_test
)


################################################################################
# Recombine target and predictors
################################################################################

train <- cbind(
  life_expectancy = train$life_expectancy,
  x_train_imp
)

validate <- cbind(
  life_expectancy = validate$life_expectancy,
  x_validate_imp
)

final_test_set <- cbind(
  life_expectancy = final_test_set$life_expectancy,
  x_final_test_imp
)


################################################################################
# Check missing values
################################################################################

if (
  sum(is.na(train)) == 0 &
  sum(is.na(validate)) == 0 &
  sum(is.na(final_test_set)) == 0
) {
  cat("No missing values were found in the training, validation, or final test set.\n")
}

################################################################################
# Check if train, validation and final test data are representative
################################################################################

# Check if train and valid data are representative

boxplot(
  train$life_expectancy,
  validate$life_expectancy,
  final_test_set$life_expectancy
)

# create table with stats

summary_table <- data.frame(
  Dataset = c("Train", "Validation", "Test"),
  N = c(nrow(train), nrow(validate), nrow(final_test_set)),
  Mean = c(
    mean(train$life_expectancy),
    mean(validate$life_expectancy),
    mean(final_test_set$life_expectancy)
  ),
  SD = c(
    sd(train$life_expectancy),
    sd(validate$life_expectancy),
    sd(final_test_set$life_expectancy)
  ),
  Min = c(
    min(train$life_expectancy),
    min(validate$life_expectancy),
    min(final_test_set$life_expectancy)
  ),
  Median = c(
    median(train$life_expectancy),
    median(validate$life_expectancy),
    median(final_test_set$life_expectancy)
  ),
  Max = c(
    max(train$life_expectancy),
    max(validate$life_expectancy),
    max(final_test_set$life_expectancy)
  )
)

summary_table[-1] <- round(summary_table[-1], 2)

kable(
  summary_table,
  caption = "Descriptive statistics of life expectancy across the data splits"
)
################################################################################
# EDA
################################################################################

#life expectancy world over the years

life_world <- life_at_birth %>%
  filter(entity == "World")

# fig 1.

ggplot(life_world,
       aes(x = year,
           y = life_expectancy,
           color = entity)) +
  geom_line(size = 1) +
  labs(
    title = "Fig 1. Life expectancy over time",
    x = "Year",
    y = "Life expectancy in years",
    color = ""
  ) +
  theme_minimal()

# find country with highest and lowest life expectancy

################################################################################

# Find the richest country using the most recent data point for each entity

gdp_current_rich <- gdp_per_capita %>%
  group_by(entity) %>%
  filter(year == max(year)) %>%  # Gets the latest year available for each country
  ungroup() %>%
  arrange(desc(gdp_pc)) %>%
  slice(1)

# Find the poorest country using the most recent data point for each entity

gdp_current_poor <- gdp_per_capita %>%
  group_by(entity) %>%
  filter(year == max(year)) %>%  # Gets the latest year available for each country
  ungroup() %>%
  arrange(gdp_pc) %>%
  slice(1)

# create a table

gdp_extremes <- bind_rows(
  gdp_current_rich %>%
    mutate(Category = "Richest Country"),
  gdp_current_poor %>%
    mutate(Category = "Poorest Country")
) %>%
  select(Category, entity, year, gdp_pc)

kable(
  gdp_extremes,
  col.names = c("Category", "Country", "Year", "GDP per Capita"),
  caption = "Countries with the Highest and Lowest GDP per Capita"
)

################################################################################

# plot life expectancy world, highest GDP and lowest GDP

data_plot <- life_at_birth %>%
  filter(entity %in% c("World", "Ireland", "South Sudan"),
         year >= 1950)

# fig 2.

ggplot(data_plot,
       aes(x = year,
           y = life_expectancy,
           color = entity)) +
  geom_line(size = 1) +
  labs(
    title = "Fig 2. life expectancy poor versus rich",
    x = "Year",
    y = "Life expectancy in years",
    color = "Country"
  ) +
  theme_minimal()
################################################################################
# fig3. Target variable

ggplot(work_set, aes(x = life_expectancy)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  labs(
    title = "Fig 3. Distribution of target variable",
    x = "Life expectancy in years",
    y = "Frequency"
  ) +
  theme_minimal()

################################################################################
# fig 4. correlation matrix

cor_matrix <- cor(
  work_set[, c(
    "life_expectancy",
    "spending_healthcare",
    "spending_edu",
    "years_in_school",
    "coverage__antigen_mcv1",
    "coverage__antigen_dtpcv3",
    "coverage__antigen_hib3",
    "coverage__antigen_rcv1",
    "share_obesity",
    "children_per_woman",
    "move_from_city",
    "move_to_city",
    "renewables_share_pct",
    "elect_dem_index",
    "gdp_pc"
  )],
  use = "complete.obs"
)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  title = "fig 4. Correlation predictors",
  mar = c(0, 0, 2, 0)
)

################################################################################
# fig 5. individual predictors with GDP as overlap

# scale
panel_scaled <- work_set %>%
  mutate(
    across(
      where(is.numeric),
      ~ as.numeric(scale(.))
    )
  )

# pivot longer
panel_long_scaled <- panel_scaled %>%
  select(-life_expectancy, -gdp_pc, -year, -entity) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "value"
  )

# fig 5.
ggplot(panel_long_scaled, aes(x = value)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  geom_density(
    data = panel_scaled,
    aes(x = gdp_pc),
    colour = "red",
    linewidth = 1,
    fill = NA,
    inherit.aes = FALSE
  ) +
  facet_wrap(
    ~ variable,
    scales = "free"
  ) +
  labs(
    title = " fig 5. Distribution of predictors with GDP per capita as overlay",
    x = "Value",
    y = "Density"
  ) +
  theme_minimal()

################################################################################
# modeling
################################################################################

################################################################################
# Simple base line
################################################################################

mean_life <- mean(train$life_expectancy)

pred_base <- rep(mean_life, nrow(validate))

rmse_simple <- sqrt(
  mean((pred_base - validate$life_expectancy)^2)
)

rmse_simple # 7.9

################################################################################
# linear model
################################################################################

model <- lm(
  life_expectancy ~ spending_healthcare + spending_edu +
    years_in_school + coverage__antigen_mcv1 + coverage__antigen_dtpcv3 + 
    coverage__antigen_hib3 + coverage__antigen_rcv1 + share_obesity + children_per_woman +
    move_from_city + move_to_city + renewables_share_pct + elect_dem_index,
  data = train
)

pred <- predict(model, newdata = validate)

rmse_lm <- sqrt(
  mean((pred - validate$life_expectancy)^2)
)

rmse_lm # 4.5

# plot model

plot(validate$life_expectancy, pred,
     main = "Fig 6.: Actual vs predicted (linear)",
     xlab = "Actual life expectancy",
     ylab = "Predicted life expectancy",
     pch = 19,
     col = "steelblue")

abline(a = 0, b = 1, col = "red", lwd = 2)

################################################################################
# linear model with gdp_pc
################################################################################

model_gdp_pc <- lm(
  life_expectancy ~ spending_healthcare + spending_edu +
    years_in_school + coverage__antigen_mcv1 + coverage__antigen_dtpcv3 + 
    coverage__antigen_hib3 + coverage__antigen_rcv1 + share_obesity + children_per_woman +
    move_from_city + move_to_city + renewables_share_pct + elect_dem_index + gdp_pc,
  data = train
)

pred <- predict(model_gdp_pc, newdata = validate)

rmse_lm_gdp <- sqrt(
  mean((pred - validate$life_expectancy)^2)
)

rmse_lm_gdp # 4.5

tidy(model_gdp_pc) %>%
  kable(digits = 3, caption = "Regression Model Results with GDP")

################################################################################
# KNN model
################################################################################

set.seed(123)

# train KNN
knn_model <- train(
  life_expectancy ~ spending_healthcare + spending_edu +
    years_in_school + coverage__antigen_mcv1 + coverage__antigen_dtpcv3 + 
    coverage__antigen_hib3 + coverage__antigen_rcv1 + share_obesity + children_per_woman +
    move_from_city + move_to_city + renewables_share_pct + elect_dem_index,
  data = train,
  method = "knn",
  preProcess = c("center", "scale"),
  tuneLength = 10
)

knn_model$bestTune

plot(knn_model)

# predict
pred_knn <- predict(knn_model, newdata = validate)

# RMSE
rmse_knn <- sqrt(
  mean((pred_knn - validate$life_expectancy)^2)
)

rmse_knn # 2.3

# plot
plot(
  validate$life_expectancy,
  pred_knn,
  main = "Figure 7: Actual vs predicted (KNN)",
  xlab = "Actual life expectancy",
  ylab = "Predicted life expectancy",
  pch = 19,
  col = "steelblue"
)

abline(a = 0, b = 1, col = "red", lwd = 2)

# variable importance

varimp_tbl <- varImp(knn_model)$importance %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  arrange(desc(Overall))

kable(
  varimp_tbl,
  digits = 3,
  caption = "Table 6. Variable importance for the KNN model"
)

################################################################################
# KNN model with gdp_pc
################################################################################

set.seed(123)

# train KNN
knn_model_gdp <- train(
  life_expectancy ~ spending_healthcare + spending_edu +
    years_in_school + coverage__antigen_mcv1 + coverage__antigen_dtpcv3 + 
    coverage__antigen_hib3 + coverage__antigen_rcv1 + share_obesity + children_per_woman +
    move_from_city + move_to_city + renewables_share_pct + elect_dem_index + gdp_pc,
  data = train,
  method = "knn",
  preProcess = c("center", "scale"),
  tuneLength = 10
)

knn_model_gdp$bestTune

plot(knn_model_gdp)

# voorspellingen
pred_knn <- predict(knn_model_gdp, newdata = validate)

# RMSE
rmse_knn_gdp <- sqrt(
  mean((pred_knn - validate$life_expectancy)^2)
)

rmse_knn_gdp # 2.2

# variable importance

varimp_tbl <- varImp(knn_model_gdp)$importance %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  arrange(desc(Overall))

kable(
  varimp_tbl,
  digits = 3,
  caption = "Table 7. Variable importance for the KNN model with GDP"
)

################################################################################
# rf model
################################################################################

set.seed(123)

tunegrid <- expand.grid(mtry = c(2,4,6,8,10))

rf_model <- train(
  life_expectancy ~ spending_healthcare + spending_edu +
    years_in_school + coverage__antigen_mcv1 + coverage__antigen_dtpcv3 + 
    coverage__antigen_hib3 + coverage__antigen_rcv1 + share_obesity + children_per_woman +
    move_from_city + move_to_city + renewables_share_pct + elect_dem_index,
  data = train,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = tunegrid,
  ntree = 500
)

pred_rf <- predict(rf_model, newdata = validate)

rmse_rf <- sqrt(
  mean((pred_rf - validate$life_expectancy)^2)
)

rmse_rf # 2.0

plot(validate$life_expectancy, pred_rf,
     main = "Figure 8: Actual vs predicted (RF)",
     xlab = "Actual life expectancy",
     ylab = "Predicted life expectancy",
     pch = 19,
     col = "steelblue")

abline(a = 0, b = 1, col = "red", lwd = 2)

# variable importance

varimp_tbl <- varImp(rf_model)$importance %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  arrange(desc(Overall))

kable(
  varimp_tbl,
  digits = 3,
  caption = "Table 8. Variable importance for the RF model"
)

plot(varImp(rf_model))

################################################################################
# rf model with gdp_pc
################################################################################

set.seed(123)

tunegrid <- expand.grid(mtry = c(2,4,6,8,10))

rf_model_gdp <- train(
  life_expectancy ~ spending_healthcare + spending_edu +
    years_in_school + coverage__antigen_mcv1 + coverage__antigen_dtpcv3 + 
    coverage__antigen_hib3 + coverage__antigen_rcv1 + share_obesity + children_per_woman +
    move_from_city + move_to_city + renewables_share_pct + elect_dem_index + gdp_pc,
  data = train,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = tunegrid,
  ntree = 500
)

pred_rf <- predict(rf_model_gdp, newdata = validate)

rmse_rf_gdp <- sqrt(
  mean((pred_rf - validate$life_expectancy)^2)
)

rmse_rf_gdp # 2

# variable importance

varimp_tbl <- varImp(rf_model_gdp)$importance %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  arrange(desc(Overall))

kable(
  varimp_tbl,
  digits = 3,
  caption = "Table 9. Variable importance for the RF model with GDP"
)

plot(varImp(rf_model_gdp))

################################################################################
# test RF model with GDP on final_test_set
################################################################################

set.seed(123)

tunegrid <- expand.grid(mtry = c(2,4,6,8,10))

rf_model_best <- train(
  life_expectancy ~ spending_healthcare + spending_edu +
    years_in_school + coverage__antigen_mcv1 + coverage__antigen_dtpcv3 + 
    coverage__antigen_hib3 + coverage__antigen_rcv1 + children_per_woman +
    move_from_city + move_to_city + renewables_share_pct + elect_dem_index + gdp_pc,
  data = train,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = tunegrid,
  ntree = 500
)

pred_rf <- predict(rf_model_best, newdata = final_test_set)

rmse_final_test <- sqrt(
  mean((pred_rf - final_test_set$life_expectancy)^2)
)

rmse_final_test # 1.8

################################################################################
# Summary
################################################################################

model_results <- data.frame(
  Model = c(
    "Baseline",
    "Linear Regression",
    "Linear Regression + GDP",
    "KNN",
    "KNN + GDP",
    "Random Forest",
    "Random Forest + GDP"
  ),
  Validation_RMSE = c(
    rmse_simple,
    rmse_lm,
    rmse_lm_gdp,
    rmse_knn,
    rmse_knn_gdp,
    rmse_rf,
    rmse_rf_gdp
  )
)

model_results$Validation_RMSE <- round(
  model_results$Validation_RMSE,
  2
)

kable(
  model_results,
  caption = "Table 10. Validation RMSE for all evaluated models"
)

final_results <- data.frame(
  Model = "Random Forest + GDP",
  Validation_RMSE = round(rmse_rf_gdp, 2),
  Final_Test_RMSE = round(rmse_final_test, 2)
)

kable(
  final_results,
  caption = "Table 11. Performance of the final selected model"
)
