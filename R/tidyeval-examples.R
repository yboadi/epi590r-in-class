library(tidyverse)
library(gtsummary)

# load and clean data
nlsy_cols <- c(
  "glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
  "id", "nsibs", "samp", "race_eth", "sex", "region",
  "income", "res_1980", "res_2002", "age_bir"
)
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
  na = c("-1", "-2", "-3", "-4", "-5", "-998"),
  skip = 1, col_names = nlsy_cols
) |>
  mutate(
    region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
    sex_cat = factor(sex, labels = c("Male", "Female")),
    race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
    eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
    glasses_cat = factor(glasses, labels = c("No", "Yes"))
  )


## The problem ----------------------------------------------------------------

# we keep writing the same thing with a different variable
summarise(nlsy, mean = mean(income, na.rm = TRUE))
summarise(nlsy, mean = mean(age_bir, na.rm = TRUE))
summarise(nlsy, mean = mean(nsibs, na.rm = TRUE))

# the obvious function DOESN'T work
# run it and read the error -- this is the point of the exercise
summarize_var_bad <- function(data, variable) {
  data |>
    summarise(mean = mean(variable, na.rm = TRUE))
}
summarize_var_bad(nlsy, income)

# dplyr looks for a COLUMN literally named `variable`, doesn't find one, gives up


## The fix: {{ }} -------------------------------------------------------------

summarize_var <- function(data, variable) {
  data |>
    summarise(
      n    = sum(!is.na({{ variable }})),
      mean = mean({{ variable }}, na.rm = TRUE),
      sd   = sd({{ variable }}, na.rm = TRUE)
    )
}

summarize_var(nlsy, income)
summarize_var(nlsy, age_bir)

# it takes data first, so it pipes
nlsy |> summarize_var(nsibs)


## {{ }} works anywhere dplyr does --------------------------------------------

# with .by =
# note the NULL default, so the group argument is optional
summarize_by <- function(data, variable, group = NULL) {
  data |>
    summarise(
      mean = mean({{ variable }}, na.rm = TRUE),
      n    = n(),
      .by  = {{ group }}
    )
}

summarize_by(nlsy, income, sex_cat)
summarize_by(nlsy, income) # still works with no group

# naming the output column after the variable
# note := instead of = when the name is computed
mean_named <- function(data, variable) {
  data |>
    summarise("mean_{{ variable }}" := mean({{ variable }}, na.rm = TRUE))
}

mean_named(nlsy, income)
mean_named(nlsy, age_bir)

# in ggplot
plot_hist <- function(data, variable) {
  ggplot(data, aes(x = {{ variable }})) +
    geom_histogram(bins = 30) +
    theme_minimal()
}

plot_hist(nlsy, income)
plot_hist(nlsy, age_bir)

# in gtsummary -- write the formatting once, change the grouping variable freely
table_by <- function(data, group) {
  data |>
    tbl_summary(
      by = {{ group }},
      include = c(race_eth_cat, eyesight_cat, age_bir)
    ) |>
    add_overall() |>
    bold_labels()
}

table_by(nlsy, sex_cat)
table_by(nlsy, region_cat)


## Where {{ }} does NOT work --------------------------------------------------

# lm() is base R -- it doesn't know anything about tidy evaluation
# run this and read the error
fit_bad <- function(data, predictor) {
  lm(income ~ {{ predictor }}, data = data)
}
fit_bad(nlsy, age_bir)

# instead, pass a string and build the formula with reformulate()
fit_income <- function(data, predictors) {
  lm(reformulate(predictors, response = "income"), data = data)
}

coef(fit_income(nlsy, "age_bir"))

# reformulate() takes a vector, so multivariable models come for free
coef(fit_income(nlsy, c("age_bir", "sex_cat", "race_eth_cat")))


#### Exercises ####

# 1. Write summarize_var_new() yourself, which returns the median, 25% percentile,
#    and 75% percentile of a variable using {{ }}.
#    Test it on income, age_bir, and nsibs.

summarize_var_new <- function(data, variable) {
	summarize(data,
				    median = median({{variable}}, na.rm = T),
				    pctl_25 = quantile ({{variable}}, p = .25, na.rm = T),
				    pctl_75 = quantile ({{variable}}, p = .75, na.rm = T)
	)
}

summarize_var_new(nlsy, income)
summarize_var_new(nlsy, age_bir)
summarize_var_new(nlsy, nsibs)
	# 2. Add a `group` argument using .by = {{ group }}, with a default so that
#    the function still works when you don't pass a group.
summarize_var_new <- function(data, variable, group = NULL) {
	summarize(data,
						median = median({{variable}}),
						pctl_25 = quantile ({{variable}}, p = .25, na.rm = T),
						pctl_75 = quantile ({{variable}}, p = .75, na.rm = T),
						.by = {{group}}

	)
}

summarize_var_new(nlsy, income, sex_cat)
summarize_var_new(nlsy, income) # should still work
# 3. Write a function summarize_two_vars() that takes a dataset and two variables and
#    returns their correlation and covariance. Use {{ }} to pass the variables. Test it
#    on income and age_bir, and on income and nsibs.
summarize_two_cars <- function(data, variable1, variable2) {
	summarize(data,
						covariance = cov({{variable1}}, {{variable2}}, use = "pairwise.complete")
						correlation = cor({{variable1}}, {{variable2}}use = "pairwise.complete"))
}
# 4. Write a function that takes a dataset and a grouping variable and returns
#    a gtsummary table stratified by it. Add at least one formatting function
#    (bold_labels(), add_overall(), add_p(), modify_caption(), ...).
#    Then call it twice with different grouping variables.
