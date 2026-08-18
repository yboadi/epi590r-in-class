library(tidyverse)
library(gtsummary)
install.packages("tidyverse")
install.packages("vctrs")

# Load and clean data
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




# simple table
tbl_summary(
  nlsy,
  by = sex_cat,
  include = c(
    sex_cat, race_eth_cat, region_cat,
    eyesight_cat, glasses, age_bir
  )
)

# add labels for the variables and for the "missing" category
tbl_summary(
  nlsy,
  by = sex_cat,
  include = c(
    sex_cat, race_eth_cat, region_cat,
    eyesight_cat, glasses, age_bir
  ),
  label = list(
    race_eth_cat ~ "Race/ethnicity",
    region_cat ~ "Region",
    eyesight_cat ~ "Eyesight",
    glasses ~ "Wears glasses",
    age_bir ~ "Age at first birth"
  ),
  missing_text = "Missing"
)

# add p-values, a total column, bold labels, and remove the footnote
tbl_summary(
  nlsy,
  by = sex_cat,
  include = c(
    sex_cat, race_eth_cat,
    eyesight_cat, glasses, age_bir
  ),
  label = list(
    race_eth_cat ~ "Race/ethnicity",
    eyesight_cat ~ "Eyesight",
    glasses ~ "Wears glasses",
    age_bir ~ "Age at first birth"
  ),
  missing_text = "Missing"
) |>
  # change the test used to compare sex_cat groups
  add_p(test = list(
    all_continuous() ~ "t.test",
    all_categorical() ~ "chisq.test"
  )) |>
  # add a total column with the number of observations
  add_overall(col_label = "**Total** N = {N}") |>
  bold_labels() |>
  # remove the default footnotes
  # (in gtsummary < 2.0 this was modify_footnote(update = everything() ~ NA),
  #  which is now deprecated -- you'll still see it suggested in a lot of places)
  remove_footnote_header() |>
  # replace the column headers and make them bold
  modify_header(label = "**Variable**", p.value = "**P**") |>
  # add a caption
  modify_caption("**Participant characteristics**")

# table that includes income and sleep variable
tbl_summary(
	nlsy,
	by = sex_cat,
	include = c (region_cat, race_eth_cat, income, sleep_wkdy, sleep_wknd)
)

#labeling the table 1 nicely
tbl_summary(
	nlsy,
	by = sex_cat,
	include = c (region_cat, race_eth_cat, income, sleep_wkdy, sleep_wknd),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		income ~ "Income",
		region_cat ~ "Region",
		sleep_wkdy ~ "Sleep on Weekday",
		sleep_wknd ~ "Sleep on Weekend"
	),
	missing_text = "Missing"
) |>
	add_p(test = list(
		all_continuous() ~ "t.test",
		all_categorical() ~ "chisq.test"
)) |>
		add_overall(col_label = "**Total** N = {N}")

#income 10th & 90th percentiles
tbl_summary(
	nlsy,
	include = c (region_cat, race_eth_cat, income, sleep_wkdy, sleep_wknd),

	by = sex_cat,


	digits = list(income ~ 3, starts_with("sleep") ~ 1),
	statistic = list (income ~ "{p10}, {p90}", starts_with("sleep") ~ {min}, {max}),

	label = list(
		race_eth_cat ~ "Race/ethnicity",
		income ~ "Income",
		region_cat ~ "Region",
		sleep_wkdy ~ "Sleep on Weekday",
		sleep_wknd ~ "Sleep on Weekend"
	),
	missing_text = "Missing"
) |>
	add_p(test = list(
		all_continuous() ~ "t.test",
		all_categorical() ~ "chisq.test"
	)) |>
	add_overall(col_label = "**Total** N = {N}")

#add footnote
modify_footnote_body(footnote = https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data
										 columns = "label",

)
