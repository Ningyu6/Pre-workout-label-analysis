#install.packages("tidyverse")

library(tidyverse)

data_folder <- "data"

products <- read_csv(
  file.path(data_folder, "ProductOverview.csv"),
  show_col_types = FALSE
)

facts <- read_csv(
  file.path(data_folder, "DietarySupplementFacts.csv"),
  show_col_types = FALSE
)

statements <- read_csv(
  file.path(data_folder, "LabelStatements.csv"),
  show_col_types = FALSE
)

data_summary <- tibble(
  dataset = c(
    "Product Overview",
    "Dietary Supplement Facts",
    "Label Statements"
  ),
  rows = c(
    nrow(products),
    nrow(facts),
    nrow(statements)
  ),
  columns = c(
    ncol(products),
    ncol(facts),
    ncol(statements)
  ),
  unique_labels = c(
    n_distinct(products$`DSLD ID`),
    n_distinct(facts$`DSLD ID`),
    n_distinct(statements$`DSLD ID`)
  )
)

print(data_summary)

# Data clean

# Check duplicate rows
duplicate_summary <- tibble(
  dataset = c(
    "Product Overview",
    "Dietary Supplement Facts",
    "Label Statements"
  ),
  duplicate_rows = c(
    sum(duplicated(products)),
    sum(duplicated(facts)),
    sum(duplicated(statements))
  )
)

print(duplicate_summary)

#  Remove exact duplicate rows

products_clean <- products %>%
  distinct()

facts_clean <- facts %>%
  distinct()

statements_clean <- statements %>%
  distinct()

# Compare row counts before and after cleaning
cleaning_summary <- tibble(
  dataset = c(
    "Product Overview",
    "Dietary Supplement Facts",
    "Label Statements"
  ),
  rows_before = c(
    nrow(products),
    nrow(facts),
    nrow(statements)
  ),
  rows_after = c(
    nrow(products_clean),
    nrow(facts_clean),
    nrow(statements_clean)
  )
)

print(cleaning_summary)

# Check whether the product IDs can be connected across tables
all(products_clean$`DSLD ID` %in% facts_clean$`DSLD ID`)
all(products_clean$`DSLD ID` %in% statements_clean$`DSLD ID`)

# Inspect column names and data types

names(products_clean)

names(facts_clean)

names(statements_clean)

glimpse(facts_clean)

# Standardize IDs and text fields

products_clean <- products_clean %>%
  mutate(
    across(
      where(is.character),
      ~ na_if(str_squish(.x), "")
    ),
    `DSLD ID` = as.character(`DSLD ID`)
  )

facts_clean <- facts_clean %>%
  mutate(
    across(
      where(is.character),
      ~ na_if(str_squish(.x), "")
    ),
    `DSLD ID` = as.character(`DSLD ID`)
  )

statements_clean <- statements_clean %>%
  mutate(
    across(
      where(is.character),
      ~ na_if(str_squish(.x), "")
    ),
    `DSLD ID` = as.character(`DSLD ID`)
  )


# Verify the result
glimpse(facts_clean)

n_distinct(products_clean$`DSLD ID`)
n_distinct(facts_clean$`DSLD ID`)
n_distinct(statements_clean$`DSLD ID`)

#  Analyze missing values

missing_summary <- facts_clean %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_count"
  ) %>%
  mutate(
    total_rows = nrow(facts_clean),
    missing_percent = round(
      missing_count / total_rows * 100,
      1
    )
  ) %>%
  arrange(desc(missing_percent))

print(missing_summary, n = Inf)

# Check dosage units

unit_summary <- facts_clean %>%
  count(
    `Amount Per Serving Unit`,
    sort = TRUE,
    name = "row_count"
  ) %>%
  mutate(
    percent = round(
      row_count / nrow(facts_clean) * 100,
      1
    )
  )

print(unit_summary, n = 20)

# Standardize dosage units

facts_clean <- facts_clean %>%
  mutate(
    unit_standard = case_when(
      `Amount Per Serving Unit` %in% c("Gram(s)", "g") ~ "g",
      
      `Amount Per Serving Unit` %in%
        c("Calorie(s)", "{Calories}") ~ "Calories",
      
      `Amount Per Serving Unit` == "None" ~ NA_character_,
      
      TRUE ~ `Amount Per Serving Unit`
    ),
    
    amount_disclosure = case_when(
      is.na(`Amount Per Serving`) ~ "Not disclosed",
      TRUE ~ "Disclosed"
    )
  )


# Check standardized units
standard_unit_summary <- facts_clean %>%
  count(
    unit_standard,
    sort = TRUE,
    name = "row_count"
  ) %>%
  mutate(
    percent = round(
      row_count / nrow(facts_clean) * 100,
      1
    )
  )

print(standard_unit_summary, n = 20)


# Check dosage disclosure status
disclosure_summary <- facts_clean %>%
  count(
    amount_disclosure,
    name = "row_count"
  ) %>%
  mutate(
    percent = round(
      row_count / sum(row_count) * 100,
      1
    )
  )

print(disclosure_summary)

# Create label-level metrics
label_metrics <- facts_clean %>%
  group_by(`DSLD ID`) %>%
  summarise(
    listed_item_count = n(),
    
    disclosed_item_count = sum(
      amount_disclosure == "Disclosed"
    ),
    
    disclosure_rate = round(
      mean(amount_disclosure == "Disclosed") * 100,
      1
    ),
    
    .groups = "drop"
  )


# Join metrics with product information
product_analysis <- products_clean %>%
  left_join(
    label_metrics,
    by = "DSLD ID"
  )

# Calculate dashboard KPIs

label_kpis <- label_metrics %>%
  summarise(
    total_labels = n(),
    
    median_listed_items = median(
      listed_item_count
    ),
    
    median_disclosure_rate = round(
      median(disclosure_rate),
      1
    ),
    
    fully_disclosed_labels = sum(
      disclosure_rate == 100
    ),
    
    fully_disclosed_percent = round(
      mean(disclosure_rate == 100) * 100,
      1
    )
  )

print(label_kpis)


# Check the new product-level dataset
glimpse(product_analysis)

# Visualize disclosure rates
median_rate <- median(
  label_metrics$disclosure_rate
)

ggplot(
  label_metrics,
  aes(x = disclosure_rate)
) +
  geom_histogram(
    binwidth = 10,
    boundary = 0,
    fill = "green",
    color = "white"
  ) +
  geom_vline(
    xintercept = median_rate,
    color = "pink",
    linewidth = 1,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, 10)
  ) +
  labs(
    title = "Distribution of Dosage Disclosure Rates",
    subtitle = "Each observation represents one pre-workout label",
    x = "Dosage disclosure rate (%)",
    y = "Number of product labels"
  ) +
  theme_minimal(base_size = 12)

# Create disclosure categories

disclosure_bands <- label_metrics %>%
  mutate(
    disclosure_band = case_when(
      disclosure_rate == 100 ~
        "Full disclosure (100%)",
      
      disclosure_rate >= 80 ~
        "High disclosure (80–<100%)",
      
      disclosure_rate >= 50 ~
        "Moderate disclosure (50–<80%)",
      
      TRUE ~
        "Low disclosure (<50%)"
    ),
    
    disclosure_band = factor(
      disclosure_band,
      levels = c(
        "Low disclosure (<50%)",
        "Moderate disclosure (50–<80%)",
        "High disclosure (80–<100%)",
        "Full disclosure (100%)"
      )
    )
  ) %>%
  count(
    disclosure_band,
    name = "label_count"
  ) %>%
  mutate(
    percent = round(
      label_count / sum(label_count) * 100,
      1
    )
  )

print(disclosure_bands)

disclosure_plot <- ggplot(
  disclosure_bands,
  aes(
    x = disclosure_band,
    y = label_count,
    fill = disclosure_band
  )
) +
  geom_col(
    width = 0.7,
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      label = paste0(
        label_count,
        " (",
        percent,
        "%)"
      )
    ),
    hjust = -0.1,
    size = 4
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "#E76F51",
      "#F4A261",
      "#E9C46A",
      "#2A9D8F"
    )
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.18)
    )
  ) +
  labs(
    title = "Dosage Disclosure Across Pre-Workout Labels",
    subtitle = "Labels grouped by the share of listed items with a disclosed dosage",
    x = NULL,
    y = "Number of product labels"
  ) +
  theme_minimal(base_size = 12)

print(disclosure_plot)

#Calculate raw ingredient prevalence

# Total number of product labels
total_labels <- n_distinct(
  facts_clean$`DSLD ID`
)

print(total_labels)

# Count how many labels contain each ingredient
ingredient_frequency_raw <- facts_clean %>%
  distinct(
    `DSLD ID`,
    Ingredient
  ) %>%
  count(
    Ingredient,
    name = "label_count",
    sort = TRUE
  ) %>%
  mutate(
    label_percent = round(
      label_count / total_labels * 100,
      1
    )
  )


# Display the top 30 raw results
print(
  ingredient_frequency_raw,
  n = 30
)

# Define the scope of ingredient analysis


nutrition_summary_items <- c(
  "Calories",
  "Calories from Fat",
  "Total Fat",
  "Fat",
  "Saturated Fat",
  "Trans Fat",
  "Cholesterol",
  "Total Carbohydrates",
  "Total Carbohydrate",
  "Carbohydrates",
  "Dietary Fiber",
  "Total Sugars",
  "Added Sugars",
  "Protein"
)


ingredient_frequency <- facts_clean %>%
  mutate(
    ingredient_scope = case_when(
      Ingredient %in% nutrition_summary_items ~
        "Nutrition summary item",
      
      TRUE ~
        "Supplement ingredient"
    )
  ) %>%
  filter(
    ingredient_scope == "Supplement ingredient"
  ) %>%
  distinct(
    `DSLD ID`,
    Ingredient
  ) %>%
  count(
    Ingredient,
    name = "label_count",
    sort = TRUE
  ) %>%
  mutate(
    label_percent = round(
      label_count / total_labels * 100,
      1
    )
  )


# Select the top 15 ingredients
top15_ingredients <- ingredient_frequency %>%
  slice_max(
    order_by = label_count,
    n = 15,
    with_ties = FALSE
  )

print(top15_ingredients)

#visualize the top 15 ingredients

top15_ingredient_plot <- ggplot(
  top15_ingredients,
  aes(
    x = reorder(Ingredient, label_count),
    y = label_count
  )
) +
  geom_col(
    fill = "#2A9D8F",
    width = 0.7
  ) +
  geom_text(
    aes(
      label = paste0(
        label_count,
        " (",
        label_percent,
        "%)"
      )
    ),
    hjust = -0.1,
    size = 3.8
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.22)
    )
  ) +
  labs(
    title = "Most Common Ingredients in Pre-Workout Labels",
    subtitle = "Exact ingredient names，conventional nutrition summary items excluded",
    x = NULL,
    y = "Number of product labels"
  ) +
  theme_minimal(base_size = 12)

print(top15_ingredient_plot)


# Build a unified label-ingredient table

label_ingredient <- facts_clean %>%
  
  # Exclude conventional nutrition summary items
  filter(
    !Ingredient %in% nutrition_summary_items
  ) %>%
  
  # One group represents one ingredient within one label
  group_by(
    `DSLD ID`,
    Ingredient
  ) %>%
  
  summarise(
    # Retain ingredient category information
    ingredient_category = paste(
      sort(
        unique(`DSLD Ingredient Categories`)
      ),
      collapse = "; "
    ),
    
    # Number of original rows behind this combination
    source_row_count = n(),
    
    # Number of serving sizes recorded for this ingredient
    serving_size_count = n_distinct(
      `Serving Size`
    ),
    
    # Whether at least one row provides a dosage
    any_dose_disclosed = any(
      !is.na(`Amount Per Serving`)
    ),
    
    # Whether every source row provides a dosage
    all_doses_disclosed = all(
      !is.na(`Amount Per Serving`)
    ),
    
    .groups = "drop"
  ) %>%
  
  # Classify ingredient-level disclosure
  mutate(
    ingredient_disclosure_status = case_when(
      all_doses_disclosed ~
        "Fully disclosed",
      
      any_dose_disclosed ~
        "Partially disclosed",
      
      TRUE ~
        "Not disclosed"
    )
  )

label_ingredient <- label_ingredient %>%
  left_join(
    products_clean %>%
      select(
        `DSLD ID`,
        `Product Name`
      ),
    by = "DSLD ID"
  ) %>%
  select(
    `DSLD ID`,
    `Product Name`,
    Ingredient,
    ingredient_category,
    source_row_count,
    serving_size_count,
    any_dose_disclosed,
    all_doses_disclosed,
    ingredient_disclosure_status
  )

glimpse(label_ingredient)

label_ingredient %>%
  count(
    ingredient_disclosure_status
  ) %>%
  print()

# Calculate refined label-level metrics

label_metrics_refined <- label_ingredient %>%
  group_by(
    `DSLD ID`,
    `Product Name`
  ) %>%
  summarise(
    # Formulation complexity
    ingredient_count = n(),
    
    # Ingredient disclosure counts
    fully_disclosed_count = sum(
      ingredient_disclosure_status ==
        "Fully disclosed"
    ),
    
    partially_disclosed_count = sum(
      ingredient_disclosure_status ==
        "Partially disclosed"
    ),
    
    not_disclosed_count = sum(
      ingredient_disclosure_status ==
        "Not disclosed"
    ),
    
    # Strict: every source row must provide a dosage
    disclosure_rate_strict = round(
      mean(all_doses_disclosed) * 100,
      1
    ),
    
    # Broad: at least one source row provides a dosage
    disclosure_rate_any = round(
      mean(any_dose_disclosed) * 100,
      1
    ),
    
    .groups = "drop"
  )

# Calculate refined dashboard KPIs

refined_kpis <- label_metrics_refined %>%
  summarise(
    total_labels = n(),
    
    median_ingredient_count = median(
      ingredient_count
    ),
    
    mean_ingredient_count = round(
      mean(ingredient_count),
      1
    ),
    
    median_disclosure_rate = round(
      median(disclosure_rate_strict),
      1
    ),
    
    fully_disclosed_labels = sum(
      disclosure_rate_strict == 100
    ),
    
    fully_disclosed_percent = round(
      mean(disclosure_rate_strict == 100) *
        100,
      1
    )
  )

print(refined_kpis)

# Compare old and refined disclosure definitions

disclosure_comparison <- label_metrics %>%
  select(
    `DSLD ID`,
    old_disclosure_rate = disclosure_rate
  ) %>%
  inner_join(
    label_metrics_refined %>%
      select(
        `DSLD ID`,
        refined_disclosure_rate =
          disclosure_rate_strict
      ),
    by = "DSLD ID"
  ) %>%
  summarise(
    old_median_rate = median(
      old_disclosure_rate
    ),
    
    refined_median_rate = median(
      refined_disclosure_rate
    ),
    
    old_fully_disclosed_labels = sum(
      old_disclosure_rate == 100
    ),
    
    refined_fully_disclosed_labels = sum(
      refined_disclosure_rate == 100
    ),
    
    mean_absolute_change = round(
      mean(
        abs(
          refined_disclosure_rate -
            old_disclosure_rate
        )
      ),
      1
    )
  )

print(disclosure_comparison)

# Create refined disclosure categories

disclosure_bands_refined <- label_metrics_refined %>%
  mutate(
    disclosure_band = case_when(
      disclosure_rate_strict == 100 ~
        "Full(100%)",
      
      disclosure_rate_strict >= 80 ~
        "High(80–<100%)",
      
      disclosure_rate_strict >= 50 ~
        "Moderate(50–<80%)",
      
      TRUE ~
        "Low(<50%)"
    ),
    
    disclosure_band = factor(
      disclosure_band,
      levels = c(
        "Low(<50%)",
        "Moderate(50–<80%)",
        "High(80–<100%)",
        "Full(100%)"
      )
    )
  ) %>%
  count(
    disclosure_band,
    name = "label_count",
    .drop = FALSE
  ) %>%
  mutate(
    percent = round(
      label_count / sum(label_count) * 100,
      1
    )
  )

print(disclosure_bands_refined)

# Plot refined disclosure categories

disclosure_plot_refined <- ggplot(
  disclosure_bands_refined,
  aes(
    x = disclosure_band,
    y = label_count,
    fill = disclosure_band
  )
) +
  geom_col(
    width = 0.7,
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      label = paste0(
        label_count,
        " (",
        percent,
        "%)"
      )
    ),
    hjust = -0.1,
    size = 4
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "red",
      "orange",
      "yellow",
      "green"
    )
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.18)
    )
  ) +
  labs(
    title = "Dosage Disclosure ",
    subtitle = paste(
      "Based on unique supplement ingredients"
    ),
    x = NULL,
    y = "Number of product labels"
  ) +
  theme_minimal(base_size = 12)

print(disclosure_plot_refined)

# Summarize formulation complexity


complexity_summary <- label_metrics_refined %>%
  summarise(
    minimum_ingredients = min(
      ingredient_count
    ),
    
    first_quartile = quantile(
      ingredient_count,
      0.25
    ),
    
    median_ingredients = median(
      ingredient_count
    ),
    
    mean_ingredients = round(
      mean(ingredient_count),
      1
    ),
    
    third_quartile = quantile(
      ingredient_count,
      0.75
    ),
    
    maximum_ingredients = max(
      ingredient_count
    )
  )

print(complexity_summary)

# Spearman correlation

complexity_correlation <- cor.test(
  label_metrics_refined$ingredient_count,
  label_metrics_refined$disclosure_rate_strict,
  method = "spearman",
  exact = FALSE
)

print(complexity_correlation)
rho_value <- round(
  unname(complexity_correlation$estimate),
  2
)

p_value <- format.pval(
  complexity_correlation$p.value,
  digits = 3
)

# Plot complexity versus disclosure

complexity_disclosure_plot <- ggplot(
  label_metrics_refined,
  aes(
    x = ingredient_count,
    y = disclosure_rate_strict
  )
) +
  geom_jitter(
    width = 0.25,
    height = 0.8,
    alpha = 0.4,
    size = 2,
    color = "blue"
  ) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    span = 0.8,
    color = "red",
    linewidth = 1.1
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    labels = function(x) {
      paste0(x, "%")
    }
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 8)
  ) +
  labs(
    title = "Formulation Complexity vs. Dosage Disclosure",
    subtitle = paste0(
      "Each point represents one product label; ",
      "Spearman rho = ",
      rho_value,
      ", p = ",
      p_value
    ),
    x = "Number of unique supplement ingredients",
    y = "Dosage disclosure rate",
    )+
  theme_minimal(base_size = 12)

print(complexity_disclosure_plot)

# Prepare final label-level metrics

label_metrics_final <- label_metrics_refined %>%
  mutate(
    disclosure_band = case_when(
      disclosure_rate_strict == 100 ~
        "Full disclosure (100%)",
      
      disclosure_rate_strict >= 80 ~
        "High disclosure (80–<100%)",
      
      disclosure_rate_strict >= 50 ~
        "Moderate disclosure (50–<80%)",
      
      TRUE ~
        "Low disclosure (<50%)"
    ),
    
    disclosure_band_order = case_when(
      disclosure_rate_strict == 100 ~ 4,
      disclosure_rate_strict >= 80 ~ 3,
      disclosure_rate_strict >= 50 ~ 2,
      TRUE ~ 1
    ),
    
    fully_disclosed_flag = case_when(
      disclosure_rate_strict == 100 ~ "Yes",
      TRUE ~ "No"
    )
  )

#  Build the Tableau dataset

tableau_data <- label_ingredient %>%
  left_join(
    label_metrics_final %>%
      select(
        -`Product Name`
      ),
    by = "DSLD ID"
  ) %>%
  left_join(
    products_clean %>%
      select(
        -`Product Name`
      ),
    by = "DSLD ID"
  ) %>%
  relocate(
    `DSLD ID`,
    `Product Name`,
    Ingredient,
    ingredient_category,
    ingredient_disclosure_status,
    ingredient_count,
    disclosure_rate_strict,
    disclosure_band,
    disclosure_band_order
  )

#  Export data for Tableau

dir.create(
  "output",
  showWarnings = FALSE
)

write_csv(
  tableau_data,
  "output/preworkout_tableau_data.csv"
)

list.files("output")

# Create an output folder
dir.create("output", showWarnings = FALSE)

# Label-level data: one row per product label
write_csv(
  label_metrics_final,
  "output/preworkout_label_metrics.csv"
)

# Ingredient-level data: one row per label-ingredient combination
write_csv(
  label_ingredient,
  "output/preworkout_label_ingredients.csv"
)

# Check exported files
list.files("output")