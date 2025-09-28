# Install packages (only once)
install.packages("tidyverse")
install.packages("plotly")
install.packages("data.table")

# Load libraries
library(tidyverse)
library(plotly)
library(data.table)

# 📂 Load CSV file
df <- read.csv("C:/Users/prati/Downloads/archive (3)/Global_Development_Indicators_2000_2020.csv")

# 👀 View first 5 rows
head(df, 5)

# 📊 Overall summary
summary(df)

# 🔍 Identify numerical columns
numerical_columns <- names(df)[sapply(df, is.numeric)]

# 🔍 Identify categorical columns
categorical_columns <- names(df)[sapply(df, is.character) | sapply(df, is.factor)]

# 📊 Summary statistics for numerical columns
summary(df[, numerical_columns])

# 📊 Summary statistics for categorical columns (similar to Python's describe(include='all'))
lapply(df[, categorical_columns, drop = FALSE], table)

# 📦 Install naniar (run once)
install.packages("naniar")

# Then load it
library(naniar)

install.packages("reshape2")

library(reshape2)

library(ggplot2)


# Count missing values per column
missing_counts <- colSums(is.na(df))

# Convert to data frame
missing_df <- data.frame(
  Column = names(missing_counts),
  Missing = missing_counts
)

# Bar plot of missing values per column
ggplot(missing_df, aes(x = reorder(Column, -Missing), y = Missing)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Missing Values per Column",
       x = "Columns",
       y = "Number of Missing Values") +
  theme_minimal() +
  
  
  # --- Value Counts of Categorical Features ---
  categorical_cols <- c("region", "income_group")

# Loop through each categorical column
for (col in categorical_cols) {
  
  ggplot(df, aes_string(x = col)) +
    geom_bar(fill = "skyblue") +
    labs(title = paste("Distribution of", col),
         x = col, y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> p
  
  print(p)
}
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  library(dplyr)
  
  
  # --- Group the data by year and calculate the mean GDP ---
  df_grouped <- df %>%
    group_by(year) %>%
    summarise(mean_gdp = mean(gdp_usd, na.rm = TRUE))
  
  # --- Line plot ---
  ggplot(df_grouped, aes(x = year, y = mean_gdp)) +
    geom_line(color = "steelblue") +
    geom_point(color = "darkred") +   # marker='o' equivalent
    labs(title = "Average Global GDP Over Years",
         x = "Year",
         y = "GDP in USD") +
    theme_minimal() +
    theme(panel.grid.major = element_line(color = "grey80"),  # grid
          panel.grid.minor = element_line(color = "grey90"))

  
  # --- Group the data by year and calculate mean renewable energy percentage ---
  df_renew <- df %>%
    group_by(year) %>%
    summarise(mean_renew = mean(renewable_energy_pct, na.rm = TRUE))
  
  # --- Area + Line plot ---
  ggplot(df_renew, aes(x = year, y = mean_renew)) +
    geom_area(fill = "green", alpha = 0.5) +     # Area (like fill_between)
    geom_line(color = "darkgreen", size = 1) +   # Line on top
    geom_point(color = "darkgreen") +            # Markers
    labs(title = "Global Renewable Energy % Over Time",
         x = "Year",
         y = "Renewable Energy (%)") +
    theme_minimal() +
    theme(panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_line(color = "grey90"))
  
  
  library(plotly)
  
  
  # --- Select top 5 countries by number of records ---
  top_countries <- df %>%
    count(country_name, sort = TRUE) %>%
    slice_head(n = 5) %>%   # Take exactly top 5
    pull(country_name)
  
  # Filter dataset for these top countries
  filtered_df <- df %>%
    filter(country_name %in% top_countries)
  
  # --- Plotly line plot ---
  fig <- plot_ly(filtered_df, 
                 x = ~year, 
                 y = ~gdp_usd, 
                 color = ~country_name, 
                 type = 'scatter', 
                 mode = 'lines',
                 line = list(width = 2)) %>%
    layout(title = "GDP Trend Over Years (Top 5 Countries)",
           xaxis = list(title = "Year"),
           yaxis = list(title = "GDP in USD"),
           legend = list(title = list(text='Country')))
  fig
  
  # --- Boxplot: GDP per Capita by Income Group ---
  ggplot(df, aes(x = income_group, y = gdp_per_capita, fill = income_group)) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Set2") +   # Similar to Seaborn Set2 palette
    labs(title = "GDP per Capita by Income Group",
         x = "Income Group",
         y = "GDP per Capita (USD)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  # --- Remove rows with NA in income_group or life_expectancy ---
  df_violin <- df[!is.na(df$income_group) & !is.na(df$life_expectancy), ]
  
  # --- Violin Plot: Life Expectancy by Income Group ---
  ggplot(df_violin, aes(x = income_group, y = life_expectancy, fill = income_group)) +
    geom_violin(trim = FALSE) +                   # Show full distribution
    scale_fill_brewer(palette = "Set2") +        # Similar to Seaborn's Set2
    labs(title = "Life Expectancy by Income Group",
         x = "Income Group",
         y = "Life Expectancy (years)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  # --- Calculate average Digital Connectivity Index (DCI) by region ---
  avg_dci <- df %>%
    group_by(region) %>%
    summarise(mean_dci = mean(digital_connectivity_index, na.rm = TRUE)) %>%
    arrange(mean_dci)  # Optional: sort by DCI for horizontal bar plot
  
  # --- Horizontal Bar Plot ---
  ggplot(avg_dci, aes(x = mean_dci, y = reorder(region, mean_dci), fill = mean_dci)) +
    geom_bar(stat = "identity") +
    scale_fill_viridis_c() +  # Similar to Seaborn 'viridis' palette
    labs(title = "Average Digital Connectivity Index by Region",
         x = "Digital Connectivity Index",
         y = "Region") +
    theme_minimal()
  
  # --- Find the latest year ---
  latest_year <- max(df$year, na.rm = TRUE)
  
  # --- Top 10 countries by GDP in the latest year ---
  top_gdp <- df %>%
    filter(year == latest_year) %>%
    arrange(desc(gdp_usd)) %>%
    slice_head(n = 10)
  
  # --- Interactive Plotly bar plot ---
  fig <- plot_ly(top_gdp,
                 x = ~country_name,
                 y = ~gdp_usd,
                 type = 'bar',
                 color = ~country_name,
                 colors = "Set2") %>%
    layout(title = paste0("Top 10 Countries by GDP in ", latest_year),
           xaxis = list(title = "Country", tickangle = -45),
           yaxis = list(title = "GDP (USD)"),
           showlegend = FALSE)
  
  fig
  
  
  # --- Select relevant columns for correlation ---
  corr_cols <- c('gdp_usd', 'population', 'gdp_per_capita', 'inflation_rate', 'unemployment_rate',
                 'co2_emissions_kt', 'renewable_energy_pct', 'life_expectancy', 'internet_usage_pct',
                 'mobile_subscriptions_per_100', 'human_development_index')
  
  # Subset the data and calculate correlation matrix
  corr_data <- df[, corr_cols]
  corr_matrix <- cor(corr_data, use = "complete.obs")
  
  # Melt the correlation matrix for ggplot2
  corr_melt <- melt(corr_matrix)
  
  ggplot(corr_melt, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                         limit = c(-1,1), space = "Lab", name="Correlation") +
    geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 3) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(title = "Correlation Matrix of Key Economic and Social Indicators",
         x = "", y = "")
  
  
  # Install pheatmap (run once)
  install.packages("pheatmap")
  
  # Then load it
  library(pheatmap)
  
  # --- Select relevant numeric columns for clustering ---
  cluster_cols <- c(
    'gdp_usd', 'gdp_per_capita', 'population', 'life_expectancy',
    'internet_usage_pct', 'human_development_index',
    'renewable_energy_pct', 'co2_emissions_per_capita_tons',
    'digital_connectivity_index', 'education_health_ratio'
  )
  
  # --- Subset data and remove rows with NA ---
  cluster_data <- df[, cluster_cols]
  cluster_data <- cluster_data[complete.cases(cluster_data), ]
  
  # --- Compute correlation matrix ---
  corr_matrix <- cor(cluster_data)
  
  # --- Clean Cluster Heatmap ---
  pheatmap(
    corr_matrix,
    clustering_method = "ward.D2",                  # Ward's method for hierarchical clustering
    color = colorRampPalette(c("blue", "white", "red"))(50),  # Color palette similar to 'vlag'
    display_numbers = FALSE,                        # Remove numeric annotations for clarity
    main = "Cluster Heatmap of Development Indicators",
    fontsize_row = 10,
    fontsize_col = 10
  )
 
  library(tidyr) 
  
  # --- Filter necessary columns ---
  index_cols <- c(
    'year', 'country_name', 'region',
    'human_development_composite',
    'global_resilience_score',
    'governance_quality_index',
    'digital_connectivity_index',
    'global_development_resilience_index'
  )
  
  df_index <- df[, index_cols]
  
  # Remove rows with NA in any of the selected columns
  df_index <- df_index[complete.cases(df_index), ]
  
  # --- Compute global average per year ---
  index_grouped <- df_index %>%
    group_by(year) %>%
    summarise(
      human_development_composite = mean(human_development_composite, na.rm = TRUE),
      global_resilience_score = mean(global_resilience_score, na.rm = TRUE),
      governance_quality_index = mean(governance_quality_index, na.rm = TRUE),
      digital_connectivity_index = mean(digital_connectivity_index, na.rm = TRUE),
      global_development_resilience_index = mean(global_development_resilience_index, na.rm = TRUE)
    )
  
  # --- Convert to long format for ggplot2 ---
  index_long <- index_grouped %>%
    pivot_longer(cols = -year, names_to = "Index", values_to = "Value")
  
  # --- Line plot ---
  ggplot(index_long, aes(x = year, y = Value, color = Index)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = "Index Trends Over Time (Global Average)",
         x = "Year",
         y = "Index Score") +
    theme_minimal() +
    theme(legend.position = "bottom")