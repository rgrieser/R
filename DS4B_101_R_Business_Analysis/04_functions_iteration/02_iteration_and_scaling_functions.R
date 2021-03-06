# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# ITERATION WITH PURRR ----

library(readxl)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(broom)

bike_orderlines_tbl <- read_rds("Desktop/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)



# 1.0 PRIMER ON PURRR ----
# Programmatically getting Excel files into R
excel_paths_tbl <- fs::dir_info("Desktop/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_raw/")

excel_paths_tbl

paths_chr <- excel_paths_tbl %>%
    pull(path)

paths_chr

# What Not To Do: Don't use for loops
excel_list <- list()
for (path in paths_chr) {
    excel_list[[path]] <- read_excel(path)
}

excel_list

# What to Do: Use map()
?map

# Method 1: function name
excel_list_2 <- paths_chr %>%
    map(read_excel) %>%
    set_names(paths_chr)

# Method 2 Anonymous function
paths_chr %>%
    map(~ read_excel(.))

# Method 3: Function specified with function()
paths_chr %>%
    map(function(x) read_excel(path = x))




# Reading Excel Sheets

excel_sheets("Desktop/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_raw/bikes.xlsx") %>%
    map(~ read_excel(path = "Desktop/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_raw/bikes.xlsx", sheet = .))


# 2.0 MAPPING DATA FRAMES ----

# 2.1 Column-wise Map ----
bike_orderlines_tbl %>% is.list()

bike_orderlines_tbl %>%
    map(~ class(.)[1])

# 2.2 Map Variants ----

?map

# Character Map
bike_orderlines_tbl %>%
    map_chr(~ class(.)[1])

# Data Frame Map
bike_orderlines_tbl %>%
    map_df(~ class(.)[1]) %>%
    gather()

bike_orderlines_tbl %>%
    map_df(~ sum(is.na(.)) / length(.)) %>%
    gather()

# 2.3 Row-wise Map ----

excel_tbl <- excel_paths_tbl %>%
    select(path) %>%
    mutate(data = path %>% map(read_excel))

excel_list

excel_tbl

# 3.0 NESTED DATA ----

# Unnest
excel_tbl

excel_tbl$data

excel_tbl$data[[3]]

excel_tbl_unnested <- excel_tbl %>%
    unnest(data, .id = "ID") 

# Nest

excel_tbl_nested <- excel_tbl_unnested %>%
    group_by(ID, path) %>%
    nest()

excel_tbl_nested$data

# Mapping Nested List Columns

x <- rep(NA, 5)
x

is.na(x) %>% all()

y <- c(1:4, NA_real_)
y

is.na(y) %>% all()

excel_tbl_nested$data[[1]] %>%
    select_if(~ !is.na(.) %>% all())

excel_tbl_nested

# Method 1: Creating a function outside of purrr::map()

select_non_na_columns <- function(data) {
    
    data %>%
        select_if(~!is.na(.) %>% all())
    
}

excel_tbl_nested$data[[2]] %>%
    select_non_na_columns()

excel_tbl_nested_fixed <- excel_tbl_nested %>%
    mutate(data_fixed = data %>% map(select_non_na_columns))

excel_tbl_nested_fixed$data[[1]]

# 4.0 MODELING WITH PURRR ----


# 4.1 Time Series Plot ----
#  - What if we wanted to approximate the 3 month rolling average with a line?
#  - We can use a smoother

# Code comes from 04_functions_iteration/01_functional_programming
rolling_avg_3_tbl <- bike_orderlines_tbl %>%
    select(order_date, category_1, category_2, total_price) %>%
    
    mutate(order_date = ymd(order_date)) %>%
    mutate(month_end = ceiling_date(order_date, unit = "month") - period(1, unit = "days")) %>%
    
    group_by(category_1, category_2, month_end) %>%
    summarise(
        total_price = sum(total_price)
    ) %>%
    mutate(rolling_avg_3 = rollmean(total_price, k = 3, na.pad = TRUE, align = "right")) %>%
    ungroup() %>%
    
    mutate(category_2 = as_factor(category_2) %>% fct_reorder2(month_end, total_price)) 

rolling_avg_3_tbl %>%
    
    ggplot(aes(month_end, total_price, color = category_2)) +
    
    # Geometries
    geom_point() +
    geom_line(aes(y = rolling_avg_3), color = "blue", linetype = 1) +
    facet_wrap(~ category_2, scales = "free_y") +
    
    # Add Loess Smoother
    geom_smooth(method = "loess", se = FALSE, span = 0.2, color = "black") +
    
    # Formatting
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K"))




# 4.2 Modeling Primer ----

# Data Preparation



# Making a loess model



# Working With Broom


    
    
# Visualizing results
    




# 4.3 Function To Return Fitted Results ----




# 4.4 Test Function on Single Element ----


# 4.5 Map Function to All Categories ----

# Map Functions



# Visualize Results



