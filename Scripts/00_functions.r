

########################
# HTML table function
########################

## For cleaning/processing note outputs
html_table <- function(data, caption = "") {
  if (nrow(data) == 0) {
    cat("<p>No data available</p>\n")
    return()
  }
  
  cat("<table>\n")
  if (caption != "") {
    cat(paste0("<caption>", caption, "</caption>\n"))
  }
  
  # Header
  cat("<tr>")
  for (col in names(data)) {
    cat(paste0("<th>", col, "</th>"))
  }
  cat("</tr>\n")
  
  # Rows
  for (i in 1:nrow(data)) {
    cat("<tr>")
    for (j in 1:ncol(data)) {
      cell_value <- data[[j]][i]  # Use [[j]][i] instead of [i,j]
      cat(paste0("<td>", cell_value, "</td>"))
    }
    cat("</tr>\n")
  }
  
  cat("</table>\n<br>\n")
}

########################
# Count select multiple
########################

## For dealing with select_multiple ODK variables that haven't been parsed
count_select_multiple <- function(data, column, filter_condition = NULL) {
  # Apply filter if provided
  if (!is.null(filter_condition)) {
    filtered_data <- data %>% filter(!!filter_condition)
  } else {
    filtered_data <- data
  }
  
  # Get the column data
  responses <- filtered_data[[column]]
  
  # Remove NA values and empty strings
  responses <- responses[!is.na(responses) & responses != ""]
  
  # Split each response by spaces and create a vector of all individual choices
  all_choices <- unlist(strsplit(responses, " "))
  
  # Count each choice and create summary table
  choice_counts <- table(all_choices)
  
  # Convert to data frame and arrange by count (descending)
  summary_df <- data.frame(
    Response = names(choice_counts),
    Count = as.numeric(choice_counts),
    Percentage = round(as.numeric(choice_counts) / length(responses) * 100, 1)
  ) %>%
    arrange(desc(Count))
  
  return(summary_df)
}

########################
# Rename If Exists
########################

## Flexible rename (won't crash if variable not in data)

rename_if_exists <- function(data, mapping) {
  mapping <- mapping[mapping %in% names(data)]
  rename(data, !!!mapping)
}


########################
# Summary table function
########################

create_summary_table <- function(data, ..., sort = TRUE) {
  data %>%
    count(..., sort = sort) %>%
    mutate(percent = round(n/sum(n)*100, 1))
}

########################
# Price statistics function (for RDT script)
########################
calc_price_stats <- function(data, price_var, exclude_values = c(-9777, -9888)) {
  data %>%
    filter(!.data[[price_var]] %in% exclude_values) %>%
    summarise(
      n = n(),
      mean_price = round(mean(.data[[price_var]], na.rm = TRUE), 2),
      median_price = median(.data[[price_var]], na.rm = TRUE),
      q25 = quantile(.data[[price_var]], 0.25, na.rm = TRUE),
      q75 = quantile(.data[[price_var]], 0.75, na.rm = TRUE),
      min_price = min(.data[[price_var]], na.rm = TRUE),
      max_price = max(.data[[price_var]], na.rm = TRUE)
    )
}

########################
# Text cleaning function 
########################

clean_text_field <- function(x) {
  x %>%
    str_to_upper() %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    str_replace_all("\t", " ") %>%
    str_replace_all("  +", " ") %>%
    str_replace_all("\\.", "") %>%
    str_replace_all(",", "") %>%
    str_replace_all("LIMITED", "LTD") %>%
    str_replace_all("PRIVATE", "PVT") %>%
    str_trim()
}


########################
# AI ingredient labeling function (anti-malarial script)
########################

create_ai_labels <- function(ai_col) {
  case_when(
    ai_col == 60 ~ "Amodiaquine",
    ai_col == 61 ~ "Artemether", 
    ai_col == 62 ~ "Artemisinin",
    ai_col == 63 ~ "Arteether",
    ai_col == 64 ~ "Artemotil",
    ai_col == 65 ~ "Artesunate",
    ai_col == 66 ~ "Atovaquone",
    ai_col == 67 ~ "Chloroproguanil",
    ai_col == 68 ~ "Chloroquine",
    ai_col == 69 ~ "Arterolane",
    ai_col == 70 ~ "Dapsone",
    ai_col == 71 ~ "Dihydroartemisinin",
    ai_col == 72 ~ "Halofantrine",
    ai_col == 73 ~ "Hydroxychloroquine",
    ai_col == 74 ~ "Lumefantrine",
    ai_col == 75 ~ "Mefloquine",
    ai_col == 76 ~ "Naphthoquine",
    ai_col == 77 ~ "Piperaquine",
    ai_col == 78 ~ "Primaquine",
    ai_col == 79 ~ "Proguanil",
    ai_col == 80 ~ "Pyronaridine",
    ai_col == 81 ~ "Pyrimethamine",
    ai_col == 82 ~ "Quinacrine",
    ai_col == 83 ~ "Quinine",
    ai_col == 85 ~ "Sulfadoxine",
    ai_col == 86 ~ "Sulfamethoxazole",
    ai_col == 87 ~ "Sulfamethoxypyrazine",
    ai_col == 88 ~ "Trimethoprim",
    ai_col == 89 ~ "Sulfalene",
    ai_col == 96 ~ "Other",
    ai_col == 98 ~ "Don't know",
    TRUE ~ ""
  )
}


########################
# Analysis indicator function to pull infor from indicator config  
########################
extract_indicator_info <- function(rmd_file) { # NEED TO ADD TO FUNCTIONS SCRIPT 
  lines <- readLines(rmd_file)
  
  # Find indicator_id (handles spaces around =)
  id_line <- grep('indicator_id\\s*=\\s*"', lines, value = TRUE)
  indicator_id <- gsub('.*indicator_id\\s*=\\s*"([^"]+)".*', "\\1", id_line[1])
  
  # Find file_suffix (handles spaces around =)
  suffix_line <- grep('file_suffix\\s*=\\s*"', lines, value = TRUE)
  file_suffix <- gsub('.*file_suffix\\s*=\\s*"([^"]+)".*', "\\1", suffix_line[1])
  
  return(list(indicator_id = indicator_id, file_suffix = file_suffix))
}