# Excel export function (outlets as columns with merged headers)
create_excel_output <- function(results_list, footnotes_list, indicator_config, 
                                country, year,
                                include_antimalarial_col = TRUE) {
  
  # Create Excel output file path
  excel_file <- here("Results", paste0("Indicator ", indicator_config$indicator_id), 
                     paste0("Indicator ", indicator_config$indicator_id, "_", country, "_", year, ".xlsx"))
  
  dir.create(dirname(excel_file), showWarnings = FALSE, recursive = TRUE)
  
  excel_success <- tryCatch({
    wb <- createWorkbook()
    
    # Styles
    header_style <- createStyle(fontColour = "white", fgFill = "#4F81BD", halign = "center",
                                textDecoration = "bold", border = "TopBottomLeftRight")
    subheader_style <- createStyle(fontColour = "white", fgFill = "#8DB4E2", halign = "center",
                                   textDecoration = "bold", border = "TopBottomLeftRight")
    data_style <- createStyle(border = "TopBottomLeftRight", halign = "center")
    text_style <- createStyle(border = "TopBottomLeftRight", halign = "left")
    number_style <- createStyle(numFmt = "0.00", border = "TopBottomLeftRight", halign = "center")
    
    for (i in seq_along(results_list)) {
      sheet_name <- names(results_list)[i]
      result_data <- results_list[[i]] %>% 
        arrange(across(any_of(c("strata1", "strata2"))))
      
      if (is.null(result_data) || nrow(result_data) == 0) next
      
      addWorksheet(wb, sheet_name)
      
      # Add title (A1)
      writeData(wb, sheet_name, sheet_name, startCol = 1, startRow = 1)
      
      # Get available outlet types for this sheet
      available_outlets <- unique(result_data$outlet_label)
      available_outlets <- available_outlets[!is.na(available_outlets)]
      
      # Determine if we have strata columns
      has_antimalarial_col <- include_antimalarial_col && "antimalarial_label" %in% names(result_data)
      has_strata1_col <- "strata1" %in% names(result_data)
      has_strata2_col <- "strata2" %in% names(result_data)
      
      # Reshape data to wide format by outlet type
      if (has_antimalarial_col) {
        # For indicators like 2.1 with antimalarial products
        wide_data <- result_data %>%
          select(antimalarial_label, outlet_label, point_estimate, lower_ci, upper_ci,
                 dplyr::any_of(c("strata1", "strata2"))) %>%
          pivot_longer(cols = c(point_estimate, lower_ci, upper_ci), 
                       names_to = "metric", values_to = "value") %>%
          mutate(
            metric = case_when(
              metric == "point_estimate" ~ "Point",
              metric == "lower_ci" ~ "Lower",
              metric == "upper_ci" ~ "Upper"
            )
          ) %>%
          unite("outlet_metric", outlet_label, metric, sep = "_") %>%
          pivot_wider(names_from = outlet_metric, values_from = value, values_fill = 0)
      } else {
        # For indicators like 1.1 without antimalarial column
        if (has_strata1_col || has_strata2_col) {
          wide_data <- result_data %>%
            select(outlet_label, point_estimate, lower_ci, upper_ci,
                   dplyr::any_of(c("strata1", "strata2"))) %>%
            pivot_longer(cols = c(point_estimate, lower_ci, upper_ci), 
                         names_to = "metric", values_to = "value") %>%
            mutate(
              metric = case_when(
                metric == "point_estimate" ~ "Point",
                metric == "lower_ci" ~ "Lower", 
                metric == "upper_ci" ~ "Upper"
              )
            ) %>%
            unite("outlet_metric", outlet_label, metric, sep = "_") %>%
            pivot_wider(names_from = outlet_metric, values_from = value, values_fill = 0)
        } else {
          # National level - simpler structure
          wide_data <- result_data %>%
            select(outlet_label, point_estimate, lower_ci, upper_ci) %>%
            pivot_longer(cols = c(point_estimate, lower_ci, upper_ci), 
                         names_to = "metric", values_to = "value") %>%
            mutate(
              metric = case_when(
                metric == "point_estimate" ~ "Point",
                metric == "lower_ci" ~ "Lower",
                metric == "upper_ci" ~ "Upper"
              ),
              row_id = 1  # Single row for national
            ) %>%
            unite("outlet_metric", outlet_label, metric, sep = "_") %>%
            pivot_wider(names_from = outlet_metric, values_from = value, values_fill = 0)
        }
      }
      
      # Build headers
      headers_row1 <- character(0)
      headers_row2 <- character(0)
      
      # Add antimalarial column if present (for 2.1)
      if (has_antimalarial_col) {
        headers_row1 <- c(headers_row1, "Antimalarial")
        headers_row2 <- c(headers_row2, "")
      }
      
      # Add strata columns if present
      if (has_strata1_col) {
        headers_row1 <- c(headers_row1, "Strata1")
        headers_row2 <- c(headers_row2, "")
      }
      if (has_strata2_col) {
        headers_row1 <- c(headers_row1, "Strata2") 
        headers_row2 <- c(headers_row2, "")
      }
      
      # Add outlet type headers (merged across 3 columns each)
      for (outlet in available_outlets) {
        # Main outlet header (will be merged across 3 columns)
        headers_row1 <- c(headers_row1, outlet, "", "")
        # Sub-headers for metrics
        headers_row2 <- c(headers_row2, "Point", "Lower", "Upper")
      }
      
      # Write main headers (row 3)
      writeData(wb, sheet_name, t(headers_row1), startCol = 1, startRow = 3, colNames = FALSE)
      addStyle(wb, sheet_name, header_style, rows = 3, cols = 1:length(headers_row1), gridExpand = TRUE)
      
      # Write sub-headers (row 4)
      writeData(wb, sheet_name, t(headers_row2), startCol = 1, startRow = 4, colNames = FALSE)
      addStyle(wb, sheet_name, subheader_style, rows = 4, cols = 1:length(headers_row2), gridExpand = TRUE)
      
      # Merge cells for outlet type headers
      strata_cols <- sum(has_antimalarial_col, has_strata1_col, has_strata2_col)
      merge_start_col <- strata_cols + 1
      for (outlet in available_outlets) {
        mergeCells(wb, sheet_name, cols = merge_start_col:(merge_start_col + 2), rows = 3)
        merge_start_col <- merge_start_col + 3
      }
      
      # Prepare data for writing
      data_start_row <- 5
      
      # Order columns correctly for the wide format
      left_col_names <- character(0)
      if (has_antimalarial_col) left_col_names <- c(left_col_names, "antimalarial_label")
      if (has_strata1_col) left_col_names <- c(left_col_names, "strata1")
      if (has_strata2_col) left_col_names <- c(left_col_names, "strata2")
      
      # Build column order: left columns, then outlet metrics
      outlet_metric_cols <- character(0)
      for (outlet in available_outlets) {
        outlet_metric_cols <- c(outlet_metric_cols,
                                paste0(outlet, "_Point"),
                                paste0(outlet, "_Lower"),
                                paste0(outlet, "_Upper"))
      }
      
      # Apply custom sorting if antimalarial data exists
      if (has_antimalarial_col && "antimalarial_order" %in% names(indicator_config)) {
        wide_data <- wide_data %>%
          arrange(factor(antimalarial_label, levels = indicator_config$antimalarial_order))
      }
      
      final_data <- wide_data %>%
        select(all_of(left_col_names), all_of(outlet_metric_cols)) %>%
        mutate(across(where(is.numeric), ~replace_na(., 0)))
      
      writeData(wb, sheet_name, final_data, startCol = 1, startRow = data_start_row, colNames = FALSE)
      
      # Apply styles with FIXED text_cols calculation
      text_cols <- if(length(left_col_names) > 0) 1:length(left_col_names) else integer(0)
      
      if (length(text_cols) > 0) {
        addStyle(wb, sheet_name, text_style,
                 rows = data_start_row:(data_start_row + nrow(final_data) - 1),
                 cols = text_cols, gridExpand = TRUE)
      }
      
      num_cols <- (length(text_cols) + 1):ncol(final_data)
      if (length(num_cols) > 0) {
        addStyle(wb, sheet_name, number_style,
                 rows = data_start_row:(data_start_row + nrow(final_data) - 1),
                 cols = num_cols, gridExpand = TRUE)
      }
      
      # Add footnote in C1
      footnote_text <- if(is.list(footnotes_list[[i]])) {
        # Handle list footnotes properly
        list_names <- names(footnotes_list[[i]])
        if(!is.null(list_names)) {
          paste(paste(list_names, footnotes_list[[i]], sep = ": "), collapse = "; ")
        } else {
          paste(unlist(footnotes_list[[i]]), collapse = "; ")
        }
      } else {
        as.character(footnotes_list[[i]])
      }
      writeData(wb, sheet_name, footnote_text, startCol = 3, startRow = 1)
      
      # Auto-fit columns
      for (col in 1:ncol(final_data)) {
        if (col <= length(text_cols)) {
          # Text columns - wider for readability
          setColWidths(wb, sheet_name, cols = col, widths = 20)
        } else {
          # Number columns - narrower
          setColWidths(wb, sheet_name, cols = col, widths = 10)
        }
      }
      
      # Freeze header rows
      freezePane(wb, sheet_name, firstActiveRow = data_start_row)
    }
    
    # ===== ADD LONG FORMAT SHEET =====
    # Get the most granular level available:
    # - If cross-strata exists (sheets starting with "cross_"), combine all of them
    # - Otherwise use strata2 if it exists
    # - Otherwise use strata1 if it exists
    # - Otherwise use national
    
    cross_sheets <- names(results_list)[grepl("^cross_", names(results_list))]
    
    if (length(cross_sheets) > 0) {
      # Combine all cross-strata sheets
      long_data <- bind_rows(results_list[cross_sheets])
    } else if ("strata2" %in% names(results_list)) {
      long_data <- results_list[["strata2"]]
    } else if ("strata1" %in% names(results_list)) {
      long_data <- results_list[["strata1"]]
    } else {
      long_data <- results_list[["national"]]
    }
    
    # Reorder columns to put strata columns first
    long_data <- long_data %>%
      select(any_of(c("strata1", "strata2")), everything())
    
    if (!is.null(long_data) && nrow(long_data) > 0) {
      addWorksheet(wb, "Long Format")
      
      # Write with auto-formatting
      writeData(wb, "Long Format", long_data, startRow = 1, startCol = 1)
      
      # Apply header style to first row
      addStyle(wb, "Long Format", header_style, rows = 1, 
               cols = 1:ncol(long_data), gridExpand = TRUE)
      
      # Auto-fit columns
      setColWidths(wb, "Long Format", cols = 1:ncol(long_data), widths = "auto")
      
      # Freeze first row
      freezePane(wb, "Long Format", firstRow = TRUE)
    }
    
    saveWorkbook(wb, excel_file, overwrite = TRUE)
    TRUE
  }, error = function(e) {
    message("Error writing Excel: ", e$message)
    FALSE
  })
  
  return(excel_success)
}
