# Load and inspect

load_and_merge <- function(folder_name) {
  # setup file directory and names
  data_path <- paste0(getwd(), "/", folder_name)
  file_paths <- dir_ls(data_path, glob = "*.csv")
  
  # load data in list
  data <- file_paths %>%
    map_dfr(read_csv, .id = "source") %>%
    mutate(source = str_replace(source, pattern = data_path, "")) %>%
    select(-id)
  
  return(data)
}


inspect_dimensionality <- function(df) {
  dimensions <- dim(df)
  print(paste("The data has", dimensions[1], "rows and", dimensions[2], "columns."))
  print("----")
}


inspect_data_type <- function(df) {
  print(sapply(df, class))
  print("----")
}


inspect_missings <- function(df) {
  missing_count <- df %>%
    map(~sum(is.na(.)))
  print(missing_count)
  print("----")
}


inspect_unique_categorical_count <- function(df) {
  unique_cat_plot <- df %>%
    select(where(is.character)) %>%
    map_df(~n_distinct(., na.rm = TRUE)) %>%
    mutate(pivot_placeholder = "pivot_placeholder") %>%
    pivot_longer(!pivot_placeholder, names_to = "feature", values_to = "unique_categorical_count") %>%
    ggplot(mapping = aes(x = feature, y = unique_categorical_count, fill = feature)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = unique_categorical_count), position=position_dodge(width=0.9), vjust=-0.25) +
      ylab("Count of unique categorical variables") +
      xlab("Feature") +
      theme_bw()
  
  return(unique_cat_plot)
}

