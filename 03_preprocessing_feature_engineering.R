# Preprocessing and Feature Engineering

convert_to_factor <- function(df) {
  df_converted <- df %>%
    mutate_if(is.character, factor)
  
  return(df_converted)
}


add_totals <- function(df) {
  df <- df %>%
    mutate(
      # calculate total minutes spent on calls
      total_minutes = total_day_minutes + total_eve_minutes + total_night_minutes + total_intl_minutes,
      # calculate total number of calls
      total_calls = total_day_calls + total_eve_calls + total_night_calls + total_intl_calls,
      # calculate total money charged
      total_charge = total_day_charge + total_eve_charge + total_night_charge + total_intl_charge
    )
  
  return(df)
}


add_charge_and_minutes_per_call <- function(df) {
  df <- df %>%
    mutate(
      # calculate minutes per day/evening/night/international/total calls (i.e average minutes per call-type)
      minutes_per_day_call = total_day_minutes / total_day_calls,
      minutes_per_eve_call = total_eve_minutes / total_eve_calls,
      minutes_per_night_call = total_night_minutes / total_night_calls,
      minutes_per_intl_call = total_intl_minutes / total_intl_calls,
      minutes_per_total_call = total_minutes / total_calls,
      # calculate charge per day/evening/night/international/total calls (i.e. average charge per call-type)
      charge_per_day_call = total_day_charge / total_day_calls,
      charge_per_eve_call = total_eve_charge / total_eve_calls,
      charge_per_night_call = total_night_charge / total_night_calls,
      charge_per_intl_call = total_intl_charge / total_intl_calls,
      charge_per_total_call = total_charge / total_calls,
      # calculate charge per day/evening/night/international/total minutes (i.e. average charge per minute)
      avg_price_per_day_minute = total_day_charge / total_day_minutes,
      avg_price_per_eve_minute = total_eve_charge / total_eve_minutes,
      avg_price_per_night_minute = total_night_charge / total_night_minutes,
      avg_price_per_intl_minute = total_intl_charge / total_intl_minutes,
      avg_price_per_total_minute = total_charge / total_minutes
    )
  
  return(df)
}


add_utilization_and_charge_per_membership_length <- function(df) {
  df <- df %>%
    mutate(
      # calculate minutes (day/eve/night/intl/total) per membership length (i.e. average minute utilization per month)
      avg_minutes_day_per_month = total_day_minutes / account_length,
      avg_minutes_eve_per_month = total_eve_minutes / account_length,
      avg_minutes_night_per_month = total_night_minutes / account_length,
      avg_minutes_intl_per_month = total_intl_minutes / account_length,
      avg_minutes_total_per_month = total_minutes / account_length,
      # calculate calls (day/eve/night/intl/total) per membership length (i.e. average call utilization per month)
      avg_calls_day_per_month = total_day_calls / account_length,
      avg_calls_eve_per_month = total_eve_calls / account_length,
      avg_calls_night_per_month = total_night_calls / account_length,
      avg_calls_intl_per_month = total_intl_calls / account_length,
      avg_calls_total_per_month = total_calls / account_length,
      # calculate calls (day/eve/night/intl/total) per membership length (i.e. average call utilization per month)
      avg_charge_day_per_month = total_day_charge / account_length,
      avg_charge_eve_per_month = total_eve_charge / account_length,
      avg_charge_night_per_month = total_night_charge / account_length,
      avg_charge_intl_per_month = total_intl_charge / account_length,
      avg_charge_total_per_month = total_charge / account_length
    )
  
  return(df)
}














