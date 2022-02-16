
# mutate(value = na_if(value, "999999999999")) %>% # moved to data cleaning script

checkSMdata <- function(df){
  # report back any possible problems with the data



}





remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


repair_limits <- function(df){
  if(!is.data.frame(df)) stop("The input provided is not a dataframe")
  if(is.null(df$valuesign)) stop("There appears to be no column named 'valuesign'")
    df <- df %>%
      mutate(originalvalue = paste(valuesign, value)) %>%
      mutate(value = case_when(
        is.na(valuesign) ~ value,
        valuesign == "=" ~ value,
        valuesign == "<" ~ 0.5 * value,
        valuesign == ">" ~ NA_real_)) %>%
      select(-valuesign)
}


