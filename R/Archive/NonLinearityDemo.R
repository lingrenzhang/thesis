load("cleaned_data.RData")

printLogIntensity <- function(stategroup, industry) {
  filter_idx = which(data$stategroup == stategroup & data$industry == industry)
  filtered_data = data[filter_idx,]
  log_intensity = log(sum(filtered_data$default) / sum(filtered_data$term))
  cat(sprintf("state group = %s, industry = %s, sample size = %d, log intensity = %f", 
              stategroup, industry, nrow(filtered_data), log_intensity))
}

printLogIntensity("Great Lakes", "Construction")
printLogIntensity("Far West", "Construction")
printLogIntensity("Great Lakes", "Retail Trade")
printLogIntensity("Far West", "Retail Trade")
