AppendTSAverages <- function(data, dataExpanded) {
  toAppend <- aggregate(
    cbind(dataExpanded$unemployment, dataExpanded$google_trend, dataExpanded$sp500_1yr, dataExpanded$cs_housing_1yr, dataExpanded$unemployment_by_state), 
    by = list(dataExpanded$index), FUN = mean)
  #this assumes that data$index is sorted, which is true in this case
  data$avg_unemployment <- toAppend$V1
  data$avg_google_trend <- toAppend$V2
  data$avg_sp500_1yr <- toAppend$V3
  data$avg_cs_housing_1yr <- toAppend$V4
  data$avg_unemployment_by_state <- toAppend$V5
  data
}

PrintCoef <- function(coefficients, filename) {
  fileConn <- file(filename)
  lines <- c('Feature & Coefficient & p-value \\\\ \\hline');
  
  rows <- rownames(coefficients);
  for(i in 1:length(rows)) {
    p_value <- coefficients[i,4]
    p_value_display <- as.character(round(p_value,4))
    if(p_value < 0.01) {
      p_value_display <- format(p_value, digits = 3, scientific = TRUE)
    }
    if(p_value < 2e-16) {
      p_value_display <- "\\textless 2e-16"
    }
    lines <- c(lines, paste(
      GetDisplayName(rows[i]), ' & ',
      as.character(round(coefficients[i,1],4)), ' & ',
      p_value_display, ' \\\\ \\hline',
      sep = ''))
  }
  
  writeLines(lines, fileConn)
  close(fileConn)  
}

PrintTables <- function(models, tag) {
  PrintCoef(summary(models$social_bernoulli)$coefficients, paste('Tables\\Social_Binary_', tag, '.txt', seq=''))  
  PrintCoef(summary(models$social_monthly)$coefficients, paste('Tables\\Social_Monthly_', tag, '.txt', seq=''))  
  PrintCoef(summary(models$econ_bernoulli)$coefficients, paste('Tables\\Econ_Binary_', tag, '.txt', seq=''))  
  PrintCoef(summary(models$econ_monthly)$coefficients, paste('Tables\\Econ_Monthly_', tag, '.txt', seq=''))  
  PrintCoef(summary(models$hybrid_bernoulli)$coefficients, paste('Tables\\Hybrid_Binary_', tag, '.txt', seq=''))
  PrintCoef(summary(models$hybrid_monthly)$coefficients, paste('Tables\\Hybrid_Monthly_', tag, '.txt', seq=''))  
}

GetSelectedModelsBIC <- function(BernoulliInSample, MonthlyInSample) {
  mod.social.bernoulli <- glm(default ~ unemployment,
                              family = "binomial",
                              data = BernoulliInSample)
  mod.social.monthly <- glm(default ~ unemployment + 
                              log(adjustedfollowercount+1) +
                              log(adjustedfriendcount+1),
                            family = "binomial",
                            data = MonthlyInSample)
  mod.econ.bernoulli <- glm(default ~ log(grossamountinthousand) + 
                              guaranteedratio + 
                              terminmonths + 
                              borrowerbanksamestate + 
                              termisfullyear + 
                              unemployment_by_state,
                            family = "binomial",
                            data = BernoulliInSample)
  mod.econ.monthly <- glm(default ~ log(grossamountinthousand) + 
                            guaranteedratio + 
                            terminmonths + 
                            borrowerbanksamestate + 
                            termisfullyear + 
                            stategroup + 
                            unemployment_by_state +
                            cs_housing_1yr,
                          family = "binomial",
                          data = MonthlyInSample)
  mod.hybrid.bernoulli <- glm(default ~ log(grossamountinthousand) + 
                                guaranteedratio + 
                                borrowerbanksamestate + 
                                termisfullyear +
                                unemployment,
                              family = "binomial",
                              data = BernoulliInSample)
  mod.hybrid.monthly <- glm(default ~ log(grossamountinthousand) + 
                              terminmonths + 
                              borrowerbanksamestate + 
                              termisfullyear + 
                              unemployment_by_state +
                              cs_housing_1yr +
                              unemployment,
                            family = "binomial",
                            data = MonthlyInSample)
  retval <- list(social_bernoulli = mod.social.bernoulli,
                 social_monthly   = mod.social.monthly,
                 econ_bernoulli   = mod.econ.bernoulli,
                 econ_monthly     = mod.econ.monthly,
                 hybrid_bernoulli = mod.hybrid.bernoulli,
                 hybrid_monthly   = mod.hybrid.monthly)
  retval
}

GetSelectedModelsAICBigArchive <- function(BernoulliInSample, MonthlyInSample) {
  mod.social.bernoulli <- bigglm(default ~ unemployment + 
                                   log(adjustedfollowercount + 1),
                                 family = binomial(),
                                 data = BernoulliInSample)
  mod.social.monthly <- bigglm(default ~ unemployment + 
                                 log(adjustedfollowercount+1) +
                                 log(adjustedfriendcount+1),
                               family = binomial(),
                               data = MonthlyInSample)
  mod.econ.bernoulli <- bigglm(default ~ log(grossamountinthousand) + 
                                 guaranteedratio + 
                                 terminmonths + 
                                 borrowerbanksamestate + 
                                 termisfullyear + 
                                 stategroup +
                                 unemployment_by_state +
                                 cs_housing_1yr,
                               family = binomial(),
                               data = BernoulliInSample)
  mod.econ.monthly <- bigglm(default ~ log(grossamountinthousand) + 
                               guaranteedratio + 
                               terminmonths + 
                               borrowerbanksamestate + 
                               termisfullyear + 
                               stategroup + 
                               unemployment_by_state +
                               gdp_by_state_diff +
                               gdp_by_industry_diff +                            
                               sp500_1yr +
                               cs_housing_1yr,
                             family = binomial(),
                             data = MonthlyInSample)
  mod.hybrid.bernoulli <- bigglm(default ~ log(grossamountinthousand) + 
                                   guaranteedratio + 
                                   terminmonths +
                                   borrowerbanksamestate + 
                                   termisfullyear +
                                   gdp_by_industry_diff +
                                   sp500_1yr +
                                   unemployment,
                                 family = binomial(),
                                 data = BernoulliInSample)
  mod.hybrid.monthly <- bigglm(default ~ log(grossamountinthousand) + 
                                 guaranteedratio +
                                 terminmonths + 
                                 borrowerbanksamestate + 
                                 termisfullyear + 
                                 stategroup +
                                 unemployment_by_state +
                                 gdp_by_state_diff +
                                 gdp_by_industry_diff +                            
                                 sp500_1yr +
                                 cs_housing_1yr +
                                 unemployment +
                                 google_trend +
                                 log(adjustedfollowercount + 1),
                               family = binomial(),
                               data = MonthlyInSample)
  retval <- list(social_bernoulli = mod.social.bernoulli,
                 social_monthly   = mod.social.monthly,
                 econ_bernoulli   = mod.econ.bernoulli,
                 econ_monthly     = mod.econ.monthly,
                 hybrid_bernoulli = mod.hybrid.bernoulli,
                 hybrid_monthly   = mod.hybrid.monthly)
  retval
}
