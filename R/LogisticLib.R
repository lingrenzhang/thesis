#expand into monthly data points
Expand <- function(input) {
  output <- input[rep(row.names(input), input$term), ]
  monthIndexList <- lapply(input$term, function(n){0:(n-1)})
  output$monthindex <- unlist(monthIndexList)
  defaultIdx <- which((output$default == 1) & (output$monthindex == (output$term - 1)))
  output$default <- 0
  output$default[defaultIdx] <- 1
  output
}

GetDisplayName <- function(raw_name) {
  raw_names <- c(
    "(Intercept)",
    "log(grossamountinthousand)", 
    "guaranteedratio",
    "terminmonths",
    "borrowerbanksamestateTRUE",
    "termisfullyearTRUE",
    "stategroupGreat Lakes",
    "stategroupMid Atlantic",
    "stategroupNew England",
    "stategroupOther",
    "stategroupPlains",
    "stategroupRocky Mountain",
    "stategroupSoutheast",
    "stategroupSouthwest",
    "industryAdministrative and Support and Waste Management and Remediation Services",
    "industryAgriculture, Forestry, Fishing and Hunting",                              
    "industryArts, Entertainment, and Recreation",                                     
    "industryConstruction",                                                            
    "industryEducational Services",                                                    
    "industryFinance and Insurance",                                                   
    "industryHealth Care and Social Assistance",                                       
    "industryInformation",                                                             
    "industryManagement of Companies and Enterprises",                                 
    "industryManufacturing",                                                           
    "industryMining",                                                                  
    "industryOther Services (except Public Administration)",                           
    "industryProfessional, Scientific, and Technical Services",                        
    "industryPublic Administration",                                                   
    "industryReal Estate Rental and Leasing",                                          
    "industryRetail Trade",                                                            
    "industryTransportation and Warehousing",
    "industryUtilities",                                                               
    "industryWholesale Trade",
    "unemployment_by_state",
    "gdp_by_state_diff",
    "gdp_by_industry_diff",
    "sp500_1yr",
    "cs_housing_1yr",
    "unemployment",
    "google_trend",
    "log(fb_likes + 1)",
    "log(adjustedfollowercount + 1)",
    "log(adjustedfriendcount + 1)",
    "log(adjustedstatuscount + 1)",
    "adjustedstatuscount",
    "existTRUE"
  )
  display_names <- c(
    "(Intercept)",
    "log(gross amount in thousand)", 
    "guaranteed ratio",
    "term in months",
    "borrower bank same state=TRUE",
    "term is full year=TRUE",
    "state group=Great Lakes",
    "state group=Mid Atlantic",
    "state group=New England",
    "state group=Other",
    "state group=Plains",
    "state group=Rocky Mountain",
    "state group=Southeast",
    "state group=Southwest",
    "industry=Administrative and Support and Waste Management and Remediation Services",
    "industry=Agriculture, Forestry, Fishing and Hunting",                              
    "industry=Arts, Entertainment, and Recreation",                                     
    "industry=Construction",                                                            
    "industry=Educational Services",                                                    
    "industry=Finance and Insurance",                                                   
    "industry=Health Care and Social Assistance",                                       
    "industry=Information",                                                             
    "industry=Management of Companies and Enterprises",                                 
    "industry=Manufacturing",                                                           
    "industry=Mining",                                                                  
    "industry=Other Services (except Public Administration)",                           
    "industry=Professional, Scientific, and Technical Services",                        
    "industry=Public Administration",                                                   
    "industry=Real Estate Rental and Leasing",                                          
    "industry=Retail Trade",                                                            
    "industry=Transportation and Warehousing",
    "industry=Utilities",                                                               
    "industry=Wholesale Trade",
    "unemployment by state",
    "GDP by state YoY growth",
    "GDP by industry YoY growth",
    "S\\&P 500 (trailing 1yr)",
    "Case Shiller Housing Index (trailing 1yr)",
    "unemployment index",
    "Google Trend index",
    "Like index",
    "Follower index",
    "Friend index",
    "monthly Tweet index",
    "monthly Tweet growth rate",
    "Twitter exist"
  )
  idx <- which(raw_names == raw_name)
  display_name <- display_names[idx]
  display_name
}

GetpValueDisplay <- function(p_value) {
  p_value_display <- as.character(round(p_value,4))
  if(p_value < 0.01) {
    p_value_display <- format(p_value, digits = 3, scientific = TRUE)
  }
  if(p_value < 2e-16) {
    p_value_display <- "\\textless 2e-16"
  }
  p_value_display  
}

PrintCoefComparison <- function(coeff_full, coeff_select, bigdata, filename) {
  fileConn <- file(filename)
  #lines <- c('\\begin{table}[h]', '\\begin{center}', '\\begin{tabular}{|l|r|r|r|r|}', '\\hline');
  lines <- c('Feature & Coefficient & p-value & Coefficient & p-value \\\\ \\hline');
  pvalue_col <- ifelse(bigdata, 5, 4)
  
  rows <- rownames(coeff_full);
  for(i in 1:length(rows)) {
    new_line <- paste(
      GetDisplayName(rows[i]), ' & ',
      as.character(round(coeff_full[i,1],4)), ' & ',
      GetpValueDisplay(coeff_full[i,pvalue_col]), sep = '')
    if(any(rownames(coeff_select)==rows[i])) {
      new_line <- paste(new_line, ' & ',
                        as.character(round(coeff_select[rows[i],1],4)), ' & ',
                        GetpValueDisplay(coeff_select[rows[i],pvalue_col]), sep = '')      
    } else {
      new_line <- paste(new_line, ' & NA & NA', sep = '')
    }
    new_line <- paste(new_line, ' \\\\ \\hline', sep = '')
    lines <- c(lines, new_line)
  }
  
  #lines <- c(lines, '\\end{tabular}', '\\end{center}', '\\end{table}');
  writeLines(lines, fileConn)
  close(fileConn)  
}

GetDefault <- function(data, forecast_horizon, bernoulli) {
  if(bernoulli) {
    retval <- data$default
  } else {
    retval <- data$default & (data$term <= forecast_horizon)
  }
  retval  
}

GetPred <- function(mod, data, forecast_horizon, bernoulli) {
  pred <- predict(mod, data, type = "response")
  if(bernoulli) {
    retval <- pred
  } else {
    # convert from monthly default rate to default rate within forecast_horizon
    # check for values outside of [0,1], check for na
    if(sum(is.na(pred)) > 0) {
      stop('unexpected na values')
    }
    if(min(pred) < 0 | max(pred) > 1) {
      stop('default probability out of range')
    }
    pred <- pmin(pred, 1)
    end_month <- 2015*12
    num_months <- pmin(end_month - data$start_month_num, data$terminmonths, forecast_horizon)
    if(min(num_months) < 0) {
      stop('invalid forecast horizon')
    }
    retval <- 1 - (1-pred)^num_months
  }
  retval
}

GetRoc <- function(pred_within_horizon, default_within_horizon) {
  predObj <- prediction(pred_within_horizon, default_within_horizon)
  performance(predObj, "tpr", "fpr")
}

GetAuc <- function(pred_within_horizon, default_within_horizon) {
  predObj <- prediction(pred_within_horizon, default_within_horizon)
  auc <- performance(predObj, "auc")
  auc@y.values[[1]]
}

IncorporateUnemployment <- function(dataExpanded) {
  raw_data <- read.csv("../Data/unemployment_estimates Lag.csv", colClasses = c("character", rep("numeric", 3)), header = TRUE)
  
  date <- as.Date(raw_data$date, '%m/%d/%Y')
  month_num <- as.numeric(format(date, '%m')) + as.numeric(format(date, '%Y')) * 12
  idx <- match(dataExpanded$start_month_num + dataExpanded$monthindex, month_num)
  unemployment <- raw_data$prediction[idx]
  dataExpanded$unemployment <- unemployment
  dataExpanded
}

IncorporateGoogleTrend <- function(dataExpanded) {
  raw_data <- read.csv("../Data/Google Trend Monthly Diff Lag.csv", colClasses = c("character", rep("numeric", 20)), header = TRUE)
  year <- as.numeric(substr(raw_data$Date, 1, 4))
  month <- as.numeric(substr(raw_data$Date, 6, 7))
  month_num <- year * 12 + month
  
  row_idx <- match(dataExpanded$start_month_num + dataExpanded$monthindex, month_num)
  col_names <- c('Date', 'Agriculture, Forestry, Fishing and Hunting', 'Mining', 'Utilities',
                 'Construction', 'Manufacturing', 'Wholesale Trade', 'Retail Trade',
                 'Transportation and Warehousing', 'Information', 'Finance and Insurance',
                 'Real Estate Rental and Leasing', 'Professional, Scientific, and Technical Services',
                 'Management of Companies and Enterprises', 
                 'Administrative and Support and Waste Management and Remediation Services',
                 'Educational Services', 'Health Care and Social Assistance', 'Arts, Entertainment, and Recreation',
                 'Accommodation and Food Services', 'Other Services (except Public Administration)',
                 'Public Administration')
  
  dataExpanded$google_trend <- 0
  
  cols <- unique(dataExpanded$industry)
  for(col_name in cols) {
    col_idx <- match(col_name, col_names)
    col_value <- raw_data[,col_idx]
    destination_row_idx_vec <- which(dataExpanded$industry == col_name)
    dataExpanded$google_trend[destination_row_idx_vec] <- col_value[row_idx[destination_row_idx_vec]]
  }
  dataExpanded
}

IncorporateGDPByIndustry <- function(dataExpanded) {
  raw_data <- read.csv("../Data/BEA_GDP_by_industry_diff Lag.csv", colClasses = c(rep("numeric", 21)), header = TRUE)
  year_num <- (dataExpanded$start_month_num + dataExpanded$monthindex - 1) %/% 12
  
  row_idx <- match(year_num, raw_data$Year)
  col_names <- c('Year', 'Agriculture, Forestry, Fishing and Hunting', 'Mining', 'Utilities',
                 'Construction', 'Manufacturing', 'Wholesale Trade', 'Retail Trade',
                 'Transportation and Warehousing', 'Information', 'Finance and Insurance',
                 'Real Estate Rental and Leasing', 'Professional, Scientific, and Technical Services',
                 'Management of Companies and Enterprises', 
                 'Administrative and Support and Waste Management and Remediation Services',
                 'Educational Services', 'Health Care and Social Assistance', 'Arts, Entertainment, and Recreation',
                 'Accommodation and Food Services', 'Other Services (except Public Administration)',
                 'Public Administration')
  
  dataExpanded$gdp_by_industry_diff <- 0
  
  cols <- unique(dataExpanded$industry)
  for(col_name in cols) {
    col_idx <- match(col_name, col_names)
    col_value <- raw_data[,col_idx]
    destination_row_idx_vec <- which(dataExpanded$industry == col_name)
    dataExpanded$gdp_by_industry_diff[destination_row_idx_vec] <- col_value[row_idx[destination_row_idx_vec]]
  }
  dataExpanded
}

IncorporateUnemploymentByState <- function(dataExpanded) {
  raw_data <- read.csv("../Data/Statewide Unemployment Lag.csv", colClasses = c(rep("numeric", 53)), header = TRUE)
  month_num <- raw_data$Year * 12 + raw_data$Month

  row_idx <- match(dataExpanded$start_month_num + dataExpanded$monthindex, month_num)
  col_names <- c('Year', 'Month',
                 'AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 'DC', 'DE', 'FL',
                 'GA', 'HI', 'IA', 'ID', 'IL', 'IN', 'KS', 'KY', 'LA', 'MA',
                 'MD', 'ME', 'MI', 'MN', 'MO', 'MS', 'MT', 'NC', 'ND', 'NE',
                 'NH', 'NJ', 'NM', 'NV', 'NY', 'OH', 'OK', 'OR', 'PA', 'RI',
                 'SC', 'SD', 'TN', 'TX', 'UT', 'VA', 'VT', 'WA', 'WI', 'WV', 'WY')
  dataExpanded$unemployment_by_state <- 0
  
  cols <- unique(dataExpanded$state_code)
  for(col_name in cols) {
    col_idx <- match(col_name, col_names)
    col_value <- raw_data[,col_idx]
    destination_row_idx_vec <- which(dataExpanded$state_code == col_name)
    dataExpanded$unemployment_by_state[destination_row_idx_vec] <- col_value[row_idx[destination_row_idx_vec]]
  }
  dataExpanded
}

IncorporateGDPByState <- function(dataExpanded) {
  raw_data <- read.csv("../Data/BEA_GDP_by_state_diff Lag.csv", colClasses = c(rep("numeric", 52)), header = TRUE)
  year_num <- (dataExpanded$start_month_num + dataExpanded$monthindex - 1) %/% 12
  
  row_idx <- match(year_num, raw_data$Year)
  col_names <- c('Year',
                 'AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 'DC', 'DE', 'FL',
                 'GA', 'HI', 'IA', 'ID', 'IL', 'IN', 'KS', 'KY', 'LA', 'MA',
                 'MD', 'ME', 'MI', 'MN', 'MO', 'MS', 'MT', 'NC', 'ND', 'NE',
                 'NH', 'NJ', 'NM', 'NV', 'NY', 'OH', 'OK', 'OR', 'PA', 'RI',
                 'SC', 'SD', 'TN', 'TX', 'UT', 'VA', 'VT', 'WA', 'WI', 'WV', 'WY')
  dataExpanded$gdp_by_state_diff <- 0
  
  cols <- unique(dataExpanded$state_code)
  for(col_name in cols) {
    col_idx <- match(col_name, col_names)
    col_value <- raw_data[,col_idx]
    destination_row_idx_vec <- which(dataExpanded$state_code == col_name)
    dataExpanded$gdp_by_state_diff[destination_row_idx_vec] <- col_value[row_idx[destination_row_idx_vec]]
  }
  dataExpanded
}

IncorporateSP500 <- function(dataExpanded) {
  raw_data_sp <- read.csv("../Data/SP 500 TR 1yr Return Lag.csv", colClasses = c(rep("numeric",3)), header = TRUE)
  month_num <- raw_data_sp$Year * 12 + raw_data_sp$Month
  idx <- match(dataExpanded$start_month_num + dataExpanded$monthindex, month_num)
  dataExpanded$sp500_1yr <- raw_data_sp$Return[idx]
  dataExpanded  
}

IncorporateCSHousingIndex <- function(dataExpanded) {
  raw_data_cs <- read.csv("../Data/SP CS Housing 1yr Return Lag.csv", colClasses = c(rep("numeric",3)), header = TRUE)
  month_num <- raw_data_cs$Year * 12 + raw_data_cs$Month
  idx <- match(dataExpanded$start_month_num + dataExpanded$monthindex, month_num)
  dataExpanded$cs_housing_1yr <- raw_data_cs$Return[idx]
  dataExpanded  
}

IncorporateExternalData <- function(dataExpanded) {
  dataExpanded <- IncorporateUnemployment(dataExpanded)
  dataExpanded <- IncorporateGoogleTrend(dataExpanded)
  dataExpanded <- IncorporateSP500(dataExpanded)
  dataExpanded <- IncorporateCSHousingIndex(dataExpanded)
  dataExpanded <- IncorporateUnemploymentByState(dataExpanded)  
  dataExpanded <- IncorporateGDPByState(dataExpanded)  
  dataExpanded <- IncorporateGDPByIndustry(dataExpanded)  
  dataExpanded
}

GetFullModels <- function(BernoulliInSample, MonthlyInSample, bigdata) {
  if(bigdata) {
    models <- GetFullModelsBig(BernoulliInSample, MonthlyInSample)
  } else {
    models <- GetFullModelsSmall(BernoulliInSample, MonthlyInSample)    
  }
  models
}

GetFullModelsSmall <- function(BernoulliInSample, MonthlyInSample) {
  mod.social.bernoulli <- glm(default ~ unemployment + 
                                   google_trend +
                                   log(fb_likes+1) +
                                   log(adjustedfollowercount+1) + 
                                   log(adjustedfriendcount+1) +
                                   adjustedstatuscount,
                                 #log(adjustedstatuscount+1),
                                 family = binomial(),
                                 data = BernoulliInSample)
  mod.social.monthly <- glm(default ~ unemployment + 
                                 google_trend +
                                 log(fb_likes+1) +
                                 log(adjustedfollowercount+1) + 
                                 log(adjustedfriendcount+1) +
                                 adjustedstatuscount,
                               #log(adjustedstatuscount+1),
                               family = binomial(),
                               data = MonthlyInSample)
  mod.econ.bernoulli <- glm(default ~ log(grossamountinthousand) + 
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
                               data = BernoulliInSample)
  mod.econ.monthly <- glm(default ~ log(grossamountinthousand) + 
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
  mod.hybrid.bernoulli <- glm(default ~ log(grossamountinthousand) + 
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
                                   log(fb_likes+1) +
                                   log(adjustedfollowercount+1) + 
                                   log(adjustedfriendcount+1) +
                                   adjustedstatuscount,
                                 #log(adjustedstatuscount+1),
                                 family = binomial(),
                                 data = BernoulliInSample)
  mod.hybrid.monthly <- glm(default ~ log(grossamountinthousand) + 
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
                                 log(fb_likes+1) +
                                 log(adjustedfollowercount+1) + 
                                 log(adjustedfriendcount+1) +
                                 adjustedstatuscount,  
                               #log(adjustedstatuscount+1),
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

GetFullModelsBig <- function(BernoulliInSample, MonthlyInSample) {
  mod.social.bernoulli <- bigglm(default ~ unemployment + 
                                google_trend +
                                log(fb_likes+1) +
                                log(adjustedfollowercount+1) + 
                                log(adjustedfriendcount+1) +
                                adjustedstatuscount,
                                #log(adjustedstatuscount+1),
                              family = binomial(),
                              data = BernoulliInSample)
  mod.social.monthly <- bigglm(default ~ unemployment + 
                              google_trend +
                              log(fb_likes+1) +
                              log(adjustedfollowercount+1) + 
                              log(adjustedfriendcount+1) +
                              adjustedstatuscount,
                              #log(adjustedstatuscount+1),
                            family = binomial(),
                            data = MonthlyInSample)
  mod.econ.bernoulli <- bigglm(default ~ log(grossamountinthousand) + 
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
                                stategroup +
                                unemployment_by_state +
                                gdp_by_state_diff +
                                gdp_by_industry_diff +
                                sp500_1yr +
                                cs_housing_1yr +
                                unemployment + 
                                google_trend +
                                log(fb_likes+1) +
                                log(adjustedfollowercount+1) + 
                                log(adjustedfriendcount+1) +
                                adjustedstatuscount,
                                #log(adjustedstatuscount+1),
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
                            log(fb_likes+1) +
                            log(adjustedfollowercount+1) + 
                            log(adjustedfriendcount+1) +
                            adjustedstatuscount,  
                            #log(adjustedstatuscount+1),
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

GetSelectedModelsAIC <- function(BernoulliInSample, MonthlyInSample, bigdata) {
  #big and small uses different covariates
  if(bigdata) {
    models <- GetSelectedModelsAICBig(BernoulliInSample, MonthlyInSample)
  } else {
    models <- GetSelectedModelsAICSmall(BernoulliInSample, MonthlyInSample)    
  }
  models
}

GetSelectedModelsAICSmall <- function(BernoulliInSample, MonthlyInSample) {
  mod.social.bernoulli <- glm(default ~ unemployment + 
                                   log(adjustedfollowercount + 1),
                                 family = binomial(),
                                 data = BernoulliInSample)
  mod.social.monthly <- glm(default ~ unemployment + 
                                 log(adjustedfollowercount+1) +
                                 log(adjustedfriendcount+1),
                               family = binomial(),
                               data = MonthlyInSample)
  mod.econ.bernoulli <- glm(default ~ log(grossamountinthousand) + 
                                 guaranteedratio + 
                                 terminmonths + 
                                 borrowerbanksamestate + 
                                 termisfullyear + 
                                 stategroup +
                                 unemployment_by_state,
                               family = binomial(),
                               data = BernoulliInSample)
  mod.econ.monthly <- glm(default ~ log(grossamountinthousand) + 
                               guaranteedratio + 
                               terminmonths + 
                               borrowerbanksamestate + 
                               termisfullyear + 
                               stategroup + 
                               unemployment_by_state +
                               gdp_by_state_diff +
                               gdp_by_industry_diff +                            
                               cs_housing_1yr,
                             family = binomial(),
                             data = MonthlyInSample)
  mod.hybrid.bernoulli <- glm(default ~ log(grossamountinthousand) + 
                                   guaranteedratio + 
                                   terminmonths +
                                   borrowerbanksamestate + 
                                   termisfullyear +
                                   sp500_1yr +
                                   unemployment,
                                 family = binomial(),
                                 data = BernoulliInSample)
  mod.hybrid.monthly <- glm(default ~ log(grossamountinthousand) + 
                                 guaranteedratio +
                                 terminmonths + 
                                 borrowerbanksamestate + 
                                 termisfullyear + 
                                 stategroup +
                                 unemployment_by_state +
                                 gdp_by_state_diff +
                                 gdp_by_industry_diff +                            
                                 cs_housing_1yr +
                                 unemployment +
                                 google_trend +
                                 log(adjustedfollowercount + 1) +
                                 log(adjustedfriendcount + 1),
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

GetSelectedModelsAICBig <- function(BernoulliInSample, MonthlyInSample) {
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
                              unemployment_by_state,
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
                            cs_housing_1yr,
                          family = binomial(),
                          data = MonthlyInSample)
  mod.hybrid.bernoulli <- bigglm(default ~ log(grossamountinthousand) + 
                                guaranteedratio + 
                                terminmonths +
                                borrowerbanksamestate + 
                                termisfullyear +
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
                              cs_housing_1yr +
                              unemployment +
                              google_trend +
                              log(adjustedfollowercount + 1) +
                              log(adjustedfriendcount + 1),
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

PrintTablesComparison <- function(models_full, models_select, bigdata, tag) {
  key <- ifelse(bigdata, 'mat', 'coefficients')
  PrintCoefComparison(summary(models_full$social_bernoulli)[key][[1]], 
                      summary(models_select$social_bernoulli)[key][[1]],
                      bigdata,
                      paste('Tables\\Social_Binary_', tag, '.txt', sep=''))  
  PrintCoefComparison(summary(models_full$social_monthly)[key][[1]], 
                      summary(models_select$social_monthly)[key][[1]], 
                      bigdata,
                      paste('Tables\\Social_Monthly_', tag, '.txt', sep=''))  
  PrintCoefComparison(summary(models_full$econ_bernoulli)[key][[1]], 
                      summary(models_select$econ_bernoulli)[key][[1]], 
                      bigdata,
                      paste('Tables\\Econ_Binary_', tag, '.txt', sep=''))  
  PrintCoefComparison(summary(models_full$econ_monthly)[key][[1]], 
                      summary(models_select$econ_monthly)[key][[1]], 
                      bigdata,
                      paste('Tables\\Econ_Monthly_', tag, '.txt', sep=''))  
  PrintCoefComparison(summary(models_full$hybrid_bernoulli)[key][[1]], 
                      summary(models_select$hybrid_bernoulli)[key][[1]], 
                      bigdata,
                      paste('Tables\\Hybrid_Binary_', tag, '.txt', sep=''))
  PrintCoefComparison(summary(models_full$hybrid_monthly)[key][[1]], 
                      summary(models_select$hybrid_monthly)[key][[1]], 
                      bigdata,
                      paste('Tables\\Hybrid_Monthly_', tag, '.txt', sep=''))  
}

getFactorOrders <- function(x) {
  ordering <- NA
  for(level in levels(x)) {
    ordering[level==x] <- seq(from = 1, to = 100, length.out = sum(level==x))
  }
  return(ordering)
}

getLogLikelihood <- function(pred, response) {
  ll <- sum(log(pred)*response + log(1-pred)*(1-response))
  ll
}

PrintInSampleLogLikelihood <- function() {
  BernoulliInSample <- as.data.frame(BernoulliInSample)
  MonthlyInSample <- as.data.frame(MonthlyInSample)
  
  predEconBernoulli <- GetPred(models$econ_bernoulli, BernoulliInSample, forecast_horizon, TRUE)
  predHybridBernoulli <- GetPred(models$hybrid_bernoulli, BernoulliInSample, forecast_horizon, TRUE)
  predSocialBernoulli <- GetPred(models$social_bernoulli, BernoulliInSample, forecast_horizon, TRUE)
  predEconMonthly <- GetPred(models$econ_monthly, MonthlyInSample, forecast_horizon, FALSE)
  predHybridMonthly <- GetPred(models$hybrid_monthly, MonthlyInSample, forecast_horizon, FALSE)
  predSocialMonthly <- GetPred(models$social_monthly, MonthlyInSample, forecast_horizon, FALSE)
  
  predEconBernoulliFull <- GetPred(fullModels$econ_bernoulli, BernoulliInSample, forecast_horizon, TRUE)
  predHybridBernoulliFull <- GetPred(fullModels$hybrid_bernoulli, BernoulliInSample, forecast_horizon, TRUE)
  predSocialBernoulliFull <- GetPred(fullModels$social_bernoulli, BernoulliInSample, forecast_horizon, TRUE)
  predSocialMonthlyFull <- GetPred(fullModels$social_monthly, MonthlyInSample, forecast_horizon, FALSE)
  predEconMonthlyFull <- GetPred(fullModels$econ_monthly, MonthlyInSample, forecast_horizon, FALSE)
  predHybridMonthlyFull <- GetPred(fullModels$hybrid_monthly, MonthlyInSample, forecast_horizon, FALSE)
  
  defaultBernoulliInSample <- GetDefault(BernoulliInSample, forecast_horizon, TRUE)
  defaultMonthlyInSample <- GetDefault(MonthlyInSample, forecast_horizon, FALSE)
  
  print(getLogLikelihood(predEconBernoulli, defaultBernoulliInSample) / length(defaultBernoulliInSample))
  print(getLogLikelihood(predHybridBernoulli, defaultBernoulliInSample) / length(defaultBernoulliInSample))
  print(getLogLikelihood(predSocialBernoulli, defaultBernoulliInSample) / length(defaultBernoulliInSample))
  print(getLogLikelihood(predEconMonthly, defaultMonthlyInSample) / length(defaultMonthlyInSample))
  print(getLogLikelihood(predHybridMonthly, defaultMonthlyInSample) / length(defaultMonthlyInSample))
  print(getLogLikelihood(predSocialMonthly, defaultMonthlyInSample) / length(defaultMonthlyInSample))
  
  print(getLogLikelihood(predEconBernoulliFull, defaultBernoulliInSample) / length(defaultBernoulliInSample))
  print(getLogLikelihood(predHybridBernoulliFull, defaultBernoulliInSample) / length(defaultBernoulliInSample))
  print(getLogLikelihood(predSocialBernoulliFull, defaultBernoulliInSample) / length(defaultBernoulliInSample))
  print(getLogLikelihood(predEconMonthlyFull, defaultMonthlyInSample) / length(defaultMonthlyInSample))
  print(getLogLikelihood(predHybridMonthlyFull, defaultMonthlyInSample) / length(defaultMonthlyInSample))
  print(getLogLikelihood(predSocialMonthlyFull, defaultMonthlyInSample) / length(defaultMonthlyInSample))
}