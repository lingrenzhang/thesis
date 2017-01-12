# TODO: where is guaranteed_ratio?  no guaranteed ratio or amount
library(survival)

raw_data <- read.csv("../Data/SBA_Loan_data_.csv", header = TRUE)
idx_not_cancelled <- which(raw_data$LoanStatus != "CANCLD")
raw_data <- raw_data[idx_not_cancelled, ]
naics_map <- read.csv("../Data/NAICS Code.csv", header = TRUE)
state_map <- read.csv("../Data/State Group.csv", header = TRUE)

# quantitative independent variables
grossamountinthousand <- raw_data$GrossApproval/1000
terminmonths <- raw_data$TermInMonths

# qualitative independent variables
businesstype <- raw_data$BusinessType
borrowerbanksamestate <- (as.character(raw_data$BorrState) == as.character(raw_data$CDC_State))
borrowerprojsamestate <- (as.character(raw_data$BorrState) == as.character(raw_data$ProjectState))
termisfullyear <- (terminmonths%%12 == 0)

naics_code <- as.numeric(substr(as.character(raw_data$NaicsCode),1,2))
naics_map_idx <- match(naics_code, naics_map$Code)
industry <- naics_map$Industry[naics_map_idx]
state_code <- raw_data$BorrState
state_map_idx <- match(state_code, state_map$Code)
stategroup <- state_map$Group[state_map_idx]

#dependent variables
default <- (raw_data$LoanStatus == "CHGOFF")
start_date <- as.Date(raw_data$ApprovalDate, '%m/%d/%Y')
start_month_num <- as.numeric(format(start_date, '%m')) + as.numeric(format(start_date, '%Y')) * 12
censor_month_num <- 1 + 2014*12
default_date <- as.Date(raw_data$ChargeOffDate, '%m/%d/%Y')
default_month_num <- as.numeric(format(default_date, '%m')) + as.numeric(format(default_date, '%Y')) * 12
term <- ifelse(default, default_month_num - start_month_num, pmin(censor_month_num - start_month_num, terminmonths))
default <- default + 0 # convert T/F to 1/0

data <- data.frame(grossamountinthousand, 
                   terminmonths, 
                   businesstype, 
                   borrowerbanksamestate, 
                   borrowerprojsamestate,
                   termisfullyear,
                   industry,
                   stategroup,
                   term,
                   default)
save(data, file = 'cleaned_data.RData')
mod.sme <- coxph(Surv(term, default) ~ grossamountinthousand + terminmonths + businesstype + borrowerbanksamestate + borrowerprojsamestate + termisfullyear + industry + stategroup, data = data)
