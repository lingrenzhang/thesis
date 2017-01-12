raw_data <- read.csv("../Data/SBA_7a_2015.csv", header = TRUE)
naics_map <- read.csv("../Data/NAICS Code.csv", header = TRUE)
state_map <- read.csv("../Data/State Group.csv", header = TRUE)

# quantitative independent variables
grossamountinthousand <- raw_data$GrossApproval/1000
guaranteedratio <- raw_data$SBAGuaranteedApproval/raw_data$GrossApproval
terminmonths <- raw_data$TermInMonths

# qualitative independent variables
businesstype <- raw_data$BusinessType
borrowerbanksamestate <- (as.character(raw_data$BorrState) == as.character(raw_data$BankState))
borrowerprojsamestate <- (as.character(raw_data$BorrState) == as.character(raw_data$ProjectState))
termisfullyear <- (terminmonths%%12 == 0)

naics_code <- as.numeric(substr(as.character(raw_data$NaicsCode),1,2))
naics_map_idx <- match(naics_code, naics_map$Code)
industry <- naics_map$Industry[naics_map_idx]
state_code <- raw_data$BorrState
state_map_idx <- match(state_code, state_map$Code)
stategroup <- state_map$Group[state_map_idx]
#stategroup[is.na(stategroup)] <- 'Other'

#dependent variables
status <- raw_data$LoanStatus
default <- (raw_data$LoanStatus == "CHGOFF")
start_date <- as.Date(raw_data$ApprovalDate, '%m/%d/%Y')
start_month_num <- as.numeric(format(start_date, '%m')) + as.numeric(format(start_date, '%Y')) * 12
censor_month_num <- 1 + 2015*12
default_date <- as.Date(raw_data$ChargeOffDate, '%m/%d/%Y')
default_month_num <- as.numeric(format(default_date, '%m')) + as.numeric(format(default_date, '%Y')) * 12
term <- ifelse(default, default_month_num - start_month_num, pmin(censor_month_num - start_month_num, terminmonths))
default <- default + 0 # convert T/F to 1/0

SBAData <- data.frame(grossamountinthousand, 
                   guaranteedratio,
                   terminmonths, 
                   businesstype, 
                   borrowerbanksamestate, 
                   borrowerprojsamestate,
                   termisfullyear,
                   industry,
                   state_code,
                   stategroup,
                   start_month_num,
                   term,
                   status,
                   default)
save(SBAData, file = 'SBAData.RData')
