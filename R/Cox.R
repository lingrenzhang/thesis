library('survival')
load('AggData.RData')

PrintCoef <- function(coefficients, filename) {
  fileConn <- file(filename)
  lines <- c('\\begin{table}[h]', '\\begin{center}', '\\begin{tabular}{|l|l|l|}', '\\hline');
  lines <- c(lines, 'Feature & Coefficient & p-value \\\\ \\hline');
  
  rows <- rownames(coefficients);
  for(i in 1:length(rows)) {
    lines <- c(lines, paste(
      rows[i], ' & ',
      as.character(round(coefficients[i,1],2)), ' & ',
      format(coefficients[i,5], scientific =  TRUE), ' \\\\ \\hline',
      sep = ''))
  }
  
  lines <- c(lines, '\\end{tabular}', '\\end{center}', '\\end{table}');
  writeLines(lines, fileConn)
  close(fileConn)  
}

#social
mod.social <- coxph(Surv(term, default) ~ log(adjustedfollowercount+1) +
                      log(adjustedfriendcount+1) +
                      log(adjustedstatuscount+1)
                    , data = data)
PrintCoef(summary(mod.social)$coefficients, 'Tables\\CoxSocial.txt')

#economical
mod.econ <- coxph(Surv(term, default) ~ log(grossamountinthousand) + 
                    guaranteedratio + 
                    terminmonths + 
                    borrowerbanksamestate + 
                    borrowerprojsamestate + 
                    termisfullyear + 
                    industry + 
                    stategroup,
                  data = data)
PrintCoef(summary(mod.econ)$coefficients, 'Tables\\CoxEcon.txt')

#hybrid
mod.hybrid <- coxph(Surv(term, default) ~ log(grossamountinthousand) + 
                      guaranteedratio + 
                      terminmonths + 
                      borrowerbanksamestate + 
                      borrowerprojsamestate + 
                      termisfullyear + 
                      industry + 
                      stategroup +
                      log(adjustedfollowercount+1) +
                      log(adjustedfriendcount+1) +
                      log(adjustedstatuscount+1),
                    data = data)
PrintCoef(summary(mod.hybrid)$coefficients, 'Tables\\CoxHybrid.txt')
