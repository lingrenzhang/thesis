load("AggData.RData")

fileConn <- file("Tables\\TwoWay.txt")
lines = c('\\begin{table}[h]', '\\begin{tabular}{|l|l|l|l|}', '\\hline');
lines = c(lines, ' & Default & No Default & Default Percentage \\\\ \\hline');

twt_default = length(which(data$exist & data$default == 1))
twt_no_default = length(which(data$exist & data$default == 0))
percentage1 = twt_default / (twt_default + twt_no_default) * 100
lines = c(lines, paste( 'Twitter & ', 
                        as.character(twt_default), ' & ', 
                        as.character(twt_no_default), ' & ',
                        as.character(round(percentage1,2)), '\\% \\\\ \\hline', sep = ''));

no_twt_default = length(which(!data$exist & data$default == 1))
no_twt_no_default = length(which(!data$exist & data$default == 0))
percentage2 = no_twt_default / (no_twt_default + no_twt_no_default) * 100
lines = c(lines, paste( 'No Twitter & ', 
                        as.character(no_twt_default), ' & ', 
                        as.character(no_twt_no_default), ' & ',
                        as.character(round(percentage2,2)), '\\% \\\\ \\hline', sep = ''));

lines = c(lines, '\\end{tabular}', '\\end{table}');
writeLines(lines, fileConn)
close(fileConn)