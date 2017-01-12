require(ffbase)
require(LaF)
require(ETLUtils)
require(biglm)
dat <- laf_open_csv(filename = "2010_BSA_Carrier_PUF.csv",
                    column_types = c("integer", "integer", "categorical", "categorical", "categorical", "integer", "integer", "categorical", "integer", "integer", "integer"),
                    column_names = c("sex", "age", "diagnose", "healthcare.procedure",
                                     "typeofservice", "service.count", "provider.type", "servicesprocessed",
                                     "place.served", "payment", "carrierline.count"), 
                    skip = 1)
x <- laf_to_ffdf(laf = dat)
table.ff(x$age)
barplot(table.ff(x$age), col = "lightblue")
mymodel <- biglm(payment ~ age, data = x)
#bigglm family = binomial()
