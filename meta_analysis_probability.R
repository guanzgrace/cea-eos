
#Use  https://stackoverflow.com/questions/21719578/confidence-interval-for-binomial-data-in-r 
# https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Jeffreys_interval 


library(readr)
library(meta)
library(tibble)
library(Hmisc)

ptf <- read_csv("~/Desktop/Notebooks/Data/prob_RE_input_0601.csv")

ptf <- as_tibble(cbind(ptf, binconf(ptf$X, ptf$N)))

results <- data.frame(variable=character(), mean=double(),
                      lower=double(), upper=double())

for (var in unique(ptf$Variable)) {
  madata <- ptf[ptf$Variable == var, ]
  
  m.hksj <- metagen(PointEst,
                    data = madata,
                    studlab = paste(Study),
                    comb.fixed = FALSE,
                    comb.random = TRUE,
                    prediction = TRUE,
                    lower=Lower,
                    upper=Upper)
  print(m.hksj)
  
  de <- list(variable=var, mean=m.hksj$TE.random,
             lower=m.hksj$lower.random, upper=m.hksj$upper.random)
  results = rbind(results, de)
}

results$lower <- ifelse(results$lower < 0, 10^-6, results$lower)
print(results)

write.csv(results, "~/Desktop/Notebooks/Data/prob_RE_output_0601.csv")



