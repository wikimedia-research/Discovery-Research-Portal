library(bandit) # install.packages("bandit")

x <- table("Includes English" = sessions$`Includes English`,
           Clickthrough = sessions$clickthrough)[2:1, 2:1]

## Test Independence
bf <- LearnBayes::ctable(x, matrix(rep(1, 4), 2))
BCDA:::interpret_bf(bf, interpreter = 'Kass and Raftery')

100*BCDA::ci_prop_diff_tail(x)
BCDA::ci_relative_risk(x)
