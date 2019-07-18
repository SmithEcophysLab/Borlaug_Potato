# analysis of springlake 2019 data

## libraries
library(tidyr)
library(emmeans)

## plot information
plot_info = read.csv("../data/springlake/springlake_layout_2019.csv")

## lai
lai_raw = read.csv("../data/springlake/lai/clean/lai_mean_SLK0719.csv")
lai_raw = rename(lai_raw, 'plot' = 'Plot')
lai = left_join(lai_raw, plot_info)
lai$plantingfac = as.factor(lai$planting)
lai = subset(lai, variety != 'UNICA' & variety != 'TACNA')

hist(lai$lai_mean)

lai_lm = lm(lai_mean ~ variety * plantingfac, data = lai)
anova(lai_lm)
emmeans(lai_lm, ~ plantingfac)
cld(emmeans(lai_lm, ~ variety * plantingfac))




