# analysis of springlake 2019 data

## libraries
library(tidyr)
library(dplyr)
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
emmeans(lai_lm, ~ plantingfac) ## planting date 2 > 1
cld(emmeans(lai_lm, ~ variety, at = list(plantingfac = '1'))) ## Atlantic and Burbank highest
cld(emmeans(lai_lm, ~ variety, at = list(plantingfac = '2'))) ## Atlantic is high
cld(emmeans(lai_lm, ~ variety))
cld(emmeans(lai_lm, ~ variety * plantingfac))

## sla
sla_raw = read.csv('../data/springlake/sla/sla_2019_07_09.csv')
sla_raw$plot_element1 = sapply(strsplit(as.character(sla_raw$sample), 'p'), "[[", 2)
sla_raw$plot = as.numeric(sapply(strsplit(as.character(sla_raw$plot_element1), 'r'), "[[", 1))
sla = left_join(sla_raw, plot_info)
sla$plantingfac = as.factor(sla$planting)
sla = subset(sla, variety != 'UNICA' & variety != 'TACNA')

hist(sla$sla)

sla_lm = lm(sla ~ variety * plantingfac, data = sla)
anova(sla_lm)
cld(emmeans(sla_lm, ~variety)) ## pretty similar
cld(emmeans(sla_lm, ~plantingfac)) ## higher SLA in early planting date
