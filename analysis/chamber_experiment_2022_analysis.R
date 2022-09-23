# chamber experiment 2022 analysis
## r script to analyze the greenhouse and growth chamber experiment from 2022

## load packages
library(plantecophys)

## load data
aci_data <- read.csv('../data/growth_chamber/licor_data_2022/Dinah_potato_curves_fullyMerged.csv')
head(aci_data)
colnames(aci_data)

## start visualization and curve fitting
levels(as.factor(aci_data$id))

### plant id1 pre_heatwave
aci_data_id1_pre = subset(aci_data, id == "1" & meas.type == 'pre_heatwave')
aci_data_id1_pre[, c(6,9:10)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id1_pre)

#### fit aci curve
fit_aci_id1_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         citransition = 300,
                         Tcorrect = TRUE,
                         data = aci_data_id1_pre)
summary(fit_aci_id1_pre)
coef_id1_pre <- coef(fit_aci_id1_pre)

#### put everything together in nice dataframe

#### plot
plot(fit_aci_id1_pre)







