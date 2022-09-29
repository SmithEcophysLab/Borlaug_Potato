# chamber experiment 2022 analysis
## r script to analyze the greenhouse and growth chamber experiment from 2022

## load packages
library(plantecophys)

## load data
aci_data <- read.csv('Dinah_potato_curves_fullyMerged.csv')
head(aci_data)
colnames(aci_data)

## start visualization, curve fitting, and data frame creation
ids <- levels(as.factor(aci_data$id))
curve_fits <- c()

### plant id1 pre_heatwave
aci_data_id1_pre = subset(aci_data, id == ids[1] & meas.type == 'pre_heatwave')
aci_data_id1_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id1_pre)
#### fit aci curve
fit_aci_id1_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         Tcorrect = FALSE,
                         fitTPU = TRUE,
                         fitmethod = 'bilinear',
                         data = aci_data_id1_pre)
summary(fit_aci_id1_pre)
coef_id1_pre <- coef(fit_aci_id1_pre)
#### plot
plot(fit_aci_id1_pre)
#### add to dataframe
aci_data_id1_pre_data <- cbind(aci_data_id1_pre[1, c(10, 284, 9, 14, 16, 19)],
                               mean(aci_data_id1_pre[,30]),
                               mean(aci_data_id1_pre[,118]),
                               fit_aci_id1_pre[[2]][1,1],
                               fit_aci_id1_pre[[2]][1,2],
                               fit_aci_id1_pre[[2]][2,1],
                               fit_aci_id1_pre[[2]][2,2],
                               fit_aci_id1_pre[[2]][3,1],
                               fit_aci_id1_pre[[2]][3,2],
                               fit_aci_id1_pre[[2]][4,1],
                               fit_aci_id1_pre[[2]][4,2],
                               fit_aci_id1_pre$RMSE,
                               fit_aci_id1_pre$Ci_transition,
                               fit_aci_id1_pre$citransition,
                               fit_aci_id1_pre$Km,
                               fit_aci_id1_pre$GammaStar,
                               fit_aci_id1_pre$fitmethod,
                               fit_aci_id1_pre$Tcorrect,
                               fit_aci_id1_pre$fitTPU)
colnames(aci_data_id1_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id1_pre_data)

### plant id1 post_heatwave
aci_data_id1_post = subset(aci_data, id == ids[1] & meas.type == 'post_heatwave')
aci_data_id1_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id1_post)
#### fit aci curve
fit_aci_id1_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 350,
                         Tcorrect = FALSE,
                         fitTPU = TRUE,
                         fitmethod = 'bilinear',
                         data = aci_data_id1_post)
summary(fit_aci_id1_post)
coef_id1_post <- coef(fit_aci_id1_post)
#### plot
plot(fit_aci_id1_post)
#### add to dataframe
aci_data_id1_post_data <- cbind(aci_data_id1_post[1, c(10, 284, 9, 14, 16, 19)],
                               mean(aci_data_id1_post[,30]),
                               mean(aci_data_id1_post[,118]),
                               fit_aci_id1_post[[2]][1,1],
                               fit_aci_id1_post[[2]][1,2],
                               fit_aci_id1_post[[2]][2,1],
                               fit_aci_id1_post[[2]][2,2],
                               fit_aci_id1_post[[2]][3,1],
                               fit_aci_id1_post[[2]][3,2],
                               fit_aci_id1_post[[2]][4,1],
                               fit_aci_id1_post[[2]][4,2],
                               fit_aci_id1_post$RMSE,
                               fit_aci_id1_post$Ci_transition,
                               fit_aci_id1_post$citransition,
                               fit_aci_id1_post$Km,
                               fit_aci_id1_post$GammaStar,
                               fit_aci_id1_post$fitmethod,
                               fit_aci_id1_post$Tcorrect,
                               fit_aci_id1_post$fitTPU)
colnames(aci_data_id1_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id1_post_data)

# ### plant id100 pre_heatwave
# aci_data_id100_pre = subset(aci_data, id == ids[2] & meas.type == 'pre_heatwave')
# aci_data_id100_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id100_pre)
# #### fit aci curve
# fit_aci_id100_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                          citransition = 300,
#                          Tcorrect = FALSE,
#                          data = aci_data_id100_pre)
# summary(fit_aci_id100_pre)
# coef_id100_pre <- coef(fit_aci_id100_pre)
# #### plot
# plot(fit_aci_id100_pre)
# #### add to dataframe
# aci_data_id100_pre_data <- cbind(aci_data_id100_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                mean(aci_data_id100_pre[,30]),
#                                mean(aci_data_id100_pre[,118]),
#                                fit_aci_id100_pre[[2]][1,1],
#                                fit_aci_id100_pre[[2]][1,2],
#                                fit_aci_id100_pre[[2]][2,1],
#                                fit_aci_id100_pre[[2]][2,2],
#                                fit_aci_id100_pre[[2]][3,1],
#                                fit_aci_id100_pre[[2]][3,2],
#                                fit_aci_id100_pre$RMSE,
#                                fit_aci_id100_pre$Ci_transition,
#                                fit_aci_id100_pre$citransition,
#                                fit_aci_id100_pre$Km,
#                                fit_aci_id100_pre$GammaStar,
#                                fit_aci_id100_pre$fitmethod,
#                                fit_aci_id100_pre$Tcorrect,
#                                fit_aci_id100_pre$fitTPU)
# colnames(aci_data_id100_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                      'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                      'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                      'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                      'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                      'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id100_pre_data)
# 
# ### plant id100 post_heatwave
# aci_data_id100_post = subset(aci_data, id == ids[2] & meas.type == 'post_heatwave')
# aci_data_id100_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id100_post)
# #### fit aci curve
# fit_aci_id100_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                           citransition = 350,
#                           Tcorrect = FALSE,
#                           data = aci_data_id100_post)
# summary(fit_aci_id100_post)
# coef_id100_post <- coef(fit_aci_id100_post)
# #### plot
# plot(fit_aci_id100_post)
# #### add to dataframe
# aci_data_id100_post_data <- cbind(aci_data_id100_post[1, c(10, 284, 9, 14, 16, 19)],
#                                 mean(aci_data_id100_post[,30]),
#                                 mean(aci_data_id100_post[,118]),
#                                 fit_aci_id100_post[[2]][1,1],
#                                 fit_aci_id100_post[[2]][1,2],
#                                 fit_aci_id100_post[[2]][2,1],
#                                 fit_aci_id100_post[[2]][2,2],
#                                 fit_aci_id100_post[[2]][3,1],
#                                 fit_aci_id100_post[[2]][3,2],
#                                 fit_aci_id100_post$RMSE,
#                                 fit_aci_id100_post$Ci_transition,
#                                 fit_aci_id100_post$citransition,
#                                 fit_aci_id100_post$Km,
#                                 fit_aci_id100_post$GammaStar,
#                                 fit_aci_id100_post$fitmethod,
#                                 fit_aci_id100_post$Tcorrect,
#                                 fit_aci_id100_post$fitTPU)
# colnames(aci_data_id100_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                       'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id100_post_data)

##############################################################################
##############################################################################
###########id100b
##############################################################################
##############################################################################
### plant id100b pre_heatwave
# aci_data_id100b_pre = subset(aci_data, id == ids[3] & meas.type == 'pre_heatwave')
# aci_data_id100b_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id100b_pre)
# #### fit aci curve
# fit_aci_id100b_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            citransition = 300,
#                            Tcorrect = FALSE,
#                            data = aci_data_id100b_pre)
# summary(fit_aci_id100b_pre)
# coef_id100b_pre <- coef(fit_aci_id100b_pre)
# #### plot
# plot(fit_aci_id100b_pre)
# #### add to dataframe
# aci_data_id100b_pre_data <- cbind(aci_data_id100b_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id100b_pre[,30]),
#                                  mean(aci_data_id100b_pre[,118]),
#                                  fit_aci_id100b_pre[[2]][1,1],
#                                  fit_aci_id100b_pre[[2]][1,2],
#                                  fit_aci_id100b_pre[[2]][2,1],
#                                  fit_aci_id100b_pre[[2]][2,2],
#                                  fit_aci_id100b_pre[[2]][3,1],
#                                  fit_aci_id100b_pre[[2]][3,2],
#                                  fit_aci_id100b_pre$RMSE,
#                                  fit_aci_id100b_pre$Ci_transition,
#                                  fit_aci_id100b_pre$citransition,
#                                  fit_aci_id100b_pre$Km,
#                                  fit_aci_id100b_pre$GammaStar,
#                                  fit_aci_id100b_pre$fitmethod,
#                                  fit_aci_id100b_pre$Tcorrect,
#                                  fit_aci_id100b_pre$fitTPU)
# colnames(aci_data_id100b_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id100b_pre_data)
# 
# ### plant id100b post_heatwave
# aci_data_id100b_post = subset(aci_data, id == ids[3] & meas.type == 'post_heatwave')
# aci_data_id100b_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id100b_post)
# #### fit aci curve
# fit_aci_id100b_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             citransition = 350,
#                             Tcorrect = FALSE,
#                             data = aci_data_id100b_post)
# summary(fit_aci_id100b_post)
# coef_id100b_post <- coef(fit_aci_id100b_post)
# #### plot
# plot(fit_aci_id100b_post)
# #### add to dataframe
# aci_data_id100b_post_data <- cbind(aci_data_id100b_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id100b_post[,30]),
#                                   mean(aci_data_id100b_post[,118]),
#                                   fit_aci_id100b_post[[2]][1,1],
#                                   fit_aci_id100b_post[[2]][1,2],
#                                   fit_aci_id100b_post[[2]][2,1],
#                                   fit_aci_id100b_post[[2]][2,2],
#                                   fit_aci_id100b_post[[2]][3,1],
#                                   fit_aci_id100b_post[[2]][3,2],
#                                   fit_aci_id100b_post$RMSE,
#                                   fit_aci_id100b_post$Ci_transition,
#                                   fit_aci_id100b_post$citransition,
#                                   fit_aci_id100b_post$Km,
#                                   fit_aci_id100b_post$GammaStar,
#                                   fit_aci_id100b_post$fitmethod,
#                                   fit_aci_id100b_post$Tcorrect,
#                                   fit_aci_id100b_post$fitTPU)
# colnames(aci_data_id100b_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id100b_post_data)

##############################################################################
##############################################################################
###########id102
##############################################################################
##############################################################################
### plant id102 pre_heatwave
aci_data_id102_pre = subset(aci_data, id == ids[4] & meas.type == 'pre_heatwave')
aci_data_id102_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id102_pre)
#### fit aci curve
fit_aci_id102_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = TRUE,
                           fitmethod = 'bilinear',
                            data = aci_data_id102_pre)
summary(fit_aci_id102_pre)
#### plot
plot(fit_aci_id102_pre)
#### add to dataframe
aci_data_id102_pre_data <- cbind(aci_data_id102_pre[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id102_pre[,30]),
                                  mean(aci_data_id102_pre[,118]),
                                  fit_aci_id102_pre[[2]][1,1],
                                  fit_aci_id102_pre[[2]][1,2],
                                  fit_aci_id102_pre[[2]][2,1],
                                  fit_aci_id102_pre[[2]][2,2],
                                  fit_aci_id102_pre[[2]][3,1],
                                  fit_aci_id102_pre[[2]][3,2],
                                 fit_aci_id102_pre[[2]][4,1],
                                 fit_aci_id102_pre[[2]][4,2],
                                  fit_aci_id102_pre$RMSE,
                                  fit_aci_id102_pre$Ci_transition,
                                  fit_aci_id102_pre$citransition,
                                  fit_aci_id102_pre$Km,
                                  fit_aci_id102_pre$GammaStar,
                                  fit_aci_id102_pre$fitmethod,
                                  fit_aci_id102_pre$Tcorrect,
                                  fit_aci_id102_pre$fitTPU)
colnames(aci_data_id102_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id102_pre_data)

### plant id102 post_heatwave
aci_data_id102_post = subset(aci_data, id == ids[4] & meas.type == 'post_heatwave')
aci_data_id102_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id102_post)
#### fit aci curve
fit_aci_id102_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = TRUE,
                            fitmethod = 'bilinear',
                             data = aci_data_id102_post)
summary(fit_aci_id102_post)
#### plot
plot(fit_aci_id102_post)
#### add to dataframe
aci_data_id102_post_data <- cbind(aci_data_id102_post[1, c(10, 284, 9, 14, 16, 19)],
                                   mean(aci_data_id102_post[,30]),
                                   mean(aci_data_id102_post[,118]),
                                   fit_aci_id102_post[[2]][1,1],
                                   fit_aci_id102_post[[2]][1,2],
                                   fit_aci_id102_post[[2]][2,1],
                                   fit_aci_id102_post[[2]][2,2],
                                   fit_aci_id102_post[[2]][3,1],
                                   fit_aci_id102_post[[2]][3,2],
                                  fit_aci_id102_post[[2]][4,1],
                                  fit_aci_id102_post[[2]][4,2],
                                   fit_aci_id102_post$RMSE,
                                   fit_aci_id102_post$Ci_transition,
                                   fit_aci_id102_post$citransition,
                                   fit_aci_id102_post$Km,
                                   fit_aci_id102_post$GammaStar,
                                   fit_aci_id102_post$fitmethod,
                                   fit_aci_id102_post$Tcorrect,
                                   fit_aci_id102_post$fitTPU)
colnames(aci_data_id102_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                         'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id102_post_data)

##############################################################################
##############################################################################
###########id104
##############################################################################
##############################################################################
### plant id104 pre_heatwave
# aci_data_id104_pre = subset(aci_data, id == ids[5] & meas.type == 'pre_heatwave')
# aci_data_id104_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id104_pre)
# #### fit aci curve
# fit_aci_id104_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            citransition = 350,
#                            Tcorrect = FALSE,
#                            data = aci_data_id104_pre)
# summary(fit_aci_id104_pre)
# #### plot
# plot(fit_aci_id104_pre)
# #### add to dataframe
# aci_data_id104_pre_data <- cbind(aci_data_id104_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id104_pre[,30]),
#                                  mean(aci_data_id104_pre[,118]),
#                                  fit_aci_id104_pre[[2]][1,1],
#                                  fit_aci_id104_pre[[2]][1,2],
#                                  fit_aci_id104_pre[[2]][2,1],
#                                  fit_aci_id104_pre[[2]][2,2],
#                                  fit_aci_id104_pre[[2]][3,1],
#                                  fit_aci_id104_pre[[2]][3,2],
#                                  fit_aci_id104_pre$RMSE,
#                                  fit_aci_id104_pre$Ci_transition,
#                                  fit_aci_id104_pre$citransition,
#                                  fit_aci_id104_pre$Km,
#                                  fit_aci_id104_pre$GammaStar,
#                                  fit_aci_id104_pre$fitmethod,
#                                  fit_aci_id104_pre$Tcorrect,
#                                  fit_aci_id104_pre$fitTPU)
# colnames(aci_data_id104_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id104_pre_data)
# 
# ### plant id104 post_heatwave
# aci_data_id104_post = subset(aci_data, id == ids[5] & meas.type == 'post_heatwave')
# aci_data_id104_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id104_post)
# #### fit aci curve
# fit_aci_id104_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             citransition = 350,
#                             Tcorrect = FALSE,
#                             data = aci_data_id104_post)
# summary(fit_aci_id104_post)
# #### plot
# plot(fit_aci_id104_post)
# #### add to dataframe
# aci_data_id104_post_data <- cbind(aci_data_id104_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id104_post[,30]),
#                                   mean(aci_data_id104_post[,118]),
#                                   fit_aci_id104_post[[2]][1,1],
#                                   fit_aci_id104_post[[2]][1,2],
#                                   fit_aci_id104_post[[2]][2,1],
#                                   fit_aci_id104_post[[2]][2,2],
#                                   fit_aci_id104_post[[2]][3,1],
#                                   fit_aci_id104_post[[2]][3,2],
#                                   fit_aci_id104_post$RMSE,
#                                   fit_aci_id104_post$Ci_transition,
#                                   fit_aci_id104_post$citransition,
#                                   fit_aci_id104_post$Km,
#                                   fit_aci_id104_post$GammaStar,
#                                   fit_aci_id104_post$fitmethod,
#                                   fit_aci_id104_post$Tcorrect,
#                                   fit_aci_id104_post$fitTPU)
# colnames(aci_data_id104_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id104_post_data)

##############################################################################
##############################################################################
###########id105
##############################################################################
##############################################################################
### plant id105 pre_heatwave
aci_data_id105_pre = subset(aci_data, id == ids[6] & meas.type == 'pre_heatwave')
aci_data_id105_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id105_pre)
#### fit aci curve
fit_aci_id105_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = TRUE,
                           fitmethod = 'bilinear',
                           data = aci_data_id105_pre)
summary(fit_aci_id105_pre)
#### plot
plot(fit_aci_id105_pre)
#### add to dataframe
aci_data_id105_pre_data <- cbind(aci_data_id105_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id105_pre[,30]),
                                 mean(aci_data_id105_pre[,118]),
                                 fit_aci_id105_pre[[2]][1,1],
                                 fit_aci_id105_pre[[2]][1,2],
                                 fit_aci_id105_pre[[2]][2,1],
                                 fit_aci_id105_pre[[2]][2,2],
                                 fit_aci_id105_pre[[2]][3,1],
                                 fit_aci_id105_pre[[2]][3,2],
                                 fit_aci_id105_pre[[2]][4,1],
                                 fit_aci_id105_pre[[2]][4,2],
                                 fit_aci_id105_pre$RMSE,
                                 fit_aci_id105_pre$Ci_transition,
                                 fit_aci_id105_pre$citransition,
                                 fit_aci_id105_pre$Km,
                                 fit_aci_id105_pre$GammaStar,
                                 fit_aci_id105_pre$fitmethod,
                                 fit_aci_id105_pre$Tcorrect,
                                 fit_aci_id105_pre$fitTPU)
colnames(aci_data_id105_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id105_pre_data)

### plant id105 post_heatwave
aci_data_id105_post = subset(aci_data, id == ids[6] & meas.type == 'post_heatwave')
aci_data_id105_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id105_post)
#### fit aci curve
fit_aci_id105_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = TRUE,
                            fitmethod = 'bilinear',
                            data = aci_data_id105_post)
summary(fit_aci_id105_post)
#### plot
plot(fit_aci_id105_post)
#### add to dataframe
aci_data_id105_post_data <- cbind(aci_data_id105_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id105_post[,30]),
                                  mean(aci_data_id105_post[,118]),
                                  fit_aci_id105_post[[2]][1,1],
                                  fit_aci_id105_post[[2]][1,2],
                                  fit_aci_id105_post[[2]][2,1],
                                  fit_aci_id105_post[[2]][2,2],
                                  fit_aci_id105_post[[2]][3,1],
                                  fit_aci_id105_post[[2]][3,2],
                                  fit_aci_id105_post[[2]][4,1],
                                  fit_aci_id105_post[[2]][4,2],
                                  fit_aci_id105_post$RMSE,
                                  fit_aci_id105_post$Ci_transition,
                                  fit_aci_id105_post$citransition,
                                  fit_aci_id105_post$Km,
                                  fit_aci_id105_post$GammaStar,
                                  fit_aci_id105_post$fitmethod,
                                  fit_aci_id105_post$Tcorrect,
                                  fit_aci_id105_post$fitTPU)
colnames(aci_data_id105_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id105_post_data)

##############################################################################
##############################################################################
###########id11
##############################################################################
##############################################################################
### plant id11 pre_heatwave
aci_data_id11_pre = subset(aci_data, id == ids[7] & meas.type == 'pre_heatwave')
aci_data_id11_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id11_pre)
#### fit aci curve
fit_aci_id11_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                          # citransition = 300,
                          Tcorrect = FALSE,
                          fitTPU = TRUE,
                          fitmethod = 'bilinear',
                           data = aci_data_id11_pre)
summary(fit_aci_id11_pre)
#### plot
plot(fit_aci_id11_pre)
#### add to dataframe
aci_data_id11_pre_data <- cbind(aci_data_id11_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id11_pre[,30]),
                                 mean(aci_data_id11_pre[,118]),
                                 fit_aci_id11_pre[[2]][1,1],
                                 fit_aci_id11_pre[[2]][1,2],
                                 fit_aci_id11_pre[[2]][2,1],
                                 fit_aci_id11_pre[[2]][2,2],
                                 fit_aci_id11_pre[[2]][3,1],
                                 fit_aci_id11_pre[[2]][3,2],
                                fit_aci_id11_pre[[2]][4,1],
                                fit_aci_id11_pre[[2]][4,2],
                                 fit_aci_id11_pre$RMSE,
                                 fit_aci_id11_pre$Ci_transition,
                                 fit_aci_id11_pre$citransition,
                                 fit_aci_id11_pre$Km,
                                 fit_aci_id11_pre$GammaStar,
                                 fit_aci_id11_pre$fitmethod,
                                 fit_aci_id11_pre$Tcorrect,
                                 fit_aci_id11_pre$fitTPU)
colnames(aci_data_id11_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id11_pre_data)

### plant id11 post_heatwave
aci_data_id11_post = subset(aci_data, id == ids[7] & meas.type == 'post_heatwave')
aci_data_id11_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id11_post)
#### fit aci curve
fit_aci_id11_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = TRUE,
                           fitmethod = 'bilinear',
                            data = aci_data_id11_post)
summary(fit_aci_id11_post)
#### plot
plot(fit_aci_id11_post)
#### add to dataframe
aci_data_id11_post_data <- cbind(aci_data_id11_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id11_post[,30]),
                                  mean(aci_data_id11_post[,118]),
                                  fit_aci_id11_post[[2]][1,1],
                                  fit_aci_id11_post[[2]][1,2],
                                  fit_aci_id11_post[[2]][2,1],
                                  fit_aci_id11_post[[2]][2,2],
                                  fit_aci_id11_post[[2]][3,1],
                                  fit_aci_id11_post[[2]][3,2],
                                 fit_aci_id11_post[[2]][4,1],
                                 fit_aci_id11_post[[2]][4,2],
                                  fit_aci_id11_post$RMSE,
                                  fit_aci_id11_post$Ci_transition,
                                  fit_aci_id11_post$citransition,
                                  fit_aci_id11_post$Km,
                                  fit_aci_id11_post$GammaStar,
                                  fit_aci_id11_post$fitmethod,
                                  fit_aci_id11_post$Tcorrect,
                                  fit_aci_id11_post$fitTPU)
colnames(aci_data_id11_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id11_post_data)

##############################################################################
##############################################################################
###########id110
##############################################################################
##############################################################################
### plant id110 pre_heatwave
aci_data_id110_pre = subset(aci_data, id == ids[8] & meas.type == 'pre_heatwave')
aci_data_id110_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id110_pre)
#### fit aci curve
fit_aci_id110_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = TRUE,
                           fitmethod = 'bilinear',
                          data = aci_data_id110_pre)
summary(fit_aci_id110_pre)
#### plot
plot(fit_aci_id110_pre)
#### add to dataframe
aci_data_id110_pre_data <- cbind(aci_data_id110_pre[1, c(10, 284, 9, 14, 16, 19)],
                                mean(aci_data_id110_pre[,30]),
                                mean(aci_data_id110_pre[,118]),
                                fit_aci_id110_pre[[2]][1,1],
                                fit_aci_id110_pre[[2]][1,2],
                                fit_aci_id110_pre[[2]][2,1],
                                fit_aci_id110_pre[[2]][2,2],
                                fit_aci_id110_pre[[2]][3,1],
                                fit_aci_id110_pre[[2]][3,2],
                                fit_aci_id110_pre[[2]][4,1],
                                fit_aci_id110_pre[[2]][4,2],
                                fit_aci_id110_pre$RMSE,
                                fit_aci_id110_pre$Ci_transition,
                                fit_aci_id110_pre$citransition,
                                fit_aci_id110_pre$Km,
                                fit_aci_id110_pre$GammaStar,
                                fit_aci_id110_pre$fitmethod,
                                fit_aci_id110_pre$Tcorrect,
                                fit_aci_id110_pre$fitTPU)
colnames(aci_data_id110_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                      'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                      'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                      'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                      'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                      'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id110_pre_data)

### plant id110 post_heatwave
aci_data_id110_post = subset(aci_data, id == ids[8] & meas.type == 'post_heatwave')
aci_data_id110_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id110_post)
#### fit aci curve
fit_aci_id110_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = TRUE,
                            fitmethod = 'bilinear',
                           data = aci_data_id110_post)
summary(fit_aci_id110_post)
#### plot
plot(fit_aci_id110_post)
#### add to dataframe
aci_data_id110_post_data <- cbind(aci_data_id110_post[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id110_post[,30]),
                                 mean(aci_data_id110_post[,118]),
                                 fit_aci_id110_post[[2]][1,1],
                                 fit_aci_id110_post[[2]][1,2],
                                 fit_aci_id110_post[[2]][2,1],
                                 fit_aci_id110_post[[2]][2,2],
                                 fit_aci_id110_post[[2]][3,1],
                                 fit_aci_id110_post[[2]][3,2],
                                 fit_aci_id110_post[[2]][4,1],
                                 fit_aci_id110_post[[2]][4,2],
                                 fit_aci_id110_post$RMSE,
                                 fit_aci_id110_post$Ci_transition,
                                 fit_aci_id110_post$citransition,
                                 fit_aci_id110_post$Km,
                                 fit_aci_id110_post$GammaStar,
                                 fit_aci_id110_post$fitmethod,
                                 fit_aci_id110_post$Tcorrect,
                                 fit_aci_id110_post$fitTPU)
colnames(aci_data_id110_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id110_post_data)

##############################################################################
##############################################################################
###########id112
##############################################################################
##############################################################################
### plant id112 pre_heatwave
aci_data_id112_pre = subset(aci_data, id == ids[9] & meas.type == 'pre_heatwave')
aci_data_id112_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id112_pre)
#### fit aci curve
fit_aci_id112_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = TRUE,
                           fitmethod = 'bilinear',
                           data = aci_data_id112_pre)
summary(fit_aci_id112_pre)
#### plot
plot(fit_aci_id112_pre)
#### add to dataframe
aci_data_id112_pre_data <- cbind(aci_data_id112_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id112_pre[,30]),
                                 mean(aci_data_id112_pre[,118]),
                                 fit_aci_id112_pre[[2]][1,1],
                                 fit_aci_id112_pre[[2]][1,2],
                                 fit_aci_id112_pre[[2]][2,1],
                                 fit_aci_id112_pre[[2]][2,2],
                                 fit_aci_id112_pre[[2]][3,1],
                                 fit_aci_id112_pre[[2]][3,2],
                                 fit_aci_id112_pre[[2]][4,1],
                                 fit_aci_id112_pre[[2]][4,2],
                                 fit_aci_id112_pre$RMSE,
                                 fit_aci_id112_pre$Ci_transition,
                                 fit_aci_id112_pre$citransition,
                                 fit_aci_id112_pre$Km,
                                 fit_aci_id112_pre$GammaStar,
                                 fit_aci_id112_pre$fitmethod,
                                 fit_aci_id112_pre$Tcorrect,
                                 fit_aci_id112_pre$fitTPU)
colnames(aci_data_id112_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id112_pre_data)

### plant id112 post_heatwave
aci_data_id112_post = subset(aci_data, id == ids[9] & meas.type == 'post_heatwave')
aci_data_id112_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id112_post)
#### fit aci curve
fit_aci_id112_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = TRUE,
                            fitmethod = 'bilinear',
                            data = aci_data_id112_post)
summary(fit_aci_id112_post)
#### plot
plot(fit_aci_id112_post)
#### add to dataframe
aci_data_id112_post_data <- cbind(aci_data_id112_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id112_post[,30]),
                                  mean(aci_data_id112_post[,118]),
                                  fit_aci_id112_post[[2]][1,1],
                                  fit_aci_id112_post[[2]][1,2],
                                  fit_aci_id112_post[[2]][2,1],
                                  fit_aci_id112_post[[2]][2,2],
                                  fit_aci_id112_post[[2]][3,1],
                                  fit_aci_id112_post[[2]][3,2],
                                  fit_aci_id112_post[[2]][4,1],
                                  fit_aci_id112_post[[2]][4,2],
                                  fit_aci_id112_post$RMSE,
                                  fit_aci_id112_post$Ci_transition,
                                  fit_aci_id112_post$citransition,
                                  fit_aci_id112_post$Km,
                                  fit_aci_id112_post$GammaStar,
                                  fit_aci_id112_post$fitmethod,
                                  fit_aci_id112_post$Tcorrect,
                                  fit_aci_id112_post$fitTPU)
colnames(aci_data_id112_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id112_post_data)

##############################################################################
##############################################################################
###########id113
##############################################################################
##############################################################################
### plant id113 pre_heatwave
aci_data_id113_pre = subset(aci_data, id == ids[10] & meas.type == 'pre_heatwave')
aci_data_id113_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id113_pre)
#### fit aci curve
fit_aci_id113_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = TRUE,
                           fitmethod = 'bilinear',
                           data = aci_data_id113_pre)
summary(fit_aci_id113_pre)
#### plot
plot(fit_aci_id113_pre)
#### add to dataframe
aci_data_id113_pre_data <- cbind(aci_data_id113_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id113_pre[,30]),
                                 mean(aci_data_id113_pre[,118]),
                                 fit_aci_id113_pre[[2]][1,1],
                                 fit_aci_id113_pre[[2]][1,2],
                                 fit_aci_id113_pre[[2]][2,1],
                                 fit_aci_id113_pre[[2]][2,2],
                                 fit_aci_id113_pre[[2]][3,1],
                                 fit_aci_id113_pre[[2]][3,2],
                                 fit_aci_id113_pre[[2]][4,1],
                                 fit_aci_id113_pre[[2]][4,2],
                                 fit_aci_id113_pre$RMSE,
                                 fit_aci_id113_pre$Ci_transition,
                                 fit_aci_id113_pre$citransition,
                                 fit_aci_id113_pre$Km,
                                 fit_aci_id113_pre$GammaStar,
                                 fit_aci_id113_pre$fitmethod,
                                 fit_aci_id113_pre$Tcorrect,
                                 fit_aci_id113_pre$fitTPU)
colnames(aci_data_id113_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id113_pre_data)

### plant id113 post_heatwave
aci_data_id113_post = subset(aci_data, id == ids[10] & meas.type == 'post_heatwave')
aci_data_id113_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id113_post)
#### fit aci curve
fit_aci_id113_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = TRUE,
                            fitmethod = 'bilinear',
                            data = aci_data_id113_post)
summary(fit_aci_id113_post)
#### plot
plot(fit_aci_id113_post)
#### add to dataframe
aci_data_id113_post_data <- cbind(aci_data_id113_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id113_post[,30]),
                                  mean(aci_data_id113_post[,118]),
                                  fit_aci_id113_post[[2]][1,1],
                                  fit_aci_id113_post[[2]][1,2],
                                  fit_aci_id113_post[[2]][2,1],
                                  fit_aci_id113_post[[2]][2,2],
                                  fit_aci_id113_post[[2]][3,1],
                                  fit_aci_id113_post[[2]][3,2],
                                  fit_aci_id113_post[[2]][4,1],
                                  fit_aci_id113_post[[2]][4,2],
                                  fit_aci_id113_post$RMSE,
                                  fit_aci_id113_post$Ci_transition,
                                  fit_aci_id113_post$citransition,
                                  fit_aci_id113_post$Km,
                                  fit_aci_id113_post$GammaStar,
                                  fit_aci_id113_post$fitmethod,
                                  fit_aci_id113_post$Tcorrect,
                                  fit_aci_id113_post$fitTPU)
colnames(aci_data_id113_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id113_post_data)

##############################################################################
##############################################################################
###########id114
##############################################################################
##############################################################################
### plant id114 pre_heatwave
aci_data_id114_pre = subset(aci_data, id == ids[11] & meas.type == 'pre_heatwave')
aci_data_id114_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id114_pre)
#### fit aci curve
fit_aci_id114_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = TRUE,
                           fitmethod = 'bilinear',
                           data = aci_data_id114_pre)
summary(fit_aci_id114_pre)
#### plot
plot(fit_aci_id114_pre)
#### add to dataframe
aci_data_id114_pre_data <- cbind(aci_data_id114_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id114_pre[,30]),
                                 mean(aci_data_id114_pre[,118]),
                                 fit_aci_id114_pre[[2]][1,1],
                                 fit_aci_id114_pre[[2]][1,2],
                                 fit_aci_id114_pre[[2]][2,1],
                                 fit_aci_id114_pre[[2]][2,2],
                                 fit_aci_id114_pre[[2]][3,1],
                                 fit_aci_id114_pre[[2]][3,2],
                                 fit_aci_id114_pre[[2]][4,1],
                                 fit_aci_id114_pre[[2]][4,2],
                                 fit_aci_id114_pre$RMSE,
                                 fit_aci_id114_pre$Ci_transition,
                                 fit_aci_id114_pre$citransition,
                                 fit_aci_id114_pre$Km,
                                 fit_aci_id114_pre$GammaStar,
                                 fit_aci_id114_pre$fitmethod,
                                 fit_aci_id114_pre$Tcorrect,
                                 fit_aci_id114_pre$fitTPU)
colnames(aci_data_id114_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id114_pre_data)

### plant id114 post_heatwave
aci_data_id114_post = subset(aci_data, id == ids[11] & meas.type == 'post_heatwave')
aci_data_id114_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id114_post)
#### fit aci curve
fit_aci_id114_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = TRUE,
                            fitmethod = 'bilinear',
                            data = aci_data_id114_post)
summary(fit_aci_id114_post)
#### plot
plot(fit_aci_id114_post)
#### add to dataframe
aci_data_id114_post_data <- cbind(aci_data_id114_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id114_post[,30]),
                                  mean(aci_data_id114_post[,118]),
                                  fit_aci_id114_post[[2]][1,1],
                                  fit_aci_id114_post[[2]][1,2],
                                  fit_aci_id114_post[[2]][2,1],
                                  fit_aci_id114_post[[2]][2,2],
                                  fit_aci_id114_post[[2]][3,1],
                                  fit_aci_id114_post[[2]][3,2],
                                  fit_aci_id114_post[[2]][4,1],
                                  fit_aci_id114_post[[2]][4,2],
                                  fit_aci_id114_post$RMSE,
                                  fit_aci_id114_post$Ci_transition,
                                  fit_aci_id114_post$citransition,
                                  fit_aci_id114_post$Km,
                                  fit_aci_id114_post$GammaStar,
                                  fit_aci_id114_post$fitmethod,
                                  fit_aci_id114_post$Tcorrect,
                                  fit_aci_id114_post$fitTPU)
colnames(aci_data_id114_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id114_post_data)

##############################################################################
##############################################################################
###########id118
##############################################################################
##############################################################################
### plant id118 pre_heatwave
# aci_data_id118_pre = subset(aci_data, id == ids[12] & meas.type == 'pre_heatwave')
# aci_data_id118_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id118_pre)
# #### fit aci curve
# fit_aci_id118_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            citransition = 450,
#                            Tcorrect = FALSE,
#                            data = aci_data_id118_pre)
# summary(fit_aci_id118_pre)
# #### plot
# plot(fit_aci_id118_pre)
# #### add to dataframe
# aci_data_id118_pre_data <- cbind(aci_data_id118_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id118_pre[,30]),
#                                  mean(aci_data_id118_pre[,118]),
#                                  fit_aci_id118_pre[[2]][1,1],
#                                  fit_aci_id118_pre[[2]][1,2],
#                                  fit_aci_id118_pre[[2]][2,1],
#                                  fit_aci_id118_pre[[2]][2,2],
#                                  fit_aci_id118_pre[[2]][3,1],
#                                  fit_aci_id118_pre[[2]][3,2],
#                                  fit_aci_id118_pre$RMSE,
#                                  fit_aci_id118_pre$Ci_transition,
#                                  fit_aci_id118_pre$citransition,
#                                  fit_aci_id118_pre$Km,
#                                  fit_aci_id118_pre$GammaStar,
#                                  fit_aci_id118_pre$fitmethod,
#                                  fit_aci_id118_pre$Tcorrect,
#                                  fit_aci_id118_pre$fitTPU)
# colnames(aci_data_id118_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id118_pre_data)
# 
# ### plant id118 post_heatwave
# aci_data_id118_post = subset(aci_data, id == ids[12] & meas.type == 'post_heatwave')
# aci_data_id118_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id118_post)
# #### fit aci curve
# fit_aci_id118_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             citransition = 400,
#                             Tcorrect = FALSE,
#                             data = aci_data_id118_post)
# summary(fit_aci_id118_post)
# #### plot
# plot(fit_aci_id118_post)
# #### add to dataframe
# aci_data_id118_post_data <- cbind(aci_data_id118_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id118_post[,30]),
#                                   mean(aci_data_id118_post[,118]),
#                                   fit_aci_id118_post[[2]][1,1],
#                                   fit_aci_id118_post[[2]][1,2],
#                                   fit_aci_id118_post[[2]][2,1],
#                                   fit_aci_id118_post[[2]][2,2],
#                                   fit_aci_id118_post[[2]][3,1],
#                                   fit_aci_id118_post[[2]][3,2],
#                                   fit_aci_id118_post$RMSE,
#                                   fit_aci_id118_post$Ci_transition,
#                                   fit_aci_id118_post$citransition,
#                                   fit_aci_id118_post$Km,
#                                   fit_aci_id118_post$GammaStar,
#                                   fit_aci_id118_post$fitmethod,
#                                   fit_aci_id118_post$Tcorrect,
#                                   fit_aci_id118_post$fitTPU)
# colnames(aci_data_id118_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id118_post_data)

##############################################################################
##############################################################################
###########id118
##############################################################################
##############################################################################
### plant id118 pre_heatwave
aci_data_id118_pre = subset(aci_data, id == ids[13] & meas.type == 'pre_heatwave')
aci_data_id118_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id118_pre)
#### fit aci curve
fit_aci_id118_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = TRUE,
                           fitmethod = 'bilinear',
                           data = aci_data_id118_pre)
summary(fit_aci_id118_pre)
#### plot
plot(fit_aci_id118_pre)
#### add to dataframe
aci_data_id118_pre_data <- cbind(aci_data_id118_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id118_pre[,30]),
                                 mean(aci_data_id118_pre[,118]),
                                 fit_aci_id118_pre[[2]][1,1],
                                 fit_aci_id118_pre[[2]][1,2],
                                 fit_aci_id118_pre[[2]][2,1],
                                 fit_aci_id118_pre[[2]][2,2],
                                 fit_aci_id118_pre[[2]][3,1],
                                 fit_aci_id118_pre[[2]][3,2],
                                 fit_aci_id118_pre[[2]][4,1],
                                 fit_aci_id118_pre[[2]][4,2],
                                 fit_aci_id118_pre$RMSE,
                                 fit_aci_id118_pre$Ci_transition,
                                 fit_aci_id118_pre$citransition,
                                 fit_aci_id118_pre$Km,
                                 fit_aci_id118_pre$GammaStar,
                                 fit_aci_id118_pre$fitmethod,
                                 fit_aci_id118_pre$Tcorrect,
                                 fit_aci_id118_pre$fitTPU)
colnames(aci_data_id118_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id118_pre_data)

### plant id118 post_heatwave
aci_data_id118_post = subset(aci_data, id == ids[12] & meas.type == 'post_heatwave')
aci_data_id118_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id118_post)
#### fit aci curve
fit_aci_id118_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = TRUE,
                            fitmethod = 'bilinear',
                            data = aci_data_id118_post)
summary(fit_aci_id118_post)
#### plot
plot(fit_aci_id118_post)
#### add to dataframe
aci_data_id118_post_data <- cbind(aci_data_id118_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id118_post[,30]),
                                  mean(aci_data_id118_post[,118]),
                                  fit_aci_id118_post[[2]][1,1],
                                  fit_aci_id118_post[[2]][1,2],
                                  fit_aci_id118_post[[2]][2,1],
                                  fit_aci_id118_post[[2]][2,2],
                                  fit_aci_id118_post[[2]][3,1],
                                  fit_aci_id118_post[[2]][3,2],
                                  fit_aci_id118_post[[2]][4,1],
                                  fit_aci_id118_post[[2]][4,2],
                                  fit_aci_id118_post$RMSE,
                                  fit_aci_id118_post$Ci_transition,
                                  fit_aci_id118_post$citransition,
                                  fit_aci_id118_post$Km,
                                  fit_aci_id118_post$GammaStar,
                                  fit_aci_id118_post$fitmethod,
                                  fit_aci_id118_post$Tcorrect,
                                  fit_aci_id118_post$fitTPU)
colnames(aci_data_id118_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id118_post_data)

##############################################################################
##############################################################################
###########id121
##############################################################################
##############################################################################
### plant id121 pre_heatwave
# aci_data_id121_pre = subset(aci_data, id == ids[14] & meas.type == 'pre_heatwave')
# aci_data_id121_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id121_pre)
# #### fit aci curve
# fit_aci_id121_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            citransition = 350,
#                            Tcorrect = FALSE,
#                            data = aci_data_id121_pre)
# summary(fit_aci_id121_pre)
# #### plot
# plot(fit_aci_id121_pre)
# #### add to dataframe
# aci_data_id121_pre_data <- cbind(aci_data_id121_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id121_pre[,30]),
#                                  mean(aci_data_id121_pre[,118]),
#                                  fit_aci_id121_pre[[2]][1,1],
#                                  fit_aci_id121_pre[[2]][1,2],
#                                  fit_aci_id121_pre[[2]][2,1],
#                                  fit_aci_id121_pre[[2]][2,2],
#                                  fit_aci_id121_pre[[2]][3,1],
#                                  fit_aci_id121_pre[[2]][3,2],
#                                  fit_aci_id121_pre$RMSE,
#                                  fit_aci_id121_pre$Ci_transition,
#                                  fit_aci_id121_pre$citransition,
#                                  fit_aci_id121_pre$Km,
#                                  fit_aci_id121_pre$GammaStar,
#                                  fit_aci_id121_pre$fitmethod,
#                                  fit_aci_id121_pre$Tcorrect,
#                                  fit_aci_id121_pre$fitTPU)
# colnames(aci_data_id121_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id121_pre_data)
# 
# ### plant id121 post_heatwave
# aci_data_id121_post = subset(aci_data, id == ids[14] & meas.type == 'post_heatwave')
# aci_data_id121_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id121_post)
# #### fit aci curve
# fit_aci_id121_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             citransition = 350,
#                             Tcorrect = FALSE,
#                             data = aci_data_id121_post)
# summary(fit_aci_id121_post)
# #### plot
# plot(fit_aci_id121_post)
# #### add to dataframe
# aci_data_id121_post_data <- cbind(aci_data_id121_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id121_post[,30]),
#                                   mean(aci_data_id121_post[,118]),
#                                   fit_aci_id121_post[[2]][1,1],
#                                   fit_aci_id121_post[[2]][1,2],
#                                   fit_aci_id121_post[[2]][2,1],
#                                   fit_aci_id121_post[[2]][2,2],
#                                   fit_aci_id121_post[[2]][3,1],
#                                   fit_aci_id121_post[[2]][3,2],
#                                   fit_aci_id121_post$RMSE,
#                                   fit_aci_id121_post$Ci_transition,
#                                   fit_aci_id121_post$citransition,
#                                   fit_aci_id121_post$Km,
#                                   fit_aci_id121_post$GammaStar,
#                                   fit_aci_id121_post$fitmethod,
#                                   fit_aci_id121_post$Tcorrect,
#                                   fit_aci_id121_post$fitTPU)
# colnames(aci_data_id121_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id121_post_data)

##############################################################################
##############################################################################
###########id122
##############################################################################
##############################################################################
### plant id122 pre_heatwave
aci_data_id122_pre = subset(aci_data, id == ids[15] & meas.type == 'pre_heatwave')
aci_data_id122_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id122_pre)
#### fit aci curve
fit_aci_id122_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = TRUE,
                           fitmethod = 'bilinear',
                           data = aci_data_id122_pre)
summary(fit_aci_id122_pre)
#### plot
plot(fit_aci_id122_pre)
#### add to dataframe
aci_data_id122_pre_data <- cbind(aci_data_id122_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id122_pre[,30]),
                                 mean(aci_data_id122_pre[,118]),
                                 fit_aci_id122_pre[[2]][1,1],
                                 fit_aci_id122_pre[[2]][1,2],
                                 fit_aci_id122_pre[[2]][2,1],
                                 fit_aci_id122_pre[[2]][2,2],
                                 fit_aci_id122_pre[[2]][3,1],
                                 fit_aci_id122_pre[[2]][3,2],
                                 fit_aci_id122_pre[[2]][4,1],
                                 fit_aci_id122_pre[[2]][4,2],
                                 fit_aci_id122_pre$RMSE,
                                 fit_aci_id122_pre$Ci_transition,
                                 fit_aci_id122_pre$citransition,
                                 fit_aci_id122_pre$Km,
                                 fit_aci_id122_pre$GammaStar,
                                 fit_aci_id122_pre$fitmethod,
                                 fit_aci_id122_pre$Tcorrect,
                                 fit_aci_id122_pre$fitTPU)
colnames(aci_data_id122_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id122_pre_data)

### plant id122 post_heatwave
aci_data_id122_post = subset(aci_data, id == ids[15] & meas.type == 'post_heatwave')
aci_data_id122_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id122_post)
#### fit aci curve
fit_aci_id122_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = TRUE,
                            fitmethod = 'bilinear',
                            data = aci_data_id122_post)
summary(fit_aci_id122_post)
#### plot
plot(fit_aci_id122_post)
#### add to dataframe
aci_data_id122_post_data <- cbind(aci_data_id122_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id122_post[,30]),
                                  mean(aci_data_id122_post[,118]),
                                  fit_aci_id122_post[[2]][1,1],
                                  fit_aci_id122_post[[2]][1,2],
                                  fit_aci_id122_post[[2]][2,1],
                                  fit_aci_id122_post[[2]][2,2],
                                  fit_aci_id122_post[[2]][3,1],
                                  fit_aci_id122_post[[2]][3,2],
                                  fit_aci_id122_post[[2]][4,1],
                                  fit_aci_id122_post[[2]][4,2],
                                  fit_aci_id122_post$RMSE,
                                  fit_aci_id122_post$Ci_transition,
                                  fit_aci_id122_post$citransition,
                                  fit_aci_id122_post$Km,
                                  fit_aci_id122_post$GammaStar,
                                  fit_aci_id122_post$fitmethod,
                                  fit_aci_id122_post$Tcorrect,
                                  fit_aci_id122_post$fitTPU)
colnames(aci_data_id122_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id122_post_data)

##############################################################################
##############################################################################
###########id122
##############################################################################
##############################################################################
### plant id122 pre_heatwave
aci_data_id122_pre = subset(aci_data, id == ids[15] & meas.type == 'pre_heatwave')
aci_data_id122_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id122_pre)
#### fit aci curve
fit_aci_id122_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = TRUE,
                           fitmethod = 'bilinear',
                           data = aci_data_id122_pre)
summary(fit_aci_id122_pre)
#### plot
plot(fit_aci_id122_pre)
#### add to dataframe
aci_data_id122_pre_data <- cbind(aci_data_id122_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id122_pre[,30]),
                                 mean(aci_data_id122_pre[,118]),
                                 fit_aci_id122_pre[[2]][1,1],
                                 fit_aci_id122_pre[[2]][1,2],
                                 fit_aci_id122_pre[[2]][2,1],
                                 fit_aci_id122_pre[[2]][2,2],
                                 fit_aci_id122_pre[[2]][3,1],
                                 fit_aci_id122_pre[[2]][3,2],
                                 fit_aci_id122_pre[[2]][4,1],
                                 fit_aci_id122_pre[[2]][4,2],
                                 fit_aci_id122_pre$RMSE,
                                 fit_aci_id122_pre$Ci_transition,
                                 fit_aci_id122_pre$citransition,
                                 fit_aci_id122_pre$Km,
                                 fit_aci_id122_pre$GammaStar,
                                 fit_aci_id122_pre$fitmethod,
                                 fit_aci_id122_pre$Tcorrect,
                                 fit_aci_id122_pre$fitTPU)
colnames(aci_data_id122_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id122_pre_data)

### plant id122 post_heatwave
aci_data_id122_post = subset(aci_data, id == ids[15] & meas.type == 'post_heatwave')
aci_data_id122_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id122_post)
#### fit aci curve
fit_aci_id122_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = TRUE,
                            fitmethod = 'bilinear',
                            data = aci_data_id122_post)
summary(fit_aci_id122_post)
#### plot
plot(fit_aci_id122_post)
#### add to dataframe
aci_data_id122_post_data <- cbind(aci_data_id122_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id122_post[,30]),
                                  mean(aci_data_id122_post[,118]),
                                  fit_aci_id122_post[[2]][1,1],
                                  fit_aci_id122_post[[2]][1,2],
                                  fit_aci_id122_post[[2]][2,1],
                                  fit_aci_id122_post[[2]][2,2],
                                  fit_aci_id122_post[[2]][3,1],
                                  fit_aci_id122_post[[2]][3,2],
                                  fit_aci_id122_post[[2]][4,1],
                                  fit_aci_id122_post[[2]][4,2],
                                  fit_aci_id122_post$RMSE,
                                  fit_aci_id122_post$Ci_transition,
                                  fit_aci_id122_post$citransition,
                                  fit_aci_id122_post$Km,
                                  fit_aci_id122_post$GammaStar,
                                  fit_aci_id122_post$fitmethod,
                                  fit_aci_id122_post$Tcorrect,
                                  fit_aci_id122_post$fitTPU)
colnames(aci_data_id122_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id122_post_data)



##############################################################################
##############################################################################
###########id129
##############################################################################
##############################################################################
### plant id129 pre_heatwave
aci_data_id129_pre = subset(aci_data, id == ids[16] & meas.type == 'pre_heatwave')[c(1:95),]
aci_data_id129_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id129_pre)
#### fit aci curve
fit_aci_id129_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = TRUE,
                           fitmethod = 'bilinear',
                           data = aci_data_id129_pre)
summary(fit_aci_id129_pre)
#### plot
plot(fit_aci_id129_pre)
#### add to dataframe
aci_data_id129_pre_data <- cbind(aci_data_id129_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id129_pre[,30]),
                                 mean(aci_data_id129_pre[,118]),
                                 fit_aci_id129_pre[[2]][1,1],
                                 fit_aci_id129_pre[[2]][1,2],
                                 fit_aci_id129_pre[[2]][2,1],
                                 fit_aci_id129_pre[[2]][2,2],
                                 fit_aci_id129_pre[[2]][3,1],
                                 fit_aci_id129_pre[[2]][3,2],
                                 fit_aci_id129_pre[[2]][4,1],
                                 fit_aci_id129_pre[[2]][4,2],
                                 fit_aci_id129_pre$RMSE,
                                 fit_aci_id129_pre$Ci_transition,
                                 fit_aci_id129_pre$citransition,
                                 fit_aci_id129_pre$Km,
                                 fit_aci_id129_pre$GammaStar,
                                 fit_aci_id129_pre$fitmethod,
                                 fit_aci_id129_pre$Tcorrect,
                                 fit_aci_id129_pre$fitTPU)
colnames(aci_data_id129_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id129_pre_data)

### plant id129 post_heatwave
aci_data_id129_post = subset(aci_data, id == ids[16] & meas.type == 'post_heatwave')
aci_data_id129_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id129_post)
#### fit aci curve
fit_aci_id129_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = TRUE,
                            fitmethod = 'bilinear',
                            data = aci_data_id129_post)
summary(fit_aci_id129_post)
#### plot
plot(fit_aci_id129_post)
#### add to dataframe
aci_data_id129_post_data <- cbind(aci_data_id129_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id129_post[,30]),
                                  mean(aci_data_id129_post[,118]),
                                  fit_aci_id129_post[[2]][1,1],
                                  fit_aci_id129_post[[2]][1,2],
                                  fit_aci_id129_post[[2]][2,1],
                                  fit_aci_id129_post[[2]][2,2],
                                  fit_aci_id129_post[[2]][3,1],
                                  fit_aci_id129_post[[2]][3,2],
                                  fit_aci_id129_post[[2]][4,1],
                                  fit_aci_id129_post[[2]][4,2],
                                  fit_aci_id129_post$RMSE,
                                  fit_aci_id129_post$Ci_transition,
                                  fit_aci_id129_post$citransition,
                                  fit_aci_id129_post$Km,
                                  fit_aci_id129_post$GammaStar,
                                  fit_aci_id129_post$fitmethod,
                                  fit_aci_id129_post$Tcorrect,
                                  fit_aci_id129_post$fitTPU)
colnames(aci_data_id129_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id129_post_data)

##############################################################################
##############################################################################
###########id13
##############################################################################
##############################################################################
### plant id13 pre_heatwave
# aci_data_id13_pre = subset(aci_data, id == ids[17] & meas.type == 'pre_heatwave')
# aci_data_id13_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id13_pre)
# #### fit aci curve
# fit_aci_id13_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = TRUE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id13_pre)
# summary(fit_aci_id13_pre)
# #### plot
# plot(fit_aci_id13_pre)
# #### add to dataframe
# aci_data_id13_pre_data <- cbind(aci_data_id13_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id13_pre[,30]),
#                                  mean(aci_data_id13_pre[,118]),
#                                  fit_aci_id13_pre[[2]][1,1],
#                                  fit_aci_id13_pre[[2]][1,2],
#                                  fit_aci_id13_pre[[2]][2,1],
#                                  fit_aci_id13_pre[[2]][2,2],
#                                  fit_aci_id13_pre[[2]][3,1],
#                                  fit_aci_id13_pre[[2]][3,2],
#                                  fit_aci_id13_pre[[2]][4,1],
#                                  fit_aci_id13_pre[[2]][4,2],
#                                  fit_aci_id13_pre$RMSE,
#                                  fit_aci_id13_pre$Ci_transition,
#                                  fit_aci_id13_pre$citransition,
#                                  fit_aci_id13_pre$Km,
#                                  fit_aci_id13_pre$GammaStar,
#                                  fit_aci_id13_pre$fitmethod,
#                                  fit_aci_id13_pre$Tcorrect,
#                                  fit_aci_id13_pre$fitTPU)
# colnames(aci_data_id13_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id13_pre_data)
# 
# ### plant id13 post_heatwave
# aci_data_id13_post = subset(aci_data, id == ids[17] & meas.type == 'post_heatwave')
# aci_data_id13_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id13_post)
# #### fit aci curve
# fit_aci_id13_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = TRUE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id13_post)
# summary(fit_aci_id13_post)
# #### plot
# plot(fit_aci_id13_post)
# #### add to dataframe
# aci_data_id13_post_data <- cbind(aci_data_id13_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id13_post[,30]),
#                                   mean(aci_data_id13_post[,118]),
#                                   fit_aci_id13_post[[2]][1,1],
#                                   fit_aci_id13_post[[2]][1,2],
#                                   fit_aci_id13_post[[2]][2,1],
#                                   fit_aci_id13_post[[2]][2,2],
#                                   fit_aci_id13_post[[2]][3,1],
#                                   fit_aci_id13_post[[2]][3,2],
#                                   fit_aci_id13_post[[2]][4,1],
#                                   fit_aci_id13_post[[2]][4,2],
#                                   fit_aci_id13_post$RMSE,
#                                   fit_aci_id13_post$Ci_transition,
#                                   fit_aci_id13_post$citransition,
#                                   fit_aci_id13_post$Km,
#                                   fit_aci_id13_post$GammaStar,
#                                   fit_aci_id13_post$fitmethod,
#                                   fit_aci_id13_post$Tcorrect,
#                                   fit_aci_id13_post$fitTPU)
# colnames(aci_data_id13_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id13_post_data)

##############################################################################
##############################################################################
###########id131
##############################################################################
##############################################################################
### plant id131 pre_heatwave
aci_data_id131_pre = subset(aci_data, id == ids[18] & meas.type == 'pre_heatwave')
aci_data_id131_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id131_pre)
#### fit aci curve
fit_aci_id131_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = TRUE,
                           fitmethod = 'bilinear',
                           data = aci_data_id131_pre)
summary(fit_aci_id131_pre)
#### plot
plot(fit_aci_id131_pre)
#### add to dataframe
aci_data_id131_pre_data <- cbind(aci_data_id131_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id131_pre[,30]),
                                 mean(aci_data_id131_pre[,118]),
                                 fit_aci_id131_pre[[2]][1,1],
                                 fit_aci_id131_pre[[2]][1,2],
                                 fit_aci_id131_pre[[2]][2,1],
                                 fit_aci_id131_pre[[2]][2,2],
                                 fit_aci_id131_pre[[2]][3,1],
                                 fit_aci_id131_pre[[2]][3,2],
                                 fit_aci_id131_pre[[2]][4,1],
                                 fit_aci_id131_pre[[2]][4,2],
                                 fit_aci_id131_pre$RMSE,
                                 fit_aci_id131_pre$Ci_transition,
                                 fit_aci_id131_pre$citransition,
                                 fit_aci_id131_pre$Km,
                                 fit_aci_id131_pre$GammaStar,
                                 fit_aci_id131_pre$fitmethod,
                                 fit_aci_id131_pre$Tcorrect,
                                 fit_aci_id131_pre$fitTPU)
colnames(aci_data_id131_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id131_pre_data)

### plant id131 post_heatwave
aci_data_id131_post = subset(aci_data, id == ids[18] & meas.type == 'post_heatwave')
aci_data_id131_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id131_post)
#### fit aci curve
fit_aci_id131_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = TRUE,
                            fitmethod = 'bilinear',
                            data = aci_data_id131_post)
summary(fit_aci_id131_post)
#### plot
plot(fit_aci_id131_post)
#### add to dataframe
aci_data_id131_post_data <- cbind(aci_data_id131_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id131_post[,30]),
                                  mean(aci_data_id131_post[,118]),
                                  fit_aci_id131_post[[2]][1,1],
                                  fit_aci_id131_post[[2]][1,2],
                                  fit_aci_id131_post[[2]][2,1],
                                  fit_aci_id131_post[[2]][2,2],
                                  fit_aci_id131_post[[2]][3,1],
                                  fit_aci_id131_post[[2]][3,2],
                                  fit_aci_id131_post[[2]][4,1],
                                  fit_aci_id131_post[[2]][4,2],
                                  fit_aci_id131_post$RMSE,
                                  fit_aci_id131_post$Ci_transition,
                                  fit_aci_id131_post$citransition,
                                  fit_aci_id131_post$Km,
                                  fit_aci_id131_post$GammaStar,
                                  fit_aci_id131_post$fitmethod,
                                  fit_aci_id131_post$Tcorrect,
                                  fit_aci_id131_post$fitTPU)
colnames(aci_data_id131_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id131_post_data)

##############################################################################
##############################################################################
###########id133
##############################################################################
##############################################################################
### plant id133 pre_heatwave
aci_data_id133_pre = subset(aci_data, id == ids[19] & meas.type == 'pre_heatwave')
aci_data_id133_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id133_pre)
#### fit aci curve
fit_aci_id133_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = TRUE,
                           fitmethod = 'bilinear',
                           data = aci_data_id133_pre)
summary(fit_aci_id133_pre)
#### plot
plot(fit_aci_id133_pre)
#### add to dataframe
aci_data_id133_pre_data <- cbind(aci_data_id133_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id133_pre[,30]),
                                 mean(aci_data_id133_pre[,118]),
                                 fit_aci_id133_pre[[2]][1,1],
                                 fit_aci_id133_pre[[2]][1,2],
                                 fit_aci_id133_pre[[2]][2,1],
                                 fit_aci_id133_pre[[2]][2,2],
                                 fit_aci_id133_pre[[2]][3,1],
                                 fit_aci_id133_pre[[2]][3,2],
                                 fit_aci_id133_pre[[2]][4,1],
                                 fit_aci_id133_pre[[2]][4,2],
                                 fit_aci_id133_pre$RMSE,
                                 fit_aci_id133_pre$Ci_transition,
                                 fit_aci_id133_pre$citransition,
                                 fit_aci_id133_pre$Km,
                                 fit_aci_id133_pre$GammaStar,
                                 fit_aci_id133_pre$fitmethod,
                                 fit_aci_id133_pre$Tcorrect,
                                 fit_aci_id133_pre$fitTPU)
colnames(aci_data_id133_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id133_pre_data)

### plant id133 post_heatwave
aci_data_id133_post = subset(aci_data, id == ids[19] & meas.type == 'post_heatwave')
aci_data_id133_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id133_post)
#### fit aci curve
fit_aci_id133_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = TRUE,
                            fitmethod = 'bilinear',
                            data = aci_data_id133_post)
summary(fit_aci_id133_post)
#### plot
plot(fit_aci_id133_post)
#### add to dataframe
aci_data_id133_post_data <- cbind(aci_data_id133_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id133_post[,30]),
                                  mean(aci_data_id133_post[,118]),
                                  fit_aci_id133_post[[2]][1,1],
                                  fit_aci_id133_post[[2]][1,2],
                                  fit_aci_id133_post[[2]][2,1],
                                  fit_aci_id133_post[[2]][2,2],
                                  fit_aci_id133_post[[2]][3,1],
                                  fit_aci_id133_post[[2]][3,2],
                                  fit_aci_id133_post[[2]][4,1],
                                  fit_aci_id133_post[[2]][4,2],
                                  fit_aci_id133_post$RMSE,
                                  fit_aci_id133_post$Ci_transition,
                                  fit_aci_id133_post$citransition,
                                  fit_aci_id133_post$Km,
                                  fit_aci_id133_post$GammaStar,
                                  fit_aci_id133_post$fitmethod,
                                  fit_aci_id133_post$Tcorrect,
                                  fit_aci_id133_post$fitTPU)
colnames(aci_data_id133_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id133_post_data)

##############################################################################
##############################################################################
###########id136
##############################################################################
##############################################################################
### plant id136 pre_heatwave
# aci_data_id136_pre = subset(aci_data, id == ids[20] & meas.type == 'pre_heatwave')
# aci_data_id136_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id136_pre)
# #### fit aci curve
# fit_aci_id136_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = TRUE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id136_pre)
# summary(fit_aci_id136_pre)
# #### plot
# plot(fit_aci_id136_pre)
# #### add to dataframe
# aci_data_id136_pre_data <- cbind(aci_data_id136_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id136_pre[,30]),
#                                  mean(aci_data_id136_pre[,118]),
#                                  fit_aci_id136_pre[[2]][1,1],
#                                  fit_aci_id136_pre[[2]][1,2],
#                                  fit_aci_id136_pre[[2]][2,1],
#                                  fit_aci_id136_pre[[2]][2,2],
#                                  fit_aci_id136_pre[[2]][3,1],
#                                  fit_aci_id136_pre[[2]][3,2],
#                                  fit_aci_id136_pre[[2]][4,1],
#                                  fit_aci_id136_pre[[2]][4,2],
#                                  fit_aci_id136_pre$RMSE,
#                                  fit_aci_id136_pre$Ci_transition,
#                                  fit_aci_id136_pre$citransition,
#                                  fit_aci_id136_pre$Km,
#                                  fit_aci_id136_pre$GammaStar,
#                                  fit_aci_id136_pre$fitmethod,
#                                  fit_aci_id136_pre$Tcorrect,
#                                  fit_aci_id136_pre$fitTPU)
# colnames(aci_data_id136_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id136_pre_data)
# 
# ### plant id136 post_heatwave
# aci_data_id136_post = subset(aci_data, id == ids[20] & meas.type == 'post_heatwave')
# aci_data_id136_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id136_post)
# #### fit aci curve
# fit_aci_id136_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = TRUE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id136_post)
# summary(fit_aci_id136_post)
# #### plot
# plot(fit_aci_id136_post)
# #### add to dataframe
# aci_data_id136_post_data <- cbind(aci_data_id136_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id136_post[,30]),
#                                   mean(aci_data_id136_post[,118]),
#                                   fit_aci_id136_post[[2]][1,1],
#                                   fit_aci_id136_post[[2]][1,2],
#                                   fit_aci_id136_post[[2]][2,1],
#                                   fit_aci_id136_post[[2]][2,2],
#                                   fit_aci_id136_post[[2]][3,1],
#                                   fit_aci_id136_post[[2]][3,2],
#                                   fit_aci_id136_post[[2]][4,1],
#                                   fit_aci_id136_post[[2]][4,2],
#                                   fit_aci_id136_post$RMSE,
#                                   fit_aci_id136_post$Ci_transition,
#                                   fit_aci_id136_post$citransition,
#                                   fit_aci_id136_post$Km,
#                                   fit_aci_id136_post$GammaStar,
#                                   fit_aci_id136_post$fitmethod,
#                                   fit_aci_id136_post$Tcorrect,
#                                   fit_aci_id136_post$fitTPU)
# colnames(aci_data_id136_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id136_post_data)

##############################################################################
##############################################################################
###########id14
##############################################################################
##############################################################################
### plant id14 pre_heatwave
aci_data_id14_pre = subset(aci_data, id == ids[21] & meas.type == 'pre_heatwave')
aci_data_id14_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id14_pre)
#### fit aci curve
fit_aci_id14_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = TRUE,
                           fitmethod = 'bilinear',
                           data = aci_data_id14_pre)
summary(fit_aci_id14_pre)
#### plot
plot(fit_aci_id14_pre)
#### add to dataframe
aci_data_id14_pre_data <- cbind(aci_data_id14_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id14_pre[,30]),
                                 mean(aci_data_id14_pre[,118]),
                                 fit_aci_id14_pre[[2]][1,1],
                                 fit_aci_id14_pre[[2]][1,2],
                                 fit_aci_id14_pre[[2]][2,1],
                                 fit_aci_id14_pre[[2]][2,2],
                                 fit_aci_id14_pre[[2]][3,1],
                                 fit_aci_id14_pre[[2]][3,2],
                                 fit_aci_id14_pre[[2]][4,1],
                                 fit_aci_id14_pre[[2]][4,2],
                                 fit_aci_id14_pre$RMSE,
                                 fit_aci_id14_pre$Ci_transition,
                                 fit_aci_id14_pre$citransition,
                                 fit_aci_id14_pre$Km,
                                 fit_aci_id14_pre$GammaStar,
                                 fit_aci_id14_pre$fitmethod,
                                 fit_aci_id14_pre$Tcorrect,
                                 fit_aci_id14_pre$fitTPU)
colnames(aci_data_id14_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id14_pre_data)

### plant id14 post_heatwave
aci_data_id14_post = subset(aci_data, id == ids[21] & meas.type == 'post_heatwave')
aci_data_id14_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id14_post)
#### fit aci curve
fit_aci_id14_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = TRUE,
                            fitmethod = 'bilinear',
                            data = aci_data_id14_post)
summary(fit_aci_id14_post)
#### plot
plot(fit_aci_id14_post)
#### add to dataframe
aci_data_id14_post_data <- cbind(aci_data_id14_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id14_post[,30]),
                                  mean(aci_data_id14_post[,118]),
                                  fit_aci_id14_post[[2]][1,1],
                                  fit_aci_id14_post[[2]][1,2],
                                  fit_aci_id14_post[[2]][2,1],
                                  fit_aci_id14_post[[2]][2,2],
                                  fit_aci_id14_post[[2]][3,1],
                                  fit_aci_id14_post[[2]][3,2],
                                  fit_aci_id14_post[[2]][4,1],
                                  fit_aci_id14_post[[2]][4,2],
                                  fit_aci_id14_post$RMSE,
                                  fit_aci_id14_post$Ci_transition,
                                  fit_aci_id14_post$citransition,
                                  fit_aci_id14_post$Km,
                                  fit_aci_id14_post$GammaStar,
                                  fit_aci_id14_post$fitmethod,
                                  fit_aci_id14_post$Tcorrect,
                                  fit_aci_id14_post$fitTPU)
colnames(aci_data_id14_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id14_post_data)























