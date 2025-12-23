library(randomForestSRC)
library(caret)
library(tidyverse)
library(beepr); options(error = function() {beep(7)})
library(geobr)
# library(gridExtra)
# library(scico)
library(tornado) #importance plot
library(broom) # for tidy()
library(knitr) # for kable()

setwd("Z:/laura/naturehealth")

analysis_dir = "data/analysis_dfs"
date_normed = "2025-09-26"
remove_stuff = FALSE

muni_info_normed = read.csv("data/analysis_dfs/normed_municipal_info_compiled.csv")
weather_fit_normed <- read.csv( "data/analysis_dfs/outbreaks_through_2025-07-31_with_weather.csv")
weather_fit_normed$st_date <- as.Date(weather_fit_normed$st_date)
weather_fit_normed <- weather_fit_normed |> subset(st_date >= as.Date("2021-07-01")
                                                   & st_date < as.Date("2025-07-31")
                                                   )

weather_fit_normed$case_outcome  <- factor(weather_fit_normed$case_outcome , levels = c("OUTBREAK","ISOLATED"))
crop_info <- read.csv("data/analysis_dfs/cropland_normalized_2025-09-29.csv")
crop_info <- crop_info |> select(code_muni:name_region, all_of(contains(c("banana",                                                                "cassava","cocoa","coffee","rubber","soybean","sugarcane")
))) 

weather_fit_normed <- left_join(weather_fit_normed, muni_info_normed)
weather_fit_normed <- left_join(weather_fit_normed, crop_info)

weather_fit_normed$outbreak_tf <- weather_fit_normed$case_outcome == "OUTBREAK"

# column types -------
admin_info <- c("abbrev_state","name_region",
                "type","urban_type2010")

annual_climate_norms <- names(weather_fit_normed %>% select(contains("annual_")))
landcover_cols <-   names(weather_fit_normed %>% select(contains("lc_")))
soil_cols <-   names(weather_fit_normed %>% select(contains("soil_")))
crop_cols <-   names(weather_fit_normed %>% select(contains("pct_crop_")))
footprint_cols <- names(weather_fit_normed %>% select(contains("foot")))

weather_prior <- names(weather_fit_normed %>% select(contains("mean_prior_30")))
weather_subsequent <- names(weather_fit_normed %>% select(contains("mean_subsequent_30")))

# Variable Selection for Weather ----
weath.pred <- weather_fit_normed %>% select(
  outbreak_tf ,
  all_of(weather_prior),
  all_of(weather_subsequent),
  all_of(annual_climate_norms)
) |> as.data.frame() |> na.omit()

set.seed(2*as.numeric(as.Date("2025-09-30"))) 
ind <- sample(2, nrow(weath.pred), replace = TRUE, 
              prob = c(0.8, 0.2)
)

train_weath <- weath.pred[ind==1,]
test_weath <- weath.pred[ind==2,]

vars_weathonly <- var.select(outbreak_tf ~., data=train_weath,refit=TRUE, ntree = 1000)

weath.pred_reduced1 <- weather_fit_normed %>% select(
  outbreak_tf,
  all_of(vars_weathonly$topvars)
) |> as.data.frame() |> na.omit()

set.seed(2*as.numeric(as.Date("2025-09-30"))) 
ind <- sample(2, nrow(weath.pred_reduced1), replace = TRUE, 
              prob = c(0.8, 0.2)
)

train_weath2 <- weath.pred_reduced1[ind==1,];test_weath2 <- weath.pred_reduced1[ind==2,]

fit1 <- glm(outbreak_tf  ~ .,
            data = train_weath2,family = "binomial"(link='logit'))

summary(fit1)

fit1reduced <- step(fit1, direction = "both")
summary(fit1reduced)

# Variable Selection for Invariant -----
weath.pred_addinvar <- weather_fit_normed %>% select(
  all_of(names(fit1reduced$model)),
  log10_pop_normalized, log10_dwmd_normalized,
  all_of(landcover_cols),
  all_of(footprint_cols),
  all_of(rev(soil_cols)),
  all_of(crop_cols)
) |> as.data.frame() |> na.omit()

set.seed(3*as.numeric(as.Date("2025-09-30"))) 
ind <- sample(2, nrow(weath.pred_addinvar), replace = TRUE, 
              prob = c(0.8, 0.2)
)

train_weath3 <- weath.pred_addinvar[ind==1,]
test_weath3 <- weath.pred_addinvar[ind==2,]

fit3 <- glm(outbreak_tf  ~ .,
            data = train_weath3,family = "binomial"(link='logit'))
fit3
summary(fit3)

fit3reduced <- step(fit3, direction = "both")
summary(fit3reduced)
fit3reduced$aic

fit3reduced$aic - fit3$aic

### further reduction to minimum model -----
vars_removed <- "none"
fit_aics <- fit3reduced$aic

to_reduce <- fit3reduced
delt_aic = 0

aic_thresh = 3
while(delt_aic < aic_thresh){
  temp_sum <- as.data.frame(summary(to_reduce)$coefficients)
  
  to_remove <- row.names(temp_sum)[which.max(temp_sum$`Pr(>|z|)`)]
  if (to_remove == "(Intercept)" ) delt_aic = 1000
  check <- stats::update(to_reduce,formula = paste(". ~ . -", to_remove))
  
  delt_aic = check$aic - to_reduce$aic
  print(paste("Removed", to_remove,"delta AIC =", delt_aic))
  
  if (delt_aic < aic_thresh){
    vars_removed = c(vars_removed, to_remove)
    fit_aics = c(fit_aics, check$aic)
    to_reduce <- check
  }
  
  
}

gtestreduced <- glm(outbreak_tf ~ 1, data=train_weath3, family=binomial)
imp <- importance(to_reduce, gtestreduced)
plot(imp)

summary(to_reduce)

# write out model, produce tables -----
mappingfit <- to_reduce
save(mappingfit, file = "results/mappingfit_20251001.rda")

model_refinement <- data.frame(vars_removed=vars_removed, fit_aics=fit_aics)
model_refinement$delta_AIC <- model_refinement$fit_aics - c(NA, head(fit_aics, -1))

name_conversion <- read.csv("data/additional_data_files/all_vars_to_pretty.csv") 

model_refinement <- model_refinement |> mutate(pretty_name = name_conversion$pretty_name_normalized[match(vars_removed , name_conversion$variable_name_normalized)], .before=vars_removed)
model_refinement$pretty_name[1] <- "none"
kable(model_refinement)

write.csv(model_refinement, 
          "results/model_reduction_steps.csv", row.names = FALSE)


out <- tidy(mappingfit) |> mutate(Description = name_conversion$pretty_name[match(term, name_conversion$variable_name_normalized)], .before="term")

kable(out)


sum <- summary(mappingfit)
to_export <- as.data.frame(sum$coefficients)

to_export <- to_export %>%
  mutate(Coefficient = rownames(to_export),
         .before="Estimate") |> 
  mutate(pretty_name = name_conversion$pretty_name_normalized[match(Coefficient, name_conversion$variable_name_normalized)], .before="Coefficient")
to_export$upperCI <- to_export$Estimate + 1.96*to_export$`Std. Error`
to_export$lowerCI <- to_export$Estimate - 1.96*to_export$`Std. Error`

to_export$estimate_CI <- paste0(round(to_export$Estimate, 2), " (", round(to_export$lowerCI,2), ",", round(to_export$upperCI,2), ")")


write.csv(to_export, 
          "results/final_model_coefficients.csv", row.names = FALSE)
kable(to_export)


####Save Variable Importance Plot-----
save.width = 6; save.height = save.width

gtestreduced <- glm(outbreak_tf ~ 1, data=train_weath3, family=binomial)
imp <- importance(mappingfit, gtestreduced)
plot(imp)

correct_levels <- name_conversion$pretty_name[match(levels(imp$data$variable), name_conversion$variable_name_normalized)]

locs <- match(imp$data$variable, name_conversion$variable_name_normalized)
imp$data$variable <- name_conversion$pretty_name[locs]

imp$data$variable <- factor(imp$data$variable, levels = correct_levels)

pdf("results/figs/SuppFig_VariableImportance.pdf",
    width = save.width*2, height = save.height)
plot(imp)
dev.off()

png("for_resubmission/results/SuppFig_VariableImportance.png",
    width = save.width*2, height = save.height, units = "in", res = 720)
plot(imp)
dev.off()

