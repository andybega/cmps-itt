
# Rscript code/xgboost.R

suppressMessages({
  library("caret")
  library("tidyverse")
  library("states")
  library("broom")
  library("xgboost")
  library("recipes")
  library("lme4")
  library("doMC") 
  library("scoringRules")
  library("futile.logger")
})

flog.info("xgboost.R script start")

set.seed(1234)

registerDoMC(cores = 4) 

source("R/functions.R")

cy <- readRDS("data/cy.rds")

# Xgboost general count model ---------------------------------------------


num_data <- cy %>%
  select(gwcode, year, one_of(voi), starts_with("norm_"), starts_with("itt_alleg"), 
         "itt_RstrctAccess", "internal_confl") %>%
  mutate(gwcode_fct = factor(gwcode))
# Don't want to center/scale binary/quasi-categorical vars, so ID those
binary_vars <- num_data %>%
  summarize_all(~ mean(. %in% c(0, 1, 100)) > .9) %>%
  tidyr::gather(var, quasibinary) %>%
  filter(quasibinary) %>%
  pull(var)
num_data <- recipe(num_data) %>%
  step_dummy(gwcode_fct) %>%
  update_role(everything(), new_role = "predictor") %>%
  update_role(., starts_with("itt_alleg"), new_role = "outcome") %>%
  update_role(., gwcode, year, new_role = "ID") %>%
  step_zv(all_predictors()) %>%
  prep(retain = TRUE)
train_id_vars <- num_data %>%
  juice(has_role("ID"))
train_y <- num_data %>%
  juice(has_role("outcome"))
train_x <- num_data %>%
  juice(has_role("predictor")) %>%
  as.matrix()

trControl <- caret::trainControl(method = "cv", number = 11, 
                                 verboseIter = FALSE, savePredictions = TRUE)

# http://xgboost.readthedocs.io/en/latest/how_to/param_tuning.html
# http://xgboost.readthedocs.io/en/latest/parameter.html
generate_random_grid <- function(n) {
  data.frame(nrounds = sample(c(100, 150, 200, 500, 1000), n, replace = TRUE),
             eta = round(runif(n, 0, .5), 2),
             # tree complexity
             max_depth = sample(2:10, n, replace = TRUE),
             min_child_weight = rpois(n, 1),
             gamma = round(rexp(n, rate = 5), 1),
             # randomization
             colsample_bytree = round(rbeta(n, 3, 1), 2),
             subsample = round(rbeta(n, 5, 2), 2)
             )
}
hp_grid <- generate_random_grid(200)

mdlX <- list(
  criminal = caret::train(x = train_x, y = train_y[["itt_alleg_vtcriminal"]],
                   method = "xgbTree", objective = "count:poisson", 
                   eval_metric = "poisson-nloglik", trControl = trControl,
                   tuneGrid = hp_grid, metric = "MAE"),
  dissident = caret::train(x = train_x, y = train_y[["itt_alleg_vtdissident"]],
                    method = "xgbTree", objective = "count:poisson", 
                    eval_metric = "poisson-nloglik", trControl = trControl,
                    tuneGrid = hp_grid, metric = "MAE"),
  marginalized = caret::train(x = train_x, y = train_y[["itt_alleg_vtmarginalized"]],
                       method = "xgbTree", objective = "count:poisson", 
                       eval_metric = "poisson-nloglik", trControl = trControl,
                       tuneGrid = hp_grid, metric = "MAE")
)

write_rds(mdlX, path = "output/mdl-xgboost.rds")


oos_preds <- mdlX %>%
  map_dfr(., .id = "yname", function(mm) {
    out <- mm$pred %>%
      filter(nrounds   == mm$bestTune$nrounds,
             max_depth == mm$bestTune$max_depth,
             eta       == mm$bestTune$eta,
             gamma     == mm$bestTune$gamma,
             colsample_bytree == mm$bestTune$colsample_bytree,
             min_child_weight == mm$bestTune$min_child_weight,
             subsample == mm$bestTune$subsample)
    out <- out %>%
      rename(y = obs, yhat = pred, row_index = rowIndex) %>%
      select(y, yhat, row_index) %>%
      mutate(yhat = as.numeric(yhat)) %>%
      as_tibble()
    out <- out %>% arrange(row_index) %>% select(-row_index)
    out
  })

fit_xgboost <- oos_preds %>%
  rename(outcome = yname) %>%
  group_by(outcome) %>%
  summarize(MAE = mae(y, yhat),
            RMSE = rmse(y, yhat),
            CRPS = mean(crps_pois(y, yhat)) ) %>%
  mutate(model_name = "XGBoost") %>%
  select(outcome, model_name, MAE, RMSE, CRPS)
write_csv(fit_xgboost, "output/xgboost-fit.csv")

flog.info("xgboost.R script end")

