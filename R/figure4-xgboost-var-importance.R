
library("xgboost")
library("ggplot2")
library("caret")

source("R/functions.R")

# change this to mdl-xgboost.rds if you re-ran 2-xgboost.R to re-train a 
# XGBoost model
mdlX <- readRDS("output/mdl-xgboost-orig.rds")

predictor_importance <- mdlX %>%
  map_dfr(., .id = "outcome", function(mm) {
    imp <- varImp(mm)$importance
    imp <- tibble(predictor = rownames(imp), importance = imp[, 1])
    imp
  })

p <- predictor_importance %>%
  dplyr::filter(!str_detect(predictor, "gwcode_fct")) %>%
  mutate(predictor = rename_terms(predictor)) %>%
  group_by(predictor) %>%
  mutate(avg_imp = mean(importance)) %>%
  arrange(avg_imp) %>%
  ungroup() %>%
  mutate(predictor = factor(predictor) %>% fct_inorder()) %>%
  ggplot(., aes(x = importance, y = predictor)) +
  geom_linerangeh(aes(xmin = 0, xmax = importance, y = predictor, group = outcome), 
                  linetype = 1, color = "gray90", position = position_dodgev(height = .5)) +
  geom_point(aes(colour = outcome), position = position_dodgev(height = .5)) + 
  theme_ipsum() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = c(.8, .2)) +
  labs(x = "XGBoost variable importance, 0-100", y = "") +
  scale_colour_discrete("Outcome")
p
ggsave(p, file = "output/figures/xgboost-variable-importance-v1.png", height = 6, width = 10)
