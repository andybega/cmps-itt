
library("tidyverse")
library("states")
library("hrbrthemes")

cy <- readRDS("data/cy.rds")

#' Tidy up a correlation matrix
#' 
#' X is a matrix
tidy_cormat <- function(X) {
  cor(X) %>%
    as_tibble() %>% 
    mutate(var1 = names(.)) %>% 
    gather(var2, cor, -var1) 
}

cors_by_country <- cy %>%
  select(gwcode, itt_alleg_vtcriminal:itt_alleg_vtunst) %>%
  group_by(gwcode) %>%
  nest() %>%
  mutate(cormat = map(data, tidy_cormat)) %>%
  mutate(data = NULL) %>%
  unnest()

keep <- c("itt_alleg_vtcriminal", "itt_alleg_vtdissident", "itt_alleg_vtmarginalized", "itt_alleg_vtunst")

pair_cor <- cors_by_country %>% 
  group_by(var1, var2) %>%
  summarize(cor = mean(cor, na.rm = T)) %>%
  ungroup() %>%
  filter(var1 %in% keep & var2 %in% keep) %>%
  mutate(var1 = str_replace(var1, "itt_alleg_vt", "") %>% str_to_title() %>% dplyr::recode("Unst" = "Unknown"),
         var2 = str_replace(var2, "itt_alleg_vt", "") %>% str_to_title() %>% dplyr::recode("Unst" = "Unknown")) 

p <- cors_by_country %>%
  filter(var1!=var2) %>%
  filter(var1 %in% keep & var2 %in% keep) %>%
  mutate(var1 = str_replace(var1, "itt_alleg_vt", "") %>% str_to_title() %>% dplyr::recode("Unst" = "Unknown"),
         var2 = str_replace(var2, "itt_alleg_vt", "") %>% str_to_title() %>% dplyr::recode("Unst" = "Unknown")) %>%
  ggplot(., aes(x = cor)) +
  facet_grid(var1 ~ var2) +
  geom_histogram(binwidth = .05, alpha = .5) +
  scale_x_continuous(limits = c(-1, 1)) +
  theme_ipsum() +
  geom_vline(xintercept = 0, linetype = 3) +
  geom_vline(data = pair_cor, aes(xintercept = cor)) +
  geom_text(data = pair_cor, aes(x = cor - .1, y = 9, label = paste0("bar(r) == ", round(cor, 2))), 
            parse = TRUE, hjust = 1) +
  #ggtitle("Allegations of torture against different types of victims are only loosely correlated within countries",
  #        sub = "Each plot is a histogram of the pairwise correlations within each country for # of allegations of torture of x and y victim types") +
  labs(x = "Correlation between # of allegations of torture in each country for victim types x and y", y = "Count")
p 
ggsave(p, file = "output/figures/allegations-by-victim-pairwise-correlations.png", height = 6, width = 8)
