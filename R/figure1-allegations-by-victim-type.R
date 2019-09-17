
library("tidyverse")
library("states")
library("hrbrthemes")

cy <- readRDS("data/cy.rds")

# Allegations by victim type
p <- cy %>%
  select(gwcode, year, starts_with("itt_alleg"), -itt_alleg_vtall) %>%
  gather(Victim, Allegations, starts_with("itt_alleg")) %>%
  # Clean up victim factor labels
  mutate(Victim = str_replace(Victim, "itt_alleg_vt", "") %>% str_to_title(),
         Victim = factor(Victim) %>% fct_recode(Unknown = "Unst", POW = "Pow")) %>%
  group_by(Victim) %>%
  summarize(Allegations = sum(Allegations)) %>%
  # Sort by # allegations so we can re-order factor lables, so the plot is high to low
  arrange(Allegations) %>%
  mutate(Victim = fct_inorder(Victim)) %>%
  ungroup() %>%
  ggplot(.) +
  ggstance::geom_barh(aes(y = Victim, x = Allegations), stat = "identity",
                      width = .8) +
  geom_text(aes(y = Victim, x = Allegations, 
                label = formatC(Allegations, format="d", big.mark=",")),
            nudge_x = c(2*130, 3*130, rep(-100, 4)), hjust = 1, 
            colour = c(rep("gray10", 2), rep("gray95", 4))) +
  theme_ipsum() +
  labs(y = "", x = "# Allegations") +
  theme(axis.text=element_text(size=11, colour = "gray10", family = "Arial"))
p
ggsave(p, file = "output/figures/allegations-by-victim.png", height = 5, width = 8)
