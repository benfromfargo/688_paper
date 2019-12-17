library(tidyverse)
library(ggwaffle)
library(waffle)
BeRn::set_my_theme()

econ_data <- readxl::read_xlsx('econ_premption.xlsx')

all_years <- tibble(year = 1984:2019)

econ_data <- left_join(all_years, econ_data, by = "year")

econ_data <- econ_data %>%
  group_by(year) %>% 
  arrange(year, policy) %>% 
  mutate(rank = ifelse(is.na(state), 0, order(policy))) %>% 
  ungroup()

colors <- c("PW" = "#228833", "NA" = "#FFFFFF", "PLA" = "#CCBB44", "MW" = "#EE6677", "PL" = "#AA3377", "GE" = "#4477AA", "FS" = "#66CCEE")

econ_data %>% 
  ggplot(aes(year, rank, fill = policy)) + 
  geom_waffle(size = .75) +
  #geom_text(aes(label = state)) +
  scale_fill_manual(breaks = c("PW", "PLA", "MW", "PL", "GE", "FS"),
                    labels = c("PW" = "Prevailing wage",
                                "PLA" = "Project labor agreements", 
                                "MW" = "Minimum wage", 
                                "PL" = "Paid leave", 
                                "GE" = "Gig economy",
                                "FS" = "Fair scheduling"),
                      values = colors,
                    name = NULL) +
  scale_x_continuous(limits = c(1983,2020),
                  breaks = seq(1984, 2019, 5),
                   labels = seq(1984, 2019, 5)) +
  xlab(NULL) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.002)),
                     limits = c(0,30),
                     breaks = seq(0,30,5),
                     labels = seq(0,30,5)) +
  ylab("Number of policies preempted") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow=TRUE))

ggsave("../paper/plots/preemption_plot.pdf", device = cairo_pdf)  
  

