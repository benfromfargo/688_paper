library(tidyverse)

BeRn::set_my_theme()

# Read in main data from 1980 to 2011 ####
raw_data <- readxl::read_xlsx("correlatesofstatepolicyprojectv2_1.xlsx",
                             sheet = "Government_edit")

# data only goes through 2011 - note dropping DC and NE
final_data <- raw_data %>% 
  filter(year >= 1980 & year < 2012) %>% 
  filter(st != "DC" & st != "NE") 

# national counts of rep and dem control
final_data <- final_data %>% 
  group_by(year) %>% 
  mutate(nat_dem_unified = sum(dem_unified)) %>% 
  mutate(nat_rep_unified = sum(rep_unified)) %>% 
  ungroup()

final_data_long <- final_data %>% 
  select(year, state, nat_dem_unified, nat_rep_unified) %>% 
  pivot_longer(cols = nat_dem_unified:nat_rep_unified,
               names_to = "party_control",
               values_to = "number_govs") %>% 
  mutate(party_control = ifelse(party_control == "nat_dem_unified", "Democrat",
                                ifelse(party_control == "nat_rep_unified", "Republican",
                                       "ERROR")))

# just keep national level data
final_data_long <- final_data_long %>% 
  select(year, party_control, number_govs) %>% 
  distinct()

# Read in NCSL data from 2012 to 2017 ####
sheets <- as.character(2012:2017)
ncsl_data <- map(sheets, ~ readxl::read_xlsx("ncsl/control_2012_2017.xlsx",
                                             sheet = .,
                                             skip = 1))

ncsl_data_df <- reduce(ncsl_data, rbind)

ncsl_data_df <- ncsl_data_df %>% 
  filter(STATE != "Nebraska") %>% 
  rename(year = Year, state_control = `State Control`) %>% 
  mutate(rep_unified = ifelse(state_control == "Rep", 1, 0)) %>% 
  mutate(dem_unified = ifelse(state_control == "Dem", 1, 0))

# create annual counts
ncsl_data_df <- ncsl_data_df %>% 
  group_by(year) %>% 
  mutate(nat_dem_unified = sum(dem_unified)) %>% 
  mutate(nat_rep_unified = sum(rep_unified)) %>% 
  ungroup() %>% 
  select(year, nat_dem_unified, nat_rep_unified) %>% 
  distinct()

# Pivot to long 
ncsl_long <- ncsl_data_df %>% 
  pivot_longer(nat_dem_unified:nat_rep_unified,
               names_to = "party_control",
               values_to = "number_govs") %>% 
  mutate(party_control = ifelse(party_control == "nat_rep_unified", "Republican",
                                ifelse(party_control == "nat_dem_unified", "Democrat",
                                       "ERROR")))


# Join ncsl to original data from 1980 to 2011
final_data_long <- rbind(final_data_long, ncsl_long)

# plot
final_data_long %>% 
  ggplot() +
  geom_line(aes(year, number_govs, color = party_control)) +
  scale_color_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
  scale_x_continuous(breaks = seq(1980, 2016, 4),
                   labels = as.character(seq(1980, 2016, 4))) +
  theme(legend.title = element_blank(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  guides(color = guide_legend(ncol = 2, byrow=TRUE)) +
  xlab(NULL) +
  ylab("State governments under unified control") +
  labs(caption = "Note: Excludes Nebraska. Sources: Correlates of State Policy Project and National\nConference of State Legislatures")
ggsave("../paper/plots/party_control.pdf", device = cairo_pdf)



    