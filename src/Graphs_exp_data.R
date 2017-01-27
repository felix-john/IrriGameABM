### Producing graphs from irrigation game experiment 

library(ggplot2)

# Extraction
exp.df %>%
  # filter(LimComm == 0) %>%
  ggplot() +
  geom_line(aes(x = Round, y = Extraction, colour = factor(LimComm), group = c(Group))) +
  facet_grid(Position ~ treatment, labeller = "label_both") +
  labs(y = "Extraction [tokens]", colour = "Limited \nCommunication", title = "Individual water collection for different treatments")

filename <- "Extraction_ind_by_Pos&Treatment.png"
ggsave(filename, path = "./Graphics/", dpi = 300, device = "png", width = 11.69, height = 8.27, units = "in") #A4

# Individual investment
exp.df %>%
  # filter(LimComm == 0) %>%
  ggplot() +
  geom_line(aes(x = Round, y = Investment, colour = factor(LimComm), group = c(Group))) +
  facet_grid(Position ~ treatment, labeller = "label_both") +
  labs(y = "Investment [tokens]", colour = "Limited \nCommunication", title = "Individual investment for different treatments")

filename <- "Investment_ind_by_Pos&Treatment.png"
ggsave(filename, path = "./Graphics", dpi = 300, device = "png", width = 11.69, height = 8.27, units = "in") #A4

# Group investment
group_summary %>%
  ggplot() +
  geom_line(aes(x = Round, y = Investment_group_mean)) +
  geom_line(aes(x = Round, y = Investment_group_mean + Investment_group_SD), linetype = "dashed") +
  geom_line(aes(x = Round, y = Investment_group_mean - Investment_group_SD), linetype = "dashed") +
  facet_grid(LimComm ~ treatment, labeller = "label_both") +
  labs(y = "Total investment [tokens]", colour = "Limited \nCommunication", title = "Group investment for different treatments")

filename <- "Investment_group_by_Treatment.png"
ggsave(filename, path = "./Graphics", dpi = 300, device = "png", width = 11.69, height = 8.27, units = "in") #A4

# Infrastructure
group_summary %>%
  # filter(LimComm == 0) %>%
  ggplot() +
  geom_line(aes(x = Round, y = Infra_mean)) +
  geom_line(aes(x = Round, y = Infra_mean + Infra_SD), linetype = "dashed") +
  geom_line(aes(x = Round, y = Infra_mean - Infra_SD), linetype = "dashed") +
  # geom_line(aes(x = Round, y = Infra_decline), colour = "grey") +
  facet_grid(LimComm ~ treatment, labeller = "label_both") +
  labs(y = "Infrastructure", title = "Infrastructure level at the end of each round")

filename <- "Infrastructure_by_Treatment.png"
ggsave(filename, path = "./Graphics", dpi = 300, device = "png", width = 11.69, height = 8.27, units = "in") #A4

# Waterflux
exp.df %>%
  filter(Position == 1) %>%
  group_by(treatment, LimComm, Round) %>%
  mutate(Waterflux_mean = mean(Waterflux),
         Waterflux_SD = sd(Waterflux)) %>%
  ggplot() +
  geom_line(aes(x = Round, y = Waterflux_mean)) +
  geom_line(aes(x = Round, y = Waterflux_mean + Waterflux_SD), linetype = "dashed") +
  geom_line(aes(x = Round, y = Waterflux_mean - Waterflux_SD), linetype = "dashed") +
  # geom_line(aes(x = Round, y = Water_capacity), colour = "grey") +
  facet_grid(LimComm ~ treatment, labeller = "label_both") +
  labs(y = "Water flow [cfps]", title = "Actual water flow in each round")

filename <- "Waterflow_by_Treatment.png"
ggsave(filename, path = "./Graphics", dpi = 300, device = "png", width = 11.69, height = 8.27, units = "in") #A4


# Utilization of water capacity
exp.df %>%
  filter(Position == 1) %>%
  group_by(treatment, LimComm, Round) %>%
  mutate(Utilization_mean = mean(Water_capacity - Water_rain),
         Utilization_SD = sd(Water_capacity - Water_rain),
         Utilization_max = max(Water_capacity - Water_rain),
         Utilization_min = min(Water_capacity - Water_rain)) %>%
  ggplot() +
  geom_line(aes(x = Round, y = Utilization_mean)) +
  geom_line(aes(x = Round, y = Utilization_mean + Utilization_SD), linetype = "dashed") +
  geom_line(aes(x = Round, y = Utilization_mean - Utilization_SD), linetype = "dashed") +
  geom_line(aes(x = Round, y = Utilization_min), colour = "grey") +
  geom_line(aes(x = Round, y = Utilization_max), colour = "grey") +
  # geom_line(aes(x = Round, y = Water_capacity), colour = "grey") +
  facet_grid(LimComm ~ treatment, labeller = "label_both") +
  labs(y = "Supported water flow - theoretical water flow [cfps]", title = "Utilization of water capacity in each round")

filename <- "Capacity_utilization_by_Treatment.png"
ggsave(filename, path = "./Graphics", dpi = 300, device = "png", width = 11.69, height = 8.27, units = "in") #A4


# Total extraction
exp.df %>%
  filter(Position == 1) %>%
  ggplot() +
  geom_line(aes(x = Round, y = Extraction_group, colour = factor(LimComm), group = c(Group))) +
  facet_grid(. ~ treatment, labeller = "label_both") +
  labs(y = "Group extraction [tokens]", colour = "Limited \nCommunication", title = "Total water collection for different treatments")

filename <- "Extraction_group_by_Treatment.png"
ggsave(filename, path = "./Graphics/", dpi = 300, device = "png", width = 11.69, height = 8.27, units = "in") #A4

# Gini investment
summary_withingroups %>%
  # filter(Position == 1) %>%
  ggplot() +
  geom_line(aes(x = Round, y = Gini_investment_mean, colour = factor(LimComm), group = c(Group))) +
  facet_grid(. ~ treatment, labeller = "label_both") +
  labs(y = "Gini", colour = "Limited \nCommunication", title = "Investment inequality for different treatments")

filename <- "Gini_invest_by_Treatment.png"
ggsave(filename, path = "./Graphics", dpi = 300, device = "png", width = 11.69, height = 8.27, units = "in") #A4


# Gini extraction
summary_withingroups %>%
  # filter(Position == 1) %>%
  ggplot() +
  geom_line(aes(x = Round, y = Gini_extraction_mean, colour = factor(LimComm), group = c(Group))) +
  facet_grid(. ~ treatment, labeller = "label_both") +
  labs(y = "Gini", colour = "Limited \nCommunication", title = "Extraction inequality for different treatments")

filename <- "Gini_extract_by_Treatment.png"
ggsave(filename, path = "./Graphics", dpi = 300, device = "png", width = 11.69, height = 8.27, units = "in") #A4

#### correlograms ####

library(corrgram)
exp.df %>%
  select(-c(Water_rain_t.1, LVI, HVI, LVW, HVW, Infra_decline)) %>%
  corrgram(lower.panel = panel.pie, upper.panel = panel.pts, diag.panel = panel.density, main = "Overall correlogram")

exp.df %>% as.data.frame() %>%
  filter(treatment == "baseline" & LimComm == 0) %>%
  select(-c(Water_rain_t.1, LVI, HVI, LVW, HVW, Infra_decline, LimComm, treatment, Water_rain, Group, diff_Invest)) %>%
  corrgram(lower.panel = panel.pie, upper.panel = panel.pts, diag.panel = panel.density, main = "Baseline_FullComm")

exp.df %>% as.data.frame() %>%
  filter(treatment == "LVI" & LimComm == 0) %>%
  select(-c(Water_rain_t.1, LVI, HVI, LVW, HVW, LimComm, treatment, Water_rain, diff_Invest, Group)) %>%
  corrgram(lower.panel = panel.pie, upper.panel = panel.pts, diag.panel = panel.density, main = "LVI_FullComm")


exp.df %>% as.data.frame() %>%
  filter(treatment == "HVW" & LimComm == 0) %>%
  select(-c(Water_rain_t.1, LVI, HVI, LVW, HVW, LimComm, Infra_decline, treatment, diff_Invest, Group)) %>%
  corrgram(lower.panel = panel.pie, upper.panel = panel.pts, diag.panel = panel.density, main = "HVW_FullComm")


#### stability of behviour ####
library(dplyr)
test <- exp.df %>%
  group_by(treatment, LimComm, Round, Group) %>%
  summarise(diff_Invest_group = sum(abs(diff_Invest))) %>%
  group_by(treatment, LimComm, Round) %>%
  summarise(sd_diff_Invest_group = sd(diff_Invest_group),
            mean_diff_Invest_group = mean(diff_Invest_group))

ggplot(test, aes(x = Round, y = mean_diff_Invest_group)) +
  geom_line(
    # aes(group = Group)
  ) +
  geom_line(aes(y = mean_diff_Invest_group + sd_diff_Invest_group), linetype = "dashed") +
  geom_line(aes(y = mean_diff_Invest_group - sd_diff_Invest_group), linetype = "dashed") +
  facet_grid(LimComm ~ treatment, labeller = label_both) +
  labs(title = "How much do groups change their investment from one round to the next?", 
       y = "change in investment (mean ± SD)")
filename <- "diff_Invest_group.png"
ggsave(filename, path = "./Graphics", dpi = 300, device = "png", width = 11.69, height = 8.27, units = "in")