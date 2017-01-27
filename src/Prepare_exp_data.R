# Read in experimental dataset
library(xlsx)
exp.df.raw <- read.xlsx2(file = "Data_import.xlsx", sheetIndex = 1, header = T, 
                     # colClasses = c(rep("character", 8), rep("numeric",13)))
                     colClasses = c(rep("numeric",21)))

exp.df <- filter(exp.df.raw, 
                  Group >= 0 & 
                    Round >= 0 & 
                    Position >= 0 & 
                    LimComm >= 0 & 
                    LVI >= 0 & 
                    HVI >= 0 & 
                    LVW >= 0 & 
                    HVW >= 0)

a <- character(length=length(exp.df$LVI))
a[which(exp.df$LVI == 1)] <- "LVI"
a[which(exp.df$HVI == 1)] <- "HVI"
a[which(exp.df$LVW == 1)] <- "LVW"
a[which(exp.df$HVW == 1)] <- "HVW"
a[which(exp.df$LVI == 0 & exp.df$HVI == 0 & exp.df$LVW == 0 & exp.df$HVW == 0)] <- "baseline"
exp.df$treatment <- as.factor(a)


group_totals <- exp.df  %>%
  # group_by(LVI, HVI, LVW, HVW, LimComm, Round, Group) %>%
  group_by(treatment, LimComm, Round, Group) %>%
  summarise(Investment_group = sum(Investment),
            Extraction_group = sum(Extraction),
            Earning_group = sum(Earning_ind))

exp.df <- merge(exp.df, group_totals, all = T)
            

# calculate means and standard deviations across groups and positions for each round
library(dplyr)
library(reshape2)
group_summary <- exp.df  %>%
  group_by(treatment, LimComm, Round) %>%
  summarise(n = length(unique(Group)),
            Infra_mean = mean(Infra_final),
            Infra_SD = sd(Infra_final),
            Investment_mean = mean(Investment),
            Investment_SD = sd(Investment),
            Extraction_mean = mean(Extraction),
            Extraction_SD = sd(Extraction),
            Investment_group_mean = mean(Investment_group),
            Investment_group_SD = sd(Investment_group),
            Extraction_group_mean = mean(Extraction_group),
            Extraction_group_SD = sd(Extraction_group),
            Earning_group_mean = mean(Earning_group),
            Earning_group_SD = sd(Earning_group)) %>% as.data.frame()

# summarise data across groups of the same treatment for each round and position
agent_summary <- exp.df  %>%
  group_by(treatment, LimComm, Round, Position) %>%
  summarise(n = length(unique(Group)),
            Infra_mean = mean(Infra_final),
            Infra_SD = sd(Infra_final),
            Investment_mean = mean(Investment),
            Investment_SD = sd(Investment),
            Extraction_mean = mean(Extraction),
            Extraction_SD = sd(Extraction),
            Gini_investment_mean = mean(Gini_investment),
            Gini_investment_SD = sd(Gini_investment),
            Gini_extraction_mean = mean(Gini_extraction),
            Gini_extraction_SD = sd(Gini_extraction),
            Gini_earnings_mean = mean(Gini_earnings),
            Gini_earnings_SD = sd(Gini_earnings))

# summarise date across positions for each group and round
summary_withingroups <- exp.df  %>%
  group_by(treatment, LimComm, Round, Group) %>%
  summarise(n = length(unique(Group)),
            Infra_decline = mean(Infra_decline),
            Infra_mean = mean(Infra_final),
            Infra_SD = sd(Infra_final),
            Gini_investment_mean = mean(Gini_investment),
            Gini_investment_SD = sd(Gini_investment),
            Gini_extraction_mean = mean(Gini_extraction),
            Gini_extraction_SD = sd(Gini_extraction),
            Gini_earnings_mean = mean(Gini_earnings),
            Gini_earnings_SD = sd(Gini_earnings)) %>% 
  as.data.frame()


#### extract infrastructure data for export into NL model ####
a <- filter(agent_summary, 
            LimComm == 0 
            & treatment == "LVI"
            & Position == 1
            & Round > 10) %>%
  as.data.frame() %>% 
  select(Round, Infra_mean : Gini_earnings_SD) %>%
  round(digits = 2) 
a[,"Infra_mean"]


#### extract agent extraction and investment data for export into NL model ####
# (adjust treatments and position as necessary)
a <- filter(agent_summary, 
       LimComm == 0 
       & treatment == "HVW"
       & Position == 1
       & Round > 10)%>%
  as.data.frame() %>% 
  select(Round, Investment_mean : Gini_earnings_SD) %>%
  round(digits = 2) 
a[,"Investment_mean"]


##### extract Gini data for export into NL model ####
# re-run after recalculating and correcting Gini coefficients
# (adjust treatments as necessary)
a <- filter(agent_summary, 
            LimComm == 0
            & treatment == "LVI"
            & Position == 1
            & Round > 10) %>%
  as.data.frame() %>% 
  select(Round, Investment_mean : Gini_earnings_SD) %>%
  round(digits = 3) 
a[,"Gini_investment_mean"]




##### recalculate Gini coefficients #####

# For HVI with LimComm, Gini_extraction_mean returns NaN for rounds 14 and 15 as well as 18-20
agent_summary %>%
  filter(treatment == "HVI" & Round %in% c(14, 15, 18:20) & LimComm == 1) %>%
  select(treatment, LimComm, Round, Position, Gini_extraction_mean, Gini_extraction_SD) %>%
  as.data.frame()

exp.df %>%
  filter(treatment == "HVI" #& Round %in% c(14, 15, 18:20) 
         & LimComm == 1) %>%
  select(treatment, LimComm, Round, Group, Position, Extraction, Gini_extraction) %>%
  as.data.frame() %>%View()


max(summary_withingroups$Gini_extraction_SD)
unique(summary_withingroups$Gini_investment_SD) # 3 values are unequal to 0 (e-10)

# Gini_extraction differs often among groups - why?!
unique(summary_withingroups[summary_withingroups$Gini_extraction_SD > 0,"Group"])
# These are the groups where Gini_extraction differs within the group in at least 1 round
# [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 NA

summary_withingroups$Infra_SD
filter(summary_withingroups, Group == is.na(Group))
unique(summary_withingroups$Group)
unique(summary_withingroups[summary_withingroups$Gini_extraction_SD > 0,"Round"])

Ginis <- exp.df  %>%
  group_by(treatment, LimComm, Round, Group) %>%
  summarise(inv_NA = anyNA(Gini_investment),
            ext_NA = anyNA(Gini_extraction),
            inv_0 = any(Gini_investment == 0),
            ext_0 = any(Gini_extraction == 0))
# LVI              HVI             LVW              HVW            LimComm           Round      
# Min.   :0.0000   Min.   :0.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   : 1.00  
# 1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.: 5.75  
# Median :0.0000   Median :0.000   Median :0.0000   Median :0.0000   Median :1.0000   Median :10.50  
# Mean   :0.1136   Mean   :0.125   Mean   :0.1136   Mean   :0.1477   Mean   :0.5227   Mean   :10.50  
# 3rd Qu.:0.0000   3rd Qu.:0.000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:15.25  
# Max.   :1.0000   Max.   :1.000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :20.00  
# Group         inv_NA          ext_NA          inv_0           ext_0        
# Min.   : 1.00   Mode :logical   Mode :logical   Mode :logical   Mode :logical  
# 1st Qu.:11.75   FALSE:880       FALSE:856       FALSE:691       FALSE:492      
# Median :22.50   NA's :0         TRUE :24        TRUE :189       TRUE :364      
# Mean   :22.50                   NA's :0         NA's :0         NA's :24       
# 3rd Qu.:33.25                                                                  
# Max.   :44.00                                                                  

library(ineq)
Gini_new <- exp.df  %>%
  group_by(treatment, LimComm, Round, Group) %>%
  summarise(Gini_investment_new = if (sum(Investment) == 0) {0} else { Gini(Investment) },
            Gini_extraction_new = if (sum(Extraction) == 0) {0} else { Gini(Extraction) },
            Gini_earnings_new = if (sum(Earning_ind) == 0) {0} else { Gini(Earning_ind) }
            ) %>% 
  as.data.frame()

# merge tables with new and old Gini values and round to 6 digits 
Compare_Gini <- merge(summary_withingroups, Gini_new) %>% 
  as.data.frame() %>% 
  select(treatment:Group, Gini_investment_new, Gini_investment_mean, Gini_investment_SD, 
         Gini_extraction_new, Gini_extraction_mean, Gini_extraction_SD, 
         Gini_earnings_new, Gini_earnings_mean, Gini_earnings_SD) 

Compare_Gini <- Compare_Gini %>%
  mutate_each(funs(round(., digits = 6)), Gini_investment_new:Gini_earnings_SD)

with(Compare_Gini, identical(Gini_investment_new, Gini_investment_mean))
# [1] TRUE

with(Compare_Gini, identical(Gini_earnings_new, Gini_earnings_mean))
# [1] TRUE

with(Compare_Gini, identical(Gini_extraction_new, Gini_extraction_mean))
# [1] FALSE


# insert new (= correct) Gini coefficients into exp.df
exp.df.new <- select(exp.df, - c(Gini_investment : Gini_earnings))
Gini_new <- rename(Gini_new, 
                   Gini_investment = Gini_investment_new, 
                   Gini_extraction = Gini_extraction_new,
                   Gini_earnings = Gini_earnings_new
)
exp.df.new <- merge(exp.df.new, Gini_new, all = T)

exp.df <- exp.df.new


#### calculate changes of investment levels across rounds ####

# create new variable that shows the change of investment in each obs.
exp.df <- exp.df %>%
  group_by(treatment, LimComm, Group, Position) %>%
  arrange(Round) %>%
  mutate(diff_Invest = append(NA, diff(Investment))) 

# means across groups
test <- exp.df %>%
  group_by(treatment, LimComm, Round, Position) %>% 
  mutate(diff_m10 = mean(diff_Invest == "-10"),
            diff_m9 = mean(diff_Invest == "-9"),
            diff_m8 = mean(diff_Invest == "-8"),
            diff_m7 = mean(diff_Invest == "-7"),
            diff_m6 = mean(diff_Invest == "-6"),
            diff_m5 = mean(diff_Invest == "-5"),
            diff_m4 = mean(diff_Invest == "-4"),
            diff_m3 = mean(diff_Invest == "-3"),
            diff_m2 = mean(diff_Invest == "-2"),
            diff_m1 = mean(diff_Invest == "-1"),
            diff_0 = mean(diff_Invest == "0"),
            diff_1 = mean(diff_Invest == "1"),
            diff_2 = mean(diff_Invest == "2"),
            diff_3 = mean(diff_Invest == "3"),
            diff_4 = mean(diff_Invest == "4"),
            diff_5 = mean(diff_Invest == "5"),
            diff_6 = mean(diff_Invest == "6"),
            diff_7 = mean(diff_Invest == "7"),
            diff_8 = mean(diff_Invest == "8"),
            diff_9 = mean(diff_Invest == "9"),
            diff_10 = mean(diff_Invest == "10"))

## test area
test <- exp.df %>%
  group_by(treatment, LimComm, Group, Position) %>%
  arrange(Round) %>%
  mutate(diff_Invest = append(NA, diff(Investment)))
test$diff_Invest1 <- as.factor(as.character(test$diff_Invest))

test%>% as.data.frame() %>% filter(LVI == 1, LimComm == 0, Round %in% c(12, 13)) %>% select(Group, Round, Position, Investment, diff_Invest) %>% arrange(Round, Position) 

# means  across groups
test1 <- exp.df %>%
  group_by(treatment, LimComm, Round, Position) %>% 
  summarise(diff_m10 = sum(diff_Invest == "-10"),
            diff_m9 = sum(diff_Invest == "-9"),
            diff_m8 = sum(diff_Invest == "-8"),
            diff_m7 = sum(diff_Invest == "-7"),
            diff_m6 = sum(diff_Invest == "-6"),
            diff_m5 = sum(diff_Invest == "-5"),
            diff_m4 = sum(diff_Invest == "-4"),
            diff_m3 = sum(diff_Invest == "-3"),
            diff_m2 = sum(diff_Invest == "-2"),
            diff_m1 = sum(diff_Invest == "-1"),
            diff_0 = sum(diff_Invest == "0"),
            diff_1 = sum(diff_Invest == "1"),
            diff_2 = sum(diff_Invest == "2"),
            diff_3 = sum(diff_Invest == "3"),
            diff_4 = sum(diff_Invest == "4"),
            diff_5 = sum(diff_Invest == "5"),
            diff_6 = sum(diff_Invest == "6"),
            diff_7 = sum(diff_Invest == "7"),
            diff_8 = sum(diff_Invest == "8"),
            diff_9 = sum(diff_Invest == "9"),
            diff_10 = sum(diff_Invest == "10"))

test1%>% as.data.frame() %>% filter(treatment == "LVI", LimComm == 0, Round %in% c(12, 13)) %>% select(Round, Position, diff_0) %>% arrange(Round, Position) 

test1 <- test1 %>% 
  mutate(sum_diff = diff_m10 + diff_m9 + diff_m8 + diff_m7 + diff_m6 + diff_m5 + diff_m4 + diff_m3 + diff_m2 + diff_m1 + diff_0 + diff_1 + diff_2 + diff_3 + diff_4 + diff_5 + diff_6 + diff_7 + diff_8 + diff_9 + diff_10) %>% as.data.frame() 

test2 <- test1 %>%
  group_by(treatment, LimComm, Round, Position) %>% 
  mutate(diff_m10_rel = diff_m10 / sum_diff,
            diff_m9_rel = diff_m9 / sum_diff,
            diff_m8_rel = diff_m8 / sum_diff,
            diff_m7_rel = diff_m7 / sum_diff,
            diff_m6_rel = diff_m6 / sum_diff,
            diff_m5_rel = diff_m5 / sum_diff,
            diff_m4_rel = diff_m4 / sum_diff,
            diff_m3_rel = diff_m3 / sum_diff,
            diff_m2_rel = diff_m2 / sum_diff,
            diff_m1_rel = diff_m1 / sum_diff,
            diff_0_rel = diff_0 / sum_diff,
            diff_1_rel = diff_1 / sum_diff,
            diff_2_rel = diff_2 / sum_diff,
            diff_3_rel = diff_3 / sum_diff,
            diff_4_rel = diff_4 / sum_diff,
            diff_5_rel = diff_5 / sum_diff,
            diff_6_rel = diff_6 / sum_diff,
            diff_7_rel = diff_7 / sum_diff,
            diff_8_rel = diff_8 / sum_diff,
            diff_9_rel = diff_9 / sum_diff,
            diff_10_rel = diff_10 / sum_diff)

test3 <- test2 %>%
  group_by(LVI, HVI, LVW, HVW, LimComm, Position) %>% 
  summarise(sum_diff_m10 = sum(diff_m10, na.rm = T),
         sum_diff_m9 = sum(diff_m9, na.rm = T),
         sum_diff_m8 = sum(diff_m8, na.rm = T),
         sum_diff_m7 = sum(diff_m7, na.rm = T),
         sum_diff_m6 = sum(diff_m6, na.rm = T),
         sum_diff_m5 = sum(diff_m5, na.rm = T),
         sum_diff_m4 = sum(diff_m4, na.rm = T),
         sum_diff_m3 = sum(diff_m3, na.rm = T),
         sum_diff_m2 = sum(diff_m2, na.rm = T),
         sum_diff_m1 = sum(diff_m1, na.rm = T),
         sum_diff_0 = sum(diff_0, na.rm = T),
         sum_diff_1 = sum(diff_1, na.rm = T),
         sum_diff_2 = sum(diff_2, na.rm = T),
         sum_diff_3 = sum(diff_3, na.rm = T),
         sum_diff_4 = sum(diff_4, na.rm = T),
         sum_diff_5 = sum(diff_5, na.rm = T),
         sum_diff_6 = sum(diff_6, na.rm = T),
         sum_diff_7 = sum(diff_7, na.rm = T),
         sum_diff_8 = sum(diff_8, na.rm = T),
         sum_diff_9 = sum(diff_9, na.rm = T),
         sum_diff_10 = sum(diff_10, na.rm = T))

Group1 <- test %>% filter(Group == 1) %>% select(Position, Round, Investment, diff_Invest) %>% as.data.frame()


