# Classroom Observation scoring Sheet Descriptive Results ####


# Set seed for creating reproducible results 
set.seed(3335)

##  Data Script  ####
source("Scripts/PLAY.Data.R")


## Pilot data ####
pilot.COX <- CO.Scoring.Sheet %>%
  filter(select_type_of_assessment=="pilot_data_assessment") %>%
  select(COX1, COX2, COX3, COX4, COX5, COX6)

# COA: Support for Agency
pilot.COA <- CO.Scoring.Sheet %>%
  filter(select_type_of_assessment=="pilot_data_assessment") %>%
  select(COA1, COA2, COA3, COA4, COA5, COA6, COA7)

# COC: Support for personal and social connection 
pilot.COC <- CO.Scoring.Sheet %>%
  filter(select_type_of_assessment=="pilot_data_assessment") %>%
  select(COC1,COC2, COC3, COC4, COC5)

# COE: Support for emotional climate
pilot.COE <- CO.Scoring.Sheet %>%
  filter(select_type_of_assessment=="pilot_data_assessment") %>%
  select(COE1,COE2,COE3,COE4,COE5,COE6,COE7)

## Descriptive Statistics ####

### COX: Support from Exploration ####

# Histograms per item
hist.COX1 <- pilot.COX %>%
  select(COX1) %>%
  ggplot(aes(COX1)) +
  geom_bar(stat="count", width=0.95, fill="steelblue")+
  theme_minimal()

hist.COX2 <- pilot.COX %>%
  select(COX2) %>%
  ggplot(aes(COX2)) +
  geom_bar(stat="count", width=0.95, fill="steelblue")+
  theme_minimal()

hist.COX3 <- pilot.COX %>%
  select(COX3) %>%
  ggplot(aes(COX3)) +
  geom_bar(stat="count", width=0.95, fill="steelblue")+
  theme_minimal()

hist.COX4 <- pilot.COX %>%
  select(COX4) %>%
  ggplot(aes(COX4)) +
  geom_bar(stat="count", width=0.95, fill="steelblue")+
  theme_minimal()

hist.COX5 <- pilot.COX %>%
  select(COX5) %>%
  ggplot(aes(COX5)) +
  geom_bar(stat="count", width=0.95, fill="steelblue")+
  theme_minimal()

hist.COX6 <- pilot.COX %>%
  select(COX6) %>%
  ggplot(aes(COX6)) +
  geom_bar(stat="count", width=0.95, fill="steelblue")+
  theme_minimal()

# Histograms in the grid
COX.hists <- plot_grid(hist.COX1, hist.COX2, hist.COX3, hist.COX4, hist.COX5, hist.COX6)

# Frequency tables of each of the items

# Descriptive stats
COX.descriptive.stats <- describe(pilot.COX, quant = c(.25, .75), IQR = T)
COX.descriptive.stats <- COX.descriptive.stats %>%
  select(mean, median, mad,sd, skew, kurtosis, se, Q0.25, Q0.75, IQR) %>%
  rename(Q1 = Q0.25, Q3 = Q0.75)

# Convert item rownames to columns
# COX.descriptive.stats <- tibble::rownames_to_column(COX.descriptive.stats, "Item")

#Create a flextable object
# COX.descriptive.stats <- COX.descriptive.stats %>%
#   mutate_if(is.numeric, round, 1) %>%
#   flextable() %>%
#   border_inner() %>%
#   border_outer() %>%
#   bold(bold = T, part = "header") %>%
#   add_footer_lines("*mad = median absolute deviation,  *sd = standard deviation, *se = standard error")


### COA: Support for Agency ####
COA.descriptive.stats <- describe(pilot.COA, quant = c(.25, .75), IQR = T)
COA.descriptive.stats <- COA.descriptive.stats %>%
  select(mean, median, mad,sd, skew, kurtosis, se, Q0.25, Q0.75, IQR) %>%
  rename(Q1 = Q0.25, Q3 = Q0.75)

# COA.descriptive.stats <- tibble::rownames_to_column(COA.descriptive.stats, "Item")
# 
# COA.descriptive.stats <- COA.descriptive.stats %>%
#   mutate_if(is.numeric, round, 1) %>%
#   arrange(desc(skew))
#   
# flextable(COA.descriptive.stats)

# Histograms
hist.COA1 <- pilot.COA %>%
  select(COA1) %>%
  group_by(COA1) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COA1, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)

hist.COA2 <- pilot.COA %>%
  select(COA2) %>%
  group_by(COA2) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/74*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COA2, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)


hist.COA3 <- pilot.COA %>%
  select(COA3) %>%
  group_by(COA3) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/74*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COA3, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)

hist.COA4 <- pilot.COA %>%
  select(COA4) %>%
  group_by(COA4) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/74*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COA4, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)

hist.COA5 <- pilot.COA %>%
  select(COA5) %>%
  group_by(COA5) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/74*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COA5, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)

hist.COA6 <- pilot.COA %>%
  select(COA6) %>%
  group_by(COA6) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/74*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COA6, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)

hist.COA7 <- pilot.COA %>%
  select(COA7) %>%
  group_by(COA7) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/74*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COA7, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)

COA.hists <- plot_grid(hist.COA1, hist.COA2, hist.COA3, hist.COA4, hist.COA5, hist.COA6, hist.COA7)
COA.hists



### COC: Support for personal and social connection  ####
COC.descriptive.stats <- describe(pilot.COC, quant = c(.25, .75), IQR = T)
COC.descriptive.stats <- COC.descriptive.stats %>%
  select(mean, median, mad,sd, skew, kurtosis, se, Q0.25, Q0.75, IQR) %>%
  rename(Q1 = Q0.25, Q3 = Q0.75)

# COC.descriptive.stats <- tibble::rownames_to_column(COC.descriptive.stats, "Item")
# 
# COC.descriptive.stats <- COC.descriptive.stats %>%
#   mutate_if(is.numeric, round, 1) %>%
#   arrange(desc(skew))
# 
# flextable(COC.descriptive.stats)

# Histograms
hist.COC1 <- pilot.COC %>%
  select(COC1) %>%
  group_by(COC1) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COC1, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)

hist.COC2 <- pilot.COC %>%
  select(COC2) %>%
  group_by(COC2) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/74*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COC2, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)


hist.COC3 <- pilot.COC %>%
  select(COC3) %>%
  group_by(COC3) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/74*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COC3, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)

hist.COC4 <- pilot.COC %>%
  select(COC4) %>%
  group_by(COC4) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/74*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COC4, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)

hist.COC5 <- pilot.COC %>%
  select(COC5) %>%
  group_by(COC5) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/74*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COC5, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)

COC.hists <- plot_grid(hist.COC1, hist.COC2, hist.COC3, hist.COC4, hist.COC5)
COC.hists

### COE: Support for emotional climate ####
COE.descriptive.stats <- describe(pilot.COE, quant = c(.25, .75), IQR = T)
COE.descriptive.stats <- COE.descriptive.stats %>%
  select(mean, median, mad,sd, skew, kurtosis, se, Q0.25, Q0.75, IQR) %>%
  rename(Q1 = Q0.25, Q3 = Q0.75)

# COE.descriptive.stats <- tibble::rownames_to_column(COE.descriptive.stats, "Item")
# 
# COE.descriptive.stats <- COE.descriptive.stats %>%
#   mutate_if(is.numeric, round, 1) %>%
#   arrange(desc(skew))
# 
# flextable(COE.descriptive.stats)

# Histograms
hist.COE1 <- pilot.COE %>%
  select(COE1) %>%
  group_by(COE1) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COE1, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)

hist.COE2 <- pilot.COE %>%
  select(COE2) %>%
  group_by(COE2) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/74*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COE2, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)


hist.COE3 <- pilot.COE %>%
  select(COE3) %>%
  group_by(COE3) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/74*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COE3, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)

hist.COE4 <- pilot.COE %>%
  select(COE4) %>%
  group_by(COE4) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/74*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COE4, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)

hist.COE5 <- pilot.COE %>%
  select(COE5) %>%
  group_by(COE5) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/74*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COE5, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)


hist.COE6 <- pilot.COE %>%
  select(COE6) %>%
  group_by(COE6) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/74*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COE6, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)


hist.COE7 <- pilot.COE %>%
  select(COE7) %>%
  group_by(COE7) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/74*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COE7, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.0, size = 3, color ="#000000", show.legend = F)


COE.hists <- plot_grid(hist.COE1, hist.COE2, hist.COE3, hist.COE4, hist.COE5, hist.COE6, hist.COE7)
COE.hists


# Combine Desc stats to generate a table
co.pilot.desc.stats <- rbind(COX.descriptive.stats, COA.descriptive.stats, COC.descriptive.stats, COE.descriptive.stats)
co.pilot.desc.stats

# Convert item from rowname to column
co.pilot.desc.stats <- tibble::rownames_to_column(co.pilot.desc.stats, "Item")


# Create a flextable object
co.pilot.desc.stats <- co.pilot.desc.stats %>%
  mutate_if(is.numeric, round, 1) %>%
  flextable() %>%
  border_inner() %>%
  border_outer() %>%
  bold(bold = T, part = "header") %>%
  add_footer_lines("*mad = median absolute deviation,  *sd = standard deviation, *se = standard error")

CO.SC.numeric <- CO.Scoring.Sheet %>%
  select(COX1:COE7) %>%
  mutate_if(is.character,as.numeric)

graphics.off()
par("mar") 
par(mar=c(1,1,1,1))
pairs(CO.SC.numeric[, c(1:5)], main = "Scatter Plot Matrix for Classroom Scoring Sheet Data")

## Factor Analysis ####




