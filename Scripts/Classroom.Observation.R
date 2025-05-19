# Classroom Observation scoring Sheet Descriptive Results ####\
# Libraries ####


##  Data Script  ####
source("Scripts/PLAY.Data.R")

## Pilot & inter-rater reliability data ####
raw.pilot <- CO.Scoring.Sheet %>%
  filter(select_type_of_assessment=="pilot_data_assessment")

raw.irr <- CO.Scoring.Sheet %>%
  filter(select_type_of_assessment=="irr") 

dim(raw.pilot)
dim(raw.irr)


# Check for duplicates in IRR data:  Check for paired observations.
# dupes.raw.irr - DF containing all paired observations.
dupes.raw.irr <- raw.irr %>%
  group_by(name_of_school, grade, lesson_subject, date_of_observation) %>%
  mutate(dupe =n()>1) %>%
  filter(dupe ==TRUE)


# distinct.raw.irr - A DF containing unique observations from irr data. to be merged with Pilot data for analysis
distinct.raw.irr <-  dupes.raw.irr[!duplicated(dupes.raw.irr[,c('name_of_school', 'grade', 'lesson_subject', 'date_of_observation')]),]
distict.raw.irr <- distinct.raw.irr %>%
  dplyr::select(-dupe)

# dim(distict.raw.irr)
# dim(raw.pilot)

pilot.data <- rbind(raw.pilot, distict.raw.irr)


# DF for Internal consistency
ic.pilot.data <- pilot.data %>%
  dplyr::select(22:46)


# Constructs
pilot.COX <- ic.pilot.data %>%
  dplyr::select(COX1, COX2, COX3, COX4, COX5, COX6)

# COA: Support for Agency
pilot.COA <- ic.pilot.data %>%
  dplyr::select(COA1, COA2, COA3, COA4, COA5, COA6, COA7)

# COC: Support for personal and social connection 
pilot.COC <- ic.pilot.data %>%
  dplyr::select(COC1,COC2, COC3, COC4, COC5)

# COE: Support for emotional climate
pilot.COE <- ic.pilot.data %>%
  dplyr::select(COE1,COE2,COE3,COE4,COE5,COE6,COE7)

## Descriptive Statistics ####

### COX: Support from Exploration ####
# Histograms per item
# stacked.COX <- pilot.COX %>%
#   # group_by(.dots = names(.)) %>%
#   # group_by(!!! rlang::syms(names(.))) %>%
#   group_by_all() %>% 
#   summarise(count = n())

# View(stacked.COX)

# stacked.COX <- pilot.COX %>%
#   pivot_longer(cols = c( "Teacher gives hints or suggestions to encourage students to continue to get to the answer or explore the concept",
#                          "Teacher asks open-ended questions or prompts for students to share their opinions or preferences", 
#                          "Teacher asks questions to generate explanations/reasons for phenomena or results",
#                          "Teacher promotes students thinking by themselves by asking a comparison categorization or prediction question or through a task they give them",
#                          "Teacher gives student(s) a chance to try or explore something first before being shown how to use / answer it"), names_to = 'Construct', 
#                values_to = 'count')
  
  

hist.COX1 <- pilot.COX %>%
  dplyr::select(COX1) %>%
  group_by(COX1) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COX1, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.4, size = 3, color ="#000000", show.legend = F)


hist.COX2 <- pilot.COX %>%
  dplyr::select(COX2) %>%
  group_by(COX2) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COX2, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.4, size = 3, color ="#000000", show.legend = F)

hist.COX3 <- pilot.COX %>%
  dplyr::select(COX3) %>%
  group_by(COX3) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COX3, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.4, size = 3, color ="#000000", show.legend = F)


hist.COX4 <- pilot.COX %>%
  dplyr::select(COX4) %>%
  group_by(COX4) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COX4, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.4, size = 3, color ="#000000", show.legend = F)


hist.COX5 <- pilot.COX %>%
  dplyr::select(COX5) %>%
  group_by(COX5) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COX5, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.4, size = 3, color ="#000000", show.legend = F)


hist.COX6 <- pilot.COX %>%
  dplyr::select(COX6) %>%
  group_by(COX6) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COX6, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.4, size = 3, color ="#000000", show.legend = F)

# Histograms in the grid
COX.hists <- plot_grid(hist.COX1, hist.COX2, hist.COX3, hist.COX4, hist.COX5, hist.COX6)
COX.hists

# COX.hists1
# Frequency tables of each of the items

# Descriptive stats
COX.descriptive.stats <- describe(pilot.COX, quant = c(.25, .75), IQR = T)
COX.descriptive.stats <- COX.descriptive.stats %>%
  dplyr::select(mean, median, mad,sd, skew, kurtosis, se, Q0.25, Q0.75, IQR) %>%
  rename(Q1 = Q0.25, Q3 = Q0.75)

### COA: Support for Agency ####
COA.descriptive.stats <- describe(pilot.COA, quant = c(.25, .75), IQR = T)
COA.descriptive.stats <- COA.descriptive.stats %>%
  dplyr::select(mean, median, mad,sd, skew, kurtosis, se, Q0.25, Q0.75, IQR) %>%
  rename(Q1 = Q0.25, Q3 = Q0.75)


# Histograms
hist.COA1 <- pilot.COA %>%
  dplyr::select(COA1) %>%
  group_by(COA1) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 1) %>%
  ggplot(aes(COA1, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.5, size = 3, color ="#000000", show.legend = F)

hist.COA2 <- pilot.COA %>%
  dplyr::select(COA2) %>%
  group_by(COA2) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 1) %>%
  ggplot(aes(COA2, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.5, size = 3, color ="#000000", show.legend = F)


hist.COA3 <- pilot.COA %>%
  dplyr::select(COA3) %>%
  group_by(COA3) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 1) %>%
  ggplot(aes(COA3, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.5, size = 3, color ="#000000", show.legend = F)

hist.COA4 <- pilot.COA %>%
  dplyr::select(COA4) %>%
  group_by(COA4) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 1) %>%
  ggplot(aes(COA4, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.5, size = 3, color ="#000000", show.legend = F)

hist.COA5 <- pilot.COA %>%
  dplyr::select(COA5) %>%
  group_by(COA5) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 1) %>%
  ggplot(aes(COA5, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.5, size = 3, color ="#000000", show.legend = F)

hist.COA6 <- pilot.COA %>%
  dplyr::select(COA6) %>%
  group_by(COA6) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 1) %>%
  ggplot(aes(COA6, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.5, size = 3, color ="#000000", show.legend = F)

hist.COA7 <- pilot.COA %>%
  dplyr::select(COA7) %>%
  group_by(COA7) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 1) %>%
  ggplot(aes(COA7, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.5, hjust=0.6, size = 3, color ="#000000", show.legend = F)

COA.hists <- plot_grid(hist.COA1, hist.COA2, hist.COA3, hist.COA4, hist.COA5, hist.COA6, hist.COA7)
COA.hists



### COC: Support for personal and social connection  ####
COC.descriptive.stats <- describe(pilot.COC, quant = c(.25, .75), IQR = T)
COC.descriptive.stats <- COC.descriptive.stats %>%
  select(mean, median, mad,sd, skew, kurtosis, se, Q0.25, Q0.75, IQR) %>%
  rename(Q1 = Q0.25, Q3 = Q0.75)


# Histograms
hist.COC1 <- pilot.COC %>%
  dplyr::select(COC1) %>%
  group_by(COC1) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COC1, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.4, size = 3, color ="#000000", show.legend = F)

hist.COC2 <- pilot.COC %>%
  dplyr::select(COC2) %>%
  group_by(COC2) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COC2, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.4, size = 3, color ="#000000", show.legend = F)


hist.COC3 <- pilot.COC %>%
  dplyr::select(COC3) %>%
  group_by(COC3) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COC3, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.4, size = 3, color ="#000000", show.legend = F)

hist.COC4 <- pilot.COC %>%
  dplyr::select(COC4) %>%
  group_by(COC4) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COC4, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.4, size = 3, color ="#000000", show.legend = F)

hist.COC5 <- pilot.COC %>%
  dplyr::select(COC5) %>%
  group_by(COC5) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COC5, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.4, size = 3, color ="#000000", show.legend = F)

COC.hists <- plot_grid(hist.COC1, hist.COC2, hist.COC3, hist.COC4, hist.COC5)
COC.hists

### COE: Support for emotional climate ####
COE.descriptive.stats <- describe(pilot.COE, quant = c(.25, .75), IQR = T)
COE.descriptive.stats <- COE.descriptive.stats %>%
  select(mean, median, mad,sd, skew, kurtosis, se, Q0.25, Q0.75, IQR) %>%
  rename(Q1 = Q0.25, Q3 = Q0.75)

# Histograms
hist.COE1 <- pilot.COE %>%
  dplyr::select(COE1) %>%
  group_by(COE1) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COE1, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.5, size = 3, color ="#000000", show.legend = F)

hist.COE2 <- pilot.COE %>%
  dplyr::select(COE2) %>%
  group_by(COE2) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COE2, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.5, size = 3, color ="#000000", show.legend = F)


hist.COE3 <- pilot.COE %>%
  dplyr::select(COE3) %>%
  group_by(COE3) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COE3, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.5, size = 3, color ="#000000", show.legend = F)

hist.COE4 <- pilot.COE %>%
  dplyr::select(COE4) %>%
  group_by(COE4) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COE4, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.5, size = 3, color ="#000000", show.legend = F)

hist.COE5 <- pilot.COE %>%
  dplyr::select(COE5) %>%
  group_by(COE5) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COE5, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.5, size = 3, color ="#000000", show.legend = F)


hist.COE6 <- pilot.COE %>%
  dplyr::select(COE6) %>%
  group_by(COE6) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COE6, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.5, size = 3, color ="#000000", show.legend = F)


hist.COE7 <- pilot.COE %>%
  dplyr::select(COE7) %>%
  group_by(COE7) %>%
  summarise(count = n()) %>%
  mutate(`%` = count/sum(count)*100) %>%
  mutate_if(is.numeric, round, 0) %>%
  ggplot(aes(COE7, count)) +
  geom_col(fill=qlp2) +
  theme_minimal() + 
  geom_label(aes(label = paste0(scales::comma(count),  " (",`%`,"%", ")")), position = position_dodge(0.9), vjust = 0.5, size = 3, color ="#000000", show.legend = F)


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


# Correlation Analysis

#  Internal Consistency  Analysis ####

# REF: https://rforhr.com/cronbachsalpha.html

`Cronbach’s alpha (α)` <- c('.95-1.00','.90-.94','.80-.89','.70-.79','.60-.69','.00-.59')
Intepretation <- c('Excellent','Great','Good','Acceptable','Questionable','Unacceptable')
IC.Descriptors <- data.frame(`Cronbach’s alpha (α)`, Intepretation)

IC.Descriptors <- IC.Descriptors %>%
  flextable() %>%
  border_inner() %>%
  border_outer() %>%
  bold(bold = T, part = "header")

# Anything before a comma represents row, anything after the comma references columns


set.seed(45664)
# COX1
ic.COX <- alpha(ic.pilot.data[,c("COX1","COX2","COX3", "COX4", "COX5","COX6")])
ic.COX <- ic.COX$total

# COA: Support for Agency.
ic.COA <- alpha(ic.pilot.data[,c("COA1","COA2","COA3", "COA4", "COA5","COA6", "COA7")], check.keys=TRUE)
ic.COA <- ic.COA$total

# COC: Support for personal and social connection. 
ic.COC <- alpha(ic.pilot.data[,c("COC1","COC2","COC3", "COC4", "COC5")])
ic.COC <- ic.COC$total

# COE: Support for emotional climate.
ic.COE <- alpha(ic.pilot.data[,c("COE1","COE2","COE3", "COE4", "COE5","COE6", "COE7")], check.keys=TRUE)
ic.COE <- ic.COE$total

# Items
# Format internal consistency results to a dataframe.
ic.DF <- rbind(ic.COX, ic.COA, ic.COC, ic.COE)

ic.DF <- ic.DF %>%
  mutate_if(is.numeric, round, 2) %>%
  rename(`Raw alpha` = raw_alpha, 
         `Std alpha` = std.alpha)

Item = c("COX","COA","COC", "COE")
ic.DF['Construct'] <- Item

ic.DF<-ic.DF[,c(10, 1:9)]


ic.DF <- ic.DF %>%
  flextable() %>%
  border_inner() %>%
  border_outer() %>%
  bold(bold = T, part = "header")

# Correlation Analysis ####
corr.plot.ic <- corrplot(cor(ic.pilot.data, use="complete.obs"), order = "original", tl.col='black', tl.cex=.75) 


# Correlation matrix
co.matrix <- cor(ic.pilot.data)

# Factor Analysis ####

# Conduct KMO to the dataset
# KMO(co.matrix, use = "complete.obs", cor_method = "pearson")

# Conduct Bartlett’s Test of Sphericity
cb.test <- cortest.bartlett(co.matrix, n = nrow(ic.pilot.data))
# cortest.bartlett(ic.pilot.data)
det.matrix <- det(co.matrix)


scree.plot <- scree(ic.pilot.data)


# Parallel Analysis
parallel <- fa.parallel(ic.pilot.data)
paran.plot <- paran(ic.pilot.data, cfa = TRUE)

pa.plot <- paran(co.matrix, cfa = TRUE, graph = T, color = TRUE, col = c(qlp1, qlp2, qlp3))

# Exploratory Factor Analysis
fa(ic.pilot.data, nfactors = 1, rotate = "oblimin")
object.fa <- fa(ic.pilot.data, nfactors = 2, rotate = "oblimin")

# Get Factor Loadings to CSV

daiagramfa <- fa.diagram(object.fa, main = "2 factor loadings on the constructs")


# efa.co <- fa(ic.pilot.data, nfactors = 2, rotate = "oblimin", fm="minres")
# print(efa.co)
# efa.co$scores

# Confirmatory Factor Analysis ####

# Create a model for confirmatory factor Analysis
model.cfa <- 'COX =~ COX1 + COX2 + COX3 + COX4 + COX5 
              COA =~ COA1+COA2+COA3+COA4+COA5+COA6+COA7
              COC =~ COC1+COC2+COC3+COC4+COC5
              COE =~ COE1+COE2+COE3+COE4+COE5+COE6+COE7'

cfa.model.fit <- cfa(model = model.cfa, data = ic.pilot.data, std.lv = TRUE, missing = "fiml", mimic = "Mplus")
summary(cfa.model.fit, fit.measures=TRUE, ci=TRUE, standardized=TRUE)

# Fit the model using the cfa function 
# fitMeasures(fit.cfa, c("chisq.scaled", "df", "pvalue.scaled"))

# improve indices using modindices
table.mi <-  modindices(cfa.model.fit, minimum.value = 10, sort. = T)

table.mi <- table.mi %>%
  mutate_if(is.numeric, round, 2) %>%
  flextable() %>%
  border_inner() %>%
  border_outer() %>%
  bold(bold = T, part = "header")
  

# Re-run CFA model with the two covariances.
cov.model.cfa <- 'COX =~ COX1 + COX2 + COX3 + COX4 + COX5 
              COA =~ COA1+COA2+COA3+COA4+COA5+COA6+COA7
              COC =~ COC1+COC2+COC3+COC4+COC5
              COE =~ COE1+COE2+COE3+COE4+COE5+COE6+COE7
              COA5 ~~ COA6'

cov.model.fit <- cfa(model = cov.model.cfa, data = ic.pilot.data)


cfa.lavaanplot <- lavaanPlot(model = cfa.model.fit, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"),
           coefs = T, stand=T, covs=T)

class(cfa.lavaanplot)

