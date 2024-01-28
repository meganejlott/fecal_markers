#Load Libraries
library(tidyverse)
library(magrittr)
library(ggstatsplot)
library(reshape2)
library(viridis)
library(ggpubr)
library(ggthemes)
library(fifer)

#Load Data 
data_full = read_csv("./data/supplementary_data_21JUNE2023.csv")


#################################################################################
############################Filter for QC Flags##################################
#################################################################################

data = 
  data_full %>%
  filter(participant_id != "NA") %>%
  filter(Target != "NA") %>%
  filter(Target != "BCOV") %>%
  filter(Target != "IPC") %>%
  pivot_wider(
    id_cols = c("participant_id"), 
    names_from = "Target", 
    values_from = c("reps_pos", "Cq_ave", "Cq_sd", "Cq_cov",
      "copies_gram_stool_ave", "copies_gram_stool_std", "copies_gram_stool_cov", 
      "copies_h20", "copies_pbs", "detection", "QC_code", "group"), 
    names_sep = ".") 

col.names = names(data)

data = 
  data %>% 
  filter(QC_code.pmmov != "Discard due to Contamination") %>% 
  filter(QC_code.pmmov != "Failed PCR") %>% 
  filter(QC_code.crassphage != "Discard due to Contamination") %>% 
  filter(QC_code.crassphage != "Failed PCR") %>% 
  filter(QC_code.HF183 != "Discard due to Contamination") %>% 
  filter(QC_code.HF183 != "Failed PCR")
  

#Reshape into a useable form. 

test = 
  data %>%
  melt(id.vars = c("participant_id")) %>% 
  mutate(variable = as.character(variable)) %>%
  separate(col = variable, into = c("variable", "Target"), sep = "\\.") %>%
  pivot_wider(names_from = "variable", values_from = "value")

##################################################################################
#############################Detection Frequency##################################
##################################################################################

#From the current data, how many samples are positive for each marker? 

#Total 
test %>% 
  mutate(detection = as.numeric(detection)) %>%
  dplyr::group_by(Target) %>% 
  dplyr::summarize(n = n(), 
                   n.pos = sum(detection)) %>%
  mutate(p.pos = n.pos/n*100)


#Adult Vs. Child
test %>% 
  mutate(detection = as.numeric(detection)) %>%
  dplyr::group_by(Target, group) %>% 
  dplyr::summarize(n = n(), 
                   n.pos = sum(detection)) %>%
  mutate(p.pos = n.pos/n*100)


#How many samples were positive for all three markers? 
data %>% 
  ungroup() %>%
  filter(copies_gram_stool_ave > 1) %>%
  mutate(log_copies = log10(copies_gram_stool_ave)) %>%
  dplyr::select(Sample, log_copies, Target) %>% 
  pivot_wider(names_from = Target, values_from = log_copies, id_cols = Sample) %>%
  drop_na()

#How many samples were positive for two or more markers?
data %>% 
  dplyr::group_by(Sample) %>%
  dplyr::summarize(n.pos = sum(detection)) %>%
  filter(n.pos > 1) 

#How many samples were negative for all three markers? 
test = data %>% 
  ungroup() %>%
  mutate(log_copies = log10(copies_gram_stool_ave)) %>%
  dplyr::select(Sample, log_copies, Target) %>% 
  pivot_wider(names_from = Target, values_from = log_copies, id_cols = Sample) %>%
  drop_na()

#How many samples were positive for only one marker?
data %>% 
  dplyr::group_by(Sample) %>%
  dplyr::summarize(n.pos = sum(detection)) %>%
  filter(n.pos == 1) 

#Is frequency of detection different between the markers, significantly?
fisher.test(data$Target, data$detection, simulate.p.value=TRUE)
chisq.post.hoc(table(data$Target, data$detection))

adults = data %>% filter(group == "adult")
chisq.post.hoc(table(adults$Target, adults$detection))

children = data %>% filter(group == "child")
fisher.test(children$Target, children$detection, simulate.p.value=TRUE)


data %>% 
  ggbarstats(
    x = detection, 
    y = Target
  )


#Is the detection of crAssphage independant of the detection of HF183 & PMMoV?
test = data %>% 
  mutate(detection = as.character(detection)) %>%
  mutate(detection = factor(detection)) %>%
  dplyr::select(Sample, Target, detection) %>% 
  pivot_wider(names_from = "Target", values_from = "detection")

#fisher.test(test$crassphage, test$HF183, simulate.p.value=TRUE)
#fisher.test(test$crassphage, test$pmmov, simulate.p.value=TRUE)
#fisher.test(test$pmmov, test$HF183, simulate.p.value=TRUE)


##################################################################################
#############################Concentration Values#################################
##################################################################################

#Distributions of each marker
distributions = data %>% 
  filter(copies_gram_stool_ave > 1) %>% 
  dplyr::select(Sample, copies_gram_stool_ave, Target) %>% 
  pivot_wider(names_from = Target, values_from = copies_gram_stool_ave, id_cols = Sample)

summary(distributions)

#Min, Max, and Med of each marker
distributions = data %>% 
  filter(copies_gram_stool_ave > 1) %>% 
  mutate(log_copies = log10(copies_gram_stool_ave)) %>%
  dplyr::select(Sample, log_copies, Target) %>% 
  pivot_wider(names_from = Target, values_from = log_copies, id_cols = Sample)

summary(distributions)


#What does the distribution look like? 
data %>% 
  filter(copies_gram_stool_ave > 1) %>%
  ggplot(aes(x = log10(copies_gram_stool_ave))) + 
  geom_histogram() + 
  facet_wrap(Target~group, ncol = 2, scales = "free")

cbPalette <- c("#999999", "#E69F00")

cbPalette <- c("#0072B2", "#D55E00")

fig1 = data %>% 
  filter(copies_gram_stool_ave > 1) %>%
  mutate(group = case_when(group == "adult" ~ "Adult", 
                           TRUE ~ "Child")) %>%
  mutate(Target = case_when(Target == "crassphage" ~ "crAssphage",
                            Target == "pmmov" ~ "PMMoV", 
                            TRUE ~ Target)) %>%
  ggplot(aes(x = Target, y = log10(copies_gram_stool_ave))) + 
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6, aes(fill = group), binwidth = 0.5, position=position_dodge(0.8)) + 
  xlab("Marker") + 
  ylab("Log10 (Copies per Gram)") + 
  labs(fill = "") +
  #facet_wrap(~ group, ncol = 1) + 
  scale_fill_grey() +
  theme_few() + 
  theme(panel.grid = element_line(colour = "snow2")) + 
  ylim(0, 15) + 
  theme(text = element_text(size = 20)) 

tiff('./figures/fig1.tiff', units="in", width=12, height=6, res=300, compression = 'lzw')
plot(fig1)
dev.off()

#It appears that crAssphage and HF183 are multimodal, while PMMoV is unimodal.
#Is this statistically accurate? 

library(diptest)
dip.test(distributions$crassphage)
dip.test(distributions$HF183)
dip.test(distributions$pmmov)

library(LaplacesDemon)
is.unimodal(distributions$crassphage)
is.unimodal(distributions$HF183)
is.unimodal(distributions$pmmov)

is.bimodal(distributions$crassphage)
is.bimodal(distributions$HF183)

Modes(distributions$crassphage)$modes
Modes(distributions$HF183)$modes

shapiro.test(distributions$crassphage)
shapiro.test(distributions$HF183)
shapiro.test(distributions$pmmov)


#What does the distribution look like? 
adult = data %>% 
  filter(group == "adult") %>%
  filter(copies_gram_stool_ave > 1) %>%
  ggplot(aes(x = Target, y = log10(copies_gram_stool_ave))) + 
  geom_boxplot() + 
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) + 
  xlab("Marker") + 
  ylab("Log10(Copies per Gram)") + 
  theme_few() + 
  theme(panel.grid = element_line(colour = "snow2")) + 
  ylim(0, 17)



child = data %>% 
  filter(group == "child") %>%
  filter(copies_gram_stool_ave > 1) %>%
  ggplot(aes(x = Target, y = log10(copies_gram_stool_ave))) + 
  geom_boxplot() + 
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) + 
  xlab("Marker") + 
  ylab("Log10(Copies per Gram)") + 
  theme_few() + 
  theme(panel.grid = element_line(colour = "snow2")) + 
  ylim(0,17)

distribution = ggarrange(adult, 
                         child,
                         ncol = 1, 
                         align = "hv", 
                         labels = c("A", "B"))

plot(distribution)



data %>% 
  ungroup() %>%
  filter(copies_gram_stool_ave > 1) %>%
  mutate(log_copies = log10(copies_gram_stool_ave)) %>%
  grouped_ggbetweenstats(
    x = Target,
    y = log_copies,
    grouping.var = group, 
    type = "np", 
    xlab = "Marker", 
    ylab = "Log10 (Copies per Gram)", 
    plotgrid.args = list(nrow = 2),
  )



data %>% 
  ungroup() %>%
  filter(copies_gram_stool_ave > 1) %>%
  mutate(log_copies = log10(copies_gram_stool_ave)) %>%
  grouped_ggbetweenstats(
    x = group,
    y = log_copies,
    grouping.var = Target, 
    type = "np", 
    xlab = "Marker", 
    ylab = "Log10 (Copies per Gram)", 
    plotgrid.args = list(nrow = 2),
  )

#Is the concentration of crAssphage different between adults and children? 
crassphage = data %>% filter(Target == "crassphage")
wilcox.test(data = crassphage, copies_gram_stool_ave~group)

HF183 = data %>% filter(Target == "HF183")
wilcox.test(data = HF183, copies_gram_stool_ave~group)

pmmov = data %>% filter(Target == "pmmov")
wilcox.test(data = pmmov, copies_gram_stool_ave~group)

#Are high shedders of crassphage also high shedders of HF183?

data %>% 
  ungroup() %>%
  filter(copies_gram_stool_ave > 1) %>%
  mutate(log_copies = log10(copies_gram_stool_ave)) %>%
  dplyr::select(Sample, log_copies, Target) %>% 
  filter(Target != "pmmov") %>% 
  pivot_wider(names_from = Target, values_from = log_copies, id_cols = Sample) %>%
  drop_na() %>%
  ggscatterstats(
    x = crassphage,
    y = HF183,
    grouping.var = group, 
    type = "np", 
    xlab = "crAssphage", 
    ylab = "HF183", 
    marginal = FALSE
  )


#Are high shedders of HF183 also high shedders of PMMoV?
data %>% 
  ungroup() %>%
  filter(copies_gram_stool_ave > 1) %>%
  mutate(log_copies = log10(copies_gram_stool_ave)) %>%
  dplyr::select(Sample, log_copies, Target) %>% 
  filter(Target != "crassphage") %>% 
  pivot_wider(names_from = Target, values_from = log_copies, id_cols = Sample) %>%
  drop_na() %>%
  ggscatterstats(
    x = pmmov,
    y = HF183,
    grouping.var = group, 
    type = "np", 
    xlab = "PMMoV", 
    ylab = "HF183", 
    marginal = FALSE
  )


#Are high shedders of crAssphage high shedders of PMMoV? 
data %>% 
  ungroup() %>%
  filter(copies_gram_stool_ave > 1) %>%
  mutate(log_copies = log10(copies_gram_stool_ave)) %>%
  dplyr::select(Sample, log_copies, Target) %>% 
  filter(Target != "HF183") %>% 
  pivot_wider(names_from = Target, values_from = log_copies, id_cols = Sample) %>%
  drop_na() %>%
  ggscatterstats(
    x = pmmov,
    y = crassphage,
    grouping.var = group, 
    type = "np", 
    xlab = "PMMoV", 
    ylab = "crAssphage", 
    marginal = FALSE
  )



distributions %>% 
  ggplot(aes(x = crassphage)) + 
  geom_histogram(binwidth = 0.5) 

distributions %>% 
  ggplot(aes(x = HF183)) + 
  geom_histogram(binwidth = 0.5) 



###########################################################################
##########################Repeat Samples###################################
###########################################################################

#Load Data 
repeat_samples = read_csv("./data/raw_data/repeat_samples.csv")

#Update Sample ID to match full dataset

repeat_samples$Sample = gsub("-", "", repeat_samples$Sample) 
repeat_samples$Original_Sample = gsub("-", "", repeat_samples$Original_Sample) 

repeat_samples %<>%
  mutate(Sample = paste(Sample, 4, sep = "")) %>% 
  mutate(Original_Sample = paste(Original_Sample, 4, sep = "") )

repeat_samples = left_join(repeat_samples, 
                           data %>% 
                             dplyr::select(Sample, Target, reps_pos, detection, copies_gram_stool_ave, recorded_date)
                           , by = "Sample") 


test = repeat_samples %>%
  dplyr::select(Original_Sample, Target, Sample_Rep, reps_pos, detection, copies_gram_stool_ave, recorded_date) %>%
  drop_na() %>%
  pivot_wider(names_from = Sample_Rep, values_from =  c(reps_pos, detection, copies_gram_stool_ave, recorded_date)) %>% 
  drop_na() %>% 
  mutate(log.copies.stool_1 = log10(as.numeric(copies_gram_stool_ave_1))) %>% 
  mutate(log.copies.stool_2 = log10(as.numeric(copies_gram_stool_ave_2)))

########

repeat_samples %>% 
  dplyr::select(Original_Sample, Target, Sample_Rep, reps_pos, detection, copies_gram_stool_ave) %>%
  drop_na() %>%
  pivot_wider(names_from = Sample_Rep, values_from =  c(reps_pos, detection, copies_gram_stool_ave)) %>% 
  drop_na() %>%
  mutate(Rep_1 = log10(copies_gram_stool_ave_1), 
         Rep_2 = log10(copies_gram_stool_ave_2)) %>%
  dplyr::select(Original_Sample, Target, Rep_1, Rep_2) %>%
  melt(id.vars = c("Original_Sample", "Target")) %>%
  separate(variable, into = c("drop", "Rep"), sep = "_") %>% 
  dplyr::select(-drop) %>%
  mutate(Target == case_when(Target == "crassphage" ~ "crAssphage", 
                             Target == "pmmov" ~ "PMMoV", 
                             TRUE ~ "HF183")) %>%
  grouped_ggwithinstats(
    x = Rep, 
    y = value, 
    pairwise.display = "all",
    grouping.var = "Target", 
    type = "np",
    xlab = "Collection", 
    ylab = "Log10(Copies per Gram)"
  )


test %>% 
  grouped_ggscatterstats(
    x = log.copies.stool_1, 
    y = log.copies.stool_2, 
    grouping.var = "Target", 
    type = "np",
    xlab = "Collection 1", 
    ylab = "Collection 2", 
    marginal = FALSE
  )



crassphage_repeat = test %>% filter(Target == "crassphage")
HF183_repeat = test %>% filter(Target == "HF183")
pmmov_repeat = test %>% filter(Target == "pmmov")


#Are the concentrations different?
wilcox.test(crassphage_repeat$log.copies.stool_1, crassphage_repeat$log.copies.stool_2, paired = TRUE)
wilcox.test(HF183_repeat$log.copies.stool_1, HF183_repeat$log.copies.stool_2, paired = TRUE)
wilcox.test(pmmov_repeat$log.copies.stool_1, pmmov_repeat$log.copies.stool_2, paired = TRUE)

#Is the detection frequency different? 
#fisher.test(crassphage_repeat$detection_1, crassphage_repeat$detection_2, simulate.p.value = TRUE)
#fisher.test(HF183_repeat$detection_1, HF183_repeat$detection_2, simulate.p.value = TRUE)
#fisher.test(pmmov_repeat$detection_1, pmmov_repeat$detection_2, simulate.p.value = TRUE)
#fisher.test(repeat_samples$Sample_Rep, repeat_samples$detection, simulate.p.value = TRUE)

write_csv(test, "./data/processed_data/repeat_samples.csv")



################################################################################
#######################Adult & Child Pairs######################################
################################################################################


#Load Data 
child_parent_pairs = read_csv("./data/raw_data/child_parent_pairs.csv")

#Update Sample ID to match full dataset

child_parent_pairs$Sample = gsub("-", "", child_parent_pairs$Sample) 
child_parent_pairs$Pair = gsub("-", "", child_parent_pairs$Pair) 

child_parent_pairs %<>%
  mutate(Sample = paste(Sample, 4, sep = "")) %>% 
  mutate(Pair = paste(Pair, 4, sep = "") )

child_parent_pairs = left_join(child_parent_pairs %>% 
                                 dplyr::select(Sample, Pair), 
                               data %>% 
                                 dplyr::select(Sample, Target, reps_pos, detection, copies_gram_stool_ave), 
                               by = "Sample")


child_parent_pairs = left_join(child_parent_pairs, 
                               data %>% 
                                 dplyr::select(Sample, Target, reps_pos, detection, copies_gram_stool_ave), 
                               by = c("Pair" = "Sample", "Target"))

child_parent_pairs$reps.pos_c = child_parent_pairs$reps_pos.x
child_parent_pairs$detection_c = child_parent_pairs$detection.x
child_parent_pairs$copies.stool_c = child_parent_pairs$copies_gram_stool_ave.x


child_parent_pairs$reps.pos_a = child_parent_pairs$reps_pos.y
child_parent_pairs$detection_a = child_parent_pairs$detection.y
child_parent_pairs$copies.stool_a = child_parent_pairs$copies_gram_stool_ave.y



child_parent_pairs %<>% 
  dplyr::select(Sample, Pair, Target, reps.pos_c, reps.pos_a, detection_c, detection_a, copies.stool_c, copies.stool_a) %>% 
  drop_na() %>% 
  mutate(log.copies.stool_c = log10(copies.stool_c)) %>% 
  mutate(log.copies.stool_a = log10(copies.stool_a)) 


child_parent_pairs %>% 
  grouped_ggscatterstats(
    x = log.copies.stool_c, 
    y = log.copies.stool_a, 
    grouping.var = "Target", 
    type = "np",
    xlab = "Child", 
    ylab = "Adult", 
    marginal = FALSE
  )


crassphage_pairs = child_parent_pairs %>% filter(Target == "crassphage")
HF183_pairs = child_parent_pairs %>% filter(Target == "HF183")
pmmov_pairs = child_parent_pairs %>% filter(Target == "pmmov")


#Are the concentrations different between adults and children?
wilcox.test(crassphage_pairs$log.copies.stool_c, crassphage_pairs$log.copies.stool_a, paired = TRUE)
wilcox.test(HF183_pairs$log.copies.stool_c, HF183_pairs$log.copies.stool_a, paired = TRUE)
wilcox.test(pmmov_pairs$log.copies.stool_c, pmmov_pairs$log.copies.stool_a, paired = TRUE)

wilcox.test(child_parent_pairs$log.copies.stool_c, child_parent_pairs$log.copies.stool_a, paired = TRUE)

#Is the detection frequency different between adults and their paired children? 
fisher.test(child_parent_pairs$detection_c, child_parent_pairs$detection_a, simulate.p.value = TRUE)

write_csv(child_parent_pairs, "./data/processed_data/child_adult_pairs.csv")

##########################################################################################

#Is titer correlated with BCoV recovery?

data %<>% 
  mutate(log10_copies = log10(copies_gram_stool_ave)) %>%
  mutate(shed.level = case_when(Target == "crassphage" & log10_copies > 6.5 ~ "High",
                                Target == "crassphage" & log10_copies < 0 ~ NA_character_,
                                Target == "HF183" & log10_copies > 7.5 ~ "High", 
                                Target == "HF183" & log10_copies < 0 ~ NA_character_, 
                                Target == "pmmov" ~ NA_character_, 
                                TRUE ~ "Low")) 

library(ggstatsplot)


data %>%
  filter(bcov_recovery < 1000) %>%
  filter(bcov_recovery > 0) %>%
  ggplot(aes(x = bcov_recovery)) + 
  geom_histogram() + 
  xlab("BCoV Recovery (%)") + 
  ylab("Count") + 
  theme_bw() + 
  ggtitle("A")

data %>% 
  mutate(Target = case_when(Target == "crassphage" ~ "crAssphage", 
                            Target == "pmmov" ~ "PMMoV", 
                            TRUE ~ Target)) %>%
  filter(bcov_recovery < 1000) %>%
  filter(copies_gram_stool_ave > 1) %>%
  grouped_ggscatterstats(
    x = bcov_recovery, 
    y = log10_copies, 
    grouping.var = Target, 
    xlab = "BCoV Recovery (%)", 
    ylab = "Log10 (Copies per Gram)", 
    marginal = FALSE, 
  )

data %>% 
  mutate(Target = case_when(Target == "crassphage" ~ "crAssphage", 
                            TRUE ~ Target)) %>%
  filter(Target != "pmmov") %>%
  filter(bcov_recovery < 1000) %>%
  grouped_ggbetweenstats(
    x = shed.level, 
    y = bcov_recovery, 
    grouping.var = Target, 
    ylab = "BCoV Recovery (%)", 
    xlab = "Titer")

recovery_titer = ggarrange(recovery_titer_A, 
                           recovery_titer_C, 
                           recovery_titer_B,
                           labels = c("A", "B", "C"), 
                           heights = c(1,1,1), 
                           widths = c(1,1,2))

plot(recovery_titer)

