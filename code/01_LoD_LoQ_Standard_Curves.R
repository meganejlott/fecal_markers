#Load Libraries
library(tidyverse)
library(magrittr)
library(ggstatsplot)
library(purrr)
library(reshape2)
library(viridis)
library(ggpubr)
library(ggthemes)
library(ggpmisc)


############################################################################
#################LoD, LoQ & Standard Curves#################################
############################################################################

#Load data
std_curves = read_csv("./data/raw_data/standard_curves.csv")

crassphage = std_curves %>% filter(Target == "crassphage")
HF183 = std_curves %>% filter(Target == "HF183")
pmmov  = std_curves %>% filter(Target == "pmmov")

#tiff('./figures/crAssphage_LoQ.tiff', units="in", width = 6, height = 4, res=600, compression = 'lzw', pointsize = 12)
qqnorm(crassphage$Cq, pch = 1, frame = FALSE, ylab = "Cq Values", main= "Normal QQ Plot of \n crAssphage Standards")
qqline(crassphage$Cq, col = "steelblue", lwd = 2)
dev.off()

#tiff('./figures/HF183_LoQ.tiff', units="in", width = 6, height = 4, res=600, compression = 'lzw', pointsize = 12)
qqnorm(HF183$Cq, pch = 1, frame = FALSE,  ylab = "Cq Values", main= "Normal QQ Plot of \n HF183 Standards")
qqline(HF183$Cq, col = "steelblue", lwd = 2)
dev.off()

#tiff('./figures/PMMoV_LoQ.tiff', units="in", width = 6, height = 4, res=600, compression = 'lzw', pointsize = 12)
qqnorm(pmmov$Cq, pch = 1, frame = FALSE,  ylab = "Cq Values", main= "Normal QQ Plot of \n PMMoV Standards")
qqline(pmmov$Cq, col = "steelblue", lwd = 2)
#dev.off()


###LoD###
#For a given concentration, how many replicates are positive?
#What proportion is positive? 

crassphage_lod = crassphage %>% 
  mutate(Cq = as.numeric(Cq)) %>%
  mutate(detection = case_when(Cq != "NaN" ~ 1, 
                               TRUE ~ 0)) %>%
  group_by(dilution, quantity, log_quantity) %>% 
  summarize(n = n(), 
            n.pos = sum(detection)) %>% 
  mutate(p.pos = n.pos/n) %>% 
  mutate(log.2 = log2(quantity)) %>% 
  filter(dilution != "1") 


pmmov_lod = pmmov %>% 
  mutate(Cq = as.numeric(Cq)) %>%
  mutate(detection = case_when(Cq != "NaN" ~ 1, 
                               TRUE ~ 0)) %>%
  group_by(dilution, quantity) %>% 
  summarize(n = n(), 
            n.pos = sum(detection)) %>% 
  mutate(p.pos = n.pos/n)


HF183_lod = HF183 %>% 
  mutate(Cq = as.numeric(Cq)) %>%
  mutate(detection = case_when(Cq != "NaN" ~ 1, 
                               TRUE ~ 0)) %>%
  group_by(dilution, quantity) %>% 
  summarize(n = n(), 
            n.pos = sum(detection)) %>% 
  mutate(p.pos = n.pos/n*100)


lod = std_curves %>% 
  mutate(Cq = as.numeric(Cq)) %>%
  mutate(detection = case_when(Cq != "NaN" ~ 1, 
                               TRUE ~ 0)) %>%
  mutate(Target = case_when(Target == "crassphage" ~ "crAssphage", 
                            Target == "pmmov" ~ "PMMoV", 
                            TRUE ~ Target)) %>%
  filter(dilution != "1") %>% 
  group_by(Target, dilution, quantity) %>% 
  summarize(n = n(), 
            n.pos = sum(detection)) %>% 
  mutate(p.pos = n.pos/n*100) %>% 
  ggplot(aes(x = log10(quantity), y = p.pos)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~Target, ncol = 1) + 
  xlab("Log10 (Copies)") + 
  ylab("Positive Replicates (%)") + 
  theme_classic()

#tiff('./figures/LoD.tiff', units="in", width = 4, height = 4, res=600, compression = 'lzw', pointsize = 12)
plot(lod)
#dev.off()


############################################################
##################STANDARD CURVES###########################
############################################################

#Average technical replicates
std_curves %<>% 
  dplyr::group_by(Target, replicate, dilution) %>% 
  dplyr::summarize(mean_Cq = mean(Cq, na.rm = TRUE),
                   sd_Cq = sd(Cq, na.rm = TRUE),
                   quantity = mean(quantity))  

#From the three biological replicates, determine the CoV for each dilution
cov = std_curves %>% 
  group_by(Target, dilution) %>% 
  summarize(mean = mean(mean_Cq), 
            sd = sd(mean_Cq)) %>% 
  mutate(cov = (sd/mean)*100)


crassphage = std_curves %>% 
  filter(Target == "crassphage") %>% 
  filter(dilution > 3 & dilution < 12)

pmmov = std_curves %>% 
  filter(Target == "pmmov") %>% 
  filter(dilution > 2 & dilution < 11)

HF183 = std_curves %>% 
  filter(Target == "HF183") %>% 
  filter(dilution > 2 & dilution < 11)

########

my.formula <- y ~ x

#CrAssphage
crassphage.std = ggplot(data = crassphage, aes(x = log10(quantity*10), y = mean_Cq)) +
  geom_smooth(method = "lm", se=FALSE, formula = my.formula) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, coef.digits = 5, f.digits = 5, p.digits = 10) +
  geom_point() +
  xlab("Log10 (Copies)") + 
  ylab("Cq") + 
  theme_bw() + 
  ggtitle("crAssphage") + 
  xlim(0,10) + 
  ylim(0,40) 

#PMMoV
pmmov.std = ggplot(data = pmmov, aes(x = log10(quantity*10), y = mean_Cq)) +
  geom_smooth(method = "lm", se=FALSE, formula = my.formula) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, coef.digits = 5, f.digits = 5, p.digits = 10) +
  geom_point() +
  xlab("Log10 (Copies)") + 
  ylab("Cq") + 
  theme_bw() + 
  ggtitle("PMMoV") + 
  xlim(0,10) + 
  ylim(0,40) 

#HF183
hf183.std = ggplot(data = HF183, aes(x = log10(quantity*10), y = mean_Cq)) +
  geom_smooth(method = "lm", se=FALSE, formula = my.formula) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, coef.digits = 5, f.digits = 5, p.digits = 10) +
  geom_point() +
  xlim(0,10) + 
  ylim(0,40) +
  xlab("Log10 (Copies)") + 
  ylab("Cq") + 
  theme_bw() + 
  ggtitle("HF183")


standard_curves_fig = ggarrange(crassphage.std, 
                                hf183.std, 
                                pmmov.std, 
                                ncol = 1)

#tiff('./figures/standard_curves.tiff', units="in", width = 4, height = 6, res=600, compression = 'lzw', pointsize = 12)
plot(standard_curves_fig)
#dev.off()

#crAssphage y = 37.678 - 3.3104x
#PMMoV: y = 41.205 - 3.1519x
#HF183: y = 35.949 - 3.2548x

###############################################################################
#############################LOQ##############################################

#load data
std_curves = read_csv("./data/raw_data/standard_curves.csv")

crassphage = std_curves %>% filter(Target == "crassphage")
HF183 = std_curves %>% filter(Target == "HF183")
pmmov  = std_curves %>% filter(Target == "pmmov")


#Determine the LOD and LOQ by plotting the Normal QQ-Plot
qqnorm.ct.crassphage <- qqnorm(std_curves$Cq[which(std_curves$Target=="crassphage")], plot.it = F) %>% as.data.frame()
qqnorm.ct.HF183 <- qqnorm(std_curves$Cq[which(std_curves$Target=="HF183")], plot.it = F) %>% as.data.frame()
qqnorm.ct.pmmov <- qqnorm(std_curves$Cq[which(std_curves$Target=="pmmov")], plot.it = F) %>% as.data.frame()


qqnorm.Explorer.ct <- function(qqnorm.ct){
  qqnorm.ct <- qqnorm.ct[which(complete.cases(qqnorm.ct)),]
  qqnorm.ct <- qqnorm.ct[order(qqnorm.ct$x),]
  qqnorm.ct <- cbind(qqnorm.ct, rbind(NA, qqnorm.ct[-nrow(qqnorm.ct),])) %>% setNames(., nm = c("x", "y", "x-1", "y-1"))
  qqnorm.ct %<>% mutate(rise = y-`y-1`, run = x-`x-1`) %>% mutate(slope = rise / run)
  
  qqnorm.ct$lod <- NA
  qqnorm.ct$loq <- NA
  
  prev.slope <- 1
  lod.found <- 0
  for(i in nrow(qqnorm.ct):2){
    if(lod.found==0){
      if(qqnorm.ct$slope[i]<1 & prev.slope <1){
        qqnorm.ct$lod[i] <- 1
        lod.found <- 1
      }else{
        prev.slope <- qqnorm.ct$slope[i]
      }
    }
    if(lod.found==1){
      if(qqnorm.ct$slope[i]>1){
        qqnorm.ct$loq[i] <- 1
        break
      }else{
        prev.slope <- qqnorm.ct$slope[i]
      }
    }
  }
  
  
  lod.ct <- qqnorm.ct$y[which(qqnorm.ct$lod==1)]
  loq.ct <- qqnorm.ct$y[which(qqnorm.ct$loq==1)]
  
  return(list(qqnorm.dataset = qqnorm.ct, lod = lod.ct, loq = loq.ct))
}



qqnorm.ct.crassphage <- qqnorm.Explorer.ct(qqnorm.ct.crassphage)
qqnorm.ct.HF183 <- qqnorm.Explorer.ct(qqnorm.ct.HF183)
qqnorm.ct.pmmov <- qqnorm.Explorer.ct(qqnorm.ct.pmmov)




#LoQ crAssphage = Cq 39.18931
#LoD crAssphage = 0.152 copies
crassphage_loq = 10^((39.18931-37.678)/-3.3104) 

#LoQ HF183 = 35.26606
HF183_loq = 10^((35.26606-35.949)/-3.2548)

#LoQ PMMoV = 36.33765
pmmov_loq =  10^((36.33765-41.205)/-3.1519) 

