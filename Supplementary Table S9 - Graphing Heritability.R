olibrary(reshape2)
library(ggplot2)


READ THIS before calculating heritability
Piepho, H.P., Möhring, J., 2007. Computing heritability and selection response from
unbalanced plant breeding trials. Genetics 177, 1881-1888. http://dx.doi.org/10.
1534/genetics.107.074229.

#Import new df
premelt<-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/Heritabilitydata_294.csv")
premelt<-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Raw Data/Heritabilitydata_old.csv")
premelt<-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/Rep_Effect/Heritabilitydata_MG2_test.csv")
premelt<-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Heritability/Heritabilitydata_292.csv")
colnames(premelt)

postmelt <- melt(premelt, id.vars = c("Trait", "Day"))

#old data from NAPB Conference
#postmelt<-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Raw Data/Heritability_postmelt.csv")

postmelt$variable <- gsub("Rep...", "", postmelt$variable) #need to remove the "REP" term in each row

colnames(postmelt) <- c("Trait", "Day", "Rep", "Heritability")
postmelt$Trait <- as.factor(postmelt$Trait)
postmelt$Rep <- as.numeric(postmelt$Rep)
postmelt$Day <- as.factor(postmelt$Day)
str(postmelt)

#postmelt$Trait <- factor(postmelt$Trait, levels = c("TRL","PRL","LRB","WID","TRArea","LED"))
heritability_data <- postmelt %>% filter(Trait == "TRL"|Trait == "PRL"|Trait == "WID"|Trait == "CVA"|Trait == "LRB"|Trait == "VOL"|Trait == "LRA"|Trait == "SOL2"|Trait == "LED"|Trait == "RHZO"|Trait == "TRL_GR"|Trait == "TRLUpper"|Trait == "Root_weight")

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/Rep_Effect/Heritability_292Genotypes_Feb24.png", units = "in" ,height = 8, width = 8, res=200)
ggplot(data = heritability_data, aes(x=Rep, y=Heritability, z = Day, group = Day, colour = Day)) +
     geom_line(aes(), size = 1) +
     facet_wrap(~Trait, scales = "free_x", ncol = 3, labeller = label_parsed) +
     scale_x_continuous(breaks = pretty(postmelt$Rep, n = 7)) +
     xlab("Replications") +
     ylab("Heritability (Broad-Sense)") +
     theme_bw() +
     scale_color_manual(values=c("#6090ff", "#ff4c4c", "#55d655")) +
     theme(panel.background = element_blank(), strip.background = element_blank())
dev.off()

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/Rep_Effect/Heritability_MG2_6.tiff", units = "in" ,height = 5, width = 10, res=200)
ggplot(data = postmelt, aes(x=Rep, y=Heritability, z = Day, group = Day, colour = Day)) +
  geom_line(aes(), size = 1) +
  facet_wrap(~Trait, scales = "free_x", ncol = 3, labeller = label_parsed) +
  scale_x_continuous(breaks = pretty(postmelt$Rep, n = 7)) +
  xlab("Replications") +
  ylab("Heritability (Broad-Sense)") +
  theme_bw() +
  scale_color_manual(values=c("#FF6600", "#009999", "purple")) +
  theme(panel.background = element_blank(), strip.background = element_blank())
dev.off()

