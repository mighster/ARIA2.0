library(tidyverse)
library(ggpubr)
library(corrplot)
library(gplots)
library(RColorBrewer)
library(LDheatmap)
require(made4)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("made4", version = "3.8")

###############################################################################
############################# Explore Correlations ############################
###############################################################################

df1 <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Data/KGF_AdjustedBLUPsAllDays_thinned_Oct24_tall_TRL_GR.csv", header = T)
metadata <- read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/Cluster_metadata.csv")
colnames(df1)

MG2data <- df1 %>% filter(MG == "2")
data <- cbind(Day9_data,Day9_metadata[(ncol(Day9_metadata))])

df1 <- df1[,c(3,31:ncol(df1))]

Day6_data <- subset(MG2data, MG2data$Day == "6")
Day9_data <- subset(MG2data, MG2data$Day == "9")
Day12_data <- subset(MG2data, MG2data$Day == "12")
Day6_metadata <- subset(metadata, metadata$Day == "6")
Day9_metadata <- subset(metadata, metadata$Day == "9")
Day12_metadata <- subset(metadata, metadata$Day == "12")

colnames(Day6_data)
Day6_corr <- cor(x=Day6_data[,(32:72)], y = NULL, use = "everything", method = c("pearson"))
Day9_corr <- cor(x=Day9_data[,(32:72)], y = NULL, use = "everything", method = c("pearson"))
Day12_corr <- cor(x=Day12_data[,(32:72)], y = NULL, use = "everything", method = c("pearson"))


#Day6_corr <- cor(x=Day6_data[,(2:50)], y = NULL, use = "everything", method = c("spearman"))
#Day9_corr <- cor(x=Day9_data[,-1], y = NULL, use = "everything", method = c("spearman"))
#Day12_corr <- cor(x=Day12_data[,-1], y = NULL, use = "everything", method = c("spearman"))

# make figure colored by correlations

tiff("C:/Users/falk/Google Drive/PhD/Papers/RSA ARIA Methods Paper - Zaki Vahid Kevin/Figures/Correlations/Day6_correlations_hclust_complete_Feb21.tiff", width = 15, height = 14, units = 'in', res = 150)
corrplot(Day6_corr,type="lower", order = 'hclust',hclust.method = 'complete',mar = c(1, 1, 1, 1))
dev.off()
tiff("C:/Users/falk/Google Drive/PhD/Papers/RSA ARIA Methods Paper - Zaki Vahid Kevin/Figures/Correlations/Day9_correlations_hclust_complete_Feb21.tiff", width = 15, height = 14, units = 'in', res = 150)
corrplot(Day9_corr,type="lower", order = 'hclust',hclust.method = 'complete',mar = c(1, 1, 1, 1))
dev.off()
tiff("C:/Users/falk/Google Drive/PhD/Papers/RSA ARIA Methods Paper - Zaki Vahid Kevin/Figures/Correlations/Day12_correlations_hclust_complete_Feb21.tiff", width = 15, height = 14, units = 'in', res = 150)
corrplot(Day12_corr,type="lower", order = 'hclust',hclust.method = 'complete',mar = c(1, 1, 1, 1))
dev.off()

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Correlations/Day6_correlations_hclust_wardD_Jan29.tiff",compression = "lzw", width = 15, height = 14, units = 'in', res = 500)
corrplot(Day6_corr,type="lower", order = 'hclust',hclust.method = 'ward.D')
dev.off()
tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Correlations/Day9_correlations_hclust_wardD_Jan29.tiff",compression = "lzw", width = 15, height = 14, units = 'in', res = 500)
corrplot(Day9_corr,type="lower", order = 'hclust',hclust.method = 'ward.D')
dev.off()
tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Correlations/Day12_correlations_hclust_wardD_Jan29.tiff",compression = "lzw", width = 15, height = 14, units = 'in', res = 500)
corrplot(Day12_corr,type="lower", order = 'hclust',hclust.method = 'ward.D')
dev.off()

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Correlations/Day6_correlations_hclust_square_Jan29.tiff",compression = "lzw", width = 15, height = 14, units = 'in', res = 300)
corrplot.mixed(Day6_corr, order = 'hclust',lower.col = "black",addrect = 2,tl.srt=45)
dev.off()
tiff("Day9_correlations_hclust_triangle_Jan29.tiff",compression = "lzw", width = 15, height = 14, units = 'in', res = 300)
corrplot(Day9_corr,type="lower", order = 'hclust',tl.srt=45)
dev.off()
tiff("Day12_correlations_hclust_triangle_Jan29.tiff",compression = "lzw", width = 15, height = 14, units = 'in', res = 300)
corrplot(Day12_corr,type="lower", order = 'hclust',tl.srt=45)
dev.off()


corrplot_Day9 <- corrplot(Day9_corr, order = 'hclust')
heatmap(x = corrplot_Day9, symm = TRUE)
heatplot(corrplot_Day9)
corrplot.mixed(Day9_corr)

apply(Day6_corr,1,abs) %>% colSums() %>% sum()
apply(Day9_corr,1,abs) %>% colSums() %>% sum()
apply(Day12_corr,1,abs) %>% colSums() %>% sum()
