# Description: HSD
# by Kevin Falk
# May 2018

###############################################################
library(dplyr)
library(lme4)
library(agricolae)
library(reshape2)

#Import new df of BLUP DATA
AllData_Heuristic <- read.csv("C:/Users/gsw685/Google Drive/PhD/PhD Projects/Blue Steel/Paper#1/RSA ARIA Methods Paper - Zaki Vahid Kevin/Submission/3rd Revision/Three_Genos_Heuristic.csv")
AllData_CNN <- read.csv("C:/Users/gsw685/Google Drive/PhD/PhD Projects/Blue Steel/Paper#1/RSA ARIA Methods Paper - Zaki Vahid Kevin/Submission/3rd Revision/Traits/soybean.csv")
AllData_CNN <- read.csv("C:/Users/gsw685/Google Drive/PhD/PhD Projects/Blue Steel/Paper#1/RSA ARIA Methods Paper - Zaki Vahid Kevin/Submission/3rd Revision/Traits_updated/traitsupdated-11-23-2019.csv")

joined <- left_join(AllData_Heuristic,AllData_CNN,by="Unique")
joined %>% select("Unique","TRL.x","TRL.y") %>% corr

#Three_Genos_Heuristic <- AllData_Heuristic %>%
#  filter(((Entry == "127")|(Entry == "298")|(Entry  == "199")))
#Three_Genos_CNN <- AllData_CNN %>%
#  filter(((Entry == "127")|(Entry == "298")|(Entry  == "199")))

write.csv(Three_Genos_Heuristic,"C:/Users/gsw685/Google Drive/PhD/PhD Projects/Blue Steel/Paper#1/RSA ARIA Methods Paper - Zaki Vahid Kevin/Submission/3rd Revision/Three_Genos_Heuristic.csv")


metadata <- read.csv("C:/Users/gsw685/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/Cluster_metadata.csv")
#remove BAD genotypes PI594438 (273) PI495832 (202) PI467333A (382) PI473899 (390) PI506528 (204) PI445845 (367)

export <- AllData %>% select(Entry,Day,PER,DIA,VOL,PRA,TRLUpper,TRLLower,CVA,DEP,WID,WDR,NWA,RHZO,LRB)
write.csv(export3,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Data/Export3Genos_for_LDA.csv")

str(AllData)
#split data into df Days
Day6_data <- subset(Three_Genos, Three_Genos$Day == "6")
Day9_data <- subset(Three_Genos, Three_Genos$Day == "9")
Day12_data <- subset(Three_Genos, Three_Genos$Day == "12")
Day6_data <- subset(AllData, AllData$Day == "6")
Day9_data <- subset(AllData, AllData$Day == "9")
Day12_data <- subset(AllData, AllData$Day == "12")

#take only the columns with numerical data
colnames(AllData)
colnum=c(7:41) 
Day6DataOutput <- matrix()
i = 1
for (i in 1:34){  #this second loop runs through each TRAIT, one at a time
  
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day6_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day6_data1 <- Day6_data  #Create a temporary dataframe so that we can modify and iterate over the column name
  colnames(Day6_data1)[x] <-"y"  #renames the trait variable as "y" for the model analysis below
  #model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day6_data1) #BLUP random effects
  model<- aov(Day6_data1$y~Entry, data=Day6_data1) #ANOVA test in which the 
  HSD_data <- HSD.test(model, 'Entry',console=TRUE)  #Multiple comparisons of treatments by means of HSD and a grouping of treatments.
  colnames(HSD_data$groups) <- c(trait,"group")
  rownamez <- as.data.frame(rownames(HSD_data$groups))
  colnames(rownamez) <- "Entry"
  grouped <- cbind(rownamez, HSD_data$groups) %>% arrange(Entry)
  #add columns to existing dataframe   
  Day6DataOutput <- cbind(Day6DataOutput,grouped)
}

Day6DataOutput$Day <- c(6)

melt(Day6DataOutput, id.vars = c("Day","Entry","group"))


##########################################################################################
#take only the columns with numerical data
colnames(AllData)
colnum=c(7:41) 
Day9DataOutput <- matrix()
i = 1
for (i in 1:34){  #this second loop runs through each TRAIT, one at a time
  
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day9_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day9_data1 <- Day9_data  #Create a temporary dataframe so that we can modify and iterate over the column name
  colnames(Day9_data1)[x] <-"y"  #renames the trait variable as "y" for the model analysis below
  #model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day9_data1) #BLUP random effects
  model<- aov(Day9_data1$y~Entry, data=Day9_data1) #ANOVA test in which the 
  HSD_data <- HSD.test(model, 'Entry',console=TRUE)  #Multiple comparisons of treatments by means of HSD and a grouping of treatments.
  colnames(HSD_data$groups) <- c(trait,"group")
  rownamez <- as.data.frame(rownames(HSD_data$groups))
  colnames(rownamez) <- "Entry"
  grouped <- cbind(rownamez, HSD_data$groups) %>% arrange(Entry)
  #add columns to existing dataframe   
  Day9DataOutput <- cbind(Day9DataOutput,grouped)
}

Day9DataOutput$Day <- c(9)

melt(Day9DataOutput, id.vars = c("Day","Entry","group"))
##########################################################################################
#take only the columns with numerical data
colnames(AllData)
colnum=c(7:41) 
Day12DataOutput <- matrix()
i = 1
for (i in 1:34){  #this second loop runs through each TRAIT, one at a time
  
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day12_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day12_data1 <- Day12_data  #Create a temporary dataframe so that we can modify and iterate over the column name
  colnames(Day12_data1)[x] <-"y"  #renames the trait variable as "y" for the model analysis below
  #model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day12_data1) #BLUP random effects
  model<- aov(Day12_data1$y~Entry, data=Day12_data1) #ANOVA test in which the 
  HSD_data <- HSD.test(model, 'Entry',console=TRUE)  #Multiple comparisons of treatments by means of HSD and a grouping of treatments.
  colnames(HSD_data$groups) <- c(trait,"group")
  rownamez <- as.data.frame(rownames(HSD_data$groups))
  colnames(rownamez) <- "Entry"
  grouped <- cbind(rownamez, HSD_data$groups) %>% arrange(Entry)
  #add columns to existing dataframe   
  Day12DataOutput <- cbind(Day12DataOutput,grouped)
}

Day12DataOutput$Day <- c(12)

melt(Day12DataOutput, id.vars = c("Day","Entry","group"))
#bind the data frames
data_output <- rbind(Day6DataOutput[,2:ncol(Day6DataOutput)],Day9DataOutput[,2:ncol(Day9DataOutput)],Day12DataOutput[,2:ncol(Day12DataOutput)])

write.csv(data_output, "C:/Users/gsw685/Google Drive/PhD/PhD Projects/Blue Steel/Paper#1/RSA ARIA Methods Paper - Zaki Vahid Kevin/Submission/3rd Revision/3GenotypesGrowth.csv")
#output that beast
#################
#write.csv(data_output,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/kmeans.8_HSD.csv")

Day6 <- data_output %>% filter(Day == "6")
Day9 <- data_output %>% filter(Day == "9")
Day12 <- data_output %>% filter(Day == "12")

write.csv(Day12,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/snp35k_HSD_Day12_groupings.csv")
Day6 %>% filter()
groupings6 <- Day6[c(1,2,5),c(FALSE,FALSE,TRUE)]
groupings9 <- Day9[c(1,2,5),c(FALSE,FALSE,TRUE)]
groupings12 <- Day12[c(1,2,5),c(FALSE,FALSE,TRUE)]

snp35k_groupings <- as.data.frame(rbind(table(unlist(groupings6)),table(unlist(groupings9)),table(unlist(groupings12))))
write.csv(snp35k_groupings,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/snp35k_HSD_groupings.csv")
kmeans.8_groupings <- as.data.frame(rbind(table(unlist(groupings6)),table(unlist(groupings9)),table(unlist(groupings12))))
write.csv(kmeans.8_groupings,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/kmeans.8_HSD_groupings.csv")



