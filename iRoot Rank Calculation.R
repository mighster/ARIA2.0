library(tidyverse)

AllData <- read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Data/KGF_AdjustedBLUPsAllDays_thinned_Oct24_tall_TRL_GR.csv")

#split data into df Days
Day6_data <- subset(AllData, AllData$Day == "6")
Day9_data <- subset(AllData, AllData$Day == "9")
Day12_data <- subset(AllData, AllData$Day == "12")

colnames(Day6_data)
?rank

############################################################################################

#take only the columns with numerical data
colnum=c(31:ncol(Day9_data)) 
Day9_ranks <- data.frame(matrix(vector(),292,1, dimnames=list(c(), c("Entry")))) #create new dataframe that has 292 rows
Day9_ranks$Entry <- (Day9_data$Entry) #add column of Entry numbers from dataset
Day9_ranks$Name <- (Day9_data$Name) #add column of Name numbers from dataset
Day9_ranks$Day <- (Day9_data$Day) #add column of Days from dataset
colnames(Day9_data)

for (i in 1:50) {  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day9_data)[x] #sets the current column header as the trait name
  Rank <- as.data.frame(rank(desc((Day9_data)[x])))
  rank <- names(Rank)[1] <- trait  #set current trait as column header
  Day9_ranks <- cbind(Day9_ranks,Rank) #add columns to existing dataframe   
}

############################################################################################
##################################### Scavenger ############################################
############################################################################################

IdeotypeVar <-  Day9_ranks %>%
                            select(TRLUpper,WID,TRL_GR) #select RSA traits to use for particular ideotype
IdeotypeSums <- IdeotypeVar %>%
                            mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day9_data %>%
                  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

Scavenger <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together
  
Scavenger <- arrange(Scavenger, SUM) %>% mutate(ScavengerRanked = rank(SUM, ties.method = "max")) #sort new df based on master ranking column with no.1 being best
ScavengerTop10 <- as.data.frame(Scavenger[1:10,1, drop = FALSE])
#colnames(ScavengerTop10) <-  "Scavenger"
############################################################################################
################################## Steep, Deep, Cheap#######################################
############################################################################################

Day9_ranks <- mutate(Day9_ranks, LRADescending = 293 - LRAMediangle)  #invert the ranking for LRAMediangle as a lower angle, not higher angle is desirable
Day9_ranks <- mutate(Day9_ranks, SOL2 = 293 - SOL2)  #invert the ranking for SOL2 as a lower density, not higher density is desirable

IdeotypeVar <-  Day9_ranks %>%
                            select(LRADescending,PRL,SOL2,TRL_GR) #select RSA traits to use for particular ideotype
IdeotypeSums <- IdeotypeVar %>%
                              mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day9_data %>%
                   select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

SteepDeepCheap <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together
 
SteepDeepCheap <- arrange(SteepDeepCheap, SUM) %>% mutate(DTRanked = rank(SUM, ties.method = "max")) #sort new df based on master ranking column with no.1 being best
SteepDeepCheap[1:12,]
SteepDeepCheapTop10 <- as.data.frame(SteepDeepCheap[1:10,1, drop = FALSE])
#colnames(SteepDeepCheapTop10) <-  "SDC"
############################################################################################
################################## Best Genotype ###########################################
############################################################################################

IdeotypeVar <-  Day9_ranks %>%
                            select(Root_weight,TRL,PRL,CVA,VOL,WID,LRB,RHZO) #select RSA traits to use for particular ideotype

IdeotypeSums <- IdeotypeVar %>%
                              mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day9_data %>%
                  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

BestGenotype <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together
BestGenotype <- arrange(BestGenotype, SUM) %>% mutate(BestRanked = rank(SUM, ties.method = "max")) #sort new df based on master ranking column with no.1 being best
BestGenotypeTop10 <- as.data.frame(BestGenotype[1:10,1, drop = FALSE])
#colnames(BestGenotypeTop10) <-  "Best"
############################################################################################
################################## Beard Type ###########################################
############################################################################################

Day9_ranks <- mutate(Day9_ranks, LRADescending = 293 - LRAMediangle)  #invert the ranking for LRAMediangle as a lower angle, not higher angle is desirable
Day9_ranks <- mutate(Day9_ranks, WIDinv = 293 - WID)  #invert the ranking for WID as narrow is desired, not wide
Day9_ranks <- mutate(Day9_ranks, LEDinv = 293 - LED)  #invert the ranking for more bottom half roots are desired
Day9_ranks <- mutate(Day9_ranks, SOL2inv = 293 - SOL2)  #invert the ranking for SOL2 as a lower density, not higher density is desirable

IdeotypeVar <-  Day9_ranks %>%
                          select(TRL,LRB,SOL2inv,LRADescending,WIDinv,LEDinv) #select RSA traits to use for particular ideotype

IdeotypeSums <- IdeotypeVar %>%
                            mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day9_data %>%
                  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

BeardType <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together

BeardType <- arrange(BeardType, SUM) %>% mutate(BeardRanked = rank(SUM, ties.method = "max")) #sort new df based on master ranking column with no.1 being best
BeardType[1:20,1:10]
BeardTop10 <- as.data.frame(BeardType[1:10,1, drop = FALSE])
#colnames(BeardTop10) <-  "Beard"

############################################################################################
################################## Umbrella Type ###########################################
############################################################################################

IdeotypeVar <-  Day9_ranks %>%
                         select(CVA,LRAMediangle,WID,LED,PRL) #select RSA traits to use for particular ideotype

IdeotypeSums <- IdeotypeVar %>%
  mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day9_data %>%
  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

UmbrellaType <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together

UmbrellaType <- arrange(UmbrellaType, SUM) %>% mutate(UmbrellaRanked = rank(SUM, ties.method = "max"))
UmbrellaType[1:20,1:9]
UmbrellaTop10 <- as.data.frame(UmbrellaType[1:10,1, drop = FALSE])
#colnames(UmbrellaTop10) <-  "Umbrella"

Top10Ideotypes <- cbind(BestGenotypeTop10,UmbrellaTop10,BeardTop10,ScavengerTop10,SteepDeepCheapTop10)
write.csv(Top10Ideotypes,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ideotypes/Top10Ideotypes.csv")
AllIdeotypes <- cbind(BestGenotype[,c(1,12), drop = FALSE],UmbrellaType[,c(9), drop = FALSE],BeardType[,c(10), drop = FALSE],Scavenger[,c(7), drop = FALSE],SteepDeepCheap[,c(8), drop = FALSE])
colnames(AllIdeotypes) <- c("Entry","BestGenotype","Umbrella","Beard","Scavenger","DT")
write.csv(AllIdeotypes,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ideotypes/AllIdeotypes.csv")


#############################################################################################################
########################  Lets just look at the USA Ranks for Ideotype  #####################################
#############################################################################################################
metadata <- read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/Cluster_metadata.csv")
USA <- metadata %>% filter(Day == "9") %>% select("Entry","Country","BestGenotype","Umbrella","Beard","Scavenger","DT") %>% filter(Country =="USA")
#############################################################################################################
################################## Extract MEANS from Clusters ##############################################
#############################################################################################################

`%>%` <- magrittr::`%>%`

BestGenotype <- Day9_data %>%
  filter(Entry %in% BestGenotypeTop10$Entry)%>% #Create meta df with Entry, PI and Cluster info
  rowwise() %>% 
  mutate(Ideotype = "BestGeno")
Scavenger <- Day9_data %>%
  filter(Entry %in% ScavengerTop10$Entry)%>% #Create meta df with Entry, PI and Cluster info
  rowwise() %>% 
  mutate(Ideotype = "Scav")
SteepDeepCheap <- Day9_data %>%
  filter(Entry %in% SteepDeepCheapTop10$Entry)%>% #Create meta df with Entry, PI and Cluster info
  rowwise() %>% 
  mutate(Ideotype = "SDC")
Beard <- Day9_data %>%
  filter(Entry %in% BeardTop10$Entry) %>%#Create meta df with Entry, PI and Cluster info
  rowwise() %>% 
  mutate(Ideotype = "Beard")
Umbrella <- Day9_data %>%
  filter(Entry %in% UmbrellaTop10$Entry)%>% #Create meta df with Entry, PI and Cluster info
  rowwise() %>% 
  mutate(Ideotype = "Umbrella")
SuperTraits <- rbind(BestGenotype,Scavenger,SteepDeepCheap,Beard,Umbrella)
colnames(SuperTraits)

SuperTraitMeans <- SuperTraits %>% group_by(Ideotype) %>% summarise_all(funs(mean))

write.csv(SuperTraitMeans,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ideotypes/SuperTraitMeans.csv")
########################################################################################################################################################################################

write.csv(Scavenger, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/Scavenger_Nov9.csv")
write.csv(SteepDeepCheap, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/SteepDeepCheap_Nov9.csv")
write.csv(BestGenotype, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/BestGenotype_Nov9.csv")
write.csv(BeardType, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/BeardType_Dec2.csv")

write.csv(BestGenotype[1:12,1:ncol(BestGenotype)], "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/BestGenotype_PIEchart.csv")
write.csv(SteepDeepCheap[1:12,1:ncol(SteepDeepCheap)], "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/SteepDeepCheap_PIEchart.csv")
write.csv(Scavenger[1:12,1:ncol(Scavenger)], "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/Scavenger_PIEchart.csv")


########################################################################################################################################################################################
########################################################################################################################################################################################
########################################################################################################################################################################################






############################################################################################
##################################### Day 6 ################################################
############################################################################################

#take only the columns with numerical data
colnum=c(31:ncol(Day6_data)) 
Day6_ranks <- data.frame(matrix(vector(),292,1, dimnames=list(c(), c("Entry")))) #create new dataframe that has 292 rows
Day6_ranks$Entry <- (Day6_data$Entry) #add column of Entry numbers from dataset
Day6_ranks$Name <- (Day6_data$Name) #add column of Name numbers from dataset
Day6_ranks$Day <- (Day6_data$Day) #add column of Days from dataset


for (i in 1:49){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day6_data)[x] #sets the current column header as the trait name
  Rank <- as.data.frame(rank(desc((Day6_data)[x])))
  rank <- names(Rank)[1] <- trait  #set current trait as column header
  Day6_ranks <- cbind(Day6_ranks,Rank) #add columns to existing dataframe   
}

############################################################################################
##################################### Scavenger ############################################
############################################################################################

IdeotypeVar <-  Day6_ranks %>%
  select(TRLUpper,WID,TRL) #select RSA traits to use for particular ideotype
IdeotypeSums <- IdeotypeVar %>%
  mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day6_data %>%
  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

Scavenger <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together

Scavenger <- arrange(Scavenger, SUM) %>% mutate(ScavengerRanked = rank(SUM, ties.method = "max")) #sort new df based on master ranking column with no.1 being best
ScavengerTop10 <- as.data.frame(Scavenger[1:10,1, drop = FALSE])
#colnames(ScavengerTop10) <-  "Scavenger"
############################################################################################
################################## Steep, Deep, Cheap#######################################
############################################################################################

Day6_ranks <- mutate(Day6_ranks, LRADescending = 293 - LRAMediangle)  #invert the ranking for LRAMediangle as a lower angle, not higher angle is desirable
Day6_ranks <- mutate(Day6_ranks, SOL2 = 293 - SOL2)  #invert the ranking for SOL2 as a lower density, not higher density is desirable

IdeotypeVar <-  Day6_ranks %>%
  select(LRADescending,PRL,SOL2,TRL) #select RSA traits to use for particular ideotype
IdeotypeSums <- IdeotypeVar %>%
  mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day6_data %>%
  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

SteepDeepCheap <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together

SteepDeepCheap <- arrange(SteepDeepCheap, SUM) %>% mutate(DTRanked = rank(SUM, ties.method = "max")) #sort new df based on master ranking column with no.1 being best
SteepDeepCheap[1:12,]
SteepDeepCheapTop10 <- as.data.frame(SteepDeepCheap[1:10,1, drop = FALSE])
#colnames(SteepDeepCheapTop10) <-  "SDC"
############################################################################################
################################## Best Genotype ###########################################
############################################################################################

IdeotypeVar <-  Day6_ranks %>%
  select(Root_weight,TRL,PRL,CVA,VOL,WID,LRB,RHZO) #select RSA traits to use for particular ideotype

IdeotypeSums <- IdeotypeVar %>%
  mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day6_data %>%
  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

BestGenotype <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together
BestGenotype <- arrange(BestGenotype, SUM) %>% mutate(BestRanked = rank(SUM, ties.method = "max")) #sort new df based on master ranking column with no.1 being best
BestGenotypeTop10 <- as.data.frame(BestGenotype[1:10,1, drop = FALSE])
#colnames(BestGenotypeTop10) <-  "Best"
############################################################################################
################################## Beard Type ###########################################
############################################################################################

Day6_ranks <- mutate(Day6_ranks, LRADescending = 293 - LRAMediangle)  #invert the ranking for LRAMediangle as a lower angle, not higher angle is desirable
Day6_ranks <- mutate(Day6_ranks, WIDinv = 293 - WID)  #invert the ranking for WID as narrow is desired, not wide
Day6_ranks <- mutate(Day6_ranks, LEDinv = 293 - LED)  #invert the ranking for more bottom half roots are desired
Day6_ranks <- mutate(Day6_ranks, SOL2inv = 293 - SOL2)  #invert the ranking for SOL2 as a lower density, not higher density is desirable

IdeotypeVar <-  Day6_ranks %>%
  select(TRL,LRB,SOL2inv,LRADescending,WIDinv,LEDinv) #select RSA traits to use for particular ideotype

IdeotypeSums <- IdeotypeVar %>%
  mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day6_data %>%
  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

BeardType <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together

BeardType <- arrange(BeardType, SUM) %>% mutate(BeardRanked = rank(SUM, ties.method = "max")) #sort new df based on master ranking column with no.1 being best
BeardType[1:20,1:10]
BeardTop10 <- as.data.frame(BeardType[1:10,1, drop = FALSE])
#colnames(BeardTop10) <-  "Beard"

############################################################################################
################################## Umbrella Type ###########################################
############################################################################################

IdeotypeVar <-  Day6_ranks %>%
  select(CVA,LRAMediangle,WID,LED,PRL) #select RSA traits to use for particular ideotype

IdeotypeSums <- IdeotypeVar %>%
  mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day6_data %>%
  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

UmbrellaType <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together

UmbrellaType <- arrange(UmbrellaType, SUM) %>% mutate(UmbrellaRanked = rank(SUM, ties.method="max"))
UmbrellaType[1:20,1:9]
UmbrellaTop10 <- as.data.frame(UmbrellaType[1:10,1, drop = FALSE])
#colnames(UmbrellaTop10) <-  "Umbrella"

Scavenger <- arrange(Scavenger,ScavengerRanked) %>% select(Entry,ScavengerRanked) %>% arrange(Entry)
write.csv(Scavenger, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/Scavenger_Day6.csv")
SteepDeepCheap <- arrange(SteepDeepCheap,DTRanked) %>% select(Entry,DTRanked) %>% arrange(Entry)
write.csv(SteepDeepCheap, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/DroughtTolerant_Day6.csv")
BestGenotype <- arrange(BestGenotype,BestRanked) %>% select(Entry,BestRanked) %>% arrange(Entry)
write.csv(BestGenotype, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/BestGenotype_Day6.csv")
BeardType <- arrange(BeardType,BeardRanked) %>% select(Entry,BeardRanked) %>% arrange(Entry)
write.csv(BeardType, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/BeardType_Day6.csv")
UmbrellaType <- arrange(UmbrellaType,UmbrellaRanked) %>% select(Entry,UmbrellaRanked) %>% arrange(Entry)
write.csv(UmbrellaType, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/Umbrella_Day6.csv")

Day6_ideotypes_combined <- cbind(BestGenotype,Scavenger,SteepDeepCheap,BeardType,UmbrellaType)
write.csv(Day6_ideotypes_combined, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/Day6_ideotypes_combined.csv")


############################################################################################
##################################### Day 9  ###############################################
############################################################################################



#take only the columns with numerical data
colnum=c(31:ncol(Day9_data)) 
Day9_ranks <- data.frame(matrix(vector(),292,1, dimnames=list(c(), c("Entry")))) #create new dataframe that has 292 rows
Day9_ranks$Entry <- (Day9_data$Entry) #add column of Entry numbers from dataset
Day9_ranks$Name <- (Day9_data$Name) #add column of Name numbers from dataset
Day9_ranks$Day <- (Day9_data$Day) #add column of Days from dataset

ncol(Day9_data) - 31
for (i in 1:50){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day9_data)[x] #sets the current column header as the trait name
  Rank <- as.data.frame(rank(desc((Day9_data)[x])))
  rank <- names(Rank)[1] <- trait  #set current trait as column header
  Day9_ranks <- cbind(Day9_ranks,Rank) #add columns to existing dataframe   
}

All_Ranks <- rbind(Day6_ranks,Day9_ranks,Day9_ranks)
############################################################################################
##################################### Scavenger ############################################
############################################################################################

IdeotypeVar <-  Day9_ranks %>%
  select(TRLUpper,WID,TRL_GR) #select RSA traits to use for particular ideotype
IdeotypeSums <- IdeotypeVar %>%
  mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day9_data %>%
  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

Scavenger <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together
Scavenger <- arrange(Scavenger, SUM) %>% mutate(ScavengerRanked = rank(SUM, ties.method = "max")) #sort new df based on master ranking column with no.1 being best

ScavengerTop10Day9 <- as.data.frame(Scavenger[1:10,1, drop = FALSE])
#colnames(ScavengerTop10) <-  "Scavenger"
############################################################################################
################################## Steep, Deep, Cheap#######################################
############################################################################################

Day9_ranks <- mutate(Day9_ranks, LRADescending = 293 - LRAMediangle)  #invert the ranking for LRAMediangle as a lower angle, not higher angle is desirable
Day9_ranks <- mutate(Day9_ranks, SOL2 = 293 - SOL2)  #invert the ranking for SOL2 as a lower density, not higher density is desirable

IdeotypeVar <-  Day9_ranks %>%
  select(LRADescending,PRL,SOL2,TRL_GR) #select RSA traits to use for particular ideotype
IdeotypeSums <- IdeotypeVar %>%
  mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day9_data %>%
  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

SteepDeepCheap <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together

SteepDeepCheap <- arrange(SteepDeepCheap, SUM)%>% mutate(DTRanked = rank(SUM, ties.method = "max")) #sort new df based on master ranking column with no.1 being best

SteepDeepCheap[1:12,]
SteepDeepCheapTop10Day9 <- as.data.frame(SteepDeepCheap[1:10,1, drop = FALSE])
#colnames(SteepDeepCheapTop10) <-  "SDC"
############################################################################################
################################## Best Genotype ###########################################
############################################################################################

IdeotypeVar <-  Day9_ranks %>%
  select(Root_weight,TRL,PRL,CVA,VOL,WID,LRB,RHZO) #select RSA traits to use for particular ideotype

IdeotypeSums <- IdeotypeVar %>%
  mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day9_data %>%
  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

BestGenotype <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together

BestGenotype <- arrange(BestGenotype, SUM) %>% mutate(BestRanked = rank(SUM, ties.method = "max")) #sort new df based on master ranking column with no.1 being best
BestGenotypeTop10Day9 <- as.data.frame(BestGenotype[1:10,1, drop = FALSE])
#colnames(BestGenotypeTop10) <-  "Best"
############################################################################################
################################## Beard Type ###########################################
############################################################################################


Day9_ranks <- mutate(Day9_ranks, LRADescending = 293 - LRAMediangle)  #invert the ranking for LRAMediangle as a lower angle, not higher angle is desirable
Day9_ranks <- mutate(Day9_ranks, WIDinv = 293 - WID)  #invert the ranking for WID as narrow is desired, not wide
Day9_ranks <- mutate(Day9_ranks, LEDinv = 293 - LED)  #invert the ranking for more bottom half roots are desired
Day9_ranks <- mutate(Day9_ranks, SOL2inv = 293 - SOL2)  #invert the ranking for SOL2 as a lower density, not higher density is desirable

IdeotypeVar <-  Day9_ranks %>%
  select(TRL,LRB,SOL2inv,LRADescending,WIDinv,LEDinv) #select RSA traits to use for particular ideotype

IdeotypeSums <- IdeotypeVar %>%
  mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day9_data %>%
  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

BeardType <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together

BeardType <- arrange(BeardType, SUM) %>% mutate(BeardRanked = rank(SUM, ties.method = "max")) #sort new df based on master ranking column with no.1 being best
BeardType[1:20,1:10]
BeardTop10 <- as.data.frame(BeardType[1:10,1, drop = FALSE])
#colnames(BeardTop


############################################################################################
################################## Umbrella Type ###########################################
############################################################################################

IdeotypeVar <-  Day9_ranks %>%
  select(CVA,LRAMediangle,WID,LED,PRL) #select RSA traits to use for particular ideotype

IdeotypeSums <- IdeotypeVar %>%
  mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day9_data %>%
  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

UmbrellaType <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together

UmbrellaType <- arrange(UmbrellaType, SUM)%>% mutate(UmbrellaRanked = rank(SUM, ties.method = "max")) #sort new df based on master ranking column with no.1 being best
UmbrellaType[1:20,1:9]
UmbrellaTop10Day9 <- as.data.frame(UmbrellaType[1:10,1, drop = FALSE])


Scavenger <- arrange(Scavenger,ScavengerRanked) %>% select(Entry,ScavengerRanked) %>% arrange(Entry)
write.csv(Scavenger, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/Scavenger_Day9.csv")
SteepDeepCheap <- arrange(SteepDeepCheap,DTRanked) %>% select(Entry,DTRanked) %>% arrange(Entry)
write.csv(SteepDeepCheap, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/DroughtTolerant_Day9.csv")
BestGenotype <- arrange(BestGenotype,BestRanked) %>% select(Entry,BestRanked) %>% arrange(Entry)
write.csv(BestGenotype, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/BestGenotype_Day9.csv")
BeardType <- arrange(BeardType,BeardRanked) %>% select(Entry,BeardRanked) %>% arrange(Entry)
write.csv(BeardType, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/BeardType_Day9.csv")
UmbrellaType <- arrange(UmbrellaType,UmbrellaRanked) %>% select(Entry,UmbrellaRanked) %>% arrange(Entry)
write.csv(UmbrellaType, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/Umbrella_Day9.csv")

Day9_ideotypes_combined <- cbind(BestGenotype,Scavenger,SteepDeepCheap,BeardType,UmbrellaType)
write.csv(Day9_ideotypes_combined, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/Day9_ideotypes_combined.csv")








############################################################################################
##################################### Day 12  ##############################################
############################################################################################



#take only the columns with numerical data
colnum=c(31:ncol(Day12_data)) 
Day12_ranks <- data.frame(matrix(vector(),292,1, dimnames=list(c(), c("Entry")))) #create new dataframe that has 292 rows
Day12_ranks$Entry <- (Day12_data$Entry) #add column of Entry numbers from dataset
Day12_ranks$Name <- (Day12_data$Name) #add column of Name numbers from dataset
Day12_ranks$Day <- (Day12_data$Day) #add column of Days from dataset

ncol(Day12_data) - 31
for (i in 1:50){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day12_data)[x] #sets the current column header as the trait name
  Rank <- as.data.frame(rank(desc((Day12_data)[x])))
  rank <- names(Rank)[1] <- trait  #set current trait as column header
  Day12_ranks <- cbind(Day12_ranks,Rank) #add columns to existing dataframe   
}

All_Ranks <- rbind(Day6_ranks,Day9_ranks,Day12_ranks)
############################################################################################
##################################### Scavenger ############################################
############################################################################################

IdeotypeVar <-  Day12_ranks %>%
  select(TRLUpper,WID,TRL_GR) #select RSA traits to use for particular ideotype
IdeotypeSums <- IdeotypeVar %>%
  mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day12_data %>%
  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

Scavenger <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together
Scavenger <- arrange(Scavenger, SUM) %>% mutate(ScavengerRanked = rank(SUM, ties.method = "max")) #sort new df based on master ranking column with no.1 being best

ScavengerTop10Day12 <- as.data.frame(Scavenger[1:10,1, drop = FALSE])
#colnames(ScavengerTop10) <-  "Scavenger"
############################################################################################
################################## Steep, Deep, Cheap#######################################
############################################################################################

Day12_ranks <- mutate(Day12_ranks, LRADescending = 293 - LRAMediangle)  #invert the ranking for LRAMediangle as a lower angle, not higher angle is desirable
Day12_ranks <- mutate(Day12_ranks, SOL2 = 293 - SOL2)  #invert the ranking for SOL2 as a lower density, not higher density is desirable

IdeotypeVar <-  Day12_ranks %>%
  select(LRADescending,PRL,SOL2,TRL_GR) #select RSA traits to use for particular ideotype
IdeotypeSums <- IdeotypeVar %>%
  mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day12_data %>%
  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

SteepDeepCheap <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together

SteepDeepCheap <- arrange(SteepDeepCheap, SUM)%>% mutate(DTRanked = rank(SUM, ties.method = "max")) #sort new df based on master ranking column with no.1 being best

SteepDeepCheap[1:12,]
SteepDeepCheapTop10Day12 <- as.data.frame(SteepDeepCheap[1:10,1, drop = FALSE])
#colnames(SteepDeepCheapTop10) <-  "SDC"
############################################################################################
################################## Best Genotype ###########################################
############################################################################################

IdeotypeVar <-  Day12_ranks %>%
  select(Root_weight,TRL,PRL,CVA,VOL,WID,LRB,RHZO) #select RSA traits to use for particular ideotype

IdeotypeSums <- IdeotypeVar %>%
  mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day12_data %>%
  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

BestGenotype <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together

BestGenotype <- arrange(BestGenotype, SUM) %>% mutate(BestRanked = rank(SUM, ties.method = "max")) #sort new df based on master ranking column with no.1 being best
BestGenotypeTop10Day12 <- as.data.frame(BestGenotype[1:10,1, drop = FALSE])
#colnames(BestGenotypeTop10) <-  "Best"
############################################################################################
################################## Beard Type ###########################################
############################################################################################

Day12_ranks <- mutate(Day12_ranks, LRADescending = 293 - LRAMediangle)  #invert the ranking for LRAMediangle as a lower angle, not higher angle is desirable
Day12_ranks <- mutate(Day12_ranks, WIDinv = 293 - WID)  #invert the ranking for WID as narrow is desired, not wide
Day12_ranks <- mutate(Day12_ranks, LEDinv = 293 - LED)  #invert the ranking for more bottom half roots are desired
Day12_ranks <- mutate(Day12_ranks, SOL2inv = 293 - SOL2)  #invert the ranking for SOL2 as a lower density, not higher density is desirable

IdeotypeVar <-  Day12_ranks %>%
  select(TRL,LRB,SOL2inv,LRADescending,WIDinv,LEDinv) #select RSA traits to use for particular ideotype

IdeotypeSums <- IdeotypeVar %>%
  mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day12_data %>%
  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

BeardType <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together

BeardType <- arrange(BeardType, SUM) %>% mutate(BeardRanked = rank(SUM, ties.method = "max"))#sort new df based on master ranking column with no.1 being best
BeardType[1:20,1:10]
BeardTop10Day12 <- as.data.frame(BeardType[1:10,1, drop = FALSE])
#colnames(BeardTop10) <-  "Beard"

############################################################################################
################################## Umbrella Type ###########################################
############################################################################################

IdeotypeVar <-  Day12_ranks %>%
  select(CVA,LRAMediangle,WID,LED,PRL) #select RSA traits to use for particular ideotype

IdeotypeSums <- IdeotypeVar %>%
  mutate(SUM = IdeotypeVar %>% rowSums()) #sum the rankings to make a master ranking column
Meta <- Day12_data %>%
  select(Entry,Name,snp35k) #Create meta df with Entry, PI and Cluster info

UmbrellaType <- cbind(Meta, IdeotypeSums) #bind meta and ideotype df together

UmbrellaType <- arrange(UmbrellaType, SUM)%>% mutate(UmbrellaRanked = rank(SUM, ties.method = "max")) #sort new df based on master ranking column with no.1 being best
UmbrellaType[1:20,1:9]
UmbrellaTop10Day12 <- as.data.frame(UmbrellaType[1:10,1, drop = FALSE])


Scavenger <- arrange(Scavenger,ScavengerRanked) %>% select(Entry,ScavengerRanked) %>% arrange(Entry)
write.csv(Scavenger, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/Scavenger_Day12.csv")
SteepDeepCheap <- arrange(SteepDeepCheap,DTRanked) %>% select(Entry,DTRanked) %>% arrange(Entry)
write.csv(SteepDeepCheap, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/DroughtTolerant_Day12.csv")
BestGenotype <- arrange(BestGenotype,BestRanked) %>% select(Entry,BestRanked) %>% arrange(Entry)
write.csv(BestGenotype, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/BestGenotype_Day12.csv")
BeardType <- arrange(BeardType,BeardRanked) %>% select(Entry,BeardRanked) %>% arrange(Entry)
write.csv(BeardType, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/BeardType_Day12.csv")
UmbrellaType <- arrange(UmbrellaType,UmbrellaRanked) %>% select(Entry,UmbrellaRanked) %>% arrange(Entry)
write.csv(UmbrellaType, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/Umbrella_Day12.csv")

Day12_ideotypes_combined <- cbind(BestGenotype,Scavenger,SteepDeepCheap,BeardType,UmbrellaType)
write.csv(Day12_ideotypes_combined, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Ranks/Day12_ideotypes_combined.csv")


