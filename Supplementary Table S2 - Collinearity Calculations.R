install.packages(clusterGeneration)
install.packages(VIF)
install.packages(fmsb)
library('caret')
library(dplyr)
#require(MASS)
library(tidyverse)
require(clusterGeneration)
library(VIF)
library(fmsb)
library(lme4)
library(ggplot2)
library(dplyr)
library(nlme)
library(plyr)
##############################################################################
############### Tutorial #####################################################
###  https://www.r-bloggers.com/collinearity-and-stepwise-vif-selection/ #####
##############################################################################
set.seed(2)
num.vars<-15
num.obs<-200
cov.mat<-genPositiveDefMat(num.vars,covMethod="unifcorrmat")$Sigma
rand.vars<-mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)

parms<-runif(num.vars,-10,10)
y<-rand.vars %*% matrix(parms) + rnorm(num.obs,sd=20)

lm.dat<-data.frame(y,rand.vars)
form.in<-paste('y ~',paste(names(lm.dat)[-1],collapse='+'))
mod1<-lm(form.in,data=lm.dat)
summary(mod1)
summary(model)

vif_func(in_frame=rand.vars,thresh=5,trace=T)

keep.dat<-vif_func(in_frame=rand.vars,thresh=5,trace=F)
form.in<-paste('y ~',paste(keep.dat,collapse='+'))
mod2<-lm(form.in,data=lm.dat)
summary(mod2)
###############################################################################
################################## End of Tutorial ############################
###############################################################################



###############################################################################
############################# Explore Correlations ############################
###############################################################################

BLUPs <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Data/KGF_AdjustedBLUPsAllDays_thinned_Oct24_tall_TRL_GR.csv")
df1 <- BLUPs %>% dplyr::select(-(TRL_GR))
df1 <- BLUPs %>%  dplyr::select(Day,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL2,LED,RHZO,TRL_GR,TRLUpper,Root_weight)

df1 <- df1[,c(3,31:ncol(df1))]
colnames(df1)
Day6_data <- subset(df1, df1$Day == "6")
Day9_data <- subset(df1, df1$Day == "9")
Day12_data <- subset(df1, df1$Day == "12")

colnames(Day6_data)
cor(x=Day6_data[,c(-1,-12)], y = NULL, use = "everything", method = c("pearson"))
cor(x=Day9_data[,-1], y = NULL, use = "everything", method = c("pearson"))
cor(x=Day12_data[,c(-1,-35)], y = NULL, use = "everything", method = c("pearson"))

###################################################################################################################
# BEFORE VIF Selection
###################################################################################################################
y <- (Day6_data$TRL + Day6_data$PRL + Day6_data$WID + Day6_data$CVA + Day6_data$LRB + Day6_data$VOL + Day6_data$LRA + Day6_data$SOL2 + Day6_data$LED + Day6_data$RHZO + Day6_data$TRLUpper + Day6_data$Root_weight)
lm.dat<-data.frame(y,Day6_data[,c(-1,-12)])
form.in<-paste('y ~',paste(names(lm.dat)[-1],collapse='+'))
mod1<-lm(form.in,data=lm.dat)
summary(mod1)

y <- (Day9_data$TRL + Day9_data$PRL + Day9_data$WID + Day9_data$CVA + Day9_data$LRB + Day9_data$VOL + Day9_data$LRA + Day9_data$SOL2 + Day9_data$LED + Day9_data$RHZO + Day9_data$TRL_GR + Day9_data$TRLUpper + Day9_data$Root_weight)
lm.dat<-data.frame(y,Day9_data[,-1])
form.in<-paste('y ~',paste(names(lm.dat)[-1],collapse='+'))
mod1<-lm(form.in,data=lm.dat)
summary(mod1)

y <- (Day12_data$TRL + Day12_data$PRL + Day12_data$WID + Day12_data$CVA + Day12_data$LRB + Day12_data$VOL + Day12_data$LRA + Day12_data$SOL2 + Day12_data$LED + Day12_data$RHZO + Day12_data$TRL_GR + Day12_data$TRLUpper + Day12_data$Root_weight)
lm.dat<-data.frame(y,Day12_data[,-1])
form.in<-paste('y ~',paste(names(lm.dat)[-1],collapse='+'))
mod1<-lm(form.in,data=lm.dat)
summary(mod1)

####################################################################################
################ Remove collinear traits with a VIF threshold of 5 #################
####################################################################################

vif_func(in_frame=Day6_data[,-1],thresh=5,trace=T)
vif_func(in_frame=Day9_data[,-1],thresh=5,trace=T)
vif_func(in_frame=Day12_data[,-1],thresh=5,trace=T)

vif_func(in_frame=Day6_data,thresh=5,trace=T)
vif_func(in_frame=Day9_data,thresh=5,trace=T)
vif_func(in_frame=Day12_data,thresh=5,trace=T)

Day6_VIFiltered <- Day6_data %>% dplyr::select(SL_PL, MED, LED, VOL, DEP, SOL2, CM, CMM, CMB, CP, CPT, LRA, SRL.LRB, TRLUpper.RootWeight, Shoot.Root.Weight)
Day9_VIFiltered <- Day9_data %>% dplyr::select(SL_PL, MED, VOL, WDR, NLR, TRALower, SOLinv, CM, CMT, CMM, CMB, CP, CPT, LRA, LRA.WID, TRLUpper.RootWeight, Shoot.Root.Weight)
Day12_VIFiltered <- Day12_data %>% dplyr::select(SL_PL, LED, VOL, DEP, SOL2, BSH, CM, CMM, CMB, CP, CPT, LRA, RootWeight.NetworkArea, LRA.WID, Shoot.Root.Weight, TRL_GR)

colnames(df1)
Day6_VIFiltered <- df1[,1:31] %>% filter(Day == 6) %>% cbind(Day6_VIFiltered)
Day9_VIFiltered <- df1[,1:31] %>% filter(Day == 9) %>% cbind(Day9_VIFiltered)
Day12_VIFiltered <- df1[,1:31] %>% filter(Day == 12) %>% cbind(Day12_VIFiltered)

colnames(Day12_VIFiltered)

write.csv(Day6_VIFiltered, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Collinearity/Day6_VIFiltered.csv",row.names = F)
write.csv(Day9_VIFiltered, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Collinearity/Day9_VIFiltered.csv",row.names = F)
write.csv(Day12_VIFiltered, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Collinearity/Day12_VIFiltered.csv",row.names = F)

# PCA with function prcomp
Day6_PCA <- prcomp(Day6_VIFiltered[,32:ncol(Day6_VIFiltered)], scale. = TRUE) #Performs a principal components analysis on the given data matrix and returns the results as an object of class prcomp
Day9_PCA <- prcomp(Day9_VIFiltered[,32:ncol(Day9_VIFiltered)], scale. = TRUE) #Performs a principal components analysis on the given data matrix and returns the results as an object of class prcomp
Day12_PCA <- prcomp(Day12_VIFiltered[,32:ncol(Day12_VIFiltered)], scale. = TRUE) #Performs a principal components analysis on the given data matrix and returns the results as an object of class prcomp


##########################################################################
# After VIF Selection 
##########################################################################
df1 <- BLUPs %>%  dplyr::select(Day,PRL,WID,LRB,VOL,LRA,SOL2,LED,TRL_GR,Root_weight)
Day6_data <- subset(df1, df1$Day == "6")
Day9_data <- subset(df1, df1$Day == "9")
Day12_data <- subset(df1, df1$Day == "12")

y <- (Day6_data$PRL + Day6_data$WID + Day6_data$LRB + Day6_data$VOL + Day6_data$LRA + Day6_data$SOL2 + Day6_data$LED + Day6_data$Root_weight)
lm.dat<-data.frame(y,Day6_data[,c(-1,-12)])
form.in<-paste('y ~',paste(names(lm.dat)[-1],collapse='+'))
mod1<-lm(form.in,data=lm.dat)
summary(mod1)

y <- (Day9_data$PRL + Day9_data$WID + Day9_data$LRB + Day9_data$VOL + Day9_data$LRA + Day9_data$SOL2 + Day9_data$LED + Day9_data$TRL_GR + Day9_data$Root_weight)
lm.dat<-data.frame(y,Day9_data[,-1])
form.in<-paste('y ~',paste(names(lm.dat)[-1],collapse='+'))
mod1<-lm(form.in,data=lm.dat)
summary(mod1)

y <- (Day12_data$PRL + Day12_data$WID + Day12_data$LRB + Day12_data$VOL + Day12_data$LRA + Day12_data$SOL2 + Day12_data$LED + Day12_data$TRL_GR + Day12_data$Root_weight)
lm.dat<-data.frame(y,Day12_data[,-1])
form.in<-paste('y ~',paste(names(lm.dat)[-1],collapse='+'))
mod1<-lm(form.in,data=lm.dat)
summary(mod1)




# loadings
Day6_PCA_loading = Day6_PCA$x #the value of the rotated data (the centred data multiplied by the rotation matrix) 

Day6_PCA_data <- cbind(Day6_data[,1:31],Day6_PCA_loading)
Day9_PCA_data <- cbind(Day9_data[,1:31],Day6_PCA_loading)
Day12_PCA_data <- cbind(Day12_data[,1:31],Day6_PCA_loading)
colnames(Day6_data)

ggplot(Day9_PCA_data, aes(x = PC1, y = PC2, color = as.factor(PhenoCluster))) +
  geom_point(alpha=0.5,size=2) +
  labs(x = c(Day6_PCA$sdev[1],"PC1 (%)"), y = c(Day6_PCA$sdev[2], "PC2 (%)"))+
  theme_classic()+
  scale_color_brewer(palette="Set1")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

summary(Day6_PCA)

####################################################################################
################ VIF Function from r-Bloggers.com ##################################
####################################################################################

vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  library(fmsb)
  
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}
