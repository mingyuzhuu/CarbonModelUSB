library(ProjectTemplate)
library(ggplot2)
library(plyr)
library(reshape2)
library(tidyselect)
library(dplyr)
library(data.table)
library(tinytex)
library(gridExtra)
library(zoo)

setwd('E:/20190923 Newcastle-M.Res/OneDrive - Newcastle University/20_21_autumn/210324 Journal Paper/4_USB Carbon/Carbon')
#create.project('Carbon')
load.project()

###########################
#add EUI into the dataset

termData$actEUI = termData$actual/12.5  
termData$DEUI = termData$day/12.5 
termData$NEUI = termData$night/12.5  


termData$simEUI = termData$sim/12.5  
termData$simDEUI = termData$simDay/12.5 
termData$simNEUI = termData$simNight/12.5 


#termData$gapEUI = termData$actEUI - termData$simEUI
#termData$gapDEUI = termData$DEUI - termData$simDEUI
#termData$gapNEUI = termData$NEUI - termData$simNEUI


#####

nontermData$actEUI = nontermData$actual/12.5  
nontermData$DEUI = nontermData$day/12.5 
nontermData$NEUI = nontermData$night/12.5  


nontermData$simEUI = nontermData$sim/12.5  
nontermData$simDEUI = nontermData$simDay/12.5 
nontermData$simNEUI = nontermData$simNight/12.5 


#nontermData$gapEUI = nontermData$actEUI - nontermData$simEUI
#nontermData$gapDEUI = nontermData$DEUI - nontermData$simDEUI
#nontermData$gapNEUI = nontermData$NEUI - nontermData$simNEUI



###############################
## log transformation of EUI and occupancy
termData$logDEUI = log(termData$DEUI)
termData$logDocc = log(termData$occDay)
termData$logNEUI = log(termData$NEUI)
termData$logNocc = log(termData$occNight)





nontermData$logDEUI = log(nontermData$DEUI)
nontermData$logDocc = log(nontermData$occDay)
nontermData$logNEUI = log(nontermData$NEUI)
nontermData$logNocc = log(nontermData$occNight)

##################################

## standardize the occupancy and weather data to a mean of 0 and sd of 1

termdataset = termData %>% mutate_at(c('tem','ws','humid','sr','logDocc','logNocc'), ~(scale(.) %>% as.vector))

#termdataset = subset(termdataset, select = -c( gapEUI,gapDEUI,gapNEUI))


##


nontermdataset = nontermData %>% mutate_at(c('tem','ws','humid','sr','logDocc','logNocc'), ~(scale(.) %>% as.vector))

#nontermdataset = subset(nontermdataset, select = -c(gapEUI,gapDEUI,gapNEUI))

##################################
##################################
##################################
#####multiple regression###########

termdaymodel = lm(termdataset$logDEUI ~ termdataset$tem + termdataset$logDocc)
summary(termdaymodel)

termnightmodel = lm(termdataset$logNEUI ~ termdataset$tem)
summary(termnightmodel)


nontermdaymodel = lm(nontermdataset$logDEUI ~ nontermdataset$tem  + nontermdataset$logDocc)
summary(nontermdaymodel)


nontermnightmodel = lm(nontermdataset$logNEUI ~ nontermdataset$tem  + nontermdataset$logNocc)
summary(nontermnightmodel)

##################################
##################################
##################################
################################################
#####term time occupied (daytime)###########

termdataset$preDEUI = 5.756492-0.083560*termdataset$tem + 0.061265*termdataset$logDocc

termdataset$normDEUI = exp(termdataset$logDEUI - termdataset$preDEUI + 5.756492)

termdataset$Dgap  = termdataset$normDEUI - termdataset$simDEUI

termdataset$DCarbon = termdataset$normDEUI*termdataset$intensity

termdataset$DCarbonGap = termdataset$Dgap*termdataset$intensity

range01 <- function(x){(x-min(x))/(max(x)-min(x))}    ####define the function to rescale between 0 and 1


termdataset$Dgapscore = 1 - range01(termdataset$DCarbonGap)

termdataset$Dscore = 1 - range01(termdataset$DCarbon)




#####term time unoccupied (nighttime)###########

termdataset$Ngap = termdataset$NEUI - termdataset$simNEUI

termdataset$NCarbon = termdataset$NEUI*termdataset$intensity

termdataset$NCarbonGap = termdataset$Ngap*termdataset$intensity



termdataset$Ngapscore = 1 - range01(termdataset$NCarbonGap)

termdataset$Nscore = 1 - range01(termdataset$NCarbon)
#####simplified dataset###########

termscore = subset(termdataset, select = c(X,Dgapscore,Dscore,Ngapscore,Nscore))#######################


################################################
################################################
################################################
################################################
#####nonterm time occupied (daytime)###########

nontermdataset$preDEUI = 5.6557+0.08372*nontermdataset$tem + 0.18959*nontermdataset$logDocc

nontermdataset$normDEUI = exp(nontermdataset$logDEUI - nontermdataset$preDEUI + 5.6557)

nontermdataset$Dgap  = nontermdataset$normDEUI - nontermdataset$simDEUI

nontermdataset$DCarbon = nontermdataset$normDEUI*nontermdataset$intensity

nontermdataset$DCarbonGap = nontermdataset$Dgap*nontermdataset$intensity



nontermdataset$Dgapscore = 1 - range01(nontermdataset$DCarbonGap)

nontermdataset$Dscore = 1 - range01(nontermdataset$DCarbon)


#####nonterm time unoccupied (nighttime)###########

nontermdataset$Ngap = nontermdataset$NEUI - nontermdataset$simNEUI

nontermdataset$NCarbon = nontermdataset$NEUI*nontermdataset$intensity

nontermdataset$NCarbonGap = nontermdataset$Ngap*nontermdataset$intensity



nontermdataset$Ngapscore = 1 - range01(nontermdataset$NCarbonGap)

nontermdataset$Nscore = 1 - range01(nontermdataset$NCarbon)
#####simplified dataset###########

nontermscore = subset(nontermdataset, select = c(X,Dgapscore,Dscore,Ngapscore,Nscore))#######################



################################################
################################################
################################################
################################################

#getwd()

#write.csv(termscore,"data\\termscore.csv", row.names = FALSE)

#write.csv(nontermscore,"data\\nontermscore.csv", row.names = FALSE)


################################################
################################################
################################################
################################################

#################
#get over all all-time score

q1 = subset(nontermdataset, select = c(X,occTotal,tem,ws,humid,sr,actEUI,simEUI,intensity))
q1$X <- as.Date(q1$X, "%d/%m/%Y")


q2 = subset(termdataset, select = c(X,occTotal,tem,ws,humid,sr,actEUI,simEUI,intensity))
q2$X <- as.Date(q2$X, "%Y-%m-%d")

q = rbind(q1, q2)
q = q %>% arrange(X)
q$gap = q$actEUI - q$simEUI


#q1$logEUI = log(q1$actEUI)
#q1$logOcc = log(q1$occTotal)
#q1 = q1 %>% mutate_at(c('logOcc'), ~(scale(.) %>% as.vector))


#q2$logEUI = log(q2$actEUI)
#q2$logOcc = log(q2$occTotal)
#q2 = q2 %>% mutate_at(c('logOcc'), ~(scale(.) %>% as.vector))

######Get Carbon

q$Carbon = q$actEUI*q$intensity

q$CarbonGap = q$gap*q$intensity



################
######Get Carbon Score

q$score = 1 - range01(q$Carbon)
q$gapscore = 1 - range01(q$CarbonGap)

######################
######################

AllTimescore = subset(q, select = c(X,score,gapscore))#######################

######################
######################

#getwd()

#write.csv(AllTimescore,"data\\Allscore.csv", row.names = FALSE)


######################
######################

nontermscore$X <- as.Date(nontermscore$X, "%d/%m/%Y")

######################
######################
######################
######################
######################
######################
######################
######################Produce Graphs


vis = merge(x = Allcore, y = termscore, by = "X",
               all.x = TRUE)


###########################

termscore$X <- as.Date(termscore$X, "%Y-%m-%d")

nontermscore$X <- as.Date(nontermscore$X, "%Y-%m-%d")

df = bind_rows(termscore,nontermscore)

df = df %>% arrange(X)



########

Allcore$X <- as.Date(Allcore$X, "%Y-%m-%d")

vis = merge(x = Allcore, y = df, by = "X",
            all.x = TRUE)

########


write.csv(vis,"data\\vis.csv", row.names = FALSE)


################################
################################
################################
################################
################################
##rolling mean 

scoreMean = rollmean(vis$score, 15,na.pad=TRUE)
DscoreMean = rollmean(vis$Dscore, 15,na.pad=TRUE)
NscoreMean = rollmean(vis$Nscore, 15,na.pad=TRUE)

vis$date = as.Date(vis$X, "%Y-%m-%d")


ggplot(data = vis,aes(x = date)
) + geom_line(aes(y = DscoreMean ),colour = 'red',size =1.5
) + geom_line(aes(y = NscoreMean), colour = 'blue', size =1.5
) + geom_line(aes(y = scoreMean), colour = 'black', size =1.5)




######################
######################
######################

gapscoreMean = rollmean(vis$gapscore, 15,na.pad=TRUE)
DgapscoreMean = rollmean(vis$Dgapscore, 15,na.pad=TRUE)
NgapscoreMean = rollmean(vis$Ngapscore, 15,na.pad=TRUE)

ggplot(data = vis,aes(x = date)
) + geom_line(aes(y = DgapscoreMean ),colour = 'red',size =1.5
) + geom_line(aes(y = NgapscoreMean), colour = 'blue', size =1.5
) + geom_line(aes(y = gapscoreMean), colour = 'black', size =1.5)



