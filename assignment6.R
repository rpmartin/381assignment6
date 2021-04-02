
###### This is the file where you save your R code that produces the objects (graphs, tables) that you want to embed into 
# your assignment.  Make sure that there are no errors when you source this file before sourcing it from assignment3.Rmd file.
# To show you how it is done I have included code below that answers questions 1 and 2.

rm(list=ls()) #makes sure that your work environment is clean.
library("tidyverse") #we use functions from this library.
library("viridis")
library("knitr") #we use functions from this library.
slash <- ifelse(.Platform$OS.type=="unix", "/", "\\") #The eternal directory battle: Windows vs. *nix
options(scipen=10000)
####################which section?
section <- 1
#########################
mydf1 <- read_csv(paste("publicdata1",slash,"tax.csv",sep=""))%>%
  mutate(section=1)
mydf2 <- read_csv(paste("publicdata2",slash,"tax.csv",sep=""))%>%
  mutate(section=2)
mydf <- bind_rows(mydf1,mydf2)
spread <- mydf%>%group_by(rematching,round,section)%>%summarize(sd_choice=sd(choice))
ggplot(spread,aes(x=round,y=sd_choice,colour=rematching))+geom_smooth(method="lm")+geom_point()

mydf <- mydf%>%
  group_by(oneid,section)%>% #group the data by student id so we can calculate...
  mutate(lag_ave_other=lag(ave_other))%>% # because you don't see others' choices till the following round. 
  filter(lag_ave_other>49, size==3, round>2)%>% #get rid of garbage
  mutate(ave_profit=mean(profit,na.rm=TRUE), # each subject's average profit
         cum_mean_others=cummean(lag_ave_other)) #the average of all the other tax rates you observed before your choice of tax rate. 
# e.g. in round 4 you would have observed choices for rounds 2 and 3. (you didn't get feedback on round 1.)

