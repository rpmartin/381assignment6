###### This is the file where you save your R code that produces the objects (graphs, tables) that you want to embed into 
# your assignment.  Make sure that there are no errors when you source this file before sourcing it from assignment3.Rmd file.
# To show you how it is done I have included code below that answers questions 1 and 2.

rm(list=ls()) #makes sure that your work environment is clean.
library("tidyverse") #we use functions from this library.
library("knitr") #we use functions from this library.
slash <- ifelse(.Platform$OS.type=="unix", "/", "\\") #The eternal directory battle: Windows vs. *nix
options(scipen=10000) #dont use scientific notation.
####################which section?
section <- 1
#########################
mydf <- read_csv(paste("publicdata",section,slash,"tax",section,".csv",sep="")) # reads in the data for your section.
mydf <- mydf%>%
  group_by(oneid)%>% #group the data by student id so we can calculate...
  mutate(lag_ave_other=lag(ave_other))%>% # because you don't see others' choices till the following round. 
  filter(round>2,size>2)%>% #keep data from rounds 3-20 for groups of 3.
  mutate(ave_profit=mean(profit,na.rm=TRUE), # each subject's average profit
         cum_mean_others=cummean(lag_ave_other)) #the average of all the other tax rates observed before choice. 
# e.g. in round 5 this is the average of ave_other for rounds 2,3 and 4. (no feedback on round 1.)
write_csv(mydf, file=paste("publicdata",section,"/mydf",section,".csv",sep="")) #write the dataframe for excel users. 

subject_averages <- mydf%>% #create dataframe subject_averages by taking dataframe mydf THEN
  group_by(oneid,rematching)%>% #grouping the data by individual (rematching does not vary by individual, we just want to keep it) THEN
  summarize(ave_choice=mean(choice),ave_profit=mean(profit))# create summary statistics for each subject: the average choice, and their average profit.
write_csv(subject_averages, file=paste("publicdata",section,"/subject_averages",section,".csv",sep=""))#write the dataframe for excel users.
##############################################

