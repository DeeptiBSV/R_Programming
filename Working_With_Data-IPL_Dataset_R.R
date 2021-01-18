# Working With Data Assignment 1. This is R script used for this assignment
#loading the package necesary to run this file
#First check for package required is installed, if not we need to install it

needed_packages <- c("dplyr", "ggplot2", "tidyverse", "readxl","sqldf","reshape2","data.table","treemap")   

# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed)   
library(tidyverse)
library(ggplot2)# for plots
library(dplyr)
library(readxl)# for readings xlsx file
library(sqldf)
library(reshape2)
library(data.table)
library(treemap)
#Loading IPL Dataset from kaggle
matches <- read.csv('https://raw.githubusercontent.com/DeeptiBSV/WWD-Dataset/main/matches.csv',header = TRUE)
strikerate<- read.csv('https://raw.githubusercontent.com/DeeptiBSV/WWD-Dataset/main/most_runs_average_strikerate.csv',header = TRUE)
homeaway <- read.csv('https://raw.githubusercontent.com/DeeptiBSV/WWD-Dataset/main/teamwise_home_and_away.csv', header = TRUE)
deliveries <- read.csv('https://raw.githubusercontent.com/DeeptiBSV/WWD-Dataset/main/deliveries.csv',header = TRUE)

dim(matches)

dim(strikerate)
head(strikerate)

dim(deliveries)
head(matches)
head(deliveries)


#MERGE matches and Homeaway
#com_matches_home<-matches%>%left_join(homeaway,by=c("team1"="team"))
#View(com_matches_home)
mat_home<- matches%>%left_join(homeaway,by=c("winner"="team"))

dim(mat_home)
###merge deliveries and Strikerate
del_fin<-deliveries%>%left_join(strikerate,by =c("batsman"="batsman"))



#####cleaning the dataset as after merging the dataset we can many unecessary columns and rows which we can remove

ipl_df1<-na.omit(mat_home)

sum(is.na(ipl_df1))
dim(ipl_df1)

#cleaning del_fin merged data
del_fin<-na.omit(del_fin)
##FInd out NA values and remove the unecessary columns
sapply(ipl_df1, function(x) sum(is.na(x)))

ipl_fin<-ipl_df1

ipl_fin<-subset(ipl_fin, select = -c(dl_applied,umpire3))
del_fin<-subset(del_fin,select = -c(player_dismissed,dismissal_kind,fielder))
dim(ipl_fin)
dim(del_fin)


#########################################Data Exploration#################################
##Summary of IPL dataset
summary(ipl_fin)

#QFind No of seasons in which match was played and in which season most matches were played
length(unique(ipl_fin$Season))
ipl_fin %>% group_by(Season) %>% summarise(match_cnt = n()) %>%  filter(match_cnt == max(match_cnt))
#totalNo of Unique players
length(unique(ipl_fin$winner))

#Which  team has won the match max number of times all over the the 10 years and how many times
consitent_team<-sqldf('select Season, winner from ipl_fin group by Season')
consitent_team%>% group_by(winner)%>%summarise(count=n())%>%filter(count==max(count))

##Which team has won by maximum runs and minimum wickets
maxrun <-ipl_fin%>%filter(win_by_runs==max(win_by_runs))%>%select('winner','win_by_runs')
maxrun1<-sqldf('select distinct winner,win_by_runs from maxrun')
maxrun1
minwick <-ipl_fin%>% filter(win_by_wickets != 0) %>% filter(win_by_wickets == min(win_by_wickets)) %>%select('winner', 'win_by_wickets')
minwick1<-sqldf('select distinct winner,win_by_wickets from minwick')
minwick1


##Which player has been awarded player of the match maximum number of times 
pom<-ipl_fin %>% group_by(player_of_match) %>% summarise(pom_count = n()) %>%filter(pom_count ==max(pom_count))
pom


#Top batsman of the IPL within the years given
Top_bat<- del_fin %>% group_by(batsman)%>% summarise(runs=sum(batsman_runs)) %>% arrange(desc(runs))%>%top_n(5)
Top_bat

#create a graph on number of matches won by each team more than or equal to 10 
matchwon<-as.data.frame(table(ipl_fin$winner)) %>% filter(Freq >=10)
matchwon%>%arrange(desc(Freq))
matchwon <- rename(matchwon, Team = Var1 , matches_won = Freq)
mw<-sqldf('select  Team, matches_won from matchwon order by matches_won desc')
mw

mw_gg1<-ggplot(data=mw,aes(x=reorder(Team,matches_won),y=matches_won,fill=matches_won))+
  geom_bar(stat="identity")+labs(title="Fig 1 Team Vs Win count",x="Team",y="Total_Matches_won")+
  geom_text(aes(label = matches_won,y=matches_won+3.5))+coord_flip()
mw_gg1

#Histogram of win by runs
wr<-ggplot(data=ipl_fin,aes(win_by_runs))+geom_histogram(fill="Blue",binwidth = 25)
wr

#Find Total number of Matches played in different cities  and create a bar plot
cc<-sqldf('select city,winner,count(city) as matchcount from ipl_fin group by city order by matchcount desc')
cc
ggplot(ipl_fin[which(!is.na(ipl_fin$city)),],aes(city,fill=city,rm.na=T)) +geom_bar()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ylab("Number of Matches Played")+ggtitle("Total Matches in City")+
  guides(fill=FALSE)


###How is win by runs is distributed for the team based on the decision to first batting or fielding

win_through_tossdec<-sqldf('select winner,sum(win_by_runs) as total_runs,toss_decision from ipl_fin where win_by_runs !=0 group by winner')
win_through_tossdec
qplot(data = win_through_tossdec, x = winner, y =total_runs , facets = win_through_tossdec$toss_decision~.,fill = "win_by_runs") +
  geom_bar(stat = "identity", alpha = 10) +
  theme_bw() +
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=90, hjust=1))+
  labs(title = "Win by Batting Vs Fielding",
       subtitle = "Win_By_Runs",
       x = "winner",
       y = "total_runs" )



#####################################PART 3 , Focus on subset of a Dataset################################################

#Which Team have more number of 6s,4s and compare with the graphs of 6 and 4s of each team.
runs_team<-del_fin%>%select(batting_team ,batsman_runs) %>%filter(batsman_runs %in% c(6,4))

runs_team
s<-sqldf("select batting_team as Team ,count(batsman_runs) as sixes from 
         runs_team where batsman_runs = 6 group by batting_team")
s
f<-sqldf('select batting_team as Team , count(batsman_runs) as fours from runs_team 
      where batsman_runs = 4 group by batting_team')
f
run_t<-sqldf('select s.Team,s.sixes, f.fours from s
                           inner join f
                           on s.Team = f.Team')
run_t
run_sf<-melt(run_t,id.vars = c('Team'))
run_sf
run_sf_gg<-ggplot(data=run_sf,aes(x=Team,y=value,fill=as.factor(variable)))+
           geom_bar(stat="identity",position='dodge')+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_text(aes(label = value))
          
run_sf_gg


#IS TOSS a DECISION FACTOR,does winning the toss actually increases the chances of winning the game.
ipl_fin$toss<-ifelse(as.character(ipl_fin$toss_winner)==as.character(ipl_fin$winner),"Won","Lost")

ggplot(ipl_fin[which(!is.na(ipl_fin$toss)),],aes(toss, fill = toss))+ 
  geom_bar()+ xlab("Toss") +ylab("Number of matches won")+ ggtitle("Is Toss a Decision factor ")

#How many times one has won against other teams, consider 2 teams of your interest and calculate the winning counts.
#plot the treemap for the same for easy visualization
csk1 <- ipl_fin %>% 
  select(team1,team2,winner) %>% 
  filter(team1 != 'Chennai Super Kings' & winner == 'Chennai Super Kings' ) %>% 
  group_by(team1) %>% 
  summarise(c1 = n()) 

csk2 <- ipl_fin %>% 
  select(team1,team2,winner) %>% 
  filter(team2 != 'Chennai Super Kings' & winner == 'Chennai Super Kings' ) %>% 
  group_by(team2) %>% 
  summarise(c2 = n()) 

CSK <- merge(x = csk1 , y = csk2 ,by.x = 'team1' , by.y = 'team2',all = T ) ;
CSK[is.na(CSK)] <- 0
CSK <- rename(CSK , Team = team1)

CSK <- CSK %>% 
  mutate (CSK = c1 + c2 ) %>% 
  select(Team ,CSK)
CSK
#Tree map on how CSK win Treemap against other teams.
treemap(CSK ,                            
        index = "Team" ,                   
        vSize = "CSK",                   
        type = "index" ,                 
        palette = "Set1",                
        title = "CSK win Treemap against other Teams",
        fontsize.title=08,              
        #fontface.labels=c(2,4),
        fontface.labels="bold",
        bg.labels=c("transparent"),
        border.col=c("white"),            
        border.lwds=c(1,1),              
        fontsize.labels=c(8,0),          
        fontcolor.labels=c("black"),
        aspRatio= 0.8,
        fontfamily.title = "serif")

##Mumbai Indians
mi1 <- ipl_fin %>% 
  select(team1,team2,winner) %>% 
  filter(team1 != 'Mumbai Indians' & winner == 'Mumbai Indians' ) %>% 
  group_by(team1) %>% 
  summarise(m1 = n()) 

mi2 <- ipl_fin %>% 
  select(team1,team2,winner) %>% 
  filter(team2 != 'Mumbai Indians' & winner == 'Mumbai Indians' ) %>% 
  group_by(team2) %>% 
  summarise(m2 = n()) 

mi <- merge(x = mi1 , y = mi2 ,by.x = 'team1' , by.y = 'team2',all = T ) ;
mi[is.na(mi)] <- 0
mi<- rename(mi , Team = team1)
mi<- mi %>% 
  mutate(mi = m1 + m2 ) %>% 
  select(Team ,mi)
mi

treemap(mi ,                           
        index = "Team" ,                
        vSize = "mi",                   
        type = "index" ,                 
        palette = "Set1",                     
        title = "MI win treemap against other teams ", 
        fontsize.title=08,               
        fontface.labels="bold",
        bg.labels=c("transparent"),
        border.col=c("white"),           
        border.lwds=c(1,1),              
        fontsize.labels=c(8,0),         
        fontcolor.labels=c("black"),
        aspRatio= 0.8,
        fontfamily.title = "serif")


###Calculate the winning percentage of each team State1.Which team has 

mp1<-sqldf('select team1, count(team1) as T1count from ipl_fin group by team1')

mp2<-sqldf('select team2, count(team2) as t2count from ipl_fin group by team2')

mp <- merge(x = mp1 , y = mp2 ,by.x = 'team1' , by.y = 'team2',all = T ) ;
mpc<-sqldf('select team1 as team ,(T1count+T2count) as mpcount from mp group by team')
mpc
matwincount<-sqldf('select winner,count(winner) as wincount1 from ipl_fin group by winner')
matwincount

win_perc<-sqldf('select mpc.team,mpc.mpcount,mwc.winner,mwc.wincount1 
                from mpc left join matwincount mwc on mpc.team=mwc.winner')

win_perc$winp<-round((win_perc$wincount1/win_perc$mpcount),4)*100

win_perc<-na.omit(win_perc)%>%arrange(desc(winp))
win_perc

winper_plot<-ggplot(data=win_perc,aes(x=reorder(team,-winp),y=winp))+
             geom_point()+geom_segment(aes(x=team,xend=team,y=0,yend=winp))+
             theme(axis.text.x = element_text(angle = 90, hjust = 1))+
             labs(x='Team',y='WinPercentage',title='Team Winning Percentage')
             
winper_plot
           


########################################################END#####################################






