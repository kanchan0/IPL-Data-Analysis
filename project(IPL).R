#############  loading pakages needed for the project   #############

library(readr)
library(ggplot2) 
library(dplyr)
library(treemap)
library(tidyr)
library(tidyverse)


deliveries<-read.csv("C:/Users/Coolk/Desktop/R/R_Project/IPL/deliveries.csv")
matches<-read.csv("C:/Users/Coolk/Desktop/R/R_Project/IPL/matches.csv")
matches<-matches[matches$result=="normal",]

##########   Matches played in different cities  #########
ggplot(matches[which(!is.na(matches$city)),],aes(city,fill= city,rm.na=T)) +geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
    ylab("Number of Matches Played") +
    guides(fill=FALSE)



############  IS WINNING TOSS IN IPL HAS ADVANTAGE   ###########################
matches$toss_match<-ifelse(as.character(matches$toss_winner)==as.character(matches$winner),"Won","Lost")
ggplot(matches[which(!is.na(matches$toss_match)),],aes(toss_match, fill = toss_match))+ 
    geom_bar()+ xlab("Toss") +ylab("Number of matches won")+ ggtitle("How much of a advantage is winning the toss")





############# NUMBER OF MATCHES PLAYED BY EACH TEAM ################################

ggplot(as.data.frame(table(matches$team2) + table(matches$team1)),aes(reorder(Var1,-Freq),Freq,fill = Var1)) +
geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Player")+ylab("Number of Matches") +guides(fill=FALSE)


########### NUMBER OF MATCHES WON BY EACH TEAMS ###########################

ggplot(matches,aes(winner)) +geom_bar(fill="#0072B2") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Team")+
    ylab("Matches won")

########### IS HOME ADVANGE IS A REAL THING IN IPL HISTORY #############################

Data<-matches[matches$season!="2009",]
Data$date<- as.Date(Data$date)
Data1<-Data[Data$date < as.Date("2014-04-16") | Data$date > as.Date("2014-04-30"),]
Data1$home_team[Data1$city=="Bangalore"]<- "Royal Challengers Bangalore"
Data1$home_team[Data1$city=="Chennai"]<- "Chennai Super Kings"
Data1$home_team[Data1$city=="Delhi"]<- "Delhi Daredevils"
Data1$home_team[Data1$city=="Chandigarh"]<- "Kings XI Punjab"
Data1$home_team[Data1$city=="Jaipur"]<- "Rajasthan Royals"
Data1$home_team[Data1$city=="Mumbai"]<- "Mumbai Indians"
Data1$home_team[Data1$city=="Kolkata"]<- "Kolkata Knight Riders"
Data1$home_team[Data1$city=="Kochi"]<- "Kochi Tuskers Kerala"
Data1$home_team[Data1$city=="Hyderabad" & Data1$season <=2012]<- "Deccan Chargers"
Data1$home_team[Data1$city=="Hyderabad" & Data1$season >2012]<- "Sunrisers Hyderabad"
Data1$home_team[Data1$city=="Ahmedabad"]<- "Rajasthan Royals"
Data1$home_team[Data1$city=="Dharamsala"]<- "Kings XI Punjab"
Data1$home_team[Data1$city=="Visakhapatnam" & Data1$season== 2015]<- "Sunrisers Hyderabad"
Data1$home_team[Data1$city=="Ranchi" & Data1$season== 2013]<- "Kolkata Knight Riders"
Data1$home_team[Data1$city=="Ranchi" & Data1$season > 2013]<- "Chennai Super Kings"
Data1$home_team[Data1$city=="Rajkot" ]<- "Gujarat Lions"
Data1$home_team[Data1$city=="Kanpur" ]<- "Gujarat Lions"
Data1$home_team[Data1$city=="Raipur" ]<- "Delhi Daredevils"
Data1$home_team[Data1$city=="Nagpur" ]<- "Deccan Chargers"
Data1$home_team[Data1$city=="Indore" ]<- "Kochi Tuskers Kerala"
Data1$home_team[Data1$city=="Pune" & Data1$season!= 2016]<- "Pune Warriors"
Data1$home_team[Data1$city=="Pune" & Data1$season== 2016]<- "Rising Pune Supergiants"
Data1<-Data1[ which(!is.na(Data1$home_team)),]
Data1$win_host <- ifelse(as.character(Data1$winner)==as.character(Data1$home_team),"Home","Away")

ggplot(Data1[which(!is.na(Data1$win_host)),],aes(win_host,fill= win_host))+geom_bar()+
    ggtitle("Is home advantage a real thing in IPL?")+
    xlab("Team")+
    ylab("Number of Matches won")+labs(aesthetic="Winner")






################# WINNING PERCENTAGE OF EACH TEAM ###############################

matches_won<-as.data.frame(table(matches$winner))
colnames(matches_won)[2]<-"Won"
matches_played<-as.data.frame(table(matches$team2) + table(matches$team1))
colnames(matches_played)[2]<-"Played"

ggplot(left_join(matches_played,matches_won ),aes(reorder(Var1,-Won/Played),Won*100/Played,fill = Var1)) +geom_bar(stat = "identity")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Team")+
    ylab("Win Percentage") +  guides(fill=FALSE)+coord_cartesian(ylim = c(0, 100))


 
#####################  TOP BATSMAN IN IPL ################################################################

df<- deliveries %>% group_by(batsman)%>% summarise(runs=sum(batsman_runs)) %>% arrange(desc(runs)) %>%
    filter(runs > 3000) 
df %>% ggplot(aes(reorder(batsman,-runs),runs,fill=batsman)) +geom_bar(stat = "identity") +xlab("Batsman")+ 
    ylab("Runs")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    xlab("Player")+ ggtitle("Top Batsmen")+ guides(fill=F)

#########################   TOP BOWLER IN IPL TILL 2016  #################################
df<-deliveries %>% group_by(bowler) %>% filter(player_dismissed!="") %>% 
    summarise(wickets= length(player_dismissed)) %>% top_n(n=10,wt=wickets) 
df %>% ggplot(aes(reorder(bowler,-wickets),wickets,fill=bowler))+geom_bar(stat = "identity") +
    ylab("Wickets")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Player")+ ggtitle("Top Bowlers")+ 
    guides(fill=F)


##########################   TOTAL RUNS SCORED IN EACH OVER OF INNINGS ####################3

df <- deliveries %>% group_by(over) %>% filter(is_super_over==0) %>% summarise(Runs= sum(total_runs))
##print(df)
df %>% ggplot(aes(over,Runs,fill=over))+geom_bar(stat = "identity")+scale_x_continuous(breaks = 1:20)+ 
    guides(fill=F) +xlab("Over") + ylab("Total runs scored") +
    ggtitle("Total number of runs scored in each over of the innings")



###################  TOP STRIKER OF THE BALL  ###############################

deliveries %>% group_by(batsman) %>% filter(length(total_runs)>500) %>% 
    summarise(strike_rate= mean(batsman_runs)*100) %>% top_n(n=10,wt=strike_rate) %>%
    ggplot(aes(reorder(batsman,-strike_rate),strike_rate,fill=batsman))+ geom_bar(stat="identity")+ 
    xlab("Batsman") + ylab("Strike Rate") +
    ggtitle("top striker of ball",subtitle = "Minimum 500 balls faced")+
    theme(axis.text.x = element_text(angle = 75, hjust = 1)) + guides(fill=F)


########################  TOP RUN GETTERS AGAINST DIFFERENT TEAMS ##############

df<-deliveries %>% filter(batsman=="V Kohli"| batsman=="SK Raina" |batsman=="RG Sharma"|batsman=="G Gambhir")  %>% 
    group_by(batsman,bowling_team) %>% summarise(runs = sum(batsman_runs)) %>% filter(runs >100)
treemap(df, 
        index=c("batsman","bowling_team"), 
        vSize = "runs", 
        vColor = "bowling_team",
        type="categorical", #Type sets the organization and color scheme of your treemap
        palette = brewer.pal(12,"Set3"),  #Select your color palette from the RColorBrewer presets or make your own.
        fontsize.title = 15,
        fontfamily.title = "serif",
        fontfamily.labels = "symbol",
        title = "Runs against diff teams",
        aspRatio = 1,
        border.col="#FFFFFF",bg.labels = "#FFFFFF" ,fontcolor.labels= "black",fontsize.legend = 0
)


#################3  TYPE OF DISMISSAL OF TOP GETTERS  #############################################

df<-deliveries %>% 
    filter(player_dismissed=="V Kohli"| player_dismissed=="SK Raina" |player_dismissed=="RG Sharma"|player_dismissed=="G Gambhir") %>%
    group_by(player_dismissed,dismissal_kind) %>% summarise(type= length(dismissal_kind))

treemap(df, 
        index=c("player_dismissed","dismissal_kind"), 
        vSize = "type", 
        vColor = "dismissal_kind",
        type="categorical", 
        palette = brewer.pal(6,"Set2"),  
        fontsize.title = 15,
        fontfamily.title = "serif",
        fontfamily.labels = "italic",
        title = "Type of Dismissals ",
        aspRatio = 1,
        border.col="#FFFFFF",bg.labels = "black" ,fontcolor.labels= "#FFFFFF",fontsize.legend = 0)



######################### STRIKE RATE OF TOP GETTERS AT DIFFERENT STAGES OF THE GAME  #################3

deliveries %>% 
    filter(batsman=="V Kohli"| batsman=="SK Raina" |batsman=="RG Sharma"|batsman=="G Gambhir") %>% 
    group_by(batsman,over) %>% summarise(strike= mean(batsman_runs)*100) %>%  
    ggplot(aes(over,strike, col=batsman)) + geom_line(size=2) + ylab("Strike Rate") + 
    ggtitle("Strike rate in different stages of the game ") + scale_x_continuous(breaks = 1:20)




##########################  SEASON-WISE RUN COMPARISON   ####################################################

deliveries %>% left_join(matches) %>% 
    filter(batsman=="V Kohli"| batsman=="SK Raina" |batsman=="RG Sharma"|batsman=="G Gambhir") %>% 
    group_by(batsman,season) %>% summarise(runs = sum(batsman_runs)) %>%
    ggplot(aes(season,runs, col=batsman)) +geom_line(size= 1) + 
    ggtitle("Season wise run comparison of top getters") +
    scale_x_continuous(breaks = 2008:2016)



################################ RUN VS BALL FACES OF TOP SCORERES   #############################

deliveries %>%filter(batsman=="V Kohli"| batsman=="SK Raina" |batsman=="RG Sharma"|batsman=="G Gambhir") %>% 
    group_by(match_id) %>%
    mutate(cum_run= cumsum(batsman_runs),cum_ball=1:length(match_id)) %>%
    filter(player_dismissed=="V Kohli"|player_dismissed=="SK Raina" |player_dismissed=="RG Sharma"|player_dismissed=="G Gambhir") %>% 
    ggplot(aes(cum_ball,cum_run,col=batsman)) +geom_point() + xlab("Balls") +
    ylab("Runs")+ ggtitle("Runs scored vs balls faced in all the matches")

###################### INNING PROGRESSION BY PLAYER  ###############################################


deliveries %>% filter(batsman=="V Kohli") %>%
    group_by(match_id) %>% 
    mutate(cum_run= cumsum(batsman_runs),cum_ball=1:length(match_id)) %>%
    ggplot(aes(cum_ball,cum_run,col= as.factor(batsman_runs))) + 
    geom_point()  + labs(col="Type of Runs") + xlab("Balls") + ylab("Runs")+
    ggtitle("Innings Progression- Virat Kohli") +  coord_cartesian(ylim = c(0, 120)) 

deliveries %>% filter(batsman=="G Gambhir") %>%
    group_by(match_id) %>% 
    mutate(cum_run= cumsum(batsman_runs),cum_ball=1:length(match_id)) %>%
    ggplot(aes(cum_ball,cum_run,col= as.factor(batsman_runs))) + 
    geom_point()  + labs(col="Type of Runs") + xlab("Balls") + ylab("Runs")+
    ggtitle("Innings Progression- G Gambhir")+  coord_cartesian(ylim = c(0, 120)) 
deliveries %>% filter(batsman=="RG Sharma") %>%
    group_by(match_id) %>% 
    mutate(cum_run= cumsum(batsman_runs),cum_ball=1:length(match_id)) %>%
    ggplot(aes(cum_ball,cum_run,col= as.factor(batsman_runs))) + 
    geom_point()  + labs(col="Type of Runs") + xlab("Balls") + ylab("Runs")+
    ggtitle("Innings Progression- RG Sharma")+  coord_cartesian(ylim = c(0, 120)) 
deliveries %>% filter( batsman=="SK Raina" ) %>%
    group_by(match_id) %>% 
    mutate(cum_run= cumsum(batsman_runs),cum_ball=1:length(match_id)) %>%
    ggplot(aes(cum_ball,cum_run,col= as.factor(batsman_runs))) + 
    geom_point()  + labs(col="Type of Runs") + xlab("Balls") + ylab("Runs")+
    ggtitle("Innings Progression- SK Raina")+  coord_cartesian(ylim = c(0, 120))

