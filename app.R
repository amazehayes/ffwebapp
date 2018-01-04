library(shiny)
library(shinydashboard)
library(DT)
library(rsconnect)

consistency_2017 <- read.csv("consistency_2017.csv")
consistency_2017 <- setNames(consistency_2017, c("player","team","pos","posrank","avg",
                                                 "std.dev","total","floor","ceiling","#top12",
                                                 "#13-24","#25-36","#rest",""))
consistency_2017_2 <- consistency_2017[,1:13]

weekly_data <- read.csv("Weekly Data.csv")

weekly_2016 <- read.csv("2016 Weekly Data.csv")
weekly_2015 <- read.csv("2015 Weekly Data.csv")
weekly_2014 <- read.csv("2014 Weekly Data.csv")
weekly_2013 <- read.csv("2013 Weekly Data.csv")
weekly_2012 <- read.csv("2012 Weekly Data2.csv")
weekly_2011 <- read.csv("2011 Weekly Data.csv")
weekly_2010 <- read.csv("2010 Weekly Data.csv")

weekly_2016_points <- weekly_2016[,1:29]
weekly_2016_rank <- weekly_2016[,c(1:5,31:51)]

weekly_2015_points <- weekly_2015[,1:29]
weekly_2015_rank <- weekly_2015[,c(1:5,31:51)]

weekly_2014_points <- weekly_2014[,1:29]
weekly_2014_rank <- weekly_2014[,c(1:5,31:51)]

weekly_2013_points <- weekly_2013[,1:29]
weekly_2013_rank <- weekly_2013[,c(1:5,31:51)]

weekly_2012_points <- weekly_2012[,1:29]
weekly_2012_rank <- weekly_2012[,c(1:5,31:51)]

weekly_2011_points <- weekly_2011[,1:29]
weekly_2011_rank <- weekly_2011[,c(1:5,31:51)]

weekly_2010_points <- weekly_2010[,1:29]
weekly_2010_rank <- weekly_2010[,c(1:5,31:51)]

weekly_2016_points <- setNames(weekly_2016_points, c("posrank","player","pos","team","G","1","2","3","4",
                                                     "5","6","7","8","9","10","11","12","13",
                                                     "14","15","16","17","total","avg","stddev",
                                                     "floor","ceiling","CV","COR"))
weekly_2016_rank <- setNames(weekly_2016_rank, c("posrank","player","pos","team","G","1","2","3","4",
                                                 "5","6","7","8","9","10","11","12","13",
                                                 "14","15","16","17","#1-12","#13-24",
                                                 "#25-36","#rest"))
weekly_2015_points <- setNames(weekly_2015_points, c("posrank","player","pos","team","G","1","2","3","4",
                                                     "5","6","7","8","9","10","11","12","13",
                                                     "14","15","16","17","total","avg","stddev",
                                                     "floor","ceiling","CV","COR"))
weekly_2015_rank <- setNames(weekly_2015_rank, c("posrank","player","pos","team","G","1","2","3","4",
                                                 "5","6","7","8","9","10","11","12","13",
                                                 "14","15","16","17","#1-12","#13-24",
                                                 "#25-36","#rest"))
weekly_2014_points <- setNames(weekly_2014_points, c("posrank","player","pos","team","G","1","2","3","4",
                                                     "5","6","7","8","9","10","11","12","13",
                                                     "14","15","16","17","total","avg","stddev",
                                                     "floor","ceiling","CV","COR"))
weekly_2014_rank <- setNames(weekly_2014_rank, c("posrank","player","pos","team","G","1","2","3","4",
                                                 "5","6","7","8","9","10","11","12","13",
                                                 "14","15","16","17","#1-12","#13-24",
                                                 "#25-36","#rest"))
weekly_2013_points <- setNames(weekly_2013_points, c("posrank","player","pos","team","G","1","2","3","4",
                                                     "5","6","7","8","9","10","11","12","13",
                                                     "14","15","16","17","total","avg","stddev",
                                                     "floor","ceiling","CV","COR"))
weekly_2013_rank <- setNames(weekly_2013_rank, c("posrank","player","pos","team","G","1","2","3","4",
                                                 "5","6","7","8","9","10","11","12","13",
                                                 "14","15","16","17","#1-12","#13-24",
                                                 "#25-36","#rest"))
weekly_2012_points <- setNames(weekly_2012_points, c("posrank","player","pos","team","G","1","2","3","4",
                                                     "5","6","7","8","9","10","11","12","13",
                                                     "14","15","16","17","total","avg","stddev",
                                                     "floor","ceiling","CV","COR"))
weekly_2012_rank <- setNames(weekly_2012_rank, c("posrank","player","pos","team","G","1","2","3","4",
                                                 "5","6","7","8","9","10","11","12","13",
                                                 "14","15","16","17","#1-12","#13-24",
                                                 "#25-36","#rest"))
weekly_2011_points <- setNames(weekly_2011_points, c("posrank","player","pos","team","G","1","2","3","4",
                                                     "5","6","7","8","9","10","11","12","13",
                                                     "14","15","16","17","total","avg","stddev",
                                                     "floor","ceiling","CV","COR"))
weekly_2011_rank <- setNames(weekly_2011_rank, c("posrank","player","pos","team","G","1","2","3","4",
                                                 "5","6","7","8","9","10","11","12","13",
                                                 "14","15","16","17","#1-12","#13-24",
                                                 "#25-36","#rest"))
weekly_2010_points <- setNames(weekly_2010_points, c("posrank","player","pos","team","G","1","2","3","4",
                                                     "5","6","7","8","9","10","11","12","13",
                                                     "14","15","16","17","total","avg","stddev",
                                                     "floor","ceiling","CV","COR"))
weekly_2010_rank <- setNames(weekly_2010_rank, c("posrank","player","pos","team","G","1","2","3","4",
                                                 "5","6","7","8","9","10","11","12","13",
                                                 "14","15","16","17","#1-12","#13-24",
                                                 "#25-36","#rest"))

yearly <- read.csv("Yearly Data.csv")

yearly_rank <- yearly[,1:17]
yearly_rank_current <- yearly_rank[1:971,]
yearly_rank_past <- yearly_rank[972:1457,]

yearly_points <- yearly[,c(1:3,19:26)]
yearly_points_current <- yearly_points[1:971,]
yearly_points_past <- yearly_points[972:1457,]

yearly_rank <- setNames(yearly_rank, c("player","team","pos","2017","2016","2015","2014","2013","2012",
                                       "2011","2010","#1","#top5","#1-12","#13-24","#25-36","#rest"))
yearly_points <- setNames(yearly_points, c("player","team","pos","2017","2016","2015","2014","2013","2012",
                                           "2011","2010"))
yearly_rank_past <- setNames(yearly_rank_past, c("player","team","pos","2017","2016","2015","2014","2013","2012",
                                                 "2011","2010","#1","#top5","#1-12","#13-24","#25-36","#rest"))
yearly_points_past <- setNames(yearly_points_past, c("player","team","pos","2017","2016","2015","2014","2013","2012",
                                                     "2011","2010"))
yearly_rank_current <- setNames(yearly_rank_current, c("player","team","pos","2017","2016","2015","2014","2013","2012",
                                                       "2011","2010","#1","#top5","#1-12","#13-24","#25-36","#rest"))
yearly_points_current <- setNames(yearly_points_current, c("player","team","pos","2017","2016","2015","2014","2013","2012",
                                                           "2011","2010"))

yearly_rank_2 <- yearly_rank[,4:11]
yearly_rank_2 <- setNames(yearly_rank_2,c("2017","2016","2015","2014","2013","2012","2011","2010"))
rownames(yearly_rank_2) <- yearly_rank$player
yeartran <- t(yearly_rank_2)



qbdata <- read.csv("QBdata.csv")
qbdata <- transform(qbdata,
                    Comp. = as.numeric(sub("%","",Comp.)),
                    TD. = as.numeric(sub("%","",TD.)),
                    INT. = as.numeric(sub("%","",INT.)),
                    Comp..1 = as.numeric(sub("%","",Comp..1)),
                    TD..1 = as.numeric(sub("%","",TD..1)),
                    X.Pass.TDs = as.numeric(sub("%","",X.Pass.TDs)),
                    INT..1 = as.numeric(sub("%","",INT..1)),
                    X.INTs = as.numeric(sub("%","",X.INTs)),
                    Comp..2 = as.numeric(sub("%","",Comp..2)),
                    TD..2 = as.numeric(sub("%","",TD..2)),
                    X.Pass.TDs.1 = as.numeric(sub("%","",X.Pass.TDs.1)),
                    INT..2 = as.numeric(sub("%","",INT..2)),
                    X.INT = as.numeric(sub("%","",X.INT)),
                    FP.from.Yards = as.numeric(sub("%","",FP.from.Yards)),
                    FP.from.TDs = as.numeric(sub("%","",FP.from.TDs)),
                    FP.from.Rushing = as.numeric(sub("%","",FP.from.Rushing)))
qbdata <- setNames(qbdata,c("Year","Player","Age","Season","Round","Overall",
                            "Team","HeadCoach","OffCoordinator","DefCoordinator","SOS",
                            "Oline","Games","PassAtt","PassComp","Comp%","PassYards","PassTDs","INTs",
                            "Att/G","Comp/G","YPA","YPG","TD/G","INT/G","TD/INT","TD%","INT%",
                            "RushAtt","RushYards","RushTDs","RAtt/G","RYards/G","TotalTDs",
                            "RZ.PassAtt<20","RZ.PassComp<20","RZ.Comp%<20","RZ.TDs<20","RZ.INTs<20",
                            "RZ.TDPer<20","%PassTDs<20","RZ.INT%<20","%INTs<20","RZ.TD/INT<20",
                            "RZ.PassAtt<10","RZ.PassComp<10","RZ.Comp%<10","RZ.TDs<10","RZ.INTs<10",
                            "RZ.TD%<10","%PassTDs<10","RZ.INT%<10","%INTs<10","RZ.TD/INT<10",
                            "FanPts","PPG","PPAtt","PosRank","OverRank","FPfromYards","FPfromTDs","FPfromRush",
                            "YardMonster","TDdepend","MobileQB"))

rbdata <- read.csv("RBdata.csv")
rbdata <- transform(rbdata,
                    TD. = as.numeric(sub("%","",TD.)),
                    Rec. = as.numeric(sub("%","",Rec.)),
                    TD..1 = as.numeric(sub("%","",TD..1)),
                    Total.TD. = as.numeric(sub("%","",Total.TD.)),
                    Rush.Att. = as.numeric(sub("%","",Rush.Att.)),
                    TD..2 = as.numeric(sub("%","",TD..2)),
                    X.Rush.TD = as.numeric(sub("%","",X.Rush.TD)),
                    Rush.Att..1 = as.numeric(sub("%","",Rush.Att..1)),
                    TD..3 = as.numeric(sub("%","",TD..3)),
                    X.Rush.TD.1 = as.numeric(sub("%","",X.Rush.TD.1)),
                    Rush.Att..2 = as.numeric(sub("%","",Rush.Att..2)),
                    TD..4 = as.numeric(sub("%","",TD..4)),
                    X.Rush.TD.2 = as.numeric(sub("%","",X.Rush.TD.2)),
                    Rec..1 = as.numeric(sub("%","",Rec..1)),
                    Target. = as.numeric(sub("%","",Target.)),
                    TD..5 = as.numeric(sub("%","",TD..5)),
                    X.Rec.TD = as.numeric(sub("%","",X.Rec.TD)),
                    Rec..2 = as.numeric(sub("%","",Rec..2)),
                    Target..1 = as.numeric(sub("%","",Target..1)),
                    TD..6 = as.numeric(sub("%","",TD..6)),
                    X.Rec.TD.1 = as.numeric(sub("%","",X.Rec.TD.1)),
                    X.Touches = as.numeric(sub("%","",X.Touches)),
                    TD..7 = as.numeric(sub("%","",TD..7)),
                    X.Total.TD = as.numeric(sub("%","",X.Total.TD)),
                    X.Touches.1 = as.numeric(sub("%","",X.Touches.1)),
                    X.Total.TD.1 = as.numeric(sub("%","",X.Total.TD.1)),
                    TD..8 = as.numeric(sub("%","",TD..8)),
                    FP.from.Rec = as.numeric(sub("%","",FP.from.Rec)),
                    FP.from.RuYards = as.numeric(sub("%","",FP.from.RuYards)),
                    FP.from.Total.TD = as.numeric(sub("%","",FP.from.Total.TD)))
rbdata <- setNames(rbdata,c("Year","Player","Age","Season","Round","Overall",
                            "Team","HeadCoach","OffCoordinator","DefCoordinator","SOS",
                            "Oline","Games","RushAtt","RushYards","YPC","RushTDs",
                            "RuAtt/G","RuYPG","RushTD%","Targets","Receptions","Reception%","RecYards",
                            "RecTDs","Targets/G","Receptions/G","YPR","RecYPG","RecTD%","Touches",
                            "TotalYards","TotalTDs","YPT","TotalYPG","TotalTD%",
                            "RZ.RushAtt<20","RZ.RushYards<20","RZ.RushTDs<20","RZ.%RushAtt<20",
                            "RZ.RushTD%<20","RZ.%RushTD<20","RZ.RushAtt<10","RZ.RushYards<10",
                            "RZ.RushTDs<10","RZ.%RushAtt<10","RZ.RushTD%<10","RZ.%RushTD<10",
                            "RZ.RushAtt<5","RZ.RushYards<5","RZ.RushTDs<5","RZ.%RushAtt<5",
                            "RZ.RushTD%<5","RZ.%RushTD<5","RZ.Targets<20","RZ.Receptions<20",
                            "RZ.Rec%<20","RZ.RecYards<20","RZ.RecTDs<20","RZ.%Targets<20",
                            "RZ.RecTD%<20","RZ.%RecTD<20","RZ.Targets<10","RZ.Receptions<10",
                            "RZ.Rec%<10","RZ.RecYards<10","RZ.RecTDs<10","RZ.%Targets<10",
                            "RZ.RecTD%<10","RZ.%RecTD<10","RZ.Touches<20","RZ.TotalYards<20",
                            "RZ.TotalTDs<20","RZ.%Touches<20","RZ.TotalTD%<20","RZ.%TotalTD<20",
                            "RZ.Touches<10","RZ.TotalYards<10","RZ.TotalTDs<10","RZ.%Touches<10",
                            "RZ.TotalTD%<10","RZ.%TotalTD<10","FanPts","PPG","PPTouch","PosRank",
                            "OverRank","FPfromRec","FPfromRuYards","FPfromTotalTDs","PPRMachine",
                            "YardMonster","TDdepend"))

wrdata <- read.csv("WRdata.csv")
wrdata <- transform(wrdata, 
                    Reception. = as.numeric(sub("%","",Reception.)),
                    Market.Share = as.numeric(sub("%","",Market.Share)),
                    Rec..TD. = as.numeric(sub("%","",Rec..TD.)),
                    Rush.TD. = as.numeric(sub("%","",Rush.TD.)),
                    Total.TD. = as.numeric(sub("%","",Total.TD.)),
                    Reception..1 = as.numeric(sub("%","",Reception..1)),
                    X.Targets = as.numeric(sub("%","",X.Targets)),
                    TD. = as.numeric(sub("%","",TD.)),
                    X.Rec.TD = as.numeric(sub("%","",X.Rec.TD)),
                    Team.Target. = as.numeric(sub("%","",Team.Target.)),
                    Reception..2 = as.numeric(sub("%","",Reception..2)),
                    TD..1 = as.numeric(sub("%","",TD..1)),
                    X.Rec.TD.1 = as.numeric(sub("%","",X.Rec.TD.1)),
                    X.Targets.1 = as.numeric(sub("%","",X.Targets.1)),
                    FP.from.Rec. = as.numeric(sub("%","",FP.from.Rec.)),
                    FP.from.Yards = as.numeric(sub("%","",FP.from.Yards)),
                    FP.from.TDs = as.numeric(sub("%","",FP.from.TDs)),
                    Team.Target..1 = as.numeric(sub("%","",Team.Target..1)))
wrdata <- setNames(wrdata,c("Year","Player","Age","Season","Round","Overall",
                            "Team","HeadCoach","OffCoordinator","DefCoordinator","SOS",
                            "Oline","Games","Targets","Receptions","Reception%","RecYards",
                            "RecTDs","Targets/G","MarketShare","Receptions/G","YPTarget","YPR",
                            "RecYPG","RecTD%","RushAtt","RushYards","RushTDs","RuAtt/G","RuYPG",
                            "RushTD%","TotalTDs","TotalTD%","RZ.Targets<20","RZ.Receptions<20",
                            "RZ.Rec%<20","RZ.RecTDs<20","RZ.%Targets<20","RZ.RecTD%<20",
                            "RZ.%RecTD<20","RZ.TeamTarget%<20","RZ.Targets<10","RZ.Receptions<10",
                            "RZ.Rec%<10","RZ.RecTDs<10","RZ.%Targets<10","RZ.RecTD%<10",
                            "RZ.%RecTD<10","RZ.TeamTarget%<10","FanPts","PPG","PPTarget","PosRank",
                            "OverRank","FPfromRec","FPfromRecYards","FPfromTDs","PPRMachine",
                            "YardMonster","TDdepend"))

tedata <- read.csv("TEdata2.csv",fileEncoding="latin1")
tedata <- transform(tedata, 
                    Reception. = as.numeric(sub("%","",Reception.)),
                    Market.Share = as.numeric(sub("%","",Market.Share)),
                    TD. = as.numeric(sub("%","",TD.)),
                    Reception..1 = as.numeric(sub("%","",Reception..1)),
                    X.Targets = as.numeric(sub("%","",X.Targets)),
                    TD..1 = as.numeric(sub("%","",TD..1)),
                    X.Rec.TD = as.numeric(sub("%","",X.Rec.TD)),
                    Team.Target. = as.numeric(sub("%","",Team.Target.)),
                    Reception..2 = as.numeric(sub("%","",Reception..2)),
                    X.Targets.1 = as.numeric(sub("%","",X.Targets.1)),
                    TD..2 = as.numeric(sub("%","",TD..2)),
                    X.Rec.TD.1 = as.numeric(sub("%","",X.Rec.TD.1)),
                    Team.Target..1 = as.numeric(sub("%","",Team.Target.)),
                    FP.from.Rec. = as.numeric(sub("%","",FP.from.Rec.)),
                    FP.from.Yards = as.numeric(sub("%","",FP.from.Yards)),
                    FP.from.TDs = as.numeric(sub("%","",FP.from.TDs)))
tedata <- setNames(tedata,c("Year","Player","Age","Season","Round","Overall",
                            "Team","HeadCoach","OffCoordinator","DefCoordinator","SOS",
                            "Oline","Games","Targets","Receptions","Reception%","RecYards",
                            "RecTDs","Targets/G","MarketShare","Receptions/G","YPTarget","YPR",
                            "RecYPG","RecTD%","RZ.Targets<20","RZ.Receptions<20",
                            "RZ.Rec%<20","RZ.RecTDs<20","RZ.%Targets<20","RZ.RecTD%<20",
                            "RZ.%RecTD<20","RZ.TeamTarget%<20","RZ.Targets<10","RZ.Receptions<10",
                            "RZ.Rec%<10","RZ.RecTDs<10","RZ.%Targets<10","RZ.RecTD%<10",
                            "RZ.%RecTD<10","RZ.TeamTarget%<10","FanPts","PPG","PPTarget","PosRank",
                            "OverRank","FPfromRec","FPfromRecYards","FPfromTDs","PPRMachine",
                            "YardMonster","TDdepend"))

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "Fantasy Stats"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("dashboard")),
      menuItem("Start/Sit Tool", tabName = "tool", icon = icon("wrench")),
      menuItem("Consistency Data", tabName = "consistency", icon = icon("table")),
      menuItem("Weekly Data", tabName = "weekly", icon = icon("table")),
      menuItem("Yearly Data", tabName = "yearly", icon = icon("table"),
               menuSubItem("Data", tabName = "yearlydata"),
               menuSubItem("Chart",tabName = "yearlychart")),
      menuItem("Database", tabName = "database", icon = icon("database"),
               menuSubItem("Quarterback", tabName = "data_qb"),
               menuSubItem("Running Back", tabName = "data_rb"),
               menuSubItem("Wide Receiver", tabName = "data_wr"),
               menuSubItem("Tight End", tabName = "data_te"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "welcome",
              h1("Welcome!"),  
              "Welcome to Fantasy Stats! This website aims to be your one-stop-shop for fantasy football data and statistics! Each tab contains different, sortable data for your fantasy football data needs. If you use the data, all we ask is you mention where you got it, from us!",
              br(),
              br(),
              strong("Start/Sit Tool"),
              ("- Compares two players based on percentage each player hits X amount of points, with graphs!"),
              br(),
              br(),
              strong("Consistency Data"),
              ("- Datatable of each player's consistency stats based on average and standard deviation, and more!"),
              br(),
              br(),
              strong("Weekly Data"),
              ("- Datatable of every player's weekly production (points and rank) since 2010."),
              br(),
              br(),
              strong("Yearly Data"),
              ("- Datatable of every player's yearly production (points and rank) since 2010."),
              br(),
              br(),
              strong("Database"),
              ("- A massive, user-controlled fantasy football database in which you control the stats you want to see from the players you want to see. "),
              br(),
              br(),
              "We are always looking to improve the site! If you notice any bugs or errors, or want to see other stats and data, message Addison Hayes (@amazehayes_roto) on Twitter or email ajh5737@gmail.com with suggestions, comments, or questions!",
              br(),
              br(),
              "Enjoy!"
              
      ),
      
      tabItem(tabName = "tool",
              fluidRow(column(6, selectInput("con_playerA", "Choose Player A:",
                                             unique(as.character(weekly_data$Player)),
                                             selected = "Aaron Rodgers")),
                       (column(6, selectInput("con_playerB", "Choose Player B:",
                                              unique(as.character(weekly_data$Player)),
                                              selected = "Drew Brees")))),
              fluidRow(column(6, numericInput("con_numberA", "Select Points Needed (Greater Than):",
                                              value = 20, min = 0, max = 50, step = 0.1)),
                       column(6, numericInput("con_numberB", "Select Points Needed (Greater Than):",
                                              value = 20, min = 0 , max = 50, step = 0.1))),
              fluidRow(column(6, verbatimTextOutput("probA")),
                       column(6, verbatimTextOutput("probB")),
                       fluidRow(column(6,plotOutput("con_graphA")),
                                column(6,plotOutput("con_graphB"))))),
      
      tabItem(tabName = "consistency",
              fluidRow(column(4,selectInput("pos","Position:",c("All",unique(
                as.character(consistency_2017_2$pos)))))),
              
              fluidRow(DT::dataTableOutput("consistency_2017_2"))),
      
      tabItem(
        tabName = "weekly",
        fluidRow(
          column(4, selectInput("weekly_year","Choose Year:",
                                c("2016","2015","2014","2013","2012","2011","2010"))),
          column(4, selectInput("tog_weekly","Choose Type:",
                                c("Points","Rank"))),
          column(4, selectInput("pos_weekly", "Position:",
                                c("All","QB","RB","WR","TE")))),
        fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("weekly"))),
      
      tabItem(tabName = "yearlydata",
              fluidRow(
                column(4, selectInput("tog_yearly","Choose Type:",
                                      c("Points","Rank"),selected = "Points")),
                column(4, selectInput("group_yearly", "Choose Player Group:",
                                      c("All", "Current", "Past"))),
                column(4, selectInput("pos_yearly", "Position:",
                                      c("All", unique(as.character(yearly$pos)))))),
              fluidRow(DT::dataTableOutput("yearly"))),
      
      tabItem(tabName = "yearlychart",
              fluidRow(
                column(4, selectInput("player_yearly","Choose Player:",
                                      c(unique(as.character(yearly$player)))))),
              fluidRow(
                column(8, plotOutput("yearly_graph")),
                column(4, tableOutput("yearly_posrank")))),
      
      tabItem(tabName = "data_qb", 
              fluidPage(
                fluidRow(
                  column(4, selectInput("qb_vars","Select Column(s):", choices = list(
                    Player = c("Year","Player","Age","Season","Games"),
                    Draft = c("Round","Overall"),
                    Team = c("Team","HeadCoach","OffCoordinator","DefCoordinator","SOS","Oline"),
                    Passing = c("PassAtt","PassComp","Comp%","PassYards","PassTDs","INTs",
                                "Att/G","Comp/G","YPA","YPG","TD/G","INT/G","TD/INT","TD%","INT%"),
                    Rushing = c("RushAtt","RushYards","RushTDs","RAtt/G","RYards/G","TotalTDs"),
                    RedZone20 = c("RZ.PassAtt<20","RZ.PassComp<20","RZ.Comp%<20","RZ.TDs<20","RZ.INTs<20",
                                  "RZ.TDPer<20","%PassTDs<20","RZ.INT%<20","%INTs<20","RZ.TD/INT<20"),
                    RedZone10 = c("RZ.PassAtt<10","RZ.PassComp<10","RZ.Comp%<10","RZ.TDs<10","RZ.INTs<10",
                                  "RZ.TD%<10","%PassTDs<10","RZ.INT%<10","%INTs<10","RZ.TD/INT<10"),
                    Fantasy = c("FanPts","PPG","PPAtt","PosRank","OverRank","FPfromYards","FPfromTDs",
                                "FPfromRush","YardMonster","TDdepend","MobileQB")
                  ),multiple = TRUE,selected = c("Year","Player","Team","Games","PassAtt","PassComp",
                                                 "Comp%","PassYards","PassTDs","INTs")))),
                fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("qbdata")))),
      
      tabItem(tabName = "data_rb",
              fluidPage(
                fluidRow(
                  column(4, selectInput("rb_vars","Select Column(s):", choices = list(
                    Player = c("Year","Player","Age","Season","Games"),
                    Draft = c("Round","Overall"),
                    Team = c("Team","HeadCoach","OffCoordinator","DefCoordinator","SOS","Oline"),
                    Rushing = c("RushAtt","RushYards","YPC","RushTDs","RuAtt/G","RuYPG","RuTD%"),
                    Receiving = c("Targets","Receptions","Reception%","RecYards","RecTDs","Targets/G",
                                  "Receptions/G","YPR","ReYPG","RecTD%"),
                    Total = c("TotalTDs","YPT","TotalYPG","TotalTD%"),
                    RedZone20 = c("RZ.RushAtt<20","RZ.RushYards<20","RZ.RushTDs<20","RZ.%RushAtt<20",
                                  "RZ.RushTD%<20","RZ.%RushTD<20","RZ.Targets<20","RZ.Receptions<20",
                                  "RZ.Rec%<20","RZ.RecYards<20","RZ.RecTDs<20","RZ.%Targets<20",
                                  "RZ.RecTD%<20","RZ.%RecTD<20","RZ.Touches<20","RZ.TotalYards<20",
                                  "RZ.TotalTDs<20","RZ.%Touches<20","RZ.TotalTD%<20","RZ.%TotalTD<20"),
                    RedZone10 = c("RZ.RushAtt<10","RZ.RushYards<10","RZ.RushTDs<10","RZ.%RushAtt<10",
                                  "RZ.RushTD%<10","RZ.%RushTD<10","RZ.Targets<10","RZ.Receptions<10",
                                  "RZ.Rec%<10","RZ.RecYards<10","RZ.RecTDs<10","RZ.%Targets<10",
                                  "RZ.RecTD%<10","RZ.%RecTD<10","RZ.Touches<10","RZ.TotalYards<10",
                                  "RZ.TotalTDs<10","RZ.%Touches<10","RZ.TotalTD%<10","RZ.%TotalTD<10"),
                    RedZone5 = c("RZ.RushAtt<5","RZ.RushYards<5","RZ.RushTDs<5","RZ.%RushAtt<5",
                                 "RZ.RushTD%<5","RZ.%RushTD<5"),
                    Fantasy = c("FanPts","PPG","PPTouch","PosRank","OverRank","FPfromRec",
                                "FPfromRuYards","FPfromTotalTD","PPRMachine","YardMonster","TDdepend")
                  ),multiple = TRUE,selected = c("Year","Player","Team","Games","RushAtt","RushYards",
                                                 "YPC","RushTDs")))),
                fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("rbdata")))),
      
      tabItem(tabName = "data_wr",
              fluidPage(
                fluidRow(
                  column(4, selectInput("wr_vars","Select Column(s):", choices = list(
                    Player = c("Year","Player","Age","Season","Games"),
                    Draft = c("Round","Overall"),
                    Team = c("Team","HeadCoach","OffCoordinator","DefCoordinator","SOS","Oline"),
                    Receiving = c("Targets","Receptions","Reception%","RecYards","RecTDs",
                                  "Targets/G","MarketShare","Receptions/G","YPTarget","YPR",
                                  "RecYPG","RecTD%"),
                    Rushing = c("RushAtt","RushYards","RushTDs","RuAtt/G","RuYPG","RushTD%"),
                    Total = c("TotalTDs","TotalTD%"),
                    RedZone20 = c("RZ.Targets<20","RZ.Receptions<20","RZ.Rec%<20","RZ.RecTDs<20",
                                  "RZ.%Targets<20","RZ.RecTD%<20","RZ.%RecTD<20","RZ.TeamTarget%<20"),
                    RedZone10 = c("RZ.Targets<10","RZ.Receptions<10","RZ.Rec%<10","RZ.RecTDs<10",
                                  "RZ.%Targets<10","RZ.RecTD%<10","RZ.%RecTD<10","RZ.TeamTarget%<10"),
                    Fantasy = c("FanPts","PPG","PPTarget","PosRank","OverRank","FPfromRec",
                                "FPfromRecYards","FPfromTDs","PPRMachine","YardMonster","TDdepend")
                  ),multiple = TRUE,selected = c("Year","Player","Team","Games","Targets","Receptions",
                                                 "Reception%","RecYards","RecTDs")))),
                fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("wrdata")))),
      
      tabItem(tabName = "data_te",
              fluidPage(
                fluidRow(
                  column(4, selectInput("te_vars","Select Column(s):", choices = list(
                    Player = c("Year","Player","Age","Season","Games"),
                    Draft = c("Round","Overall"),
                    Team = c("Team","HeadCoach","OffCoordinator","DefCoordinator","SOS","Oline"),
                    Receiving = c("Targets","Receptions","Reception%","RecYards","RecTDs",
                                  "Targets/G","MarketShare","Receptions/G","YPTarget","YPR",
                                  "RecYPG","RecTD%"),
                    RedZone20 = c("RZ.Targets<20","RZ.Receptions<20","RZ.Rec%<20","RZ.RecTDs<20",
                                  "RZ.%Targets<20","RZ.RecTD%<20","RZ.%RecTD<20","RZ.TeamTarget%<20"),
                    RedZone10 = c("RZ.Targets<10","RZ.Receptions<10","RZ.Rec%<10","RZ.RecTDs<10",
                                  "RZ.%Targets<10","RZ.RecTD%<10","RZ.%RecTD<10","RZ.TeamTarget%<10"),
                    Fantasy = c("FanPts","PPG","PPTarget","PosRank","OverRank","FPfromRec",
                                "FPfromRecYards","FPfromTDs","PPRMachine","YardMonster","TDdepend")
                  ),multiple = TRUE,selected = c("Year","Player","Team","Games","Targets","Receptions",
                                                 "Reception%","RecYards","RecTDs")))),
                fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("tedata"))))
      
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  #Print Welcome Tab
  output$welcometext <- ({
    renderText("")
  })
  
  #Print Consistency Datatable
  output$consistency_2017_2 <- DT::renderDataTable({
    DT::datatable({
      
      if(input$pos == "All") {
        consistency_2017_2
      }
      
      if(input$pos != "All") {
        consistency_2017_2 <- consistency_2017_2[consistency_2017$pos == input$pos,]
      }
      
    })
    
    consistency_2017_2 <- consistency_2017_2[order(consistency_2017_2$pos, consistency_2017_2$posrank),]
    
  }, rownames = FALSE, options = list(lengthMenu = c(12,24,36,50)))
  
  #Print Start/Sit Tool
  output$probA <- renderText({
    x <- as.matrix(weekly_data[,2:120])
    rownames(x) <- weekly_data$Player
    pointsA <- input$con_numberA
    count <- 0
    
    p1 <- x[(input$con_playerA),]
    
    for (i in 1:length(p1)) {
      if(p1[i] >= pointsA & !is.na(p1[i])) {
        count = sum(p1>=pointsA, na.rm = TRUE)
      }
    }
    a <- count/length(na.omit(p1))
    paste(signif(a, digits = 4)*100,"%")
  })
  
  output$probB <- renderText({
    x <- as.matrix(weekly_data[,2:120])
    rownames(x) <- weekly_data$Player
    pointsB <- input$con_numberB
    count <- 0
    
    p2 <- x[(input$con_playerB),]
    
    for (i in 1:length(p2)) {
      if(p2[i] >= pointsB & !is.na(p2[i])) {
        count = sum(p2>=pointsB, na.rm = TRUE)
      }
    }
    b <- count/length(na.omit(p2))
    paste(signif(b, digits = 4)*100,"%")
  })
  
  output$con_graphA <- renderPlot({
    x <- as.matrix(weekly_data)
    rownames(x) <- weekly_data$Player
    p1 <- x[(input$con_playerA),]
    p1 <- as.numeric(p1)
    hist(p1, main = paste("Histogram of", input$con_playerA), xlab = "Fantasy Points")
  })
  
  output$con_graphB <- renderPlot({
    x <- as.matrix(weekly_data)
    rownames(x) <- weekly_data$Player
    p2 <- x[(input$con_playerB),]
    p2 <- as.numeric(p2)
    hist(p2, main = paste("Histogram of", input$con_playerB), xlab = "Fantasy Points")
  })
  
  
  #Weekly Tab
  output$weekly <- DT::renderDataTable({
    DT::datatable({
      
      if(input$weekly_year == "2016" & input$tog_weekly == "Points" & input$pos_weekly == "All") {
        weekly <- weekly_2016_points
      }
      
      if(input$weekly_year == "2016" & input$tog_weekly == "Points" & input$pos_weekly != "All") {
        weekly <- weekly_2016_points
        weekly <- weekly_2016_points[weekly_2016_points$pos == input$pos_weekly,]
      }
      
      if(input$weekly_year == "2016" & input$tog_weekly == "Rank" & input$pos_weekly == "All") {
        weekly <- weekly_2016_rank
      }
      
      if(input$weekly_year == "2016" & input$tog_weekly == "Rank" & input$pos_weekly != "All") {
        weekly <- weekly_2016_rank
        weekly <- weekly_2016_rank[weekly_2016_rank$pos == input$pos_weekly,]
      }
      
      if(input$weekly_year == "2015" & input$tog_weekly == "Points" & input$pos_weekly == "All") {
        weekly <- weekly_2015_points
      }
      
      if(input$weekly_year == "2015" & input$tog_weekly == "Points" & input$pos_weekly != "All") {
        weekly <- weekly_2015_points
        weekly <- weekly_2015_points[weekly_2015_points$pos == input$pos_weekly,]
      }
      
      if(input$weekly_year == "2015" & input$tog_weekly == "Rank" & input$pos_weekly == "All") {
        weekly <- weekly_2015_rank
      }
      
      if(input$weekly_year == "2015" & input$tog_weekly == "Rank" & input$pos_weekly != "All") {
        weekly <- weekly_2015_rank
        weekly <- weekly_2015_rank[weekly_2015_rank$pos == input$pos_weekly,]
      }
      
      if(input$weekly_year == "2014" & input$tog_weekly == "Points" & input$pos_weekly == "All") {
        weekly <- weekly_2014_points
      }
      
      if(input$weekly_year == "2014" & input$tog_weekly == "Points" & input$pos_weekly != "All") {
        weekly <- weekly_2014_points
        weekly <- weekly_2014_points[weekly_2014_points$pos == input$pos_weekly,]
      }
      
      if(input$weekly_year == "2014" & input$tog_weekly == "Rank" & input$pos_weekly == "All") {
        weekly <- weekly_2014_rank
      }
      
      if(input$weekly_year == "2014" & input$tog_weekly == "Rank" & input$pos_weekly != "All") {
        weekly <- weekly_2014_rank
        weekly <- weekly_2014_rank[weekly_2014_rank$pos == input$pos_weekly,]
      }
      
      if(input$weekly_year == "2013" & input$tog_weekly == "Points" & input$pos_weekly == "All") {
        weekly <- weekly_2013_points
      }
      
      if(input$weekly_year == "2013" & input$tog_weekly == "Points" & input$pos_weekly != "All") {
        weekly <- weekly_2013_points
        weekly <- weekly_2013_points[weekly_2013_points$pos == input$pos_weekly,]
      }
      
      if(input$weekly_year == "2013" & input$tog_weekly == "Rank" & input$pos_weekly == "All") {
        weekly <- weekly_2013_rank
      }
      
      if(input$weekly_year == "2013" & input$tog_weekly == "Rank" & input$pos_weekly != "All") {
        weekly <- weekly_2013_rank
        weekly <- weekly_2013_rank[weekly_2013_rank$pos == input$pos_weekly,]
      }
      
      if(input$weekly_year == "2012" & input$tog_weekly == "Points" & input$pos_weekly == "All") {
        weekly <- weekly_2012_points
      }
      
      if(input$weekly_year == "2012" & input$tog_weekly == "Points" & input$pos_weekly != "All") {
        weekly <- weekly_2012_points
        weekly <- weekly_2012_points[weekly_2012_points$pos == input$pos_weekly,]
      }
      
      if(input$weekly_year == "2012" & input$tog_weekly == "Rank" & input$pos_weekly == "All") {
        weekly <- weekly_2012_rank
      }
      
      if(input$weekly_year == "2012" & input$tog_weekly == "Rank" & input$pos_weekly != "All") {
        weekly <- weekly_2012_rank
        weekly <- weekly_2012_rank[weekly_2012_rank$pos == input$pos_weekly,]
      }
      
      if(input$weekly_year == "2011" & input$tog_weekly == "Points" & input$pos_weekly == "All") {
        weekly <- weekly_2011_points
      }
      
      if(input$weekly_year == "2011" & input$tog_weekly == "Points" & input$pos_weekly != "All") {
        weekly <- weekly_2011_points
        weekly <- weekly_2011_points[weekly_2011_points$pos == input$pos_weekly,]
      }
      
      if(input$weekly_year == "2011" & input$tog_weekly == "Rank" & input$pos_weekly == "All") {
        weekly <- weekly_2011_rank
      }
      
      if(input$weekly_year == "2011" & input$tog_weekly == "Rank" & input$pos_weekly != "All") {
        weekly <- weekly_2011_rank
        weekly <- weekly_2011_rank[weekly_2011_rank$pos == input$pos_weekly,]
      }
      
      if(input$weekly_year == "2010" & input$tog_weekly == "Points" & input$pos_weekly == "All") {
        weekly <- weekly_2010_points
      }
      
      if(input$weekly_year == "2010" & input$tog_weekly == "Points" & input$pos_weekly != "All") {
        weekly <- weekly_2010_points
        weekly <- weekly_2010_points[weekly_2010_points$pos == input$pos_weekly,]
      }
      
      if(input$weekly_year == "2010" & input$tog_weekly == "Rank" & input$pos_weekly == "All") {
        weekly <- weekly_2010_rank
      }
      
      if(input$weekly_year == "2010" & input$tog_weekly == "Rank" & input$pos_weekly != "All") {
        weekly <- weekly_2010_rank
        weekly <- weekly_2010_rank[weekly_2010_rank$pos == input$pos_weekly,]
      }
      
    })
    
    weekly <- weekly[order(weekly$pos, weekly$posrank),]
  }, rownames = FALSE, options = list(lengthMenu = c(12,24,36,50)))
  
  #Yearly Data
  output$yearly <- DT:: renderDataTable({
    DT::datatable({
      
      if (input$tog_yearly == "Points" & input$group_yearly == "All" & input$pos_yearly == "All") {
        yearly <- yearly_points
      }
      
      if (input$tog_yearly == "Points" & input$group_yearly == "All" & input$pos_yearly != "All") {
        yearly <- yearly_points
        yearly <- yearly_points[yearly_points$pos == input$pos_yearly,]
      }
      
      if (input$tog_yearly == "Points" & input$group_yearly == "Current" & input$pos_yearly == "All") {
        yearly <- yearly_points_current
      }
      
      if(input$tog_yearly == "Points" & input$group_yearly == "Current" & input$pos_yearly != "All") {
        yearly <- yearly_points_current
        yearly <- yearly_points_current[yearly_points_current$pos == input$pos_yearly,]
      }
      
      if (input$tog_yearly == "Points" & input$group_yearly == "Past" & input$pos_yearly == "All") {
        yearly <- yearly_points_past
      }
      
      if(input$tog_yearly == "Points" & input$group_yearly == "Past" & input$pos_yearly != "All") {
        yearly <- yearly_points_past
        yearly <- yearly_points_past[yearly_points_past$pos == input$pos_yearly,]
      }
      
      if (input$tog_yearly == "Rank" & input$group_yearly == "All" & input$pos_yearly == "All") {
        yearly <- yearly_rank
      }
      
      if (input$tog_yearly == "Rank" & input$group_yearly == "All" & input$pos_yearly != "All") {
        yearly <- yearly_rank
        yearly <- yearly_rank[yearly_rank$pos == input$pos_yearly,]
      }
      
      if (input$tog_yearly == "Rank" & input$group_yearly == "Current" & input$pos_yearly == "All") {
        yearly <- yearly_rank_current
      }
      
      if(input$tog_yearly == "Rank" & input$group_yearly == "Current" & input$pos_yearly != "All") {
        yearly <- yearly_rank_current
        yearly <- yearly_rank_current[yearly_rank_current$pos == input$pos_yearly,]
      }
      
      if (input$tog_yearly == "Rank" & input$group_yearly == "Past" & input$pos_yearly == "All") {
        yearly <- yearly_rank_past
      }
      
      if(input$tog_yearly == "Rank" & input$group_yearly == "Past" & input$pos_yearly != "All") {
        yearly <- yearly_rank_past
        yearly <- yearly_rank_past[yearly_points_rank$pos == input$pos_yearly,]
      }
    })
    yearly <- yearly[order(yearly$pos),]
  }, rownames = FALSE, options = list(lengthMenu = c(12,24,36,50)))
  
  #Yearly Graph
  output$yearly_graph <- renderPlot({
    
    x <- as.matrix(yearly[,4:11])
    rownames(x) <- yearly$player
    p1 <- x[(input$player_yearly),]
    p2 <- names(p1)
    yrange <- range(1,p1, na.rm = TRUE)
    xrange <- range(p2, na.rm = FALSE)
    yrange2 <- ceiling(yrange[2]/5)*yrange[2]
    
    plot(na.omit(p1),type = "l",axes = FALSE, ylim = rev(range(yrange)),xlab = "Year",
         ylab = "Position Finish",main = paste("Yearly Finishes for", input$player_yearly))
    axis(1, at=1:8, lab=c("'17","'16","'15","'14","'13","'12","'11","'10"))
    axis(2, at=c(1,5*1:yrange[2]))
  })
  
  output$yearly_posrank <- renderTable({
    
    yeartran[,input$player_yearly]
    
  }, rownames = TRUE)
  
  #QB Database
  output$qbdata <- DT:: renderDataTable({
    DT::datatable(
      
      if(!is.null(input$qb_vars)) {
        qbdata[,input$qb_vars, drop = FALSE]
      }
      , rownames = FALSE,filter = "top" ,options = 
        list(lengthMenu = c(10,25,50,100))
      
    )
  })
  
  #RB Database
  output$rbdata <- DT:: renderDataTable({
    DT::datatable(
      
      if(!is.null(input$rb_vars)) {
        rbdata[,input$rb_vars, drop = FALSE]
      }
      , rownames = FALSE,filter = "top" ,options = 
        list(lengthMenu = c(10,25,50,100))
      
    )
  })
  
  #WR Database
  output$wrdata <- DT:: renderDataTable({
    DT::datatable(
      
      if(!is.null(input$wr_vars)) {
        wrdata[,input$wr_vars, drop = FALSE]
      }
      , rownames = FALSE,filter = "top" ,options = 
        list(lengthMenu = c(10,25,50,100))
      
    ) 
    
  })
  
  #TE Database
  output$tedata <- DT:: renderDataTable({
    DT::datatable(
      
      if(!is.null(input$te_vars)) {
        tedata[,input$te_vars, drop = FALSE]
      }
      , rownames = FALSE,filter = "top" ,options = 
        list(lengthMenu = c(10,25,50,100))
      
    ) 
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

