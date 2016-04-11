library(shiny)
library(ggplot2)
library(magrittr)
library(reshape2)
library(utils)
library(plyr)
library(dplyr)
library(tools)
library(scales)

# Define server logic for random distribution application
shinyServer(function(input, output) {


  
  # pre data loading
  teams <- read.csv("team.csv",na.strings=c("","NA"))
  recent_teams <- teams %>% filter(year > 1985)

  salary <- read.csv("salary.csv",na.strings=c("","NA"))
  
  salary_ttl_year <- salary %>%
    group_by(year, team_id) %>%
    summarise(ttl_salary = sum(salary)) %>%
    filter(year >= 1985)
  
  salary_f <- merge(salary_ttl_year, recent_teams[, c("team_id", "franchise_id")], by="team_id")
  salary_f <- unique(salary_f[,])
  
  #selectInput("teamddl","select a team",c(unique(recent_teams$franchise_id),"pick one"),"pick one")
  #make dynamic selection     
#   teamOptions <- sort(as.character(recent_teams[c(unique(recent_teams$franchise_id)), "name"]))
#   output$selectUI <- renderUI({ selectInput("team_select", "Test selection", teamOptions, selected=teamOptions[1]) })

  salary_avg_year <- salary_ttl_year %>%
    group_by(year) %>%
    summarise(ttl_salary = mean(ttl_salary, na.rm=TRUE)) %>%
    mutate(team_id = "Avg")
  salary_avg_year$franchise_id <- "Avg"
  
  salary_f <- rbind(salary_f, salary_avg_year)

  dollar_per_win <- recent_teams %>%
    left_join(., salary_f, by = c("year", "franchise_id")) %>%
    mutate(dollar_per_win = ttl_salary / w)
  dollar_per_win <- dollar_per_win[, c("year", "team_id.x", "dollar_per_win", "franchise_id")]
  dollar_per_win <- plyr::rename(x = dollar_per_win, replace = c("team_id.x"="team_id"))

  dollar_per_win_avg <- dollar_per_win %>%
  group_by(year) %>%
  summarise(dollar_per_win = mean(dollar_per_win, na.rm=TRUE)) %>%
    mutate(team_id = "Avg")
  dollar_per_win_avg$franchise_id <- "Avg"

  dollar_per_win <- rbind(dollar_per_win, dollar_per_win_avg)

  output$plot1 <- renderPlot({
    team <- input$selectTeam
    n <- input$n
    ggplot(recent_teams[recent_teams$franchise_id == team, ], aes(x = year, y = w)) + geom_bar(stat="identity") + coord_cartesian(ylim=c(30,130), xlim=c(1985, 2015))
    })      

  output$plot2 <- renderPlot({
    team <- input$selectTeam
    n <- input$n
    ggplot(NULL, aes(x = year, y = ttl_salary)) + geom_bar(stat="identity", aes(fill=franchise_id), data=salary_f[salary_f$franchise_id == team, ], alpha=0.5) + geom_bar(stat="identity", aes(fill=franchise_id), data=salary_f[salary_f$franchise_id == "Avg", ], alpha=0.5) + coord_cartesian(ylim=c(10000000,250000000), xlim=c(1985, 2015)) + scale_y_continuous(labels = dollar)     
    
    })

  output$plot3 <- renderPlot({
    team <- input$selectTeam
    n <- input$n
    ggplot(NULL, aes(x = year, y = dollar_per_win)) + geom_bar(stat="identity", aes(fill=franchise_id), data=dollar_per_win[dollar_per_win$franchise_id == team, ], alpha=0.5) + geom_bar(stat="identity", aes(fill=franchise_id), data=dollar_per_win[dollar_per_win$franchise_id == "Avg", ], alpha=0.5) + coord_cartesian(ylim=c(100000,2500000), xlim=c(1985, 2015)) + scale_y_continuous(labels = dollar)     
    
})

#g2 <- ggplot(dollar_per_win_oak, aes(y = dollar_per_win, x = year)) + geom_line()

})