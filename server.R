####################################################
##R server script
##Ryan Hardy
###################################################

library(dplyr)
library(ggplot2)
library(Lahman)
library(DT)

#modify the Teams object (creating new teams object)
teams <- Teams %>% filter(yearID > 1900, lgID == "NL" | lgID == "AL") %>% 
  mutate(winPercentage = W / G, runsPerGame = R / G, HRPerGame = HR / G, TB = H + X2B + 2 * X3B + 3 * HR, TBPerGame = TB / G, KPerGame = SO / G, KPerBB = SO / W, WHIP = (3 * (H + BB) / IPouts)) %>% 
  select(-c(franchID, divID, Rank, G, Ghome, W, L, DivWin, WCWin, LgWin, WSWin, name, park, attendance, BPF, PPF, teamIDBR, teamIDlahman45, teamIDretro))

#####################################################
#Basic syntax for a shiny server
shinyServer(function(input, output) {
  
  output$pointPlot <- renderPlot({
    # Create plot
    g <- ggplot(teams, aes(x = yearID, y = !!sym(input$statChoices))) + 
      labs(x = "Year") + 
      xlim(input$yearChosen)
    if(input$leagueColor == TRUE & input$trendLine == FALSE){
      # Sort by league
      g + 
        geom_point(aes(color = lgID))
    }
    else if(input$leagueColor == FALSE & input$trendLine == TRUE){
      # Add trend line
      g + 
        geom_point(color = 'black') + 
        geom_smooth(method = loess)
    }
    else if(input$leagueColor == TRUE & input$trendLine == TRUE){
      # Sort by league and add trend line
      g + 
        geom_point(aes(color = lgID)) + 
        geom_smooth(aes(group = lgID, color = lgID), method = loess)
    }
    else{
      # No league sort or trend line
      g + 
        geom_point(color = 'black')
    }
  })
  
  output$summary <- DT::renderDataTable({
    # Create table, group yearID's together and calculate mean, rounded to 2 decimals for the input statistic
    tab <- teams %>%
      group_by(yearID) %>% 
      summarize(Avg = round(mean(!!sym(input$statChoices)), 2))
  })
})