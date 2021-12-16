####################################################
##R ui script
##Ryan Hardy
###################################################

library(dplyr)
library(Lahman)
library(DT)

#modify the Teams object (creating new teams object)
teams <- Teams %>% filter(yearID > 1900, lgID == "NL" | lgID == "AL") %>% 
  mutate(winPercentage = W / G, runsPerGame = R / G, HRPerGame = HR / G, TB = H + X2B + 2 * X3B + 3 * HR, TBPerGame = TB / G, KPerGame = SO / G, KPerBB = SO / W, WHIP = (3 * (H + BB) / IPouts)) %>% 
  select(-c(franchID, divID, Rank, G, Ghome, W, L, DivWin, WCWin, LgWin, WSWin, name, park, attendance, BPF, PPF, teamIDBR, teamIDlahman45, teamIDretro))

###################################################
shinyUI(fluidPage(
  # Add title
  titlePanel("Visualizing Baseball Data Over Time"),
  fluidRow(
 
    sidebarLayout(
        sidebarPanel(
          # Add Slider
          sliderInput(inputId = "yearChosen", label = "Include Years in Range", min = 1901, max = 2020, value = c(1901, 2020), sep = ""),
          # Add statistic choices
          selectInput(inputId = "statChoices", label = "Statistic to Plot", choices = c("X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "HBP", "SF", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "winPercentage", "runsPerGame", "HRPerGame", "TBPerGame", "KPerGame", "KPerBB", "WHIP")),
          # Add checkbox for sorting by league
          checkboxInput(inputId = "leagueColor", label = "Color by League?"),
          # Add checkbox for adding trend line
          checkboxInput(inputId = "trendLine", label = "Add in trend across time?")
        ),
        mainPanel(
          # Add plot output
          plotOutput("pointPlot"),
          br(),
          # Add summary table
          DT::dataTableOutput("summary")
        )
    )
  )
))

