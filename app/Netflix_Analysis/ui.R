
library(shinydashboard)
source("global.R")

ui <- dashboardPage(skin = "red",
                    
                    dashboardHeader(title = "Dashboard Netflix",
                                    titleWidth = 350),
                    
                    dashboardSidebar(
                      width = 350,
                      sidebarMenu(
                        menuItem("First Charged and Ongoing Customers",
                                 tabName = "ongoing", 
                                 icon = icon("user-plus")),
                        menuItem("Gave Up and Canceled Customers",
                                 tabName = "canceled",
                                 icon = icon("user-slash"))
                      )
                    ),
                    
                    dashboardBody(
                      tags$head(tags$style(HTML('
                      .main-header .logo {
                       font-family: "Georgia", Times, "Times New Roman", serif;
                       font-weight: bold;
                       font-size: 24px;
                      }
                      '))),
                      tabItems(
                        tabItem(tabName = "ongoing",
                                fluidRow(
                                  box(width = 12,
                                      valueBoxOutput("signup_no", width = 3),
                                      valueBoxOutput("charged_prop", width = 3),
                                      valueBoxOutput("ongoing_prop", width = 3),
                                      valueBoxOutput("mean_watching_on", width = 3)
                                  )
                                ),
                                fluidRow(
                                  box(width = 12,
                                      plotlyOutput("line_ongoing"),
                                      height = 420)
                                ),
                                fluidRow(
                                  box(width = 4,
                                      plotlyOutput("line_variation")
                                  ),
                                  box(width = 4,
                                      plotOutput("charged_bar")
                                  ),
                                  box(width = 4,
                                      plotOutput("ongoing_bar")
                                  )
                                )
                        ),
                        tabItem(tabName = "canceled",
                                fluidRow(
                                  box(width = 12,
                                    valueBoxOutput("givenup_prop", width = 3),
                                    valueBoxOutput("canceled_prop", width = 3),
                                    valueBoxOutput("mean_watching_off", width = 3),
                                    valueBoxOutput("mean_days_in_month",width = 3)
                                  )
                                ),
                                fluidRow(
                                  box(width = 12,
                                    plotlyOutput("line_off")
                                  )
                                ),
                                fluidRow(
                                  box(width = 4,
                                    fluidRow(infoBoxOutput("mean_days_until_cancel", width = 14)),
                                    plotlyOutput("hist_off")
                                  ),
                                  box(width = 4,
                                      plotOutput("givenup_bar", height = 515)
                                  ),
                                  box(width = 4,
                                      plotOutput("canceled_bar", height = 515)
                                      )
                                )
                        )
                      )
                    )
                    
)


