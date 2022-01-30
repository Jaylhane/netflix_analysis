

library(shiny)
source("global.R")


shinyServer(function(input, output) {
  
  
  ####Charged and Ongoing Customers-------------
  
  output$line_ongoing <- renderPlotly({
    ggplotly(Line_Plot_Signup_and_Ongoing, tooltip = "text")
  })
  
  output$signup_no <- renderValueBox({
    valueBox(
      Qty_Signeup,"Sign Up", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$charged_prop <- renderValueBox({
    valueBox(
      paste0(Prop_Charged,"%"),"First charge on card", icon = icon("percent"),
      color = "green"
    )
  })
  
  output$ongoing_prop <- renderValueBox({
    valueBox(
      paste0(Prop_Ongoing,"%"),"Remains in plataform", icon = icon("percent"),
      color = "green"
    )
    
  })
  
  output$mean_watching_on <- renderValueBox({
    valueBox(
      Mean_Time_Watched_On,"Average time watched", icon = icon("tv"),
      color = "yellow"
    )
  })
  
  output$line_variation <- renderPlotly({
    ggplotly(Line_Plot_Proportion_Evolution, tooltip = "text")
  })
  
  output$charged_bar <- renderPlot({
    arranged_bars_plot(customer_status = "Charged_Customers_Count",
                       plot_title = "First Charged Customers by:")
  })
  
  output$ongoing_bar <- renderPlot({
    arranged_bars_plot(customer_status = "Ongoing_Customers_Count",
                       plot_title = "Ongoing Customers by:")
  })
  
  ####Givenup and Canceled Customers--------
  
  output$line_off <- renderPlotly({
    ggplotly(Line_Plot_Givenup_and_Cancel, tooltip = "text")
  })
  
  output$givenup_prop <- renderValueBox({
    valueBox(
      paste0(Prop_Givenup,"%"),"Gave Up", icon = icon("percent"),
      color = "red"
    )
  })
  
  output$canceled_prop <- renderValueBox({
    valueBox(
      paste0(Prop_Canceled,"%"),"Cancel", icon = icon("percent"),
      color = "red"
    )
    
  })
  
  output$mean_watching_off <- renderValueBox({
    valueBox(
      Mean_Time_Watched_Off,"Average time watched", icon = icon("tv"),
      color = "yellow"
    )
  })
  
  output$mean_days_in_month <- renderValueBox({
    valueBox(
      Mean_Days_In_Month,"Average months until cancellation", icon = icon("calendar-day"),
      color = "yellow"
    )
  })
  
  output$mean_days_until_cancel <- renderInfoBox({
    infoBox("Average days until cancellation",Mean_Days_Until_Cancel, icon = icon("user-clock"),
      color = "purple"
    )
  })
  
  output$hist_off <- renderPlotly({
    ggplotly(Histogram_Days_Until_Cancelations, tooltip = "text")
  })
  
  output$givenup_bar <- renderPlot({
    arranged_bars_plot(customer_status = "Giveup_Customers_Count",
                       plot_title = "Customer Gave Up by:")
  })
  
  output$canceled_bar <- renderPlot({
    arranged_bars_plot(customer_status = "Canceled_Customers_Count",
                       plot_title = "Canceled Customers by:")
  })
  
  
})
