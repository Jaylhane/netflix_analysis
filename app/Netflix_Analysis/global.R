# Global file with graphics and metrics 

source("data.R")

####Metrics---------------

Qty_Signeup <- sum(netflix$Signup_Customer)

Prop_Charged <- round((sum(netflix$Charged_Customers)/Qty_Signeup)*100,2)

Prop_Ongoing <- round((sum(netflix$Ongoing_Customers)/Qty_Signeup)*100,2)

df_charged_ongoing <- netflix %>% 
  filter(Charged_Customers==1|Ongoing_Customers==1)

Mean_Time_Watched_On <- round(mean(as.numeric(df_charged_ongoing$Time_Watching),na.rm = TRUE),2)

df_givenup_canceled <- netflix %>% 
  filter(Giveup_Customer==1|Canceled_Customers==1)

Prop_Givenup <- round((sum(netflix$Giveup_Customer)/Qty_Signeup)*100,2)

Prop_Canceled <- round((sum(netflix$Canceled_Customers)/Qty_Signeup)*100,2)

Mean_Time_Watched_Off <- round(mean(as.numeric(df_givenup_canceled$Time_Watching),na.rm = TRUE),2)

Days_Using_Until_Cancel <- data.frame(round(difftime(df_givenup_canceled$Cancel_Date,df_givenup_canceled$Signup_Date,units = "day"),2))

names(Days_Using_Until_Cancel) <- "Days_Using_Until_Cancel"

Mean_Days_Until_Cancel <- round(mean(as.numeric(Days_Using_Until_Cancel$Days_Using_Until_Cancel),na.rm = TRUE),2)

Mean_Days_In_Month <- round(Mean_Days_Until_Cancel/30,2)

# Graphics

#lower <- with(Quantity_Evolution_Count,as.POSIXct(strftime(min(Signup_Date),"%Y-%m-%d")))
#upper <- with(Quantity_Evolution_Count,as.POSIXct(strftime(as.Date(max(Signup_Date))+2,"%Y-%m-%d"))-1)

Line_Plot_Signup_and_Ongoing <- Quantity_Evolution_Count %>% 
  filter(Customers_Status%in%c("Signup_Customers_Count",
                               "Charged_Customers_Count",
                               "Ongoing_Customers_Count")) %>% 
  #mutate(label = if_else(Signup_Date == max(Signup_Date), as.character(Customers_Status), NA_character_)) %>% 
  ggplot(aes(x=Signup_Date, y=Qty, colour = Customers_Status, group=Customers_Status,
             text=paste("Customer Status:",sub(pattern = "_Customers_Count",replacement =" " ,x=Customers_Status),
                        "<br>Date",Signup_Date,
                        "<br>Frequency",Qty)))+
  geom_line(size=1)+
  scale_color_manual(values = c("lightskyblue3","palegreen3","olivedrab4"))+
  #scale_x_datetime(limits = c(lower,upper))+
  #geom_label_repel(aes(label = label),
  #                 nudge_x = 20,
  #                 direction = "x",
  #                 na.rm = TRUE)+
  labs(title = "Sign Up and Ongoing Customers Evolution",
       y="",
       x="Sign Up Date")+
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.x.bottom = element_text(vjust = 1))

Line_Plot_Proportion_Evolution <- Proportion_Evolution %>% 
  filter(Customers_Status%in%c("Charged_Variation",
                               "Ongoing_Variation",
                               "Canceled_Variation")) %>% 
  #mutate(label = if_else(Week_No == "2013_52", as.character(Customers_Status), NA_character_)) %>% 
  ggplot(aes(x=as.factor(Week_No), y=Qty, 
             group=Customers_Status, colour=Customers_Status,
             text=paste("Customer Status: ",sub(pattern = "_Variation",replacement =" " ,x=Customers_Status),
                        "<br>Week: ",Week_No,
                        "<br>Variation: ",Qty)))+
  geom_path(size=1)+
  scale_colour_manual(values = c("palegreen3",
                                 "olivedrab4",
                                 "salmon3"))+
  theme(legend.position = "none",
        plot.title = element_text(size=12))+
  #geom_label_repel(aes(label = label),
  #                 nudge_x = 0,
  #                 direction = "x",
  #                 na.rm = TRUE)+
  labs(title = "Proportional Variation from Sign Up,\nOngoing Customers and Canceled\nby Week",
       y="",
       x="Week Nº")+
  geom_hline(yintercept = 0)+
  ggExtra::removeGrid()

# conclusão: perdemos capacidade de retenção , suposições que podemos fazer: será que há algum número ideal de assinantes associado a nossa capacidade de retenção? o que influencia na retenção? 

Histogram_Days_Until_Cancelations <- Days_Using_Until_Cancel %>% 
  ggplot(aes(Days_Using_Until_Cancel,
             text=paste("Days until cancelation:",Days_Using_Until_Cancel)))+
  geom_histogram(bins = 30)+
  labs(title = "Histogram of days until cancellation",
       x="Number of days",
       y="Frequency")


Line_Plot_Givenup_and_Cancel <- Quantity_Evolution_Count %>% 
  filter(Customers_Status%in%c("Signup_Customers_Count","Giveup_Customers_Count","Canceled_Customers_Count")) %>% 
  ggplot(aes(x=Signup_Date, y=Qty, colour = Customers_Status, group=Customers_Status,
             text=paste("Customer Status: ",sub(pattern ="_Customers_Count" ,replacement =" " ,x = Customers_Status),
                        "<br>Date: ",Signup_Date,
                        "<br>Frequency: ",Qty)))+
  geom_line(size=1)+
  scale_color_manual(values = c("lightskyblue3",
                                "red4",
                                "salmon3"))+
  labs(title = "Give Up and Cancellation Evolution",
       y="",
       x="Sign Up Date")+
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.x.bottom = element_text(vjust = 1))

# Bars Plot Function

list_specific_dataset <- list(Channel_Count,Plan_Count,Movie_Genre_Count)

max_finder <- function(customer_status){
  
  max1 <- list_specific_dataset[[1]] %>% 
    filter(Customers_Status==customer_status) %>% 
    summarise_if(is.numeric, max) %>% 
    pull()
  
  max2 <- list_specific_dataset[[2]] %>% 
    filter(Customers_Status==customer_status) %>% 
    summarise_if(is.numeric, max)%>% 
    pull()
  
  max3 <- list_specific_dataset[[3]] %>% 
    filter(Customers_Status==customer_status) %>% 
    summarise_if(is.numeric, max)%>% 
    pull()
  
  upper <- max(max1,max2,max3)
  
  return(upper)
}

bar_plot <- function(specific_dataset, customer_status, plot_title, plot_subtitle){
  
  bar_plot <- specific_dataset %>% 
    filter(Customers_Status==customer_status) %>% 
    ggplot(aes(x=.data[[colnames(specific_dataset)[1]]],y=Qty,fill=Customers_Status))+
    geom_col(position = "dodge")+
    scale_fill_manual(values = "firebrick")+
    guides(fill="none")+
    labs(title=plot_title,
         subtitle = plot_subtitle,
         x="",
         y="")+
    gghighlight(max(Qty), max_highlight = 1)+
    ggExtra::removeGrid()+
    scale_y_continuous(limits=c(0,max_finder(customer_status)))+
    coord_flip()
  return(bar_plot)
}

arranged_bars_plot <- function(customer_status, plot_title){
  
  vector_plot_subtitle <- c("Channel","Plan","Movie Genre")
  
  channel_plot <- bar_plot(list_specific_dataset[[1]],customer_status, plot_title, vector_plot_subtitle[1])
  
  plan_plot <- bar_plot(list_specific_dataset[[2]],customer_status, "", vector_plot_subtitle[2])
  
  genre_plot <- bar_plot(list_specific_dataset[[3]],customer_status, "", vector_plot_subtitle[3])
  
  barplots=list(channel_plot,plan_plot,genre_plot)
  
  margin = theme(plot.margin = unit(c(0.01,0.01,0.01,0.01), "mm"))
  
  gridExtra::grid.arrange(grobs = lapply(barplots, "+", margin))
  
}




