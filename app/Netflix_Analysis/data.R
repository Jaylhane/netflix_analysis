# Datasets for Netflix Analysis 

# Librarys and theme_set

####Datasets for Netflix Analysis-----------------------------

library(tidyverse)
library(gghighlight)
library(lubridate)
library(ggrepel)
library(plotly)
library(chron)

theme_set(theme_minimal()+
            theme(axis.title.y = element_text(size = 8),
                  axis.title.x = element_text(size = 8)))

# Dataset

usage_table <- readxl::read_excel("craft.xlsx", sheet = "Usage Table")

costumer_table <- readxl::read_excel("craft.xlsx", sheet = "Customer Table")

netflix <- full_join(usage_table,costumer_table) %>% 
  select(-c(Customer_Name))%>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(Movie_Name = as.character(Movie_Name),
         Time_Watching = round(digits = 2,
                               difftime(End_Time,Start_Time,
                                        units = "hours")),
         Months_Until_Cancel = round((Signup_Date%--%Cancel_Date)/dmonths(1),2),
         Signup_Customer = ifelse(is.na(Signup_Date),0,1),
         Charged_Customers = ifelse(is.na(First_Charge_Date),0,1),
         Giveup_Customer = ifelse(is.na(First_Charge_Date),1,0),
         Canceled_Customers = ifelse(is.na(Cancel_Date),0,1),
         Ongoing_Customers = ifelse(Charged_Customers==1&is.na(Cancel_Date),1,0)
  ) %>% 
  relocate(Time_Watching, .after = End_Time) %>% 
  relocate(Data_available, .after= last_col()) 

rm(usage_table,costumer_table)

# Specific datasets for graphics and measures

Customers_Counts <- netflix %>% 
  group_by(Signup_Date) %>% 
  summarise(Signup_Customer_Count = sum(Signup_Customer),
            Giveup_Customer_Count=sum(Giveup_Customer),
            Charged_Customers_Count = sum(Charged_Customers),
            Canceled_Customers_Count = sum(Canceled_Customers),
            Ongoing_Customers_Count = sum(Ongoing_Customers))

Channel_Count <- netflix %>%
  group_by(Channel) %>% 
  summarise(Signup_Customers_Count=sum(Signup_Customer),
            Giveup_Customers_Count=sum(Giveup_Customer),
            Charged_Customers_Count = sum(Charged_Customers),
            Canceled_Customers_Count = sum(Canceled_Customers),
            Ongoing_Customers_Count = sum(Ongoing_Customers)) %>% 
  reshape2::melt(id.vars="Channel",
                 variable.name="Customers_Status",
                 value.name = "Qty")

Plan_Count <- netflix %>%
  group_by(Plan) %>% 
  summarise(Signup_Customers_Count=sum(Signup_Customer),
            Giveup_Customers_Count=sum(Giveup_Customer),
            Charged_Customers_Count = sum(Charged_Customers),
            Canceled_Customers_Count = sum(Canceled_Customers),
            Ongoing_Customers_Count = sum(Ongoing_Customers))%>% 
  reshape2::melt(id.vars="Plan",
                 variable.name="Customers_Status",
                 value.name = "Qty")

Movie_Genre_Count <- netflix %>%
  group_by(Movie_Genre) %>% 
  summarise(Signup_Customers_Count=sum(Signup_Customer),
            Giveup_Customers_Count=sum(Giveup_Customer),
            Charged_Customers_Count = sum(Charged_Customers),
            Canceled_Customers_Count = sum(Canceled_Customers),
            Ongoing_Customers_Count = sum(Ongoing_Customers)) %>% 
  na.omit()%>% 
  reshape2::melt(id.vars="Movie_Genre",
                 variable.name="Customers_Status",
                 value.name = "Qty")

Quantity_Evolution_Count <- netflix %>%
  group_by(Signup_Date) %>% 
  summarise(Signup_Customers_Count=sum(Signup_Customer),
            Giveup_Customers_Count=sum(Giveup_Customer),
            Charged_Customers_Count = sum(Charged_Customers),
            Canceled_Customers_Count = sum(Canceled_Customers),
            Ongoing_Customers_Count = sum(Ongoing_Customers)) %>% 
  reshape2::melt(id.vars="Signup_Date",
                 variable.name="Customers_Status",
                 value.name = "Qty")

Proportion_Evolution <- netflix %>% 
  mutate(Week_No=str_c(year(Signup_Date),"_",week(Signup_Date))) %>%
  group_by(Week_No) %>% 
  summarise(
    Charged_Customers_Count = sum(Charged_Customers),
    Ongoing_Customers_Count = sum(Ongoing_Customers),
    Canceled_Customers_Count = sum(Canceled_Customers))

Proportion_Evolution$Charged_Variation <- NA

Proportion_Evolution$Ongoing_Variation <- NA

Proportion_Evolution$Canceled_Variation <- NA

for( i in 2:length(Proportion_Evolution$Week_No)){
  
  Proportion_Evolution$Ongoing_Variation[i] = 
    (Proportion_Evolution$Ongoing_Customers_Count[i]/Proportion_Evolution$Ongoing_Customers_Count[i-1])-1
  
  Proportion_Evolution$Canceled_Variation[i] = 
    (Proportion_Evolution$Canceled_Customers_Count[i]/Proportion_Evolution$Canceled_Customers_Count[i-1])-1
  
  Proportion_Evolution$Charged_Variation[i] = 
    (Proportion_Evolution$Charged_Customers_Count[i]/Proportion_Evolution$Charged_Customers_Count[i-1])-1
}

rm(i)

Proportion_Evolution <- Proportion_Evolution %>% 
  reshape2::melt(id.vars="Week_No",
                 variable.name="Customers_Status",
                 value.name = "Qty") %>% 
  mutate(Qty = round(ifelse(Qty%in%c(NA,"Inf"),0,Qty),2))


