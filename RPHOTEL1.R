library(dplyr)
library(tidyr)
library(ggplot2)
library(lattice)
library(lubridate)
library(psych)
library(DT)
library(tidyverse)

#import datasets
hotels1 = read.csv("hotels1.csv")
hotels2 = read.csv("hotels2.csv")
hotels3 = read.csv("hotels3.csv")
hotels4 = read.csv("hotels4.txt", sep = " ")

# Combine Datasets
hotels5 <- merge(hotels1, hotels2, by=c("reservation_id"), all = TRUE)
hotels5$company = "NA"

names(hotels5)[names(hotels5) == "Agent"] <- "agent"
names(hotels5)[names(hotels5) == "Reservation_Status"] <- "reservation_status"

names(hotels5)[names(hotels5) == "Company"] <- "company"


names(hotels5)[names(hotels5) == "Agent"] <- "agent"
names(hotels5)[names(hotels5) == "reservation_status"] <- "reservation_status"

head(hotels5)

#Ensuring that all hotel dataframes contain same column names to create 1 dataframe

head(hotels4)
colnames(hotels4)
colnames(hotels3)
colnames(hotels3) == colnames(hotels4)

compare.cols = colnames(hotels3) == colnames(hotels4)
compare.cols

compare.cols = colnames(hotels5) == colnames(hotels3)
compare.cols

length(hotels5)
colnames(hotels5)
colnames(hotels3)

hotels3[compare.cols]

colnames(hotels5) == colnames(hotels3)
colnames(hotels3) == colnames(hotels4)
colnames(hotels3) == colnames(hotels5)

hotelgrp <- rbind(hotels5, hotels3, hotels4)

hotelgrp$hotel=tolower(hotelgrp$hotel)

#Creating a column for arrival dates in MM/DD/YYY format

hotelgrp$arrival_date = " "


hotelgrp$arrival_date <- as.POSIXct( hotelgrp$arrival_date, format="%m/%d/%Y")

hotelgrp$arrival_date = paste(hotelgrp$arrival_date_month,  hotelgrp$arrival_date_day_of_month, hotelgrp$arrival_date_year )

hotelgrp <- hotelgrp %>% relocate(arrival_date, .before = arrival_date_week_number)


#check for missing data

library(mice)
md.pattern(hotelgrp)

#Recheck for any other missing data
colSums(is.na(hotelgrp))# It appears that the children column is missing data

#Attempting to fix the data that is missing

hotelgrp$children[is.na(hotelgrp$children)] <- 0
colSums(is.na(hotelgrp))


#convert values with characters into factors and perform exploratory analysis
hotelgrp<- hotelgrp%>%
  mutate(
    hotel=as.factor(hotel),
    is_canceled=as.factor(is_canceled),
    arrival_date=as.factor(arrival_date),
    country=as.factor(country),
    market_segment=as.factor(market_segment),
    distribution_channel=as.factor(distribution_channel),
    reserved_room_type=as.factor(reserved_room_type),
    assigned_room_type=as.factor(assigned_room_type),
    deposit_type=as.factor(deposit_type),
    customer_type=as.factor(customer_type),
    reservation_status=as.factor(reservation_status),
    reservation_status_date=as.factor(reservation_status_date),
    company=as.factor(company)
  )

#write data to csv
write.csv(hotelgrp,"C:\\Users\\Ruema\\Desktop\\hotelgrp.csv", row.names = FALSE)


#data exploration

head(hotelgrp)
summary(hotelgrp)
nrow(hotelgrp)  
ncol(hotelgrp)        


#checking for outliers

hotelgrp%>%
  filter(adr>1000)

#obtaining the average adr for weekend nights and weeknights

hotelgrp$weekend_adr <- hotelgrp$stays_in_weekend_nights * hotelgrp$adr
hotelgrp$weekday_adr <- hotelgrp$stays_in_week_nights * hotelgrp$adr

#Checking for summary stats on  the new columns for the adr for weekend and weekday stays

summary(hotelgrp$weekday_adr)
summary(hotelgrp$weekend_adr)

#Here is where we get the min,median,and mode for all numerical data within the frame
summary(hotelgrp)


# Taking a look at when more children were visiting the hotel and which hotels had more bookings with chidren

hotelgrp %>% # This data shows that there were morestays without children than with children 
  count(children)


hotelgrp %>%
  mutate(arrival_date_month = factor(arrival_date_month,
                                     levels = month.name
  )) %>%
  count(hotel, arrival_date_month, children) %>%
  group_by(hotel, children) %>%
  mutate(proportion = n / sum(n)) %>%
  ggplot(aes(arrival_date_month, proportion, fill = children)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~hotel, nrow = 2) +
  labs(
    x = NULL,
    y = "Amount of Hotel Stays",
    fill = NULL
  )


#Checking Monthly Cancelations
#There are more cancelations at the city hotel in 2016 

hotelgrp %>% ggplot(aes(x=arrival_date_month, fill=hotel)) + 
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("darkslategray", "darkolivegreen"),labels=c("City Hotel", "Resort Hotel")) +
  scale_y_continuous(name = "Bookings",labels = scales::comma) +
  guides(fill=guide_legend(title=NULL))  +
  facet_grid(arrival_date_year ~ .) + 
  theme(legend.position="bottom", axis.text.x=element_text(angle=0, hjust=1, vjust=0.5)) +
  scale_x_discrete(labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="", y="Bookings") +
  ggtitle("Monthly Cancelations")


hotelgrp%>% filter(hotel != "resort hotel") %>% arrange(is_canceled)


hotelgrp %>% # This data shows that there were morestays without children than with children 
  count(children)


#Liniar Regression for ADR

adrlinreg <- lm(adr ~ arrival_date_year, hotelgrp)
print(adrlinreg)

#Overall visualizaion of data via charts

weekendadr <- hotelgrp$weekend_adr
hist(weekendadr)

weekdayadr <- hotelgrp$weekday_adr
hist(weekdayadr)


#Creating a Histogram for Yearly Bookings
hist(hotelgrp$arrival_date_year, prob = T, main = "Histogram of Yearly Hotel Reservations", xlab = "Yearly Reservations")
# The majority of bookings were in 2016
