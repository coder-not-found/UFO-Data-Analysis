# Roll no 25
# Question Set 26

#Support your answers with code results and visualizations
#What areas of the country are most likely to have UFO sightings?
#Are there any trends in UFO sightings over time? Do they tend to be clustered or seasonal?
#Do clusters of UFO sightings correlate with landmarks, such as airports or government research centers?
#What are the most common UFO descriptions?


# ========================= Read Data ============================================
getwd() 

df <- read.csv('26.csv', stringsAsFactors = FALSE)
dim(df)
head(df, 5)


# =============================== Question ===================================

#What areas of the country are most likely to have UFO sightings?



state_counts <- df %>%
  filter(state != '')%>%
  count(city, state, sort=TRUE)%>%
  unite('location', -n, sep=',')


state_counts %>%
  filter(n > 90)%>%
  mutate(x = factor(location))%>%    
  ggplot(aes(x, n))+    
  geom_segment(aes(x=reorder(x,n), xend=x, y=0, yend=n), size=0.5)+    
  geom_point(color='blue', size=1)+
  
  coord_flip()+
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5))+
  xlab("Location (x)")+
  ylab("Count/Occurences (n)")+
  labs(title='Which areas most likely to have UFO sightings?')


# =========================  Question  ========================================

#Are there any trends in UFO sightings over time? Do they tend to be clustered or seasonal?

library(lubridate)

df$datetime <- mdy_hm(df$datetime)

df %>%
  mutate(datetime = floor_date(datetime, unit='1 year'))%>%
  group_by(datetime)%>%
  filter(datetime > '1939-01-01')%>%
  summarize(sight = n())%>%
  ggplot(aes(datetime, sight))+
  geom_line()+
  theme( axis.text = element_text(size= 7))+
  scale_x_datetime(date_breaks = '5 years', date_labels = '%Y') + theme(panel.background = element_rect(fill = NA)) +labs(title = "UFO Sightings Since 1939",x = "Date", y = "Sightings")
                                                              

df <- df %>%
  mutate(day = day(datetime),
         month = month(datetime),
         year = year(datetime),
         hour = hour(datetime))

df %>%
  mutate(month = factor(month), 
         day = factor(day))%>%
  filter(between(year, 1950, 1974))%>%
  group_by(year, month)%>%
  summarize(sight = n())%>%
  ggplot(aes(month, sight, group=year))+
  geom_line()+
  facet_wrap(~ year, ncol = 5, scales = 'free')+
  theme( axis.text = element_text(size= 7))+
  xlab("Months")+
  ylab("Sightings")+
  labs(title=' UFO sightings seasonal')

# =================================== Question =================================

df %>%
  count(shape, sort=TRUE)

p <-  ggplot(df,aes(x= shape))+
      theme( axis.text.y = element_text(size= 7))+
      coord_flip()+
      geom_bar(aes( fill= 'steelblue'))+
      ylab("Occurences")+
      xlab("Shape of the UFO")+
      labs(title=' Common UFO descriptions')

p




