library(tidyverse)

setwd("C:/Users/charl/Documents/qss17fdata/")

load(file = "Raleigh2.Rdata")


#Figure 3 

DF <- tbl_df(stops) %>%
  select(year,stopid,officerid, race, gender, age, AgeGroup, purpose, searchoccur, InFigure12) %>%
  mutate(b = ifelse(race == "Black",1,0)) %>%
  mutate(w = ifelse(race == "White",1,0)) %>%
  mutate(bt = sum(b)) %>%
  mutate(wt = sum(w))


df <- stops %>%
  group_by(race,purpose) %>%
  summarise(sum_search = sum(searchoccur)) %>%
  spread(key = race, value = sum_search)

df2 <- stops %>%
  group_by(race,purpose) %>%
  summarise(total_stops = n()) %>%
  spread(key = race, value = total_stops)

Whitetot <- df2$White

Blacktot <- df2$Black

df["Whitetot"] <- Whitetot
df["Blacktot"] <- Blacktot

df$purpose <- factor(df$purpose, levels = c("Seat Belt","Stop Light/Sign",
                                              "Speed Limit",
                                              "Vehicle Regulatory",
                                              "Safe Movement",
                                              "Vehicle Equipment",
                                              "Other Vehicle",
                                              "Investigation","Driving Impaired"))


df3 <- df %>%
  group_by(purpose) %>%
  mutate(blackper = (Black/Blacktot)*100) %>%
  mutate(whiteper = (White/Whitetot)*100) %>%
  mutate(per = ((blackper - whiteper)/(whiteper))*100) %>%
  select(per,purpose)


                      
ggplot(df3, aes(purpose, per)) +
  geom_bar(stat = "identity", fill = "black", width = .5) +
  scale_y_continuous(breaks = seq(-50, 250, 50), limits = c(-50,325),labels = paste0(seq(-50, 250, 50))) +
  labs(title = "Figure 3: Difference in Likelihood of Being Searched, by Race and Type of Stop",size = 10,x="",y="Percent Difference") +
  theme_bw() +
  coord_flip()


#Figure 5

DF$gender <- as.character(DF$gender)

df4 <- DF %>%
  select(race,gender,year,searchoccur) %>%
  group_by(gender,race,year) %>%
  summarise(total_stops = n())

View(df4)

df5 <- DF %>%
  select(race,gender,year,searchoccur,AgeGroup) %>%
  group_by(gender,race,year) %>%
  summarise(total_search = sum(searchoccur))

totalsearches <- df5$total_search


df6 <- df4 %>%
  ungroup() %>%
  group_by(gender,year) %>%
  filter(race != "Other") %>%
  spread(key = race,value=total_stops)


df7 <- df5 %>%
  ungroup() %>%
  group_by(gender,year) %>%
  filter(race != "Other") %>%
  spread(key = race,value=total_search)

Whitesearch <- df7$White

Blacksearch <- df7$Black

df6["Whitesearch"] <- Whitesearch
df6["Blacksearch"] <- Blacksearch



DF7$gender <- as.character(DF7$gender)


df7 <- df6 %>%
  mutate(blackper = (Blacksearch/Black)*100) %>%
  mutate(whiteper = (Whitesearch/White)*100) %>%
  mutate(persearch = ((blackper - whiteper)/(whiteper))*100) %>%
  select(persearch,year,gender)

df7$gender <- factor(df7$gender, levels = c("Male", "Female"),
                     labels = c("Men","Women"))

df7$gender <- as.character(df7$gender)

ggplot(df7, aes(x = year, y = persearch, group = gender)) + 
  geom_line(aes(linetype=gender),size = 2) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  scale_x_continuous(breaks = seq(2002, 2012, 2), labels = paste0(seq(2002, 2012, 2))) +
  scale_y_continuous(breaks = seq(-25, 250, 50),limits = c(-25,225),labels = paste0(seq(0, 250, 50))) +
  labs(x = "Year",y = "Percent Difference", colour = "",
       title = "Figure 5: Racial Differences in the Likelihood of Search by Gender") +
  theme_bw() +
  theme(legend.position="bottom",legend.title=element_blank(), legend.key.width = unit(3.75, "cm"),
        legend.background = element_rect(fill="white",size=0.5, linetype="solid",colour="black"))


  
                                         
#Figure 7 


df8 <- DF %>%
  filter(race != "Other") %>%
  select(race,gender,AgeGroup,searchoccur) %>%
  unite(racegender, race, gender) %>%
  group_by(racegender,AgeGroup) %>%
  summarise(total_stops = n())

df9 <- DF %>%
  filter(race != "Other") %>%
  select(race,gender,AgeGroup,searchoccur) %>%
  unite(racegender, race, gender) %>%
  group_by(racegender,AgeGroup) %>%
  summarise(total_searches = sum(searchoccur))

total_searches <- df9$total_searches

df8["total_searches"] <- total_searches

df10 <- df8 %>%
  group_by(AgeGroup) %>%
  mutate(searchrate = (total_searches / total_stops)*100) %>%
  select(AgeGroup,racegender,searchrate)

year_bins <- c("Less than 20","20-24","25-29","30-34","35-39","40-44","45-49","50+")

df10$AgeGroup <- factor(df10$AgeGroup, levels = year_bins)

racegender_bins <- c("Black_Male","White_Male","Black_Female","White_Female")
racegender_names <- c("Black Male","White Male","Black Female","White Female")

df10$racegender <- factor(df10$racegender, levels = racegender_bins,labels = racegender_names)


ggplot(df10, aes(x = AgeGroup, y = searchrate, width = 0.5)) + 
  geom_bar(stat = "identity",fill="black",color="black") +
  scale_y_continuous(breaks = seq(0, 8, 2),limits = c(0,7.8),labels = paste0(seq(0, 8, 2))) +
  labs(x = "",y = "", title = "Figure 7: Search Rates by Race, Gender, and Age Group") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.text = element_text(size=10)) +
  facet_grid(~racegender)


#Figure 12 


df11 <- DF %>%
  group_by(officerid) %>%
  filter(InFigure12!=FALSE,race!="Other") %>%
  group_by(officerid,race) %>%
  summarise(total_stops = n()) %>%
  spread(key = race, value = total_stops)

df12 <- DF %>%
  group_by(officerid) %>%
  filter(InFigure12!=FALSE,race!="Other") %>%
  group_by(officerid,race) %>%
  summarise(races_search = sum(searchoccur)) %>%
  spread(key = race, value = races_search) 


Whitetot <- df11$White

Blacktot <- df11$Black

df12["Whitetot"] <- Whitetot
df12["Blacktot"] <- Blacktot


df13 <- df12 %>%
  mutate(blackper = (Black/Blacktot)*100) %>%
  mutate(whiteper = (White/Whitetot)*100)

df13 <- df13 %>% mutate(cut = ifelse(((blackper / whiteper) >= 2) | ((whiteper / blackper) >= 2),
                                     "High_Discrepancy_Officer","Officer"))

df13$cut <- factor(df13$cut, levels = c("Officer", "High_Discrepancy_Officer"),
                     labels = c("Officer", "High Discrepancy Officer"))

ggplot(df13, aes(blackper, whiteper)) + 
  geom_point(aes(shape = cut),size = 2.5) + 
  theme_bw() + 
  ggtitle("Figure 12: Percent of White and Black Drivers Searched, by Officer") + 
  geom_abline(aes(intercept = 0, slope = 2, size = "Ratio 2:1")) + 
  geom_abline(aes(intercept = 0, slope = 1, size = "Ratio 1:1")) + 
  geom_abline(aes(intercept = 0, slope = 0.5, size = "Ratio 1:2")) +
  scale_y_continuous(name = "Percent of Whites Searched") + 
  scale_x_continuous(name = "Percent of Blacks Searched") + 
  scale_size_manual(values = c(1.5,0.75,0.85), name = "") + 
  scale_shape_manual(values = c(19,1), name = "") + 
  theme(legend.position="bottom", legend.box = "vertical", 
        legend.background = element_rect(colour = "black"), 
        legend.key.width = unit(4,"line"), legend.text = element_text(size = 10), 
        axis.text.y = element_text(angle = 90, hjust = .5, vjust = 1))
        


