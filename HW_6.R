library(tidyverse)
library(plotly)
library(base)
library(ggplot2)
setwd("C:/Users/charl/Documents/qss17fdata/")

#Figure 1

df <- nov %>%
  group_by(year) %>%
  mutate(voters = ifelse(votereg == "Voted",wt,0)) %>%
  summarise(totalpop = sum(wt), na.rm = T, voters = sum(voters),na.rm = T) %>%
  mutate(participation = (voters / totalpop) * 100)



ggplot(df, aes(year, participation)) +
  geom_line() +
  scale_y_continuous(breaks = seq(35, 80, 05), limits = c(35,80),labels = paste0(seq(35, 80, 5))) +
  scale_x_continuous(breaks = seq(1996, 2012, 4), limits = c(1995,2013),labels = paste0(seq(1996, 2012, 4))) +
  labs(title = "Figure 1: Voter Turnout in Presidential Elections, 1996 ??? 2012",x="",y="Percent Voted") +
  theme_bw() + 
  geom_label(aes(label = round(participation, digits=1)),nudge_x = 0, nudge_y = 0) +
  geom_hline(yintercept=50, linetype="dashed", 
             color = "black", size=0.75)


#Figure 2

df2 <- nov %>%
  filter(race != "other") %>%
  group_by(year,race) %>%
  mutate(voters = ifelse(votereg == "Voted",wt,0)) %>%
  summarise(totalpop = sum(wt), na.rm = T, voters = sum(voters),na.rm = T) %>%
  mutate(participation = ((voters / totalpop)*100))

race_bins <- c("white","black","hispanic")

df2$race <- factor(df2$race, levels = race_bins, labels = c("White","Black","Hispanic"))

df2 <- df2 %>% 
  mutate(Race = print(race))

ggplot(df2, aes(x = year, y = participation, group = Race)) +
  geom_line(aes(linetype = Race), size = 0.75) +
  scale_linetype_manual(values = c("solid", "dashed", "longdash")) +
  scale_y_continuous(breaks = seq(35, 80, 05), limits = c(35,80),labels = paste0(seq(35, 80, 5))) +
  scale_x_continuous(breaks = seq(1996, 2012, 4), limits = c(1996,2012),labels = paste0(seq(1996, 2012, 4))) +
  labs(title = "Figure 2: Voter Turnout in Presidential Elections By Race, 1996 ??? 2012",x="",y="Percent Voted") +
  theme_bw() + 
  theme(legend.position = c(0.1,0.91), legend.box = "vertical", 
        legend.background = element_rect(colour = "black"), 
        legend.key.width = unit(4,"line"), legend.text = element_text(size = 10)) +
  geom_label(aes(label = round(participation, digits=1)),nudge_x = 0, nudge_y = 0) +
  geom_hline(yintercept=50, linetype="dashed", 
             color = "black", size=0.75)


#Figure 3

df3 <- nov %>%
  filter(race != "other") %>%
  group_by(year,race, sex) %>%
  mutate(voters = ifelse(votereg == "Voted",wt,0)) %>%
  summarise(totalpop = sum(wt), na.rm = T, voters = sum(voters),na.rm = T) %>%
  mutate(participation = (voters / totalpop)*100)

race_bins <- c("white","black","hispanic")

df3$race <- factor(df3$race, levels = race_bins, labels = c("White","Black","Hispanic"))

df3 <- df3 %>%
  mutate(Sex = print(sex))
ggplot(df3, aes(x = year, y = participation, group = Sex)) +
  geom_line(aes(linetype = Sex), size = 0.75) +
  scale_linetype_manual(values = c("solid", "dashed", "longdash")) +
  scale_y_continuous(breaks = seq(35, 80, 05), limits = c(35,80),labels = paste0(seq(35, 80, 5))) +
  scale_x_continuous(breaks = seq(1996, 2012, 4), limits = c(1995,2013),labels = paste0(seq(1996, 2012, 4))) +
  labs(title = "Figure 3: Voter Turnout in Presidential Elections By Race and Sex, 1996 ??? 2012",x="",y="Percent Voted") +
  theme_bw() + 
  geom_label(aes(label = round(participation, digits=1)),nudge_x = 0, nudge_y = 0) +
  geom_hline(yintercept=50, linetype="dashed", 
             color = "black", size=0.75) +
  facet_grid(~race) + 
  theme(legend.position = c(0.9,0.9), legend.box = "vertical", 
        legend.background = element_rect(colour = "black"), 
        legend.key.width = unit(4,"line"), legend.text = element_text(size = 10))


#Figure 4

df <- nov %>%
  filter(year==2008, race != "hispanic") %>%
  filter(race != "other") %>%
  select(wt, agegroup, sex, race, votereg) %>%
  group_by(agegroup,race,sex) %>%
  mutate(voters = ifelse(votereg == "Voted",wt,0)) %>%
  summarise(totalpop = sum(wt), na.rm = T, voters = sum(voters),na.rm = T) 
  
  

year_bins <- c("18 to 24","25 to 34","35 to 44","45 to 54","55 to 64","65+")


df$agegroup <- factor(df$agegroup, levels = year_bins)

jail$Age <- factor(jail$Age, levels = year_bins)


df2 <- df %>% left_join(jail, by = c("agegroup" = "Age", "race" = "Race", "sex" = "Sex"))




DF <- df2 %>%
  mutate(voters = as.integer(voters)) %>%
  mutate(rate = (voters / totalpop) * 100) %>%
  mutate(ratewithconv = (voters / (jailed + totalpop)*100)) 
           


vote_long <- DF %>% gather(inclusionstatus, partper, rate:ratewithconv) %>%
  mutate(Race = print(race))

lab <- c("Excluding Incarcerated","Including Incarcerated")

vote_long$inclusionstatus <- factor(vote_long$inclusionstatus, levels = c("rate", "ratewithconv"),
                   labels = c("Excluding Incarcerated", "Including Incarcerated"))

vote_long$Race <- factor(vote_long$Race, levels = c("black","white"),
                   labels = c("Black","White"))

ggplot(vote_long, aes(x = agegroup, y = partper, group = Race)) +
  geom_line(aes(linetype = Race), size = 0.01) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_y_continuous(breaks = seq(40, 80, 5), limits = c(39,81),labels = paste0(seq(40, 80, 5))) +
  labs(title = "Figure 4: Comparing Turnout in the 2008 Presidential Election Before and After Adjusting
to Include Incarcerated Men and Women (By Age Group, Sex and Race)",x="",y="Percent Voted") +
  theme_bw() +
  geom_label(aes(label = round(partper, digits=1),check_overlap = TRUE,label.size = 0.15)) +
  geom_hline(yintercept=50, linetype="dashed", 
             color = "black", size=0.5) +
  facet_grid(sex~inclusionstatus) +
  theme(legend.position = c(0.6,0.4))

 