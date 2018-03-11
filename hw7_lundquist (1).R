#1) How many prisoners are estimated to be in state prisons? How many are estimated to be in federal
#prisons? (Use INSTITUTION to distinguish between state and federal prisoners. Use WT to make the
#estimate.)

library(tidyverse)

df <- inmates %>%
  select(INSTITUTION,WT) %>%
  na.omit() %>%
  mutate(state = ifelse(INSTITUTION == "State", WT, 0)) %>%
  mutate(feds = ifelse(INSTITUTION == "Federal", WT, 0)) %>%
  mutate(totalstate = sum(state), totalfed = sum(feds))
  summarise(totalstate = sum(state), totalfed = sum(feds))

#Total State = 1226171

#Total Federal = 129299

View(df)
  
#2) What percent of prisoners are recidivists? (use the HISTORY variable)

df2 <- inmates %>%
  select(HISTORY,WT) %>%
  na.omit() %>%
  mutate(rec = ifelse(HISTORY == "Recidivist", WT, 0)) %>%
  mutate(rec = sum(rec)) %>%
  mutate(total = sum(WT)) %>%
  mutate(recper = (rec/total))

#70%

#3) What percent of prisoners are black? What percent are Hispanic? What percent are white?

df3 <- inmates %>%
  select(RACE,WT) %>%
  na.omit() %>%
  mutate(white = ifelse(RACE == "White",WT,0)) %>%
  mutate(black = ifelse(RACE == "Black", WT, 0)) %>%
  mutate(hispanic = ifelse(RACE == "Hispanic", WT, 0)) %>%
  mutate(total = sum(WT)) %>%
  mutate(white = sum(white),black = sum(black),hispanic = sum(hispanic)) %>%
  mutate(white = white/total,black=black/total,hispanic=hispanic/total)

#Incarceration Demographics: Race
#White = 34.3%
#Black = 40.8%
#Hispanic = #18.8%
  

#4) Among those in federal prison, what percent are male? Among those in state prison, what percent are
#male?

df4 <- inmates %>%
  select(INSTITUTION,GENDER,WT) %>%
  na.omit() %>%
  filter(GENDER == "Male") %>%
  mutate(malefed = ifelse(INSTITUTION == "Federal", WT, 0)) %>%
  mutate(malefed = sum(malefed)) %>%
  mutate(malestate = ifelse(INSTITUTION == "State", WT, 0)) %>%
  mutate(malestate = sum(malestate))

df4 <- df4 %>% left_join(df) %>%
  mutate(malefed = malefed / totalfed, malestate = malestate / totalstate)

#Percentage of Prison Pop that is Male
#State = 93.2%
#Federal = 91.6%


  
#5) What percent of white prisoners under the age of 25 are serving a life or death sentence? What
#percent of black prisoners under the age of 25 are serving life or death sentences? (Use the variable
#LIFE_DEATH, which is true for those prisoners who are serving a life or death sentence)

inmates$AGE_CAT

df5 <- inmates %>%
  select(AGE_CAT,WT,RACE,LIFE_DEATH) %>%
  filter(AGE_CAT == "< 25 yrs",RACE == "White") %>%
  na.omit() %>%
  mutate(total = sum(WT)) %>%
  mutate(life = ifelse(LIFE_DEATH == T,WT,0)) %>%
  mutate(tlife = sum(life)) %>%
  mutate(lifeper = tlife/total)

#Total for whites = 1.52%


df6 <- inmates %>%
  select(AGE_CAT,WT,RACE,LIFE_DEATH) %>%
  filter(AGE_CAT == "< 25 yrs", RACE == "Black") %>%
  na.omit() %>%
  mutate(total = sum(WT)) %>%
  mutate(life = ifelse(LIFE_DEATH == T,WT,0)) %>%
  mutate(tlife = sum(life)) %>%
  mutate(lifeper = tlife/total)

#Total for blacks = 3.07%

#6) What percent of female prisoners are in prison for a violent crime? What is the percent for males? (Use
#the variable OFFENSE)

inmates$OFFENSE

df7 <- inmates %>%
  select(AGE_CAT,WT,GENDER,OFFENSE) %>%
  filter(GENDER == "Male") %>%
  na.omit() %>%
  mutate(total = sum(WT)) %>%
  mutate(violent = ifelse(OFFENSE == "VIOLENT",WT,0)) %>%
  mutate(tviolent= sum(violent)) %>%
  mutate(maleper = tviolent/total)

#Percent for men = 45.3%

df8 <- inmates %>%
  select(AGE_CAT,WT,GENDER,OFFENSE) %>%
  filter(GENDER == "Female") %>%
  na.omit() %>%
  mutate(total = sum(WT)) %>%
  mutate(violent = ifelse(OFFENSE == "VIOLENT",WT,0)) %>%
  mutate(tviolent = sum(violent)) %>%
  mutate(femaleper = tviolent/total)

#Percent for women = 27.2%

#7) Among Black men in prison younger than 25, what percent are serving time for a drug offense? What
#is the percent for whites? (Use the variable OFFENSE)

df9 <- inmates %>%
  select(AGE_CAT,WT,RACE,OFFENSE) %>%
  filter(AGE_CAT == "< 25 yrs",RACE == "White") %>%
  na.omit() %>%
  mutate(total = sum(WT)) %>%
  mutate(whitedrug = ifelse(OFFENSE == "DRUG",WT,0)) %>%
  mutate(whitedrug = sum(whitedrug)) %>%
  mutate(whiteper = whitedrug/total)

#Percent for whites = 16.8%

df10 <- inmates %>%
  select(AGE_CAT,WT,RACE,OFFENSE) %>%
  na.omit() %>%
  filter(AGE_CAT == "< 25 yrs",RACE == "Black") %>%
  mutate(total = sum(WT)) %>%
  mutate(blackdrug = ifelse(OFFENSE == "DRUG",WT,0)) %>%
  mutate(blackdrug = sum(blackdrug)) %>%
  mutate(blackper = blackdrug/total)

#Percent for blacks = 30.6%


#8) What is the mean length of sentence among black men younger than 25 in prison for drug possession?
#What is the mean length for whites? (Use the variable SENTENCEMTH for the length of sentence in
#months. Note that SENTENCEMTH has NAs. When calculating the mean, drop these NAs. Use the
#variable TYPEOFFENSE to identify drug possession offenses. Remember to use WT.)

inmates$TYPEOFFENSE

df11 <- inmates %>%
  select(AGE_CAT,WT,RACE,TYPEOFFENSE,SENTENCEMTH) %>%
  filter(RACE == "White",TYPEOFFENSE == "Drug possession",AGE_CAT == "< 25 yrs") %>%
  na.omit() %>%
  summarise(whitelengthmean = mean(SENTENCEMTH)) 

#mean sentence for whites found guilty of drug possession = 40.1 months

df12 <- inmates %>%
  select(AGE_CAT,WT,RACE,TYPEOFFENSE,SENTENCEMTH) %>%
  filter(RACE == "Black",TYPEOFFENSE == "Drug possession",AGE_CAT == "< 25 yrs") %>%
  na.omit() %>%
  summarise(blacklengthmean = mean(SENTENCEMTH)) 

#mean sentence for blacks found guilty of drug possession = 61.8

          
#9) Make the following barplot, which displays the percent of prisoners in prison for a drug offense, for a
#property offense, and for a violent offense by race. (HINT: Use geom_text(nudge_y = 2) to place the
#labels above the bar.)

inmates$OFFENSE

df13 <- inmates %>%
  select(RACE,OFFENSE,WT) %>%
  filter(RACE == "White") %>%
  mutate(drugs = ifelse(OFFENSE == "DRUG", WT, 0)) %>%
  mutate(violence = ifelse(OFFENSE == "VIOLENT",WT, 0)) %>%
  mutate(property = ifelse(OFFENSE == "PROPERTY",WT, 0)) %>%
  mutate(total = sum(WT)) %>%
  mutate(drugs = sum(drugs),violence = sum(violence),property = sum(property)) %>%
  mutate(DRUG = drugs/total, PROPERTY = property/total, VIOLENT = violence / total) %>%
  select(RACE, DRUG, PROPERTY, VIOLENT)

df13 <- df13[1,]


df14 <- inmates %>%
  select(RACE,OFFENSE,WT) %>%
  filter(RACE == "Black") %>%
  mutate(drugs = ifelse(OFFENSE == "DRUG", WT, 0)) %>%
  mutate(violence = ifelse(OFFENSE == "VIOLENT",WT, 0)) %>%
  mutate(property = ifelse(OFFENSE == "PROPERTY",WT, 0)) %>%
  mutate(total = sum(WT)) %>%
  mutate(drugs = sum(drugs),violence = sum(violence),property = sum(property)) %>%
  mutate(DRUG = drugs/total, PROPERTY = property/total, VIOLENT = violence / total) %>%
  select(RACE, DRUG, PROPERTY, VIOLENT)


df14 <- df14[1,]


df15 <- inmates %>%
  select(RACE,OFFENSE,WT) %>%
  filter(RACE == "Hispanic") %>%
  mutate(drugs = ifelse(OFFENSE == "DRUG", WT, 0)) %>%
  mutate(violence = ifelse(OFFENSE == "VIOLENT",WT, 0)) %>%
  mutate(property = ifelse(OFFENSE == "PROPERTY",WT, 0)) %>%
  mutate(total = sum(WT)) %>%
  mutate(drugs = sum(drugs),violence = sum(violence),property = sum(property)) %>%
  mutate(DRUG = drugs/total, PROPERTY = property/total, VIOLENT = violence / total) %>%
  select(RACE, DRUG, PROPERTY, VIOLENT)


df15 <- df15[1,]

df13 <- df13 %>% 
  gather(Offense, Rate, DRUG:PROPERTY:VIOLENT) 

df14 <- df14 %>% 
  gather(Offense, Rate, DRUG:PROPERTY:VIOLENT) 

df15 <- df15 %>% 
  gather(Offense, Rate, DRUG:PROPERTY:VIOLENT) 

library(data.table)

rate <- c(df13$Rate,df14$Rate,df15$Rate) 
race <- c(df13$RACE,df14$RACE,df15$RACE) 
offense <- c(df13$Offense,df14$Offense,df15$Offense) 


df16 <- data.frame(rate, race, offense) %>%
  mutate(rate = round(rate,digits = 2))

race_bins <- c("Black","Hispanic","White")


df16$race <- factor(df16$race, levels = race_bins)

df16 <- df16 %>%
  mutate(rate = rate * 100)


ggplot(df16, aes(x = offense, y = rate)) + 
  geom_bar(stat = "identity",fill="black",color="black") +
  facet_grid(~race) +
  scale_y_continuous(breaks = seq(0, 50, 5),limits = c(0,50),labels = paste0(seq(0, 50, 5), "%")) +
  labs(x = "Offense",y = "Percent (%)", title = "Percent of Prisoners in State and Federal Prisons by Race and Offense, 2004") +
  theme_bw() +
  geom_text(data=df16, mapping=aes(label= paste0(rate,"%")), nudge_y = 2)
            
           