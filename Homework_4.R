#1) Plot the white mortality rate (Q_White) against the black mortality rate (Q_Black) at every single age from 0 to
#60 years old for men in 1970. The age should be plotted along the x-axis and the mortality rate should be plotted 
#along the y-axis. Then do the same for 2010. The x-axis and the y-axis should have the same range for both the 
#1970 plot and the 2010 plot. Compare the two plots and describe what you see. (1 point)

library(tidyverse)


life_long <- life %>% gather(Race, Rate, Q_Black:Q_White)

li <- life_long %>%
  filter(Gender == "male", Age < 61, Year == 1970 | Year == 2010) 

View(li)



View(li)


ggplot(li, aes(x = Age, y = Rate, colour = Race)) + 
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 60, 5), limits = c(0, 60)) +
  facet_grid(~Year) +
  scale_y_continuous(breaks = seq(0, 0.035, 0.0025),limits = c(0,0.035),labels = paste0(seq(0, 0.035, 0.0025))) +
  labs(x = "Age",y = "Mortaity Rate", colour = "Race",
       title = "1970 and 2010: Black and White Mortality Rate ages 0 to 60") +
  theme(plot.title = element_text(hjust = 0.5))



#2) What was the total population of 21-year-old black females in 1988. What was the mortality rate for this 
#population? What was the rate for white females of the same age for that year? (1 point)

bfpop <- life_long %>%
  filter(Year == 1988,Gender == "female",Age==21)

View(bfpop)

#Black Pop = 274,132
#Black Rate = 0.0008
#White Rate = 0.000497

#3) Multiplying the black mortality rate by the black population, estimate the total number of deaths among black 
#women in 1988 who were 21 years old. Now make that same estimate using the white mortality rate instead of the black
#mortality rate. Give the di???erence between the two numbers. Note that this is the total number of "excess deaths" 
#for that gender, age, and year (1 point).

sum2 <- function(blackrate,whiterate,blackpop){
  excess <- ((blackrate * blackpop) - (whiterate * blackpop))
}

a <- sum2(0.0008,0.000497,274132)



b <- 274132*0.0008

#Black Pop x Black Rate = 219.3

w <- 274132*0.000497

#Black Pop x White Rate = 136.24

excess <- b - w

#Excess Deaths = 83


#4) Compute the total number of excess deaths for every gender, age, and year and add this to the dataframe as a 
#variable called Excess. Now sum Excess by age. Sum the number of excess deaths by year. Plot the trend, where the 
#x-axis is the year and the y-axis is the total number of excess deaths. Provide proper labeling. And describe what 
#you observe from the plot. (1 point)

life2 <- life %>%
  mutate(BDeaths = Q_Black * BlackPop) %>%
  mutate(WDeaths = Q_White * BlackPop) %>%
  mutate(Excess = BDeaths - WDeaths)

library(data.table)

Life2 <- data.table(life2)

View(Life2)

age <- as.data.table(Life2)[, sum(Excess), by = .(Age)]

ageyear <- as.data.table(Life2)[, sum(Excess), by = .(Age,Year)]

year <- as.data.table(Life2)[, sum(Excess), by = .(Year)]

View(year)

ggplot(year, aes(x = Year, y = V1)) + 
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1970, 2010, 2), limits = c(1970, 2010)) +
  scale_y_continuous(breaks = seq(50000, 1000000, 5000),limits = c(50000,100000),
                     labels = paste0(seq(50000, 1000000, 5000))) +
  labs(x = "Year",y = "Excess Mortality", 
       title = "1970-2010: Excess Black Mortality Relative to White Mortality") +
  theme(plot.title = element_text(hjust = 0.5))

#Notable peak starting in 1982, one year after Reagan's election and the beginning of Reagon's implementation of 
#"Trickle Down" Economic Policies, the war on drugs, and more, and peaking in 1994, one year after Bill Clinton
#was first elected. There was a decline from 1994 to 1997, followed by 9 years of slight downward, put positive
#progressed. In 2006 to 2010 there were huge declines, from around 85,000 excess mortalities to just above
#70,000. 

#5) In question 3, you estimated the total number of excess deaths among black women who were 21 years old in 1988. 
#Now suppose those women who died in excess survived instead. Had they been exposed to white rates of mortality for 
#every year until their death, how many would have hypothetically survived until 2010? (2 points)

life3 <- life %>%
  filter(Gender == "female", Age == 21,Year>1987) %>%
  mutate(BDeaths = Q_Black * BlackPop) %>%
  mutate(WDeaths = Q_White * BlackPop) %>%
  mutate(excess = BDeaths - WDeaths) 

year <- 1988
age <- 21
gender <- "female"
excess <- 83.11

while(year < 2010 & age < 85) {
  age <- age + 1 
  year <- year + 1 
  q_white <- life$Q_White[life$Age == age & life$Year == year &
                            life$Gender == gender] 
  excess <- excess*(1-q_white)
}

#81.6453



#6) In question 5, you estimated the total number of hypothetical survivors among 21-year-old black women who died in
#excess in 1988. Now do this for every gender, age, and year in the dataset. How many hypothetical survivors are 
#there in total? In other words, among the black men and women who died between 1970 and 2010, how many would have 
#been alive in 2010 had they been exposed to white rates of mortality in every year of life? Assume that no 
#individual who died in excess lives past 85 years old.1 (2 points)



life6 <- life %>%
  mutate(BDeaths = Q_Black * BlackPop) %>%
  mutate(WDeaths = Q_White * BlackPop) %>%
  mutate(Excess = BDeaths - WDeaths)

l6 <- life6 %>%
  filter(Gender=="female")

View(l6)

f2 <- function (year,gender,age, excess) {
  while(year < 2010 & age < 85) {
    age <- age + 1
    year <- year + 1
    if (age < 85){
      q_w <- life6$Q_White[life6$Age == age & life6$Year == year &  life6$Gender==gender]
      excess <- (excess)*(1-q_w)
    } else {
      excess <- 0
    }
  }
  surv <- c(return(excess))
}


yrs <- c(1970:2010)
ages <- c(0:85)
femaleexcess <- l6$Excess

idx <- c(1:40)
idz <-c(1:86)
idj <- c(1:3485)


for (i in idx){
  yr <- yrs[i]
  for (z in idz){
    age <- ages[z]
    for (j in idj){
      excess <- femaleexcess[j]
      femalesurvivors <- f2(yr,"female",age,femaleexcess)
    }
  }
}

l6 <- life6 %>%
  filter(Gender=="male")

maleexcess <- l6$Excess



for (i in idx){
  yr <- yrs[i]
  for (z in idz){
    age <- ages[z]
    for (j in idj){
      excess <- maleexcess[j]
    malesurvivors <- f2(yr,"male",age,maleexcess)
    }
  }
}

surv <- c(femalesurvivors,malesurvivors)
life6["Survivors"] <- surv
View(life6)

q6 <- life6 %>%
  mutate(total = sum(Survivors))

#3,080,323 survivors


#7) How many hypothetical survivors would be of voting age in 2010? (2 points)


df2 <- q6 %>%
  filter(2010 - Year + age > 17) %>%
  mutate(total = sum(Survivors))


#1,565,501 would have been of voting age
