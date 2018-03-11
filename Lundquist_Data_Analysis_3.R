#1) Let's get a sense of how many households are in the sample first. Calculate the number of households
#sampled in each survey year. Print the number you calculate for each year. Then, for 2015 only,
#calculate the number of households sampled in each state. Print the number you calculate for the states
#with the five most households sampled. Then do the same for the states with the five fewest households
#sampled.

names(cps)

cps2 <- cps %>%
  group_by(year) %>%
  summarise(N = length(year))

print(cps2)

df1 <- cps %>%
  group_by(state) %>%
  filter(year == 2015) %>%
  summarise(N2 = length(state))

require(data.table)

dt1 <- data.table(df1,key = "N2")

dt1high <- dt1[c(51:47),]

dt1low <- dt1[c(1:5),]

print(dt1high)

print(dt1low)

#2) For any survey year, are there states that did not produce a single household headed by a black college
#graduate? If so, which states?


df22 <- cps %>%
  mutate(bcg = (ifelse(race =="black" & collegegrad == TRUE, 1, 0))) %>%
  group_by(year,state) %>%
  mutate(N = sum(bcg))

df2 <- df22 %>%
  group_by(year,state) %>%
  filter(N == 0) 
View(df2)


library(data.table)

Df2 <- data.table(df2)

as.data.table(Df2)[, sum(N), by = .(year, state)]

#2005: Montana, Wyoming
#2006: North Dakota, Montana, Wyoming
#2008: Wyoming, South Dakota, Idaho
#2009: South Dakota
#2010: South Dakota
#2011: North Dakota
#2012: Montana
#2015: Wyoming

#3) In the 2010 household sample, what percent of the households are headed by men? Now use the hwt
#variable to weight the households. In the 2010 household population, what percent of the households
#are estimated to be headed by men? Is there any difference between the sample and the estimated
#population.

library(dplyr)

df3 <- cps %>%
  filter(year == 2010) %>%
  mutate(N = sum(hwt)) %>%
  mutate(M = (ifelse(sex =="M", hwt, 0))) %>%
  mutate(MN = sum(M)) %>%
  mutate(Weighted_Percent_Male_Headed_HH = MN / N) %>%
  mutate(N2 = length(year)) %>%
  mutate(M2 = (ifelse(sex =="M",1,0))) %>%
  mutate(MN2 = sum(M2)) %>%
  mutate(Unweighted_Percent_Male_Headed_HH = MN2 / N2)
  


#WeightedPercent headed by males = 50.8%
#Unweighted Percent = 50.6%
#Difference = 0.2% greater male headed under weighted
  

#4) Let's be more specific. Among households headed by married individuals in the 2010 household
#population, what percent are headed by men? Among households headed by married individuals
#younger than 30 in the 2010 household population, what percent are headed by men? Explain the
#difference, if any.

df4 <- df3 %>%
  filter(married==TRUE) %>%
  mutate(N = sum(hwt)) %>%
  mutate(M = (ifelse(sex =="M", hwt, 0))) %>%
  mutate(MN = sum(M)) %>%
  mutate(Weighted_Percent_Male_Headed_HH = MN / N) %>%
  mutate(N2 = length(year)) %>%
  mutate(M2 = (ifelse(sex =="M",1,0))) %>%
  mutate(MN2 = sum(M2)) %>%
  mutate(Unweighted_Percent_Male_Headed_HH = MN2 / N2)

#Weighted = 60.9% Married Households headed by males
#Unweighted = 60.3% Married Households headed by males


DF4 <- df4 %>%
  filter(age < 30) %>%
  select(year,hwt,sex,race,ownshome) %>%
  na.omit() %>%
  mutate(N = sum(hwt)) %>%
  mutate(M = (ifelse(sex =="M", hwt, 0))) %>%
  mutate(MN = sum(M)) %>%
  mutate(Weighted_Percent_Male_Headed_HH = MN / N) %>%
  mutate(N2 = length(year)) %>%
  mutate(M2 = (ifelse(sex =="M",1,0))) %>%
  mutate(MN2 = sum(M2)) %>%
  mutate(Unweighted_Percent_Male_Headed_HH = MN2 / N2)

#weighted = 51.1% headed by males


#difference = 9.8% more households headed by males in general married pop relative to under 30 married population
#this tells us that younger generations are more open to non traditional family structures, where the female
#is the listed household head instead of the male, and is also perhaps the breadwinner. 

#5) Among households headed by married individuals in the 2010 household population, what is the
#difference between the white rate of home ownership and the black rate of home ownership?Among
#household headed by unmarried individuals, what is the difference? Explain what you observe.

df5 <- df4 %>%
  mutate(Black = (ifelse(race == "black", hwt, 0))) %>%
  mutate(White = (ifelse(race == "white", hwt, 0))) %>%
  mutate(numblack = (sum(Black))) %>%
  mutate(numwhite = (sum(White))) %>%
  mutate(bhomeown = (ifelse(race == "black" & ownshome == TRUE,hwt,0))) %>%
  mutate(whom = (ifelse(race == "white" & ownshome ==TRUE,hwt,0))) %>%
  mutate(whomeownagg = sum(whom)) %>%
  mutate(bhomeownagg = sum(bhomeown)) %>%
  mutate(white_homeownership_rate = whomeownagg/numwhite) %>%
  mutate(black_homeownership_rate = bhomeownagg/numblack)

#Household Homeownership Rates: Married couples in 2010
#White Household Heads: 87.2%
#Black Household Heads: 68%

DF5 <- cps %>%
  filter(year == 2010,married == FALSE) %>%
  mutate(Black = (ifelse(race == "black", hwt, 0))) %>%
  mutate(White = (ifelse(race == "white", hwt, 0))) %>%
  mutate(numblack = (sum(Black))) %>%
  mutate(numwhite = (sum(White))) %>%
  mutate(bhomeown = (ifelse(race == "black" & ownshome == TRUE,hwt,0))) %>%
  mutate(whom = (ifelse(race == "white" & ownshome ==TRUE,hwt,0))) %>%
  mutate(whomeownagg = sum(whom)) %>%
  mutate(bhomeownagg = sum(bhomeown)) %>%
  mutate(white_homeownership_rate = whomeownagg/numwhite) %>%
  mutate(black_homeownership_rate = bhomeownagg/numblack)

#UnMarried Household Heads Homeownership Rates in 2010
#Unmarried White Household Heads: 59.8%
#Unmarried Black Household Heads: 37%

#Lower homeownership rates for unmarried household head relative to married household head. Married couples
#pool their incomes together, making it easier for them to own a home. Unmarried and married Black household 
#heads'homeownership rates are lower than that of, respectively, umarried and married white household heads'.
#The gap is worse for unmarried black household heads; controll for the higher population among whites (so assuming
#that there were an equal number of blacks and whites in the US) for every 100 unmarried white household head who 
#owns a home, there would be just under 62 unmarried black household heads who own homes. In the married category,
#the controlled rate falls to 100 (white owners) : 77.98 (black owners)

#6) In 2015, what percent of households headed by white individuals 25 years or older have college degrees?
#What is the percent for Black individuals? What is the percent for Hispanic individuals? What is the
#percent for Asian individuals?

library(dplyr)
library(tidyverse)
library(ggplot2)

df6 <- cps %>%
  filter(year == 2015,age >= 25) %>%
  mutate(bcollege = (ifelse(race == "black" & collegegrad == TRUE,hwt,0))) %>%
  mutate(wcollege = (ifelse(race == "white" & collegegrad ==TRUE,hwt,0))) %>%
  mutate(acollege = (ifelse(race == "asian" & collegegrad == TRUE,hwt,0))) %>%
  mutate(hcollege = (ifelse(race == "hispanic" & collegegrad ==TRUE,hwt,0))) %>%
  mutate(Black = (ifelse(race == "black", hwt, 0))) %>%
  mutate(White = (ifelse(race == "white", hwt, 0))) %>%
  mutate(Asian = (ifelse(race == "asian", hwt,0))) %>%
  mutate(Hispanic = (ifelse(race == "hispanic",hwt,0))) %>%
  mutate(NB = sum(Black),NBC = sum(bcollege),NW = sum(White),NWC=sum(wcollege),NumAs=sum(Asian),NAC=sum(acollege),
         NH = sum(Hispanic),NHC = sum(hcollege)) %>%
  mutate(blackcollegerate = NBC/NB,whitecollegerate = NWC/NW,asiancollegerate= NAC/NumAs,
         hispaniccollegerate=NHC/NH)
  

#Percent of Household Heads over 25 in 2015 with College Degrees
#White: 36.7%
#Black: 23%
#Asian: 59.4%
#Hispanic: 16.9%
  


#7) Remove the foreign born population and repeat question 7. What difference do you observer?

df7 <- cps %>%
  filter(year == 2015,age >= 25,foreign==FALSE) %>%
  mutate(bcollege = (ifelse(race == "black" & collegegrad == TRUE,hwt,0))) %>%
  mutate(wcollege = (ifelse(race == "white" & collegegrad ==TRUE,hwt,0))) %>%
  mutate(acollege = (ifelse(race == "asian" & collegegrad == TRUE,hwt,0))) %>%
  mutate(hcollege = (ifelse(race == "hispanic" & collegegrad ==TRUE,hwt,0))) %>%
  mutate(Black = (ifelse(race == "black", hwt, 0))) %>%
  mutate(White = (ifelse(race == "white", hwt, 0))) %>%
  mutate(Asian = (ifelse(race == "asian", hwt,0))) %>%
  mutate(Hispanic = (ifelse(race == "hispanic",hwt,0))) %>%
  mutate(NB = sum(Black),NBC = sum(bcollege),NW = sum(White),NWC=sum(wcollege),NumAs=sum(Asian),NAC=sum(acollege),
         NH = sum(Hispanic),NHC = sum(hcollege)) %>%
  mutate(blackcollegerate = NBC/NB,whitecollegerate = NWC/NW,asiancollegerate= NAC/NumAs,
         hispaniccollegerate=NHC/NH)

#Percent of Household Heads over 25 in 2015 with College Degrees that are not foreign born
#White: 36%
#Black: 21.2%
#Asian: 61.4%
#Hispanic: 21.3%

#Rate goes up for Asians and Hispanics, notably jumping from 16.9 to 21.3% for the latter. This puts the rate
#just flight above domestic born blacks, whose rate fell from 23 to 21.2%. The white rate fell, but by only
#0.7%. 

#8) What percent of white households are headed by individuals who are in the work force but are
#unemployed? Plot the unemployment rate for blacks, whites and Hispanics to show the trend from 2005
#to 2015. The x-axis should give the year. The y-axis should give the unemployment rate and it should
#range from 0 to 20%. There should be three lines plotted - one for each group. Both axes should be
#labeled and there should be a legend denoting which line is which. Describe what you observe.

#employed: Whether the head of household is employed (TRUE:Employed; FALSE:Unemployed; NA: Not in Labor Force)

df8 <- cps

DF8 <- df8 %>%
  group_by(race,year) %>%
  mutate(Unemployed = (ifelse(employed == FALSE,hwt,0))) %>%
  mutate(workforce = ifelse(employed == FALSE | employed == TRUE,hwt,0))

DF8[is.na(DF8)] <- 0

Df8 <- DF8 %>%
  group_by(race,year) %>%
  mutate(NumberUnemployed = sum(Unemployed)) %>%
  mutate(totalwf = sum(workforce)) %>%
  mutate(rate = NumberUnemployed/totalwf)

Df8 <- Df8 %>%
  filter(race!="other",race!="asian")

library(data.table)

Df8 <- data.table(Df8)

View(DF8)

Df8 <- as.data.table(Df8)[, mean(rate), by = .(race,year)]

View(DF8)

library(ggplot2)
  
ggplot(Df8, aes(x = year, y = V1, colour = race)) + 
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2005, 2015, 1), limits = c(2005, 2015)) +
  scale_y_continuous(breaks = seq(0, 0.2, 0.01),limits = c(0,0.2),labels = paste0(seq(0, 0.2, 0.01))) +
  labs(x = "Year",y = "Unemployment Rate", colour = "Race",
       title = "Household Head's Unemployment Rate: 2005 to 2015") +
  theme(plot.title = element_text(hjust = 0.5))

#Consistently highest unemployment rates by race is black, followed by hispanic, and then followed by white.
#All races were hit hard after recession, particularly hispanics, whose unemployment rates were only a fraction
#of a percent lower than blacks. However, from 2009 to 2010, the unemployment rate for hispanics barely rose,
#wheres they got much higher for blacks. From 2010 to 2011, the unemployment rate got lower for both whites and
#hispanics, but actually got slightly higher for blacks. Thus, they had a slower recovery. In the end, in 2015
#the unemployment rates increased marginally, by less than 1%, for each race. 

#9) Compute the median household income for blacks and for whites by educational attainment in each
#year from 2005 to 2015. (You must create a variable for eduction where "Low" denotes household heads
#with less than a high school diploma, "Middle" denotes household heads with more than a high school
#diploma but less than a college diploma, and "High" denotes household heads with a college diploma or
#more. Use hsdropout and collegegrad to create this variable) Then plot the ratio of black income to
#white income to show the trend from 2005 to 2015 for each group. The x-axis should give the year. The
#y-axis should give the black-to-white ratio and it should range from 0 to 1. And there should be three
#lines plotted - one for "low"" education, one for "middle"" education, and one for "high" education.
#Describe what you see. 

install.packages("matrixStats")
library(matrixStats)
library(dplyr)
library(ggplot2)

df9 <- cps %>%
  filter(race == "white" | race == "black") %>%
  mutate(educ = ifelse (hsdropout == TRUE, "Low",ifelse(collegegrad==TRUE,"High","Medium"))) %>%
  group_by(race,year,educ) %>%
  mutate(medianinc = weightedMedian(hhincome,w = hwt)) 

Df9 <- data.table(df9)

Df9 <- as.data.table(Df9)[, median(medianinc), by = .(race,year,educ)]


DF9 <- Df9 %>%
  group_by(year,educ) %>%
  mutate(blackinc = (ifelse(race == "black",V1,0))) %>%
  mutate(whiteinc = (ifelse(race=="white",V1,0))) %>%
  mutate(whiteinctot = (sum(whiteinc)),blackinctot = sum(blackinc)) %>%
  mutate(wbratio = blackinctot / whiteinctot)

biogeo <- transform(biogeo, 
                    Biog.aff.ord  = factor(
                      Biogeographic.affinity ,
                      levels=c( 'Bassian','Widespread','Torresian', 'Eyrean'),
                      ordered =TRUE))

DF9 <- transform(DF9, education = factor(educ, levels = c('Low','Medium','High'),ordered=TRUE))


ggplot(DF9, aes(x = year, y = wbratio, colour = education)) + 
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2005, 2015, 1), limits = c(2005, 2015)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),limits = c(0,1),labels = paste0(seq(0, 1, 0.1))) +
  labs(x = "Year",y = "Ratio of Black to White Household Income", colour = "Education Level",
       title = "Ratio of Black to White Household Income By Education Level: 2005 to 2015") +
  theme(plot.title = element_text(hjust = 0.5))

#consistently lower incomes for blacks vis a vis whites in the same educational group. Across the board,
#the income group for high education level has the least unequal distribution of income among whites and 
#blacks. Interestingly, following the 2008 financial crisis, the ratio of black to white household income 
#in the low education level rose to above that of the medium level, having previously been below it for the past.
#two years. Thereafter, the low ratio drops below the medium ratio in 2012, but has a higher ratio, for the next 
#three years. 


#10) Plot the home ownership rates for blacks, whites, and Hispanics to show the trend from 2005 to 2015.
#The x-axis should give the year. The y-axis should give the home ownership rate and it should range
#from 0 to 1. And there should be three lines plotted - one for each racial group. Both axes should be
#labeled and there should be a legend denoting which line is which. Describe what you see.

df10 <- cps %>%
  group_by(year,race) %>%
  mutate(homeown = ifelse(ownshome == TRUE, hwt, 0)) %>%
  mutate(tot = ifelse (ownshome == TRUE | ownshome == FALSE, hwt, 0)) %>%
  mutate(totalhomeown = sum(homeown),total = sum(tot)) %>%
  mutate(rate = totalhomeown/total)

Df10 <- data.table(df10)

Df10 <- Df10 %>%
  filter(race != "other",race != "asian")

Df10 <- as.data.table(Df10)[, mean(rate), by = .(race,year)]

View(Df10)


ggplot(Df10, aes(x = year, y = V1, colour = race)) + 
  geom_line() +
  geom_point() +
  labs(x = "Year",y = "Homeownership Rates", colour = "Race",
       title = "Household Home Ownership Rates By Race: 2005 to 2015") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2005, 2015, 1), limits = c(2005, 2015)) +
  scale_y_continuous(breaks = seq(0, 1, 0.05),limits = c(0,1),labels = paste0(seq(0, 1, 0.05))) 


#Consistently much higher homeownership rates among whites relative to blacks and hispanics. Slight dip
#in homeownership in 2008-2009, but smaller than expected and not notable deviation from past trend. Blacks
#have slightly lower homeownership rates than hispanics in all years after 2005. The rates of homeownership
#among households headed by blacks and hispanics are consistently below 50%; the percentage of homeownership
#among household headed by whites is 76.1% in 2005, falling to 71.77% in 2015. 
