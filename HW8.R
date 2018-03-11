#1) Sum the total population (TotPop) for each Congressional District. The District numbers for the 113th
#Congress are identified by the variable cd113. Use the functions mean() and sd() to calculate the mean
#and standard deviation of the District populations. Is the standard deviation small or large relative to
#the mean?

library(tidyverse)

df <- fl %>%
  select(cd113,TotPop) %>%
  na.omit() %>%
  mutate(cd113 = as.factor(cd113)) %>%
  group_by(cd113) %>%
  summarise(CongPop = sum(TotPop))

summary(df$CongPop)
#mean = 696300

sd(df$CongPop)

#Standard Deviation = 178.3987

#Standard deviation is small relative to the mean, indicating that there is low variance among population sizes of congressional districts.

#2) Now sum the voting-age population (VAP) for each Congressional District. Calculate the mean and
#standard deviation of the District voting-age populations. What do you notice about the difference
#between this standard deviation and the standard deviation you calculated above? What does it say
#about "one person one vote?"


df2 <- fl %>%
  select(cd113,VAP) %>%
  na.omit() %>%
  mutate(cd113 = as.factor(cd113)) %>%
  group_by(cd113) %>%
  summarise(VotePop = sum(VAP))

mean(df2$VotePop)

sd(df2$VotePop)

#Mean = 548,119.2

#SD = 17,382.9

#Standard deviation is far higher (100x higher roughly) than for total pop. This indicates that people in certain districts with smaller 
#numbers of illegible voters have disproportionate influence over who gets elected. 

#3) What percent of Florida's total population is Black? What percent of Florida's 27 Congressional
#Districts have a constituency that is a majority Black? What do you make of the difference? (Include all ages.)

df3 <- fl %>%
  select(BlackPop, HispPop, TotPop, cd113) %>%
  na.omit() %>%
  mutate(cd113 = as.factor(cd113)) %>%
  group_by(cd113) %>%
  mutate(whiteasianpop = (TotPop - (BlackPop + HispPop))) %>%
  summarise(totalblack = sum(BlackPop),totalhis = sum(HispPop), totalwa = sum(whiteasianpop),totalpop = sum(TotPop),
            perblack = (totalblack / totalpop), perhisp = (totalhis/totalpop), perwa = (totalwa/totalpop)) %>%
  mutate(blackmajor = ifelse(perblack > perhisp & perblack > perwa, "Black Majority","Not Black Majority")) %>%
  mutate(blackmajor = as.factor(blackmajor)) 

d

df3$blackmajor


#3/27 districts are black majority = 11.11%
#Not super surprising given that there is Black's make up roughly 12% of population. 

#4) What percent of Florida's total population is Hispanic? What percent of Florida's 27 Congressional
#Districts have a constituency that is a majority Hispanic? What do you make of the difference? (Include all ages.)

df4 <- fl %>%
  select(BlackPop, HispPop, TotPop, cd113) %>%
  na.omit() %>%
  mutate(cd113 = as.factor(cd113)) %>%
  group_by(cd113) %>%
  mutate(whiteasianpop = (TotPop - (BlackPop + HispPop))) %>%
  summarise(totalblack = sum(BlackPop),totalhis = sum(HispPop), totalwa = sum(whiteasianpop),totalpop = sum(TotPop),
            perblack = (totalblack / totalpop), perhisp = (totalhis/totalpop), perwa = (totalwa/totalpop)) %>%
  mutate(hispmajor = ifelse(perhisp > perwa & perhisp > perwa, "Hispanic Majority","Not Hispanic Majority")) 
  

df4$hispmajor

#4/27 districts have a Hispanic Majority. (23.53% total)
#Given that hispanics make up roughly 17.% of US population this is not very surprising/unusual. Thought, it does appear that Florida has a 
#slightly higher density of hispanics than blacks relative to the general US population.

#5) What percent of the two-party vote did McCain receive in Florida? In what percent of Congressional
#Districts did McCain win a majority of the two-party vote? What do you make of the difference?

df5 <- fl %>%
  select(mccain,obama,cd113) %>%
  na.omit() %>%
  mutate(cd113 = as.factor(cd113)) %>%
  group_by(cd113) %>%
  summarise(totmc = sum(mccain),totob = sum(obama), totalvote = sum(mccain + obama)) %>%
  mutate(majority = ifelse(totmc >totob, "McCain Majority","Obama Majority"))

df5$majority

#Mccain won 17/27 Congessional Districts. I thought Obama would have won more of them given that he won Florida in the election.

#6) For all units with a voting-age population greater than 200, make a scatter plot that displays the
#relationship between the black share of the voting age population (on the x-axis) and the McCain share
#of the two-party vote (on the y-axis). Do the same for Hispanics. Use geom_point() to create the
#points of the scatter plot. Add geom_smooth() to plot the trend (think of this as a rolling average).
#What do you make of the relationship?

df6 <- fl %>%
  filter(VAP > 200) %>%
  mutate(BlackShare = (BlackPop/VAP)) %>%
  mutate(HispanicShare = (HispPop/VAP)) %>%
  mutate(McCainShare = mccain / (obama + mccain))

ggplot(df6, aes(x = BlackShare, y = McCainShare)) +
  geom_point(size = 0.001) +
  geom_smooth() +
  labs(x = "Black Share",y = "McCain Share", 
       title = "Negative Relationship Between % of Neighborhood that is Black and % That Voted for McCain") +
  theme_bw() +
  scale_x_continuous(limits = c(0,1))

ggplot(df6, aes(x = HispanicShare, y = McCainShare)) +
  geom_point(size = 0.001) +
  geom_smooth() +
  labs(x = "Hispanic Share",y = "McCain Share", 
       title = "Non-Linear Relationship Between % of Neighborhood that is Hispanic and % That Voted for McCain") +
  theme_bw() +
  scale_x_continuous(limits = c(0,1))


                                                                                                
#7) Do the same as you did for 6, except plot voter turnout on the y-axis instead. To do this, use (mccain +
# obama)/VAP as a measure for turnout. (Note: There are some units where the presidential vote-share is greater 
#than the voting-age population. This is due to estimation errors in the data. Ignore these cases by constraining 
#the y-axis to only contain values between 0 and 1.) What do you make of the relationship?

df7 <- fl %>%
  filter(VAP > 200) %>%
  mutate(BlackShare = (BlackPop/VAP)) %>%
  mutate(HispanicShare = (HispPop/VAP)) %>%
  mutate(VoterTurnout = (obama + mccain)/VAP) %>%
  filter(VoterTurnout <= 1) %>%
  na.omit()

ggplot(df7, aes(x = BlackShare, y = VoterTurnout)) +
  geom_point(size = 0.001) +
  geom_smooth() +
  labs(x = "Black Share",y = "Voter Turnout", 
       title = "Relationship between % of Neighborhood that is Black and Voter Turnout") +
  theme_bw() +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1))

ggplot(df7, aes(x = HispanicShare, y = VoterTurnout)) +
  geom_point(size = 0.001) +
  geom_smooth() +
  labs(x = "Hispanic Share",y = "Voter Turnout", 
       title = "Non-Linear Relationship Between % of Neighborhood that is Hispanic and Voter Turnout") +
  theme_bw() +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1))

#8) Calculate McCain's share of the two-party vote for each of Florida's 27 Congressional District and plot
#the results. Arrange the Districts along the x-axis by their degree of support for McCain (from least 1
#to most). Then plot McCain's vote share along the y-axis. Your plot should look like the one below.
#What do you notice about the distribution of the partisan vote?


df8 <- fl %>%
  select(mccain, obama, cd113) %>%
  na.omit() %>%
  mutate(cd113 = as.factor(cd113)) %>%
  group_by(cd113) %>%
  summarise(mccain = sum(mccain),obama = sum(obama)) %>%
  mutate(McCainShare = (mccain / (obama + mccain)))



levels_mccain <- c(0.134606402494354, 0.188725114898972, 0.29027029461398,  0.340783234974542, 0.361121152631231, 0.377841915546952, 0.393467969961925,
                  0.429124917095366, 0.480923189605211, 0.485304396478032, 0.501236163901367, 0.504092404389369, 0.51256738142445,  0.514068999263644,
                  0.525086236033241, 0.525358262568791, 0.527611137875383, 0.53528905664071,  0.540845463895404, 0.546046143582121, 0.556180898656596,
                  0.564410347012523, 0.564775621108304, 0.573091491897455, 0.599872854131979, 0.632346547635623, 0.676238126923785)

Con_dist <- c(1:27)

DF8 <- data.frame(levels_mccain,Con_dist)

View(DF8)


ggplot(DF8,aes(x = Con_dist, y = levels_mccain)) +
  geom_point() +
  geom_smooth(method='lm',formula=~x)
  labs(x = "Congressional Districts Arranged by Support for McCain",y = "John McCain's Share of the 2???Party Vote") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0,27,1),limits = c(0,27),labels = paste(seq(0,27,1))) +
  scale_y_continuous(limits = c(0,1),breaks = seq(0,1,0.05),labels = paste(seq(0,1,0.05))) +
  geom_hline(yintercept=0.5,color = "black", size=0.75)
  
  scale_y_continuous(breaks = seq(0, 50, 5),limits = c(0,50),labels = paste0(seq(0, 50, 5), "%"))
install.packages("gridExtra")


#9) Take a look at the dataframe sims. Each column in the dataframe is the result of a computer
#simulated redistricting procedure that assigns each observation in the fl dataset to one of the 27
#Congressional Districts. Like Florida's actual Congressional Districts, these districts are drawn to be
#equally apportioned, compact, and contiguous. However, unlike Florida's Congressional Districts, these
#Districts are NOT drawn by politicians who have the incentive to gerrymander. Instead, they are
#drawn by a computer, without partisan intent. Your task is to calculate the McCain vote-share for
#every district in each of the 100 simulations. Then, for each simulation, count the number of districts
#where McCain has a majority of the two-party vote. Report the mean and standard deviation. Does the
#actual number of McCain-Majority districts fall within 2 standard deviations of the simulated mean?
#(NOTE: sims has 100 columns corresponding to the results of 100 simulations. In each column there is
# a set of district numbers that are assigned to the 15,640 units in the dataframe fl. The rows of sims
# directly map to the rows of fl such that the first row of sims corresponds to the first row of fl, the
# second row of sims corresponds to the second row of fl, and so on.)

View(fl)
View(sims)
library(dplyr)
library(tidyverse)
rbin
?gather()
a <- gather(sims,fld)

View(a)

loop through each column
problem 4/5 replace cd113 with results.1, results.2, results.3 in loop
use dplyr--  gather

df4 <- fl %>%
  select(BlackPop, HispPop, TotPop, cd113) %>%
  na.omit() %>%
  mutate(cd113 = as.factor(cd113)) %>%
  group_by(cd113) %>%
  mutate(whiteasianpop = (TotPop - (BlackPop + HispPop))) %>%
  summarise(totalblack = sum(BlackPop),totalhis = sum(HispPop), totalwa = sum(whiteasianpop),totalpop = sum(TotPop),
            perblack = (totalblack / totalpop), perhisp = (totalhis/totalpop), perwa = (totalwa/totalpop)) %>%
  mutate(hispmajor = ifelse(perhisp > perwa & perhisp > perwa, "Hispanic Majority","Not Hispanic Majority")) 
