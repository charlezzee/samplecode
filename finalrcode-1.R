library(dplyr)

setwd("C:/Users/charl/Documents/qss17fdata/")

#reading in preliminary datasets and calculating the index of dissimilarity for all counties by educational attainment and poverty status

classes <- c(rep("character", 2), rep("integer", 8))

a <- read.csv(file="datafinal - copy.csv", colClasses = classes) %>%
  filter(STATEFP != 72) %>%
  select(STATEFP,B86AA125,B86AB125,B86AC125,AX7AA125,AX7AB125,COUNTYFP,TRACTA,COUNTY,ï..State) %>%
  mutate(nocollege = print(B86AA125),collegeorgrad = (B86AB125 + B86AC125),poor = print(AX7AA125),notpoor = print(AX7AB125),
         state = print(ï..State)) %>%
  na.omit()


a2 <- a %>%
  group_by(STATEFP,COUNTYFP,COUNTY) %>%
  summarize(P = sum(poor),
            NP = sum(notpoor),
            IndexofPov = .5*sum(abs(poor/P - notpoor/NP)),
            PovRate = (P/(NP+P)))


a3 <- a %>%
  group_by(STATEFP,COUNTYFP) %>%
  summarize(NC = sum(nocollege),
            CG = sum(collegeorgrad),
            Indexofeduc = .5*sum(abs(nocollege/NC - collegeorgrad/CG)),
            ColRate = (CG/(NC+CG)))


library(tidyverse)
library(reldist)
library(stargazer)
library(matrixStats)
library(plyr)

#merging in data about # cops per county

cops <- read.csv(file="cops.csv", header=TRUE, sep=",",stringsAsFactors = FALSE) %>%
  mutate(nr = c(1:2817)) %>%
  mutate(STATEFP = ifelse(nr < 50, 1, ifelse(nr < 65,4, ifelse(nr < 140, 5,ifelse(nr < 213,6,ifelse(nr < 260,08,
         ifelse(nr == 260,10,ifelse(nr < 323,12,ifelse(nr < 459, 13,ifelse(nr <463,15,ifelse(nr <505,16,ifelse(nr <583,17,ifelse(nr < 629,18,
         ifelse(nr < 722,19,ifelse(nr <817,20,ifelse(nr < 926,21,ifelse(nr <968,22,ifelse(nr <984,23,ifelse(nr <1012,24,
        ifelse(nr <1095,26,ifelse(nr < 1182,27,ifelse(nr <1207,28,ifelse(nr <1309,29,ifelse(nr <1357,30,ifelse(nr <1443,31,
         ifelse(nr < 1459,32,ifelse(nr < 1463,33,ifelse(nr <1485,34,ifelse(nr <1513,35,ifelse(nr <1564,36,ifelse(nr <1665,37,
         ifelse(nr <1718,38,ifelse(nr <1764,39,ifelse(nr <1841,40,ifelse(nr <1877,41,ifelse(nr <1916,42,ifelse(nr <1961,43,
        ifelse(nr < 2027,45,ifelse(nr <2121,46,ifelse(nr <2356,47,ifelse(nr <2385,48,ifelse(nr <2399,49,ifelse(nr <2494,50,
        ifelse(nr <2533, 51, ifelse(nr < 2533, 53, ifelse(nr < 2588, 54, 55))))))))))))))))))))))))))))))))))))))))))))))



cops$County <- paste(cops$County, "County", sep=" ")

#reading in data about # of different types of crimes commited/arrests per county 

d <- read.csv(file="nhgis0003_ds191_20125_2012_county.csv", header=TRUE, sep=",",stringsAsFactors = FALSE) %>%
  select(COUNTYA,STATEA,QSFE001,QTME006,QTME002,QTME003,QTME005) %>%
  group_by(STATEA,COUNTYA) %>%
  mutate(medage = QSFE001,solomomrat = QTME006/QTME002,solodadrat = QTME005/QTME002) %>%
  select(STATEA,COUNTYA,solomomrat,solodadrat,medage)


d2 <- read.csv(file="nhgis0004_ds191_20125_2012_county.csv", header=TRUE, sep=",",stringsAsFactors = FALSE) %>%
  select(COUNTYA,STATEA,QU1E001) %>%
  group_by(STATEA,COUNTYA) %>%
  mutate(medinc = QU1E001) 

DF <- da35019.0001 %>%
  group_by(FIPS_ST,FIPS_CTY) %>%
  filter(JURFLAG != 1) %>%
  select(FIPS_ST,FIPS_CTY,P1TOT,P1VLNT,P1PRPTY,ROBBERY,DRGSALE,COVIND,CPOPARST,BURGLRY,LARCENY,MVTHEFT,FRGYCNT,FRAUD,EMBEZL,
         COMVICE,DRGSALE,ARSON,VANDLSM,DISORDR,VAGRANT) 

ROBBERY   ROBBERIES
BURGLRY   BURGLARIES                        
LARCENY   LARCENIES                         
MVTHEFT   MOTOR VEHICLE THEFTS 
FRGYCNT   FORGERY/COUNTERFEITING            
FRAUD     FRAUD                            
EMBEZL    EMBEZZLEMENT 
COMVICE   PROSTITUTION/COMM VICE 

DRGSALE   DRUG ABUSE SALE/MANUFACTURE 

DF$COVIND
ARSON     ARSONS

DF2 <- DF %>%
  filter(COVIND != 0) %>%
  mutate(weightedpop = CPOPARST*(COVIND/100)) %>%
  mutate(CrimeRatePer100 = ((P1TOT/weightedpop)*100))


#joining them all together

final1 <- left_join(DF2,a2,by = c("FIPS_ST" = "STATEFP","FIPS_CTY"="COUNTYFP"))
final1 <- left_join(final1,a3,by = c("FIPS_ST" = "STATEFP","FIPS_CTY"="COUNTYFP")) %>%
  na.omit() %>%
  mutate(drop = ifelse(weightedpop > 3000 && CrimeRatePer100 == 0,"Drop","dontDrop")) %>%
  filter(IndexofPov != 0, Indexofeduc != 0, drop != "Drop") 

final2 <- left_join(final1,d, by = c("FIPS_ST" = "STATEA","FIPS_CTY"="COUNTYA"))
final2 <- left_join(final2,d2, by = c("FIPS_ST" = "STATEA","FIPS_CTY"="COUNTYA")) %>%
  na.omit()


final3 <- left_join(cops,a2,by = c("County" = "COUNTY","STATEFP"="STATEFP")) 

final3 <- left_join(final3,a3)

final3 <- left_join(final3,DF2,by = c("COUNTYFP" = "FIPS_CTY","STATEFP"="FIPS_ST")) %>%
  na.omit()

final3 <- left_join(final3,final2)

final3 <- final3 %>%
  mutate(numofficers = as.numeric(Officers.officers)) %>%
  mutate(officerrate = (numofficers / CPOPARST)*100000)


install.packages("extrafont")
library(extrafont)
loadfonts(device="win") 
fonts()

#Getting frustrated with regressions.... just a ton of regressions below this (kind of p hacking by putting in different variables until I find a table that proves my point -- writing this in almost a year later)

fuckreg <- lm(formula = CrimeRatePer100 ~ PovRate, data = final1)


stargazer(reg1,type = "html",
          title = "Regression 7: Poverty Rate and Innovative Crime",
          out="out.html",dep.var.labels = "Innovative Crimes per 100 Peope",
          dep.var.caption = "",covariate.labels = c("Poverty Rate"),
          report=('vc*p'))

reg1 <- lm(formula = CrimeRatePer100 ~ PovRate+IndexofPov, data = final1)


stargazer(reg1,type = "html",
          title = "Regression Concentrated Poverty and Crime",
          out="reg_1_final_crimesper100_indexpov_cimerate.html",dep.var.labels = "County Level",
          dep.var.caption = "Crimes Reported per 100 People",covariate.labels = c("Poverty Rate","Index of Poverty"),
          report=('vc*p'))


reg2 <- lm(formula = CrimeRatePer100 ~ ColRate+Indexofeduc, data = final1)


stargazer(reg2,type = "html",
          title = "Education Inequality and Crime",
          out="reg_2_final_crimesper100_indexeduc_educrate.html",dep.var.labels = "Crimes Reported per 100 People",
          dep.var.caption = "County Level",covariate.labels = c("Rate of College Graduation","Index of Education Dissimilarity"),
          report=('vc*p'))


reg3 <- lm(formula = CrimeRatePer100 ~ PovRate + IndexofPov + ColRate + Indexofeduc, data = final1)

stargazer(reg3,type = "html",
          title = "Education vs. Income Inequality and Dissimarity and Crime",
          out="reg_3_final_crimesper100_indexeducinc_inceducrate.html",dep.var.labels = "Crimes Reported per 100 People",
          dep.var.caption = "County Level",covariate.labels = c("Poverty Rate","Index of Poverty","Rate of College Graduation",
                                                                "Index of Education Dissimilarity"), report=('vc*p'))


reg4 <- lm(formula = CrimeRatePer100 ~ IndexofPov+PovRate+ColRate+medinc+medage+solomomrat+solodadrat, data = final2)

#making the data visualizations with stargazer


stargazer(reg4,type = "html",
          title = "Regression One: Concentrated Poverty and Crime",
          out="reg_4_final_crimesper100_indexpov_cimerate.html",dep.var.labels = "County Level",
          dep.var.caption = "Crimes per 100 People",covariate.labels = c("Index of Poverty","Poverty Rate","Higher Education Rate",
                                                                                  "Median Income",
                                                                                  "Median Age","Solo Mother Household Rate",
                                                                                  "Solo Father H.H. Rate"),
          report=('vc*p'))


reg5 <- lm(formula = CrimeRatePer100 ~ Indexofeduc+ColRate+PovRate+medinc+medage+solomomrat+solodadrat, data = final2)


stargazer(reg5,type = "html",
          title = "Education Inequality and Crime",
          out="reg_5_final_crimesper100_indexeduc_educrate.html",dep.var.labels = "Crimes Reported per 100 People",
          dep.var.caption = "County Level",covariate.labels = c("Index of Education Dissimilarity","Rate of College Graduation",
                                                                "Poverty Rate",
                                                                "Median Income",
                                                                "Median Age","Solo Mother Household Rate",
                                                                "Solo Father H.H. Rate"),
          report=('vc*p'))


reg6 <- lm(formula = CrimeRatePer100 ~ IndexofPov+PovRate+ColRate+officerrate+medinc+medage+solomomrat+solodadrat, data = final3)


stargazer(reg6,type = "html",
          title = "Regression Two: Concentrated Poverty and Crime",
          out="reg_6_final_crimesper100_indexpov_cimerate.html",dep.var.labels = "County Level",
          dep.var.caption = "Crimes Reported per 100 People",covariate.labels = c("Index of Poverty","Poverty Rate","Rate of Higher Education",
                                                                                  "Officers per 100,000 Citizens",
                                                                                  "Median Income",
                                                                                  "Median Age","Solo Mother Household Rate",
                                                                                  "Solo Father H.H. Rate"),
          report=('vc*p'))


reg7 <- lm(formula = CrimeRatePer100 ~ Indexofeduc+ColRate+officerrate+medinc+PovRate+medage+solomomrat+solodadrat, data = final3)


stargazer(reg7,type = "html",
          title = "Concentrated Poverty and Crime",
          out="reg_7_final_crimesper100_indexpov_cimerate.html",dep.var.labels = "County Level",
          dep.var.caption = "Crimes Reported per 100 People",covariate.labels = c("Index of Education Dissimilarity","Rate of College Graduation","Officers per 100,000 Citizens",
                                                                                  "Median Income","Poverty Rate",
                                                                                  "Median Age","Solo Mother Household Rate",
                                                                                  "Solo Father H.H. Rate"),
          report=('vc*p'))


reg8 <- lm(formula = CrimeRatePer100 ~ IndexofPov+PovRate+Indexofeduc+ColRate+officerrate+medinc+medage+solomomrat+solodadrat, data = final3)


stargazer(reg8,type = "html",
          title = "Regression Four: Crime, Poverty & Education",
          out="reg_!!_final_crimesper100_indexpov_cimerate.html",dep.var.labels = "Crimes per 100 People",
          covariate.labels = c("Index of Poverty Dissimilarity","Poverty Rate","Index of Education Dissimilarity",
                               "Rate of Higher Education","Officers per 100,000 Citizens","Median Income",
                                                                                  "Median Age","Solo Mother Household Rate",
                                                                                  "Solo Father H.H. Rate"),
          report=('vc*p'))



reg9 <- lm(formula = CrimeRatePer100 ~ Indexofeduc+ColRate+PovRate+officerrate+medinc+medage+solomomrat+solodadrat, data = final3)


stargazer(reg9,type = "html",
          title = "Regression Three: Educational Segregation and Crime",
          out="reg_8_final_crimesper100_indexpov_cimerate.html",dep.var.labels = "Crimes per 100 People",
          covariate.labels = c("Index of Education Dissimilarity","Rate of Higher Education","Poverty Rate",
                               "Officers per 100,000 Citizens","Median Income",
                               "Median Age","Solo Mother Household Rate",
                               "Solo Father H.H. Rate"),
          report=('vc*p'))
          
#simple linear regressions
ggplot(final1, aes(x = IndexofPov, y = CrimeRatePer100)) +
  geom_point(size = 0.05) +
  geom_smooth(method='lm',colour="green") +
  labs(x = "Index of Poverty Dissimilarity",y = "Crimes per 100 Residents", 
       title = "Figure One: Concentrated Poverty and Crime") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 2, 0.5),limits = c(0,2),labels = paste(seq(0, 2, 0.5)),expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 0.5, 0.05),limits = c(0, 0.5),labels = paste(seq(0, 0.5, 0.05)),expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=14),text=element_text(family="Times New Roman", size=10,face="plain"))


ggplot(final1, aes(x = PovRate, y = CrimeRatePer100)) +
  geom_point(size = 0.1) +
  geom_smooth(method = "lm") +
  labs(x = "Percent Living Below the Poverty Line",y = "Number of Crimes per 100 Residents", 
       title = "Figure 2 : Poverty and Crime Rate") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 2, 0.5),limits = c(0,2),labels = paste(seq(0, 2, 0.5)),expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 0.5, 0.05),limits = c(0,5, 0.5),labels = paste0(seq(0, 50, 5),"%"),expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=14),text=element_text(family="Times New Roman", size=10,face="plain"))


ggplot(final1, aes(x = Indexofeduc, y = CrimeRatePer100)) +
  geom_point(size = 0.1) +
  geom_smooth(method = "lm",colour="brown1") +
  labs(x = "Index of Educational Dissimilarity",y = "Number of Crimes per 100 Residents", 
       title = "Figure Three: Educational Segregation and Crime") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 2, 0.5),limits = c(0,2),labels = paste(seq(0, 2, 0.5)),expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 0.475, 0.05),limits = c(0, 0.475),labels = paste0(seq(0, 0.475, 0.05)),expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=14),text=element_text(family="Times New Roman", size=12,face="plain"))


ggplot(final1, aes(x = ColRate, y = CrimeRatePer100)) +
  geom_point(size = 0.1) +
  geom_smooth(method = "lm") +
  labs(x = "Rate of College Educated Adults",y = "Number of Crimes per 100 Residents", 
       title = "Figure Four: Educational Attainment and Crime") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 2, 0.5),limits = c(0,2),labels = paste(seq(0, 2, 0.5)),expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0.05, 0.5, 0.05),limits = c(0.05, 0.5),labels = paste0(seq(5, 50, 5),"%"),expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=14),text=element_text(family="Times New Roman", size=10,face="plain"))

Final1 <- final1 %>%
  mutate(STATEFIPS = FIPS_ST * 1000) %>%
  mutate(FIPS = STATEFIPS+FIPS_CTY)


#lets makes makes to compare the independent and dependent variables!

df_crime_county <- Final1 %>%
  ungroup() %>%
  select(CrimeRatePer100,FIPS) %>%
  rename(c("FIPS" = "region","CrimeRatePer100"="value"))

install.packages(c("choroplethr", "choroplethrMaps")) 
library(choroplethr)
library(choroplethrMaps)


county_choropleth(df_crime_county,
                  title ="2012 County Estimates: Crime", 
                  legend = "Crimes per 100 Residents")
                  
                  theme(text=element_text(family="Times New Roman", size=10,face="plain"))

?county_choropleth()
df_poverty_county <- Final1 %>%
  ungroup() %>%
  select(PovRate,FIPS) %>%
  rename(c("FIPS" = "region","PovRate"="value"))

df_indexpov_county <- Final1 %>%
  ungroup() %>%
  select(IndexofPov,FIPS) %>%
  rename(c("FIPS" = "region","IndexofPov"="value"))


county_choropleth(df_poverty_county,
                  title ="2012 County Estimates: Poverty Rate", 
                  legend = "Poverty Rate",
                  num_colors = 9)


county_choropleth(df_indexpov_county,
                  title ="2012 County Estimates: Index of Poverty", 
                  legend = "Index of Poverty",
                  num_colors = 9)


DF <- da35019.0001 %>%
  group_by(FIPS_ST,FIPS_CTY) %>%
  filter(JURFLAG != 1) %>%
  select(FIPS_ST,FIPS_CTY,P1TOT,P1VLNT,P1PRPTY,ROBBERY,DRGSALE,COVIND,CPOPARST,BURGLRY,LARCENY,MVTHEFT,FRGYCNT,FRAUD,EMBEZL,
         COMVICE,DRGSALE,ARSON,VANDLSM,DISORDR,VAGRANT) 

ROBBERY   ROBBERIES
BURGLRY   BURGLARIES                        
LARCENY   LARCENIES                         
MVTHEFT   MOTOR VEHICLE THEFTS 
FRGYCNT   FORGERY/COUNTERFEITING            
FRAUD     FRAUD                            
EMBEZL    EMBEZZLEMENT 
COMVICE   PROSTITUTION/COMM VICE 

DRGSALE   DRUG ABUSE SALE/MANUFACTURE 


ARSON     ARSONS

DF2 <- DF %>%
  filter(COVIND != 0) %>%
  mutate(weightedpop = CPOPARST*(COVIND/100)) %>%
  mutate(CrimeRatePer100 = ((P1TOT/weightedpop)*100))


RE <- DF2 %>%
  select(FIPS_CTY,FIPS_ST,COVIND,VANDLSM,DISORDR,CPOPARST,ARSON) %>%
  group_by(FIPS_ST,FIPS_CTY) %>%
  filter(COVIND != 0) %>%
  mutate(weightedpop = CPOPARST*(COVIND/100),
            retreatcrime = (VANDLSM+DISORDR+ARSON),
             rcrate = (retreatcrime/weightedpop)*100) %>%
  na.omit()

Innovate <- DF2 %>%
  select(FIPS_CTY,FIPS_ST,COVIND,ROBBERY,BURGLRY,LARCENY,MVTHEFT,FRGYCNT,FRAUD,EMBEZL,COMVICE,CPOPARST,DRGSALE) %>%
  group_by(FIPS_ST,FIPS_CTY) %>%
  filter(COVIND != 0) %>%
  mutate(weightedpop = CPOPARST*(COVIND/100),
         innovativecrime = (ROBBERY+BURGLRY+LARCENY+MVTHEFT+FRGYCNT+FRAUD+EMBEZL+COMVICE+DRGSALE),
         icrate = (innovativecrime/weightedpop)*100) %>%
  na.omit()
  


final4 <- left_join(RE,final2)
final5 <- left_join(final4,final3) %>% na.omit()

library(st)


#more regressions

reg13 <- lm(formula = rcrate ~ Indexofeduc+IndexofPov+PovRate+ColRate+solomomrat+solodadrat+officerrate+medinc+medage, data = final5)


stargazer(reg13,type = "html",
          title = "Regression Five: Retreatist Criminal Deviance",
          out="reg_5.html",dep.var.labels = "Vandalism, Vagrancy and Disorderly Conduct",
          dep.var.caption = "Retreatist Crimes per 100 County Residents",covariate.labels = c("Index of Dissimilarity: Education",
                                                                                              "Index of Dissimilarity: Poverty",
                                                                                              "Poverty Rate",
                                                                                              "Rate of Higher Education",
                                                                                              "Solo Mother Household Rate",
                                                                                              "Solo Father H.H. Rate",
                                                                                              "Officers per 100,000 Citizens",
                                                                                              "Median Income",
                                                                                              "Median Age"),
          report=('vc*p'))

final6 <- left_join(Innovate,final2)
final7 <- left_join(final6,final3) %>% na.omit()

reg <- lm(formula = icrate ~ PovRate, data = final7)



stargazer(reg,type = "html",
          title = "Regression Seven: Poverty Rate and Innovative Crime",
          out="out.html",dep.var.labels = "Innovative Crimes per 100 Peope",
          dep.var.caption = "",covariate.labels = c("Poverty Rate"),
          report=('vc*p'))

regg <- lm(formula = icrate ~ solomomrat, data = final7)



stargazer(regg,type = "html",
          title = "Regression Eight: Rate of County Level Single Mother's Households and Innovative Crime",
          out="out2.html",dep.var.labels = "Innovative Crimes per 100 Peope",
          dep.var.caption = "",covariate.labels = c("Rate of Single Mother Households"),
          report=('vc*p'))


reg14 <- lm(formula = icrate ~ IndexofPov+Indexofeduc+ColRate+PovRate+solomomrat+solodadrat+officerrate++medinc+medage, data = final7)


stargazer(reg14,type = "html",
          title = "Regression Six: Innovative Criminal Deviance",
          out="reg_14_final_crimesper100_indexpov_cimerate.html",dep.var.labels = "Innovative Crimes per 100 County Residents",
          dep.var.caption = "",covariate.labels = c("Index of Dissimilarity: Education",
                                                                                              "Index of Dissimilarity: Poverty",
                                                                                              "Poverty Rate",
                                                                                              "Rate of Higher Education",
                                                                                              "Solo Mother Household Rate",
                                                                                              "Solo Father H.H. Rate",
                                                                                              "Officers per 100,000 Citizens",
                                                                                              "Median Income",
                                                                                              "Median Age"),
          report=('vc*p'))


reg13 <- lm(formula = rcrate ~ IndexofPov+PovRate+Indexofeduc+ColRate+solomomrat+solodadrat+medinc+medage, data = final4)


stargazer(reg13,type = "html",
          title = "",
          out="reg_13_final_crimesper100_indexpov_cimerate.html",dep.var.labels = "Vandalism, Drunkenness, Sex Offenses, Vagrancy, Drug Possestion, Disorderly Conduct",
          dep.var.caption = "Retreatist Crimes per 100,000 County Residents",covariate.labels = c("Index of Dissimilarity: Poverty",
                                                                                                  "Poverty Rate",
                                                                                                  "Index of Education Dissimilarity",
                                                                                                  "Rate of College Graduation",
                                                                                                  "Solo Mother Household Rate",
                                                                                                  "Solo Father H.H. Rate",
                                                                                                  "Median Income",
                                                                                                  "Median Age"),
          report=('vc*p'))

reg14 <- lm(formula = rcrate ~ IndexofPov+PovRate+ColRate+officerrate+solomomrat+solodadrat+medinc+medage, data = final5)


stargazer(reg14,type = "html",
          title = "",
          out="reg_14_final_crimesper100_indexpov_cimerate.html",dep.var.labels = "Vandalism, Drunkenness, Sex Offenses, Vagrancy, Drug Possestion, Disorderly Conduct",
          dep.var.caption = "Retreatist Crimes per 100,000 County Residents",covariate.labels = c("Index of Dissimilarity: Poverty",
                                                                                                  "Poverty Rate","Rate of College Graduation",
                                                                                                  "Officers per 100,000 Citizens",
                                                                                                  "Solo Mother Household Rate",
                                                                                                  "Solo Father H.H. Rate",
                                                                                                  "Median Income",
                                                                                                  "Median Age"),
          report=('vc*p'))


reg12 <- lm(formula = rcrate ~ PovRate+IndexofPov, data = final)


stargazer(reg12,type = "html",
          title = "Concentrated Poverty and Retreatist Crime",
          out="reg_final_retcrimesper100_indexpov_cimerate.html",dep.var.labels = "Vandalism, Drunkenness, Sex Offenses, Vagrancy, Drug Possestion, Disorderly Conduct",
          dep.var.caption = "Retreatist Crimes per 100 People",covariate.labels = c("Poverty Rate","Index of Poverty"),
          report=('vc*p'))


reg13 <- lm(formula = CrimeRatePer100 ~ PovRate + IndexofPov + ColRate + Indexofeduc, data = redf)

stargazer(reg13,type = "html",
          title = "Concentrated Poverty/Educational Inequity and Retreatist Crime Reponse",
          out="reg_final_retcrimesper100_indexeducinc_inceducrate.html",dep.var.labels = "Vandalism, Drunkenness, Sex Offenses, Vagrancy, Drug Possestion, Disorderly Conduct",
          dep.var.caption = "County Level",
          covariate.labels = c("Poverty Rate","Index of Poverty","Rate of College Graduation","Index of Education Dissimilarity"),
          report=('vc*p'))



#CODE NOT INCLUDED IN PAPER BELOW

PropCrimeRate = ((P1PRPTY/weightedpop)*100)

DF2 <- da35019.0001 %>%
  select(PR1PRPTY,weightedpop,)

setwd("C:/Users/charl/Documents/qss17fdata/")


df <- read.csv(file="nhgis0001_ts_nominal_county.csv", header=TRUE, sep=",",stringsAsFactors = FALSE)

#extracode


df <- read.csv(file="usa_00054_1.csv", header=TRUE, sep=",",stringsAsFactors = FALSE)

df <- df %>%
  filter(COUNTYFIPS != 0) %>%
  group_by(COUNTYFIPS,STATEFIP) 

df2 <- df %>%
  group_by(COUNTYFIPS,STATEFIP) %>%
  filter(FTOTINC != 9999999, FTOTINC!= 9999998) %>%
  na.omit() %>%
  summarise(GINI_HH = gini(FTOTINC,w = HHWT))

df3 <- df %>%
  group_by(STATEFIP,COUNTYFIPS) %>%
  filter(INCTOT != 9999999) %>%
  na.omit() %>%
  summarise(GINI_PER = gini(INCTOT,w = PERWT))

df4 <- df %>%
  filter(POVERTY != 000) %>%
  mutate(povstat = ifelse(POVERTY <= 100,"Below Poverty Line","Above Poverty Line")) %>%
  mutate(Poor = ifelse(povstat == "Below Poverty Line",HHWT,0)) %>%
  mutate(NotPoor = ifelse(povstat == "Above Poverty Line",HHWT,0)) %>%
  group_by(STATEFIP,COUNTYFIPS) %>%
  summarise(Poor = sum(Poor), NotPoor = sum(NotPoor),censuspovrate = (Poor / (NotPoor+Poor))) 


df5 <- df %>%
  filter(EMPSTAT != 00, EMPSTAT != 3) %>%
  mutate(empstat = ifelse(EMPSTAT == 2, "unemployed","employed")) %>%
  mutate(unemployed = ifelse(empstat == "unemployed",PERWT,0)) %>%
  mutate(employed = ifelse(empstat == "employed",PERWT,0)) %>%
  group_by(STATEFIP,COUNTYFIPS) %>%
  summarise(unemployed = sum(unemployed), employed = sum(employed), unemploymentrate = (unemployed / (unemployed + employed))) 

df6 <- df %>%
  filter(SEX != 9) %>%
  mutate(women = ifelse(SEX == 2, PERWT,0)) %>%
  mutate(men = ifelse(SEX == 1, PERWT,0)) %>%
  group_by(COUNTYFIPS,STATEFIP) %>%
  summarise(meanage = mean(AGE),totalwomen = sum(women),totalmen = sum(men), w2m = (totalwomen/totalmen),
            medianhhinc = weightedMedian(FTOTINC,HHWT),meanhhinc = weighted.mean(FTOTINC,HHWT))

df7 <- df %>%
  mutate(educstat = ifelse(EDUC < 9, "No College","College")) %>%
  mutate(Nocollege = ifelse(educstat == "No College",PERWT,0)) %>%
  mutate(college = ifelse(educstat == "College",PERWT,0)) %>%
  group_by(STATEFIP,COUNTYFIPS) %>%
  summarise(No_Coll = sum(Nocollege), Coll = sum(college), c2ncratio = (No_Coll / Coll)) 

df29 <- df %>%
  group_by(STATEFIP,COUNTYFIPS) %>%
  mutate(public = ifelse(SCHLTYPE == 1, PERWT,0)) %>%
  mutate(private = ifelse(SCHLTYPE == 2,PERWT,0)) %>%
  summarise(totpub = sum(public), totpriv = sum(private),privateoverpublic = (totpriv/totpub),totalpop = sum(PERWT))


df5 <- df %>%
  mutate(Black = ifelse(RACED == 200, PERWT,0)) %>%
  mutate(White = ifelse(RACED == 100, PERWT,0)) %>%
  group_by(STATEFIP,COUNTYFIPS,PUMA) %>%
  summarise(black = sum(Black), white = sum(White),bpwrate = (black/white)) %>%
  ungroup()


df6 <- df5 %>%
  group_by(STATEFIP,COUNTYFIPS) %>%
  summarize(B = sum(black),
            W = sum(white), Index = .5*sum(abs(black/B - white/W))) %>%
  ungroup() 

DF <- da35019.0001 %>%
  group_by(FIPS_ST,FIPS_CTY) %>%
  filter(JURFLAG != 1) %>%
  select(FIPS_ST,FIPS_CTY,P1TOT,P1VLNT,P1PRPTY,ROBBERY,AGASSLT,DRUGTOT,DRGSALE,DRUNK,DISORDR,COVIND,CPOPARST) 


finaldf <- left_join(ljps,DF,by = c("COUNTYFIPS" = "FIPS_CTY", "STATEFIP" = "FIPS_ST"))

df14 <- finaldf %>%
  filter(COVIND != 0) %>%
  mutate(weightedpop = CPOPARST*(COVIND/100)) %>%
  mutate(CrimeRatePer100 = ((P1TOT/weightedpop)*100), ViolentRate = ((P1VLNT/weightedpop)*100),PropCrimeRate = ((P1PRPTY/weightedpop)*100),
         DrugSaleRate = ((DRGSALE/weightedpop)*100),DRUGVIORATE = ((DRUGTOT/weightedpop)*100), GINIHH100 = GINI_HH*100,GINIPER100 = GINI_PER*100,
         meaninc1000 = (meanhhinc/1000), medianinc1000 = (medianhhinc/1000))

df14 <- df14 %>%
  arrange(total)

View(df14)

reg1 <- lm(formula = CrimeRatePer100 ~ GINI_HH, data = df14)


stargazer(reg1,type = "html",Title = "Regression One: Income Inequality and Crime",
          out="regression1_final.html",dep.var.labels = "Crimes Reported per 100 People",
          dep.var.caption = "County Level",covariate.labels = "GINI Coefficient: 0 - 1", object.names = TRUE, report=('vc*p'))



reg2 <- lm(formula = CrimeRatePer100 ~ GINI_HH + povertyrate + unemploymentrate + meaninc1000 + meanage, data = df14)

stargazer(reg2,type = "html",Title = "Multivariate Regression Two",
          out="regression2_final.html",dep.var.labels = "Crimes Reported per 100 People",
          dep.var.caption = "County Level",covariate.labels = c("GINI Coefficient: 0-1", "Poverty Rate", "Unemployment Rate",
                                                                "Mean Income in 1000's of $'s","Mean Age"),object.names = TRUE, report=('vc*p'))


reg3 <- lm(formula = CrimeRatePer100 ~ GINI_HH + meaninc1000 + meanage + unemploymentrate, data = df14)

stargazer(reg3,type = "html",Title = "Multivariate Regression Three",
          out="regression3_final.html",dep.var.labels = "Crimes Reported per 100 People",
          dep.var.caption = "County Level",covariate.labels = c("GINI Coefficient", "Mean Income in 1000's of Dollars",
                                                                "Mean Age","Unemployment Rate"),
          object.names = TRUE, report=('vc*p'))



reg4 <- lm(formula = CrimeRatePer100 ~ GINI_HH + meaninc1000 + meanage, data = df14)

stargazer(reg4,type = "html",Title = "Multivariate Regression Three",
          out="regression4_final.html",dep.var.labels = "Crimes Reported per 100 People",
          dep.var.caption = "County Level",covariate.labels = c("GINI Coefficient", "Mean Income in 1000's of Dollars",
                                                                "Mean Age"),object.names = TRUE, report=('vc*p'))


reg5 <- lm(formula = DrugSaleRate ~ GINI_HH, data = df14)


stargazer(reg5,type = "html",Title = "Regression 5: Income Inequality and Drug Dealing",
          out="regression5_final.html",dep.var.labels = "Number of People Arrested for Drug Sale/Production per 100 People",
          dep.var.caption = "County Level",covariate.labels = "GINI Coefficient: 0 - 1",object.names = TRUE, report=('vc*p'))



reg6 <- lm(formula = DrugSaleRate ~ GINI_HH + povertyrate + unemploymentrate + meaninc1000 + meanage, data = df14)

stargazer(reg6,type = "html",Title = "",
          out="regression6_final.html",dep.var.labels = "Number of People Arrested for Drug Sale/Production per 100 People",
          dep.var.caption = "County Level",covariate.labels = c("GINI Coefficient: 0-1", "Poverty Rate", "Unemployment Rate",
                                                                "Mean Income in 1000's of $'s","Mean Age"),object.names = TRUE,
          report=('vc*p'))


reg7 <- lm(formula = DrugSaleRate ~ GINI_HH + meaninc1000 + meanage + unemploymentrate, data = df14)

stargazer(reg7,type = "html",Title = "",
          out="regression7_final.html",dep.var.labels = "Number of People Arrested for Drug Sale/Production per 100 People",
          dep.var.caption = "County Level",covariate.labels = c("GINI Coefficient: 0-1", "Mean Income in 1000's of Dollars",
                                                                "Mean Age","Unemployment Rate"),
          object.names = TRUE, report=('vc*p'))



reg8 <- lm(formula = DrugSaleRate ~ GINI_HH + meaninc1000 + meanage, data = df14)

stargazer(reg8,type = "html",Title = "",
          out="regression8_final.html",dep.var.labels = "Number of People Arrested for Drug Sale/Production per 100 People",
          dep.var.caption = "County Level",covariate.labels = c("GINI Coefficient", "Mean Income in 1000's of Dollars",
                                                                "Mean Age"),object.names = TRUE, report=('vc*p'))





reg9 <- lm(formula = PropCrimeRate ~ GINI_HH, data = df14)

summary(reg9)


stargazer(reg9,type = "html",Title = "",
          out="regression9_final.html",dep.var.labels = "Includes Robbery, Burglary, Larceny, and Motor Vehicle Theft",
          dep.var.caption = "Number of People Arrested for Property Crimes per 100 People",
          covariate.labels = c("GINI Coefficient: 0-1"),report=('vc*p'))



reg10 <- lm(formula = PropCrimeRate ~ GINI_HH + povertyrate + unemploymentrate + meaninc1000 + meanage, data = df14)



stargazer(reg10,type = "html",Title = "",
          out="regression10_final.html",dep.var.labels = "Includes Robbery, Burglary, Larceny, and Motor Vehicle Theft",
          dep.var.caption = "Number of People Arrested for Property Crimes per 100 People",
          covariate.labels = c("GINI Coefficient: 0-1", "Poverty Rate", "Unemployment Rate","Mean Income in 1000's of $'s","Mean Age"),
          report=('vc*p'))



reg11 <- lm(formula = PropCrimeRate ~ GINI_HH + meaninc1000 + meanage, data = df14)

stargazer(reg11,type = "html",Title = "",
          out="regression11_final.html",dep.var.labels = "Includes Robbery, Burglary, Larceny, and Motor Vehicle Theft",
          dep.var.caption = "Number of People Arrested for Property Crimes per 100 People",covariate.labels = c("GINI Coefficient: 0-1", "Mean Income in 1000's of Dollars",
                                                                                                                "Mean Age"),
          object.names = TRUE, report=('vc*p'))


ggplot(df14, aes(x = GINI_HH, y = CrimeRatePer100)) +
  geom_point(size = 0.1) +
  geom_smooth(method='lm') +
  labs(x = "County GINI Coefficient",y = "Number of Crimes per 100 Residents", 
       title = "Positive Relationship Between Income Inequality and Crime") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 2, 0.5),limits = c(0,2),labels = paste(seq(0, 2, 0.5))) +
  scale_x_continuous(breaks = seq(0.4, 0.55, 0.025),limits = c(0.4, 0.55),labels = paste(seq(0.4, 0.55, 0.025)))




df14 <- df14 %>% arrange((GINI_HH)) 
View(df14)

df$EDUC

df7 <- df %>%
  mutate(educstat = ifelse(EDUC < 9, "No College","College")) %>%
  mutate(Nocollege = ifelse(educstat == "No College",PERWT,0)) %>%
  mutate(college = ifelse(educstat == "College",PERWT,0)) %>%
  group_by(STATEFIP,COUNTYFIPS) %>%
  summarise(No_Coll = sum(Nocollege), Coll = sum(college), c2ncratio = (No_Coll / Coll)) 

final2 <- left_join(df2,df3)
final2 <- left_join(final2,df4)
final2 <- left_join(final2,df5)
final2 <- left_join(final2,df6)
final2 <- left_join(final2,DF2,by = c("COUNTYFIPS" = "FIPS_CTY", "STATEFIP" = "FIPS_ST")




reg9 <- lm(formula = PropCrimeRate ~ GINI_HH, data = df14)

summary(reg9)


stargazer(reg9,type = "html",Title = "",
          out="regression9_final.html",dep.var.labels = "Includes Robbery, Burglary, Larceny, and Motor Vehicle Theft",
          dep.var.caption = "Number of People Arrested for Property Crimes per 100 People",
          covariate.labels = c("GINI Coefficient: 0-1"),report=('vc*p'))


reg10 <- lm(formula = PropCrimeRate ~ PovRate + IndexofPov, data = final1)


stargazer(reg4,type = "html",
          title = "Concentrated Poverty and Property Crime",
          out="reg_final_propcrimesper100_indexpov_cimerate.html",
          dep.var.labels = "Includes Robbery, Burglary, Larceny, and Motor Vehicle Theft",
          dep.var.caption = "Property Crimes per 100 People",covariate.labels = c("Poverty Rate","Index of Poverty"),
          report=('vc*p'))


reg11 <- lm(formula = PropCrimeRate ~ ColRate.x+Indexofeduc.x, data = final1)


stargazer(reg12,type = "html",
          title = "Education Inequality and Property Crime",
          out="reg_final_propcrimesper100_indexeduc_educrate.html",
          dep.var.labels = "Includes Robbery, Burglary, Larceny, and Motor Vehicle Theft",
          dep.var.caption = "Property Crimes per 100 People",
          covariate.labels = c("Rate of College Graduation","Index of Education Dissimilarity"), 
          report=('vc*p'))


reg13 <- lm(formula = PropCrimeRate ~ PovRate + IndexofPov + ColRate + Indexofeduc, data = final1)

stargazer(reg13,type = "html",
          title = "Education vs. Income Inequality and Dissimarity and Property Crime Crime",
          out="reg_final_propcrimesper100_indexeducinc_inceducrate.html",dep.var.labels = "Property Crimes Reported per 100 People",
          dep.var.caption = "County Level",covariate.labels = c("Poverty Rate","Index of Poverty","Rate of College Graduation",
                                                                "Index of Education Dissimilarity"),
          object.names = TRUE, report=('vc*p'))
library(plyr)


