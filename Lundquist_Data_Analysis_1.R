#1. Multiply 8 times 3

8 * 3

#2. Sum the Following Intergers: 1L, 
#24L, 42L, #3L, 6L, 3L, 29L

24L + 42L + 3L  + 6L +3L + 29L 

#3.

x <- c(3,1,2)

a <- c(1,2,3,4,5)
b <- c(5,4,3,2,1)

a + b

#4

sum( c(TRUE,FALSE,TRUE) ) == 2

#TRUE == 1, FALSE == 0. SO...
# 1 + 0 + 1 = 2

#6
z <- c(1,52,3,4,5,5,9,7)
z[z>=5]

# 1. 
#Calculate the percent of the population that is incarcerated in each county. List the ???ve counties with 
#the highest percent. The list should include the county's state, name, and percent incarcerated.

#importing package required for analysis

library(dplyr)

#setting the working directory

setwd("C:/Users/charl/Documents/qss17fdata") 

#importing the dataset

df <- read.csv(file="county_total.csv", header=TRUE, sep=",",stringsAsFactors = FALSE)

#creating a new variable- the incarceration rate per county

df2 <- df %>% 
  mutate(Incper = abs((Jail/Pop)*100))

#selecting the necessary variables and grouping by the incarceration percent

df2 <- df2 %>%
  group_by(Incper) %>%
  select(County_Name, State_Name, Incper) 

#determining the countries with the highest incarceration rates using the data.table function


require(data.table)
d <- data.table(df2,key = "Incper")

#installing packages to visualize the data

library(ggplot2)

install.packages("RColorBrewer")
library(RColorBrewer)

install.packages("scales")
library(scales)

#visualizing the data

d <- d[c(3137:3141),]

d$County_Name <- factor(d$County_Name, levels = d$County_Name[order(d$Incper)])


?ggplot



ggplot(d, aes(x=County_Name, y=Incper,fill=State_Name)) + 
  geom_bar(stat="identity") +
  labs(x = "County",y = "Percent of Pop Incarcerated", 
       fill = "State",title = "Counties with Highest Incarceration Percentages") +
  theme(axis.text.x= element_text(size=10, angle=90), plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels=c("Garza County" = "Garza","Bent County" = "Bent","West Feliciana Parish" = "W.F.P.",
                   "Concho County" = "Concho", "Crowley County" = "Crowley")) +
  scale_fill_brewer(palette = "Spectral")
  
  




visual + scale_color_brewer(palette="Dark2") 





#2. Among counties where the black population is greater than 20,000, ???nd the county with the smallest 
#ratio of non-incarcerated black men to non-incarcerated black women.Give the state, name, and ratio 
#for that county.


df3 <- df %>%
  select(State_Name,County_Name,Black_Male_Jail,Black_Female_Jail,Pop,Black_Female,Black_Male,Black) %>%
  filter(Black > 20000)

df4 <- df3 %>%
  mutate(nonincblackm = Black_Male - Black_Male_Jail) %>%
  mutate(nonincblackf = Black_Female - Black_Female_Jail) %>%
  mutate(ratio = nonincblackm / nonincblackf) 

str(df4)

df5 <- df4 %>% 
  select(ratio,State_Name,County_Name)

df5

df5[which.max(df5$ratio),]

#max: El Paso, Texas. Ratio of non incarcerated black men to women = 1.236159

df5[which.min(df5$ratio),]

#min: Kings County, New York. Ratio of non incarcerated black men to women = 0.7803792


summary(df3)

#3) Make a scatter plot that displays the relationship between the percent of a county that is black and the 
#percent that is male (only among the non-incarcerated population).The x-axis should indicate the 
#fraction of the county's non-incarcerated population that is black.And the y-axis should represent the 
#fraction of the non-incarcerated population that is male.Label the axes and give the plot a title.Save 
#the plot as a pdf ???le and attach it withe homework (in the plots tab in RStudio, click Export > Save as PDF).


df6 <- df %>%
  mutate(numberofnonincmales = (Male - Male_Jail)) %>%
  mutate(countnoninc = Pop - Jail) %>%
  mutate(percentnonincmale = (numberofnonincmales/countnoninc)) %>%
  mutate(percentblack = Black / countnoninc) %>%
  select(percentnonincmale,percentblack)

names(df)

ggplot(df6, aes(x=percentblack, y=percentnonincmale)) +
         geom_point(shape="a",size=1) +
         xlab("Free Black Males per County / Free County Pop") +
         ylab("# of Free Males / Free County Pop") +
         ggtitle("Percent of Free County Population that is Black vs. Male")

library(ggplot2)
    
  
         
  

