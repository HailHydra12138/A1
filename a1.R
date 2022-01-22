#=========================================================================
# Econ 613 A1
# Yuqi Zhou
#=========================================================================
install.packages("plyr")
install.packages("dplyr")
install.packages("readr")
library(haven)
library(ggplot2)
library(tidyverse)
setwd("C:/Users/35387/OneDrive/Documents/ECON 613/Data")

# Exercise 1 Basic Statistics
#(1) Number of households surveyed in 2007.
dathh2007 <- read.csv('dathh2007.csv')
nrow(dathh2007)
# Ans: 10498
#(2) Number of households with marital status "Couple with kids" in 2005.
dathh2005 <- read.csv('dathh2005.csv')
sum(dathh2005$mstatus == "Couple, with Kids")
# Ans: 3374
#(3) Number of individuals surveyed in 2008.
datind2008 <- read.csv('datind2008.csv')
nrow(datind2008)
# Ans: 25510
#(4) Number of individuals aged between 25 and 35 in 2016.
datind2016 <- read.csv('datind2016.csv')
age = 0
total_number = 0
for (age in 25:35) {
  ans = sum(datind2016$age == age)
  total_number = total_number + ans
}
# Ans:2765
#(5) Cross-table gender/profession in 2009.
datind2009 <- read.csv('datind2009.csv')
table_genderprofession <- table(datind2009$gender,datind2009$profession)
#(6) Distribution of wages in 2005 and 2019. Report the mean, the standard deviation, 
# the inter-decile ratio D9/D1 and the Gini coefficient.
datind2005 <- read.csv('datind2005.csv')
datind2019 <- read.csv('datind2019.csv')
# We need to delete people who are not employed.
wages_in_2005 = datind2005$wage[!is.na(datind2005$wage)] 
wages_in_2005 = wages_in_2005[wages_in_2005 != 0]
wages_in_2019 = datind2019$wage[!is.na(datind2019$wage)]
wages_in_2019 = wages_in_2019[wages_in_2019 != 0]
# Construct a function that returns mean, standard deviation, D9/D1 and Gini Coefficient. 
data_function = function(vec){
  mean = mean(vec)
  sd = sd(vec)
  inter_decile_ratio  = (quantile(vec,probs = 
                         seq(0.1,0.9,by = 0.1))[[9]])/(quantile(vec,probs = seq(0.1,0.9,by = 0.1))[[1]])
  gini_coefficient = sum(outer(vec, vec, FUN=function(x,y){abs(x-y)})) / (2 * (length(vec))^2 * mean(vec))
  return(c(mean,sd,inter_decile_ratio,gini_coefficient))
} 
data_of_wages_in_2005 <- as.character(print(data_function(wages_in_2005)))
data_of_wages_in_2019 <- as.character(print(data_function(wages_in_2019)))
#(7) Distribution of age in 2010. Plot an histogram. Is there any difference between men and women?
datind2010 <- read.csv('datind2010.csv')
age_2010 = datind2010$age
density_2010 = density(age_2010)
plot(density_2010,main = "Age in 2010")
datind2010_male <-datind2010 %>% filter(datind2010$gender == "Male")
hist(datind2010_male$age, main = "Age - Male", xlab = "Age of Male") 
datind2010_female <- datind2010 %>% filter(datind2010$gender == "Female")
hist(datind2010_female$age, main = "Age - Female", xlab = "Age of Female") 

#(8) Number of individuals in Paris in 2011.
datind2011 <- read.csv('datind2011.csv')
dathh2011 <- read.csv('dathh2011.csv')
data_2011 <- merge(datind2011, dathh2011, by = "idmen")
count(data_2011$location == "Paris")
# Ans = 3514

# Exercise 2 Merge Datasets num_ind_paris <- 
library(plyr)
library(dplyr)
library(readr)

#(1) Read all individual datasets from 2004 to 2019. Append all these datasets.
datind <- list.files(path = "C:/Users/35387/OneDrive/Documents/ECON 613/Data", 
                    pattern = "datind", full.names = TRUE) %>% 
           lapply(read_csv) %>% bind_rows()

#(2) Read all household datasets from 2004 to 2019. Append all these datasets.
dathh <- list.files(path = "C:/Users/35387/OneDrive/Documents/ECON 613/Data", 
                    pattern = "dathh", full.names = TRUE) %>% 
                    lapply(read_csv) %>% bind_rows()

#(3) List the variables that are simultaneously present in the individual and household datasets.
indvariables <- names(datind) 
hhvariables <- names(dathh) 
intersection <- intersect(indvariables, hhvariables)
# Ans: "V1"  "idmen" "year" 

#(4) Merge the appended individual and household datasets.
# Assume that all individuals as well as households are unique. 
alldata <- merge(dathh, datind, by=c("idmen", "year"))

#(5) Number of households in which there are more than four family members.
# Assume all individuals in a household are documented in datind dataset. 
idmen_info <- alldata %>% group_by(idmen, year) %>%
                      dplyr::summarise(n = n())
pop_per_household <- idmen_info %>% group_by(idmen) %>%
                         slice(which.max(n))
pop_greater_than_4 <- sum(pop_per_household$n > 4)
# Ans: 4201

#(6) Number of households in which at least one member is unemployed.
unemplyed_people <- filter(alldata, empstat == "Unemployed")
idmen_info <- unemplyed_people %>% group_by(idmen, year) %>%
              dplyr::summarise(n = n())
unemployed_per_household <- idmen_info %>% group_by(idmen) %>%
  slice(which.max(n))
unemployed_at_least_1 <- sum(unemployed_per_household$n > 1)
# Ans: 1240

#(7) Number of households in which at least two members are of the same profession.
members_of_same_profession_d <- alldata %>% group_by(idmen, profession) %>% 
                                filter(n()>1)
members_of_same_profession <- NROW(unique(members_of_same_profession_d$idmen))
# Ans: 31296

#(8) Number of individuals in the panel that are from household-Couple with kids
household_couple_with_kids <- filter(alldata, mstatus == "Couple, with Kids")
nrow(count(household_couple_with_kids$idmen))
# Ans: 11376

#(9) Number of individuals in the panel that are from Paris.
ind_from_Paris_all <- filter(alldata, location == "Paris")
nrow(count(ind_from_Paris_number$idmen))
# Ans: 4665

#(10) Find the household with the most number of family members. Report its idmen.
max_number_household <- max(pop_per_household$n)
max_number_household_idmen <- filter(pop_per_household, n == "14")
#Ans: n = 14, happended in 2004 and 2010.

#(11) Number of households present in 2010 and 2011.
data_in_2010_and_2011 <- filter(alldata, year == "2010" | year == "2011")
nrow(count(data_in_2010_and_2011$idmen))
# Ans: 13426

# Exercise 3: Migration
# (1) Find out the year each household enters and exits the panel. 
data_time_spent <- alldata %>% dplyr::group_by(idmen) %>% 
                   mutate(entrance_year = min(year), exit_year = max(year),
                   time_spent = exit_year - entrance_year + 1) 
hist(data_time_spent$time_spent, xlab = "Time Spent of households in Panel") 

# (2) Based on datent, identify whether or not a household moved into its current 
# dwelling at the year of survey. Report the first 10 rows of your result and plot 
# the share of individuals in that situation across years.
data_time_spent <- data_time_spent %>% dplyr::group_by(idmen, year) %>% 
                   mutate(moved_same_year = (datent == year))
data_moved_in <- data.frame(idmen = data_time_spent$idmen, idind = data_time_spent$idind, 
                            year = data_time_spent$year, moved_same_year = data_time_spent$sameyear) 
# Print first 10 rows of the result
head(data_moved_in, 10) 
ind_moved_in_same_year <- data_moved_in %>% dplyr::group_by(year) %>% 
                          summarise(num_ind_sameyear = n())
ind_moved_in_same_year %>% ggplot(aes(x = year, y = num_ind_sameyear)) + geom_line()


# (3) Based on myear and move, identify whether or not household migrated at the year of survey. 
library(ggplot2)
myear_exist = data_time_spent %>% filter(year <= "2014")
move_exist <- data_time_spent %>% filter(year > "2014")
myear_migrated_exist = myear_exist$year - myear_exist$myear == 0 
move_migrated_exist = move_exist$move == 2
migrated_same_year <- data.frame(idmen = data_time_spent$idmen, idind = data_time_spent$idind,
                                 year = data_time_spent$year, same_year = c(myear_migrated_exist, move_migrated_exist))
head(migrated_same_year,10)
migrated_same_year2 <- migrated_same_year %>% dplyr::group_by(year) %>% 
                       dplyr::summarise(num_hh = n())
num_migrated_same_year <- migrated_same_year %>% dplyr::group_by(year) %>% summarize(num_migrated = same_year == 1)share_fam_migrated_same_year <- data.frame(2004:2019, migrated_same_year2$num_hh, num_migrated_same_year$num_migrated)
share_fam_migrated_same_year %>% ggplot(aes(x = year, y = num_migrated/num_hh)) + geom_point() + labs(x = "The given Year", y = "Individuals Moving in That Survey Year")

# (4) Mix the two plots you created above in one graph, clearly label the graph. Do you prefer one method over the other? Justify.
data_combined <- data.frame(migrated_same_year, migrated_same_year2)
ggplot(data_combined, aes(x = year)) + geom_point(data = migrated_same_year, aes(y = same_year)) +
geom_point(data = num_migrated_same_year, aes(y = num_migrated_sameyear2, size = 4))
# (5) For households who migrate, find out how many households had at least one family member changed his/her profession or employment status.


# Exercise 4: Attrition
# Compute the attrition across each year, where attrition is defined as the 
# reduction in the number of individuals staying in the data panel. 
# Report your final result as a table in proportions.
entrance_years <- data.frame(idind = data_time_spent$idind, 
                             entrance_year = data_time_spent$entrance_year) %>% distinct() 
# It represents the year each individual enters the panel
exit_years <- data.frame(idind = data_time_spent$idind, 
                         exit_year = data_time_spent$exit_year) %>% distinct() 
# It represents the year each individual exits the panel

