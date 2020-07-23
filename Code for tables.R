library(dplyr)
library(formattable)
library(ggplot2)


install.packages("devtools")

install.packages("easyGgplot2")
library(devtools)
install_github("kassambara/easyGgplot2")
library(easyGgplot2)



##### Import Data ####
primary_care <- read.csv("/Users/elianasullivan/Box/Eli's WFH stuff/CARAVAN/data/for code/drive time_PC.csv")
VA <- read.csv("/Users/elianasullivan/Box/Eli's WFH stuff/CARAVAN/data/for code/drive time_VA.csv")
ruca <- read.csv("/Users/elianasullivan/Box/Eli's WFH stuff/CARAVAN/data/for code/ruca codes.csv")
all <- read.csv("/Users/elianasullivan/Box/Eli's WFH stuff/CARAVAN/data/for code/VA data by census tract.csv")

pc_scores <- read.csv("/Users/elianasullivan/Box/Eli's WFH stuff/CARAVAN/data/for code/pc_2sfca.csv")
va_scores <- read.csv("/Users/elianasullivan/Box/Eli's WFH stuff/CARAVAN/data/for code/va_2sfca.csv")

combo_scores <- rbind(pc_scores, va_scores)

##### Cleaning and Formatting ####
names(all)[2] <- "FIPS"

#add columns from RUCA (just primary and secondary codes)
all <- merge(x = all, y = ruca[ , c("FIPS.Code","Primary.RUCA.Code","Secondary.RUCA.Code")], by.x ="FIPS", by.y = "FIPS.Code")

#add columns for access
all$access <- "No Spatial Access to Primary Care"
all$access[all$FIPS %in% primary_care$GEOID] <- "Non-VA Access"
all$access[all$FIPS %in% VA$GEOID] <- "VA Access"

#assign rurality designations
all$rurality <- ifelse(all$Primary.RUCA.Code < 4, "Urban", ifelse(all$Primary.RUCA.Code < 7, "Micropolitan", "Rural"))


##### Making Access Table ####

# FIND THE TOTAL NUMBER OF VETS, BY RURALITY AND TOTAL
#this is vets by rurality total (used to find percentages w the above)
vets_rurality <- all %>%
  group_by(rurality) %>%
  summarize(vets = sum(Total.Veterans.18.Years.and.Over))
#make the totals into a vector
rurality_num <- pull(vets_rurality, vets)
#add a number for total
rurality_num <- c(rurality_num,sum(rurality_num))


# FIND THE NUMBER OF VETS BY ACCESS AND RURALITY AND TOTAL
#calculate the number of vets per access category
vet_nums <- all %>%
  group_by(rurality,access) %>%
  summarize(vets = sum(Total.Veterans.18.Years.and.Over))
#calculate the number of vets total
vets_access <- all %>%
  group_by(access) %>%
  summarize(vets = sum(Total.Veterans.18.Years.and.Over))
#makes the stratified vets column into a vector
access_rurality <- pull(vet_nums, vets)
#makes the all vets into a vector
vets_access <- pull(vets_access, vets)
#combo
vec <- c(access_rurality, vets_access)

#calculates the percent of vets in each category, uses totals
percents <- c()
count <- 1
index <- 1
for (i in vec) {
  percents <- c(percents, (i/rurality_num[index]))
  if (count %% 3 == 0) {
    count <- 0
    index <- index + 1
  }
  count <- count + 1
}

#makes the two way table
table_one <- matrix(percents, ncol = 3, byrow =TRUE)
rownames(table_one) <- c(sprintf("Micropolitan (n=%s)",rurality_num[1]), sprintf("Rural (n=%s)",rurality_num[2]), 
                         sprintf("Urban (n=%s)",rurality_num[3]), sprintf("Total (n=%s)",rurality_num[4]))
colnames(table_one) <- c("No Spatial Access to Primary Care", "Spatial Access to non-VA and/or VA Primary Care", "Spatial Access to VA Primary Care")


#creates a data frame from the table
df_one <- as.data.frame.matrix(table_one)

#switch the order of rows and columns
df_one <- df_one[,c("Spatial Access to VA Primary Care", "Spatial Access to non-VA and/or VA Primary Care", "No Spatial Access to Primary Care")]
df_one <- df_one[c(3,1,2,4),]

#if we stop here, we have the %s that include the number that can only access non-va

#now making a column re how many people can access once access is expanded
expanded_access <- df_one$`Spatial Access to VA Primary Care`+df_one$`Spatial Access to non-VA and/or VA Primary Care`
df_one[2] <- expanded_access

#formats the table with percents and nicer formatting
formattable(df_one, list('No Spatial Access to Primary Care'=percent, 'Spatial Access to non-VA and/or VA Primary Care'=percent, 'Spatial Access to VA Primary Care'= percent), align = rep("c",NCOL(df_one)))



### CENSUS TRACT RURALITY DESIGNATION CALCULATIONS ####

ruca_table <- table(ruca$Primary.RUCA.Code)
ruca_df <- data.frame(ruca_table)

urban <- sum(ruca_df[,2][1:3])
micro <- sum(ruca_df[,2][4:5]) #strangely there are no 6s so index gets bumped
rural <- sum(ruca_df[,2][6:9])
total <- urban + micro + rural + 4

c(urban,micro,rural)
#this is the percent of OR census tracts that are urban, micro, rural
percent(c(urban,micro,rural)/total)


## number of veterans in different rualities ##
urban_vets <- 199329
rural_vets <- 30588
micro_vets <- 58623
total_vets <- urban_vets+rural_vets+micro_vets

percent(c(urban_vets,micro_vets,rural_vets)/total_vets)
total_vets


#### TOTAL NUMBER OF NON-VETS BY RURALITY AND TOTAL ####
#this is vets by rurality total (used to find percentages w the above)
nonvets_rurality <- all %>%
  group_by(rurality) %>%
  summarize(nonvets = sum(Total.Nonveterans.18.Years.and.Over))
#make the totals into a vector
rurality_non_num <- pull(nonvets_rurality, nonvets)
#add a number for total
rurality_non_num <- c(rurality_non_num,sum(rurality_non_num))

#mirco, rural, urban
# percent of nonvets over 18 in each area
percent(rurality_non_num[1:3]/rurality_non_num[4])




####### 2SFCA analysis #######

#stratify by rurality
## VA only
va_score_summary <- va_scores %>%
  group_by(URF) %>%
  summarize(mean(ProToPop), median(ProToPop))
va_score_summary


## VA and non-VA
pc_score_summary <- pc_scores %>%
  group_by(URF) %>%
  summarize(mean(ProToPop), median(ProToPop))
pc_score_summary


## ANOVA ## 

#one-way primary care
one_way_pc <- aov(ProToPop ~ URF, data = pc_scores)
summary(one_way_pc)
TukeyHSD(one_way_pc)

#one-way VA
one_way_va <- aov(ProToPop ~ URF, data = va_scores)
summary(one_way_va)
TukeyHSD(one_way_va)

one_way_va$model

#two way with URF and pc/va
two_way <- aov(ProToPop ~ URF + which, data = combo_scores)
summary(two_way)
TukeyHSD(two_way)

#t-test for pc vs vA 
t.test(pc_scores$ProToPop, va_scores$ProToPop)

#just summary
summary(pc_scores)
summary(va_scores)



## counting zeros ## 

# totals
total_urban <- sum(va_scores$URF == 'Urban')
total_rural <- sum(va_scores$URF == 'Rural')

#VA only
percent(sum(va_scores$URF == 'Urban' & va_scores$ProToPop == 0)/total_urban)
percent(sum(va_scores$URF == 'Rural' & va_scores$ProToPop == 0)/total_rural)

#all pc
percent(sum(pc_scores$URF == 'Urban' & pc_scores$ProToPop == 0)/total_urban)
percent(sum(pc_scores$URF == 'Rural' & pc_scores$ProToPop == 0)/total_rural)

#### DISTRIBUTIONS ####
# holding on this for now but can show, maybe erin #

ggplot2.histogram(data=pc_scores, xName='ProToPop',
                  groupName='URF', legendPosition="top")

## all primary care
ggplot2.histogram(data=pc_scores, xName='ProToPop',
                  groupName='URF', legendPosition="top",
                  alpha=0.5, addDensity=TRUE)

#va only
ggplot2.histogram(data=va_scores, xName='ProToPop',
                  groupName='URF', legendPosition="top",
                  alpha=0.5, addDensity=TRUE)

#here's all three next to each other
ggplot(pc_scores,aes(x=ProToPop))+geom_histogram()+facet_grid(~URF)+theme_bw()



#one more thing, just the distributions and not historgrams
ggplot(pc_scores, aes(x=ProToPop, color=URF)) +
  geom_density()


##### GRAVEYARD #####


#actual numbers
table_two <- matrix(vec, ncol = 3, byrow =TRUE)
rownames(table_one) <- c(sprintf("Micropolitan (n=%s)",rurality_num[1]), sprintf("Rural (n=%s)",rurality_num[2]), 
                         sprintf("Urban (n=%s)",rurality_num[3]), sprintf("Total (n=%s)",rurality_num[4]))
colnames(table_one) <- c("No Spatial Access to Primary Care", "Non-VA Only", "Both")
table_two

#assign VA, all-pc or none
all$primary_care <- 0
all$VA <- 0
all$primary_care[all$FIPS %in% primary_care$GEOID] <- 1
all$VA[all$FIPS %in% VA$GEOID] <- 1
all$both <- all$VA + all$primary_care

#split into seperate data frames by rurality
urban <- subset(all, all$Primary.RUCA.Code > 0 & all$Primary.RUCA.Code < 4)
micro <- subset(all, all$Primary.RUCA.Code > 3 & all$Primary.RUCA.Code < 7)
rural <- subset(all, all$Primary.RUCA.Code > 6)

urban %>%
  group_by(both) %>%
  summarize(vets = sum(Total.Veterans.18.Years.and.Over))
  
df_micro <- micro %>%
  group_by(both) %>%
  summarize(vets = sum(Total.Veterans.18.Years.and.Over))

df_rural <- rural %>%
  group_by(both) %>%
  summarize(vets = sum(Total.Veterans.18.Years.and.Over))


# % within 30 mins of VA-primary care
# % within 30 mins of any primary care


#need to make a new df
new_df <- data.frame(
  Setting = c("Urban", "Micropolitan", "Rural", "Total"),
  Total_Veterans = c(sum(urban$Total.Veterans.18.Years.and.Over), sum(micro$Total.Veterans.18.Years.and.Over), 
                     sum(rural$Total.Veterans.18.Years.and.Over), sum(all$Total.Veterans.18.Years.and.Over))
)

formattable::percent(c(df_one[1:3,1],df_one[1:3,2],df_one[1:3,3]))

 