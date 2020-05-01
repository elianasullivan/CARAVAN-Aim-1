library(dplyr)
library(formattable)

### import data ####
primary_care <- read.csv("/Users/elianasullivan/Box/Eli's WFH stuff/CARAVAN/data/for code/drive time_PC.csv")
VA <- read.csv("/Users/elianasullivan/Box/Eli's WFH stuff/CARAVAN/data/for code/drive time_VA.csv")
ruca <- read.csv("/Users/elianasullivan/Box/Eli's WFH stuff/CARAVAN/data/for code/ruca codes.csv")
all <- read.csv("/Users/elianasullivan/Box/Eli's WFH stuff/CARAVAN/data/for code/VA data by census tract.csv")

#### cleaning ####
names(all)[2] <- "FIPS"

#add columns from RUCA (just primary and secondary codes)
all <- merge(x = all, y = ruca[ , c("FIPS.Code","Primary.RUCA.Code","Secondary.RUCA.Code")], by.x ="FIPS", by.y = "FIPS.Code")

#add columns for access
all$access <- "No Access"
all$access[all$FIPS %in% primary_care$GEOID] <- "Non-VA Access"
all$access[all$FIPS %in% VA$GEOID] <- "VA Access"

#assign rurality designations
all$rurality <- ifelse(all$Primary.RUCA.Code < 4, "Urban", ifelse(all$Primary.RUCA.Code < 7, "Micropolitan", "Rural"))

#this is vets by rurality total (used to find percentages w the above)
vets_rurality <- all %>%
  group_by(rurality) %>%
  summarize(vets = sum(Total.Veterans.18.Years.and.Over))

#make the totals into a vector
rurality_num <- pull(vets_rurality, vets)
#add a number for total
rurality_num <- c(rurality_num,sum(rurality_num))


#calculate the number of vets per category
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

#calculate the percentages
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
colnames(table_one) <- c("None", "Non-VA Only", "Both")

df_one <- as.data.frame.matrix(table_one)

formattable(df_one, list('None'=percent, 'Non-VA Only'=percent, 'Both'= percent), align = rep("c",NCOL(df_one)))




##### GRAVEYARD #####


#actual numbers
table_two <- matrix(vec, ncol = 3, byrow =TRUE)
rownames(table_one) <- c(sprintf("Micropolitan (n=%s)",rurality_num[1]), sprintf("Rural (n=%s)",rurality_num[2]), 
                         sprintf("Urban (n=%s)",rurality_num[3]), sprintf("Total (n=%s)",rurality_num[4]))
colnames(table_one) <- c("None", "Non-VA Only", "Both")
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




