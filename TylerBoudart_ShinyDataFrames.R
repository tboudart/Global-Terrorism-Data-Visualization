# Written by Tyler Boudart

library(dplyr)
library(ggplot2)
library(tidyr)
library(plyr)

#Creating Data Frames

terrorism_trimmed <- read.csv("~/2_Personal/DePaul/DSC 465 - Data Visualization/Project - DSC 465/Version 2/terrorism_trimmed.csv", stringsAsFactors=TRUE)

terrorism_trimmed <- terrorism_trimmed %>%
  mutate(year = iyear) %>%
  select(-c(1))

terrorism_trimmednkill <- terrorism_trimmed %>% drop_na(nkill)

# Data frame for terrorist groups
Groupcount <- ddply(terrorism_trimmed, .(gname, year), nrow)
colnames(Groupcount)[3]  <- "attack_count"
GroupnkillSum <- aggregate(nkill ~ gname + year, data = terrorism_trimmednkill, sum)
colnames(GroupnkillSum)[3]  <- "kills_total"
GroupnkillMean <- aggregate(nkill ~ gname + year, data = terrorism_trimmednkill, mean)
colnames(GroupnkillMean)[3]  <- "kills_mean"
GroupnkillTable <- left_join(GroupnkillSum, GroupnkillMean, by = c("gname" = "gname", "year" = "year"))
GroupTable <- left_join(GroupnkillTable, Groupcount, by = c("gname" = "gname", "year" = "year"))

# Data frame for target nationality
natltycount <- ddply(terrorism_trimmed, .(natlty1_txt, year), nrow)
colnames(natltycount)[3]  <- "attack_count"
natltynkillSum <- aggregate(nkill ~ natlty1_txt + year, data = terrorism_trimmednkill, sum)
colnames(natltynkillSum)[3]  <- "kills_total"
natltynkillMean <- aggregate(nkill ~ natlty1_txt + year, data = terrorism_trimmednkill, mean)
colnames(natltynkillMean)[3]  <- "kills_mean"
natltynkillTable <- left_join(natltynkillSum, natltynkillMean, by = c("natlty1_txt" = "natlty1_txt", "year" = "year"))
natltyTable <- left_join(natltynkillTable, natltycount, by = c("natlty1_txt" = "natlty1_txt", "year" = "year"))


# Data frame for target type
targtypecount <- ddply(terrorism_trimmed, .(targtype1_txt, year), nrow)
colnames(targtypecount)[3]  <- "attack_count"
targtypenkillSum <- aggregate(nkill ~ targtype1_txt + year, data = terrorism_trimmednkill, sum)
colnames(targtypenkillSum)[3]  <- "kills_total"
targtypenkillMean <- aggregate(nkill ~ targtype1_txt + year, data = terrorism_trimmednkill, mean)
colnames(targtypenkillMean)[3]  <- "kills_mean"
targtypenkillTable <- left_join(targtypenkillSum, targtypenkillMean, by = c("targtype1_txt" = "targtype1_txt", "year" = "year"))
targtypeTable <- left_join(targtypenkillTable, targtypecount, by = c("targtype1_txt" = "targtype1_txt", "year" = "year"))

# Data frame for region attack occured
Regioncount <- ddply(terrorism_trimmed, .(region_txt, year), nrow)
colnames(Regioncount)[3]  <- "attack_count"
RegionnkillSum <- aggregate(nkill ~ region_txt + year, data = terrorism_trimmednkill, sum)
colnames(RegionnkillSum)[3]  <- "kills_total"
RegionnkillMean <- aggregate(nkill ~ region_txt + year, data = terrorism_trimmednkill, mean)
colnames(RegionnkillMean)[3]  <- "kills_mean"
RegionnkillTable <- left_join(RegionnkillSum, RegionnkillMean, by = c("region_txt" = "region_txt", "year" = "year"))
RegionTable <- left_join(RegionnkillTable, Regioncount, by = c("region_txt" = "region_txt", "year" = "year"))

# Data frame for primary weapon used
Weaponcount <- ddply(terrorism_trimmed, .(weaptype1_txt, year), nrow)
colnames(Weaponcount)[3]  <- "attack_count"
WeaponnkillSum <- aggregate(nkill ~ weaptype1_txt + year, data = terrorism_trimmednkill, sum)
colnames(WeaponnkillSum)[3]  <- "kills_total"
WeaponnkillMean <- aggregate(nkill ~ weaptype1_txt + year, data = terrorism_trimmednkill, mean)
colnames(WeaponnkillMean)[3]  <- "kills_mean"
WeaponnkillTable <- left_join(WeaponnkillSum, WeaponnkillMean, by = c("weaptype1_txt" = "weaptype1_txt", "year" = "year"))
WeaponTable <- left_join(WeaponnkillTable, Weaponcount, by = c("weaptype1_txt" = "weaptype1_txt", "year" = "year"))


# Data frame for atack type
attacktypecount <- ddply(terrorism_trimmed, .(attacktype1_txt, year), nrow)
colnames(attacktypecount)[3]  <- "attack_count"
attacktypenkillSum <- aggregate(nkill ~ attacktype1_txt + year, data = terrorism_trimmednkill, sum)
colnames(attacktypenkillSum)[3]  <- "kills_total"
attacktypenkillMean <- aggregate(nkill ~ attacktype1_txt + year, data = terrorism_trimmednkill, mean)
colnames(attacktypenkillMean)[3]  <- "kills_mean"
attacktypenkillTable <- left_join(attacktypenkillSum, attacktypenkillMean, by = c("attacktype1_txt" = "attacktype1_txt", "year" = "year"))
attacktypeTable <- left_join(attacktypenkillTable, attacktypecount, by = c("attacktype1_txt" = "attacktype1_txt", "year" = "year"))

# Data frame for country attack occured
countrycount <- ddply(terrorism_trimmed, .(country_txt, year), nrow)
colnames(countrycount)[3]  <- "attack_count"
countrynkillSum <- aggregate(nkill ~ country_txt + year, data = terrorism_trimmednkill, sum)
colnames(countrynkillSum)[3]  <- "kills_total"
countrynkillMean <- aggregate(nkill ~ country_txt + year, data = terrorism_trimmednkill, mean)
colnames(countrynkillMean)[3]  <- "kills_mean"
countrynkillTable <- left_join(countrynkillSum, countrynkillMean, by = c("country_txt" = "country_txt", "year" = "year"))
countryTable <- left_join(countrynkillTable, countrycount, by = c("country_txt" = "country_txt", "year" = "year"))

# Clean up table schema
colnames(GroupTable)[1] <- "Title"
colnames(natltyTable)[1] <- "Title"
colnames(targtypeTable)[1] <- "Title"
colnames(RegionTable)[1] <- "Title"
colnames(WeaponTable)[1] <- "Title"
colnames(attacktypeTable)[1] <- "Title"
colnames(countryTable)[1] <- "Title"

natltyTable$Title <- ifelse(natltyTable$Title == "", "Unknown",as.character(natltyTable$Title))
WeaponTable$Title <- ifelse(WeaponTable$Title == "Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)", 
                            "Vehicle (not to include vehicle-borne explosives)", 
                            as.character(WeaponTable$Title))


colnames(natltyTable)[1] <- "Title"
colnames(targtypeTable)[1] <- "Title"
colnames(RegionTable)[1] <- "Title"
colnames(WeaponTable)[1] <- "Title"
colnames(attacktypeTable)[1] <- "Title"
colnames(countryTable)[1] <- "Title"

