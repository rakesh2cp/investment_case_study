################################################################################
# R Code for data cleaning and analysis of  Fund mangagement company ,         #
# an asset management                                                          #
# company.                                                                     #
#                                                                              #
#                                                                              #
# Please open this file in RStudio for better code readability                 #
#                                                                              #
################################################################################

# Setting The Working Directory
setwd("/path-to-data-files")

#Import required Packages/libraries
library(stringr)
library(dplyr)
library(tidyr)
library(readr)

#Checkpoint 1: Data Cleaning 1 -----


#Reading companies data, converting key to lower case and removing whitespace
companies <-
  read.csv(
    "companies.txt",
    sep = "\t",
    fill = TRUE,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
companies$permalink <- tolower(companies$permalink)
companies$permalink <- trimws(companies$permalink, which = "both")


#Reading round2.csv data, converting key to lower case and removing whitespace
rounds2 <-
  read.csv(
    "rounds2.csv",
    fill = TRUE,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
rounds2$company_permalink <- tolower(rounds2$company_permalink)
rounds2$permalink <- trimws(rounds2$company_permalink, which = "both")


#1.1

rounds2 %>%
  summarise(n_distinct(company_permalink))


#1.2

companies %>%
  summarise(n_distinct(permalink))


#1.4

master_frame <-
  merge(
    x = rounds2,
    y = companies,
    by.x = "company_permalink",
    by.y = "permalink",
    all.x = TRUE
  )

#Checkpoint 2: Funding Type Analysis----

#2.1 - 2.4
#comment:-
#we are using one auery to find all "funding_round_type" and their average funding amout.

master_frame %>%
  group_by(funding_round_type) %>%
  summarise(avg_funding = mean(raised_amount_usd, na.rm = TRUE)) %>%
  arrange(desc(avg_funding))

#2.5
#Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round,
#which investment type is the most suitable for it?
master_frame %>%
  group_by(funding_round_type) %>%
  summarise(avg_funding = mean(raised_amount_usd, na.rm = TRUE)) %>%
  arrange(desc(avg_funding)) %>%     
  filter(avg_funding>=5000000, avg_funding<=15000000) 
  

#Checkpoint 3: Country Analysis ----

#make a data frame named top9 with the top nine countries
top9 <- master_frame %>%
  select(country_code, funding_round_type, raised_amount_usd) %>%
  filter(funding_round_type == "venture", country_code != "NA") %>%
  group_by(country_code) %>%
  summarise(sum_fund = sum(raised_amount_usd, na.rm = TRUE)) %>%
  arrange(desc(sum_fund)) %>%
  head(9)

#Create a dataframe of english speaking coutries.
country_code <-
  c('USA', 'CHN', 'GBR', 'IND', 'CAN', 'FRA', 'ISR', 'DEU', 'JPN')
official_language_EN <- c(T, F, T, T, T, F, F, F, F)
english_country <- data.frame(country_code, official_language_EN)

#merging 'english_country' to 'top9' dataframe
top9 <-
  merge(x = top9,
        y = english_country,
        by = "country_code",
        all.x = TRUE)

#Top 3 English-Speaking Countries
top9 %>%
  filter(official_language_EN == T) %>%
  arrange(desc(sum_fund)) %>%
  select(country_code) %>%
  head(3)

#Checkpoint 4: Sector Analysis 1----

#Extract the primary sector of each category list from the category_list column
master_frame$primary_sector <-
  lapply(strsplit(as.character(master_frame$category_list), "\\|"), "[", 1)

#converting primary_sector to lower case
master_frame$primary_sector <- tolower(master_frame$primary_sector)

#removing white space from primary_sector
master_frame$primary_sector <-
  trimws(master_frame$primary_sector, which = "both")

#Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors
#Reading mapping.csv file and loading into a 'mapping' datafrme
mapping <-
  read.csv(
    "mapping.csv",
    fill = T,
    stringsAsFactors = F,
    na.strings = c("", "NA")
  )



#data cleaning
#replacing '0' with 'na'
mapping$category_list <-
  gsub("(.*[^\\.])(0)", "\\1na", mapping$category_list)
#This is going to replace all '0' which are not at the first position or at the last position
mapping$category_list <-
  gsub("(.*[^\\.])(0)", "\\1na", mapping$category_list)
#Used two times intentionally, since in one system work perfectly but for the other group members system doesn't,
#need to use the same command again. That's why we are using the same command to replace the second occurrence of '0'.
#It works perfectly for our group.

mapping$category_list <- gsub("^0", "na", mapping$category_list)
#This is going to replace all '0' which are at the first position


#converting "category_list" to lower case
mapping$category_list <- tolower(mapping$category_list)
#removing white space from ""category_list""
mapping$category_list <- trimws(mapping$category_list, which = "both")

#conveting "mapping" dataframe from wide to long.
mapping_long <- mapping %>%
  na.omit() %>%
  gather(
    main_sector,
    my_value,
    Automotive...Sports:Social..Finance..Analytics..Advertising,
    na.rm = T
  )


#extrcating the relevant observations from the above dataset
mapping_final <- mapping_long %>%
  filter(my_value == 1) %>%
  select(-my_value)


#Country 1 - USA
#Country 2 - GBR
#Country 3 - IND
# funding_round_type - venture


#Checkpoint 5: Sector Analysis 2----
#5.1
#merging 'master_frame' to 'mapping_final' dataframe.
master_frame_final <-
  merge(
    x = master_frame,
    y = mapping_final,
    by.x = "primary_sector",
    by.y = "category_list",
    all.x = TRUE
  )
#Rearranging the columns for convenience (not necessary)
master_frame_final <- master_frame_final[c(2:16, 1, 17, 18)]


#Create three separate data frames D1, D2 and D3 for each of the three countries containing the observations of
#funding type FT falling within the 5-15 million USD range
#data Frame D1 for USA

D1 <- master_frame_final %>%
  filter(
    country_code == "USA",
    funding_round_type == "venture",
    raised_amount_usd >= 5000000,
    raised_amount_usd <= 15000000
  )

#data Frame D2 for GBR
D2 <- master_frame_final %>%
  filter(
    country_code == "GBR",
    funding_round_type == "venture",
    raised_amount_usd >= 5000000,
    raised_amount_usd <= 15000000
  )

#data Frame D3 for IND
D3 <- master_frame_final %>%
  filter(
    country_code == "IND",
    funding_round_type == "venture",
    raised_amount_usd >= 5000000,
    raised_amount_usd <= 15000000
  )

# combinging all 3 dataFrame
DataFrame_all <- rbind(D1, D2, D3)

#The total number (or count) of investments for each main sector in a separate column
DataFrame_all %>%
  group_by(main_sector) %>%
  summarise(count = n())  %>%
  arrange(desc(count))

#The total amount invested in each main sector in a separate column
DataFrame_all %>%
  group_by(main_sector) %>%
  summarise(total_investment = sum(raised_amount_usd)) %>%
  arrange(desc(total_investment))

#1	Total number of Investments (count by desc)
DataFrame_all %>%
  group_by(country_code) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#2	Total amount of investment (USD)
DataFrame_all %>%
  group_by(country_code) %>%
  summarise(total_investment = sum(raised_amount_usd)) %>%
  arrange(desc(total_investment))

#3	Top Sector name (no. of investment-wise)
DataFrame_all %>%
  select(country_code, main_sector, raised_amount_usd) %>%
  group_by(country_code, main_sector) %>%
  summarise(count = n()) %>%
  mutate(rank = dense_rank(desc(count))) %>%
  filter(rank == "1") %>%
  arrange(desc(count)) %>%
  select(-rank, -count)


#4	Second Sector name (no. of investment-wise)
DataFrame_all %>%
  select(country_code, main_sector, raised_amount_usd) %>%
  group_by(country_code, main_sector) %>%
  summarise(count = n()) %>%
  mutate(rank = dense_rank(desc(count))) %>%
  filter(rank == "2") %>%
  arrange(desc(count)) %>%
  select(-rank, -count)

#5	Third Sector name (no. of investment-wise)
DataFrame_all %>%
  select(country_code, main_sector, raised_amount_usd) %>%
  group_by(country_code, main_sector) %>%
  summarise(count = n()) %>%
  mutate(rank = dense_rank(desc(count))) %>%
  filter(rank == "3") %>%
  arrange(desc(count)) %>%
  select(-rank, -count)

#6	Number of investments in top sector (3)
DataFrame_all %>%
  select(country_code, main_sector, raised_amount_usd) %>%
  group_by(country_code, main_sector) %>%
  summarise(count = n()) %>%
  mutate(rank = dense_rank(desc(count))) %>%
  filter(rank == "1") %>%
  arrange(desc(count)) %>%
  select(-rank)


#7	Number of investments in second sector (4)
DataFrame_all %>%
  select(country_code, main_sector, raised_amount_usd) %>%
  group_by(country_code, main_sector) %>%
  summarise(count = n()) %>%
  mutate(rank = dense_rank(desc(count))) %>%
  filter(rank == "2") %>%
  arrange(desc(count)) %>%
  select(-rank)

#8	Number of investments in third sector (5)
DataFrame_all %>%
  select(country_code, main_sector, raised_amount_usd) %>%
  group_by(country_code, main_sector) %>%
  summarise(count = n()) %>%
  mutate(rank = dense_rank(desc(count))) %>%
  filter(rank == "3") %>%
  arrange(desc(count)) %>%
  select(-rank)

#9	For point 3 (top sector count-wise), which company received the highest investment?

#USA
DataFrame_all %>%
  select(name, country_code, main_sector, raised_amount_usd) %>%
  filter(country_code == "USA", main_sector == "Others") %>%
  select(name, country_code, raised_amount_usd) %>%
  arrange(desc(raised_amount_usd)) %>%
  select(country_code, company_name=name) %>%
  head(1)

#GBR
DataFrame_all %>%
  select(name, country_code, main_sector, raised_amount_usd) %>%
  filter(country_code == "GBR", main_sector == "Others") %>%
  select(name, country_code, raised_amount_usd) %>%
  arrange(desc(raised_amount_usd)) %>%
  select(country_code, company_name=name) %>%
  head(1)

#IND
DataFrame_all %>%
  select(name, country_code, main_sector, raised_amount_usd) %>%
  filter(country_code == "IND", main_sector == "Others") %>%
  select(name,country_code, raised_amount_usd) %>%
  arrange(desc(raised_amount_usd)) %>%
  select(country_code, company_name=name) %>%
  head(1)

#10	For point 4 (second best sector count-wise), which company received the highest investment?
# you can use different data set (D1,D2,D3) for this calculation, but we are using the combined data frame for all calculation.
#USA
DataFrame_all %>%
  select(name, country_code, main_sector, raised_amount_usd) %>%
  filter(country_code == "USA",
         main_sector == "Social..Finance..Analytics..Advertising") %>%
  select(name, country_code, raised_amount_usd) %>%
  arrange(desc(raised_amount_usd)) %>%
  select(country_code, company_name=name) %>%
  head(1)

#GBR
DataFrame_all %>%
  select(name, country_code, main_sector, raised_amount_usd) %>%
  filter(country_code == "GBR",
         main_sector == "Social..Finance..Analytics..Advertising") %>%
  select(name, country_code, raised_amount_usd) %>%
  arrange(desc(raised_amount_usd)) %>%
  select(country_code, company_name=name) %>%
  head(1)

#IND
DataFrame_all %>%
  select(name, country_code, main_sector, raised_amount_usd) %>%
  filter(country_code == "IND",
         main_sector == "Social..Finance..Analytics..Advertising") %>%
  select(name, country_code, raised_amount_usd) %>%
  arrange(desc(raised_amount_usd)) %>%
  select(country_code, company_name=name) %>%
  head(1)

#Checkpoint 6: Plots----
#We are exporting following data frame for tableau graph plotting.

#Plot_6.1
write.csv(master_frame, "master_frame.csv", row.names = FALSE)

#Plot_6.2
write.csv(top9, "top9.csv", row.names = FALSE)

#Plot_6.3
DataFrame_tableau <- DataFrame_all %>%
  select(country_code, main_sector, raised_amount_usd) %>%
  group_by(country_code, main_sector) %>%
  summarise(count = n()) %>%
  mutate(rank = dense_rank(desc(count)))

write.csv(DataFrame_tableau, "DataFrame_tableau.csv", row.names = FALSE)
