###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.
#library(readxl) #for loading Excel files. My data is a CSV.
library(dplyr) #for data processing
library(here) #to set paths
library(lubridate) #for dates
library(tidyr) #for cleaning


#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","raw_data","ARP_raw.csv")

#load data. 
#note that for functions that come from specific packages (instead of base R)
# I often specify both package and function like so
#package::function() that's not required one could just call the function
#specifying the package makes it clearer where the function "lives",
#but it adds typing. You can do it either way.

rawdata <- read.csv(data_location)

#take a look at the data
dplyr::glimpse(rawdata)


#Next, I'll clean up the date column. Below, I set each column into the
#date the stool sample is produced and the time the stool sample was produced.
#I am also making sure the columns are read as dates and times as opposed
#to characters.
#For this, I will use the lubridate package.
cleandata <- rawdata %>% separate(recorded_date, into = c(
  "date_produced","time_produced"), sep = " ")
cleandata$date_produced <- mdy(cleandata$date_produced)
cleandata$time_produced <- hm(cleandata$time_produced)


#Looking at the column "pets," I can see that there is an excess of words for
#the "no" category, so I want to simplify that as "No".
#I'd also like to make an additional column of "Yes" pets and "No".

cleandata <- mutate(cleandata, has_pets = ifelse(grepl(
  "I do not live with any companion animals", pets), "No", "Yes"))

#Later, I will move this column to the right of the "pets" column (currently 11).

#I want to see all unique values in the pets column.
unique(cleandata$pets)
#After looking at this, I want to create a column that indicates if a
#person has a dog or a cat.

cleandata <- mutate(cleandata, cat_andor_dog =ifelse(grepl(
  "Cats|Dogs", pets), "Yes",
  "No"))

#Similarly to the animal data, I want to make a column for medical exposure that
#is only the yes and no of exposure to healthcare settings. Then I want to move
#this column to column 8.

cleandata <- mutate(cleandata, healthcare_exposure_yn =ifelse(grepl(
  "No, I do not have regular exposure to healthcare environments", healthcare_exposure), "No",
  "Yes"))


#Now I want to do some rearranging.
#Now I want to rearrange the columns so pets is in the 12th column and 
#cat_andor_dog is the 13th.
#I want to move the y/n exposure to healthcare environment regularly to
#beside healthcare_exposure

cleandata <- cleandata[,c(1:8, 46, 9:12, 44,45, 13:43)]

#I'm also removing the column half_total_isolates because I see no use for it.
#Delete time sample produced, age column (because there is already a numeric age column).

cleandata <- cleandata[-c(36, 3, 28)]


#Another thing I am considering is removing any columns that have "No" in the 
#ecoli_cultured column because we cannot study the antibiotic patterns of
#the samples that do not produce E. coli. I will revisit this idea later because
#it may be dangerous to delete large chunks of data like that.






#I decided to not use the following chunk but I might return to it it I see it valuable. 
#Next, I want to create a column for each antibiotic that has the proportion of
#resistant isolates out of the total for the sample. 
#I might delete this later. It may be too much information at this point.

#cleandata <- cleandata %>% mutate(
  #proportion_amp_resistant = amp_resistant_isolates / total_isolates) %>%
  #mutate(ceft_proportion_resistant = ceft_resistant_isolates / total_isolates) %>%
  #mutate(cipro_proportion_resistant = cipro_resistant_isolates / total_isolates) %>%
  #mutate(tetra_proportion_resistant = tetra_resistant_isolates / total_isolates) %>%
 # mutate(trimet_proportion_resistant = trimet_resistant_isolates / total_isolates)

#Ultimately I decided not to include this because it feels unnecessary for the clean data.
  




#Rename fully clean data as processeddata

processeddata <- cleandata

# save data as RDS
# I suggest you save your processed and cleaned data as RDS or RDA/Rdata files. 
# This preserves coding like factors, characters, numeric, etc. 
# If you save as CSV, that information would get lost.
# See here for some suggestions on how to store your processed data:
# http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata

# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(processeddata, file = save_data_location)


