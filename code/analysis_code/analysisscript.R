###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving



library(dplyr) #for data wrangling
library(reshape) #for reshaping and melting data

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

######################################
#Data exploration/description
######################################
#I'm using basic R commands here.
#Lots of good packages exist to do more.
#For instance check out the tableone or skimr packages

#summarize data 
mysummary = summary(mydata)

#look at summary
print(mysummary)


#Let's look at demographics for the study.
mydata %>% ggplot(aes(x = race)) +
  geom_bar()
#^The race data shows that a majority of the participants are white.
mydata %>% ggplot(aes(x = biological_sex)) +
  geom_bar()
#^This biological sex data shows that, when observing males and females,
#the participant group is primarily female.
mydata %>% ggplot(aes(x = numeric_age)) +
  geom_bar()
#^This age data shows that the age of the participants skews
#strongly left/young.

#Let's look at the food data. I want to make a separate data set of
#food information so I can make a melted data set.
#The figure at the end shows the number of participants that ate
#a typ

fooddata <- mydata[,c(1,20:24)] %>% 
  melt(id=c("fake_id")) %>%
  subset(select=c("variable", "value"))
fooddata$value <- as.character(fooddata$value)
fooddata <- fooddata %>% mutate(value, count = 1) %>%
  group_by(variable, value) %>%
  summarise(num_participants = sum(count))
fooddata$value <- as.numeric(fooddata$value)
  
  
fooddata_plot <- fooddata %>% ggplot(aes(x=value, y = num_participants
                        , fill = variable)) +
  geom_bar(position="dodge", stat="identity") +
  xlab("Number days a food type was consumed in last week")
fooddata_plot

#On another note, let's compare a participant's tetracycline resistance
#with its ampicillin resistance. This will be the proportion of E.coli
#isolates.

abs_pattern <- mydata %>% subset(select=c(28:33)) %>%
  mutate(amp_proportion_resistant = amp_resistant_isolates / total_isolates) %>%
  mutate(ceft_proportion_resistant = ceft_resistant_isolates / total_isolates) %>%
  mutate(cipro_proportion_resistant = cipro_resistant_isolates / total_isolates) %>%
  mutate(tetra_proportion_resistant = tetra_resistant_isolates / total_isolates) %>%
  mutate(trimet_proportion_resistant = trimet_resistant_isolates / total_isolates)

amp_tet_plot <- abs_pattern %>% ggplot(
  aes(x=amp_proportion_resistant, y = tetra_proportion_resistant)) +
  geom_point()

#Wow, there kind of seem to be patterns here, but also in other ways there
#is no correlation. I had a thought that we could remove any zero values, but 
#that is a really bad idea because it is straight up removing data to make 
#a pattern, so I will not do that. You can kind of see a diagonal line, but
#I do not think we can say anything because of the excessive outliers.


#I want to see if ampicillin resistance and number of days an individual ate
#fruit that week are correlated

amp_rawproduce <- mydata %>% subset(select = c(1,24,28, 29)) %>%
  mutate(amp_prop_res = amp_resistant_isolates / total_isolates)
amp_rawproduce$eat_raw_fruits_or_vegetables_past_week <- as.character(amp_rawproduce$eat_raw_fruits_or_vegetables_past_week)

amp_produce_plot <- amp_rawproduce %>% ggplot(aes(eat_raw_fruits_or_vegetables_past_week, amp_prop_res)) + 
  geom_point()
#I know that number of days fruit was eaten is categorical and not numeric,
#but at this time I do not have a better way to approach this.


#None of this data is ready to put in the manuscript.
#save data frame table to file for later use in manuscript
#summarytable_file = here("results", "summarytable.rds")
#saveRDS(summary_df, file = summarytable_file)

######################################
#Data fitting/statistical analysis
######################################

# fit linear model
amptetfit <- lm(amp_proportion_resistant ~ tetra_proportion_resistant, abs_pattern)  

# place results from fit into a data frame with the tidy function
amptettable <- broom::tidy(amptetfit)

#look at fit results
print(amptettable)

# save ampicillin tetracycline fit results table  
amptettable_file = here("results", "amptettable.rds")
saveRDS(amptettable, file = amptettable_file)

#save ampicillin tetracycline figure results
amp_tet_plot_figure = here("results", "amp_tet_plot")
saveRDS(amp_tet_plot, file = amp_tet_plot_figure)


#save ampicillin-raw produce figure  
amp_produce_plot_figure = here("results", "amp_produce_plot")
saveRDS(amp_produce_plot, file = amp_produce_plot_figure)
