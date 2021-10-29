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
library(tidymodels) #for modeling

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


#histogram for samples with greater than or equal to one isolate showing resistance 
#to a given antibiotic


mydata[c("amp_resistant_isolates")] %>% filter(amp_resistant_isolates >= 1) %>% 
  ggplot(aes(x=amp_resistant_isolates)) + geom_histogram() #ampicillin

mydata[c("ceft_resistant_isolates")] %>% filter(ceft_resistant_isolates >= 1) %>% 
  ggplot(aes(x=ceft_resistant_isolates)) + geom_histogram() #ceftriaxone

mydata[c("cipro_resistant_isolates")] %>% filter(cipro_resistant_isolates >= 1) %>% 
  ggplot(aes(x=cipro_resistant_isolates)) + geom_histogram() #ciprofloxacin

mydata[c("tetra_resistant_isolates")] %>% filter(tetra_resistant_isolates >= 1) %>% 
  ggplot(aes(x=tetra_resistant_isolates)) + geom_histogram() #tetracycline

mydata[c("trimet_resistant_isolates")] %>% filter(trimet_resistant_isolates >= 1) %>% 
  ggplot(aes(x=trimet_resistant_isolates)) + geom_histogram() #trimethoprim

#There is not a strong pattern amongst any of these.On an intitial glance,
#ciprofloxacin and ceftriaxone are skewed left and the rest are skewed right.

#Examine seasonality of resistance to different antibiotics,
#starting with ampicillin
mydata %>% ggplot(aes(y=amp_resistant_isolates, x = date_produced)) +  geom_point()
#ceftriaxone
mydata %>% ggplot(aes(y=ceft_resistant_isolates, x = date_produced)) +  geom_point()
#ciprofloxacin
mydata %>% ggplot(aes(y=cipro_resistant_isolates, x = date_produced)) +  geom_point()
#tetracycline
mydata %>% ggplot(aes(y=tetra_resistant_isolates, x = date_produced)) +  geom_point()
#trimethoprim
mydata %>% ggplot(aes(y=trimet_resistant_isolates, x = date_produced)) +  geom_point()

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

#abs_pattern <- mydata %>% subset(select=c(28:33)) %>%
  #mutate(amp_proportion_resistant = amp_resistant_isolates / total_isolates) %>%
  #mutate(ceft_proportion_resistant = ceft_resistant_isolates / total_isolates) %>%
  #mutate(cipro_proportion_resistant = cipro_resistant_isolates / total_isolates) %>%
  #mutate(tetra_proportion_resistant = tetra_resistant_isolates / total_isolates) %>%
  #mutate(trimet_proportion_resistant = trimet_resistant_isolates / total_isolates)

#adding columns that show proportion of resistant isolates out of the total isolates
#removing objects that have no e coli (ecoli_culured == no)
abs_pattern <- mydata %>% filter(total_isolates > 0) %>%
  mutate(amp_proportion_resistant = amp_resistant_isolates / total_isolates) %>%
  mutate(ceft_proportion_resistant = ceft_resistant_isolates / total_isolates) %>%
  mutate(cipro_proportion_resistant = cipro_resistant_isolates / total_isolates) %>%
  mutate(tetra_proportion_resistant = tetra_resistant_isolates / total_isolates) %>%
  mutate(trimet_proportion_resistant = trimet_resistant_isolates / total_isolates)

amp_tet_plot <- abs_pattern %>% ggplot(
  aes(x=amp_proportion_resistant, y = tetra_proportion_resistant)) +
  geom_point()

amp_cef_plot <- abs_pattern %>% ggplot(
  aes(x=amp_proportion_resistant, y = ceft_proportion_resistant)) +
  geom_point()

amp_cip_plot <- abs_pattern %>% ggplot(
  aes(x=amp_proportion_resistant, y = cipro_proportion_resistant)) +
  geom_point()

amp_tri_plot <- abs_pattern %>% ggplot(
  aes(x=amp_proportion_resistant, y = trimet_proportion_resistant)) +
  geom_point()

cef_cip_plot <- abs_pattern %>% ggplot(
  aes(x=ceft_proportion_resistant, y = cipro_proportion_resistant)) +
  geom_point()

cef_tet_plot <- abs_pattern %>% ggplot(
  aes(x=ceft_proportion_resistant, y = cipro_proportion_resistant)) +
  geom_point()

cef_tri_plot <- abs_pattern %>% ggplot(
  aes(x = ceft_proportion_resistant, y = trimet_proportion_resistant)) +
  geom_point()

cip_tet_plot <- abs_pattern %>% ggplot(
  aes(x = cipro_proportion_resistant, y = tetra_proportion_resistant)) +
  geom_point()

cip_tri_plot <- abs_pattern %>% ggplot(
  aes(x = cipro_proportion_resistant, y = trimet_proportion_resistant)) +
  geom_point()

tet_tri_plot <- abs_pattern %>% ggplot(aes(x=tetra_proportion_resistant, y=trimet_proportion_resistant)) +
  geom_point()

amp_tet_plot #there is a trend?
amp_cef_plot #flat line seems to be the trend
amp_cip_plot #flat line seems to be the trend
amp_tri_plot #appears to be a trend
cef_cip_plot #not enough resistance to draw patterns. A lot of zeros
cef_tet_plot #not enough ceftriaxone resistance to draw trendline
cef_tri_plot #not enough ceftriaxone resistance to draw a trendline
cip_tet_plot #not enough ciprofloxacin resistance to draw a trendline
cip_tri_plot # not enough ciprofloxacin resistance to draw a trendline
tet_tri_plot #maybe a trend?

#noteworthy trends:
#amp_tet_plot
#amp_tri_plot
#tet_tri_plot


#Wow, there kind of seem to be patterns here, but also in other ways there
#is no correlation. I had a thought that we could remove any zero values, but 
#that is a really bad idea because it is straight up removing data to make 
#a pattern, so I will not do that. You can kind of see a diagonal line, but
#I do not think we can say anything because of the excessive outliers.

#Because ceftriaxone and ciprofloxacin do not have enough resistance
#to analyze thoroughly in my opinion, we will focus on ampicillin,
#tetracycline, and trimethoprim.

#I want to see if ampicillin resistance and number of days an individual ate
#fruit that week are correlated

#AMP AND PRODUCE
amp_rawproduce <- mydata %>% subset(select = c(1,24,28, 29)) %>%
  mutate(amp_prop_res = amp_resistant_isolates / total_isolates)
amp_rawproduce$eat_raw_fruits_or_vegetables_past_week <- as.character(amp_rawproduce$eat_raw_fruits_or_vegetables_past_week)

abs_pattern %>% ggplot(aes(eat_raw_fruits_or_vegetables_past_week, amp_prop_res)) + 
  geom_point()


amp_produce_plot <- amp_rawproduce %>% ggplot(aes(eat_raw_fruits_or_vegetables_past_week, amp_prop_res)) + 
  geom_point()
amp_produce_plot

#TET AND PRODUCE
tet_rawproduce <- mydata %>% subset(select = c(1,24,28, 29)) %>%
  mutate(tet_prop_res = tet_resistant_isolates / total_isolates)
amp_rawproduce$eat_raw_fruits_or_vegetables_past_week <- as.character(amp_rawproduce$eat_raw_fruits_or_vegetables_past_week)

amp_produce_plot <- amp_rawproduce %>% ggplot(aes(eat_raw_fruits_or_vegetables_past_week, amp_prop_res)) + 
  geom_point()
amp_produce_plot


#I know that number of days fruit was eaten is categorical and not numeric,
#but at this time I do not have a better way to approach this.








######################################
#Data fitting/statistical analysis
######################################

# first line: fit linear model ampicillin and tetracycline
# second line: place results from fit into a data frame with tidy function
# third line: look at fit results
amptetfit <- lm(amp_proportion_resistant ~ tetra_proportion_resistant, abs_pattern)  
amptettable <- broom::tidy(amptetfit)
print(amptettable)

# first line: fit linear model ampicillin and trimethoprim
# second line: place results from fit into a data frame with tidy function
# third line: look at fit results
amptrifit <- lm(amp_proportion_resistant ~ trimet_proportion_resistant, abs_pattern)  
amptritable <- broom::tidy(amptrifit)
print(amptritable)

# first line: fit linear model tetracycline and trimethoprim
# second line: place results from fit into a data frame with tidy function
# third line: look at fit results
tettrifit <- lm(tetra_proportion_resistant ~ trimet_proportion_resistant, abs_pattern)  
tettritable <- broom::tidy(tettrifit)
print(tettritable)

# save ampicillin tetracycline fit results table  
amptettable_file = here("results", "amptettable.rds")
saveRDS(amptettable, file = amptettable_file)

# save ampicillin trimethoprim fit results table  
amptritable_file = here("results", "amptritable.rds")
saveRDS(amptritable, file = amptritable_file)

# save tetracycline trimethoprim fit results table  
tettritable_file = here("results", "tettritable.rds")
saveRDS(tettritable, file = tettritable_file)

#save ampicillin tetracycline figure results
amp_tet_plot_figure = here("results", "amp_tet_plot")
saveRDS(amp_tet_plot, file = amp_tet_plot_figure)

#save ampicillin trimethoprim figure results
amp_tet_plot_figure = here("results", "amp_tri_plot")
saveRDS(amp_tri_plot, file = amp_tri_plot_figure)

#save tetracycline trimethoprim figure results
amp_tet_plot_figure = here("results", "tet_tri_plot")
saveRDS(tet_tri_plot, file = tet_tri_plot_figure)

#save ampicillin-raw produce figure  
amp_produce_plot_figure = here("results", "amp_produce_plot")
saveRDS(amp_produce_plot, file = amp_produce_plot_figure)

#################################################################
#####################################################################
#Modeling (Part 3)

lm_mod <- linear_reg() %>% set_engine("lm")

#Fits another linear model to the continuous outcome
#(ampicillin resistance proportion) using all (important) predictors of interest.
ampprop_and_predictors <- abs_pattern %>%
  subset(select = c(amp_proportion_resistant,
                    regular_healthcare_exposure,
                    healthcare_exposure_yn,
                    regular_animal_exposure,
                    has_pets,
                    cat_andor_dog,
                    untreated_recreational_water_exposure_past_week,
                    untreated_recreational_water_exposure_past_month,
                    treated_recreational_water_exposure_past_month,
                    treated_recreational_water_exposure_past_week,
                    eat_poultry_past_week,
                    eat_pork_or_beef_past_week,
                    eat_fish_or_shellfish_past_week,
                    eat_dairy_past_week,
                    eat_raw_fruits_or_vegetables_past_week))
ampprop_and_predictors


lm_ampall_model <- lm_mod %>% fit(amp_proportion_resistant ~ ., data = ampprop_and_predictors)
ampallpredictors_lm <- tidy(lm_ampall_model)
ampallpredictors_lm

ampallpredictors_table <- broom::tidy(lm_ampall_model)
ampallpredictors_table

#Looking at this, only dairy seems to have statistical significance. 
#Again, these are p-values, so approach with some skepticism. 

abs_pattern %>% ggplot(aes(x = eat_dairy_past_week, y = amp_proportion_resistant)) +
  geom_point()
amp_dairy_yn_plot <- abs_pattern %>% mutate(any_dairy_week_yn = eat_dairy_past_week > 0) %>% 
  ggplot(aes(x = any_dairy_week_yn, y = amp_proportion_resistant)) +
  geom_boxplot()

#plotting but only for isolates that show SOME ampicillin resistance.
#I'm just doing this for exploration's sake.
abs_pattern %>% mutate(any_dairy_week_yn = eat_dairy_past_week > 0) %>% 
  filter(amp_proportion_resistant > 0) %>%
  ggplot(aes(x = any_dairy_week_yn, y = amp_proportion_resistant)) +
  geom_boxplot() #the mean is a lot higher


#Tetracycline
tetprop_and_predictors <- abs_pattern %>%
  subset(select = c(tetra_proportion_resistant,
                    regular_healthcare_exposure,
                    healthcare_exposure_yn,
                    regular_animal_exposure,
                    has_pets,
                    cat_andor_dog,
                    untreated_recreational_water_exposure_past_week,
                    untreated_recreational_water_exposure_past_month,
                    treated_recreational_water_exposure_past_month,
                    treated_recreational_water_exposure_past_week,
                    eat_poultry_past_week,
                    eat_pork_or_beef_past_week,
                    eat_fish_or_shellfish_past_week,
                    eat_dairy_past_week,
                    eat_raw_fruits_or_vegetables_past_week))
tetprop_and_predictors

lm_tetall_model <- lm_mod %>% fit(tetra_proportion_resistant ~ ., data = tetprop_and_predictors)
tetallpredictors_lm <- tidy(lm_tetall_model)
tetallpredictors_lm
#no significance for tetracycline

tetallpredictors_table <- broom::tidy(lm_tetall_model)
tetallpredictors_table

#Trimethoprim
triprop_and_predictors <- abs_pattern %>%
  subset(select = c(trimet_proportion_resistant,
                    regular_healthcare_exposure,
                    healthcare_exposure_yn,
                    regular_animal_exposure,
                    has_pets,
                    cat_andor_dog,
                    untreated_recreational_water_exposure_past_week,
                    untreated_recreational_water_exposure_past_month,
                    treated_recreational_water_exposure_past_month,
                    treated_recreational_water_exposure_past_week,
                    eat_poultry_past_week,
                    eat_pork_or_beef_past_week,
                    eat_fish_or_shellfish_past_week,
                    eat_dairy_past_week,
                    eat_raw_fruits_or_vegetables_past_week))
triprop_and_predictors

lm_triall_model <- lm_mod %>% fit(trimet_proportion_resistant ~ ., data = triprop_and_predictors)
triallpredictors_lm <- tidy(lm_triall_model)
triallpredictors_lm
#no significance for trimethoprim

triallpredictors_table <- broom::tidy(lm_triall_model)
triallpredictors_table 


#Fits another linear model to the 
#categorical outcome using all (important) predictors of interest.
log_model <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

abs_pattern_yndairy <- abs_pattern %>% mutate(any_dairy_week_yn = eat_dairy_past_week > 0)
abs_pattern_yndairy <- abs_pattern_yndairy$any_dairy_week_yn %>% as.factor()

#linear regression between ampicillin and dairy (as y/n)
ampdairy_lm_fit <- 
  lm_mod %>% 
  fit(amp_proportion_resistant ~ any_dairy_week_yn, data = abs_pattern_yndairy)
ampdairy_lm <- tidy(ampdairy_lm_fit)
ampdairy_lm

ampallpredictors_table
tetallpredictors_table
triallpredictors_table

ampallpredictors_figure = here("results", "ampallpredictors_table")
saveRDS(ampallpredictors_table, file = ampallpredictors_figure)

tetallpredictors_figure = here("results", "tetallpredictors_table")
saveRDS(tetallpredictors_table, file = tetallpredictors_figure)

triallpredictors_figure = here("results", "triallpredictors_table")
saveRDS(triallpredictors_table, file = triallpredictors_figure)
