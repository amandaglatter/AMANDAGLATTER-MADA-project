###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#Sometimes I get errors loading the data, in which case I do the following:
#Restart your R session
#Make sure the working directory is set to the source file location
# click "session", "set working directory", "source file location."

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


amp_hist <- mydata[c("amp_resistant_isolates")] %>% filter(amp_resistant_isolates >= 1) %>% 
  ggplot(aes(x=amp_resistant_isolates)) + geom_histogram() #ampicillin

ceft_hist <- mydata[c("ceft_resistant_isolates")] %>% filter(ceft_resistant_isolates >= 1) %>% 
  ggplot(aes(x=ceft_resistant_isolates)) + geom_histogram() #ceftriaxone

cipro_hist <- mydata[c("cipro_resistant_isolates")] %>% filter(cipro_resistant_isolates >= 1) %>% 
  ggplot(aes(x=cipro_resistant_isolates)) + geom_histogram() #ciprofloxacin

tetra_hist <- mydata[c("tetra_resistant_isolates")] %>% filter(tetra_resistant_isolates >= 1) %>% 
  ggplot(aes(x=tetra_resistant_isolates)) + geom_histogram() #tetracycline

trimet_hist <- mydata[c("trimet_resistant_isolates")] %>% filter(trimet_resistant_isolates >= 1) %>% 
  ggplot(aes(x=trimet_resistant_isolates)) + geom_histogram() #trimethoprim

amp_hist
ceft_hist
cipro_hist
tetra_hist
trimet_hist

#There is not a strong pattern amongst any of these.On an intitial glance,
#ciprofloxacin and ceftriaxone are skewed left and the rest are skewed right.

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
  xlab("Number days a food type was consumed in last week") +
  theme(legend.key.size = unit(0.25, 'cm'))
fooddata_plot


#On another note, let's compare a participant's tetracycline resistance
#with its ampicillin resistance. This will be the proportion of E.coli
#isolates.

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
  geom_point() + 
  xlab("Proportion of E. coli resistant to ampicillin") +
  ylab("Proportion of E. coli resistant to tetracycline")

amp_cef_plot <- abs_pattern %>% ggplot(
  aes(x=amp_proportion_resistant, y = ceft_proportion_resistant)) +
  geom_point()

amp_cip_plot <- abs_pattern %>% ggplot(
  aes(x=amp_proportion_resistant, y = cipro_proportion_resistant)) +
  geom_point()

amp_tri_plot <- abs_pattern %>% ggplot(
  aes(x=amp_proportion_resistant, y = trimet_proportion_resistant)) +
  geom_point()+ 
  xlab("Proportion of E. coli resistant to ampicillin") +
  ylab("Proportion of E. coli resistant to trimethoprim")

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
  geom_point() + 
  xlab("Proportion of E. coli resistant to tetracycline") +
  ylab("Proportion of E. coli resistant to trimethoprim")

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

amp_rawproduce %>% ggplot(aes(eat_raw_fruits_or_vegetables_past_week, amp_prop_res)) + 
  geom_point()


amp_produce_plot <- amp_rawproduce %>% ggplot(aes(eat_raw_fruits_or_vegetables_past_week, amp_prop_res)) + 
  geom_point()
amp_produce_plot

#TET AND PRODUCE
tet_rawproduce <- mydata %>% subset(select = c(1,24,28, 32)) %>%
  mutate(tet_prop_res = tetra_resistant_isolates / total_isolates)
tet_rawproduce$eat_raw_fruits_or_vegetables_past_week <- as.character(tet_rawproduce$eat_raw_fruits_or_vegetables_past_week)

tet_produce_plot <- tet_rawproduce %>% ggplot(aes(eat_raw_fruits_or_vegetables_past_week, tet_prop_res)) + 
  geom_point()
tet_produce_plot

#Let's look at dairy's relationship with ampicillin and tetracycline
#After looking at the results, I wonder if there is a better way to visualize
#AMP AND dairy
amp_dairy <- mydata %>% subset(select = c(1,23,28, 29)) %>%
  mutate(amp_prop_res = amp_resistant_isolates / total_isolates)
amp_dairy$eat_dairy_past_week <- as.character(amp_dairy$eat_dairy_past_week)

amp_dairy %>% ggplot(aes(eat_dairy_past_week, amp_prop_res)) + 
  geom_point()


#TET AND dairy
tet_dairy <- mydata %>% subset(select = c(1,23,28, 32)) %>%
  mutate(tet_prop_res = tetra_resistant_isolates / total_isolates)
tet_dairy$eat_dairy_past_week <- as.character(tet_dairy$eat_dairy_past_week)

tet_dairy %>% ggplot(aes(eat_dairy_past_week, tet_prop_res)) + 
  geom_point()


#Let's look at shellfish's relationship with ampicillin and tetracycline
#After looking at the results, I wonder if there is a better way to visualize
#AMP AND shellfish and fish
amp_fish <- mydata %>% subset(select = c(1,22,28, 29)) %>%
  mutate(amp_prop_res = amp_resistant_isolates / total_isolates)
amp_fish$eat_fish_or_shellfish_past_week <- as.character(amp_fish$eat_fish_or_shellfish_past_week)

amp_fish %>% ggplot(aes(eat_fish_or_shellfish_past_week, amp_prop_res)) + 
  geom_point()


#TET AND fish
tet_fish <- mydata %>% subset(select = c(1,22,28, 32)) %>%
  mutate(tet_prop_res = tetra_resistant_isolates / total_isolates)
tet_fish$eat_fish_or_shellfish_past_week <- as.character(tet_dairy$eat_fish_or_shellfish_past_week)

tet_fish %>% ggplot(aes(eat_fish_or_shellfish_past_week, tet_prop_res)) + 
  geom_point()
#Because there seems to be a pretty good split between no fish consumption and
#any fish comsumption, this might be a good thing to model.



#I know that number of days fruit was eaten is categorical and not numeric,
#but at this time I do not have a better way to approach this.


#save exploratory images
#Save figure comparing proportions of ampicillin and
#tetracycline resistance
amp_tet_figure = here("results","amp_tet_plot.png")
ggsave(filename = amp_tet_figure, plot=amp_tet_plot)

amp_tri_figure = here("results", "amp_tri_plot.png")
ggsave(filename = amp_tri_figure, plot = amp_tri_plot)

tet_tri_figure = here("results", "tet_tri_plot.png")
ggsave(filename = tet_tri_figure, plot = tet_tri_plot)

fooddata_figure = here("results","fooddata_figure.png")
ggsave(filename = fooddata_figure, plot=fooddata_plot)






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

#Looking at this, dairy seems to have the most statistical significance.
#Again, these are p-values, so approach with some skepticism. 

#make a df with columns that simply say if the person consumed a certain food
#type in the last week or not.
any_eachfood_yn <- abs_pattern[c(1,20:24,28:48)] %>% 
  mutate(any_fish_yn = eat_fish_or_shellfish_past_week > 0,
         any_poultry_yn = eat_poultry_past_week > 0,
         any_beefpork_yn = eat_pork_or_beef_past_week > 0,
         any_dairy_yn = eat_dairy_past_week > 0,
         any_rawproduce_yn = eat_raw_fruits_or_vegetables_past_week > 0) 

##next: change any NA value to zero
any_eachfood_yn$any_fish_yn <- any_eachfood_yn$any_fish_yn %>%  replace(is.na(.), FALSE)
any_eachfood_yn$any_poultry_yn <- any_eachfood_yn$any_poultry_yn %>%  replace(is.na(.), FALSE)
any_eachfood_yn$any_beefpork_yn <- any_eachfood_yn$any_beefpork_yn %>%  replace(is.na(.), FALSE)
any_eachfood_yn$any_dairy_yn <- any_eachfood_yn$any_dairy_yn %>%  replace(is.na(.), FALSE)
any_eachfood_yn$any_rawproduce_yn <- any_eachfood_yn$any_rawproduce_yn %>%  replace(is.na(.), FALSE)

any_eachfood_yn


###############DAIRY
abs_pattern %>% ggplot(aes(x = eat_dairy_past_week, y = amp_proportion_resistant)) +
  geom_point()

amp_dairy_plot <- any_eachfood_yn %>%
  ggplot(aes(x = any_dairy_yn, y = amp_proportion_resistant)) +
  geom_boxplot() + 
  xlab("Consumed Dairy in last Week") +
  ylab("Proportion Resistant Ampicillin Isolates")+
  ggtitle("Proportion of Ampicillin resistance vs. dairy consumption")+
  labs(subtitle = "For all participants with culturable E. coli")
amp_dairy_plot

summary(any_eachfood_yn) #there are only 14 participants who consumed no dairy.


#plotting but only for isolates that show SOME ampicillin resistance.
#I'm just doing this for exploration's sake.

any_amp_dairy_plot <- any_eachfood_yn %>%
  filter(amp_proportion_resistant > 0) %>%
  ggplot(aes(x = any_dairy_yn, y = amp_proportion_resistant)) +
  geom_boxplot() + 
  xlab("Consumed Dairy in last Week") +
  ylab("Proportion Resistant Ampicillin Isolates") +
  ggtitle("Proportion of Ampicillin resistance vs. dairy consumption")+
  labs(subtitle = "For participants who showed SOME ampicillin resistance")#the mean is a lot higher
any_amp_dairy_plot
#notes: for participants who did not consume dairy, 
#there are only 14 than 25 participants. This number decreases even further when 
#you consider the number of participants who showed any E. coli resistance.



####################SHELLFISH
#I want to look at shellfish because there is a better distribution of 
#participants who consumed no vs. some (shell)fish in the last week. 
#For example, if you look at dairy, there are only 14 individuals who consumed no dairy.
#For shellfish, there are greater than 100.
abs_pattern %>% ggplot(aes(x = eat_fish_or_shellfish_past_week, y = amp_proportion_resistant)) +
  geom_point()

#AMP and FISH
amp_fish_yn_plot <- any_eachfood_yn %>%  ggplot(aes(x = any_fish_yn, y = amp_proportion_resistant)) +
  geom_boxplot()+
  ggtitle("Proportion of Ampicillin resistance vs. Fish consumption")+
  labs(subtitle = "For participants all participants with culturable E. coli")
amp_fish_yn_plot

#plotting but only for isolates that show SOME ampicillin resistance.
#I'm just doing this for exploration's sake.
some_amp_fish_yn_plot <- any_eachfood_yn %>%  filter(amp_proportion_resistant > 0) %>% 
  ggplot(aes(x = any_fish_yn, y = amp_proportion_resistant)) +
  geom_boxplot() +
  ggtitle("Proportion of Ampicillin resistance vs. Fish consumption")+
  labs(subtitle = "For participants who showed SOME ampicillin resistance")
  #the false mean is a higher, but is this significant?
some_amp_fish_yn_plot 
#the false mean is a higher, but is this significant? 
#am I messing with the data too much?



#fit looking at all resistance patterns (not just any resistance) comparing
#fish and ampicillin resistance
ampfish_lm_fit <- 
  lm_mod %>% 
  fit(amp_proportion_resistant ~ any_fish_yn, data = any_eachfood_yn)
ampfish_lm <- tidy(ampfish_lm_fit)
ampfish_lm
# P value does not show a significant relationship

#########BEEF AND PORK

amp_beefpork_yn_plot <- any_eachfood_yn %>%  ggplot(aes(x = any_beefpork_yn, y = amp_proportion_resistant)) +
  geom_boxplot()+
  ggtitle("Proportion of Ampicillin resistance vs. Beef and/or Pork consumption")+
  labs(subtitle = "For participants all participants with culturable E. coli")
amp_beefpork_yn_plot

#plotting but only for isolates that show SOME ampicillin resistance.
#I'm just doing this for exploration's sake.
some_amp_beefpork_yn_plot <- any_eachfood_yn %>%  filter(amp_proportion_resistant > 0) %>% 
  ggplot(aes(x = any_beefpork_yn, y = amp_proportion_resistant)) +
  geom_boxplot() +
  ggtitle("Proportion of Ampicillin resistance vs. Beef and/or Pork consumption")+
  labs(subtitle = "For participants who showed SOME ampicillin resistance")
#the false mean is a higher, but is this significant?
some_amp_beefpork_yn_plot 
#the false mean is a higher, but is this significant? 
#am I messing with the data too much?

##############################################################################
#############################################################################
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
#no significance for trimethoprim. Dairy consumption is the most significant factor.

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
  fit(amp_proportion_resistant ~ any_dairy_yn, data = any_eachfood_yn)
ampdairy_lm <- tidy(ampdairy_lm_fit)
ampdairy_lm

#linear regression between SOME ampicillin resistance and dairy (as y/n)
any_ampdairy <- any_eachfood_yn %>%
  filter(amp_proportion_resistant > 0)

any_ampdairy_lm_fit <- lm_mod %>% fit(amp_proportion_resistant ~ any_dairy_yn, data = any_ampdairy)
any_ampdairy_lm <- tidy(any_ampdairy_lm_fit)
any_ampdairy_lm

ampallpredictors_table
tetallpredictors_table
triallpredictors_table

ampallpredictors_figure = here("results", "ampallpredictors_table")
saveRDS(ampallpredictors_table, file = ampallpredictors_figure)

tetallpredictors_figure = here("results", "tetallpredictors_table")
saveRDS(tetallpredictors_table, file = tetallpredictors_figure)

triallpredictors_figure = here("results", "triallpredictors_table")
saveRDS(triallpredictors_table, file = triallpredictors_figure)



