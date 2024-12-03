library(ggplot2)
library(tidyverse)
dat = read.csv("data/HSR2_24-06-04_1210.csv",na.strings = "NA")
head(dat)
str(dat$a1)
str(dat)
class(dat$a1)
levels(dat$a1)
table(dat$a1)
table(dat$pro_name)
levels(dat$pro_name) # our class is not a factor
table(dat$cbo_name)
##################### Exploring 
n_distinct(dat)
n_distinct(dat) == dim(dat)
dat |> group_by(cbo_name) |>tally()
table(dat$cbo_name)
table(dat$a1)
dat |> group_by(a1) |>tally()

table(dat$pro_name)
dat |> group_by(pro_name) |> tally()

##################################################################################################
## Data Cleaning and Preparation 
##################################################################################################

# REMOVING TEST CASE--------------------------------------------------
# Number of test case in the data set

pid_test <- dat |> filter(if_any(everything(), ~ grepl("test", .x, ignore.case=TRUE))) |> pull(pid)
pid_test #4
dim(dat)[1] #216

## Removing test case 
'%!in%' <- function(x,y)!('%in%'(x,y))
dat <- dat |> filter(pid %!in% pid_test)

dim(dat)[1] #213

### NOTES -----------------------------------------------------------
# 1. With single response question if the participants did not answer, it will be marked as NA 
# 2. With multiple-response question,if participants did not answer, it will me marked as 0. 
# For those question, we will have extended question with suffix _96 for "Other" with yes, no. If it is yes (1)
# We will follow up with specify that factor 
# We have no benefit/ useful info from the participants whose answers for all question is NA, we should remove them.
# Due to the default answer is 0 for no response for multiple-response questions, I use criteria of removing observations
# is that if he/she has NA for entire answers from a1 to k4 (index from 22 to 156)

### REMOVING PARTICIPANTS WHOSE ANSWERS FROM A1 TO K4 ARE ALL NA
pid_excl <- dat |> filter(if_all(c(a1:k4), is.na)) |> pull(pid)
length(pid_excl) #93

dat <- dat|> filter(pid %!in% pid_excl)

dim(dat)[1] #120


# CONSENT COMPLETE VARIABLE--------------------------------------------------
## The number of consent_complete is 0, not finished
dat |> select(consent_complete) |> table() # 5 cases not completed consent.
#dat |> filter(consent_complete ==0) |> 
  #select(matches("consent", ignore.case=TRUE), matches("survey_complete", ignore.case=TRUE)) |> view()
## In 5 cases where complete_consent are 0, one participant consented to both form 1, and 2. 
## The Other 4 cases marked all consent questions as 0. Need to go back to check the form.
## Since all five participant completed the survey, we keep them in for now.  


# SURVEY COMPLETE VARIABLE ------------------------------------------------
## Survey complete marked as 0: 14| marked as 2: 106
dat |> select(survey_complete) |> table()
#dat |> filter(survey_complete ==0) |> view()

## Confusion about how the variable is assigned values.

# DUPLICATE CONSUMER NAME ------------------------------------------------
pid_dup_excl <- c("129", "50", "220", "248", "101", "100", "207", "25")

length(pid_dup_excl)

## There are 7 consumers that their names repeat more than two
dat |> select(consumer_name) |> mutate(consumer_name = tolower(str_trim(consumer_name))) |>
  filter(consumer_name !="") |>
  group_by(consumer_name) |> filter(n()>1) |> table()

  
## Examine the duplicates individually 
### 1. Alexa roman: pid 129 & 149 same email, phone numbers are one digit different, both survey and consent completed
### The surveys were done in two different locations, on two different day. 
### Drop 129 but need to reconsider later
  #dat |> filter(grepl("alexa roman", consumer_name, ignore.case=TRUE)) |> view() 
  #dat |> filter(grepl("alexa roman", consumer_name, ignore.case=TRUE)) |>
    #select(pid, contains("time", ignore.case=TRUE)) |> view()
  
### 2. george thomas:pid 50& 59. pid 50: survey comple marked as 0, pid 59: marked as 2
#### Two records were on same day, different time
### pid 50 happened at 2023-12-01 16:13:41; pid 59 2023-12-01 21:18:27. 
  #dat |> filter(grepl("george thomas", consumer_name, ignore.case=TRUE)) |> view() 

  
### 3. marcelle bondima: pid(220, 225, 248) has three records at three different dates. All 3 records had 0 for survey complete
### Pid 220 ended at question a15; The pid 225 ended at question h20; The pid 248 stopped at b12.
  #dat |> filter(grepl("marcelle bondima", consumer_name, ignore.case=TRUE)) |> view()

### 4. margaret martinez: pid 101, 153. It seems the two records were entried twice, the answers are identical. 
  # Drop pid 101, keep 153 because pid 101 missed signature picture and there few answers marked NA
  #dat |> filter(grepl("margaret martinez", consumer_name, ignore.case=TRUE)) |> view()


### 5. mayda cantos, pid 100 & 152. This seem two records acidentally entered twice, the answers are identical. 
  # Drop pid 100 because it missed signature picture and there few answers marked NA 
  #dat |> filter(grepl("mayda cantos", consumer_name, ignore.case=TRUE)) |> view()
  
  
### 6. reyes gerena, pid 207, 209. The surveys were conducted on two different date.Both survey_complete ==2
  # There are few answers were not matches btween two surveys. Drop one for now. 
  #dat |> filter(grepl("reyes gerena", consumer_name, ignore.case=TRUE)) |> view()

### 7. shanika jones, pid 25 & 31. The surveys were on the same day and time frame were very close to each other. 
  # Drop pid 25 because it was marked survey_complete ==0
  
  #dat |> filter(grepl("shanika jones", consumer_name, ignore.case=TRUE)) |> view()
  
## Removing duplicates-------------------------------------
  dim(dat)[1]#120
  dat <- dat |> filter(pid %!in% pid_dup_excl)
  dim(dat)[1] #112
  
## REMOVE SOME IRRELEVANT VARIABLES
  hss <- dat
  dim(hss) #112; 282
  dim(dat) 

hss$redcap_survey_identifier <- NULL
hss$consentform3___1 <- NULL
hss$consentform3___2 <- NULL
hss$consentform3___3 <- NULL
hss$cbo_name1 <- NULL  
hss$cbo_name2 <- NULL  
hss$cbo_name3 <- NULL
str(hss)

## CATEGORICAL VARIABLES
### CBO NAMES ---------------------------------------------------------------
class(hss$cbo_name)
table(hss$cbo_name)
levels(hss$cbo_name)
hss$cbo_name <- recode_factor(dat$cbo_name, "1"="HCCI", "2"="HOPE", "3"="PC Setting")
table(hss$cbo_name)
hss$cbo_name <- factor(hss$cbo_name, levels = c ("PC Setting","HCCI","HOPE"))
table(hss$cbo_name)

## PRO_NAME ----------------------------------------------------------------
class(hss$pro_name)

### Provider names seem to have many typo. 

### Vidya Sharma has different variations
hss |> filter(grepl("sharma", pro_name, ignore.case=TRUE)) |> select(pid, pro_name)

## Testing to replace all the variation of Vidya name in to Vidya Sharma
hss <- hss |> mutate(pro_name = if_else(grepl("sharma", pro_name, ignore.case=TRUE), "Vidya Sharma", pro_name))

hss|> filter(pro_name =="Vidya Sharma") |> select(pro_name) |> table() ## 11

## HCCI
### There are 2 variations of HCCI
hss |> filter(grepl("hcci", pro_name, ignore.case=TRUE))|> n_distinct()
hss <- hss |> mutate(pro_name = if_else(grepl("hcci", pro_name, ignore.case=TRUE),"HCCI", pro_name))

## NA in the Pro_name
## NA values and random word happened 18 times in pro_name
hss |> filter(grepl("^(NA|N/A|none|idk)$", pro_name, ignore.case=TRUE)) |> select(pro_name) |> table()

# Manipulating all empty into NA, there would be 19 + 4 NA is 23 NA. Note that, in this case I use NA not "NA"
 hss <- hss|> mutate(pro_name=if_else(grepl("^(NA|N/A|none|idk)$", pro_name, ignore.case=TRUE),NA,pro_name))
 hss |> filter(is.na(pro_name)) |> dim()
 
## Rodini, Mudunuru
hss <- hss |> mutate(pro_name = if_else(grepl("rodini", pro_name, ignore.case=TRUE),"Rodini Shiamilis", pro_name))
   
hss <- hss |> mutate(pro_name = if_else(grepl("Mudunuru", pro_name, ignore.case=TRUE),"Mudunuru", pro_name))

## white space cause a trouble for Spencer and Ryan  name
hss <- hss |> mutate(pro_name = if_else(grepl("spencer", pro_name, ignore.case=TRUE),"Spencer Washington", pro_name))
 
hss <- hss |> mutate(pro_name = if_else(grepl("ryan", pro_name, ignore.case=TRUE),"Ryan Health", pro_name)) 



 ## if want to show NA on the table we need to do 
 table(hss$pro_name, useNA = "ifany")
 
 ## After correcting typo, now move to the factor part
 ## By using levels function we can see if there is any typo in the values. It is a handy tool
 
levels(hss$pro_name)
levels(as.factor(hss$pro_name)) #22
table(hss$pro_name)

################################
# ggplot(data=dat,mapping=aes(x=cbo_name, y=a1)+
#          stat_summary(fun = "mean")
# 
# ggplot(data = dat, aes(x=as.factor(a1)))+
#   geom_bar()+
#   scale_x_discrete(drop =TRUE)
# 
# ggplot(data=dat, aes(x=a1))+
#   geom_histogram() 
# 
# summary(dat$a1)
