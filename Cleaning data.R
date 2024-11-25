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


pid_test <- dat |> filter(if_any(everything(), ~ grepl("test", .x, ignore.case=TRUE))) |> pull(pid)
#dat |> filter(if_any(everything(), ~str_detect(.x, regex("test", ignore_case=TRUE)))) |> pull(pid)

##Removing test case 
'%!in%' <- function(x,y)!('%in%'(x,y))
dat <- dat |> filter(pid %!in% pid_test)

## Checking on duplicate consumers by looking at name
dat |> select(consumer_name) |> mutate(consumer_name = tolower(str_trim(consumer_name))) |>
  filter(consumer_name !="") |>
  group_by(consumer_name) |> filter(n()>1) |> table()


  group_by(consumer_name) |> filter(n()>1) |> select(consumer_name) |> table()
  
## Now we explore the duplicates individually 
  dat |> filter(grepl("alexa roman", consumer_name, ignore.case=TRUE)) |> view() 
## Alexa has two records both are done throughly 

## Correction the typo of pro_name
### Vidya Sharma has 19 different variations
dat |> filter(grepl("sharma", pro_name, ignore.case=TRUE)) |> select(pid, pro_name)

## Testing to replace all the variation of Vidya name in to Vidya Sharma
dat <- dat |> mutate(pro_name = if_else(grepl("sharma", pro_name, ignore.case=TRUE), "Vidya Sharma", pro_name))

## HS
dat |> filter(grepl("strong", pro_name, ignore.case=TRUE)) |> select(pid, pro_name)
dat <-dat |> mutate(pro_name = if_else(grepl("strong", pro_name, ignore.case=TRUE), "Harlem Strong", pro_name)) 

## HCCI
### There are 10 variations of HCCI
dat |> filter(grepl("hcci", pro_name, ignore.case=TRUE))|> n_distinct()
dat <- dat |> mutate(pro_name = if_else(grepl("hcci", pro_name, ignore.case=TRUE),"HCCI", pro_name))

## NA values and random word happened 19 times in pro_name
dat |> filter(grepl("^(NA|N/A|none|idk)$", pro_name, ignore.case=TRUE)) |> select(pro_name) |> table()

# By manupulating all empty into NA, there would be 19 + 4 NA is 23 NA. Note that, in this case I use NA not "NA"
 dat <- dat|> mutate(pro_name=if_else(grepl("^(NA|N/A|none|idk)$", pro_name, ignore.case=TRUE),NA,pro_name))
 
 ## Rodini
dat <- dat |> mutate(pro_name = if_else(grepl("rodini", pro_name, ignore.case=TRUE),"Rodini Shiamilis", pro_name))
   
 
 dat <- dat |> mutate(pro_name = if_else(grepl("Mudunuru", pro_name, ignore.case=TRUE),"Mudunuru", pro_name))

 ## white space cause a trouble for Spencer and Ryan  name
 dat <- dat |> mutate(pro_name = if_else(grepl("spencer", pro_name, ignore.case=TRUE),"Spencer Washington", pro_name))
 
dat <- dat |> mutate(pro_name = if_else(grepl("ryan", pro_name, ignore.case=TRUE),"Ryan Health", pro_name)) 

levels(as.factor(dat$cbo_name))
table(dat$consumer_name) # We see that ther are few names with more than 1 records
dat |> group_by(consumer_name) |> filter(n() > 1) |> select(pid, consumer_name, pro_name, cbo_name, doi)
levels(as.factor(dat$consumer_name))

 ## if want to show NA on the table we need to do 
 table(dat$pro_name, useNA = "ifany")
 
 ## After correcting typo, now move to the factor part
 ## By using levels function we can see if there is any typo in the values. It is a handy tool
 
levels(dat$pro_name)
levels(as.factor(dat$pro_name))

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
