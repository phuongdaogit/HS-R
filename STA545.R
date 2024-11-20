### Follow along with STA545 https://stat545.com/basic-data-care.html
#install.packages("gapminder")
library(gapminder)
metdata = read.csv("MetObjects.txt", header = TRUE, stringsAsFactors = FALSE)
lines <- readLines("MetObjects.txt")
lines
col_count <- sapply(strsplit(lines, ","), length)
table(col_count)

### Explore the data
dim(metdata)
str(metdata)
class(metdata$Repository)
table(metdata$Repository)
class(metdata$Department)
table(metdata$Department)      
unique(metdata$Department)    
levels(metdata$Department)#null means the class is not a factor     
      

####
n_distinct(metdata)
n_distinct(metdata) == dim(metdata)
