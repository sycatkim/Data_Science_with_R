#  This follows chapter 12 of r4ds.  

rm(list =ls())
library(tidyr)
library(dplyr)

table1  # if this doesn't load, make sure you have the most recent versions of tidyr and dplyr.
table2
table3
table4a
table4b
table5

# TIDY DATA:
# Each variable must have its own column.
# Each observation must have its own row.
# Each value must have its own cell.
# http://r4ds.had.co.nz/images/tidy-1.png

# which table above is tidy?

#  Simpler instructions:
# Put each dataset in a tibble.
# Put each variable in a column.


# dplyr, ggplot2, and all other the packages 
# in the tidyverse are designed to work with tidy data. 

table1 %>% 
  mutate(rate = cases / population * 10000)

table1 %>% 
  count(year, wt = cases)

library(ggplot2)
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))


# Exercise:
# Compute the rate for table2, and table4a + table4b. You will need to perform four operations:
#   
#   Extract the number of TB cases per country per year.
# Extract the matching population per country per year.
# Divide cases by population, and multiply by 10000.
# Store back in the appropriate place.
# Which representation is easiest to work with? Which is hardest? Why?




#     In data analysis, tidy data is the agile stance.  It facilitates quick and dynamic interactions with your data.

# I like to think:  
# Rows index the *units of observation* (i.e. "an observation" is one row).  
# The first columns identify the unit (country x year).  
# Then, the later columns give "the measurement"  



# to tidy the data (or organize it), two functions are particularly helpful:
# gather
# spread

# We are going to use these functions to tidy:
table2
table4a
table4b

#  In brief....
# When measurements are distributed across different rows (i.e. measurements on a unit of analysis takes up several rows), 
#  then spread gets you tidy.  Think "spread out wide" because it takes a table which is too long and makes it shorter and wider.

# When units of analysis are distributed across different columns (i.e. variables of the "same thing" take up several columns),
#   then gather gets you tidy.  Gather takes a table which is too wide and makes it thinner and taller. 


##################
### gather #######
##################

table4a  # columns are identifiers!
table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")

# pipe data, then 
#   names of columns you wish to "gather" into one column, then
#   key = name of column with the gathered column names, then
#   value = column name of the values that are pulled from columns. 

# http://r4ds.had.co.nz/images/tidy-9.png

# alternative syntax:
table4a %>% 
  gather(`1999`:`2000`, key = "year", value = "cases")

# Or, as said in r4ds, to gather the data, we need three things or "parameters"

# 1) The set of columns that represent values, not variables. 
# In this example, those are the columns 1999 and 2000.
# 
# 2) The name of the variable whose values form the column names. 
# I call that the key, and here it is year.
# 
# 3) The name of the variable whose values are spread over the cells. 
# I call that value, and here itâ€™s the number of cases.


#  Here is how I (Karl) would say those points..
# 1) The set of columns that should be "units of observation"
# In this example, those are the columns 1999 and 2000.

# 2) The name of the new column (whose elements in the new tibble are the old columns names)
#   I think of this column as helping to identify the unit of analysis (part of the first set of columns)
# 
# 3) The name of the new column (whose elements in the new tibble are the elements of the old tibble)
#   I think of this column as giving the measurements of interest (part of the last set of columns)

table4b

table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")

#  the ellipsis (i.e. "...") in the help documentation is *super important*
#    Usually, the ellipsis is ignored in the help documentation... this makes it tricky to comprehend the documentation!!!


#  Now, these two new data sets need to be put together!
#    This is peeking ahead to chapter 13.

tidy4a <- table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")
tidy4b <- table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")
left_join(tidy4a, tidy4b)
left_join(tidy4a, tidy4b,c("country", "year"))
left_join(tidy4a, tidy4b, "country")  # be careful!  


table2  # this has the "opposite problem"!

##################
### spread #######
##################

# Spreading is the opposite of gathering. You use it when an observation is scattered across multiple rows. 
table2

# This time, however, we only need two parameters:

# 1) The column that contains variable names, the key column. Here, itâ€™s type.
# 
# 2) The column that contains values forms multiple variables, the value column. Here itâ€™s count.


spread(table2, key = type, value = count)

# http://r4ds.had.co.nz/images/tidy-8.png


# gather() makes wide tables narrower and longer; 
# spread() makes long tables shorter and wider.

# gather is not *exactly* the "inverse" of spread.... but it is really close:

# what changes:
stocks <- data_frame(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`:`2016`)
stocks

# why doesn't this run?
table4a %>% 
  gather(1999, 2000, key = "year", value = "cases")



#  is this data tidy?  why doesn't it spread?
people <- frame_data(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

spread(people, key = key, value =value)


#  tidy this:
preg <- frame_data(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)
# answer is below... don't cheat!!


#separate and unite are less useful... but maybe you might need them.

#Separate splits one column into two different columns
table3
table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/", convert=T)  # sep is a regular expression!

table3 %>% 
  separate(year, into = c("century", "year"), sep = 2)  # this probably isn't a good idea for any ensuing data analysis... it is here to illustrate the function.

table3 %>% 
  separate(year, into = c("century", "year"), sep = "2")  #  notice the difference between this line and the last...


# Unite puts to columns together into a single column.
table5
table5 %>% 
  unite(new, century, year, sep = "")



#########################
##### Missing values#####
#########################

# Missing values are annoying.
# Missing values should be expected.

# how many missing values?
stocks <- data_frame(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

# how many missing values?
stocks

# how many missing values?
stocks %>%  spread(key =qtr, value = return)

# how many missing values?
stocks %>% 
  complete(year, qtr)



# do you want to remove NA's?
stocks %>% 
  spread(year, return) %>% 
  gather(year, return, `2015`:`2016`, na.rm = TRUE)




#  READ SECTION 12.6 IN R4DS!!!
# http://r4ds.had.co.nz/tidy-data.html#case-study
#  It puts all the pieces together in a case study.
#  Follow along by typing the code in your terminal.  



#  Tidy data is great for "90%" of "small data" problems (remember: small data is data that you can fit in memory, i.e. load into R)
#    As such, I think of tidy data as a default "null hypothesis" for how to store data.
#    Of course, there is another "10%" of problems.  See here:  http://simplystatistics.org/2016/02/17/non-tidy-data/










# answer to tidying preg...
# preg %>% gather(male:female, key = sex, value = count)
# or:
# tidyPreg = preg %>% gather(male,female, key = gender, value = num) %>% select(gender, pregnant, num)
# tidyPreg$num[1]= 0



