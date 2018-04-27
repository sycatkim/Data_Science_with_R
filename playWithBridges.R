#  In class exercise....
#  Load all the bridges data. 
#    identify bridge quality and make a map of all US counties where you color each county 
#    by the number of failing or bad bridges.  
#  The code below does something like this for *just* Wisconsin.




# install.packages("ggplot2")
# # install.packages("plyr")
# install.packages("choroplethr")
# install.packages("choroplethrMaps")
# # install.packages("dplyr")
# install.packages("dtplyr")
# 
# library(plyr)
library(choroplethr)
library(choroplethrMaps)
# library(readr)
# library(dplyr)
library(data.table)
library(tidyverse)
# library(dtplyr)

# I like downloading straight from url's.  
# If I was planning to share this code in a long time, I would worry about stability of the accessibility (url/data/format) and maybe backup locally.
# For class, downloading is straightforward.

# this gets all the data.  
dat <- fread("curl https://www.fhwa.dot.gov/bridge/nbi/2017hwybronlyonefile.zip | funzip") %>% as.tbl


keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008" , "COUNTY_CODE_003", "LAT_016", "LONG_017", "TOLL_020" , "ADT_029"           ,      "YEAR_ADT_030" ,
         "YEAR_BUILT_027" , "DECK_COND_058" , "SUPERSTRUCTURE_COND_059", "SUBSTRUCTURE_COND_060"  , "CHANNEL_COND_061","CULVERT_COND_062", "DATE_OF_INSPECT_090"   ,  "FRACTURE_092A"     ,      "UNDWATER_LOOK_SEE_092B" , "SPEC_INSPECT_092C"  )

# x = M[,match(keep, colnames(M))]

x = select(dat, one_of(keep))  # see chapter 5 (section 4) in r4ds.

wi = filter(x, STATE_CODE_001 == 55)


# make function to rate bridge as NA, good, bad, fail, using 
# colnames(wi)[10:13]
# "SUPERSTRUCTURE_COND_059" "SUBSTRUCTURE_COND_060"   "CHANNEL_COND_061"        "CULVERT_COND_062"  
# good = 5:9
# bad = 2:4
# fail = 0:1

# cond "condition" is the minimum of the given ratings. 
wi = mutate(wi, cond = pmin(SUPERSTRUCTURE_COND_059, SUBSTRUCTURE_COND_060, CHANNEL_COND_061,CULVERT_COND_062, 
                            na.rm = T))

rateIt = function(cond){
  # gives a good to fail rating for cond.
  rate = rep("good", length(cond))
  rate[cond<5] = "bad"
  rate[cond <2]= "fail"
  return(rate)
}

wi$rate = rateIt(wi$cond)
table(wi$cond)
table(wi$rate)
wi = filter(wi, cond>1)
ggplot(data = wi, mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027, col = rate)) +geom_point() + geom_smooth()

map = ggplot(data = wi, mapping = aes(y = lat, x = lon))
map + geom_point(aes(col=rate))+ scale_colour_brewer(palette = "Spectral")  

# where are these bad roads?!!??
ggplot(data = wi, mapping = aes(x = rate, y = log(ADT_029))) + geom_boxplot()
colnames(wi)

# use a data playground!

wi = mutate(wi, fips = STATE_CODE_001*1000+COUNTY_CODE_003)
# why doesn't this work (??)...
# unite(wi, col = tmp, STATE_CODE_001, COUNTY_CODE_003 , sep = "") 
wi = wi %>% mutate(fips = STATE_CODE_001*1000+COUNTY_CODE_003)
wi$fips %>% head

wi = wi %>% mutate(good = (rate == "good"))
table(wi$good)
fipsdat = wi %>% group_by(fips) %>% summarize(propGoodRoads = mean(good))

# dat = wi %>% group_by(good) %>% summarize(tmp = mean(lat))

fipsdat %>% transmute(region = fips, value = propGoodRoads) %>% county_choropleth(state_zoom = "wisconsin")
