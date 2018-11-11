Untitled
================
Stuti Pandey
4/25/2018

``` r
library(data.table)
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.4
    ## ✔ tidyr   0.8.0     ✔ stringr 1.3.0
    ## ✔ readr   1.1.1     ✔ forcats 0.2.0

    ## ── Conflicts ───────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::between()   masks data.table::between()
    ## ✖ dplyr::filter()    masks stats::filter()
    ## ✖ dplyr::first()     masks data.table::first()
    ## ✖ dplyr::lag()       masks stats::lag()
    ## ✖ dplyr::last()      masks data.table::last()
    ## ✖ purrr::transpose() masks data.table::transpose()

``` r
library(tidytext)
library(magrittr)
```

    ## 
    ## Attaching package: 'magrittr'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     set_names

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

``` r
data1<-fread("gun.tweets.csv")%>%as.tbl
```

    ## Warning in fread("gun.tweets.csv"): Bumped column 15 to type character
    ## on data row 789, field contains '"Ms"'. Coercing previously read values
    ## in this column from logical, integer or numeric back to character which
    ## may not be lossless; e.g., if '00' and '000' occurred before they will
    ## now be just '0', and there may be inconsistencies with treatment of ',,'
    ## and ',NA,' too (if they occurred in this column before the bump). If
    ## this matters please rerun and set 'colClasses' to 'character' for this
    ## column. Please note that column type detection uses a sample of 1,000 rows
    ## (100 rows at 10 points) so hopefully this message should be very rare.
    ## If reporting to datatable-help, please rerun and include the output from
    ## verbose=TRUE.

    ## Warning in fread("gun.tweets.csv"): Bumped column 40 to type character on
    ## data row 6861, field contains '"39.998 -75.1448"'. Coercing previously read
    ## values in this column from logical, integer or numeric back to character
    ## which may not be lossless; e.g., if '00' and '000' occurred before they
    ## will now be just '0', and there may be inconsistencies with treatment of
    ## ',,' and ',NA,' too (if they occurred in this column before the bump).
    ## If this matters please rerun and set 'colClasses' to 'character' for this
    ## column. Please note that column type detection uses a sample of 1,000 rows
    ## (100 rows at 10 points) so hopefully this message should be very rare.
    ## If reporting to datatable-help, please rerun and include the output from
    ## verbose=TRUE.

    ## Warning in fread("gun.tweets.csv"): Bumped column 41 to type character on
    ## data row 6861, field contains '"-75.1448 39.998"'. Coercing previously read
    ## values in this column from logical, integer or numeric back to character
    ## which may not be lossless; e.g., if '00' and '000' occurred before they
    ## will now be just '0', and there may be inconsistencies with treatment of
    ## ',,' and ',NA,' too (if they occurred in this column before the bump).
    ## If this matters please rerun and set 'colClasses' to 'character' for this
    ## column. Please note that column type detection uses a sample of 1,000 rows
    ## (100 rows at 10 points) so hopefully this message should be very rare.
    ## If reporting to datatable-help, please rerun and include the output from
    ## verbose=TRUE.

``` r
text_df1 <- data_frame(tweet = 1:nrow(data1), text = data1$text)
tt1  = text_df1 %>% unnest_tokens(word, text)
tt2=tt1 %>%group_by(tweet)%>%left_join(get_sentiments("bing"))%>%left_join(get_sentiments("afinn"))%>%count(sentiment)%>%
  ungroup()%>%spread(sentiment, n, fill = 0)%>% mutate(sentiment = positive - negative)
```

    ## Joining, by = "word"

    ## Joining, by = "word"

``` r
tt2<-tt2[,-4]
tt2%>%ggplot(aes(x=sentiment))+geom_histogram(binwidth = 1,fill="pink")
```

![](finalproject_stuti_files/figure-markdown_github/cars-1.png) \#removng columns with both positve and negative as zero

![](finalproject_stuti_files/figure-markdown_github/pressure-1.png)
