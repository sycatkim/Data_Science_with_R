    library(data.table)
    library(tidyverse)

    ## -- Attaching packages -------------------------------------------------------- tidyverse 1.2.1 --

    ## √ ggplot2 3.0.0     √ purrr   0.2.5
    ## √ tibble  1.4.2     √ dplyr   0.7.6
    ## √ tidyr   0.8.1     √ stringr 1.3.1
    ## √ readr   1.1.1     √ forcats 0.3.0

    ## -- Conflicts ----------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::between()   masks data.table::between()
    ## x dplyr::filter()    masks stats::filter()
    ## x dplyr::first()     masks data.table::first()
    ## x dplyr::lag()       masks stats::lag()
    ## x dplyr::last()      masks data.table::last()
    ## x purrr::transpose() masks data.table::transpose()

    library(tidytext)
    library(Matrix)

    ## 
    ## Attaching package: 'Matrix'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     expand

    # install.packages("glmnet")
    library(glmnet)

    ## Loading required package: foreach

    ## 
    ## Attaching package: 'foreach'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     accumulate, when

    ## Loaded glmnet 2.0-16

    library(randomForest)

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    data = fread("https://raw.githubusercontent.com/bpb27/political_twitter_archive/master/realdonaldtrump/realdonaldtrump.csv") %>% as.tbl

    ## Warning in require_bit64(): Some columns are type 'integer64' but
    ## package bit64 is not installed. Those columns will print as strange
    ## looking floating point data. There is no need to reload the data. Simply
    ## install.packages('bit64') to obtain the integer64 print method and print
    ## the data again.

    str(data)

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    30563 obs. of  8 variables:
    ##  $ favorite_count         : int  0 2077 1 62 291 928 9 1705 15 202 ...
    ##  $ source                 : chr  "Twitter for iPhone" "Twitter for Android" "Twitter for Android" "Twitter for iPhone" ...
    ##  $ text                   : chr  "The #MarchForLife is so important. To all of you marching --- you have my full support!" "Mexico has taken advantage of the U.S. for long enough. Massive trade deficits &amp; little help on the very weak border must c "Look forward to seeing final results of VoteStand. Gregg Phillips and crew say at least 3,000,000 votes were illegal. We must d "Miami-Dade Mayor drops sanctuary policy. Right decision. Strong! https://t.co/MtPvaDC4jM" ...
    ##  $ in_reply_to_screen_name: chr  "" "" "" "" ...
    ##  $ is_retweet             : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ created_at             : chr  "Fri Jan 27 16:27:02 +0000 2017" "Fri Jan 27 13:19:10 +0000 2017" "Fri Jan 27 13:12:52 +0000 2017" "Thu Jan 26 23:53:37 +0000 2017" ...
    ##  $ retweet_count          : int  0 535 1 22 95 171 9 445 5 105 ...
    ##  $ id_str                 : 'integer64' num  1.62e-253 1.61e-253 1.61e-253 1.55e-253 1.55e-253 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

    #making a new dataframe. col_1=order of tweets. col_2= text of tweet
    text_df <- data_frame(tweet = 1:nrow(data), text = data$text)
    head(text_df)

    ## # A tibble: 6 x 2
    ##   tweet text                                                              
    ##   <int> <chr>                                                             
    ## 1     1 The #MarchForLife is so important. To all of you marching --- you~
    ## 2     2 Mexico has taken advantage of the U.S. for long enough. Massive t~
    ## 3     3 Look forward to seeing final results of VoteStand. Gregg Phillips~
    ## 4     4 Miami-Dade Mayor drops sanctuary policy. Right decision. Strong! ~
    ## 5     5 Will be interviewed by @SeanHannity on @FoxNews at 10:00pm tonigh~
    ## 6     6 Spoke at the Congressional @GOP Retreat in Philadelphia, PA. this~

this does a lot of processing!
==============================

to lower, remove @ \# , .
=========================

often these make sense on a first cut.
======================================

worth revisiting before "final results"!
========================================

    tt  = text_df %>% unnest_tokens(word, text)
    str(tt)

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    519844 obs. of  2 variables:
    ##  $ tweet: int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ word : chr  "the" "marchforlife" "is" "so" ...

    # each word of each tweet is separated.
    head(tt, 10)

    ## # A tibble: 10 x 2
    ##    tweet word        
    ##    <int> <chr>       
    ##  1     1 the         
    ##  2     1 marchforlife
    ##  3     1 is          
    ##  4     1 so          
    ##  5     1 important   
    ##  6     1 to          
    ##  7     1 all         
    ##  8     1 of          
    ##  9     1 you         
    ## 10     1 marching

make the document-term matrix.
==============================

I sometimes call this the bag-of-words-matrix.
==============================================

making sparse matrix by using the above data.

    dt = cast_sparse(tt, tweet, word)
    str(dt)

    ## Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
    ##   ..@ i       : int [1:491991] 0 1 5 6 7 8 12 13 14 18 ...
    ##   ..@ p       : int [1:40224] 0 12453 12455 19171 20766 20896 29971 31728 37506 43679 ...
    ##   ..@ Dim     : int [1:2] 30563 40223
    ##   ..@ Dimnames:List of 2
    ##   .. ..$ : chr [1:30563] "1" "2" "3" "4" ...
    ##   .. ..$ : chr [1:40223] "the" "marchforlife" "is" "so" ...
    ##   ..@ x       : num [1:491991] 1 1 1 1 1 1 1 1 1 1 ...
    ##   ..@ factors : list()

    dim(dt)

    ## [1] 30563 40223

    head(dt)

    ## 6 x 40223 sparse Matrix of class "dgCMatrix"

    ##    [[ suppressing 31 column names 'the', 'marchforlife', 'is' ... ]]

    ##                                                                       
    ## 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 . . . . . . . . . . . . . . . . . ......
    ## 2 1 . . . . . . 1 . . . . . . 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ......
    ## 3 . . . . . 1 . 1 . . . . . . . . . . . . . . . . . . . . . . . ......
    ## 4 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ......
    ## 5 . . . . . . . . . . . . . . . . . . . . . . . . . . . . 1 . . ......
    ## 6 1 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ......
    ## 
    ##  .....suppressing columns in show(); maybe adjust 'options(max.print= *, width = *)'
    ##  ..............................

histogram of the sparse matrix.

    hist(rowSums(dt))

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    cs = colSums(dt)
    hist(log(cs[cs>1]))

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-4-2.png) \#
which words get the most "favorites"? \# what is a meaningful way to
measure this? \# it is not "trivial" because each tweet has lots of
words... \# we want to say which *words* get the most favorites.

we are going to play with a hold out set.
=========================================

trying to predict:
==================

    Y = log(data$favorite_count + 1)

before going on, we are going to have a hold out test set...
============================================================

    leaveout = sample(1:nrow(dt), 20000)
    cs = colSums(dt[-leaveout,])
    # select some good words:
    D = Diagonal(ncol(dt), 1/(cs+2))  

    # without the +2 ...  score = {average value of Y} over tweets with that word.
    #   why +2???
    score = t(Y[-leaveout])%*%dt[-leaveout,]%*%D %>% as.vector
    score %>% hist

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    colnames(dt)[which.max(score)]  # what is the "most favorited word"?

    ## [1] "crooked"

    colnames(dt)[order(-score)[1:10]]  # what are the top 10?

    ##  [1] "crooked"        "draintheswamp"  "sanders"        "trumppence16"  
    ##  [5] "hillary"        "crookedhillary" "bernie"         "americafirst"  
    ##  [9] "clinton"        "imwithyou"

    # how many tweets use these words?  (we might be concerned if less than ~3 tweets used the word...)
    cs[order(-score)[1:10]]  

    ##        crooked  draintheswamp        sanders   trumppence16        hillary 
    ##             72             32             22             17            208 
    ## crookedhillary         bernie   americafirst        clinton      imwithyou 
    ##             16             33             23            140             11

If only given bag-of-words for a tweet, can we predict how many favorites it will get?
======================================================================================

THIS IS A PLAYGROUND WITH 4 KNOBS. (you could imagine making this a shiny app!)
===============================================================================

to predict Y, we will change:
=============================

1) number of features (chosen by top score!)
============================================

2) ridge vs lambda vs elastic net vs randomForest
=================================================

3) number of "leave out"
========================

4) instead of "favorites", use "retweets"!
==========================================

5) incorporate other data in dat to do the prediction!
======================================================

we will compare based on MSE
============================

    ### LASSO:
    d = 10
    fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout])
    Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
    (Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-8-1.png) \#
how many nonzero betas?

    colSums(fullSolution$beta != 0) %>% plot 

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-9-1.png)

    plot(colSums(fullSolution$beta != 0),colMeans((Y[leaveout] - Yhat)^2), type = 'l')

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-9-2.png)

Change the value of d

    d = 100
    fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout])
    Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
    (Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-10-1.png)

    colSums(fullSolution$beta != 0) %>% plot 

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-10-2.png)

    plot(colSums(fullSolution$beta != 0),colMeans((Y[leaveout] - Yhat)^2), type = 'l')

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-10-3.png)

    d = 200
    fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout])
    Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
    (Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-11-1.png)

    colSums(fullSolution$beta != 0) %>% plot 

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-11-2.png)

    plot(colSums(fullSolution$beta != 0),colMeans((Y[leaveout] - Yhat)^2))

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-11-3.png)

    d = 2000
    fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout])
    Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
    (Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-12-1.png)

    plot(colSums(fullSolution$beta != 0),colMeans((Y[leaveout] - Yhat)^2))

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-12-2.png)

    d = 20000
    fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout])
    Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
    (Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-13-1.png)

    plot(colSums(fullSolution$beta != 0),colMeans((Y[leaveout] - Yhat)^2))

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-13-2.png)

### Random forest; what is "fair comparison" for leaveout

    d = 10
    rf = randomForest(x = as.matrix(dt[-leaveout,order(-score)[1:d]]), y = Y[-leaveout], ntree = 50)
    plot(rf)

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-14-1.png)

this takes ~1 minute:
=====================

    d = 100
    rf = randomForest(x = as.matrix(dt[-leaveout,order(-score)[1:d]]), y = Y[-leaveout], ntree = 50)
    plot(rf)

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-15-1.png)

#### RIDGE

    d = 10
    fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout], alpha = 0)
    Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
    (Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-16-1.png)

    colSums(fullSolution$beta != 0) %>% plot 

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-16-2.png)

    d = 1000
    fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout], alpha = 0)
    Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
    (Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-16-3.png)

    colSums(fullSolution$beta != 0) %>% plot 

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-16-4.png)

    d = 20000
    fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout], alpha = 0)
    Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
    (Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-16-5.png)

ELASTIC NET
===========

    d = 20000
    fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout], alpha = .1)
    Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
    (Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-17-1.png)

    plot(colSums(fullSolution$beta != 0),colMeans((Y[leaveout] - Yhat)^2))

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-17-2.png)

    d = 20000
    fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout], alpha = .5)
    Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
    (Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-17-3.png)

    plot(colSums(fullSolution$beta != 0),colMeans((Y[leaveout] - Yhat)^2))

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-17-4.png)

    d = 10000
    fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout], alpha = .9)
    Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
    (Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-17-5.png)

    plot(colSums(fullSolution$beta != 0),colMeans((Y[leaveout] - Yhat)^2))

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-17-6.png)

increase the training set!
==========================

    leaveout = sample(1:nrow(dt), 5000)

    d = 20000
    fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout], alpha = .9)
    Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
    (Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-18-1.png)

    plot(colSums(fullSolution$beta != 0),colMeans((Y[leaveout] - Yhat)^2))


    cutt  = 200
    lines(cutt*c(1,1), c(-9999,999999))

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-18-2.png)

    modelNumber = which(colSums(fullSolution$beta!=0)>cutt)[1]
    model = which(fullSolution$beta[,modelNumber]!=0)
    best = order(-score)[1:d][model]
    length(best)

    ## [1] 210

what would happen if we used: as.matrix(dt)\[-leaveout,best\]? (hint: don't try it)
===================================================================================

    #leaveout = sample(1:nrow(dt), 10000)
    #rf = randomForest(x = as.matrix(dt[-leaveout,best]), y = Y[-leaveout], ntree = 50)
    #plot(rf)

this is me trying to get fancy... (and utterly failing to dramatically improve predictions)
===========================================================================================

let's take the svd to make "better features"
============================================

    library(rARPACK)
    A = dt[-leaveout,best]; Dl = Diagonal(nrow(A), 1/sqrt(rowSums(A)+10)); Dr = Diagonal(ncol(A), 1/sqrt(colSums(A)+10))
    s = svds(Dl%*%A%*%Dr, k = 50)
    plot(s$d[-1])

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-20-1.png)

    u  = t(apply(s$u[,1:35],1,function(x) return(x/sqrt(sum(x^2) + 1/(sqrt(nrow(A))*20)  ))))
    rfSVD = randomForest(x = u, y = Y[-leaveout], ntree = 50)
    plot(rfSVD)

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-20-2.png) \#
because I was curious, I also tried rotating with fastICA \# (which is
cool, but not at all clear why it might help here.)

    library(fastICA)
    fica= fastICA(u, 35)
    str(fica)

    ## List of 5
    ##  $ X: num [1:25563, 1:35] -0.0875 0.0537 0.0476 0.1164 0.0938 ...
    ##   ..- attr(*, "scaled:center")= num [1:35] -0.167824 -0.016468 0.020232 -0.020336 0.000733 ...
    ##  $ K: num [1:35, 1:35] -0.593 -0.582 -0.951 0.207 1.604 ...
    ##  $ W: num [1:35, 1:35] 0.01334 -0.02977 -0.01153 -0.00549 0.0366 ...
    ##  $ A: num [1:35, 1:35] -0.00633 0.01371 -0.05657 0.00726 -0.0115 ...
    ##  $ S: num [1:25563, 1:35] 0.1075 -0.0671 -0.0836 -0.0152 0.3232 ...

    rfICA = randomForest(x = fica$S, y = Y[-leaveout], ntree = 50)
    plot(rfICA)

![](trumpTwitter_files/figure-markdown_strict/unnamed-chunk-21-1.png) \#
SQRT Lasso picks a lambda for you without cross-validation!

    source("https://raw.githubusercontent.com/cran/RPtests/master/R/sqrt_lasso.R")
    sqrtLasso = sqrt_lasso(x = dt[,order(-score)[1:10]], y = Y)
