library(data.table)
library(tidyverse)
library(tidytext)
library(Matrix)
# install.packages("glmnet")
library(glmnet)
library(randomForest)


data = fread("https://raw.githubusercontent.com/bpb27/political_twitter_archive/master/realdonaldtrump/realdonaldtrump.csv") %>% as.tbl
str(data)

text_df <- data_frame(tweet = 1:nrow(data), text = data$text)

# this does a lot of processing! 
#  to lower, remove @ # , . 
#  often these make sense on a first cut.
#  worth revisiting before "final results"!
tt  = text_df %>% unnest_tokens(word, text)
str(tt)

# make the document-term matrix.  
#   I sometimes call this the bag-of-words-matrix.
dt = cast_sparse(tt, tweet, word)
str(dt)
dim(dt)
hist(rowSums(dt))
cs = colSums(dt)
hist(log(cs[cs>1]))


# let's take the svd to make clusters/topics 
library(rARPACK)
A = dt; Dl = Diagonal(nrow(A), 1/sqrt(rowSums(A)+10)); Dr = Diagonal(ncol(A), 1/sqrt(colSums(A)+10))
L = Dl%*%A%*%Dr
s = svds(L, k = 10)
plot(s$d[-1])
u = s$u 

plot(as.data.frame(u[sample(nrow(A),1000),]), pch = ".")

text_df[which(s$u[,2]< -.01),]$text
colnames(dt)[which(s$v[,2]> .1)]
