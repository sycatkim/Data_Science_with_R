# matrix completion!
# Suppose matrix M has missing elements (NA's or zeros in sparse matrix).  
# we estimate M's leading singular decomp  M \approx UDV^T *on the observed data*
#  then, impute missing elements from UDV^T.  

# I like our code fastAdi, 
# but there are more famous packages like softImpute
# https://cran.r-project.org/web/packages/softImpute/softImpute.pdf

source("https://raw.githubusercontent.com/karlrohe/Adi/master/adaptiveImpute_Rfunction")


# xfull = fread("../data/ml-100k/u.data") %>% as_tibble()
xfull = fread("http://pages.stat.wisc.edu/~karlrohe/classes/data//ml-100k/u.data") %>% as_tibble()
colnames(xfull) = c("person", "movie", "rating", "time")
testSet = sample(nrow(xfull), 10000)
x = xfull[-testSet,]
xtest = xfull[testSet,]
X = spMatrix(nrow = max(xfull$person), ncol = max(xfull$movie), i = x$person, j = x$movie, x = x$rating)
fit = fastAdi(M.p = X, r = 10, tol = .0001) # decrease tol to make it more accurate. 
preds = impute(fit, as.matrix(xtest[,1:2]))
# root mean squared error of predictions on test set:
mean((preds-xtest$rating)^2) %>% sqrt

# is rmse different from chance?
mean((preds-sample(xtest$rating))^2) %>% sqrt


plot(xtest$rating, preds)
boxplot(preds~xtest$rating)
preds[preds<1] = 1
preds[preds>5] = 5
mean((preds-xtest$rating)^2) %>% sqrt


movDat = fread("http://pages.stat.wisc.edu/~karlrohe/classes/data//ml-100k/u.item") %>% as_tibble()

context = movDat$V2
rotDat = varimax(fit$v)  # this is popular in factor analysis... 
newV = rotDat$loadings
printSummary(newV[,1], context)
printSummary(newV[,2], context)



printSummary= function(vec, context){
  # for the largest and smallest top10 elements of vec, print the context.
  rv = rank(vec, ties.method = "random")
  topCon = context[rv <12][1:10]
  botCon= context[rv > length(rv)-12][1:10]
  print(cbind(topCon, botCon))
}