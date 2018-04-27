library(rARPACK)
library(choroplethr)
rm(list = ls())
load(file = url("http://pages.stat.wisc.edu/~karlrohe/classes/data/wikiCountyGraph.RData"))

str(A)
cs = colSums(A)
rs =rowSums(A)
A = A[, cs > 5 &  cs< (length(rs)/2)]
dim(A)
rs = rowSums(A)
cs = colSums(A)

L = Diagonal(length(rs), 1/sqrt(rs+2))%*%A%*% Diagonal(length(cs), 1/sqrt(cs+2))
?svds
s = svds(L,k = 100)
plot(s$d, main = "leading singular values. We want to look for a gap")
hist(s$d, main = "leading singular values. We want to look for a gap")

u = s$u[,1:50]
u = t(apply(u, 1, function(x) return(x/sqrt(sum(x^2) + 10^(-10)))))
vu = varimax(u) 
df = cbind(as.numeric(rownames(A)), vu$loadings[,3]) 
colnames(df) = c("region", "value")
df %>%  as.data.frame %>% county_choropleth(num_colors = 1)

set.seed(1)
km = kmeans(u, centers  = 50, nstart = 10)
km$cluster %>% table
km$cluster %>% table %>% min # unbalaned! awkward.

df = data_frame(as.numeric(rownames(A)), km$cluster==3) 
colnames(df) = c("region", "value")
df %>%  as.data.frame %>% county_choropleth()