# for wikipedia pages for all US counties,
#  find the urls that these pages point to.
#  create the adjacency matrix.
#  saved in file data/wikiCountyGraph.RData


library(stringr)

scrapeLinks = function(x){  
  # given a line of cnty, it extracts the url for a wiki page of a county
  # stolen from http://stackoverflow.com/questions/3746256/extract-links-from-webpage-using-r
  url = paste("https://en.wikipedia.org/wiki/", 
              sub(" ", "_", x[1]),",_", sub(" ", "_", x[2]), sep="")  # the page url's are nicely structured!
  lines = try(readLines(url), silent = T)
  if(length(lines) ==0) return(c())
  html <- paste(lines, collapse="\n")
  matched <- str_match_all(html, "<a href=\"(.*?)\"")
  links <- matched[[1]][, 2]
  return(links)
}


x =  c("Autauga_County","Alabama")
scrapeLinks(x)

# you need to define allCountyNames to have rows like x
# then, this next line will take a bit of time....
a = apply(allCountyNames, 1, scrapeLinks)


j =(unlist(a))
fj = factor(j)
p = c(0,cumsum(unlist(lapply(a, length))))

A = sparseMatrix(j = as.numeric(fj), p = p, x = rep(1, length(fj)))
dim(A)
colnames(A) = levels(fj)
rownames(A) = cnty[1:4,1]
# rownames(A) = cnty[,1]

# save(A, file = "data/wikiCountyGraph.RData")

