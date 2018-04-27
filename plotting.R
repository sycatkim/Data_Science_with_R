#  This follows chapter 3 in r4ds


library(ggplot2)
library(dplyr)

mpg


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

mpg %>%  ggplot + 
  geom_point(mapping = aes(x = displ, y = hwy))



# <DATA> %>% ggplot + 
#   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# aes = aesthetic properties  

# aesthetic |es�THetik| (also esthetic)
# adjective
# concerned with beauty or the appreciation of beauty: the pictures give great aesthetic pleasure.
# �� giving or designed to give pleasure through beauty; of pleasing appearance.
# noun [ in sing. ]
# a set of principles underlying and guiding the work of a particular artist or artistic movement: the Cubist aesthetic.

# faceting:

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(~ class)


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(class ~ cyl)




# Compare these two plots:

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))




#geoms

# compare these two plots

# left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# right
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))


ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))


ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv))+ 
  geom_point(mapping = aes(x = displ, y = hwy, color = drv))


# lots of geoms: 
# http://r4ds.had.co.nz/images/visualization-geoms-2.png
# http://r4ds.had.co.nz/images/visualization-geoms-3.png
# http://r4ds.had.co.nz/images/visualization-geoms-4.png

#  Lots of extensions!  
# http://www.ggplot2-exts.org/gallery/    (and more not listed here)

# here is a cheat sheet!  https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf


# here is the "theory" behind ggplot...
# source:  http://sctyner.github.io/ggplot_tutorial.html#1 


# gg = grammar of graphics.

# The grammar's "Parts of Speech"
# Data (noun/subject)
# Aesthetic mappings (adjectives)
# Geom (verb)
# Stat (adverb)
# Position (preposition)

# "Sentences" and "Paragraphs"
# All components combine to make a layer (sentence)
# Can places layers on top of each other (paragraph)
# String it all together with + (punctuation)




# there is a lot more if you are interested...
#  below is a template.  
#   I've never used stat or coordinate function... perhaps less useful... 
#   perhaps I haven't made it there yet.
# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(
#     mapping = aes(<MAPPINGS>),
#     stat = <STAT>, 
#     position = <POSITION>
#   ) +
#   <COORDINATE_FUNCTION> +
#   <FACET_FUNCTION>