# load diamond dataset

glimpse (diamonds)

# L5, q1: Price Histograms with Facet and Color

# Create a histogram of diamond prices, facet by color
# use cut to color histogram bars

summary(diamonds$price)


ggplot (diamonds,aes(x=price)) + 
  geom_histogram(aes(fill=cut)) +
 labs(title="Histogram for Diamond Prices") +
 labs(x="Price", y="Count")  +
  facet_wrap (~color,ncol=3) +
  scale_fill_brewer(type = 'qual')


# L5, q2: Price vs. Table Colored by Cut

# Create a scatterplot of diamond price vs. table and color the points by the cut of
# the diamond. Sample output: http://i.imgur.com/rQF9jQr.jpg


ggplot (diamonds,aes(x=table, y=price)) + 
  geom_point(aes(color=cut)) +
  labs(title="Histogram for Diamond Prices") +
  labs(x="Table", y="Price")  +
  scale_color_brewer(type = 'qual')+
  xlim(48,80)

#L5, q3: Typical Table Value

# a) What is the typical table range for the majority
#    of diamonds of ideal cut?

ggplot (subset(diamonds,diamonds$cut=='Ideal'),aes(x=table, y=price)) + 
  geom_point(aes(color=cut)) +
  labs(title="Histogram for Diamond Prices") +
  labs(x="Table", y="Price")  

# In looking at the graphs, the typical "ideal cut" range is 53 - 57



# a) What is the typical table range for the majority
#    of diamonds of premium cut?


ggplot (subset(diamonds,diamonds$cut=='Premium'),aes(x=table, y=price)) + 
  geom_point(aes(color=cut)) +
  labs(title="Histogram for Diamond Prices") +
  labs(x="Table", y="Price")

# In looking at the graphs, the typical "premium cut" range is 58 - 61


#L5, q4: Price vs. Volume and Diamond Clarity

# Create a scatterplot of diamond price vs.
# volume (x * y * z) and color the points by
# the clarity of diamonds. Use scale on the y-axis
# to take the log10 of price. You should also
# omit the top 1% of diamond volumes from the plot.

diamonds$volume <- diamonds$x*diamonds$y*diamonds$z

ggplot(data = diamonds, aes(x = diamonds$volume, y = log10(price))) + 
  geom_point(aes (color = clarity)) +
  xlim (0, quantile(diamonds$volume, 0.99)) +
  labs(title="Scatterplot for Diamond Prices vs. Volume") +
  labs(x="Volume", y="log10Price") +
  scale_color_brewer(type = 'div')
  

#L5, q5: Proportion of Friendships Initiated

# Your task is to create a new variable called 'prop_initiated'
# in the Pseudo-Facebook data set. The variable should contain
# the proportion of friendships that the user initiated.

glimpse (pf)

pf$prop_initiated <- pf$friendships_initiated / pf$friend_count 


#L5, q6: prop_initiated vs. tenure
# Create a line graph of the median proportion of
# friendships initiated ('prop_initiated') vs.
# tenure and color the line segment by
# year_joined.bucket.

pf$year_joined <- floor(2014 - (pf$tenure / 365))

pf.year_joined.bucket <- cut (pf$year_joined, 
                              breaks = c(2004,2009,2011,2012,2014))

ggplot(data = pf, aes(x = pf$tenure, y = pf$prop_initiated )) + 
  geom_line(aes (color = pf.year_joined.bucket), 
            stat = 'summary', fun.y = mean) +
  labs(title="Lineplot for Median Proportion of Friends Initiated") +
  labs(x="Tenure", y="Proportion Initiated") 

glimpse (pf.year_joined.bucket)


#L5, q7: Smoothing prop_initiated vs. tenure
# Smooth the last plot you created of
# of prop_initiated vs tenure colored by
# year_joined.bucket. You can bin together ranges
# of tenure or add a smoother to the plot.

ggplot(data = pf, aes(x = 7*round(pf$tenure/7), y = pf$prop_initiated )) + 
  geom_smooth(aes (color = pf.year_joined.bucket), 
            stat = 'summary', fun.y = mean) +
  labs(title="Lineplot for Median Proportion of Friends Initiated") +
  labs(x="Tenure", y="Proportion Initiated") 

#L5, q8: Greatest prop_initiated Group

# In looking at the previous graph in Q7, clearly the (2012,2014] group have 
# the highest friends initiated ratio (between .65 and .85)


#L5, q9: Largest Group Mean prop_initiated

summary (subset(pf$prop_initiated, pf.year_joined.bucket == "(2012,2014]"))

#output:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0000  0.5115  0.7018  0.6654  0.8490  1.0000    1468 



#L5, q10:  Price/Carat Binned, Faceted, & Colored

# Create a scatter plot of the price/carat ratio
# of diamonds. The variable x should be
# assigned to cut. The points should be colored
# by diamond color, and the plot should be
# faceted by clarity.

  # The plot should look something like this.
  # http://i.imgur.com/YzbWkHT.jpg

?facet_wrap

ggplot(data = diamonds, aes(x = cut, y = price/carat )) + 
  geom_point(aes (color = color)) + 
  facet_wrap (~clarity, ncol = 3) +
  scale_color_brewer(type = 'div')




#L5, q10: Gapminder Multivariate Analysis
# The Gapminder website contains over 500 data sets with information about
# the world's population. Your task is to continue the investigation you did at the
# end of Problem Set 4 or you can start fresh and choose a different
# data set from Gapminder.
# In your investigation, examine 3 or more variables and create 2-5 plots that make
# use of the techniques from Lesson 5.

# Use: http://www.gapminder.org/data/