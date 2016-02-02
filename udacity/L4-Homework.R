# Use diamonds data set.

# Question 1
# Your first task is to create a scatterplot of price vs x.
# using the ggplot syntax.

glimpse (diamonds)

ggplot (aes(x=x,y=price),data=diamonds) + geom_point()

# Question 2

  # observations of scatterplot of price vs x.
  # 1. they seem to be related in non-linear manner (e.g exponential)
  # 2. there seems to be strong correlation between the two 
  # 3. there are a few outliers where x = 0

# Question 3

# What is the correlation between price and x?
with (diamonds,cor.test(price,x,method='pearson'))
  # correlation = 0.8844352 

# What is the correlation between price and y?
with (diamonds,cor.test(price,y,method='pearson'))
  # correlation = 0.8654209 

# What is the correlation between price and z?
with (diamonds,cor.test(price,z,method='pearson'))
  # correlation = 0.8612494

# Question 4

# Create a simple scatter plot of price vs depth.
ggplot (aes(x=depth,y=price),data=diamonds) + geom_point()

# Question 5 - Adjustments - price vs. depth

# Change the code to make the transparency of the
# points to be 1/100 of what they are now and mark
# the x-axis every 2 units. 
# ggplot(data = diamonds, aes(x = depth, y = price)) + 
#  geom_point()

ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha=.1) +
  scale_x_continuous(breaks = seq(0,80,2))

# Question 6 - Typical Depth Range
  # based on the scatterplot, most depth values are between 58 an 64


# Question 7

# what is the correlation between depth and price?
with (diamonds,cor.test(depth,price,method='pearson'))

  # correlation = -0.0106474 

  # depth and price are not correlated, which makes sense visually 
  # since the price can be low for both low and high depths

  # -0.3 > correlation > 0.3 is meaningful, but small. 
  # Around 0.5 is moderate, and 0.7 and greater is large


# Question 8 - price vs. carat

# Create a scatterplot of price vs carat and omit the top 1% of 
# price and carat values.
ggplot(data = diamonds, aes(x = carat, y = price)) + 
  geom_point() +
  xlim (0, quantile(diamonds$carat, 0.99)) +
  ylim (0, quantile(diamonds$price, 0.99))


# Question 9 - price vs. volume

# Create a scatterplot of price vs. volume (x * y * z).
# This is a very rough approximation for a diamond's volume.
ggplot(data = diamonds, aes(x = (x*y*z), y = price)) + 
  geom_point()

# Create a new variable for volume in the diamonds data frame.
# This will be useful in a later exercise.
diamonds.volume <- diamonds$x*diamonds$y*diamonds$z

# Question 10 - Findings - price vs. volume
# What are your observations between price and volume?

  # 1. price and volume seem to correlated, possibly in a linear way
  # 2. There are only a few outliers

# Question 11 - Correlations on Subsets
# What is the correlation of price and volume? 
# Exclude diamonds that have a volume of 0 or that are greater than or equal to 800

  # 1. create a subset of diamonds that removes outliers
  diamonds_noout <- subset(diamonds, diamonds.volume > 0 & diamonds.volume < 800)

  # 2. create volume variable in diamond subset
  diamonds_noout$volume <- diamonds_noout$x*diamonds_noout$y*diamonds_noout$z

  # 3. confirm that volume added by "View" new dataset
  View (diamonds_noout)

  #4. calculate correlation with new, smaller dataset that excludes outliers
  with (diamonds_noout, cor.test(volume, price, method ='pearson'))

  # correlation = 0.9235455 

# Question 12 - Adjustments - price vs. volume
# Subset the data to exclude diamonds with a volume greater than or equal to 800. Also, exclude diamonds
# with a volume of 0. Adjust the transparency of the points and add a linear model to the plot. (See the
# Instructor Notes or look up the documentation of geom_smooth() for more details about smoothers.)  
  
  # 1. create a subset of diamonds that removes diamonds with a volume greater than or equal to 800 and volume 0
  diamonds_noout <- subset(diamonds, diamonds.volume > 0 & diamonds.volume < 800)
  
  # 2. Adjust transparency
  
  ggplot(data = diamonds_noout, aes(x = volume, y = price)) + 
    geom_point(alpha= 0.1) 

  # 2. Adjust transparency and add linear model to plot
  
  ggplot(data = diamonds_noout, aes(x = volume, y = price)) + 
    geom_point(alpha= 0.1) +
    stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE,
                colour = "red") 
    
 
# Question 13 - Mean Price by Clarity
  
# Use the function dplyr package to create a new data frame containing
# info on diamonds by clarity called diamondsByClarity
# The data frame should contain the following variables in this order.
  
  #       (1) mean_price
  #       (2) median_price
  #       (3) min_price
  #       (4) max_price
  #       (5) n
  
# where n is the number of diamonds in each level of clarity.  
  library(dplyr)
  
  diamondsByClarity <- diamonds %>%
      group_by (clarity) %>%
      summarize (mean_price = mean(price),
                 median_price = median(price),
                 min_price = min(price),
                 max_price = max (price),
                 n = n())
      
  View (diamondsByClarity)

  
# Question 14 - Bar Charts of Mean Price
#  We've created summary data frames with the mean price by clarity and color. 
#  Write additional code to create two bar plots on one output image using the 
# grid.arrange() function from the package gridExtra.

  diamonds_by_clarity <- group_by(diamonds, clarity)
  diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))
  
  diamonds_by_color <- group_by(diamonds, color)
  diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))
  
  p1 <- ggplot(data = diamonds_mp_by_clarity, aes(x=clarity, y= mean_price)) + geom_boxplot()
    
  p2 <- ggplot(data = diamonds_mp_by_color, aes(x=color, y= mean_price)) + geom_boxplot()
  
    
  grid.arrange(p1,p2,ncol = 1)
