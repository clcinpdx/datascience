# load diamond dataset
data("diamonds") 

# diamonds - price vs. carats
qplot(x=price,y=carat,data=diamonds)

# price per carat of diamonds across
# the different colors of diamonds using boxplots

p1 = qplot (x=color,y=price,data=diamonds, 
        xlab = 'Diamond Color', 
        ylab = 'Diamond Price',
        geom = 'boxplot') +
     coord_cartesian(ylim=c(0,7750))

p2 = qplot (x=clarity,y=price,data=diamonds, 
            xlab = 'Diamond Clarity', 
            ylab = 'Diamond Price',
            geom = 'boxplot') +
  coord_cartesian(ylim=c(0,6500))

grid.arrange (p1,p2,ncol =1)

