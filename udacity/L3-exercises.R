summary (pf)

install.packages("gridExtra")

summary (pf$friend_count)

# regular, log10, and square root
rm(p1)
rm(p2)
rm(p3)

p1 <- ggplot(aes(x=friend_count),data = subset(pf,!is.na(pf$gender))) + 
      geom_histogram() +
      facet_wrap(~gender)
p2 <- p1 + scale_x_log10()
p3 <- p1 + scale_x_sqrt()

grid.arrange(p1,p2,p3,ncol=1) 

# frequency Polygons

qplot (x=friend_count, y = ..count../sum(..count..),
    data = subset (pf, !is.na(pf$gender)),
    xlab = 'Friend Count',
    ylab = 'Proporation of Users with that friend count',
    binwidth = 10, geom = 'freqpoly', color = gender) +
  scale_x_continuous(lim = c(0,1000), breaks = seq(0,1000,50))+
  scale_y_log10()


# Number of likes on the web, data summarized and split between genders
by (pf$www_likes,pf$gender,sum)



