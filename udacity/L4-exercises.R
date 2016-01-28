# load pseuo_facebook data
getwd()

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/udacity')

pf <- read.csv('pseudo_facebook.tsv',sep = '\t')

glimpse(pf)

# L4, p4 (transparency and jitter to better reflect age being continuous)
ggplot(aes(x=age,y=friend_count),data=pf,ylab='friends') +
  geom_jitter(alpha=1/20) +
  xlim(13,90)

# L4, p5 (coord_trans) : generated plot with two continuous variables
ggplot(aes(x=age,y=friend_count),data=pf,ylab='friends') +
  geom_point(alpha=1/20) +
  xlim(13,90) +
  coord_trans(y = "sqrt")
  
#L4, p6 (Alpha and Jitter) : friends initiated and age
qplot(x=age,y=friendships_initiated,data=pf)

ggplot(aes(x=age, y=friendships_initiated),data=pf)
ggplot(aes(x=age,y=friend_count),data=pf)

    geom_point(alpha=1/20) +
  xlim(13,90) +
  coord_trans(y = "sqrt")
