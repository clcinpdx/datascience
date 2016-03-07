set.seed(1234)
X <- rep(c("A","B"),20)
Y <- sample(c("C","D"),40,replace=T)

table(X,Y)

chisq.test(table(X,Y),correct=F)
# I don't use Yates continuity correction

#Let's make a matrix with tons of columns

Data <- as.data.frame(
  matrix(
    sample(letters[1:3],2000,replace=T),
    ncol=25
  )
)

# You want to select which columns to use
columns <- c(3,7,11,24)
vars <- names(Data)[columns]

# say you need to know which ones are associated with each other.
out <-  apply( combn(columns,2),2,function(x){
  chisq.test(table(Data[,x[1]],Data[,x[2]]),correct=F)$p.value
})

out <-  apply( combn(loan$bad_loan,loan$addr_state),2,function(x){
  chisq.test(table(loan[,x[1]],loan[,x[2]]),correct=F)$p.value
})


out <- cbind(as.data.frame(t(combn(vars,2))),out)
