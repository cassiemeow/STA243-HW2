fancy = read.csv(".../fancyhouse.csv")
house = read.csv(".../housingprice.csv")
train = read.csv(".../train.data.csv")
test = read.csv(".../test.data.csv")

### (a)
# R2 indicates the percentage of the variance in the dependent variable that the independent variables explain collectively.
#train.house = lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot, data = train)
#summary(train.house)$r.squared # 0.5101139
#test.house = lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot, data = test)
#summary(test.house)$r.squared # 0.5054477 

train.X <- as.matrix(cbind(rep(1, nrow(train)), apply(train[,5:8],2,scale)))
train.y <- as.matrix(train[,4])
fit <- lm(train.y ~ train.X[,-1])

test.X <- as.matrix(cbind(rep(1, nrow(test)), apply(test[,5:8],2,scale)))
test.y <- as.matrix(test[,4])

pred.y <- test.X %*% matrix(fit$coefficients, ncol = 1, nrow = ncol(test.X))
   
# function to calculate R-square
rsquare <- function(obs, fitted) {
  r2 = 1 - (sum((obs-fitted)^2)) / (sum((obs-mean(obs))^2))
  return(r2)
}
 
train.r2 <- rsquare(train$price, fit$fitted.values) # 0.5101139
test.r2 <- rsquare(test.y, pred.y) # 0.5049332



### (b)
fit.house = lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot, data = house)
predict(fit.house, fancy)



### (c)
#train.house.2 = lm(price ~ sqft_living+sqft_lot+bedrooms*bathrooms, data = train)
#summary(train.house.2)$r.squared # 0.5173533
#test.house.2 = lm(price ~ sqft_living+sqft_lot+bedrooms*bathrooms, data = test)
#summary(test.house.2)$r.squared # 0.5110569

fit.2 <- lm(train.y ~ train.X[,2]*train.X[,3]+train.X[,4]+train.X[,5])
test.X.2 <- as.matrix(cbind(test.X, test.X[,2]*test.X[,3]))

pred.y.2 <- test.X.2 %*% matrix(fit.2$coefficients, ncol = 1, nrow = ncol(test.X.2))

train.r2.2 <- rsquare(train$price, fit.2$fitted.values) # 0.5173533
test.r2.2 <- rsquare(test.y, pred.y.2) # 0.5105277



### (d)
gda <- function(data, stepsize, max.iter, standardize = T, seed=123) {
  
  set.seed(seed)
  data <- as.matrix(data)
  if(standardize) {data <- scale(data)} # scale data if required
  theta <- matrix(runif(n = ncol(data)), ncol=ncol(data), nrow=1)
  
  X <- cbind(X0 = 1, data[,-ncol(data)]) # add a column of 1 to serve as the intercept
  y <- data[,ncol(data)]
  
   
  # randomly generates starting values of theta
  theta.new <- matrix(ncol=length(theta), nrow=1)
  n <- nrow(X)
  
  step <- 1
  while ( step <= max.iter ) { 
    res <- (X %*% t(theta)) - y
    
    for(i in 1:length(theta)) {
      p <- res * X[,i]
      gradient <- sum(p) / n
      theta.new[1,i] <- theta[1,i] - stepsize * gradient
    }
    step <- step + 1
    theta <- theta.new
  }
  return(theta)
}

gda.version2 <- function(data, stepsize=0.01, epsilon=0.1, seed=123) {
  
  set.seed(seed)
  data <- as.matrix(data)
  data <- apply(data,2,scale)
  
  X <- cbind(X0 = 1, data[,-ncol(data)]) # add a column of 1 to serve as the intercept
  Xmean <- apply(X,2,mean)
  Xsd <- apply(X,2,sd)
  
  y <- data[,ncol(data)]
  ymean <- mean(y)
  ysd <- sd(y)

  theta <- runif(n = ncol(X)) # randomly generates starting values of theta
  n <- nrow(X)
  
  thres <- 2 * epsilon
  while ( thres > epsilon ) { 
    # gradient <- t(X) %*% X %*% theta - t(X) %*% y
    res <- (X %*% theta) - y
    gradient <- (t(X) %*% res) / n
    theta <- theta - stepsize * gradient
    thres <- norm(gradient,"2")
  }
  theta.new <- ysd * theta/Xsd
  return( theta.new )
}
