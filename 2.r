fancy = read.csv(".../fancyhouse.csv")
house = read.csv(".../housingprice.csv")
train = read.csv(".../train.data.csv")
test = read.csv(".../test.data.csv")

### (a)
# R2 indicates the percentage of the variance in the dependent variable that the independent variables explain collectively.

train.house = lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot, data = train)
summary(train.house)$r.squared # 0.5101139

test.house = lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot, data = test)
summary(test.house)$r.squared # 0.5054477 

### (b)
fit.house = lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot, data = house)
predict(fit.house, fancy)

### (c)
train.house.2 = lm(price ~ sqft_living+sqft_lot+bedrooms*bathrooms, data = train)
summary(train.house.2)$r.squared # 0.5173533

test.house.2 = lm(price ~ sqft_living+sqft_lot+bedrooms*bathrooms, data = test)
summary(test.house.2)$r.squared # 0.5110569

### (d)
gda <- function(X, y, stepsize, max.iter, epsilon = 0.001, standardize = T) {
  
  if(standardize) {X <- scale(X)} # scale data if required
  X <- as.matrix(cbind(X0 = 1, X)) # add a column of 1 to serve as the intercept
  
  theta <- matrix(runif(n = ncol(X)), ncol=ncol(X), nrow=1) 
  # randomly generates starting values of theta
  theta.new <- matrix(1, ncol=length(theta), nrow=1)
  n <- nrow(X)
  
  step <- 0
  while ( step <= max.iter ) { 
    error <- (X %*% t(theta)) - y
    
    if ( abs(theta.new-theta) < epsilon ) { 
      break 
      } 
    else {
      for(i in 1:length(theta)) {
        gradient <- sum(error * X[,i]) / n
        theta.new[1,i] <- theta[1,i] - stepsize * gradient
      }
      step <- step + 1
    }
  }
  if (step == max.iter) {
    print("The optimal value does not converge")
  }
  return(theta.new)
}
