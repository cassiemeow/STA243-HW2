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
gda <- function(X, y, stepsize, iteration, epsilon = 0.001, normalize = T) {

  X  <- as.matrix(X)
  y <- as.matrix(y)
  
  if(normalize) {X <- scale(X)} # scale data if required
	X <- cbind(X0 = 1, X) # add a column of 1 to serve as the intercept
  
  # create a function for obtaining first derivative of f(theta)
  derivative <- function(cov, res, theta) {
    deriv <- (t(cov) %*% ((cov %*% t(theta)) - res)) /nrow(res)
    return(t(deriv))
  }
	
	theta <- t(as.matrix(runif(n = ncol(X))))
	theta.new <- theta + 1 # randomly assign initial values for theta

	rec <- vector("list", iteration) 
	rec[[1]] <- theta.new

	step <- 1 
	while(any(abs(theta.new - theta) > epsilon) & step <= iteration) {
		# gradient descent 
		theta.new <- theta - stepsize * derivative(X, y, theta)
		rec[[step]] <- theta.new
		
		step <- step + 1
	}
	
	out <- data.frame(do.call(rbind, rec), row.names = NULL)
	return(list(out = out))
}
