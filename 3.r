
fancy = read.csv("fancyhouse.csv")
house = read.csv("housingprice.csv")
train = read.csv("train.data.csv")
test = read.csv("test.data.csv")

### (a)
# R2 indicates the percentage of the variance in the dependent variable that the independent variables explain collectively.

train.house = lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot, data = train)
summary(train.house)$r.squared # 0.5101139

test.house = lm(price ~  bedrooms + bathrooms + sqft_living + sqft_lot, data = test)
summary(test.house)$r.squared # 0.5054477 

### (b)
fit.house     = lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot, data = house)
predict(fit.house, fancy)

### (c)
train.house.2 = lm(price ~ sqft_living + sqft_lot + bedrooms*bathrooms, data = train)
summary(train.house.2)$r.squared # 0.5173533

test.house.2  = lm(price ~ sqft_living + sqft_lot + bedrooms*bathrooms, data = test)
summary(test.house.2)$r.squared # 0.5110569

### (d)

#check global variable in function 
checkStrict <- function(f, silent=FALSE) {
  vars <- codetools::findGlobals(f)
  found <- !vapply(vars, exists, logical(1), envir=as.environment(2))
  if (!silent && any(found)) {
    warning("global variables used: ", paste(names(found)[found], collapse=', '))
    return(invisible(FALSE))
  }
  !any(found)
}


gda <- function(data, eps = 0.01, max.iter = 30, standardize = T, seed=123) {
  
  set.seed(seed)
  #scaling data 
  data <- as.matrix(data)
  p = ncol(data)
  n = nrow(data)
  if(standardize) {data <- scale(data)} # scale data if required

  #predictor and response 
  X <- cbind(X0 = 1, data[,-p]) # add a column of 1 to serve as the intercept
  y <- data[,p]
  
  #oracke solution(after passing the test, can be removed)
  theta_opt = summary(lm(y ~ X[,2] + X[,3] + X[,4] + X[,5]))$coefficients[,1]
  
  #starting values of theta
  theta <- matrix(runif(n = p), ncol = p, nrow=1)
  theta.new <- theta
  
  ###gradient descent###
  
  #tuning parameter
  eigen = eigen(t(X) %*% X, only.values = TRUE)
  stepsize = 2 / (eigen$values[1] + eigen$values[p])
  
  #iteration
  step <- 1
  while ( step <= max.iter & norm(theta_opt - theta) > eps ) { 
    theta = theta.new
    res <- (X %*% t(theta)) - y
    gradient = t(res) %*% X
    theta.new = theta - gradient * stepsize
    step <- step + 1
    print(norm(theta_opt - theta))
  }
  return(theta)
}






