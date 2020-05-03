
fancy = read.csv("fancyhouse.csv")
house = read.csv("housingprice.csv")
train = read.csv("train.data.csv")
test = read.csv("test.data.csv")

### (a)

# R2 indicates the percentage of the variance in the dependent variable that the independent variables explain collectively.

train.house = lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot, data = train)
# summary(train.house)$r.squared # 0.5101139

# test.house = lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot, data = test)
# summary(test.house)$r.squared # 0.5054477

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

out <- cbind(train.r2, test.r2) %>% as.data.frame()
colnames(out) <- c("Training Data", "Test Data")
rownames(out) <- "R-square Value"

knitr::kable(out, align = "c") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)



### (b)
fit.house = lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot, data = house)
pred <- predict(fit.house, fancy) # 15,390,168

out <- cbind("15,390,168", "127,000,000") %>% as.data.frame()
colnames(out) <- c("Predicted Value", "Real Value")
rownames(out) <- "Bill Gate's House"

knitr::kable(out, align = "c") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)



### (c)
train.house.2 = lm(price ~ sqft_living+sqft_lot+bedrooms*bathrooms, data = train)
summary(train.house.2)

# test.house.2 = lm(price ~ sqft_living+sqft_lot+bedrooms*bathrooms, data = test)
# summary(test.house.2)$r.squared # 0.5110569

fit.2 <- lm(train.y ~ train.X[,2]*train.X[,3]+train.X[,4]+train.X[,5])
test.X.2 <- as.matrix(cbind(test.X, test.X[,2]*test.X[,3]))

pred.y.2 <- test.X.2 %*% matrix(fit.2$coefficients, ncol = 1, nrow = ncol(test.X.2))

train.r2.2 <- rsquare(train$price, fit.2$fitted.values) # 0.5173533
test.r2.2 <- rsquare(test.y, pred.y.2) # 0.5105277

out <- cbind(train.r2.2, test.r2.2) %>% as.data.frame()
colnames(out) <- c("Training Data", "Test Data")
rownames(out) <- "R-square Value"

knitr::kable(out, align = "c") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)




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


### (e)

sgda <- function(data, eps = 0.01, max.iter = 30, stepsize = 0.1, standardize = T, seed=123) {
  
  set.seed(seed)
  #scaling data 
  data <- as.matrix(data)
  p = ncol(data)
  n = nrow(data)
  if(standardize) {data <- scale(data)} # scale data if required

  #predictor and response 
  X <- cbind(X0 = 1, data[,-p]) # add a column of 1 to serve as the intercept
  y <- data[,p]
  stochastic.list <- sample(1:n, max.iter, replace=TRUE)
  
  #oracke solution(after passing the test, can be removed)
  theta_opt = summary(lm(y ~ X[,2] + X[,3] + X[,4] + X[,5]))$coefficients[,1]
  
  #starting values of theta
  theta <- matrix(runif(n = p), ncol = p, nrow=1)
  theta.new <- theta
  
  ###gradient descent###
  
  #iteration
  step <- 1
  while ( step <= max.iter & norm(theta_opt - theta) > eps ) { 
    theta = theta.new
    res <- (X[stochastic.list,] %*% t(theta)) - y[stochastic.list]
    gradient = t(res) %*% X[stochastic.list,]
    theta.new = theta - gradient * stepsize
    step <- step + 1
    print(norm(theta_opt - theta))
  }
  return(theta)
}



