
fancy <- read.csv("../data/fancyhouse.csv")
house <- read.csv("../data/housingprice.csv")
train <- read.csv("../data/train.data.csv")
test  <- read.csv("../data/test.data.csv")

library(kableExtra)
library(dplyr)
library(ggplot2)
options(digits=6)



### (a)

# R2 indicates the percentage of the variance in the dependent variable that the independent variables explain collectively.

train.house <- lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot, data = train)
# summary(train.house)$r.squared # 0.5101139

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
fit.house <- lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot, data = house)
pred <- predict(fit.house, fancy) # 15,390,168

out <- cbind("15,390,168", "127,000,000") %>% as.data.frame()
colnames(out) <- c("Predicted Value", "Real Value")
rownames(out) <- "Bill Gate's House"

knitr::kable(out, align = "c") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)



### (c)
train.house.2 <- lm(price ~ sqft_living+sqft_lot+bedrooms*bathrooms, data = train)
summary(train.house.2)

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


##################### For linear model fit with no interaction term:
dat.lm <- train[,c(5:8,4)]
dat.lm.scale <- scale(dat.lm)
y <- dat.lm.scale[,ncol(dat.lm.scale)]
X <- cbind(1, dat.lm.scale[,-ncol(dat.lm.scale)])

theta_opt_lm <- summary(lm(y ~ X[,2]+X[,3]+X[,4]+X[,5]))$coefficients[,1]

dat.lm.test <- test[,c(5:8,4)]

##################### For linear model fit with interaction term:
dat.lm.int <- cbind(train[c(5:8)], train[5]*train[6], train[4])
colnames(dat.lm.int) <- c("bedrooms", "bathrooms", "sqft_living","sqft_lot",
                         "interaction", "price")

dat.lm.int.scale <- scale(dat.lm.int)
y <- dat.lm.int.scale[,ncol(dat.lm.int.scale)]
X <- cbind(1, dat.lm.int.scale[,-ncol(dat.lm.int.scale)])

dat.lm.int.test <- cbind(test[c(5:8)], test[5]*test[6], test[4])
colnames(dat.lm.int.test) <- c("bedrooms", "bathrooms", "sqft_living","sqft_lot",
                         "interaction", "price")
dat.lm.int.scale.test <- scale(dat.lm.int.test)
y.test <- dat.lm.int.scale.test[,ncol(dat.lm.int.scale.test)]
X.test <- cbind(1, dat.lm.int.scale.test[,-ncol(dat.lm.int.scale.test)])

theta_opt_lm.int <- summary(lm(y ~ X[,2]+X[,3]+X[,4]+X[,5]+X[,6]))$coefficients[,1]



##################### Create a Gradient Descent Function:
gda <- function(data, eps = 0.001, max.iter = 50, standardize = T, seed=123,
                optimal.theta) {
  
  set.seed(seed)
  #scaling data 
  data <- as.matrix(data)
  p <- ncol(data)
  n <- nrow(data)
  if(standardize) {data <- scale(data)} # scale data if required

  #predictor and response 
  X <- cbind(X0 = 1, data[,-p]) # add a column of 1 to serve as the intercept
  y <- data[,p]
  
  #starting values of theta
  theta <- matrix(runif(n = p), 
                  ncol = p, nrow=1)
  theta.new <- theta
  
  ###gradient descent###
  
  #tuning parameter
  eigen <- eigen(t(X) %*% X, only.values = TRUE)
  stepsize <- 2 / (eigen$values[1] + eigen$values[p])
  
  #iteration
  step <- 1
  while ( step <= max.iter & norm(optimal.theta - theta) > eps ) { 
    theta <- theta.new
    res <- (X %*% t(theta)) - y
    gradient <- t(res) %*% X
    theta.new <- theta - gradient * stepsize
    step <- step + 1
    print(norm(optimal.theta - theta))
  }
  return(theta)
}

GD.out <- gda(train[,c(5:8,4)], standardize = TRUE, optimal.theta = theta_opt_lm)

GD.out.int <- gda(dat.lm.int, 
                  standardize = TRUE, optimal.theta = theta_opt_lm.int,
                  max.iter = 500)

## R-square for training data
get.r2 <- function (train = train[,c(5:8,4)],
                    test = test[,c(5:8,4)], SGD_result) {
  ## R-square for training data
  X.prep <- scale(train)
  X <- cbind(X0 = 1, X.prep[,-ncol(X.prep)])
  pred.y <- X %*% SGD_result[1,]
  r2.train <- rsquare(X.prep[,5], pred.y)

  ## R-square for test data
  X.prep <- scale(test)
  X <- cbind(X0 = 1, X.prep[,-ncol(X.prep)])
  pred.y <- X %*% SGD_result[1,]
  r2.test <- rsquare(X.prep[,5], pred.y)

  return(c(r2.train,r2.test))
}



r2.lm <- get.r2(train[,c(5:8,4)], test[,c(5:8,4)], GD.out)
r2.lm.int <- get.r2(dat.lm.int, dat.lm.int.test, GD.out.int)


## make a table for comparison
out <- cbind(c(train.r2, test.r2),c(r2.lm[1], r2.lm[2]), 
             c(train.r2.2, test.r2.2),c(r2.lm.int[1],r2.lm.int[2])) %>% as.data.frame()
colnames(out) <- c("lm Fit", "Gradient Descent - lm", 
                   "lm with Interaction Fit", "Gradient Descent - lm+int")
rownames(out) <- c("Training Data", "Test Data")

knitr::kable(out, align = "c", caption = "R-square Comparison") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)




### (e)


sgda <- function(data, data_test, max.iter = 30, stepsize = 5, diminish = TRUE, 
                 standardize = T, seed=123, optimal = theta_opt_lm.int) {

  set.seed(seed)
  #scaling data
  data <- as.matrix(data)
  data_test <- as.matrix(data_test)
  p = ncol(data)
  n = nrow(data)
  if (standardize) { 
    data <- scale(data)
    data_test <- scale(data_test)
  } # scale data if required


  #predictor and response
  X <- cbind(X0 = 1, data[,-p]) # add a column of 1 to serve as the intercept
  y <- data[,p]
  
  #test data
  X.test <- cbind(X0 = 1, data_test[, -p])
  y.test <- data_test[,p]
  
  #starting values of theta
  theta <- matrix(runif(n = p), ncol = p, nrow = 1)
  train.loss <- norm(y - X %*% t(theta), "2") 
  test.loss <- norm(y.test - X.test %*% t(theta), "2")

  #iteration
  j <- 0
  ref <- 1
  stochastic.list <- sample(1:n, n)

  while ( j <= max.iter ) {
    j <- j + 1

    if(diminish){
      eta <- stepsize / (j + 1)
    }else{
      eta <- stepsize 
    }
    
    
    X.new <- X[stochastic.list[j],] %>% as.matrix()

    res <- ( t(X.new) %*% t(theta) ) - y[stochastic.list[j]]
    gradient <- t(res) %*% t(X.new)
    theta <- theta - gradient * eta
    ref <- norm(gradient,"2")
    train.loss <- c(train.loss, norm(y - X %*% t(theta), "2")   )
    test.loss <- c(test.loss, norm(y.test - X.test%*%t(theta), "2")  )
    print(eta)
    print(norm(optimal - theta))
  }
  
  color = c("train.loss" = "black", "test.loss" = "red")
  gg = ggplot() + 
    geom_point(aes(x = 1:length(test.loss), y = test.loss, color = "test.loss"), size = 0.5) + 
    geom_line(aes(x = 1:length(test.loss), y = test.loss, color = "test.loss"), size = 0.5)  + 
    geom_point(aes(x = 1:length(train.loss), y = train.loss, color = "train.loss"), size = 0.5) + 
    geom_line(aes(x = 1:length(train.loss),y  = train.loss, color = "train.loss"), size = 0.5) +
    xlab("iteration") + 
    ylab("L2 Loss") + 
    labs(title = "Loss function ", color = "Legend") +
    theme(plot.title = element_text(hjust = 0.5, size = 20)) + 
    scale_color_manual(values = color)
  
  show(gg)
  print(paste0("Rsquare for training data is ", rsquare(y, X%*%t(theta))))
  print(paste0("Rsquare for test data is ", rsquare(y.test, X.test %*% t(theta))))
  

  # theta <- y * theta/X
  return(theta)
}


checkStrict(sgda)

##### NO interaction 
#fix stepsize
sgda(dat.lm, dat.lm.test, standardize = T, stepsize = 2, diminish = TRUE, max.iter = 3000, optimal = theta_opt_lm)
#diminish stepsize
sgda(dat.lm, dat.lm.test, standardize = T, stepsize = 0.02, diminish = FALSE, max.iter = 3000, optimal = theta_opt_lm)




##### interaction 
# SGD.out <- sgda(train[,c(5:8,4)], standardize = T, stepsize = 1, max.iter = 1000)
#fix stepsize
sgda(dat.lm.int, dat.lm.int.test, standardize = T, stepsize = 0.02, diminish = FALSE, max.iter = 3000) #Rsquare 0.5149 / 0.509 
#diminish stepsize
sgda(dat.lm.int, dat.lm.int.test, standardize = T, stepsize = 2, diminish = TRUE, max.iter = 3000) #Rsquare 0.486 / 0.486






output <- sapply(seq(1,10,1), 
                 FUN = function(x) get.r2(train[,c(5:8,4)],test[,c(5:8,4)],
                                          sgda(train[,c(5:8,4)], standardize = T, 
                                               stepsize = x, max.iter = 2000)))

# bibi <- sapply(seq(1,10,1), FUN = function(x) get.r2(int.data, int.data.test,
#   SGD_result = sgda(dat.lm.int, standardize = T, stepsize = x, max.iter = 100)))

# save(output, file = "/Users/xuchenghuiyun/Desktop/STA243/data/output.rda")
# load("/Users/xuchenghuiyun/Desktop/STA243/data/output.rda")


## select stepsize
out.new <- t(output) %>% as.data.frame()
out.new <- cbind(as.character(seq(1,10,1)), output)
colnames(out.new) <- c("c in stepsize = c/t+1","Training Data", "Test Data")

knitr::kable(out.new, align = "c", caption = "Tuning Parameter Selection") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)


## make a table for comparison
ok <- get.r2(sgda(train[,c(5:8,4)], standardize = T, stepsize = 2, max.iter = 2000))

out <- cbind(c(train.r2, test.r2),c(ok[1], ok[2])) %>% as.data.frame()
colnames(out) <- c("lm Fit", "Stochastic Gradient Descent")
rownames(out) <- c("Training Data", "Test Data")

knitr::kable(out, align = "c", caption = "R-square Comparison") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)



