# Corelation assignment:
#read data
data<-readRDS("F:/IITM sem4/STATISTICAL LEARNING/Q1_data_01.Rda")
#View(data)
# i) the correlation between the predictors and 
#also between the predictor and the outcome
cor(data)
cor(data,method = "spearman")
cor(data,method = "kendall")

# ii) Generate the scatterplot matrix
pairs(data)

# iii) 
# From the co-relation values we can see that there is a strong
# influence of x4 on y and x1,x2, x3 do not have a strong influence on y


# iv) Fitting linear model on data
model_fit <- lm(y ~ x1 + x2 + x3 + x4,data = data)
summary(model_fit)

# From the co-efficients of x1,x2 and x3 we can say that x1,x2 are
# significant predictors and x3,x4 are less significant


# Regression_polynomial fitting :

# i) Plots of the function y
y1 = function(x){exp(-5*(x-0.3)*(x-0.3))+0.5*exp(-100*(x-0.5)*(x-0.5))+0.5*exp(-100*(x-0.75)*(x-0.75))}
curve(y1, from=0, to=1.5 , xlab="x", ylab="y")

y2 = function(x){2-3*x+10*x**4 - 5*x**9 +6*x**14}
curve(y2, from=0, to=1 , xlab="x", ylab="y")

# ii) Randomly extract 100 points from the function and add 
#normally distributed noise to the data points to get "noisy data"

get_data = function(f, sample_size = 100) {
  x = runif(n = sample_size, min = 0, max = 1.5)
  eps = rnorm(n = sample_size, mean = 0, sd = 0.7)
  y_hat = f(x) + eps
  data.frame(x, y_hat)
}

data1 = get_data(y1)
print(data1)

# iii)Fitting polynomial of degree d = (7,11,24) to the noisy data

poly_fit_7 <- lm(y_hat ~ poly(x,degree = 7, raw = TRUE),data = data1)
summary(poly_fit_7)

poly_fit_11 <- lm(y_hat ~ poly(x,degree = 11, raw = TRUE),data = data1)
summary(poly_fit_11)

poly_fit_24 <- lm(y_hat ~ poly(x,degree = 24, raw = TRUE),data = data1)
summary(poly_fit_24)

# iv) Bias & variance of the models fitted

#x2 <- runif(n = 99,min = 0,max = 1.5)
#x2 <- data.frame(x2)

get_bias = function(estimate, truth) {
  mean(estimate) - truth
}

get_var = function(estimate){
  mean((estimate - mean(estimate))**2)
}

get_mse = function(truth, estimate) {
  mean((estimate - truth) ^ 2)
}


#yy = function(x){1.473-12.922*x + 74.949*x**2 - 61.165*x**3 -374.914*x**4 + 915.634*x**5 - 753.515*x**6 + 209.526*x**7}



set.seed(1)
n_s = 100
n_models = 3
x = data.frame(x = 0.95) # fixed point at which we make predictions
predictions = matrix(0, nrow = n_s, ncol = n_models)

for (i in 1:n_s) {
  
  data = get_data(y1)
  
  # fit models
  fit_7 = lm(y_hat ~ poly(x, degree = 7,raw = T), data = data)
  fit_11 = lm(y_hat ~ poly(x, degree = 11,raw = T), data = data)
  fit_24 = lm(y_hat ~ poly(x, degree = 24,raw = T), data = data)
  
  # predictions
  predictions[i, 1] = predict(fit_7, x)
  predictions[i, 2] = predict(fit_11, x)
  predictions[i, 3] = predict(fit_24, x)
}


bias = apply(predictions, 2, get_bias, truth = y1(x = 0.95))
variance = apply(predictions, 2, get_var)
mse = apply(predictions, 2, get_mse, truth = y1(x = 0.95))

# Bias and variance for the function 
#y = exp(-5*(x-0.3)*(x-0.3))+0.5*exp(-100*(x-0.5)*(x-0.5))+0.5*exp(-100*(x-0.75)*(x-0.75))

print('bias=')
bias
print('variance=')
variance
print('mse=')
mse

d_o_f = c(7,11,24)





#################### y2
predictions2 = matrix(0, nrow = n_s, ncol = n_models)
x1 <- data.frame(x=0.95)

for (i in 1:n_s) {
  
  data2 = get_data(y2)
  
  # fit models
  fit_7_2 = lm(y_hat ~ poly(x, degree = 7,raw = T), data = data2)
  fit_11_2 = lm(y_hat ~ poly(x, degree = 11,raw = T), data = data2)
  fit_24_2 = lm(y_hat ~ poly(x, degree = 24,raw = T), data = data2)
  
  # predictions
  predictions2[i, 1] = predict(fit_7_2, x1)
  predictions2[i, 2] = predict(fit_11_2, x1)
  predictions2[i, 3] = predict(fit_24_2, x1)
}


bias2 = apply(predictions2, 2, get_bias, truth = y2(x = 0.95))
variance2 = apply(predictions2, 2, get_var)
mse2 = apply(predictions2, 2, get_mse, truth = y2(x = 0.95))

# Bias and variance for the function 
# y = 2-3*x+10*x**4 - 5*x**9 +6*x**14

print('bias=')
bias2
print('variance=')
variance2
print('mse=')
mse2

## v)
# The bias-variance plot for the function
# y = exp(-5*(x-0.3)*(x-0.3))+0.5*exp(-100*(x-0.5)*(x-0.5))+0.5*exp(-100*(x-0.75)*(x-0.75))
plot(d_o_f, mse, xlab='Degree_of _freedom', ylab='MSE', type='p', col='orange', lwd=3, lty=1)
lines(d_o_f, bias, col='red', lwd=3, lty=2)
lines(d_o_f, variance, col='blue', lwd=3, lty=2)
legend(0.01,0.6,c('Bias', 'Variance', 'MSE'), col=c('red', 'blue', 'orange'))
plot(bias,variance,type = 'l',col = 'blue',lwd=3,lty=2)

#The bias-variance plot for the function
# y = 2-3*x+10*x**4 - 5*x**9 +6*x**14

plot(d_o_f, mse2, xlab='Degree_of _freedom', ylab='MSE', type='p', col='orange', lwd=3, lty=1,ylim = c(0,1))
lines(d_o_f, bias2, col='red', lwd=3, lty=2)
lines(d_o_f, variance2, col='blue', lwd=3, lty=2)
legend(0.01,0.6,c('Bias', 'Variance', 'MSE'), col=c('red', 'blue', 'orange'))
plot(bias2,variance2,type = 'l',col = 'blue',lwd=3,lty=2,xlab='bias', ylab='variance')
