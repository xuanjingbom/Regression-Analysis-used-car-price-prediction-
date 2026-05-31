CarData <- read.table("C:/University/Year 3 Sem 1/Regression Analysis/DSC2304093.txt", header = TRUE)
set.seed(0611)
dt <- sort(sample(nrow(CarData), nrow(CarData)* 0.7))
CarData_e <- CarData[dt, ] #estimation
CarData_p <- CarData[-dt, ] #prediction

#1
plot(CarData_e $mileage, CarData_e $price, xlab = "mileage driven (miles)" , ylab = "used car price (Euros)")

#2
plot(CarData_e $mileage, log(CarData_e $price), xlab = "mileage driven" , ylab = "log(price)")
#abline(CarData_e.model, col = "blue", lwd = 2)  #fitted line

#3
cor(CarData_e $mileage, log(CarData_e $price))

#4
CarData_e.model <- lm(log(price) ~ mileage, data = CarData_e)
summary(CarData_e.model)

#5
log_price_p <- predict(CarData_e.model, newdata = CarData_p)
price_p <- exp(log_price_p) # Because the model is log(price), we need to use exp() to restore it to price

#6
plot(CarData_p $price, price_p, xlab = "Actual price from test set", ylab = "prediction price from model")

#7
conf_9446 <- predict(CarData_e.model, newdata = data.frame(mileage = 9446), interval = "confidence", level = 0.95)
exp(conf_9446)

#8
conf_18513 <- predict(CarData_e.model, newdata = data.frame(mileage = 18513), interval = "confidence", level = 0.95)
exp(conf_18513)

#9
pred_9446 <- predict(CarData_e.model, newdata = data.frame(mileage = 9446), interval = "prediction", level = 0.95)
exp(pred_9446)

#10
pred_18513 <- predict(CarData_e.model, newdata = data.frame(mileage = 18513), interval = "prediction", level = 0.95)
exp(pred_18513)





#extra 1
mileages <- c(100, 1000, 10000, 100000, 1000000)
new_mileage_df <- data.frame(mileage = mileages)

# Predict log(price)
log_price_preds <- predict(CarData_e.model, newdata = new_mileage_df)

# Convert back to price
price_preds <- exp(log_price_preds)

# Result table
data.frame(mileage = mileages, predicted_price = round(price_preds, 2))



