#parameter columns
#DATE    OPEN 	    HIGH	    LOW	       CLOSE	
#VOLUME	 P_VC       VOLUMERMB	REAL       PRICE    MA5
#MA10	   MA20	      MA50	    MA60	     DIFFMA	   
#NC2	   P_NC2      NC3	      P_NC3      NC5	     
#P_NC5   NC10	      P_NC10	  C_MA5      C_MA10
#EPS	   PE	        SSEC	    DIFFSSEC	 P_DIFFSSEC 
#RSI6    RSI12      RSI24     MACD       K            D      RAVI
#ADX	   BB	        HH2	      HH3	       HH5          LL2	   LL3	
#expected parameter HH10
#label columns
#L1	L2	L3	L4	L5	L6	L7	L8	L9	L10	L15	L20	L25	L30	L40	L50													
setwd("~/accomplishment/work/project of ST5218/code")
library(e1071)
data = read.csv(file="600000_data.csv", header=T,sep=";")
acc <- 0
row_data <- nrow(data)
#predict period:1:10, 15, 20, 25, 30, 40, 50
train_begin_ind <- 1026
train_end_ind <- 1891#1891
last_ind <- 2263

# establish training set
train_ind <- c(train_begin_ind: train_end_ind)
#train_col_name <- c("D","ADX","BB","DIFFMA","MACD","RAVI","RSI6","RSI12","RSI24","P_VC")
train_col_name <- c("C_MA10","C_MA5","LL2","HH2","K","D","ADX","BB","DIFFMA","MACD","RAVI","RSI6","RSI12","RSI24","P_VC")
label_col_name <- c("L50")
row_train <- length(train_ind)
#train_label <- data[train_ind,label_col_name]
train_data <- data[train_ind,train_col_name]

# establish testing set
test_begin_ind <- train_end_ind+1  # for L50 we should use train_end_ind-48
test_ind <- c(test_begin_ind:last_ind)
row_test <- length(test_ind)
#test_label <- data[test_ind,label_col_name]
test_data <- data[test_ind,train_col_name]
# full data
full_ind <- c(train_begin_ind:last_ind)
full_data <-data[full_ind,train_col_name]
# trading period data
trading_begin_ind <- test_begin_ind
trading_ind <- c(trading_begin_ind:last_ind)
row_trading <- length(trading_ind)



# svm fit
first_pre_ind <- 52;
acc <- 0
for (i in 1: 16)
{
  train_label <- data[train_ind,first_pre_ind+i]
  test_label <- data[test_ind,first_pre_ind+i]
#  model <- svm(train_label ~ ., data = train_data, 
#                    method = "C-classification",kernel = "polynomial",degree=3, cost=1, gamma=(1/ncol(train_data)),scale=TRUE)
# summary(model)
  model <- svm(train_label ~ ., data = train_data, 
                     method = "C-classification",kernel = "polynomial",degree=3, cost=0.5, gamma=(1/ncol(train_data)),scale=TRUE)                   
# model <- glm(train_label ~. , data = train_data, family = "binomial")
# determine accuracy applying test data
  acc[i] <- mean(test_label == ifelse(predict(model,test_data)>0,1,0))

# summary(model)
}

# choose the best model
max_ind <- which.max(acc)
train_label <- data[train_ind, max_ind + first_pre_ind]
model <- svm(train_label ~ ., data = train_data, 
                 method = "C-classification",kernel = "polynomial",degree=3, cost=0.5, gamma=(1/ncol(train_data)),scale=TRUE)
#model <- glm(train_label ~. , data = train_data, family = "binomial")
summary(model)



#plot(c(1:length(pr)), wealth, type = "l", xlim=c(0, length(pr)), ylim = c(0,max(wealth)), col = "red", ylab = "BANLANCE", xlab = "DATE")
# points(c(1:row), margin, type = "l", col = "blue")


# trading strategy
# hld = indicator of holding position
# balance = initial account balance
# profit = record profit of every trade
# n_enter & time_enter & t = variables for ploting
hld <- 0
balance <- 1
profit <- 0
n_enter <- 0
wealth <- 1
pr <- ifelse(predict(model,test_data)>0,1,0)
# The variable balance_2, time_sell & t for plotting
balance_2 <- 1
t <- 2
time_sell <- 1

decision_vector <-0

# enter = the price when entering a position
# posit = position size
for (i in 1: row_trading){
  stock_ind <- i + trading_begin_ind - 1
  n_enter[i] <- 0
  if (pr[i] == 1)
  {
    if (1)#(data$CLOSE[stock_ind]-data$CLOSE[stock_ind-49])<0) #to buy or keep hold
    {
      decision_vector[i] <- 1
      if (hld == 0)
      {
        hld <- 1
        enter <- data$CLOSE[stock_ind]
        n_enter[i] <- enter
        if (i == 1)
        {
          posit <- wealth[i]/data$CLOSE[stock_ind]
        }
        else  #i>1
        {
          posit <- wealth[i-1]/data$CLOSE[stock_ind]
          wealth[i] <- wealth[i-1] 
        }
      }
      else #(hld == 1)
      {
        wealth[i] <- posit*data$CLOSE[stock_ind]
      }
    }
    else #((data$CLOSE[stock_ind]-data$CLOSE[stock_ind-49])>=0)
    {
      decision_vector[i] <- 0;
      if (hld == 1)
      {
        wealth[i] = posit*data$CLOSE[stock_ind] 
      }
      else #hld == 0
      {
        if(i>1)
          wealth[i] <- wealth[i-1]
      }
    }
  }
  if(pr[i] == 0) 
  {
    decision_vector[i] <- 0;
    if (hld == 1)
    {
      hld <- 0
      wealth[i] = posit*data$CLOSE[stock_ind]
      profit = posit*(data$CLOSE[stock_ind]  - enter)
      posit <- 0
      enter <- 0   
      balance_2[t] <- balance_2[t-1] + profit#only nonzero for the sell day
      time_sell[t] <- i
      t <- t + 1
    }
    else #hld == 0
    {
      if(i>1)
          wealth[i] <- wealth[i-1]
    }
  }
  if (i > 1)
  {
    balance[i] <- balance[i-1] + profit
    #wealth[i] <- wealth[i-1] + posit*(data$CLOSE[stock_ind]-data$CLOSE[stock_ind-1])
    profit <- 0
  }
}

# establish the final data frame
label_ind <- c(1:10, 15, 20, 25, 30, 40, 50)
acc_frame <- data.frame(label = label_ind, accuracy = acc*100)
balance_frame <- data.frame(ssec = data$SSEC[trading_ind]/data$SSEC[trading_begin_ind], stock = data$CLOSE[trading_ind],
                            bbindex = data$BB[trading_ind], balance = balance, n_enter = n_enter)
balance_2_frame <- data.frame(time = time_sell, balance = balance_2)
# plotting (ggplot2)
require(ggplot2)
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

# 1 
p <- ggplot(acc_frame, aes(x = label, y = accuracy))
plot_a <- p + geom_point(size = 5, shape = 1) + geom_line() + geom_hline(yintercept = 60, lty = 2) +
  labs(x = "Label Lag") + labs(y = "Accuracy (%)") + ggtitle("Accuracy vs. Dependent Variable")
# 2
p <- ggplot(balance_2_frame, aes(x = time, y = balance))
plot_b <- p + geom_line() + labs(x = "Date") + labs(y = "Balance") +
  ggtitle("Account Balance")
# 3
p <- ggplot(balance_frame, aes(x = c(1:row_trading), y = n_enter))
plot_c <- p + geom_point(size = 3, alpha = 0.8, colour = "red") +
  geom_line(aes(x = c(1: row_trading), y = stock), colour = "black") +
  ylim(min(balance_frame$stock),max(balance_frame$stock)) + ggtitle("Stock Price & Entering Points") +
  labs(x = "Date") + labs(y = "Stock price")

print(plot_a, vp = vplayout(1,1))
print(plot_b, vp = vplayout(2,1))
print(plot_c, vp = vplayout(3,1))
# 2 dimension
# new_data <- data.frame(train_data,train_label)
# p <- ggplot(new_data,aes(x=train_data,y=c(1:length(train_data))))
# p + geom_point(aes(colour = train_label))
acc[max_ind]


#max(data[c(1892:2263),c("CLOSE")])/min(data[c(1892:2263),c("CLOSE")])
#data[1892,c("CLOSE")]/data[2263,c("CLOSE")]
