#' input mean_x, mean_y, Var_x, Var_y, correlation (x,y)
#' install.packages : readxl
#' no missing value is allowed
#' Please refer to the excel file for the data format.
#' @excemples
#' \dontrun{
#'  #simulation data
#'   XA<-c(1,2,3,4,5,6,1,2,3,4,5,6);YA<-c(2,3,4,5,6,7,0,1,2,3,4,5);A<-data.frame(XA,YA)
#'   XB<-c(7,8,9,10,11,12,7,8,9,10);YB<-c(8,9,10,11,12,9,5,6,7,8);B<-data.frame(XB,YB)
#'   XC<-c(1,2,3,1,2,3,4,5,6,7,8,11);YC<-c(5,7,9,11,2,1,4,5,6,8,10,13);C<-data.frame(XC,YC)
#'   colnames(A) <- c("X", "Y");colnames(B) <- c("X", "Y");colnames(C) <- c("X", "Y")
#'   r_i <- c(cor(A)['X','Y'],cor(B)['X','Y'],cor(C)['X','Y']) # corr
#'   Xmean_i <- c(mean(A$X),mean(B$X),mean(C$X)) # X Mean
#'   Ymean_i <- c(mean(A$Y),mean(B$Y),mean(C$Y)) # Y Mean
#'   varX_i <- c(var(A$X),var(B$X),var(C$X))  # X Varience
#'   varY_i <- c(var(A$Y),var(B$Y),var(C$Y))  # Y Varience
#'   n_i <- c(nrow(A),nrow(B),nrow(C)) # Sample size
#'   data<-cbind(r_i,Xmean_i,Ymean_i,varX_i ,varY_i,n_i)
#'   write.csv(data, file = paste0("C:/Users/Users/Desktop/FL_corr.csv"), row.names = FALSE)
#'  }

#'install.packages : readxl
 
data<-read.csv("your data path/test.csv",header = T)
FL_corr <- function(data) {
  # read data
  data=data
  
  # read data information from Excel
  r_i <- data$r_i    #corr
  Xmean_i <- data$Xmean_i  # X mean
  Ymean_i <- data$Ymean_i  # Y mean
  varX_i <- data$varX_i  # X variance
  varY_i<- data$varY_i  # Y variance
  n_i <- data$n_i  # sample size
  
  # Calculate how many local source information
  k <- nrow(data)
  
  n_total <- sum(n_i) # total sample size
  global_Xmean <- sum(Xmean_i * n_i) / n_total
  global_Ymean <- sum(Ymean_i * n_i) / n_total
  
  # calculate SXX
  SXX <- sum((n_i - 1) * varX_i) + sum(n_i * (Xmean_i - global_Xmean)^2)
  
  # calculate SYY
  SYY <- sum((n_i - 1) * varY_i) + sum(n_i * (Ymean_i - global_Ymean)^2)
  
  # calculate SXY
  SXY <- sum(r_i * (n_i - 1) * sqrt(varX_i * varY_i)) +
    sum(n_i * (Xmean_i - global_Xmean) * (Ymean_i - global_Ymean))
  
  # calculate gobal r
  r_final <- SXY / sqrt(SXX * SYY)
  
  # calculate  MSE
  MSE_total <- (sum(SYY) / (sum(n_i) - 2)) * (1 - r_final^2)
  # calculate point estimate
  beta_hat_total<-SXY/SXX
  # calculate standard deviation
  se_beta_total <- sqrt(MSE_total / SXX)
  
  # output
  cat("local source =", k, "\n")
  cat("gobal corr =", r_final, "\n")
  cat("point estimate =", beta_hat_total, "\n")
  cat("standard deviation =", se_beta_total, "\n")
}

