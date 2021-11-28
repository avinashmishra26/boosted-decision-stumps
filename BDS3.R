plot_graph <- function(X, Y){
  plot(X, Y, main="Boosting(B) Vs Test MSE Error",
     sub="For constant learning Rate = 0.01", xlab="Boosting(B - Iteration)", 
     ylab="Test MSE", lwd = 1, col='red')
}