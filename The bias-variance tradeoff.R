# TO play with this simulation, execute every line of code.  
# Then, you can repeat the simulation using only the last line of code.

library(splines)

set.seed(1)
n = 100
x = runif(n) 
x= sort(x)

f = function(x){
  y = 10*(sin(x*pi*1.8) + 3*x) 
  return(y)
}

plotNS = function(fit, col){
  xDense = seq(0,1,len=100)
  lines(cbind(xDense, predict(fit, data.frame(x = xDense))), col = col, lwd = 3)
}


sampleAndFit = function(){
  y = f(x)+3*rnorm(n)
  
  plot(x,y, xlim =c(0,1), ylim = c(0,25))
  
  
  fit1 = lm(y~x) 
  fit2 = lm(y~ns(x, df = 5)) 
  fit3 = lm(y~ns(x, df = 20)) 
  
  # plotNS(fit1, col = "orange")  # comment and uncomment these lines of code
  # plotNS(fit2, col = "blue")
  # plotNS(fit3, col = "darkgreen")
  
  lines(cbind(x,f(x)), lwd =3)
  
}

# The black line is the true regression function (not observed in practice!)
sampleAndFit()  # repeat this line of code!  
# If you have this file open in R studio, then highlight that line and hit command+return, then again and again.
# After you have done it a few times, go to lines 32-34 and comment/uncomment those lines.