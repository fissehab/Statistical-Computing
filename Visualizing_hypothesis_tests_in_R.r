#Visualizing hypothesis tests in R


#The red distribution is what you can expect to see if you plot
#repeated samples when the null hypothesis is true.
#You can recognize a null hypothesis because it sounds like:
# "there was no difference"
#for instance:
#"The intervention did not affect blood pressure."

#one tailed test   
#rare in health sciences, more common in industrial process control
x=seq(50,140,length=200)
y1=dnorm(x,80, 10)
plot(x,y1,type='l',lwd=2,col='red')
y2=dnorm(x,110, 10)
lines(x,y2,type='l',lwd=2,col='blue')
abline(v=qnorm(0.95,80,10)) 

#two tailed test
#common in the health sciences, because in most cases, both
#an increase or a decrease in a variable would affect health
x=seq(50,140,length=200)
y1=dnorm(x,80, 10)
plot(x,y1,type='l',lwd=2,col='red')
y2=dnorm(x,110, 10)
lines(x,y2,type='l',lwd=2,col='blue')
abline(v=qnorm(0.025,80,10))
abline(v=qnorm(0.975,80,10))

#colour the rejection area, also referred to as alpha.
#If the p-value is equal to or lower than alpha - reject the null hypothesis
x=seq(50,140,length=200)
y1=dnorm(x,80, 10)
plot(x,y1,type='l',lwd=2,col='red')
y2=dnorm(x,110, 10)
lines(x,y2,type='l',lwd=2,col='blue')
cord.x1 <- c((round(qnorm(0.975, 80, 10))),seq((round(qnorm(0.975, 80, 10))), 120,1),120) 
cord.y1 <- c(0,dnorm(seq((round(qnorm(0.975, 80, 10))), 120, 1), 80, 10),0) 
polygon(cord.x1,cord.y1,col='red')
cord.x2 <- c(50,seq(50,round(qnorm(0.025, 80, 10),1)),round(qnorm(0.025, 80, 10))) 
cord.y2 <- c(0,dnorm(seq(50,round(qnorm(0.025, 80, 10),1)), 80, 10),0) 
polygon(cord.x2,cord.y2,col='red')

#Imagine that the alternative hypothesis were true. 
#Beta is the risk that you will keep (=not reject) the false null hypothesis
x=seq(50,140,length=200)
y1=dnorm(x,80, 10)
plot(x,y1,type='l',lwd=2,col='red')
y2=dnorm(x,110, 10)
lines(x,y2,type='l',lwd=2,col='blue')
cord.x2<- c(0,seq((round(1-qnorm(0.025,110,10))),100,1),100)
cord.y2 <- c(0,dnorm(seq((round(1-qnorm(0.025, 110, 10))), 100, 1), 110, 10),0) 
polygon(cord.x2,cord.y2,col='red')
abline(v=round(qnorm(0.975, 80, 10, lower.tail=T)))
abline(v=round(qnorm(0.025, 80, 10, lower.tail=T)))
text(95,0.005, "ß ",xpd=5)

#Statistical power, 1-beta, is the probability to reject a false null hypothesis
x<- seq(50,140,length=200)
y1<- dnorm(x,80, 10)
plot(x,y1,type='l',lwd=2,col='red')
y2<- dnorm(x,110, 10)
lines(x,y2,type='l',lwd=2,col='blue')
cord.x2<- c(0,seq((round(1-qnorm(0.025,110,10))),100,1),100)
cord.y2 <- c(0,dnorm(seq((round(1-qnorm(0.025, 110, 10))), 100, 1), 110, 10),0) 
polygon(cord.x2,cord.y2,col='red')
abline(v=round(qnorm(0.975, 80, 10, lower.tail=T)))
abline(v=round(qnorm(0.025, 80, 10, lower.tail=T)))
cord.x1 <- c(100,seq(round(qnorm(0.975, 80, 10, lower.tail=T)), 140,1),140) 
cord.y1 <- c(0,dnorm(seq(round(qnorm(0.975, 80, 10, lower.tail=T)),140, 1), 110, 10),0) 
polygon(cord.x1,cord.y1,col='6')
text(95,0.005, "ß ",xpd=5)
text(115,0.005, "1-ß ",xpd=5)


