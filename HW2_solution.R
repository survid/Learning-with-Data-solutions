library(dplyr)
v1<-vector(length=100000)
vrand<-vector(length=100000)
vmin<-vector(length=100000)

for (i in 1:100000){
outcomes<-c("H","T")
full_set<-rep(outcomes,1000)
out<-sample(outcomes,size=10000,replace=T)
result_matrix<-data.frame(matrix(out,nrow=1000,ncol=10,byrow=T))
table(result_matrix)
result_matrix[,ncol(result_matrix)+1]<-rowSums(result_matrix=="H")
result_matrix[,ncol(result_matrix)+1]<-rowSums(result_matrix=="T")
colnames(result_matrix)[11:12]<-c("Count_heads","Count_tails")


c1<-result_matrix[1,]
crand<-sample(1:nrow(result_matrix),size=1,replace=F)
crand<-result_matrix[crand,]
cmin<-result_matrix[which(result_matrix$Count_heads==min(result_matrix$Count_heads))[1],]

v1[i]<-c1$Count_heads/(c1$Count_heads+c1$Count_tails)
vrand[i]<-crand$Count_heads/(crand$Count_heads+crand$Count_tails)
vmin[i]<-cmin$Count_heads/(cmin$Count_heads+cmin$Count_tails)

}
##1-b
##2-d

##########Linear regression##Q5-7######################
#########Estimation using linear regression
lin_reg<-function(x,y){
  ##Add bias
  x<-cbind(bias=rep(1,nrow(x)),x)
  ####Pseudo Inverse
  pseudo=solve(as.matrix(t(x))%*%as.matrix(x))%*%as.matrix(t(x))
  w<-pseudo %*% y
  error<-sum(y !=sign(t(w)%*%t(x)))
  list(w=w,error=error)
}
lin_reg_nobias<-function(x,y){
  ##Add bias
  #x<-cbind(bias=rep(1,nrow(x)),x)
  ####Pseudo Inverse
  pseudo=solve(as.matrix(t(x))%*%as.matrix(x))%*%as.matrix(t(x))
  w<-pseudo %*% y
  error<-sum(y !=sign(t(w)%*%t(x)))
  list(w=w,error=error)
}  
######Which side of the line the point lies#####

chooseSide <- function(f, x) {
  m <- matrix(c(f[2,1] - f[1,1], x[1] - f[1,1], 
                f[2,2] - f[1,2], x[2] - f[1,2]), 
              byrow=TRUE, nrow=2)
  sign(det(m))
}

#################Generate the target function#######
#wts<-list()
err<-vector()
for(i in 1:1000)
{
x<-data.frame(X1=runif(100,min=-1,max=1),X2=runif(100,min=-1,max=1))
f <- matrix(c(runif(2, min = -1, max = 1), -1, 1),
            nrow = 2)
w1=(f[2,2]-f[1,2])/(f[2,1]-f[1,1])
w0=f[2,2]-w1*f[2,1]
y <- apply(x, 1, function(z) chooseSide(f, z))

# library(ggplot2)
# p<-ggplot(D,aes(x=X1,y=X2))+geom_point()+theme(legend.position="none")
# p_f<-p+geom_abline(slope=w1,intercept=w0,colour="red")
# p_f

err[i]<-(lin_reg(x,y)$error)/100
w<-(lin_reg(x,y))$w
}

err_out<-vector()
for(i in 1:1000)
{
x_out<-data.frame(X1=runif(1000,min=-1,max=1),X2=runif(1000,min=-1,max=1))
y_out<-apply(x_out, 1, function(z) chooseSide(f, z))
x_out1 <- cbind(bias = rep(1, nrow(x_out)), x_out)
# library(ggplot2)
# p<-ggplot(x,aes(x=X1,y=X2))+geom_point()+theme(legend.position="none")
# p_f<-p+geom_abline(slope=wt[2],intercept=wt[1],colour="red")
# p_f
err_out[i]<-(sum(y_out !=sign(t(w)%*%t(x_out1))))
}
################################PLA#####################
x_pla<-data.frame(X1=runif(10,min=-1,max=1),X2=runif(10,min=-1,max=1))
pla_h<-function(X,w)
{
  scalar_prod<-as.matrix(cbind(1,X$X1,X$X2))%*%as.matrix(w) 
  return(as.vector(sign(scalar_prod)))
}

f <- matrix(c(runif(2, min = -1, max = 1), -1, 1),nrow = 2)
w1=(f[2,2]-f[1,2])/(f[2,1]-f[1,1])
w0=f[2,2]-w1*f[2,1]
y_pla <- apply(x_pla, 1, function(z) chooseSide(f, z))
iter=1
wt<-list()
while(iter<1000)
{
  y_pred=pla_h(x_pla,w)
  D_mis<-subset(x_pla,y_pla!=y_pred)
  if(nrow(D_mis)==0){
    break
  }
  #####Choose one misclassified point
  else{  
    x_t=D_mis[1,]
    aaa<-c(1,x_t$X1,x_t$X2)
    w=w+(aaa*x_t$y)
    print(iter)
    #print(i)
    iter=iter+1
  }
  wt[[i]]=w
}  
w=t(t(w))

p_g<-p_f+geom_abline(slope=-w[2,]/w[3,],intercept=-w[1,]/w[3,],color="blue")
p_g


################################Non-linear transformation################
err_nonliner<-vector()

for (i in 1:1000)
{
xD<-data.frame(X1=runif(1000,min=-1,max=1),X2=runif(1000,min=-1,max=1))
f<-function(x){
  sign(x[,1]^2+x[,2]^2-0.6)
}
y<-f(xD)
sample_noise<-sample(1000,size=(0.1*1000))
y[sample_noise]=-y[sample_noise]
err_nonliner[i]<-(lin_reg(xD,y)$error)
wt<-(lin_reg(xD,y))$w
}

Eout<-vector()
for (i in 1:1000)
{
f<-function(x){
  sign(x[,1]^2+x[,2]^2-0.6)
}
x <- matrix(runif(2 * 1000, min=-1, max=1), ncol=2)
y<- f(x)

###Add Noise##
sampleidX<-sample(1000,100)
y[sampleidX]<--y[sampleidX]

fit_lin<-lin_reg(x,y)

###Transformation###
transformation<-function(X){
  cbind(1,X[,1],X[,2],X[,1]*X[,2],X[,1]^2,X[,2]^2)
}

###OOS
x_out<-matrix(runif(2 * 1000, min=-1, max=1), ncol=2)
y_out<-f(x)
x_out_transformed<-transformation(x_out)
fit_transform<-lin_reg_nobias(x_out_transformed,y)

###Add Noise##
sampleidX<-sample(1000,100)
y_out[sampleidX]<--y_out[sampleidX]

fit_lin<-lin_reg(x,y)

##########OOS error##
Eout <- sum(y_out != sign(t(fit_transform$w) %*% t(x_out_transformed)))
}