set.seed(100)
pla_h<-function(X,w)
{
  scalar_prod<-as.matrix(cbind(1,X$X1,X$X2))%*%as.matrix(w) 
  return(as.vector(sign(scalar_prod)))
}

f<-function(X)
{
  return(pla_h(X,c(w0,w1,w2)))
}

w0<-runif(1,min=-999,max=999)
w1<-runif(1,min=-999,max=999)
w2<-runif(1,min=-999,max=999)

D<-data.frame(X1=runif(10,min=-1,max=1),X2=runif(10,min=-1,max=1))
D<-cbind(D,y=f(D))

library(ggplot2)
p<-ggplot(D,aes(x=X1,y=X2,col=as.factor(y+3)))+geom_point()+theme(legend.position="none")
p_f<-p+geom_abline(slope=-w1/w2,intercept=-w0/w2,colour="red")
p_f

iter=1
w=c(0,0,0)
while(iter<15)
{
y_pred=pla_h(D[,1:2],w)
D_mis<-subset(D,y!=y_pred)
if(nrow(D_mis)==0){
  break
  }
#####Choose one misclassified point
else{  
x_t=D_mis[1,]
aaa<-c(1,x_t$X1,x_t$X2)
w=w+(aaa*x_t$y)
print(iter)
iter=iter+1
  }  
}  
w=t(t(w))
p_g<-p_f+geom_abline(slope=-w[2,]/w[3,],intercept=-w[1,]/w[3,],color="blue")
p_g

