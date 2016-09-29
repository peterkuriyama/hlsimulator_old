

require("gregmisc")

k=5
po=0.01
n=100 

probx <- function(po,n,k){
  fo = 1-po
  f = po/5;
  px = rep(NA,k+1)
  px[1] = fo**n
  px[2] = 5*((f+fo)**n - px[1])
  px[3] = 10*((2*f+fo)**n - 2*((f+fo)**n) + px[1])
  px[4] = 10*((3*f+fo)**n - 3*((2*f+fo)**n) + 3*((f+fo)**n) - px[1])
  px[5] = 5*((4*f+fo)**n - 4*((3*f+fo)**n) + 6*((2*f+fo)**n) - 4*((f+fo)**n) + px[1])
  px[6] = 1 - sum(px[1:5])
  return(px)
}

px = probx(po,n,k);

x = 0:k
Ex = sum(x*px)
Ex2 = sum(x*x*px)
Vx = Ex2 - Ex**2
print(Ex)
print(Vx)


pv = c(0.001,0.1,0.5)
Nv = 2*c(5000,50,10)
np=length(Nv)
npop = 100

catch = matrix(NA,nrow=npop,ncol=3)
mu = catch
k=5
x = 0:k

for(j in 1:3){
nv = round(seq(1,Nv[j],length=npop))
for(i in 1:npop){
  catch[i,j] = sum(x*probx(pv[j],nv[i],k))
}
mu[,j] = pv[j]*nv
}


xt = range(mu)
yt = range(catch)

plot(xt,xt,type='n',xlab='Potential Exploitable Stock Size',ylab='Expected Catch',las=1)

for(j in 1:3){
lines(mu[,j],catch[,j],col=j,lty=1,lwd=2)
}
abline(a=0,b=1,lty=2)
abline(h=1,col='grey')
abline(h=2,col='grey')
legend('topleft',lty=1,col=1:3,legend=pv,bty='n',lwd=2)
#legend('topleft',lty=1,col=2:3,legend=pv[2:3],bty='n',lwd=2)

#### with unequal hook probabilities ####;

Sx = function(k){  
  Sx = rep(0,k);
  for(i in 1:k){

    ind = combinations(k,i)
    nind = nrow(ind)
    Sxi = matrix(0,nrow=nind,k)
    for(j in 1:nind){
      Sxi[j,ind[j,]]=1
    }
    Sx = rbind(Sx,Sxi)
  }
  return(Sx)
}  

Sxi = function(k,i){  
  ind = combinations(k,i)
  nind = nrow(ind)
  Sxi = matrix(0,nrow=nind,k)
  for(j in 1:nind){
    Sxi[j,ind[j,]]=1
  }
  
  return(Sxi)
}

prob1Lx <- function(po,n,n.hook,gm){
  k = n.hook
  alpha = (1-gm)/(gm*(1-gm**n.hook))
  hook.prob = alpha*(gm**(1:n.hook))
  fi = po*hook.prob 
  fo = 1-po
  
  gx = rep(NA,k+1) 
  gx[1]=0
  for(i in 1:k){
    gx[i+1] = sum((Sxi(k,i)%*%fi + fo)**n)
  }
  
  px = rep(NA,k+1)
  px[1] = fo**n
  
  for(i in 1:k){
    px[i+1] = gx[i+1];
   signi = (-1)**i
    if(i>1){
    for(j in 1:(i-1)){
      signj = ((-1)**j)*signi
      px[i+1] = px[i+1] + signj*choose(i,j)*choose(k,i)*gx[j+1]/choose(k,j);
   }}   
   px[i+1] = px[i+1] + signi*choose(k,i)*px[1]
   }
#px=px/sum(px)

return(px)
}


n.hook=5   
gm=0.75
po=0.1
n=50

px = prob1Lx(po,n,k,gm)
x = 0:k
Ex = sum(x*px)
Ex2 = sum(x*x*px)
Vx = Ex2 - Ex**2
print(Ex)
print(Vx)
mu = n*po
print(mu)


catch1 = matrix(NA,nrow=npop,ncol=3)

for(j in 1:3){
nv = round(seq(1,Nv[j],length=npop))
for(i in 1:npop){
  catch1[i,j] = sum(x*prob1Lx(pv[j],nv[i],k,gm))
}
}


xt = range(mu)
yt = range(catch)

plot(xt,c(0,5),type='n',xlab='Potential Exploitable Stock Size',ylab='Expected Catch',las=1)

for(j in 2:3){
lines(mu[,j],catch[,j],col=j,lty=1,lwd=2)
lines(mu[,j],catch1[,j],col=j,lty=2,lwd=2)
}
abline(a=0,b=1,lty=2)
#abline(h=1,col='grey')
abline(h=3,col='grey')
legend('topleft',lty=1,col=2:3,legend=pv[2:3],bty='n',lwd=2)


######## General code ##############

prob3Lx <- function(po,n,n.hook,n.angler,gm){
  k = n.hook*n.angler
  if(gm<1){
    alpha = (1-gm)/(gm*(1-gm**n.hook))
    hook.prob = alpha*(gm**(1:n.hook))
  }
  if(gm==1){hook.prob = rep(1,n.hook)/n.hook}

  fi = po*rep(angler.prob,each=n.hook)*rep(hook.prob,times=n.angler) 
  fo = 1-po
  
  gx = rep(NA,k+1) 
  gx[1]=0
  for(i in 1:k){
    gx[i+1] = sum((Sxi(k,i)%*%fi + fo)**n)
  }
  
  px = rep(NA,k+1)
  px[1] = fo**n
  
  for(i in 1:k){
    px[i+1] = gx[i+1];
   signi = (-1)**i
    if(i>1){
    for(j in 1:(i-1)){
      signj = ((-1)**j)*signi
      px[i+1] = px[i+1] + signj*choose(i,j)*choose(k,i)*gx[j+1]/choose(k,j);
   }}   
   px[i+1] = px[i+1] + signi*choose(k,i)*px[1]
   }
#px=px/sum(px)

return(px)
}

n.angler=3
n.hook=5
k = n.angler*n.hook
x = 0:k
angler.prob = c(0.4,0.3,0.3)

gm = 0.75
po=0.0998
n=123

#n=50000
px = prob3Lx(po,n,n.hook,n.angler,gm);
Ex = sum(x*px)
Ex2 = sum(x*x*px)
Vx = Ex2 - Ex**2
print(Ex)

print(Vx)
ltxt = paste('mean = ',round(Ex,digits=2),'
Var = ',round(Vx,digits=2),sep='')

barplot(px,names=x,xlab='Number caught per drop',ylab='Probability')
legend('topright',ltxt,bty='n')

pv = c(0.001,0.1,0.5)
Nv = 4*c(5000,50,10)
np=length(nv)
npop = 300

angler.prob = c(0.4,0.3,0.3)

catch = matrix(NA,nrow=npop,ncol=3)
mu = catch

for(j in 1:3){
nv = round(seq(1,Nv[j],length=npop))
for(i in 1:npop){
  catch[i,j] = sum(x*prob3Lx(pv[j],nv[i],n.hook,n.angler,gm))
}
mu[,j] = pv[j]*nv
}

xt = range(mu)
yt = range(catch)

plot(xt,c(0,15),type='n',xlab='Potential Exploitable Stock Size',ylab='Expected Catch',las=1)

for(j in 1:3){
lines(mu[,j],catch[,j],col=j,lty=1,lwd=2)
}
abline(a=0,b=1,lty=2)
abline(h=5,col='grey')
abline(h=10,col='grey')
legend('topleft',lty=1,col=1:3,legend=pv,bty='n',lwd=2) 


del.catch = catch[2:(npop-1),1] - catch[1:(npop-2),1]
del.catch = del.catch/(mu[2:(npop-1),1] - mu[1:(npop-2),1])
tx = mu[2:(npop-1),1]
plot(tx,del.catch,type='l',xlab='Potential Exploitable Stock Size',
ylab='Expected Catch Derivative',las=1,lwd=2)
abline(h=.5,col='grey')

tx = catch[1:(npop-2),1]
plot(tx,del.catch,type='l',xlab='Expected Catch',
ylab='Expected Catch Derivative',las=1,lwd=2)
abline(h=.5,col='grey')

parm = lsfit(tx,del.catch)

theta1 = -parm$coefficients[1]/parm$coefficients[2]
theta2 = parm$coefficients[2]
muv =  seq(xt[1],xt[2],length=500)
ECpred1 = theta1*(1-exp(theta2*muv))              theta2

b = mean((k - tx)*del.catch)/mean((k - tx)**2) 

theta1=15
theta2 = b 
ECpred2 = theta1*(1-exp(-theta2*muv))

plot(xt,c(0,15),type='n',xlab='Potential Exploitable Stock Size',ylab='Expected Catch',las=1)

for(j in 1:3){
lines(mu[,j],catch[,j],col=j,lty=1,lwd=2)
}

lines(muv,ECpred1,col=4,lty=1,lwd=2)
abline(a=0,b=1,lty=2)
ltext = c(as.character(pv),"VonB")
legend('topleft',lty=1,col=1:4,legend=ltext,bty='n',lwd=2)

yt = c(min(ECpred1,ECpred2),max(ECpred1,ECpred2))
plot(xt,yt,type='n',xlab='Potential Exploitable Stock Size',ylab='Expected Catch',las=1)
lines(muv,ECpred1,col=4,lty=1,lwd=2)   
lines(muv,ECpred2,col=5,lty=1,lwd=2)

theta1 = -parm$coefficients[1]/parm$coefficients[2]
ltext = c(paste('Linf = ',round(theta1,digits=1),sep=''),'Linf = 15') 
legend('topleft',lty=1,col=4:5,legend=ltext,bty='n',lwd=2)
