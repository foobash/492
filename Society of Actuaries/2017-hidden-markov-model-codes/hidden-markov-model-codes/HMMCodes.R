#1. List of main HMM programs
#**********Program using AIC to choose number of states for the HMM multiple observations************************
# sources: 1. Ghahramani, Z. (2001). An Introduction to Hidden Markov Models and Bayesian Networks. 
#             International Journal of Pattern and Recognition and Artificial Intelligent, 15(1), 9-42.
#          2. Li, X. P., Parizeau, M., & Palmondon, R. (2000). Training Hidden Markov Models with Multiple Observations-A Combinatorial Method.
#             IEEE Trans. PAMI, 371–377, 22.
#          3. Harte, D. (2016). HiddenMarkov: Hidden Markov Models. 
#             ftp://ftp.gns.cri.nz/pub/davidh/sslib/r-repo/(R package version 1.8-7). Wellington.

# Input:* Stock prices: open, close, high, low
#       * Number of States, N
#       * Window length: (WD)is the time period that you use to calibrate model parameters for different numbers of states.
#         This program will use this WD time to calibrate HMM parameters, then update the window by moving forward to 
#         make an other calibration. The AIC of the WD calibrations is plotted. The code uses numbers of states from two to four. 
# Out put: A graph of AIC for HMM with different numbers of states. The model with the lowest AIC is the best.
# Author: Nguyet Nguyen
# Date modified: 02/15/2017
#************************************************************************************************************
source("C:/Users/Nguyet/BW.NdataNew.R")
source("C:/Users/Nguyet/ESTEP.N.DATA.R")
source("C:/Users/Nguyet/forwardback.R")
source("C:/Users/Nguyet/Estep1.R")
source("C:/Users/Nguyet/hmm.iniNdata.R")
source("C:/Users/Nguyet/forward.R")
source("C:/Users/Nguyet/argmin.R")

#Call observation data y
        
       Sp=read.table("C:/Users/Nguyet/Data/Ishares/MBB-D.txt",header=TRUE,sep="|",as.is=TRUE)
	 T1=length(Sp$Date)
	 K=4 #number of observation data                  
	 y=array(NA,dim=c(K,T1,1))
	 y[1,,]=as.numeric(Sp$Close)
	 y[2,,]=as.numeric(Sp$Open)
	 y[3,,]=as.numeric(Sp$Low)
	 y[4,,]=as.numeric(Sp$High)
       	
       #Set up training window	
	 WD=100
	 T2=T1-WD
       AIC=array(NA,dim=c(3,WD));
       for (N in 2:4){
	    for (l in 1:WD){
		x=array(NA,dim=c(K,WD,1))		
		# Training data of length WD	
		x[1,,]=y[1,,][(T2-WD+l):(T2+l-1)]
		x[2,,]=y[2,,][(T2-WD+l):(T2+l-1)]
		x[3,,]=y[3,,][(T2-WD+l):(T2+l-1)]
		x[4,,]=y[4,,][(T2-WD+l):(T2+l-1)]		
		#first step using initial parameters		
		if (l==1){
			Par=hmm.iniNdata(x,N)
			mean=Par$mean
			sigma=Par$sigma
			A=Par$A
			p=Par$p
			k=BW.NdataNew(x,A,p,mean,sigma,T,K,N)
		}
		
		# after first step using HMM parameters from previous step
		
		else{
		    k=BW.NdataNew(x,k$A,k$p,k$mean,k$sigma,T,K,N)
		}
            # calculate AIC
            b=N*N+2*N-1;
            AIC[(N-1),l]=-2*k$LL+2*b
	  	}
	}  
      y=cbind(AIC[1,],AIC[2,],AIC[3,])
      matplot(y, type = "l",col = c("red", "blue","black"),lwd=2,xlab="Simulations", ylab="AIC",main="FB")
      legend("topright", legend = c("2-states","3-states","4-states"),col=c("red", "blue","black"),lwd=2, bty="n"

#**********Program of HMM multiple observations to predict probabilities of being in a recession state*****************
# Sources: 1. Ghahramani, Z. (2001). An Introduction to Hidden Markov Models and Bayesian Networks. 
#             International Journal of Pattern and Recognition and Artificial Intelligent, 15(1), 9-42.
#          2. Li, X. P., Parizeau, M., & Palmondon, R. (2000). Training Hidden Markov Models with Multiple Observations-A Combinatorial Method.
#             IEEE Trans. PAMI, 371–377, 22.
#          3. Harte, D. (2016). HiddenMarkov: Hidden Markov Models. 
#             ftp://ftp.gns.cri.nz/pub/davidh/sslib/r-repo/(R package version 1.8-7).
# Input:* Observation data: assume that these data are independent,
#       * Number of state: N
#       * Window length: (WD)is the time period that you want to predict. 
# Note:  This program will use this WD time to set up the train data for HMM parameters, then
#        predict the probability of being in the recession state using the most current smoothed probability
#        After each prediction, traning data is updated and repeat the process to
#        make the next prediction.
#       * If the data have wide range, you should normalize them before using to have a better result.
# Out put: Predicted probabilities of being in each states of obsevations for WD period.
# Author: Nguyet Nguyen
# Date: 02/15/2017
#*********************************************************************************************************************
source("C:/Users/Nguyet/BW.NdataNew.R")
source("C:/Users/Nguyet/ESTEP.N.DATA.R")
source("C:/Users/Nguyet/forwardback.R")
source("C:/Users/Nguyet/Estep1.R")
source("C:/Users/Nguyet/hmm.iniNdata.R")
source("C:/Users/Nguyet/forward.R")
source("C:/Users/Nguyet/argmin.R")
source("C:/Users/Nguyet/NL.R")
#Call observation data y      
   DJ=read.table("C:/Users/Nguyet/Data/Ishares/MBB-D.txt",header=TRUE,sep="|",as.is=TRUE)
	 WD=252;	
       D=length(DJ$Date)-WD;	
	 K=4
	 N=2	
	 Pstate2=rep(0,WD)
	 for (i in 1:WD){
		y1=as.numeric(DJ$Close[(D-WD+i+1):(D+i)])
		y2=as.numeric(DJ$Open[(D-WD+i):(D+i-1)]);
            y3=as.numeric(DJ$Low[(D-WD+i+1):(D+i)])
		y4=as.numeric(DJ$High[(D-WD+i):(D+i-1)]);
		T=WD
		y=array(NA,dim=c(K,T,1))
		y[1,,]=y1
		y[2,,]=y2
		y[3,,]=y3
		y[4,,]=y4
		if (i==1){
		    Par=hmm.iniNdata(y,N)
	            k=BW.NdataNew(y,Par$A,Par$p,Par$mean,Par$sigma,T,K,N)
		}
		else {
		
		     k=BW.NdataNew(y,k$A,k$p,k$mean,k$sigma,T,K,N)
		}
		
	        TT=(k$mean[1]/k$sigma[1] < k$mean[2]/k$sigma[2])
	
	if (TT) {
	Pstate2[i]=k$u[T,1] 
	        } 
		else {
	Pstate2[i]=k$u[T,2]  
	        }
	 }
	print("Begin date")
	print(DJ$Date[(D+2)])
	print("End date")
	print(DJ$Date[D+WD])
      y11=DJ$Close[(D+2):(D+WD)];
      time <- as.Date(DJ$Date[(D+2):(D+WD)], "%m/%d/%Y");
      axis(1, time , format( time , "%b %y "), cex.axis = .3)  
      par(mfrow=c(2,1));
      plot(time, y11,type="l", col="blue", main="", xlab="",
	ylab="Prices")
      plot(time,Pstate2[1:(WD-1)], type="l",col="red", xlab="",
	ylab="Prob. regime 2")

#**********Program of using HMM multiple observations to predict stock prices*********************************************
# Sources: 1. Ghahramani, Z. (2001). An Introduction to Hidden Markov Models and Bayesian Networks. 
#             International Journal of Pattern and Recognition and Artificial Intelligent, 15(1), 9-42.
#          2. Li, X. P., Parizeau, M., & Palmondon, R. (2000). Training Hidden Markov Models with Multiple Observations-A Combinatorial Method.
#             IEEE Trans. PAMI, 371–377, 22.
#          3. Harte, D. (2016). HiddenMarkov: Hidden Markov Models. 
#             ftp://ftp.gns.cri.nz/pub/davidh/sslib/r-repo/(R package version 1.8-7).
# Input:* Observation data: assume that these data are independent,
#       * Number of state: N
#       * Window length: (WD)is the time period that you want to predict. 
# Note:  This program will use this WD time to set up the train data for HMM parameters, then
#        predict price of a stock or ETF. After each prediction, traning data is updated and repeat the process to
#        make the next prediction.
# Out put: Predicted prices of obsevation data for WD period and error of the prediction.
# Author: Nguyet Nguyen
# Date: 02/15/2017
#************************************************************************************************************
source("C:/Users/ntnguyen01/BW.NdataNew.R")
source("C:/Users/ntnguyen01/ESTEP.N.DATA.R")
source("C:/Users/ntnguyen01/forwardback.R")
source("C:/Users/ntnguyen01/Estep1.R")
source("C:/Users/ntnguyen01/hmm.iniNdata.R")
source("C:/Users/ntnguyen01/forward.R")
source("C:/Users/ntnguyen01/argmin.R")

#call observation data y
       Sp=read.table("C:/Users/ntnguyen01/Data/Barclays/MBG-W.txt",header=TRUE,sep="|",as.is=TRUE)
	 T1=length(Sp$Date)
	 K=4   #number of observation data
	 N=4 #number of states                  
	 y=array(NA,dim=c(K,T1,1))
	 y[1,,]=as.numeric(Sp$Close)
	 y[2,,]=as.numeric(Sp$Open)
	 y[3,,]=as.numeric(Sp$Low)
	 y[4,,]=as.numeric(Sp$High)	
      #Set up window
	 WD=52
	 T2=T1-WD
	 T=WD
	price=as.double(rep(0,WD))
	for (l in 1:WD){
		x=array(NA,dim=c(K,WD,1))
		
		# Training data
		
		x[1,,]=y[1,,][(T2-WD+l+1):(T2+l)]
		x[2,,]=y[2,,][(T2-WD+l+1):(T2+l)]
		x[3,,]=y[3,,][(T2-WD+l+1):(T2+l)]
		x[4,,]=y[4,,][(T2-WD+l+1):(T2+l)]
		
		#first step using initial parameters
		
		if (l==1){
			Par=hmm.iniNdata(x,N)
			mean=Par$mean
			sigma=Par$sigma
			A=Par$A
			p=Par$p
			k=BW.NdataNew(x,A,p,mean,sigma,T,K,N)
		}
		
		# after first step using HMM parameters from previous step
		
		else{
		    k=BW.NdataNew(x,k$A,k$p,k$mean,k$sigma,T,K,N)
		}
		
		# finding the most similar (log likelihood) date in the past
		
		ll=as.double(rep(0,WD))
	
		for (i in 1:(WD)){
	
			for (j in 1:K){
			z=y[j,,][(T2-WD-i+l+1):(T2-i+l)]
				f=forward(z,k$A,k$p,k$mean,k$sigma)
				ll[i]=ll[i]+f
		        }
		
	        }
		 point=argmin(abs(ll-k$LL))
             price[l]=y[1,,][(T2+l)]+(y[1,,][(T2-point+l+1)]-y[1,,][(T2-point+l)]);
	 }  
	 #Print results  
	 print("Begin date")
	 print(Sp$Date[(T2+2)])
	 print("End date")
	 print(Sp$Date[(T1)])
       #calculate error 
	 RER=mean(abs(pricenew-true.price)/true.price)
	 print("Error is")
	 print(RER)	
	 #Plot results 	
	 g_range=range(min(true.price),true.price,pricenew)	
	 T3=c(1:(WD-1))  
       time <- as.Date(Sp$Date[(T1-WD+2):T1], "%m/%d/%Y");
       axis(1, time , format( time , "%b %d "), cex.axis = .3)    
       plot(time,true.price,type="o", cex=.5,col="blue", xlab="",
	 ylab="Close Price",ylim=g_range);
       parnew=TRUE;
       lines(time, pricenew, type="o",pch=22, cex=.5,col="red",lty=2)
	 legend(x="topleft",g_range[2],c("True price","Estimated price"),col=c("blue","red"),pch=21:22,lty=1:2, bty = "n")

  #2. List of functions used in HMM
#function to calibrate parameters for HMM multiple observations using Baum Welch algorithm
BW.NdataNew=function(y,A,p,mean,sigma,T,K,N){
maxiter=500;
tol=1e-6;
oldLL=-100;
diff=100;
iter=0;
while ((iter < maxiter) && (diff > tol))
{
       Var=ESTEP.N.DATA(y,A,p,mean,sigma,T,K,N)
       diff=Var$LL1-oldLL
       A=Var$S
       p=Var$p
       mean=Var$mean
       sigma=Var$sigma
       oldLL=Var$LL1
       u=Var$u1
       iter = iter + 1;
}
object=list(u=u,A=A,p=p,mean=mean,sigma=sigma,LL=Var$LL1,diff=diff,iter=iter)
return(object)
}
##function calculate parameters for hmm (multiple observations)
#y is array of observation vectors 
#T is length of each observation data
#K is number of observation data
#N is number of states
ESTEP.N.DATA=function(y,A,p,mean,sigma,T,K,N){
   m=nrow(A)
   S1=matrix(as.double(0),nrow=m,ncol=m)
   u1=matrix(as.double(0),nrow=T,ncol=m)
   LL1=0
mean.new=matrix(as.double(0),nrow=1, ncol=m)
sigma.new=matrix(as.double(0),nrow=1, ncol=m)
   S2=matrix(as.double(0),nrow=m,ncol=m)

for (k in 1:K){
            x=y[k,,]
            UV=Estep.Ndata(x,A,p,mean,sigma,N)
	    u1=u1+UV$u
	    S1=S1+UV$S
	    LL1=LL1+UV$LL
	    S2=S2+diag(1/(apply(UV$v,MARGIN=2,FUN=sum)))
	mean.new=mean.new+as.numeric(matrix(x,nrow=1)%*%UV$u)
	sigma.new=sigma.new+apply((matrix(x,nrow=T,ncol=m)
	              -matrix(mean,nrow=T,ncol=m,byrow=TRUE))^2*UV$u,
                      MARGIN=2,FUN=sum)

   }

   S=S2%*%S1
   S=S/apply(S,1,sum)
mean=mean.new/apply(u1,2,sum)
sigma=sqrt(sigma.new/apply(u1,2,sum))
   u1=u1/K
   p=u1[1,]
return(list(mean=mean,sigma=sigma,LL1=LL1,S=S,p=p,u1=u1))
}
##function calculate loglikelihood of HMM using forward-backward probabilities 
#x;observation data, A:transition matrix, p: initial probability,
#mu, sigma : mean and variance of Normal distribution
forwardback=function(x,A,p,mu,sigma,N) 
{

   m=nrow(A)
   n=length(x)
prob=matrix(as.double(0), nrow=n, ncol=m)
for (k in 1:m)
prob[,k]=dnorm(x,mu[k],sigma[k],log=FALSE)

   #calculate log of forward probabilities, logalpha

    phi=as.double(p)

    logalpha=matrix(as.double(rep(0,m*n)),nrow=n)
    lscale=as.double(0)
	for (i in 1:n){
		if (i>1)
		phi=phi%*%A  
		phi=phi*prob[i,]
		sumphi=sum(phi)
		phi=phi/sumphi
		lscale=lscale+log(sumphi)
		logalpha[i,]=log(phi)+lscale
   	}
   LL=lscale

   #calculate log of backward probabilities, logbeta

logbeta=matrix(as.double(rep(0,m*n)),nrow=n)
phi=as.double(rep(1/m,m))
lscale=as.double(log(m))
for (i in seq(n-1,1,-1)){
phi=A%*%(prob[i+1,]*phi)
logbeta[i,]=log(phi)+lscale
sumphi=sum(phi)
phi=phi/sumphi
lscale=lscale+log(sumphi)
   }
return(list(logalpha=logalpha, logbeta=logbeta, LL=LL))
}
#function calculate the conditional expectations, and log likelihood of HMM
#x;observation vector, A:transition matrix, p: initial probability,
#mu, sigma : mean and variance of Normal distribution
Estep1=function(x,A,p,mean,sigma) 
{
   m=nrow(A)
   n=length(x)
   h=forwardback(x,A,p,mean,sigma)
logbeta=h$logbeta
logalpha=h$logalpha
   LL=h$LL
forward=exp(logalpha)
backward=exp(logbeta)
   u=exp(logalpha+logbeta-LL)
   v=array(NA,dim=c(n-1,m,m))
for (k in 1:m){
logprob=dnorm(x=x[-1],mean[k],sigma[k],log=TRUE)
logA=matrix(log(A[,k]), byrow=TRUE,nrow=n-1,ncol=m)
logPbeta=matrix(logprob+logbeta[-1,k], byrow=FALSE, nrow=n-1, ncol=m)
v[,,k]=logA+logalpha[-n,]+logPbeta-LL
      }
   v=exp(v)


return(list(u=u,v=v,LL=LL))
 }   
#function initial parameters for HMM, y is obervation data, N is number of states
hmm.iniNdata=function(y,N){
	m=mean(y)
	s=sd(y)
      b=N-1
	mean=rep(m,N)+rnorm(N)*s
	p=c(1,rep(0,b))
      sigma=rep(s,N)
	A=matrix(as.double(1/N),N,N)
	IV=c(list(A=A,p=p,mean=mean,sigma=sigma))
	return(IV)
}
#function calculate loglikelihood of HMM using forward algorithm
#x;observation data, A:transition matrix, p: initial probability
#mu, sigma : mean and variance of Normal distribution
forward=function(x,A,p,mu,sigma) 
{
   m=nrow(A)
   n=length(x)
prob=matrix(as.double(0), nrow=n, ncol=m)
for (k in 1:m)
prob[,k]=dnorm(x,mu[k],sigma[k],log=FALSE)

   #calculate log of forward probabilities, logalpha

phi=as.double(p)
lscale=as.double(0)
for (i in 1:n){
if (i>1)
phi=phi%*%A  
phi=phi*prob[i,]
sumphi=sum(phi)
phi=phi/sumphi
lscale=lscale+log(sumphi)

   }
   LL=lscale
return(LL)
}

#function calculate the argmin of vector v with length(v)=m
argmin=function(v){
     m=length(v)
for (i in 1:m){
if (v[i]==min(v))
am=i
     }
return(am)
}
    
	

