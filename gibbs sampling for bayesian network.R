c = c(0.5,0.5)                                                        # mentioning the probabilities of all events in the network
r = matrix(c(0.8,0.2,0.8,0.2),nrow = 2)

s = matrix(c(0.5,0.9,0.5,0.1),nrow = 2)

w = matrix(c(1.0,0.1,0.1,0.01,0.0,0.9,0.9,0.99),nrow = 4)
#print(w)

rowcalw <- function(is,ir)                                            # calculating the row of wetgrass based on values of rain,sprinkle
{
res = 0
if(ir == 1 && is == 1){res = 1}
else if(ir == 1 && is == 2){res = 2}
else if(ir == 2 && is == 1){res = 3}
else{res = 4}
return(res)
}

cond_w <- function(ir,is)                                             # calculating the conditional probability function of all events according to joint distribution
{
return(w[rowcalw(is,ir),2]/(w[rowcalw(is,ir),1]+w[rowcalw(is,ir),2]))
}

cond_c <- function(is,ir)
{
return((c[2]*r[2,ir]*s[2,is])/((c[2]*r[2,ir]*s[2,is])+(c[1]*r[1,ir]*s[1,is])))
}

cond_s <- function(ic,ir,iw)
{
return((s[ic,2]*w[rowcalw(2,ir),iw])/((s[ic,2]*w[rowcalw(2,ir),iw])+(s[ic,1]*w[rowcalw(1,ir),iw])))
}

cond_r <- function(ic,is,iw)
{
return((r[ic,2]*w[rowcalw(is,2),iw])/((r[ic,2]*w[rowcalw(is,2),iw])+(r[ic,1]*w[rowcalw(is,1),iw])))
}

#network = c(c,r,s,w)
start_point1 = c(1,2,1,2)
start_point2 = c(1,2,1,2)
q = data.frame(rbind(start_point1,start_point2))
cnt <- 1
start_point = c(1,2,1,2)              #1 - false , 2 - true

while (cnt < 1000)                    # taking a loop for 1000 times
{

y = 0
a = floor(runif(1,1,5))

if(a == 1)							# randomly instantiating one event and calculating conditional probability of that event
{y = cond_c(start_point[3],start_point[2])
}else if(a == 2){y = cond_r(start_point[1],start_point[3],start_point[4])
}else if(a == 3){y = cond_s(start_point[1],start_point[2],start_point[4])
}else{y = cond_w(start_point[2],start_point[3])}

b = runif(1,0,1)
#print(a)
#print(y)
#print(b)

if (y>b)                           # if conditional probability is greater than number generated, then we do not change the value
{					     # if conditional probability is lesser than number generated, then we change the value
#print('no change')
}else if(y<b){
if (start_point[a] == 1)
{
start_point[a] = 2
}else if(start_point[a] == 2)
{
start_point[a] = 1}
}
if (cnt > 100){
q = data.frame(rbind(start_point,q))}
cnt = cnt +1
}
print(nrow(q))
#print(q[1:50,])

#q is the dataframe of the samples

#network = c(c,r,s,w)

WTCT = subset(q,q$X4 != 1 & q$X1 !=1)
print(paste0("The probility of W = T and C = T is: ",nrow(WTCT)/nrow(q)))

WTCF = subset(q,q$X4 != 1 & q$X1 !=2)
print(paste0("The probility of W = T and C = F is: ",nrow(WTCF)/nrow(q)))

WFCT = subset(q,q$X4 != 2 & q$X1 !=1)
print(paste0("The probility of W = F and C = T is: ",nrow(WFCT)/nrow(q)))

WFCF = subset(q,q$X4 != 2 & q$X1 !=2)
print(paste0("The probility of W = F and C = F is: ",nrow(WFCF)/nrow(q)))

RTST = subset(q,q$X2 != 1 & q$X3 !=1)
print(paste0("The probility of R = T and S = T is: ",nrow(RTST)/nrow(q)))

RTSF = subset(q,q$X2 != 1 & q$X3 !=2)
print(paste0("The probility of R = T and S = F is: ",nrow(RTSF)/nrow(q)))

RFST = subset(q,q$X2 != 2 & q$X3 !=1)
print(paste0("The probility of R = F and S = T is: ",nrow(RFST)/nrow(q)))

RFSF = subset(q,q$X2 != 2 & q$X3 !=2)
print(paste0("The probility of R = F and S = F is: ",nrow(RFSF)/nrow(q)))
