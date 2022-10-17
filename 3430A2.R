###Assignment 2####
#Ravish Kamath


#### Question 1 #####
u = c(1,2,3,4)
N = 4 
n = 2
sample = t((combn(1:N, n)))
sample
delta = c(0.1, 0.1, 0.4, 0.4)
prob = delta[sample[,1]]*delta[sample[,2]]/(1-delta[sample[,1]])+
      delta[sample[,2]]*delta[sample[,1]]/(1-delta[sample[,2]])
pi = rep(0, length(u))
for(i in 1:length(u)){
  pi[i] = sum(prob[sample[,1]== i]) + sum(prob[sample[,2] == i])
}
wi = 1/pi
wi
sumWeights = rep(0, length(sample[,1]))  
sumWeights = wi[sample[,1]] + wi[sample[,2]]

expected_wi= sum(sumWeights*prob)


#### Question 2 ####
u = c(1,2,3,4)
N = 4
n = 3
sample = t((combn(1:N, n)))
sample

delta = c(0.1, 0.1, 0.4, 0.4)
prob = delta[sample[,1]]*(delta[sample[,2]]/(1-delta[sample[,1]]))*(delta[sample[,3]]/(1-delta[sample[,1]]-delta[sample[,2]]))+
       delta[sample[,1]]*(delta[sample[,3]]/(1-delta[sample[,1]]))*(delta[sample[,2]]/(1-delta[sample[,1]]-delta[sample[,3]]))+
       delta[sample[,2]]*(delta[sample[,3]]/(1-delta[sample[,2]]))*(delta[sample[,1]]/(1-delta[sample[,2]]-delta[sample[,3]]))+
       delta[sample[,2]]*(delta[sample[,1]]/(1-delta[sample[,2]]))*(delta[sample[,3]]/(1-delta[sample[,2]]-delta[sample[,1]]))+
       delta[sample[,3]]*(delta[sample[,1]]/(1-delta[sample[,3]]))*(delta[sample[,2]]/(1-delta[sample[,3]]-delta[sample[,1]]))+
       delta[sample[,3]]*(delta[sample[,2]]/(1-delta[sample[,3]]))*(delta[sample[,1]]/(1-delta[sample[,3]]-delta[sample[,2]]))

pi = rep(0, length(u))
for(i in 1:length(u)){
  pi[i] = sum(prob[sample[,1]== i]) + sum(prob[sample[,2] == i]) + sum(prob[sample[,3]== i])
}

wi = 1/pi
wi 

sumWeights = rep(0, length(sample[,1]))  
sumWeights = wi[sample[,1]] + wi[sample[,2]] + wi[sample[,3]]

expected_wi= sum(sumWeights*prob)
expected_wi


##### Question 3 ####
View(schools)
ybar = rep(0,1000)
for(i in 1:1000){
  n = sample(1:50, 5, replace = FALSE)
  teachers = schools[n,]$Teachers
  ybar[i] = mean(teachers)
}
ybar
hist(ybar)


sd(ybar)
# Looking at the histogram, if we normalize the distribution, we do seem to look at a pretty symmetrical
#normal distribution. The reason for this could be the fact that we have repeated the sample a 1000 times
#which according to the central limit theorem, becomes normally distributed for a large n numbers of times.

#We get the standard deviation to be very close to the theoretical value, 28,465, as shown in the code above


#### Question 4 ####
response = c('Virtual certainty', 'Very likely', 'Somewhat likely', 'About 50-50', 
             'Somewhat unlikely', 'Very unlikely','Absolutely not', 'No response') 
allRespondents = c(.22,.04,.19,.18,.06,.12,.15,.04)
advertisedPast = c(.35,.05,.35,.15,.10,0,0,0)
df = data.frame(response, allRespondents, advertisedPast)  
colnames(df) = c('Response', 'All respondents (82)',
                 'Those having advertised in the past (46)')
N = 1400
n = 82
nPrime = 46
#Part A
p_hat = df[1,2]
p_hat
q_hat = 1-p_hat

var_hat = (1-(n/N))*((p_hat*q_hat)/(n-1))
B = 2*(sqrt(var_hat))
round(B,4)

#Part B
p_hat = sum(df[1:4,2])
p_hat
q_hat = 1-p_hat
var_hat = (1-(n/N))*((p_hat*q_hat)/(n-1))
var_hat
B = 2*(sqrt(var_hat))
round(B,4)

#Part C
p_hat = df[5,3]
p_hat
q_hat = 1-p_hat
var_hat = (1-(nPrime/N))*((p_hat*q_hat)/(nPrime-1))
B = 2*(sqrt(var_hat))
round(B,4)

#Part D
p_hat = sum(df[1:4,3])
p_hat
q_hat = 1-p_hat
var_hat = (1-(nPrime/N))*((p_hat*q_hat)/(nPrime-1))
B = 2*(sqrt(var_hat))
round(B,4)



#### Question 5 ####

#Part A
teachers = schools$Teachers
states = schools$State
popu = schools$Pop
delta = round(schools$Pop/sum(schools$Pop),4)
N = dim(schools)[1]
n = 2
sample=expand.grid(1:N,1:N)
dim(sample)
prob = rep(0,N)
prob = delta[sample[,1]]*delta[sample[,2]]
tau_hat = 1/n*(teachers[sample[,1]]/delta[sample[,1]]+teachers[sample[,2]]/delta[sample[,2]])
expectedTau_hat = prob%*%tau_hat
expectedTau_hat


#Part B
sample = t(combn(1:N,n))
prob = rep(0,N)
prob = delta[sample[,1]]*delta[sample[,2]]/(1-delta[sample[,1]]) + 
       delta[sample[,2]]*delta[sample[,1]]/(1-delta[sample[,2]])

pi=rep(0,length(schools$Teachers))

for(i in 1:length(schools$Teachers)){
  pi[i]=sum(prob[sample[,1]==i])+sum(prob[sample[,2]==i]) 
}

tau_hat = rep(0,dim(sample)[1])
tau_hat = teachers[sample[,1]]/pi[sample[,1]]+teachers[sample[,2]]/pi[sample[,2]]
prob%*%tau_hat

