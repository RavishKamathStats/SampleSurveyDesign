##### Assignment 3 #####
########################
#### Question 1 ####
# Part A
Nh = c(20,26)
range = c(100-0, 200-10)
Sh = range/4
ah = round((Nh*Sh)/sum(Nh*Sh),4)
ah

# Part B
B = 100
NsqrdD = B^2/4

n = ceiling((sum(Nh*Sh)^2) / (NsqrdD + sum(Nh*(Sh^2))))
n
nh = ceiling(n*ah)
nh
# We come to an issue here since there are only 26 plants but our sample size tells us
#30. Hence we can use all 26 large plants in our sample size and then (41-26) = 15 for
#our small plants


#### Question 2 ####
MaleMangement = c(60,15,5)
FemaleMangement  = c(10,7,3)
MaleClerical = c(24,4,2)
FemaleClerical = c(42,30,8)
Mangement = rbind(MaleMangement,FemaleMangement)
Clerical = rbind(MaleClerical,FemaleClerical)
df = cbind(Mangement,Clerical)
df = data.frame(df)
row.names(df) = c('Male','Female')
colnames(df) = c('Mang a','Mang b','Mang c','Cler a','Cler b','Cler c')
df
nh = c(100,100)
Nh = c(300,500)
N = sum(Nh)
fh = nh/Nh


### Part A ###
hat_pMang = (df[1,1] + df[2,1])/100
hat_pMang

ssh = (nh[1]/(nh[1]-1))*(hat_pMang*(1-hat_pMang))

varhat_pMang = (1-fh[1])*(ssh/(nh[1]-1))
varhat_pMang


B = 2*sqrt(varhat_pMang)

hat_pMang + B
hat_pMang - B





### Part B ###
Wh = Nh/N

#Estimated Proportion
hat_pCler = (df[1,4] + df[2,4])/100
hat_ph = c(hat_pMang, hat_pCler)
hat_pSt = sum(Wh*hat_ph)
hat_pSt

#Estimated variance proportions
hat_pq = (hat_ph*(1-hat_ph))
hat_var_pSt = sum((Wh^2)*(1-fh)*hat_pq/(nh-1))
hat_var_pSt

B = 2*sqrt(hat_var_pSt)

hat_pSt + B
hat_pSt - B


### Part C ###
hat_pMang = df[2,2]/nh[1]
hat_pCler = df[2,5]/nh[2]
hat_ph = c(hat_tauMang, hat_tauCler)
hat_pSt = sum(Wh*hat_ph)
hat_tauSt = N*hat_pSt
hat_tauSt


hat_pq = (hat_ph*(1-hat_ph))
hat_var_pSt = sum((Wh^2)*(1-fh)*hat_pq/(nh-1))
hat_var_pSt

hat_var_tauSt = N^2 * hat_var_pSt
hat_var_tauSt

B = 2*sqrt(hat_var_tauSt)

hat_tauSt + B
hat_tauSt - B


### Part D ###
hat_pMangM = df[1,1]/nh[1]
hat_pMangF = df[2,1]/nh[1]

sh_sqrd = (nh[1]/(nh[1]-1))*(hat_pMangM*(1-hat_pMangM))

varhat_pMangM = Wh[1]^2*(1-fh[1])*sh_sqrd/nh[1]
varhat_pMangM

sh_sqrd2 = (nh[1]/(nh[1]-1))*(hat_pMangF*(1-hat_pMangF))
varhat_pMangF = Wh[1]^2*(1-fh[1])*sh_sqrd/nh[1]
varhat_pMangF

B = 2*sqrt(varhat_pMangM + varhat_pMangF)
B

(hat_pMangM - hat_pMangF) + B
(hat_pMangM - hat_pMangF) - B


### Part E ###
hat_pMang_a = (df[1,1] + df[2,1])/100
hat_pMang_a

sh_sqrd= (nh[1]/(nh[1]-1))*(hat_pMang_a*(1-hat_pMang_a))
varhat_pMang_a = Wh[1]^2*(1-fh[1])*sh_sqrd/nh[1]
varhat_pMang_a


hat_pMang_b = (df[1,2] + df[2,2])/100
hat_pMang_b

sh_sqrd2 = (nh[1]/(nh[1]-1))*(hat_pMang_b*(1-hat_pMang_b))
varhat_pMang_b = Wh[1]^2*(1-fh[1])*sh_sqrd/nh[1]
varhat_pMang_b

B = 2*sqrt(varhat_pMang_a + varhat_pMang_b)
B

(hat_pMang_a - hat_pMang_b) - B
(hat_pMang_a - hat_pMang_b) + B


#### Question 3 ####

### Part A ###

Job = c('Anesthesiologist', 'Resident', 'Nurse')
nh = c(913 + 417, 136 + 29, 860 + 240)
n = sum(nh)
# Need to assume S^2h are all equal to each other, hence ah = Wh
# Furthermore since, we can assume the population strata size Nh is quite large since we are taking the population of the United States.,
# we must ignore fpc in order to calculate the variance
ah = nh/n

yes_ph = c(.687,.824,.782)
no_ph = c(.314,.176,.218)

hat_pSt = sum(ah*yes_ph)
hat_pSt


hat_pq = yes_ph*no_ph
hat_var_pSt = sum( (ah^2) * (hat_pq/(nh - 1)) )
hat_var_pSt

B = 2*sqrt(hat_var_pSt)
B

hat_pSt + B
hat_pSt - B


### Part B ###

ssh = (nh/(nh - 1))*(yes_ph*no_ph)
hat_var_ph = ssh/(nh-1) 

B = 2*sqrt(hat_var_ph[1] + hat_var_ph[2])
B
(yes_ph[1] - yes_ph[2]) - B
(yes_ph[1] - yes_ph[2]) + B 




### Part C ###
ssh = (nh/(nh - 1))*(yes_ph*no_ph)
hat_var_ph = ssh/(nh-1) 
B = 2*sqrt(hat_var_ph[1] + hat_var_ph[3])
B
(yes_ph[1] - yes_ph[3]) - B
(yes_ph[1] - yes_ph[3]) + B

#### Question 4 ####

### Part A ###
# Need to assume S^2h are all equal to each other, hence ah = Wh
nh = c(1347, 163, 1095)
n = sum(nh)
ah = nh/n
sd = c(0.15, 0.35, 0.11)


ybar_h = c(7.63, 7.74, 6.55)
nh = c(1347, 163, 1095)
ybarSt = sum(ah*ybar_h)


hat_var_ybarSt = sum((ah^2)*((sd^2)/nh))
hat_var_ybarSt

B = 2*sqrt(hat_var_ybarSt)
B
ybarSt + B
ybarSt - B


### Part B ###
#let us look at the confidence interval for each job
B = 2*sqrt(sd[1]/nh[1] + sd[2]/nh[2])
B

ybar_h[2] - ybar_h[1] - B
ybar_h[2] - ybar_h[1] + B















