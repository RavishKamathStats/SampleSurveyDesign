#### 3430 Assignment 4 ####

#### Question 1 ####
x = c(216962, 85378, 86763, 92664, 87615, 70759, 78542, 
      97058, 101407, 88269, 127731, 123491, 176277)
y = c(300000, 92402, 125551, 135208, 126492, 99230, 116778, 
      136774, 143281, 140490, 179311, 164870, 297458)
data.frame(x,y)

r = mean(y)/mean(x)

n = 13
N = 47
f = n/N

sy2 = 1/(n-1)*sum((y - mean(y))^2)
sx2 = 1/(n-1)*sum((x - mean(x))^2)
sxy = 1/(n-1)*sum((x - mean(x))*(y-mean(y)))
p_hat = sxy/(sqrt(sx2)*sqrt(sy2))

var_hat_r = (1-f)/(n*mean(x)^2)*(sy2 + r^2*sx2 - 2*r*p_hat*sqrt(sx2)*sqrt(sy2))

B = 2*sqrt(var_hat_r)

r - B
r + B

# r is 1.436124 with a bound of 0.07215933
# CI is (1.363964, 1.436124)

#### Question 2 ####

Nh = c(250, 18)
nh = c(18, 10)
fh = nh/Nh
yh = c(2250700, 1522100)
xh = c(36475, 21125)
R_hat_h = yh/xh
Wh = Nh/nh
varh = c(725789639, 1415080000)

R_hat_sr = sum(Wh*R_hat_h)

var_hat_sr = sum(Wh^2*((1-fh)/nh) *varh)

B = 2*sqrt(var_hat_sr)


R_hat_sr - B
R_hat_sr + B


##### Question 3 ####
N = 1800
k = 50 
n = N/k
f = n/N
y = c(12, 11.91, 11.87, 12.05, 11.75, 11.85, 
      11.97, 11.98, 12.01, 11.87, 11.93, 11.98,
      12.01, 12.03, 11.98, 11.91, 11.95, 11.87,
      12.03, 11.98, 11.87, 11.93, 11.97, 12.05,
      12.01, 12, 11.90, 11.94, 11.93, 12.02,
      11.80, 11.83, 11.88, 11.89, 12.05, 12.04)
df = matrix(y, nrow = 6)

mu_hat_sy = k*sum(y)/N

s2 = var(y)
var_hat_sy = (1-f)*s2/n

B = 2*sqrt(var_hat_sy)

mu_hat_sy - B
mu_hat_sy + B

# mu = 11.94556
# B =  0.02514864
#CI = (11.92041, 11.9707)

#### Question 4 ####
n = 20
N = 96
f = n/N
repaircost = c(50,110,230,140,60,
               280,240,45,60,230,
               140,130,70,50,10,
               60,280,150,110,120)

saws = c(3,7,11,9,2,
         12,14,3,5,9,
         8,6,3,2,1,
         4,12,6,5,8)
data.frame(saws, repaircost)


ybar = sum(repaircost)/sum(saws)
ybar

sr = sd(repaircost - ybar*saws)
sr

var_hat = (1-f)/(n*(sum(saws)/n)^2)*sr^2
var_hat
B = 2*sqrt(var_hat)
B

ybar - B
ybar + B

#ybar is 19.73077
# CI is (17.95067, 19,73077)



n = 25
N = 415
f = n/N

residents = c(8,12,4,5,6,
              6,7,5,8,3,
              2,6,5,10,9,
              3,6,5,5,4,
              6,8,7,3,8)

income = c(96,121,42,65,52,
           40,75,65,45,50,
           85,43,54,49,53,
           50,32,22,45,37,
           51,30,39,47,41)
income = 1000*income

mean(residents)
median(residents)
sd(residents)
mean(income)
median(income)
sd(income)
ybar = sum(income)/sum(residents)
sr = sd(income - ybar*residents)


var_hat = (1-f)/(n*(sum(residents)/n)^2)*sr^2
var_hat


 

x = pchisq(220,df = 200, lower.tail =  TRUE)
y = pchisq(195, df = 200, lower.tail = TRUE)

x - y




