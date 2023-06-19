# 4. SIMULACION 
P0 = 96
mu = 0.19
sigma = 0.15
T = 0.5
n = 182 
dt = T/n

# 4.1 ----
set.seed(895187)
#Simulacion 
m = 1941  
Pt = matrix(NA, nrow = m, ncol = n+1) 

Pt[,1] = P0

for (i in 1:m) {
  for (t in 2:(n+1)){
    Pt[i,t] = Pt[i,t-1]*exp((mu-0.5*sigma^2)*dt + sigma*sqrt(dt)*rnorm(1))
  } 
} 

EsperanzaPtF = mean(Pt[,183]) #105.229888626251
DesvioPtF = sd(Pt[,183]) #11.2043181867133




# 4.2 ---- 
library(tidyverse)
df_Pt = as.tibble(Pt[,ncol(Pt)])

a = df_Pt %>% filter(value < EsperanzaPtF)

nrow(a) / nrow(df_Pt) # 0.5239567

# 4.3
prob = 0.95 
LI = matrix(NA, nrow = 1, ncol = n+1)
for (i in 1:(n+1)) {
  LI[i] = quantile(Pt[,183],1-prob)
}
LI
#88.08886
quantile(PT,1-prob)
