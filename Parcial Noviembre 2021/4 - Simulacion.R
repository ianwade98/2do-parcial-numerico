# 4 - Simulacion de Montecarlo

# 4.1 ----
P0 = 73
mu = 0.13
sigma = 0.1
T = 5/12
n = 151 
dt = T/n

set.seed(895700)
m = 1233 
Pt = matrix(NA, nrow = m, ncol = n+1) 
Pt[,1] = P0

for (i in 1:m) {
  for (t in 2:(n+1)){
    Pt[i,t] = Pt[i,t-1]*exp((mu-0.5*sigma^2)*dt + sigma*sqrt(dt)*rnorm(1))
  } 
} 

mean(Pt[,152])
sd(Pt[,152])
Es = mean(Pt[,152])

k=length(Pt[1,])
mean(Pt[,k])
sd(Pt[,k])

hist(Pt[,152])


# 4.2 ----
library(tidyverse)
df_Pt = as.tibble(Pt[,152])

a = df_Pt %>% filter(value > 38) %>%  filter(value < P0)

nrow(a) / nrow(df_Pt)


# 4.3 ----

df_Pt = as.tibble(Pt[,152])

a = df_Pt %>% filter(value > Es)

nrow(a) / nrow(df_Pt)

