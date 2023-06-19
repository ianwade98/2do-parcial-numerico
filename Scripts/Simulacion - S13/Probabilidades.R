# PROBABILIDAD
Start_Count = 0
for(i in 1:length(PT)){
  if(PT[i]<260.34){Start_Count = Start_Count + 1}}
Probabilidad = Start_Count/length(PT)
Probabilidad

Start_Count1 = 0
for(i in 1:length(PT)){
  if(PT[i]>577.57){ Start_Count1 = Start_Count1 + 1 }}

Probabilidad_1= Start_Count1/length(PT)
Probabilidad_1

Start_Count2 = 0
for(i in 1:nrow(P0at)){
  if(P0at[i,251]<260.34){Start_Count2 = Start_Count2 + 1}}
Probabilidad_2 = Start_Count2/nrow(P0at)
Probabilidad_2

Start_Count3 = 0
for(i in 1:nrow(P0at)){
  if(P0at[i,251]>577.57){Start_Count3 = Start_Count3 + 1 }}
Probabilidad_3 = Start_Count3/nrow(P0at)
Probabilidad_3