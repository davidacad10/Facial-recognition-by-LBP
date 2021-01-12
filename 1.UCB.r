#Data read
setwd('Research/Reinforcement Learning/Next Best Action/')
ucb = read.csv('Data/ucb_data.csv')
user = ucb$user
ucb$user <- NULL

#Benchmark-Random selection score
N = 10000
d = 6
total_reward_rand = 0
ad_selected = c()
for (i in seq(1,N,1)){
  ad = sample(x = d,size = 1)
  ad_selected[i] = ad
  reward = ucb[i,ad]
  total_reward_rand = total_reward_rand + reward
}  

#Result
print(total_reward_rand)
hist(ad_selected)

#UCB algorithm
N = 10000
d = 6
ad_selected = c()
no_selection = c(rep(0,6))
sum_reward = c(rep(0,6))
total_reward_ucb = 0



for (i in seq(1,N,1)) {
  print(i)
  max_upper_bnd = 0
  ad = 1
  for (j in seq(1,d,1)) {
    if (no_selection[j] > 0) {
      avg_reward = sum_reward[j]/no_selection[j]
      delta_j = sqrt(3/2 * log(i)/no_selection[j])
      upper_bnd = avg_reward + delta_j
    }
    else{
      upper_bnd = 1e5
    }
    if (upper_bnd > max_upper_bnd){
      max_upper_bnd = upper_bnd
      ad = j
    }
  }
  ad_selected[i] = ad
  no_selection[ad] = no_selection[ad] + 1
  reward = ucb[i,ad]
  sum_reward[ad] = sum_reward[ad] + reward
  total_reward_ucb = total_reward_ucb + reward
}  

#Result
print(total_reward_ucb)
hist(ad_selected)
