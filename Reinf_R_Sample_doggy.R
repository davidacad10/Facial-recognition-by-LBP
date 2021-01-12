
set.seed(3)
library('plot.matrix')
library(RColorBrewer)

### Plotting the matrix Function ###
plot_matrix <- function(mat_pot, digits_arg){
  digits_arg <- ifelse(missing(digits_arg),0,digits_arg)
  plot(mat_pot, cex = 1.2, fmt.cell=paste0('%.',digits_arg,'f'), 
       col=brewer.pal(3,"Blues"), breaks=c(-500, 0, 0, 500),
       xlab="state", ylab = "",main = "")
}

###### Define Environment #######
states <- seq(1, 4, by = 1);
#actions <- seq(1, 5, by = 1);
state_seq <- cbind(merge(states,states), state = seq(1,length(states)*length(states)))
state_mat <- matrix(state_seq$state, nrow = length(states), ncol= length(states))
rewards <- c(-10,-10,5,5,
             5,5,5,-10
             ,5,5,5,5
             ,5,5,5,5)

rewards_mat <- matrix(rewards, nrow = length(states), ncol= length(states))

goal <- which(rewards==max(rewards), arr.ind=TRUE)
goal
rewards_mat

## Initialize Q-Matrix
Q <- matrix(0,  nrow = length(states), ncol= length(states))

###### get Next states ####### 
diagonal_steps <- FALSE
getNextStates <- function(cs) {
  if (cs==1) {
    ns <- c(2,5);} 
  if (cs==2) {
    ns <- c(1,3,6);} 
  if (cs==3) {
    ns <- c(2,4,7);} 
  if (cs==4) {
    ns <- c(3,8);} 
  
  if (cs==5) {
    ns <- c(1,6,9);} 
  if (cs==6) {
    ns <- c(2,5,7,10);} 
  if (cs==7) {
    ns <- c(3,6,8,11);} 
  if (cs==8) {
    ns <- c(4,7,12);} 
  
  if (cs==9) {
    ns <- c(5,10,13);} 
  if (cs==10) {
    ns <- c(6,9,11,14);} 
  if (cs==11) {
    ns <- c(7,10,12,15);} 
  if (cs==12) {
    ns <- c(8,11,16);} 

  if (cs==13) {
    ns <- c(9,14);} 
  if (cs==14) {
    ns <- c(10,13,15);} 
  if (cs==15) {
    ns <- c(11,14,16);} 
  if (cs==16) {
    ns <- c(12,15);} 
  
  NS=16
  
  nss <- sort(ns[ns > 0 & ns <= NS]);
  return(nss);
}
getNextStates(8)

########### Episodes Execution ################
N <- 10        # No. of Episode
alpha <- 0.1    # Learning Rate
gamma <- 0.9    # Discount Factor


for (i in 1:N) {
  if(!is.null(dev.list()["RStudioGD"])){
    dev.off(dev.list()["RStudioGD"])
  }
  current_episode <- i;
  cat("\nStart Episode: ", current_episode)
  
  ## choose next state from possible actions at current state
  
  cs <- sample(state_seq$state, 1)
  if(i==1){cs=1}
  cat("\n\tCurrent state: ", cs)
  step_num <- 1; 
  while (T) {
    cat("\n\n\tStep no.: ", step_num)
    cat("\n\t\tCurrent State: ", cs)
    reward <- rewards[cs]
    if(reward == 0 | is.na(reward) | length(reward) == 0 ){
      reward <- 0
    }
    cat("\n\t\tReward CS: ", reward)
    next.states <- getNextStates(cs);
    cat("\n\t\tPossible next states: ", next.states)
    
    # next.states
    # If we have any states present, else choose randomly.
    if (length(next.states) == 1) {
      ns <- next.states
    } else {
      ns <- sample(next.states, 1)
    }
    cat("\n\t\tNext state: ", ns)
    
    # Update Q values for next states.
    Q[cs] <- round(Q[cs] + alpha * (reward + gamma * max(Q[getNextStates(ns)])-Q[cs]),1);
    cat("\n\t\tNew Q-Value: ", Q[cs])
    plot_matrix(Q,1)
    Sys.sleep(0.2)
    if (step_num > 15) {
      break;
    }
    cs <- ns;
    step_num <- step_num + 1;
  }
  cat("\nEnd Episode: ", current_episode)
}

Q
