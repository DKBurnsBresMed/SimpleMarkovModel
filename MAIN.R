library("expm")
library("tidyverse")


TH <- 100

# Function: calculates state population in cycle n, given initial probabilities P
calc_cycle <- function(M, n, P) {
  P %*% (M %^% n)
}

# function: calculates the state populations next cycle given survival in t1 and a matrix
#   tip, use a list of TPs to apply this in parallel to capture time-varying TPs
calc_next_cycle <- function(M, s_t1) {
  s_t1 %*% M
}


smt0 <- c(1,0,0)

tp <- t(
  matrix(c(
    0.95,0.025,0.025,
    0.025,0.95,0.025,
    0,0,1
  ),ncol = 3,nrow = 3)
)

Trace <- do.call(
  'rbind',
  lapply(1:TH,function(n) calc_cycle(tp,n,smt0))
)

colnames(Trace) <- c("Well","Sick","Dead")

TracePlot <- do.call(
  'rbind',
  lapply(1:ncol(Trace),function(COL){
    data.frame(
      time = 1:nrow(Trace),
      value = Trace[,COL],
      state = colnames(Trace)[COL]
    )
  })
)

ggplot(TracePlot,aes(x = time,y = value,colour = state)) +
  geom_line()
