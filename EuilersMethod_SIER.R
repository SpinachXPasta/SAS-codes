library("ggplot2")

#parametrs
c <- 3
b <- 0.2
a <- 1
g <- 0.1
w <- 0.2

#inital conditions
S0 <- 9990
E0 <- 10
I0 <- 0
M0 <- 0
R0 <- 0


S_ <-  function(S,E,I,M,R){
  return ((-c)*b * (I/(S+E+I+M+R))*S)
}

E_ <- function(S,E,I,M,R){
  return ((c*b * (I/(S+E+I+M+R)*S)) -(a*E))
}

I_ <- function(E,I){
  return( (a*E) - (g*I))
}

M_ <- function(I,M){
  return( (g*I) - (w*M))
}

R_ <- function(M){
  return(M*w)
}



s <- c()
e <- c()
i <-c()
m <- c()
r <- c()

t = 1
T = 100
scale = 1

for (k in 1:T){
  s <- c(s,S0 + S_(S0,E0,I0,M0,R0)*t)

  e <- c(e,E0 + E_(S0,E0,I0,M0,R0)*t)

  i <- c(i,I0 + I_(E0,I0)*t)

  m <- c(m,M0 + M_(I0,M0)*t)

  r <- c(r,R0 + R_(M0)*t)

  k1 = S0
  k2 = E0
  k3 = I0
  k4 = M0
  k5 = R0

  S0 <- S0 + S_(k1,k2,k3,k4,k5)*t
  E0 <- E0 + E_(k1,k2,k3,k4,k5)*t
  I0 <- I0 + I_(k2,k3)*t
  M0 <- M0 + M_(k3,k4)*t
  R0 <- R0 + R_(k4)*t

}



ggplot()+geom_line(aes(1:T/scale,s), color = "green")+
  geom_line(aes(1:T / scale,e), color = "blue")+
  geom_line(aes(1:T / scale,i), color = "yellow")+
  geom_line(aes(1:T/scale,m), color = "red")+
  geom_line(aes(1:T/scale ,r))



df <- data.frame(s,e,i,m,r)

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[,nums] <- round(df[,nums], digits = digits)

  (df)
}

df = round_df(df, digits=3)





