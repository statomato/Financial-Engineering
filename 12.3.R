W1 <- rnorm(1000000)
S1 <- S0 * exp((r-0.5*sigma^2)*(1)+sigma*W1)
W9 <- rnorm(1000000, sd=sqrt(9))
S10 < S1 * exp((r-0.5*sigma^2)*(1)+sigma*W9)

c <- exp(-r*9)*pmax(S10-40,0)
model <- lm(C~poly(S1, 5, raw=TRUE))
my_data <- data.frame(C=C, S1=S1)
model <- lm(C~ poly(S1, 5, raw=TRUE), data=my_data)
S <- predict(model, my_Data)
sorted_S <- sort(S)
sorted_S[1000000*0.95]; sorted_S[1000000*0.05]