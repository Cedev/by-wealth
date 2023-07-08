source('mix.R')

# numbers are _very_ approximate
gdp <- c(US=70000, China=12500)
gini <- c(US=0.41, China=0.46)
pop <- c(US=331, China=1412)

m <- log(gdp)
s <- 2*erf.inv(gini)
w <- pop/sum(pop)

world <- mix(mapply(Norm, m, s), w)

x <- seq(0, 1, 0.005)
y1 <- sapply(x, function(x) { world@mixDistr[[1]]@p(world@q(x)) })
y2 <- sapply(x, function(x) { world@mixDistr[[2]]@p(world@q(x)) })
df <- data.frame(x,y1,y2)

ggplot(df, aes(x)) +                    # basic graphical object
  geom_line(aes(y=y1), colour="blue") +  # first layer
  geom_line(aes(y=y2), colour="red")     # second layer

resolution <- 1024

ds <- world@quantile_bins(resolution)

df <- reshape2::melt(t(ds))
df[,"Var1"] <- (df[,"Var1"]-0.5)/resolution

ggplot(df, aes(x=Var1, y=value, fill=Var2)) + 
  geom_bar(stat="identity", width=1/resolution) +
  scale_fill_manual("legend", values = c("blue", "red")) +
  labs(title = "China and US population", y = "portion of combined population", x = "GDP/capita quantile")
