library(ggplot2)
library(reshape2)
library(gridExtra)

y = c(seq(1, 4, 0.1))
incumbent = (y-1)*(17+y)
entrant = (y-1)*(19-y)
dataframe_entr = data.frame(y, incumbent)
dataframe_inc = data.frame(y, entrant)
plot_dynamic = merge(dataframe_entr, dataframe_inc, by="y")
dynamic_melted <- melt(plot_dynamic, id.var='y')
colnames(dynamic_melted)[3]<- "Dynamic_component"
head(dynamic_melted)
ggplot(dynamic_melted, aes(x=y, y=Dynamic_component, col=variable)) + geom_line()





#Random Consumer Location prices graph

#p1_inc = 1/3*(y+2) - (beta/54)*(y-1)*(17+y)
#p1_e = 4/3 - 1/3*y - (beta/54)*(y-1)*(19-y)

#Myopic consumer
beta = 0
p1_inc = 1/3*(y+2) - (beta/54)*(y-1)*(17+y)
p1_e = 4/3 - 1/3*y - (beta/54)*(y-1)*(19-y)
dataframe_entr = data.frame(y, p1_inc)
dataframe_inc = data.frame(y, p1_e)
plot_price = merge(dataframe_entr, dataframe_inc, by="y")
price_melted <- melt(plot_price, id.var='y')
colnames(price_melted)[3]<- "Price"
head(price_melted)
plot1 = ggplot(price_melted, aes(x=y, y=Price, col=variable)) + geom_line()+ ggtitle("Myopic: beta = 0")

# Forward-looking consumer
y = c(seq(1, 2.5, 0.1))
beta = 1
p1_inc = 1/3*(y+2) - (beta/54)*(y-1)*(17+y)
p1_e = 4/3 - 1/3*y - (beta/54)*(y-1)*(19-y)
dataframe_entr = data.frame(y, p1_inc)
dataframe_inc = data.frame(y, p1_e)
plot_price = merge(dataframe_entr, dataframe_inc, by="y")
price_melted <- melt(plot_price, id.var='y')
colnames(price_melted)[3]<- "Price"
head(price_melted)
plot3 = ggplot(price_melted, aes(x=y, y=Price, col=variable)) + geom_line() + theme(axis.title.y=element_blank()) + ggtitle("Forward-looking: beta = 1")

# Standard value of beta
y = c(seq(1, 2.5, 0.1))
beta = 0.9
p1_inc = 1/3*(y+2) - (beta/54)*(y-1)*(17+y)
p1_e = 4/3 - 1/3*y - (beta/54)*(y-1)*(19-y)
dataframe_entr = data.frame(y, p1_inc)
dataframe_inc = data.frame(y, p1_e)
plot_price = merge(dataframe_entr, dataframe_inc, by="y")
price_melted <- melt(plot_price, id.var='y')
colnames(price_melted)[3]<- "Price"
head(price_melted)
plot2 = ggplot(price_melted, aes(x=y, y=Price, col=variable)) + geom_line() + theme(axis.title.y=element_blank()) + ggtitle("Classic: beta = 0.9")

#Random Consumer Location profit

#myopic
beta = 0 

pi_e = p1_e*((p1_inc-p1_e)/(2) + 1 - y/2) + 
  beta*(((p1_e-p1_inc)/(2)+ y/2)*((1/18*((4-y)^2))+1/2*((p1_inc-p1_e)/2+1 -y/2)))
pi_inc = p1_inc*((p1_e-p1_inc)/2+ y/2) + 
  beta* (((p1_e-p1_inc)/2 + y/2)*(1/18)*(2+y)^2 + 1/2*((p1_inc-p1_e)/2 + 1 - y/2))

dataframe_entr = data.frame(y, pi_inc)
dataframe_inc = data.frame(y, pi_e)
plot_profit = merge(dataframe_entr, dataframe_inc, by="y")
profit_melted <- melt(plot_profit, id.var='y')
colnames(profit_melted)[3]<- "Profit"
head(profit_melted)
plot4 = ggplot(profit_melted, aes(x=y, y=Profit, col=variable)) + geom_line()

#forward-looking

beta = 1 

pi_e = p1_e*((p1_inc-p1_e)/(2) + 1 - y/2) + 
  beta*(((p1_e-p1_inc)/(2)+ y/2)*((1/18*((4-y)^2))+1/2*((p1_inc-p1_e)/2+1 -y/2)))
pi_inc = p1_inc*((p1_e-p1_inc)/2+ y/2) + 
  beta* (((p1_e-p1_inc)/2 + y/2)*(1/18)*(2+y)^2 + 1/2*((p1_inc-p1_e)/2 + 1 - y/2))

dataframe_entr = data.frame(y, pi_inc)
dataframe_inc = data.frame(y, pi_e)
plot_profit = merge(dataframe_entr, dataframe_inc, by="y")
profit_melted <- melt(plot_profit, id.var='y')
colnames(profit_melted)[3]<- "Profit"
head(profit_melted)
plot6 = ggplot(profit_melted, aes(x=y, y=Profit, col=variable)) + geom_line() + theme(axis.title.y=element_blank())

#standard beta

beta = 0.9

pi_e = p1_e*((p1_inc-p1_e)/(2) + 1 - y/2) + 
  beta*(((p1_e-p1_inc)/(2)+ y/2)*((1/18*((4-y)^2))+1/2*((p1_inc-p1_e)/2+1 -y/2)))
pi_inc = p1_inc*((p1_e-p1_inc)/2+ y/2) + 
  beta* (((p1_e-p1_inc)/2 + y/2)*(1/18)*(2+y)^2 + 1/2*((p1_inc-p1_e)/2 + 1 - y/2))

dataframe_entr = data.frame(y, pi_inc)
dataframe_inc = data.frame(y, pi_e)
plot_profit = merge(dataframe_entr, dataframe_inc, by="y")
profit_melted <- melt(plot_profit, id.var='y')
colnames(profit_melted)[3]<- "Profit"
head(profit_melted)
plot5 = ggplot(profit_melted, aes(x=y, y=Profit, col=variable)) + geom_line()+ theme(axis.title.y=element_blank())

png("random_consumer_location.png", width =700, height = 350)

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=3, nrow = 2)
dev.off()



# Summarised graphs
#Price
beta = 0
p1_inc_my = 1/3*(y+2) - (beta/54)*(y-1)*(17+y)
p1_e_my = 4/3 - 1/3*y - (beta/54)*(y-1)*(19-y)
beta = 1
p1_inc_for = 1/3*(y+2) - (beta/54)*(y-1)*(17+y)
p1_e_for = 4/3 - 1/3*y - (beta/54)*(y-1)*(19-y)
beta = 0.9
p1_inc_cl = 1/3*(y+2) - (beta/54)*(y-1)*(17+y)
p1_e_cl = 4/3 - 1/3*y - (beta/54)*(y-1)*(19-y)

dataframe_entr = data.frame(y, p1_inc_my, p1_inc_for, p1_inc_cl)
dataframe_inc = data.frame(y, p1_e_my, p1_e_for, p1_e_cl)
plot_price = merge(dataframe_entr, dataframe_inc, by="y")
price_melted <- melt(plot_price, id.var='y')
colnames(price_melted)[3]<- "Price"
head(price_melted)



library(RColorBrewer)
myColors_incumbent <- brewer.pal(6,"Reds")[4:6]
myColors_entrant <- brewer.pal(6,"Blues")[4:6]
names(myColors_incumbent) <-c("p1_inc_my", "p1_inc_cl","p1_inc_for")
names(myColors_entrant) <-c("p1_e_my", "p1_e_cl", "p1_e_for")
myColors_incumbent
myColors<-c(myColors_incumbent, myColors_entrant)
colScale <- scale_colour_manual(name = "variable",values = myColors)
plot = ggplot(price_melted, aes(x=y, y=Price, col=variable)) + geom_line()

png("random_consumer_location_prices.png", width =700, height = 350)

plot + colScale
dev.off()

#Profits
beta = 0
p1_inc_my = 1/3*(y+2) - (beta/54)*(y-1)*(17+y)
p1_e_my = 4/3 - 1/3*y - (beta/54)*(y-1)*(19-y)
pi_e_my = p1_e*((p1_inc-p1_e)/(2) + 1 - y/2) + 
  beta*(((p1_e-p1_inc)/2+ y/2)*((1/18*((4-y)^2))+1/2*((p1_inc-p1_e)/2+1 -y/2)))
pi_inc_my = p1_inc*((p1_e-p1_inc)/2+ y/2) + 
  beta* (((p1_e-p1_inc)/2 + y/2)*(1/18)*(2+y)^2 + 1/2*((p1_inc-p1_e)/2 + 1 - y/2))

beta = 1
p1_inc_for = 1/3*(y+2) - (beta/54)*(y-1)*(17+y)
p1_e_for = 4/3 - 1/3*y - (beta/54)*(y-1)*(19-y)
pi_e_for = p1_e*((p1_inc-p1_e)/(2) + 1 - y/2) + 
  beta*(((p1_e-p1_inc)/(2)+ y/2)*((1/18*((4-y)^2))+1/2*((p1_inc-p1_e)/2+1 -y/2)))
pi_inc_for = p1_inc*((p1_e-p1_inc)/2+ y/2) + 
  beta* (((p1_e-p1_inc)/2 + y/2)*(1/18)*(2+y)^2 + 1/2*((p1_inc-p1_e)/2 + 1 - y/2))

beta = 0.9
p1_inc_cl = 1/3*(y+2) - (beta/54)*(y-1)*(17+y)
p1_e_cl = 4/3 - 1/3*y - (beta/54)*(y-1)*(19-y)
pi_e_cl = p1_e*((p1_inc-p1_e)/(2) + 1 - y/2) + 
  beta*(((p1_e-p1_inc)/(2)+ y/2)*((1/18*((4-y)^2))+1/2*((p1_inc-p1_e)/2+1 -y/2)))
pi_inc_cl = p1_inc*((p1_e-p1_inc)/2+ y/2) + 
  beta* (((p1_e-p1_inc)/2 + y/2)*(1/18)*(2+y)^2 + 1/2*((p1_inc-p1_e)/2 + 1 - y/2))


dataframe_entr = data.frame(y, pi_inc_my, pi_inc_for, pi_inc_cl)
dataframe_inc = data.frame(y, pi_e_my, pi_e_for, pi_e_cl)
plot_profit = merge(dataframe_entr, dataframe_inc, by="y")
profit_melted <- melt(plot_profit, id.var='y')
colnames(profit_melted)[3]<- "Profit"
head(profit_melted)

myColors_incumbent <- brewer.pal(6,"Reds")[4:6]
myColors_entrant <- brewer.pal(6,"Blues")[4:6]
names(myColors_incumbent) <-c("pi_inc_my", "pi_inc_cl","pi_inc_for")
names(myColors_entrant) <-c("pi_e_my", "pi_e_cl", "pi_e_for")
myColors_incumbent
myColors<-c(myColors_incumbent, myColors_entrant)
colScale <- scale_colour_manual(name = "variable",values = myColors)
plot = ggplot(profit_melted, aes(x=y, y=Profit, col=variable)) + geom_line()


png("random_consumer_location_profits.png", width =700, height = 350)

plot + colScale
dev.off()

