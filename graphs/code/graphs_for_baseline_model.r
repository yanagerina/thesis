library(ggplot2)
library(reshape2)
library(gridExtra)
library(RColorBrewer)

#######################################
######Entrant/Incumbent profits########
#######################################

# by y 
y = c(seq(1, 4, 0.1))
beta = 0

p1_inc_my = 1/3*(y+2) - (beta/54)*(y-1)*(17+y)
p1_e_my = 4/3 - 1/3*y - (beta/54)*(y-1)*(19-y)
pi_e_my = p1_e_my*((p1_inc_my-p1_e_my)/(2) + 1 - y/2) + 
  beta*(((p1_e_my-p1_inc_my)/(2)+ y/2)*((1/18*((4-y)^2))+1/2*((p1_inc_my-p1_e_my)/2+1 -y/2)))
pi_inc_my = p1_inc_my*((p1_e_my-p1_inc_my)/2+ y/2) + 
  beta* (((p1_e_my-p1_inc_my)/2 + y/2)*(1/18)*(2+y)^2 + 1/2*((p1_inc_my-p1_e_my)/2 + 1 - y/2))
myopic_incumbent = pi_inc_my
myopic_entrant = pi_e_my


beta = 1
p1_inc_for = 1/3*(y+2) - (beta/54)*(y-1)*(17+y)
p1_e_for = 4/3 - 1/3*y - (beta/54)*(y-1)*(19-y)
pi_e_for = p1_e_for*((p1_inc_for-p1_e_for)/(2) + 1 - y/2) + 
  beta*(((p1_e_for-p1_inc_for)/(2)+ y/2)*(1/18*((4-y)^2))+1/2*((p1_inc_for-p1_e_for)/2+1 -y/2))
pi_inc_for = p1_inc_for*((p1_e_for-p1_inc_for)/2+ y/2) + 
  beta* (((p1_e_for-p1_inc_for)/2 + y/2)*(1/18)*((2+y)^2) + 1/2*((p1_inc_for-p1_e_for)/2 + 1 - y/2))
forward_looking_incumbent = pi_inc_for
forward_looking_entrant = pi_e_for


beta = 0.9
p1_inc_cl = 1/3*(y+2) - (beta/54)*(y-1)*(17+y)
p1_e_cl = 4/3 - 1/3*y - (beta/54)*(y-1)*(19-y)
pi_e_cl = p1_e_cl*((p1_inc_cl-p1_e_cl)/(2) + 1 - y/2) + 
  beta*(((p1_e_cl-p1_inc_cl)/(2)+ y/2)*(1/18*((4-y)^2))+1/2*((p1_inc_cl-p1_e_cl)/2+1 -y/2))
pi_inc_cl = p1_inc_cl*((p1_e_cl-p1_inc_cl)/2+ y/2) + 
  beta* (((p1_e_cl-p1_inc_cl)/2 + y/2)*(1/18)*((2+y)^2) + 1/2*((p1_inc_cl-p1_e_cl)/2 + 1 - y/2))
classic_incumbent = pi_inc_cl
classic_entrant = pi_e_cl



dataframe_entr = data.frame(y, myopic_incumbent, classic_incumbent, forward_looking_incumbent)
dataframe_inc = data.frame(y, myopic_entrant, classic_entrant, forward_looking_entrant)
plot_profit = merge(dataframe_entr, dataframe_inc, by="y")
profit_melted <- melt(plot_profit, id.var='y')
colnames(profit_melted)[3]<- "Profit"
head(profit_melted)

myColors_incumbent <- brewer.pal(6,"Reds")[4:6]
myColors_entrant <- brewer.pal(6,"Blues")[4:6]
names(myColors_incumbent) <-c("myopic_incumbent", "classic_incumbent","forward_looking_incumbent")
names(myColors_entrant) <-c("myopic_entrant", "classic_entrant", "forward_looking_entrant")
myColors_incumbent
myColors<-c(myColors_incumbent, myColors_entrant)
colScale <- scale_colour_manual(name = "variable",values = myColors)
plot = ggplot(profit_melted, aes(x=y, y=Profit, col=variable)) + 
       geom_line() +xlim(1, 4.3)

png("random_consumer_location_profits.png", width =700, height = 350)

directlabels::direct.label(plot, list(cex=1.5, "last.qp"))+ colScale
dev.off()

########################################
####### Entrant/Incumbent prices #######
########################################


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

dataframe_entr = data.frame(y, p1_inc_my,  p1_inc_cl, p1_inc_for)
dataframe_inc = data.frame(y, p1_e_my, p1_e_cl, p1_e_for)
plot_price = merge(dataframe_entr, dataframe_inc, by="y")
price_melted <- melt(plot_price, id.var='y')
colnames(price_melted)[3]<- "Price"
head(price_melted)

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




########################################
##### Separate graphs by beta ##########
########################################

#Data Simulation

beta = c(seq(0, 2, 0.05))
y = c(seq(1, 4, 0.1))
profits = NULL

for (i in y){
  for (b in beta){
    
    p1_inc = 1/3*(i+2) - (b/54)*(i-1)*(17+i)
    p1_e = 4/3 - 1/3*i - (b/54)*(i-1)*(19-i)
    
    pi_inc = p1_inc*((p1_e-p1_inc)/2+ i/2) + 
      b* (((p1_e-p1_inc)/2 + i/2)*(1/18)*(2+i)^2 + 1/2*((p1_inc-p1_e)/2 + 1 - i/2))
    pi_e = p1_e*((p1_inc-p1_e)/(2) + 1 - i/2) + 
      b*(((p1_e-p1_inc)/(2)+ i/2)*(1/18*((4-i)^2))+1/2*((p1_inc-p1_e)/2+1 -i/2))
    
    
    profits = rbind(profits, data.frame(i, b, pi_inc, pi_e, p1_inc, p1_e))
    
  }
           }
head(profits)
profits$i = as.factor(profits$i)
names(profits)[names(profits) == "b"] <- "beta"
names(profits)[names(profits) == "i"] <- "y"
names(profits)[names(profits) == "pi_inc"] <- "Profit_incumbent"
names(profits)[names(profits) == "pi_e"] <- "Profit_entrant"

#Plot of profits incumbent

plot = ggplot(profits, aes(x=beta, y=Profit_incumbent, col=y)) + 
  geom_line()

png("baseline_profits_incumbent.png", width =700, height = 350)
plot + annotate("text", x = 1, y = 0.8, label = "y=1",
                parse = FALSE) + annotate("text", x = 1, y = 3.5, label = "y=4",
                                          parse = FALSE) +
    ggtitle("Profit of incumbent")
#directlabels::direct.label(plot, "angled.boxes")
dev.off()

#Plot of profits entrant

plot = ggplot(profits, aes(x=beta, y=Profit_entrant, col=y)) + 
  geom_line()
png("baseline_profits_entrant.png", width =700, height = 350)
plot + annotate("text", x = 1, y = -0.05, label = "y=4",
                parse = FALSE) + annotate("text", x = 1, y = 1.1, label = "y=1",
                                          parse = FALSE) +
        ggtitle("Profit of entrant")
#directlabels::direct.label(plot, "angled.boxes")
dev.off()


#Plot of prices incumbent

plot = ggplot(profits, aes(x=beta, y=p1_inc, col=y)) + 
  geom_line()

png("baseline_prices_incumbent.png", width =700, height = 350)
plot + annotate("text", x = 2, y = -0.5, label = "y=4", parse = FALSE) + 
                annotate("text", x = 0, y = 0.9, label = "y=1", parse = FALSE) + 
                annotate("text", x = 2, y = 1.1, label = "y=1", parse = FALSE) + 
  annotate("text", x = 0, y = 2.1, label = "y=4",parse = FALSE) + ggtitle("Price of incumbent") 
#directlabels::direct.label(plot, "angled.boxes")
dev.off()

#Plot of prices entrant

plot = ggplot(profits, aes(x=beta, y=p1_e, col=y)) + 
  geom_line()

png("baseline_prices_entrant.png", width =700, height = 350)
plot + annotate("text", x = 2, y = -1.7, label = "y=4", parse = FALSE) + 
  annotate("text", x = 0, y = 1.1, label = "y=1", parse = FALSE) + 
  annotate("text", x = 2, y = 1.2, label = "y=1", parse = FALSE) + 
  annotate("text", x = 0, y = -0.5, label = "y=4",
           parse = FALSE) + ggtitle("Price of Entrant")
#directlabels::direct.label(plot, "angled.boxes")
dev.off()
