
fb_data<-read.csv("pseudo_facebook.tsv", sep="\t")
ggplot(data = fb_data, aes(x = dob_day)) + 
  geom_histogram() + 
  scale_x_discrete(breaks = 1:31) + 
  facet_wrap(~dob_month) 

  qplot(x = friend_count, data=subset( fb_data,!is.na(gender), xlim = c(0,1000)))

  qplot(x=friend_count, data= subset( fb_data,!is.na(gender), bin_width = 25)) +
    scale_x_continuous(limits=c(0,1000),breaks = seq(0,1000,50))+
    facet_wrap(~gender)
  
  table(fb_data$gender)
  by(fb_data$friend_count, fb_data$gender, summary)
  names(fb_data)
  subset(pf, !is.na(gender))
  
  qplot(x=tenure, data=fb_data,bin_width = 50,
        color = I('black'), fill = I('#099009'))
  
  qplot(x=age, data=fb_data, bin_width = 1,
        color = I('black'), fill = I('#f79420') ,
        xlab = 'Age of the user',
        ylab= 'Number of users for age on X axis') +
        scale_x_discrete(breaks= seq(0,110,5))
  
  str(fb_data$age)
  
b1<-  qplot(x = friend_count, data = fb_data, binwidth=50,
        color = I('white'),fill = I('#7D3C98'),
         xlab='Friend count of the user',
          ylab='count') +
          scale_x_continuous(breaks = seq(0,1500,100))
  
b2<-  qplot(x = log10(fb_data$friend_count + 1), data = fb_data,
            color = I('white'),fill = I('#C0392B'),
            xlab=' Log 10 transformed Friend count of the user',
            ylab='Value') 

b3<-  qplot(x = sqrt(fb_data$friend_count), data = fb_data,
            color = I('white'),fill = I('#2E86C1'),
            xlab=' Square root transformed Friend count of the user',
            ylab='Value') 

grid.arrange(b1,b2,b3, ncol=1)

qplot(x=friend_count,
      data= subset(fb_data,!is.na(gender)), binwidth = 10, geom = 'freqpoly', color= gender) +
  scale_x_continuous(limits=c(0,1000),breaks = seq(0,1000,50))

qplot(x=www_likes,
      data= subset(fb_data,!is.na(gender)), geom = 'freqpoly', color= gender) +
  scale_x_continuous()+
  scale_x_log10()

by(fb_data$www_likes, fb_data$gender, sum)

qplot(x=gender, y= friend_count, data= subset(fb_data,!is.na(gender)), geom='boxplot')

library(ggplot2)
data(diamonds)
str(diamonds)
str(diamonds$color)

ggplot(data=diamonds, aes(x=diamonds$price),color = I('white'),fill = I('#2E86C1')) +
  geom_histogram(binwidth = 1) + xlim(0,10000) + scale_x_continuous(breaks = seq(0,10000,500))

by(diamonds$price,diamonds$cut,summary,digits = max(getOption('digits')))

summary(diamonds$price)

a<-subset(diamonds,diamonds$price>=15000)

z1<- qplot(price, data= diamonds, binwidth=25) + facet_grid(~cut, scales="free_y")

qplot(x=price, data=diamonds, geom= 'freqpoly', color= cut) + scale_x_continuous() + scale_x_log10()

qplot(x=color, y = price/carat, data= diamonds, geom= 'boxplot', color= color)+
  coord_cartesian(ylim=c(0,7500))

by(diamonds$price,diamonds$color,summary)

qplot(x=carat, data=diamonds, geom= 'freqpoly', binwidth=0.01) + scale_x_continuous(breaks= seq(0,5,0.1))

table(diamonds$carat)


motor_data<-read.csv("motor.csv", header= TRUE, stringsAsFactors = FALSE)
head(motor_data)


