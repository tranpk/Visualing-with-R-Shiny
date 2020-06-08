library(tidyverse)
library(readxl)

################ Part 3  ################
#City and Rural Population. 1890
a <- 2
b <- 3
theta <- seq(0,13.3*pi,0.01)
r <- a + b*theta
df <- data.frame(x=r*cos(theta), y=r*sin(theta)) # Cartesian coords

ggplot(df[1300:nrow(df),], aes(x,-y)) +
  geom_path(col='red',size=2.5) +
  geom_segment(x=139,y=310,xend=25,yend=410,size=2.5,col="yellow3")+
  geom_segment(x=-75,y=102.5 ,xend=143,yend=311,size=2.5,col="red")+
  geom_segment(x=25,y=406,xend=46.5,yend=435,size=2.5,col="blue")+
  geom_segment(x=50,y=435,xend=-230,yend=435,size=2.5,col="darkgreen")+
  #TEXT
  geom_text(aes(x=-100,y=410,label="78,139 AFRICAN AMERICAN IN CITIES\nOF OVER 10,000 INHABITANTS"),size=3)+
  geom_text(aes(x=135,y=410,label="AFRICAN AMERICANS IN CITIES"),size=3)+
  geom_text(aes(x=100,y=425,label="8,025"),size=3)+
  geom_text(aes(x=0,y=300,label="37,699\nAFRICAN AMERICANS\nIN CITIES\nFROM\n2,500 TO 5,000"),size=3)+
  geom_text(aes(x=0,y=0,label="734,952"),size=3)+
  geom_text(aes(x=-10,y=-129,label="AFRICAN AMERICANS LIVING IN THE COUNTRY AND VILLAGES"),size=3)+
  labs(title="CITY AND RURAL POPULATION.\n1890")+
  theme_void()+
  theme(plot.title=element_text(size=15,hjust=.5),
        panel.background = element_rect(fill = "cornsilk",color = "cornsilk"),
        plot.background = element_rect(fill = "cornsilk"))+
  ylim(-130,500)+
  xlim(-250,250)


# Religion of the African American Population
Religion_of_blacks_who_are_in_Texas <- read_excel("~/Desktop/Soft_Dev/datasets/Religion of blacks who are in Texas.xlsx")
View(Religion_of_blacks_who_are_in_Texas) 

gr <- data.frame(x=c(-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6),
                 y=c(-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6))
ggplot(gr,aes(x,y))+
  geom_segment(aes(x=-2,y=6,xend=-1.25,yend=6),size=5,color="gray")+  #Unaffiliated
  geom_segment(aes(x=-2,y=5,xend=-1.5,yend=5),size=5,color="blue")+  #Non-Christian
  geom_segment(aes(x=-2,y=4,xend=-1,yend=4),size=5,color="red")+ #Catholic
  geom_segment(aes(x=-2,y=3,xend=4,yend=3),size=5,color="darkgreen") + #Protestants
  geom_segment(aes(x=3.885,y=3,xend=3.885,yend=4),size=5,color="darkgreen")+
  geom_segment(aes(x=4,y=4,xend=0,yend=4),size=5,color="darkgreen")+
  geom_segment(aes(x=0,y=3.5,xend=0,yend=4.115),size=5,color="darkgreen")+
  geom_segment(aes(x=-.115,y=3.5,xend=.5,yend=3.5),size=5,color="darkgreen")+  
  geom_segment(aes(x=.5,y=3.5,xend=1.5,yend=3.5),size=5,color="darkgreen")+ 
  geom_segment(aes(x=-2,y=0,xend=-1.25,yend=0),size=5,color="yellow")+ #Evangelical
  geom_segment(aes(x=-2,y=1,xend=1.5,yend=1),size=5,color="yellow")+  #Mainline
  geom_segment(aes(x=-2,y=-1,xend=4,yend=-1),size=5,color="yellow")+  #Historically Black
  geom_segment(aes(x=3.885,y=0,xend=3.885,yend=-1),size=5,color="yellow")+ 
  geom_segment(aes(x=2.5,y=0,xend=4,yend=0),size=5,color="yellow")+
  
  #TEXT
  geom_text(aes(x=-2.5,y=6,label="Unaffiliated"),hjust=1.2)+
  geom_text(aes(x=-1,y=6,label=458),size=3)+  
  
  geom_text(aes(x=-2.5,y=5,label="Non-Christian"),hjust=1.2)+
  geom_text(aes(x=-1.25,y=5,label=124),size=3)+
  
  geom_text(aes(x=-2.5,y=4,label="Catholics"),hjust=1.2)+
  geom_text(aes(x=-.8,y=4,label=250),size=3)+
  
  geom_text(aes(x=-2.5,y=3,label="Protestants"),hjust=1.2)+
  geom_text(aes(x=1.9,y=3.5,label=3329),size=3)+
  
  geom_text(aes(x=-2.2,y=2,label="Protestant Sects:"),hjust=1.2)+
  
  geom_text(aes(x=-2.5,y=0,label="Evangelical"),hjust=1.2)+
  geom_text(aes(x=-1,y=0,label=166),size=3)+
  
  geom_text(aes(x=-2.5,y=1,label="Mainline"),hjust=1.2)+
  geom_text(aes(x=1.75,y=1,label=874),size=3)+
  
  geom_text(aes(x=-2.3,y=-1,label="Historically Black"),hjust=1.2)+
  geom_text(aes(x=2,y=0,label=2164),size=3)+
  
  theme_void()+
  labs(title="Religion of African Americans (in 10,000)")+
  theme(plot.title=element_text(size=15,hjust=.5,face="bold"),panel.background = element_rect(fill = "cornsilk",color = "cornsilk"),
        plot.background = element_rect(fill = "cornsilk"))+
  ylim(-2,6)+
  xlim(-4,4) 

# Assessed Valuation of all taxable property owned by Georgia African Americans
ex.1 <- data.frame(xstart=c(0,0,0,0,0,0),xend=c(1,1,1,1,1,1),
                   ystart=c(0,8,9,12,17,18),yend=c(8,9,12,17,18,19),
                   label=c("A","B","C","D","E","F"))
ggplot()+
  geom_rect(data=ex.1,aes(xmin=xstart,xmax=xend,ymin=ystart,ymax=yend,fill=label))+
  scale_fill_manual(values=c("black","#9b7f6c","#2e4c8c","#f3be17","#d0bda4","#9a142c"))+
  
  #red arrow
  geom_polygon(data=data.frame(x=c(.868,.837,.92),y=c(5,8.1,8.1)),aes(x,y),fill="#9b7f6c")+
  geom_polygon(data=data.frame(x=c(.87,.837,.92),y=c(5.5,10,10)),aes(x,y),fill="#2e4c8c")+
  geom_polygon(data=data.frame(x=c(.872,.84,.91),y=c(5.7,16,16)),aes(x,y),fill="#f3be17")+
  geom_polygon(data=data.frame(x=c(.8725,.845,.905),y=c(6,17.5,17.5)),aes(x,y),fill="#d0bda4")+
  geom_polygon(data=data.frame(x=c(.873,.85,.90),y=c(6,18.5,18.5)),aes(x,y),fill="#9a142c")+
  
  #cornsilk2 arrow
  geom_polygon(data=data.frame(x=c(.100,.07,.159),y=c(5.4,8.1,8.1)),aes(x,y),fill="#9b7f6c")+
  geom_polygon(data=data.frame(x=c(.101,.08,.157),y=c(5.6,10,10)),aes(x,y),fill="#2e4c8c")+
  geom_polygon(data=data.frame(x=c(.102,.09,.155),y=c(5.7,16,16)),aes(x,y),fill="#f3be17")+
  geom_polygon(data=data.frame(x=c(.103,.1,.153),y=c(5.65,17.5,17.5)),aes(x,y),fill="#d0bda4")+
  
  #yellow arrow
  geom_polygon(data=data.frame(x=c(.249,.215,.28),y=c(5.4,8.1,8.1)),aes(x,y),fill="#9b7f6c")+
  geom_polygon(data=data.frame(x=c(.25,.220,.27),y=c(5.6,10,10)),aes(x,y),fill="#2e4c8c")+
  geom_polygon(data=data.frame(x=c(.251,.215,.265),y=c(5.7,16,16)),aes(x,y),fill="#f3be17")+
  
  #blue arrow
  geom_polygon(data=data.frame(x=c(.378,.37,.43),y=c(5.4,8.1,8.1)),aes(x,y),fill="#9b7f6c")+
  geom_polygon(data=data.frame(x=c(.38,.375,.425),y=c(5.5,10,10)),aes(x,y),fill="#2e4c8c")+
  
  #gray arrow
  geom_polygon(data=data.frame(x=c(.625,.595,.65),y=c(5.5,8.1,8.1)),aes(x,y),fill="#9b7f6c")+
  
  #text
  annotate(geom = "text", x = .875, y = 15, label = "$13,447,423", size = 4, angle = -45)+
  annotate(geom = "text", x = .125, y = 14, label = "$12,941,230", size = 4, angle = 45)+
  annotate(geom = "text", x = .245, y = 13, label = "$12,322,003", size = 4)+
  annotate(geom = "text", x = .395, y = 8.5, label = "$8,153,390", size = 2.7, angle = 298,color="white")+
  annotate(geom = "text", x = .625, y = 7.5, label = "$5,764,293", size = 2, angle = 45)+
  
  annotate(geom = "text", x = .5, y = 0, label = "$5,393,885", size = 3.5,color="white")+
  annotate(geom = "text", x = .5, y = 7, label = 1875, size = 3.5,color="white")+
  annotate(geom = "text", x = .5, y = 8.5, label = 1880, size = 3.5)+
  annotate(geom = "text", x = .5, y = 10, label = 1885, size = 3.5,color="white")+
  annotate(geom = "text", x = .5, y = 14, label = 1890, size = 3.5)+
  annotate(geom = "text", x = .5, y = 17.5, label = 1895, size = 3.5)+
  annotate(geom = "text", x = .5, y = 18.5, label = 1899, size = 3.5)+
  
  ggtitle("ASSESSED VALUATION OF ALL TAXABLE PROPERTY\nOWNED BY GEORGIA AFRICAN AMERICANS")+
  ylim(0,19)+
  theme_void()+
  theme(plot.title=element_text(size=12,hjust=.5,face="bold"),
        panel.background = element_rect(fill = "#ecdeca",color = "#ecdeca"),
        plot.background = element_rect(fill = "#ecdeca"),
        legend.position = "none")+
  coord_polar()


################ Part 4  ################
#Conjugal Condition
dbdata1 <- read_excel("dbdata1.xlsx", sheet = "Sheet1")

data <- dbdata1
data$Marital <- factor(data$Marital,levels=c("Single","Married","Widowed"))

ggplot()+
  geom_bar(data=data[data$Gender=="Male",],aes(x=as.factor(Year),y=-Freq,fill=factor(Marital),group=Gender),
           position="fill",stat="identity",width = 1)+
  geom_bar(data=data[data$Gender=="Female",],aes(x=as.factor(Year),y=Freq,fill=factor(Marital),group=Gender),
           position="fill",stat="identity",width = 1)+
  scale_fill_manual(values=c("blue", "red", "darkgreen"))+
  scale_y_continuous(breaks=seq(-1,1,.5), labels=c(100,50,0,50,100))+
  geom_hline(yintercept = 0, size=1, color="black")+
  geom_hline(yintercept = -.5, size=.1, color="black")+
  geom_hline(yintercept = .5, size=.1, color="black")+
  
  geom_vline(xintercept = 1,size=.1, color="black")+
  geom_vline(xintercept = 2,size=.1, color="black")+
  geom_vline(xintercept = 3,size=.1, color="black")+
  geom_vline(xintercept = 4,size=.1, color="black")+
  geom_vline(xintercept = 5,size=.1, color="black")+
  geom_vline(xintercept = 6,size=.1, color="black")+
  
  annotate(geom = "text", x = 4.5, y = -.99, label = "Widowed", size =4, angle = 90)+
  annotate(geom = "text", x = 4.5, y = .98, label = "Widowed", size=4, angle = 90)+

  geom_text(aes(x=3.5,y=0.75,label="Married"),size=5)+
  geom_text(aes(x=3.5,y=-0.75,label="Married"),size=5)+
  
  geom_text(aes(x=2.5,y=-.2,label="Single"),size=5)+
  geom_text(aes(x=2.5,y=.2,label="Single"),size=5)+  
  
  geom_text(aes(x=6.55,y=-.65,label="Male"),size=3.75)+
  geom_text(aes(x=6.55,y=.65,label="Female"),size=3.75)+  
  
  coord_flip()+
  theme(plot.title=element_text(size=14,hjust=.5,face="bold"),
        plot.background = element_rect(fill = "cornsilk"),
        panel.background = element_rect(fill = "cornsilk"),
        legend.position = "none")+
  labs(x = "Year", y = "Percentage", title = "Conjugal Condition of African Americans\nat 15 years old and above")





