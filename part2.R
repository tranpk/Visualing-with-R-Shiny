library(tidyverse)
library(readxl)

################ Part 2 ################

##African-American Children enrolled in public schools
Bl_enroll <- read_excel("dbdata.xlsx", sheet = "SCHOOL ENROLLMENT BY DETAILED L")
grid <- data.frame(x=c(-1,0,1,2,3,4,5,6,7,8,9,10,11,12),
                   y=c(-1,0,1,2,3,4,5,6,7,8,9,10,11,12))
ggplot(grid,aes(x,y))+
  geom_segment(aes(x=0,y=0,xend=0,yend=.02),size=8,color="darkgreen")+ #1860
  geom_segment(aes(x=1,y=0,xend=1,yend=.65),size=8,color="darkgreen")+ #1870
  geom_segment(aes(x=2,y=0,xend=2,yend=4.55),size=8,color="darkgreen")+ #1878
  geom_segment(aes(x=3,y=0,xend=3,yend=7.15),size=8,color="darkgreen")+ #1884
  geom_segment(aes(x=4,y=0,xend=4,yend=7.8),size=8,color="darkgreen")+  #1888
  geom_segment(aes(x=5,y=0,xend=5,yend=10.4),size=8,color="darkgreen")+ #1891
  geom_segment(aes(x=6,y=0,xend=6,yend=11.7),size=8,color="darkgreen")+ #1897
  geom_segment(aes(x=7,y=0,xend=7,yend=729),size=8,color="darkgreen") + #2018
  
  #TEXT
  geom_text(aes(x=0.25,y=-25,label="1860"),hjust=1.2)+
  geom_text(aes(x=1.25,y=-25,label="1870"),hjust=1.2)+
  geom_text(aes(x=2.25,y=-25,label="1878"),hjust=1.2)+
  geom_text(aes(x=3.25,y=-25,label="1884"),hjust=1.2)+
  geom_text(aes(x=4.25,y=-25,label="1888"),hjust=1.2)+
  geom_text(aes(x=5.25,y=-25,label="1891"),hjust=1.2)+
  geom_text(aes(x=6.25,y=-25,label="1897"),hjust=1.2)+
  geom_text(aes(x=7.25,y=-25,label="2018"),hjust=1.2)+
  
  #TEXT2
  geom_text(aes(x=0.05,y=10,label="7"),hjust=1.2)+
  geom_text(aes(x=1.35,y=15,label="10,351"),hjust=1.2)+
  geom_text(aes(x=2.35,y=20,label="72,655"),hjust=1.2)+
  geom_text(aes(x=3.35,y=25,label="110,150"),hjust=1.2)+
  geom_text(aes(x=4.35,y=30,label="120,533"),hjust=1.2)+
  geom_text(aes(x=5.35,y=33,label="156,836"),hjust=1.2)+
  geom_text(aes(x=6.35,y=40,label="180,565"),hjust=1.2)+
  geom_text(aes(x=7.65,y=738,label="11,632,324"),hjust=1.2)+
  
  labs(title="AFRICAN AMERICAN CHILDREN ENROLLED\nIN THE PUBLIC SCHOOLS")+
  theme_void()+
  theme(plot.title=element_text(size=15,hjust=.5),
        panel.background = element_rect(fill = "cornsilk",color = "cornsilk"),
        plot.background = element_rect(fill = "cornsilk"))+
  scale_y_reverse()+
  ylim(740,-100)+
  xlim(-.5,8) 

##Proportion of Total African American Children of School Age Enrolled in Public Schools
year <- c("1876","1876","1886","1886","1896","1896","2018","2018")
value <- c(37.59,100-37.59,56.66,100-56.66,57.29,100-57.29,29.04,100-29.04)
status <- rep(c("enrolled" , "not enrolled") , 4)
dt <- data.frame(year, status, value)

# Stacked bar plot
ggplot(dt, aes(x=year, y=value, fill=status)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values=c("red","black"))+
  geom_text(aes(x="1876",y=.75,label="37.59%"))+
  geom_text(aes(x="1886",y=.75,label="56.66%"))+
  geom_text(aes(x="1896",y=.75,label="57.29%"))+
  geom_text(aes(x="2018",y=.75,label="29.04%"))+
  geom_text(aes(x="1876",y=1.02,label="1876"))+
  geom_text(aes(x="1886",y=1.02,label="1886"))+
  geom_text(aes(x="1896",y=1.02,label="1896"))+
  geom_text(aes(x="2018",y=1.02,label="2018"))+
  theme_void()+
  labs(title="Proportion of Total African American Children of School Age\nEnrolled in Public Schools")+
  theme(plot.title=element_text(size=15,hjust=.5,face="bold"),
        panel.background = element_rect(fill = "cornsilk",color = "cornsilk"),
        plot.background = element_rect(fill = "cornsilk"),
        legend.position = "bottom")

##Recreation Proportion of Total African American Children of School Age Enrolled in Public Schools
grid <- data.frame(x=c(0,1,2,3,4,5,6,7,8,9,10,11),
                   y=c(0,-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11))
ggplot(grid,aes(x,y))+
  geom_segment(aes(x=0,y=0,xend=0,yend=-2.5),size=33,color="red")+ #1876r
  geom_segment(aes(x=0,y=-2.5,xend=0,yend=-6.25),size=33,color="black")+ #1876b
  geom_segment(aes(x=1,y=0,xend=1,yend=-4.25),size=33,color="red")+ #1886r
  geom_segment(aes(x=1,y=-4.25,xend=1,yend=-7.7),size=33,color="black")+ #1886b
  geom_segment(aes(x=2,y=0,xend=2,yend=-5.7),size=33,color="red")+  #1896r
  geom_segment(aes(x=2,y=-5.7,xend=2,yend=-10),size=33,color="black")+ #1896b
  geom_segment(aes(x=3,y=0,xend=3,yend=-2.9),size=33,color="red")+ #2018r
  geom_segment(aes(x=3,y=-2.9,xend=3,yend=-10),size=33,color="black") + #2018b
  
  geom_text(aes(x=0,y=.15,label="1876"))+
  geom_text(aes(x=1,y=.15,label="1886"))+
  geom_text(aes(x=2,y=.15,label="1896"))+
  geom_text(aes(x=3,y=.15,label="2018"))+

  geom_text(aes(x=0,y=-1.3,label="37.59%"),size=4.5)+
  geom_text(aes(x=1,y=-2.3,label="56.66%"),size=4.5)+
  geom_text(aes(x=2,y=-3,label="57.29%"),size=4.5)+
  geom_text(aes(x=3,y=-1.5,label="29.04%"),size=4.5)+
  
  geom_text(aes(x=.25,y=-8.1,label="PROPORTION         CHILDREN ENROLLED"),size=3)+
  geom_point(aes(x=.1,y=-8.1), shape=15, fill="red", color="red", size=3) +
  geom_text(aes(x=.35,y=-9,label="PROPORTION         CHILDREN NOT ENROLLED"),size=3)+
  geom_point(aes(x=.1 ,y=-9), shape=15, fill="black", color="black", size=3) +

  theme_void()+
  labs(title="Proportion of Total African American Children of School Age\nEnrolled in Public Schools")+
  theme(plot.title=element_text(size=15,hjust=.5,face="bold"),
        panel.background = element_rect(fill = "cornsilk",color = "cornsilk"),
        plot.background = element_rect(fill = "cornsilk"),
        legend.position = "bottom") +
  ylim(-11,.5)+
  xlim(-.5,3.5) 
 

## African American Teachers in the United States
grid2 <- data.frame(x=c(-15-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,
                        0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
                    y=c(-15-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,
                        0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
ggplot(grid2,aes(x,y))+
  geom_point(x=0,y=30,size = 40*0.017, color="black")+ #3316-1897
  geom_point(x=0,y=20,size = 40, color="cornsilk2")+ #191000-1988
  geom_point(x=0,y=-2,size = 40*1.11, color="yellow")+ #212000-1991
  geom_point(x=0,y=-25.5,size = 40*1.19, color="red")+ #228000-2000
  geom_point(x=0,y=-50,size = 40*1.21, color="navy")+ #231000-2012
  #TEXT
  geom_text(aes(x=1,y=30,label="1897   3.3 (in Georgia)"))+
  geom_text(aes(x=0,y=20,label=191))+
  geom_text(aes(x=0,y=10,label=1988))+
  geom_text(aes(x=0,y=-1,label=212),color="gray")+
  geom_text(aes(x=0,y=-13,label=1991))+
  geom_text(aes(x=0,y=-26,label=228),color="yellow")+
  geom_text(aes(x=0,y=-37,label=2000))+
  geom_text(aes(x=0,y=-50,label=231),color="white")+
  geom_text(aes(x=0,y=-63,label=2012))+
  
  
  theme_void()+
  labs(title="Number of African American Teachers (in thousands) \nin the United States")+
  theme(plot.title=element_text(size=15,hjust=.5,face="bold"),panel.background = element_rect(fill = "cornsilk",color = "cornsilk"),
        plot.background = element_rect(fill = "cornsilk"),
        legend.position = "none")+
  ylim(-63,31)+
  xlim(-5,5) 


##NUMBER OF AFRICAN AMERICAN STUDENTS TAKING\nTHE VARIOUS COURSES OF STUDY\nOFFERED IN GEORGIA SCHOOLS"
test <- data.frame(x=c(-4,-3,-2,-1,0,1,2,3,4),
                   y=c(-4,-3,-2,-1,0,1,2,3,4))
ggplot(test,aes(x,y))+
  geom_segment(aes(x=-2,y=4,xend=-1.9,yend=4),size=5,color="tan")+ #BUSINESS
  geom_segment(aes(x=-2,y=3,xend=-1,yend=3),size=5,color="tan") + #CLASSICAL
  geom_segment(aes(x=-2,y=2,xend=-0.55,yend=2),size=5,color="tan") + #PROFESSIONAL
  geom_segment(aes(x=-2,y=1,xend=-0.3,yend=1),size=5,color="tan")+ #SCIENTIFIC
  geom_segment(aes(x=-2,y=0,xend=1.2,yend=0),size=5,color="tan")+  #NORMAL 
  #INDUSTRIAL
  geom_segment(aes(x=-2,y=-1,xend=3.8,yend=-1),size=5,color="tan")+
  geom_curve(aes(x=3.975,y=-1.25,xend=3.75,yend=-1),size=5,color="tan")+
  geom_curve(aes(x=3.75,y=-1.5,xend=3.975,yend=-1.25),size=5,color="tan")+
  geom_segment(aes(x=-3.8,y=-1.5,xend=3.75,yend=-1.5),size=5,color="tan")+
  geom_curve(aes(x=-4,y=-1.75,xend=-3.75,yend=-2),size=5,color="tan")+
  geom_curve(aes(x=-3.75,y=-1.5,xend=-4,yend=-1.75),size=5,color="tan")+
  geom_segment(aes(x=-3.8,y=-2,xend=-.5,yend=-2),size=5,color="tan")+
  #TEXT
  geom_text(aes(x=-3,y=4,label="BUSINESS"),hjust=1.2)+
  geom_text(aes(x=-2.7,y=4,label=12),hjust=-1.2)+
  
  geom_text(aes(x=-2.9,y=3,label="CLASSICAL"),hjust=1.2)+
  geom_text(aes(x=-2.7,y=3,label=98),hjust=-1.2)+
  
  geom_text(aes(x=-2.5,y=2,label="PROFESSIONAL"),hjust=1.2)+
  geom_text(aes(x=-2.9,y=2,label=152),hjust=-1.2)+
  
  geom_text(aes(x=-2.9,y=1,label="SCIENTIFIC"),hjust=1.2)+
  geom_text(aes(x=-2.9,y=1,label=161),hjust=-1.2)+
  
  geom_text(aes(x=-3.1,y=0,label="NORMAL"),hjust=1.2)+
  geom_text(aes(x=-2.9,y=0,label=383),hjust=-1.2)+
  
  geom_text(aes(x=-2.8,y=-1,label="INDUSTRIAL"),hjust=1.2)+
  geom_text(aes(x=-3,y=-1,label=2252),hjust=-1.2)+
  
  theme_void()+
  labs(title="Number of African American Students taking\nthe various Courses of Study\nOffered in Georgia Schools")+
  theme(plot.title=element_text(size=15,hjust=.5,face="bold"),panel.background = element_rect(fill = "cornsilk",color = "cornsilk"),
        plot.background = element_rect(fill = "cornsilk"))+
  ylim(-2,4)+
  xlim(-4,4) 

