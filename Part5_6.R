library(tidyverse)
################ Part 5 & 6  ################
b=data.frame(x=c(-5.5,-3.5,3.5,5.5), 
             y=c(0,5,5,0))
b3 = data.frame(x=c(-3.5,-1,1,3.5), 
                y=c(5,12,12,5))
gr1 = data.frame(x=c(-1,-.65,.65,1), 
                 y=c(12,13,13,12))
r2 = data.frame(x=c(-.65,-.45,.45,.65), 
                y=c(13,13.65,13.65,13))
btop = data.frame(x=c(-.45,0,.45), 
                  y=c(13.65,14.7,13.65))

ggplot() +
  geom_polygon(data=b, mapping=aes(x=x, y=y),col="chocolate4",fill="chocolate4") + 
  geom_polygon(data=b3, mapping=aes(x=x, y=y),col="blue3",fill="blue3") +
  geom_polygon(data=gr1, mapping=aes(x=x, y=y),col="goldenrod1",fill="goldenrod1") +
  geom_polygon(data=r2, mapping=aes(x=x, y=y),col="red2",fill="red2") +
  geom_polygon(data=btop, mapping=aes(x=x, y=y),col="blue3",fill="blue3") +
  
  #White text
  geom_text(aes(x=0,y=14,label=3, color="white"),size=5)+
  geom_text(aes(x=0,y=13.4,label=3, color="white"),size=5)+
  geom_text(aes(x=0,y=12.5,label=11, color="white"),size=5)+
  geom_text(aes(x=0,y=8.5,label="WEEKLY PAPERS\nTOTAL\n136", color="white"),size=5)+
  geom_text(aes(x=0,y=3,label="GRAND TOTAL\n153", color="white"),size=5)+
  scale_colour_manual(values=c("white"))+
  
  #Black Text
  geom_text(aes(x=0,y=15,label="MAGAZINES"),size=5)+
  geom_text(aes(x=.2,y=13.4,label="DAILY                  PAPERS"),size=5)+
  geom_text(aes(x=0,y=12.5,label="SCHOOL                   PAPERS"),size=5)+
  
  labs(title="African American newspapers and periodicals")+
  theme_void()+
  theme(plot.title=element_text(size=15,hjust=.5,face="bold"),
        panel.background = element_rect(fill = "cornsilk",color = "cornsilk"),
        plot.background = element_rect(fill = "cornsilk"),
        legend.position = "none")+
  ylim(0,15)+
  xlim(-6,6) 

#Link for the other book: https://www.amazon.com/W-Boiss-Data-Portraits-Visualizing/dp/1616897066

#https://www.blackprwire.com/our-services/top-150-media
#https://www.w3newspapers.com/usa/africanamerican/
#https://www.w3newspapers.com/magazines/africanamerican/

brown = data.frame(x=c(-5.5,-3.5,3.5,5.5), 
           y=c(0,5,5,0))
blue = data.frame(x=c(-3.5,-1.719,1.719,3.5), 
                  y=c(5,10,10,5))
gold = data.frame(x=c(-1.719,-.645,.645,1.719), 
                  y=c(10,13,13,10))
red = data.frame(x=c(-.645,0,.645), 
                  y=c(13,14.7,13))
ggplot() +
  geom_polygon(data=brown, mapping=aes(x=x, y=y),col="chocolate4",fill="chocolate4") + 
  geom_polygon(data=blue, mapping=aes(x=x, y=y),col="blue3",fill="blue3") +
  geom_polygon(data=gold, mapping=aes(x=x, y=y),col="goldenrod1",fill="goldenrod1") +
  geom_polygon(data=red, mapping=aes(x=x, y=y),col="red",fill="red") +
  
  #White text
  geom_text(aes(x=0,y=13.5,label=6, color="white"),size=5)+
  geom_text(aes(x=0,y=11,label="MAGAZINES\n30", color="white"),size=5)+
  geom_text(aes(x=0,y=8.5,label="NEWSPAPERS\n62", color="white"),size=5)+
  geom_text(aes(x=0,y=3,label="GRAND TOTAL\n98", color="white"),size=5)+
  scale_colour_manual(values=c("white"))+
  
  #Black Text
  geom_text(aes(x=0,y=15.2,label="RADIO\nTV PROGRAMS"),size=3)+
  
  labs(title="African American Current Media Outlets")+
  theme_void()+
  theme(plot.title=element_text(size=15,hjust=.5,face="bold"),
        panel.background = element_rect(fill = "cornsilk",color = "cornsilk"),
        plot.background = element_rect(fill = "cornsilk"),
        legend.position = "none")+
  ylim(0,15.5)+
  xlim(-6,6) 
