library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl)

pop <- read_excel("dbdata.xlsx")

shinyApp(
  ########################################################################
  ########################################################################
  ###############
  ###############  UI
  ###############
  ########################################################################
  ########################################################################
  ui = tagList(
    navbarPage(
      theme =shinytheme("darkly"),  # <--- To use a theme, uncomment this
      "Visualizing with R Shiny",
      tabPanel("Introduction", 
               sidebarPanel(tags$img(src="DB.jpg"),
                            h5("W.E.B. Du Bois"),
                            br()
               ),
               tags$strong("About this project"),
               br(),
               tags$p("In 1900, William Edward Burghardt Du Bois and his team created visualizations of the African-American population and their conditions
                with respect to the United States and the state of Georgia, just thirty-seven years after slavery had ended in the United States, 
                for their exhibit in Paris.  The reason they chose to include Georgia in their visualizations was because Georgia had the highest 
                population of African Americans.  The hope of this exhibit was to illuminate the story of the African American people, who have 
                endured the struggles of slavery and racial discrimination and have persevered for hundreds of years, to the whole world and spark 
                a cultural movement within the United States."), 
               br(),
               tags$p("Nowadays, there is an important emphasis on data: personal data must be protected and be kept private, news articles report how 
                social media have collected data from its users without their permission or awareness, machine learning can use data to predict 
                future events, etc.  Not only is data useful, but also their visualizations.  By correctly analyzing and manipulating the data, 
                they can be used to create effective visualizations that illustrate a story to the reader that would have difficult to do if the 
                data was explained with just text."),
               br(),
               tags$p("The United States is nicknamed the “Land of Many Opportunities” and is called home to people of different races and ethnicities.  
                      As a child from an immigrant family and being considered a minority in the United States population, I can feel at home but I can 
                      also feel like an outsider to this country, even though I was born on this soil.  Personally, I am interested in the growth of 
                      other minorities, particularly the African-American population, and want to see if how much the race the change over the past 
                      one-hundred and twenty years.  Additionally, I want to be able to recreate the visualizations that William Edward Burghardt Du 
                      Bois and his team created for his exhibit in Paris using R-Studio."),
               br(),br(),br(),
               tags$strong("Sources"),
               h6("Jason Forrest"),
               tags$a("Part 1", 
                      href="https://medium.com/nightingale/w-e-b-du-bois-staggering-data-visualizations-are-as-powerful-today-as-they-were-in-1900-64752c472ae4"),
               br(),
               tags$a("Part 2", 
                      href="https://medium.com/nightingale/data-journalism-in-the-study-of-w-e-b-du-bois-the-negro-problem-part-2-of-4-e5ea9b976adc"),
               br(),
               tags$a("Part 3", 
                      href="https://medium.com/nightingale/exploring-the-craft-and-design-of-w-e-b-du-bois-data-visualizations-part-3-b110d034fd36"),
               br(),
               tags$a("Part 4", 
                      href="https://medium.com/nightingale/style-and-rich-detail-on-viewing-an-original-w-e-b-du-bois-data-visualization-part-4-40cc7bd18cfb"),
               br(),
               tags$a("Part 5", 
                      href="https://medium.com/nightingale/the-legacy-of-w-e-b-du-bois-the-exhibit-of-american-negroes-part-5-6b735a426c68"),
               br(),br(),
               h6("Data"),
               tags$a("Media", 
                      href="https://www.blackprwire.com/our-services/top-150-media"), br(),
               tags$a("National Center for Education Statistics", 
                      href="https://nces.ed.gov/programs/digest/d13/tables/dt13_209.10.asp"),br(),
               tags$a("Newspapers", 
                      href="https://www.w3newspapers.com/usa/africanamerican/"),br(),
               tags$a("Religion",
                      href="https://www.pewforum.org/religious-landscape-study/state/texas/racial-and-ethnic-composition/black/"),br(),
               tags$a("United States Bureau of Labor Statistics", 
                      href="https://www.bls.gov/opub/ted/2012/ted_20121026.htm"),br(),
               tags$a("United States Census Bureau", 
                      href="https://www.census.gov/programs-surveys/acs/guidance/subjects.html")
               
               
               ),
      tabPanel("Du Bois' data visualizations with current data",
               sidebarPanel(tags$strong("Population"),
                            tags$p("In gathering data from the US Census Bureau site from the years 2010-2018, I was able to
                                   recreate the visualizations.  However, Du Bois' visualizations concerning population had 
                                   data that spanned over 100 years.  As a result, you can see a significant trend in his 
                                   visualizations compared to mine."),
                            br(),
                            tags$strong("Education"),
                            tags$p("To recreate these visualizations with current data, I used the National Center for 
                                   Education Statistics in addition to the US Census Bureau.  Out of all of these graphs, the 
                                   most surprising visualizations for me was the 'Proportion of Total African American Children of School 
                                   Age Enrolled in Public Schools' because the proportion of children enrolled in 2018 is very low."),
                            br(),
                            tags$strong("Religion"),
                            tags$p("I modeled the religion of total African American population after the percentages of the 
                                   religious composition of African Americans who are in Texas. As you can see there is a higher
                                   percentage of Christianity for the race. Mainline Protestants are mainstream while the evangelical
                                   Protestants are conservative denominations.  Both include the Baptists, Presbyterian, and Methodist sects."),
                            br(),
                            tags$strong("Conjugal Condition"),
                            tags$p("This recreation is different from Du Bois' visualizations as you can see in the graph because I
                                   was unable to find data of marital status of African Americans based on their age period.  I could 
                                   only find data based on the time periods of those who are fifteen years old and above.  It is 
                                   interesting to note that as the time passes, there is a growing trend of single.  Additionally, there 
                                   are more widows females than there are males."),
                            br(),
                            tags$strong("Media"),
                            tags$p("Although our world is becoming more digital, there are still weekly newspapers and magazines being 
                                   printed out. Additionally, I added African American radio programs and tv channels for this 
                                   visualization.  School papers and daily papers could not included since I could not find any data for
                                   these categories.")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Population",
                            tags$strong("Increase of the African American population in the United States of America"), br(),
                            plotOutput("PopIncAA"),
                            br(),
                            tags$img(src="DB1_1.png",height=525, width = 435),
                            br(),br(),br(),
                            tags$strong("Comparative Rate of Increase"), br(),
                            plotOutput("CRInc"),
                            br(),
                            tags$img(src="DB1_2.png",height=525, width = 435),
                            br(),br(),br(),
                            tags$strong("Proportion of African Americans in the Total Population of the United States"), br(),
                            tags$img(src="Part1_3.png",height=325, width = 535), br(),
                            tags$img(src="DB1_3.png",height=525, width = 435)
                            
                 ),
                   tabPanel("Education", 
                            tags$strong("African-American Children enrolled in public schools"),
                            br(),
                            plotOutput("AA_enrolledPS"),br(),
                            tags$img(src="DB2_1.png",height=525, width = 435),
                            br(),br(),br(),
                            tags$strong("Proportion of Total African American Children of School Age Enrolled in Public Schools"), br(),
                            plotOutput("PropTAA"), br(),
                            tags$img(src="DB2_2.png",height=525, width = 435),br(), br(),
                            plotOutput("PropTAA_exact"),
                            br(),br(),br(),
                            tags$strong("African American Teachers in the United States"), br(),
                            tags$img(src="Part2_3.png",height=525, width = 435),br(),br(),
                            tags$img(src="DB2_3.png",height=525, width = 435)
                  ),
                   tabPanel("Religion",
                            tags$img(src="Part3_2.png",height=525, width = 465),br(),br(),
                            tags$img(src="DB3_2.png",height=525, width = 435)
                  ),
                   tabPanel("Conjugal Condition", 
                            tags$img(src="Part4_1.png",height=425, width = 505),br(),br(),
                            tags$img(src="DB4_1.png",height=525, width = 435)
                  ),
                   tabPanel("Media",
                            tags$img(src="Part5_1.png",height=525, width = 435),br(),br(),
                            tags$img(src="DB5_1.png",height=525, width = 385)
                  )
                 )
               )
      ),
      tabPanel("Du Bois in ggplot2",
               sidebarPanel(
                 tags$strong("Description"),
                 tags$p("In recreating Du Bois' visualizations, one of the main challenges is finding current data.  
                        Since I could not find any datasets on  city and rural populations, assessed valuation of taxable property, 
                        and courses of study based on race for the African American population, I chose to just recreate the visualizations,
                        without using current data.  That way, we can compare Du Bois' visualizations and see what it would look like if he 
                        tried to use ggplot2 in R.")
                               
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("City and Rural Population",
                            tags$img(src="Rec3_1.png",height=575, width=400),
                            tags$img(src="RecDB3_1.png",height=525, width=430)
                   ),
                   tabPanel("Assessed Valuation", 
                            tags$img(src="Rec3_3_2.png",height=455, width=440), 
                            tags$img(src="RecDB3_3.png",height=480, width=415)
                   ),
                   tabPanel("Academic Courses",
                            tags$img(src="Rec2_4.png",height=525, width = 475),br(),br(),
                            tags$img(src="DB2_4.png",height=525, width = 435)
                            
                  )
                 )
               )
      )
      
      )
    
  ),
  ########################################################################
  ########################################################################
  ###############
  ###############  SERVER
  ###############
  ########################################################################
  ########################################################################
  server = function(input, output) {
    output$PopIncAA <- renderPlot({
      bl_pop <- pop %>% filter(Total == "Black or African American alone") 
      ggplot(data = bl_pop, aes(x = reorder(Year, -Estimate), y = Estimate,width=.25)) + 
        geom_bar(stat="identity",color = "darkred", fill="darkred") + 
        theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), panel.background = element_rect(fill = "cornsilk",colour = "cornsilk"),
              plot.background = element_rect(fill = "cornsilk"),axis.title.x = element_blank(),
              axis.title.y = element_blank(),axis.ticks = element_blank(), 
              axis.text.x=element_blank(),axis.text.y= element_text(color = "black", face = "bold")) + 
        coord_flip() +
        scale_x_discrete(labels=c("2018 - 41,617,764", "2017 - 41,393,491", "2016 - 40,893,369",  
                                  "2015 - 40,695,277",  "2014 - 40,379,066", "2013 - 39,919,371", 
                                  "2012 - 39,623,138",  "2011 - 39,189,528","2010 - 38,874,625"))  + 
        ggtitle("Increase of the African American population in the United States of America")
    })
    output$CRInc <- renderPlot({
      w_pop <- pop %>% filter(Total == "White alone") %>% rename("White"="Estimate") 
      w_pop$Total <- NULL
      b_pop <- bl_pop %>% rename("Black"="Estimate")
      b_pop$Total <- NULL
      wb_pop <- merge(w_pop,b_pop,by="Year")
      
      #Divide Population by 10^6 (million)
      wb_pop$Black <- as.numeric(as.character(wb_pop$Black)) / 1000000
      wb_pop$White <- as.numeric(as.character(wb_pop$White)) / 1000000
      
      ggplot(wb_pop,aes(x= Year)) + 
        geom_line(aes(y = Black, color = "darkred"), size =3) + 
        geom_line(aes(y = White, color = "darkgreen"),size =3) +
        scale_color_manual(values = c("darkgreen","darkred"),name  ="Race",labels=c("White","Black")) +
        theme_bw() + 
        theme(panel.border = element_blank(), panel.background = element_rect(fill = "cornsilk",color = "cornsilk"),
              plot.background = element_rect(fill = "cornsilk"),legend.background = element_rect(fill="cornsilk"),
              legend.position=c(.5,.5), axis.title.x = element_blank(),axis.title.y = element_blank(),
              plot.title = element_text(size = 10,face = "bold"),axis.text.y= element_text(color = "black", face = "bold"),
              axis.text.x= element_text(color = "black", face = "bold")) + 
        scale_y_continuous(limits = c(25,250), breaks=seq(25,250,25)) +
        scale_x_discrete(limits = c(2010,2011,2012,2013,2014,2015,2016,2017,2018)) + 
        ggtitle("Comparative Growth Rate of the United States White and African American populations (in millions)")
    })
    output$AA_enrolledPS <- renderPlot({
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
    })
    output$PropTAA <- renderPlot({
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
    })
    output$PropTAA_exact <-renderPlot({
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
    })
  }
)

