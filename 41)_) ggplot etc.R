library(ggplot2)
options(scipen=999)

      d1<-midwest #(dataframe from ggplot2 package)
      
      g1<-ggplot(d1,aes(x=area,y=poptotal))+
            # geom_point(col="green")+
            geom_point(aes(color=state))+
            geom_smooth(method = "lm", se=T, col="firebrick")+
            labs(title = "Area vs Population from Midwest Data in ggplot2 package",
                 y="Population", x="Area",
                 subtitle="lm is smoother",
                 caption = "Source is ggplot2 package")+
            theme(legend.position = "bottom") #lots of options under "theme" - see help/F1
       
      plot(g1)
      
      # gg<-g1
      
#if you want to remove outliers from plot
      gg1<-g1+ xlim(c(0,.1)) + ylim(c(0,500000)) #this removes outlier points
      
      gg<-g1+ coord_cartesian(xlim = c(0,.1),ylim = c(0,500000)) #This keeps outliers but doesn't show them
      
      grid.arrange(gg1,gg,ncol=1,nrow=2)
      

#COLOR CHOICES

    gg+ scale_color_brewer(palette = "Set1") #see palette choices in help
    
    #lots of palettes in  library(RColorBrewer)  brewer.pal.info


#Change axis text
    
    gg1<- gg + scale_x_continuous(breaks = seq(0,.1,.01),
                    labels = sprintf("%1.2f%%",seq(0,.1,.01)))   +
        scale_y_continuous(breaks = seq(0,1000000,200000))
    
    gg1
    
    gg2<- gg1 + theme(plot.title = element_text(size = 15,face = "bold", color = "red",lineheight = 1.2),
                                                axis.title.x=element_text(size = 15),
                                                axis.title.y=element_text(size = 15),
                                                axis.text.x=element_text(size=10,angle = 30),
                                                axis.text.y = element_text(size=10))
    
    gg2
    
#Change themes - this shows what is themes are available
    ?theme_bw
    
#Change theme as follows
    gg2 + theme_light()
    
 
    
    
    
    
       
#Challenge
    
    g1<-ggplot(d1,aes(x=popamerindian,y=popasian))+
      # geom_point(col="green")+
      geom_point(aes(col=state))+
      geom_smooth(method = "lm", se=T, col="firebrick")+
      labs(title = "IndianPop vs AsianPop from Midwest Data in ggplot2 package",
           y="Population Asian", x="Population American Indian",
           subtitle="lm is smoother",
           caption = "Source is ggplot2 package")+
      theme(legend.position = "bottom") #lots of options under "theme" - see help/F1
    
    plot(g1)
    
    gg<-g1
    
    #if you want to remove outliers from plot
    gg<-g1+ xlim(c(0,150)) + ylim(c(0,500)) #this removes outlier points
    
    gga<-g1+ coord_cartesian(xlim = c(0,150),ylim = c(0,500)) #This keeps outliers but doesn't show them
    
    grid.arrange(gg,gga,nrow=2)
    
    
    #COLOR CHOICES
    
    gg<-gg+ scale_color_brewer(palette = "Set1") #see palette choices in help
    
    #lots of palettes in  library(RColorBrewer)  brewer.pal.info
    
    
    #Change axis text
    
    # gg1<- gg + scale_x_continuous(breaks = seq(0,.1,.01),
    #                               labels = sprintf("%1.2f%%",seq(0,.1,.01)))   +
    #   scale_y_continuous(breaks = seq(0,1000000,200000))
    
    # gg1
    
    gg2<- gg + theme(plot.title = element_text(size = 12,face = "bold", color = "red",lineheight = 1.2),
                      axis.title.x=element_text(size = 10),
                      axis.title.y=element_text(size = 10),
                      axis.text.x=element_text(size=10,angle = 30),
                      axis.text.y = element_text(size=10))
    
    gg2
    
    #Change themes - this shows what is themes are available
    ?theme_bw
    
    #Change theme as follows
    gg2 + theme_light()
    
    
    
#LEGENDS
    
    #Start from last lesson creat gg2 plot
    
            g1<-ggplot(d1,aes(x=area,y=poptotal))+
              # geom_point(col="green")+
              geom_point(aes(col=state,size=popdensity))+
              geom_smooth(method = "lm", se=T, col="firebrick")+
              labs(title = "Area vs Population from Midwest Data in ggplot2 package",
                   y="Population", x="Area",
                   subtitle="lm is smoother",
                   caption = "Source is ggplot2 package")+
                  theme(legend.position = "bottom") #lots of options under "theme" - see help/F1
            
            # plot(g1)
            
            gg<-g1
            
            #if you want to remove outliers from plot
            gg<-g1+ xlim(c(0,.1)) + ylim(c(0,1000000)) #this removes outlier points
            
            ggb<-g1+ coord_cartesian(xlim = c(0,.1),ylim = c(0,1000000)) #This keeps outliers but doesn't show them
            
            grid.arrange(gg,ggb,ncol=2)
            
            #COLOR CHOICES
            
            gg+ scale_color_brewer(palette = "Set1") #see palette choices in help
            
            #lots of palettes in  library(RColorBrewer)  brewer.pal.info
            
            
            #Change axis text
            
              gg1<- gg + 
                
              # xlim(c(0,.1)) +
              # 
              # ylim(c(0,1000000)) +
              
              scale_x_continuous(breaks = seq(0,.1,.01),
                  labels = sprintf("%1.2f%%",seq(0,.1,.01)))   +
              
              scale_y_continuous(breaks = seq(0,1000000,200000)) +
                
                coord_cartesian(xlim = c(0,.1),ylim = c(0,1000000))
              
            gg1
            
            gg2<- gg1 + theme(plot.title = element_text(size = 15,face = "bold", color = "red",lineheight = 1.2),
                              axis.title.x=element_text(size = 15),
                              axis.title.y=element_text(size = 15),
                              axis.text.x=element_text(size=10,angle = 30),
                              axis.text.y = element_text(size=10)) +
              
                              coord_cartesian(xlim = c(0,.1),ylim = c(0,1000000))#This keeps outliers but doesn't show them
            
    #Add legend definition
      gg3<- gg2 + scale_color_discrete(name="STATE") +
                  scale_size_continuous(name = "Density")
            
            

            
    #add text to selected points
      # gfselect<-d1[d1$poptotal>800000,]
      d1$largecounty<-ifelse(d1$poptotal>600000,d1$county,"")
      
      gg4 + geom_text(aes(label=largecounty),size=2.5,fontface="bold",data = d1)
      
        
      
  # Fun with legends    
     
      #examples for changing legend position 
        theme(legend.position = "bottom")                                    
        theme(legend.position = "left")
        theme(legend.justification = c(1,0),legend.position = c(1,0))
        
        
        
        
  # Reverse axis
        gg3 + coord_flip()
        
  #Reverse scales:
        gg3 + scale_x_reverse() + scale_y_reverse()
        
  #Add annotation:
        library(grid)
        #add text
        textA<-"This is sample text"
        grobA<-grid.text(textA,x=.7,y=.8,
                         gp=gpar(col="dark red",fontsize=18,fontface="bold"))
        gg4<-gg3 + annotation_custom(grobA) + theme_light()
        gg4
        
        
    #mpg df from ggplot2 package    
      g<-ggplot(mpg,aes(x=displ,y=hwy))+
        geom_point()+
        geom_smooth(method = "lm")+
        theme_bw()
          
    #now make plots for each unique value for the class variable for the above plot
      g+facet_wrap(~class) #all plots have same scale
      
      g+facet_wrap(~class,scales = "free") #allows program to customize optimal size for each plot
      
      g+facet_grid(~class)  #one set of x & y values shared by all plots
      
      g+facet_grid(manufacturer~class) #makes more plots - separates each class plot into 
                                    #a plot for each manufacturer
      
      g+facet_grid(cyl~class) #makes more plots - separates each class plot into 
      #a plot for each cyl size
      
      
      
  #now use jitter to offset points revealing other identical observations
  #that are offset randaomly the width is how far the points are offset
      g+geom_jitter(width = .5)
      
      g+geom_count(color="steelblue")  #similar to size
      
      gg<-ggplot(mpg,aes(manufacturer),fill=hwy) #works only on one non-continuous variable
      gg+ geom_bar(width=.5,fill="steelblue")        #- like a histogram
      
      gg+ geom_bar(width=.5,aes(fill=as.factor(hwy))) #fill with discrete variables
            #width is not binwidth but tells what percentage of the space is filled 
      
      
      gg+ geom_bar(width=.5,aes(fill=class)) +
        scale_fill_brewer(palette = "Spectral") +  #lots of palettes (try Set3)
        theme_bw()
      
      
  #  POSITION="FILL"  I.E.aes(fill=class),position="fill
  #  THIS IS IMPORTANT SINCE IT APPEARS TO NORMALIZE EACH VARIABLE
  #  THUS SHOWING PERCENTAGE OF FILL CHOICE
      gg+ geom_bar(width=.5,aes(fill=class),position="fill") +
      scale_fill_brewer(palette = "Spectral") +  #lots of palettes (try Set3)
      theme_bw()
      
  # For continious variables using Histogram allows you to determine binwidth
      # bar charts don't allow you to change the bin width 
      
      ggg<-ggplot(mpg,aes(cty))
      ggg + geom_bar(width = .8)
      #or
      ggg + geom_histogram(bins = 100)
      #or 
      ggg + geom_histogram(binwidth = 1,aes(fill=class),color="red") +
        scale_x_continuous(breaks = seq(0,50,1))+
        theme(axis.text.x=element_text(size=13,angle = 90,color = "dark green",face = "bold")) #BEST
      
        
  # Play with AGGREGATE function
      mpg_cty<-aggregate(mpg$cty, by=list(mpg$manufacturer),FUN=mean) #returns data.frame
      names(mpg_cty)<-c("Maker","MeanHwyMileage")
      mpg2<-tbl_df(mpg_cty)
      
      g<-ggplot(mpg2,aes(x=Maker,y=MeanHwyMileage,fill=MeanHwyMileage))
      g+geom_bar(stat = "identity")   #if geom_bar has a y variable then you must use stat
      
      #try it with boxplot
      g1<-ggplot(mpg,aes(x=manufacturer,y=cty,color=manufacturer))
      g1 + geom_boxplot(fill="light yellow") + theme_light()
      
      
      #now use fill as an aes
      g1<-ggplot(mpg,aes(x=manufacturer,y=cty))
      g1 + geom_boxplot(aes(fill=class),width=.4) + theme_light()
      
      
      #look at time series
      
      g2<-ggplot(economics,aes(x=date))
      
      g2+ geom_line(aes(y=psavert,color="red")) +
          geom_line(aes(y=uempmed,color="blue"))
      
      #note following changes legend names from red/blue
      #but allows computer to choice colors
      g2+ geom_line(aes(y=psavert,color="Percent with savings")) +
        geom_line(aes(y=uempmed,color="Percent time unemployed"))
      
      #change colors
      g2+ geom_line(aes(y=psavert,color="Percent with savings")) +
        geom_line(aes(y=uempmed,color="Percent time unemployed"))+
        scale_color_manual(name="TitleName",values=c("green","orange")) +
        theme_classic()
      
  #EXTENSIONS:
      #library(ggrepel) - stops text from overlapping in a plot
      
      #library(ggfortify) - gives auto plot capability such as automatically
                  # seeing a time series in a data.frame or autoplot(fit) from lm
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      