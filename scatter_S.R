library(dplyr)
library(ggplot2)
library(tidyverse)
library("scales")
dat <- read.csv(file.choose())
#--------------------------------point plot for many samples ----------------------------
point_fun <- function(data ,point_x,point_y,title_col ,fill = "blue", width =0.4 , size =10 , angle = 90){ 
  ggsave(
    ggplot(data ,aes(x=point_x, y = point_y , show.legend = F )) + 
      geom_point(stat = 'identity' , fill = fill , width=width ) + 
      theme_update() + 
      theme(axis.text.x = element_text(size = (size/2)+3, angle = angle),
            axis.text.y = element_text(size = (size/2)+3), 
            , aspect.ratio = 2/3,axis.title=element_text(size= (size/2)+7), 
            panel.background= element_rect(fill = 'white')
            ,panel.grid=element_line(color= 'Gainsboro'),
            #panel.border = element_rect(colour = "black", fill=NA, size=1)
            axis.line = element_line(color= 'black' , size = 0.5)
      ) +
      labs(x = 'Accessions' , y = "Values") +
      scale_x_discrete(breaks =point_x )+
      scale_y_continuous(labels = label_scientific(digits = 3))+
      facet_wrap(vars({{title_col}}), scales = 'free' ,as.table = T )
    ,filename = "plot.png" , width=10, height=10 ,dpi=300) 
}

#test function 
#bar_fun(dat,dat$variable,dat$value,dat$sample , "yellow") 


#----------------------------------------------------------
#------------- point plot for single sample ----------------------------------------

point_fun <- function(data ,point_x,point_y,title_col,fill = "blue"  , width =0.4 , size =10 , angle = 90){ 
  ggsave(
    ggplot(data ,aes(x=point_x, y = point_y , show.legend = F)) + 
      geom_point(stat = 'identity' , fill = fill , width=width  ) + 
      theme_update() + 
      theme(axis.text.x = element_text(size = (size/2)+3, angle = angle),
            axis.text.y = element_text(size = (size/2)+3),plot.title = element_text(size = 20, hjust = 0.5) 
            , aspect.ratio = 2/3,axis.title=element_text(size=20), 
            panel.background= element_rect(fill = 'white')
            ,panel.grid=element_line(color= 'Gainsboro'),
            #panel.border = element_rect(colour = "black", fill=NA, size=1)
            axis.line = element_line(color= 'black' , size = 0.5)
      ) +
      labs(title =title_col , y = "Features", size = (size/2)+7 ) +
      scale_x_discrete(breaks =point_x )+
      scale_y_continuous(labels = label_scientific(digits = 3))
    ,filename = "plot.png" , width=10, height=10 ,dpi=300) 
}
