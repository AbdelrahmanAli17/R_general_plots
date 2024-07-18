
library(dplyr)
library(ggplot2)
library(tidyverse)
library("scales")

dat <- read.csv(file.choose())

#--------------------------------density plot for many features ----------------------------
den_fun <- function(data ,den_x,title_col,fill = "blue", size =10 , angle = 90 , lb,ub){ 
  ggsave(
    ggplot(data ,aes(x=den_x, show.legend = F )) + 
      geom_density( fill = fill, alpha =0.4) + 
      theme_update() + 
      theme(axis.text.x = element_text(size = (size/2)+3, angle = angle),
            axis.text.y = element_text(size = (size/2)+3), 
            , aspect.ratio = 2/3,axis.title=element_text(size= (size/2)+7), 
            panel.background= element_rect(fill = 'white')
            ,panel.grid=element_line(color= 'Gainsboro'),
            axis.line = element_line(color= 'black' , size = 0.5)
      ) +
      labs(x = 'Samples' ) +
      scale_x_continuous(breaks =c(lb,ub) )+
      scale_y_continuous(labels = label_scientific(digits = 3))+
      facet_wrap(vars({{title_col}}), scales = 'free' ,as.table = T )
    ,filename = "plot.png" , width=10, height=10 ,dpi=300) 
}

#test function 
den_fun(dat,dat$value, dat$variable, "pink",10 ,90, .0001, .0002) 


#----------------------------------------------------------
#------------- density plot for single feature ----------------------------------------

den_fun <- function(data ,den_x,title_col,fill = "blue", size =10 , angle = 90 , lb,ub){ 
  ggsave(
    ggplot(data ,aes(x=den_x, show.legend = F )) + 
      geom_density( fill = fill, alpha =0.4) + 
      theme_update() + 
      theme(axis.text.x = element_text(size = (size/2)+3, angle = angle),
            axis.text.y = element_text(size = (size/2)+3), 
            , aspect.ratio = 2/3,plot.title = element_text(20, hjust = 0.5), 
            panel.background= element_rect(fill = 'white')
            ,panel.grid=element_line(color= 'Gainsboro'),
            axis.line = element_line(color= 'black' , size = 0.5)
      ) +
      labs(title = title_col, x = 'Samples' ) +
      scale_x_continuous(breaks =c(lb,ub) )+
      scale_y_continuous(labels = label_scientific(digits = 3))
    ,filename = "plot.png" , width=10, height=10 ,dpi=300) 
}



