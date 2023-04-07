# 00-plot-themes.R
# Custom ggplot themes used in MS_access

#Define gppr_theme() function

theme_gov <- function(){
  theme_bw() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      #panel.grid.major = element_blank(),    #strip major gridlines
      #panel.grid.minor = element_blank(),    #strip minor gridlines
      
      # legend
      legend.position="bottom", 
      
      # text
      axis.text = element_text(size=7), 
      axis.text.x = element_text(size = 7,angle = 90, vjust = 0.5, hjust=1), 
      axis.title= element_text(size=10))
}


theme_gov_dark <- function(){
  theme_gray() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      #panel.grid.major = element_blank(),    #strip major gridlines
      #panel.grid.minor = element_blank(),    #strip minor gridlines
      
      # legend
      legend.position="bottom", 
      
      # text
      axis.text = element_text(size=7), 
      axis.text.x = element_text(size = 7,angle = 90, vjust = 0.5, hjust=1), 
      axis.title= element_text(size=10))
}

