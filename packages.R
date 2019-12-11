list.of.packages <- c('shiny', 'shinydashboard', 'plotly', 'shinythemes',
                      'shinyjs', 'DT', 'tidyr', 'ggplot2', 'maps', 'stringr',
                      'tidyverse', 'reshape2', 'ggrepel', 'plyr', 'tableHTML',
                      'GGally', 'scales', 'dplyr'
                      )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
paste0('Checking dependencies...')
if(length(new.packages)) install.packages(new.packages)
paste0('All dependecies satisfied...')