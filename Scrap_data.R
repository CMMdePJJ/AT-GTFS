library(rvest)
library(stringr)
#main page
parent_part = 'https://transitfeeds.com/p/auckland-transport/124'

#page number
page_numbers = c(1:5)
##to get all date you want
use_link_all = NULL
for(j in page_numbers) {
  webpage <- read_html(paste0(parent_part, '?p=', j))
  all_list = html_attr(html_nodes(webpage, "a"), "href")
  routes_link = str_extract_all(all_list, "routes", simplify = TRUE)
  use_link = all_list[routes_link == "routes"]
  
  ##to get original file date
  use_link = gsub("/p/auckland-transport/124/", "", use_link)
  use_link = gsub("/routes", "", use_link)
  use_link = use_link[!is.na(as.numeric(use_link))]
  use_link_all = c(use_link_all, use_link)
}



###

##to set fix part link
fix_part = 'https://openmobilitydata-data.s3-us-west-1.amazonaws.com/public/feeds/auckland-transport/124/'
file_part = '/original/routes.txt'
#time_part = '20170512'

###to get all date files
All_data = NULL
for(i in use_link_all) {
  url_parent <- paste0(fix_part, i, file_part)
  ###get data from website 
  txt_data <- read.table(url(url_parent), sep = ",", header = TRUE)
  txt_data$Date = i
  All_data = rbind(All_data, txt_data)
}

route_id_wd <- "~/Desktop/793/gtfs"
setwd(route_id_wd)
write.csv(All_data, "route_id_data_2023.csv", row.names = FALSE) 
