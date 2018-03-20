library(tidyverse)
library(rvest)
library(stringr)


url <- "http://www.vybory.izbirkom.ru/region/region/izbirkom?action=show&root=1&tvd=100100084849066&vrn=100100084849062&region=0&global=1&sub_region=0&prver=0&pronetvd=null&vibid=100100084849066&type=226"
page <- read_html(url)
url_regions <- page %>% html_nodes(xpath="//select[@name='gs']//option") %>% html_attr("value")
url_regions <- url_regions[-1]

eng_names <- c("voters_in_list", 
               "ballots_received",
               "ballots_issued_early",
               "ballots_issued_polling_st",
               "ballots_issued_outside_polling_st",
               "canceled_ballots",
               "ballots_mobile_boxes",
               "ballots_stationary_boxes",
               "invalid_ballots",
               "valid_ballots",
               "lost_ballots",
               "not_recorded_ballots",
               "Baburin", "Grudinin", "Zhirinovsky", "Putin", "Sobchak", "Suraykin", "Titov", "Yavlinsky")


#elections <- data.frame()

for(i in url_regions[6:length(url_regions)]){
  print(which(url_regions==i))
  page <- read_html(i)
  #Sys.sleep(.5)
  url_regions2 <- page %>% html_nodes(xpath="//form[@name='go_reg']//option") %>% html_attr("value")
  url_regions2 <- url_regions2[-1]
  url_regions2 <- str_replace_all(url_regions2, "region=0&sub_region=0", "region=1&sub_region=1")
  print(str_c(length(url_regions2), " regions"))
  
  for(j in url_regions2){
    print(which(url_regions2==j))
    page <- read_html(j)
    #Sys.sleep(.5)
    url_regions3 <- page %>% html_nodes(xpath="//select[@name='gs']//option") %>% html_attr("value")
    url_regions3 <- url_regions3[-1]
    print(str_c("NEW REGION ", length(url_regions3), " subregions"))
    
    for(l in url_regions3){
      #print(which(url_regions3==l))
      page <- read_html(l, encoding="windows-1251")
      #Sys.sleep(.5)
      if((page %>% html_node(xpath="//table[6]") %>% html_text())!=""){
        tab <- page %>% html_node(xpath="//table[6]") %>% html_table()
        tab <- filter(tab, !is.na(X1)) %>% select(-1)
        tab$X2 <- eng_names
        tab <- tab %>% separate(X3,into = c("value","prop"), sep="\\r\\n",fill="right", convert = T) %>%
          select(-prop) %>% spread(X2,value) %>% select(eng_names)
        reg <- page %>% html_nodes(xpath="//table[3]//tr[1]//a") %>% html_text()
        tab$l1 <- reg[1]
        tab$l2 <- reg[2]
        tab$l3 <- reg[3]
        
        elections <- rbind(elections, tab)
      }
    }
  }
}

write.csv(elections, "elections.csv")


page <- read_html("http://www.cikrf.ru/")
regs <- page %>% html_nodes(xpath = "//*[@id='popup_menu']//select/option") %>% html_attr("value")
regs <- str_replace_all(regs, "http://www.","") %>% str_replace_all(".izbirkom.ru", "")

for(i in regs){
  
  url <- str_c("http://www.", i, ".vybory.izbirkom.ru/region/", i, "?action=ik")
  page <- read_html(url)
  ids <- page %>% html_nodes(xpath = "//li//li//li") %>% html_attr("id")
  
  for(j in ids){
    url2 <- str_c(url, "&vrn=", j)
    page <- read_html(url2, encoding = "utf-8")
    adress <- rbind(adress, data.frame(adress = page %>% html_nodes(xpath = "//span[@id='address_ik']"),
                                       l3 = parse_number( page %>% html_nodes(xpath = "//h2]")),
                                       url = url2))
  }
}

