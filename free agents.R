library(tidyverse)
library(rvest)
library(janitor)

get_free_agents<-function(year=2016)
{
  url=paste0("https://www.basketball-reference.com/friv/free_agents.cgi?year=",year)
  a<-url %>% read_html() %>% html_nodes("table") %>% .[[1]] %>% html_table() %>% 
    mutate(WS=as.numeric(WS))
  #get rid of separator rows and players who didn't play in NBA previous season (signed from overseas, so WS=NA)
  #change Nene's name to match stats
  a<-a %>% filter(Player != "Player",!is.na(WS)) %>% 
    mutate(Rk=year,Player=ifelse(Player=="Nenê Hilário","Nenê",Player)) %>% rename(Season=Rk) %>% 
    select(Season,Player,Type,WS,Terms) %>% clean_names()
  return (a)
}

# already done for 2016-2019
# free_agents<-get_free_agents(2016)
# sapply(2017:2019,function(x){
#   new_free<-get_free_agents(x)
#   free_agents<<-rbind(free_agents,new_free)
# })
prev_free_agents=read_csv("https://github.com/sumitrodatta/contract-prediction-2020/blob/master/2016-2019%20Free%20Agents.csv?raw=true")

free_agents<-get_free_agents(2020)

# remove retired players
free_agents<-free_agents %>% filter(terms != "Retired") %>% mutate(contract_yrs=NA,yr_1_salary=NA)
# remove players going to play in different countries and players w/explicitly non-guaranteed first year
no_contract<-free_agents %>% filter(
  str_detect(terms,"China|Greece|Israel|Russia|Spain|Turkey|Italy|Moscow|Germany|France|Australia|Croatia")|
    str_detect(terms,"camp|two-way|Exhibit|2-way"))
free_agents<-anti_join(free_agents,no_contract)
no_contract<-no_contract %>% mutate(contract_yrs=0,yr_1_salary=0)
free_agents<-full_join(free_agents,no_contract)

write_csv(prev_free_agents %>% bind_rows(free_agents),"2016-2020 Free Agents.csv")
#capology not updated so checking combo of spotrac, bball-insiders & bbref
#omari spellman not free agent (traded to nyk from min)
#juwan morgan not free agent
#write_csv(free_agents,"2016-2019 Free Agents.csv")

#add Jr. to Derrick Jones
#add III to Glenn Robinson, Frank Mason
#add II to Gary Payton
#include option years, partial guaranteed years in counting contract years

#wait until after free agent window!!!
url="https://www.spotrac.com/nba/free-agents/"
fa_2021<-url %>% read_html() %>% html_nodes("table") %>% .[[1]] %>% html_table() %>% 
  rename(Player=`Player (202)`) %>% select(Player,Type) %>% mutate(season=2021) %>% 
  separate(Player,into=c('to_discard','player'),sep='\\s{2,100}') %>% select(-to_discard) %>%
  arrange(player) %>% clean_names() %>% 
  mutate(player=case_when(str_detect(player,'Boban')~'Boban Marjanović',
                          str_detect(player,'Cristiano')~'Cristiano Felício',
                          str_detect(player,'Ersan')~'Ersan İlyasova',
                          str_detect(player,'Goran')~'Goran Dragić',
                          str_detect(player,'Ishmael')~'Ish Smith',
                          str_detect(player,'Ennis III')~'James Ennis',
                          str_detect(player,'Louis Will')~'Lou Williams',
                          str_detect(player,'Nicolo')~'Nicolò Melli',
                          str_detect(player,'Otto')~'Otto Porter',
                          str_detect(player,'Patrick Mills')~'Patty Mills',
                          str_detect(player,'Willy Hernan')~'Willy Hernangómez',
                          TRUE~player)) %>%
  mutate(contract_yrs=NA,first_year_percent_of_cap=NA)

write_csv(fa_2021,"Free Agents 2021.csv")
#go into excel and correct names to match bball-ref

salary_cap_hist_url<-"https://basketball.realgm.com/nba/info/salary_cap"
salary_cap_hist<-salary_cap_hist_url %>% read_html() %>% 
  html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "compact", " " ))]') %>% 
  .[[1]] %>% html_table(fill=TRUE)
#add correct column names (ended up as first row)
colnames(salary_cap_hist)<-salary_cap_hist[1,]
salary_cap_hist<-salary_cap_hist[-1,]
#only take year and cap number, parse cap into a number (has dollar sign and commas originally)
salary_cap_hist<-salary_cap_hist %>% select(3:4) %>%
  rename(season=`Luxury Tax`,cap=BAE) %>%
  mutate(season=as.numeric(str_sub(season,start=-4))) %>%
  mutate(cap=parse_number(cap))
write_csv(salary_cap_hist,"Salary Cap History.csv")
