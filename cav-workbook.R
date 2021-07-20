library(tidyverse)
library(magrittr)
library(tdf)
library(ggpattern)
library(magick)
library(ggimage)
library(ggtext)

rbind(editions$stage_results$`1903`$`stage-1`,
editions$stage_results$`1903`$`stage-2`) %>% View()

map_dfr(editions$stage_results$`1904`,extract)

stages <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/stage_data.csv")

stages %>% filter(rank==1,
                  rider=="Merckx Eddy") %>% summarise(min(year),
                                                      max(year))

stages %>% filter(rank==1,
                  rider=="Hinault Bernard") %>% summarise(min(year),
                                                      max(year))

stages %>% filter(rank==1,
                  rider=="Le Grevès René") %>% summarise(min(year),
                                                          max(year))

stages %>% filter(rank==1,
                  rider=="Merckx Eddy") %>% arrange(desc(age))


stages %>% 
  mutate(stage_results_id = str_remove(stage_results_id,"(?<=[0-9]{2})[a-z]|(?<=[0-9]{1})[a-z]")) %>% 
  filter(rank==1) %>% 
  group_by(edition,year,stage_results_id,rank) %>% slice(1) %>% ungroup() %>%
  filter(rank==1) %>% 
  count(rider,sort = T) %>%
  ggplot(aes(n)) + geom_histogram()

stages %>% filter(rank==1) %>% count(rider,sort = T) %>% slice(1:5) %>% 
  mutate(label = link_to_img(eddies)) %>%
  ggplot(aes(fct_reorder(rider,n),n)) + geom_col(fill="#ffff00",
                                                 width = .5) + coord_flip() + 
  theme(panel.background = element_rect(fill="#fafafa"),
        text = element_text(family = "Open Sans"),
        panel.grid = element_blank()) + 
  geom_richtext(aes(label = label),
                fill = NA,
                label.color = NA,
                size = 1)
  geom_image(aes(image = image), size = .1, by = "width", asp = 1.618)

link_to_img <- function(x, width = 50) {
  glue::glue("<img src='{x}' width='{width}'/>")
}
  
extrafont::fonts()

##### stages
desc_stage <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_stages.csv") %>%
  mutate(Type = case_when(
    str_detect(Type,"(F|f)lat|Plain")== TRUE ~ "Flat Stage",
    str_detect(Type,"Hilly|(M|m)ountain")== TRUE ~ "Mountain Stage",
    str_detect(Type, "trial")== TRUE ~ "Time Trial",
    TRUE ~ as.character(Type)
  ))

head(desc_stage)

twenty_one <- cbind(Stage = c(1,2,3,4),
              Date = c("2021-06-29","2021-07-01","2021-07-06","2021-07-09"),
              Distance = rep(100,4),
              Origin = c("Redon","Tours","Albertville","Nimes"),
              Destination = c("Fougeres","Chateauroux","Valence","Carcassonne"),
              Type = rep("Flat Stage",4),
              Winner = rep("Mark Cavendish",4),
              Winner_Country = c("GBR","GBR","GBR","GBR")) %>% as_tibble() %>%
  mutate(Date = as.Date(Date),
         Distance = as.double(Distance))
  


desc_stage  %>% count(Winner,sort = T) %>% slice(1:5) %>%
  ggplot(aes(fct_reorder(Winner,n),n)) + geom_col(width = .6,
                                                  fill = "yellow") + coord_flip() + 
  scale_y_continuous(breaks = seq(0,35,by=5)) +
  theme(axis.text = element_text(size=12,
                                 family = "Open Sans",
                                 color = "black"),
        panel.background = element_rect(fill="white"),
        axis.ticks.y = element_blank(),
        axis.title = element_text(hjust = 1,
                                  family = "Open Sans",
                                  size = 12)) + 
  labs(x = NULL,
       y = "# of wins")


desc_stage %>%
  count(Type)

desc_stage %>%
  mutate(Type = case_when(
    str_detect(Type,"(F|f)lat|Plain")== TRUE ~ "Flat Stage",
    str_detect(Type,"Hilly|(M|m)ountain")== TRUE ~ "Mountain Stage",
    TRUE ~ as.character(Type)
                          )) %>%
  count(Type)

desc_stage %>% 
  filter(str_detect(Winner,
                    "Cavendish|Hinault|Merckx|André Leducq|André Darrigade")) %>% 
  filter(!str_detect(Winner,"Antonin")) %>%
  group_by(Winner,Type) %>% summarise(wins = n()) %>% ungroup() %>%
  group_by(Winner,Type) %>% summarise(wins = sum(wins)) %>% ungroup() %>%
  group_by(Winner) %>% mutate(perc = wins/sum(wins),
                              total_wins = sum(wins)) %>% ungroup() %>%
  ggplot(aes(Winner,perc,fill=Type)) + geom_col() + coord_flip()


###### length #####

wins_alter <- desc_stage %>% 
  filter(str_detect(Winner,
                    "Cavendish|Hinault|Merckx|André Leducq|André Darrigade"),
         !str_detect(Winner,"Antonin")) %>% 
  select(Winner,Date,Type) %>%
  mutate(year = year(Date)) %>%
  group_by(Winner) %>% mutate(distance = year-min(year)) %>% ungroup() %>% 
  group_by(Winner,year,distance,Type) %>% summarise(num_wins = n()) 


desc_stage %>% 
  filter(str_detect(Winner,
                    "Cavendish|Hinault|Merckx|André Leducq|André Darrigade"),
         !str_detect(Winner,"Antonin")) %>%
  group_by(Winner) %>% summarise(end = max(Date),
                                 start = min(Date)) %>% ungroup() %>%
  mutate(gap = year(end)-year(start),
         beg = 0)%>%
  ggplot(aes(y= Winner,xend=gap,x=beg,yend=Winner))  +
  geom_jitter(width = .1,
              height=.2,
              inherit.aes = F,data = wins_alter,aes(distance,Winner,size=num_wins,fill=Type),
              pch = 21,
              color = "black") + 
  scale_size(range = c(3, 15)) + 
  scale_x_continuous(breaks = seq(0,12,by=1)) + 
  theme(axis.text = element_text(size=12,
                                 family = "Open Sans",
                                 color = "black"),
        panel.background = element_rect(fill="white"),
        axis.ticks = element_blank(),
        axis.title = element_text(hjust = 1,
                                  family = "Open Sans",
                                  size = 12),
        legend.position = "bottom",
        legend.text = element_text(family="Open Sans"),
        legend.title = element_text(family = "Open Sans"),
        panel.grid.major.x = element_line(size = .2,
                                          linetype = "dashed",
                                          color = "black")) +
  scale_fill_manual(values = c("#4ba82e","red","yellow")) +
  labs(x = "# years since first tdf win",
       y = NULL)  +
  guides(size = "none")
  


desc_stage %>% View() 
  filter(str_detect(Winner,
                    "Cavendish|Hinault|Merckx|André Leducq|André Darrigade")) %>% 
  filter(!str_detect(Winner,"Antonin")) %>%
  group_by(Winner,Type) %>% summarise(wins = n()) %>% ungroup() %>%
  group_by(Winner,Type) %>% summarise(wins = sum(wins)) %>% ungroup() %>%
  group_by(Winner) %>% mutate(perc = wins/sum(wins),
                              total_wins = sum(wins)) %>% ungroup() %>%
  ggplot(aes(Winner,perc,fill=Type)) + geom_col_pattern(aes(pattern_density=Type),
                                                        pattern='circle',
                                                        pattern_fill="red",
                                                        pattern_color="red",
                                                        color = "black",
                                                        size=.3) + coord_flip() + 
  theme(axis.text = element_text(size=12,
                                 family = "Open Sans",
                                 color = "black"),
        panel.background = element_rect(fill="white"),
        axis.ticks = element_blank(),
        axis.title = element_text(hjust = 1,
                                  family = "Open Sans",
                                  size = 12),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        legend.text = element_text(family="Open Sans"),
        legend.title = element_text(family = "Open Sans")) +
  scale_fill_manual(values = c("#4ba82e","white","yellow"))+
labs(y = "",
     x = "") + 
  scale_pattern_density_manual(values = c(0,0.5,0))



