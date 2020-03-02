library(tidyverse)
dict <- read_csv("elefen_dict.csv")
freq <- read_csv("elefen_frequency_tatoeba.csv")

freq %>% 
  inner_join(dict, by = c("words" = "word")) %>% 
  arrange(desc(freq)) %>%  
  write_csv("elefen_dict_by_frequency.csv") 


dict %>% 
  filter(str_detect(gramatica, "sufisa|prefisa"), 
         !is.na(defini)) %>% 
  rowwise %>% 
  mutate(english = !is.na(en), 
         random = rdunif(1, 1, 10000)) %>%
  arrange(desc(random)) %>% 
  arrange(desc(english)) %>% 
  select(-random, -english) %>% 
  write_csv("elefen-affixes.csv")
