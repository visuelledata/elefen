library(tidyverse)
library(tidytext)
dict <- read_csv("elefen_dict.csv")
freq <- read_csv("elefen_frequency_tatoeba.csv")
affixes <- 
  read_lines("dont-upload.txt") %>% 
  as.tibble() %>% 
  mutate(value = str_split(value, " ")) %>% 
  unnest() %>% 
  filter(str_detect(value, "([A-Za-z]-)|(-[A-Za-z])")) %>% 
  mutate(value = str_remove_all(value, '”|“|:|\\(|\\)|\\.'))


freq %>% 
  inner_join(dict, by = c("words" = "word")) %>% 
  arrange(desc(freq)) %>%  
  write_csv("elefen_dict_by_frequency.csv") 

common_affixes <- affixes %>% 
  inner_join(dict, by = c("value" = "word")) %>% 
  pull(value)

dict %>% 
  filter(str_detect(gramatica, "sufisa|prefisa"), 
         !is.na(defini)) %>% 
  rowwise %>% 
  mutate(english = !is.na(en), 
         random = rdunif(1, 1, 10000), 
         common = word %in% common_affixes) %>%
  arrange(desc(random)) %>% 
  arrange(desc(common)) %>% 
  arrange(desc(english)) %>% 
  filter(word != en) %>% 
  select(-random, -english) %>% 
  write_csv("elefen-affixes.csv") %>% View()
