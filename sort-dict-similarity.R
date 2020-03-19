library(tidyverse)
library(tidytext)


dict <- read_csv("elefen_dict.csv")

sim_scores <- 
  dict %>% 
  select(en, word) %>% 
  distinct(en, word) %>% 
  #filter(!str_detect(en, " "), !str_detect(word, " ")) %>% 
  mutate(sim = stringdist::stringdist(en, word)) %>% 
  arrange(sim) %>% 
  select(word, sim) %>% 
  distinct()

dict %>% 
  inner_join(sim_scores) %>% 
  arrange(sim) %>% 
  select(-word_id, -sim) %>% 
  write_csv("dict_similarity_alphabetic.csv")

dict[sample(nrow(dict)),] %>% 
  inner_join(sim_scores) %>% 
  arrange(sim) %>% 
  select(-word_id, -sim) %>% 
  write_csv("dict_similarity_rand.csv")
  
#------- German
sim_scores <- 
  dict %>% 
  select(de, word) %>% 
  distinct(de, word) %>% 
  #filter(!str_detect(en, " "), !str_detect(word, " ")) %>% 
  mutate(sim = stringdist::stringdist(de, word)) %>% 
  arrange(sim) %>% 
  select(word, sim) %>% 
  distinct()

dict %>% 
  inner_join(sim_scores) %>% 
  arrange(sim) %>% 
  select(-word_id, -sim) %>%  
  select(word, gramatica, defini, de, everything()) %>%
  write_csv("dict_similarity_alphabetic_de.csv")

dict[sample(nrow(dict)),] %>% 
  inner_join(sim_scores) %>% 
  arrange(sim) %>% 
  select(-word_id, -sim, everything()) %>% 
  write_csv("dict_similarity_rand_de.csv")
