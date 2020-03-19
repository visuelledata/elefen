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
  select(de, word, en) %>% 
  #distinct(de, word) %>% 
  #filter(!str_detect(en, " "), !str_detect(word, " ")) %>% 
  mutate(proper_noun = str_detect(en, "[A-Z]"), 
         de = str_to_lower(de), 
         word = str_to_lower(word)) %>% 
  mutate(sim = stringdist::stringdist(de, word)) %>% 
  mutate(de = if_else(proper_noun, str_to_title(de), identity(de)), 
         word = if_else(proper_noun, str_to_title(word), identity(word))) %>% 
  arrange(sim) %>% 
  select(word, sim) %>% 
  distinct()

dict %>% 
  left_join(sim_scores) %>% 
  arrange(sim) %>% 
  select(-word_id, -sim) %>%  
  select(word, gramatica, defini, de, everything()) %>% 
  filter(str_detect(de, "[A-Z]")) %>% 
  write_csv("dict_similarity_alphabetic_de.csv")

dict[sample(nrow(dict)),] %>% 
  left_join(sim_scores) %>% 
  arrange(sim) %>% 
  select(-word_id, -sim, everything()) %>% 
  write_csv("dict_similarity_rand_de.csv")
