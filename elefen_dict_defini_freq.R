# Basically worthless

library(tidyverse)
library(tidytext)

dict <- read_csv("elefen_dict.csv")
elefen_sentences_tatoeba <- read_csv("elefen_sentences_tatoeba.csv")


words_tatoeba <- 
  unnest_tokens(elefen_sentences_tatoeba, ws, lfn_sentence) %>% 
  select(-eng_sentence)

freq <- dict %>% 
  select(defini) %>% 
  unnest_tokens(ws, defini) %>%
  bind_rows(words_tatoeba) %>% 
  group_by(ws) %>% 
  summarize(freq = n()) %>%
  mutate(freq = freq / sum(freq)) %>% 
  arrange(desc(freq)) %>% 
  filter(!is.na(ws))
  
def_freq <- dict %>% 
  select(defini, word) %>% 
  unnest_tokens(ws, defini, drop = FALSE) %>% 
  inner_join(freq) %>% 
  # filter(ws != "un", ws != "de", ws != "la", ws != "con", 
  #        ws != "en", ws != "a", ws != "o", ws != "e", 
  #        ws != "es", ws != "cual", ws != "person", 
  #        ws != "per") %>% 
  group_by(defini) %>% 
  summarize(score = mean(freq)) %>% 
  arrange(desc(score))

dict %>% 
  full_join(def_freq) %>% 
  arrange(desc(score)) %>% 
  select(word, en, defini) %>% 
  #filter(!is.na(en)) %>% 
  View()
