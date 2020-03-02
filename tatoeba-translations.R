library(tidyverse)
library(tidytext)
sentences <- read_delim("C:/Users/chris/Downloads/sentences.tar/sentences.csv", 
                        "\t", escape_double = FALSE, trim_ws = TRUE, 
                        col_names = c("id", "lang", "sentence"))

links <- read_delim("C:/Users/chris/Downloads/links.tar/links.csv", 
                    "\t", escape_double = FALSE, col_names = c("from", "to"), 
                    trim_ws = TRUE)

lfn <- sentences %>% 
  filter(lang == "lfn") %>% 
  rename(lfn_sentence = sentence)

eng <- sentences %>% 
  filter(lang == "eng") %>% 
  rename(eng_sentence = sentence)

translations <- 
  lfn %>% 
  full_join(links, by = c("id" = "from")) %>% 
  full_join(eng, by = c("to" = "id")) %>% 
  filter(!is.na(eng_sentence)) %>% 
  mutate(len = nchar(lfn_sentence)) %>% 
  arrange(len) %>% 
  select(-id, -lang.x, -lang.y, -to) %>% 
  distinct(lfn_sentence, .keep_all = TRUE) %>% 
  filter(str_detect(lfn_sentence, "(\\w+\\s){2,}\\w+"), 
         !str_detect(lfn_sentence, "[А-Яа-я]")) %>% 
  select(-len) %>% 
  write_csv("elefen_sentences.csv")
  
lfn %>% 
  unnest_tokens(words, lfn_sentence) %>% 
  count(words) %>% 
  arrange(desc(n)) %>% 
  filter(!str_detect(words, "[А-Яа-я]")) %>% 
  mutate(freq = n / sum(n)) %>% 
  select(-n) %>% 
  write_csv("elefen_frequency_tatoeba.csv")



