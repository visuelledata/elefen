library(tidyverse)

entries <- read_csv('http://elefen.org/disionario/disionario_completa.txt', 
                    col_names = FALSE)


entries %>% 
  mutate(word_id = cumsum(str_detect(X1, "\\.") & str_detect(X1, "--"))) %>% 
  group_by(word_id) %>% 
  summarize(word = paste(X1, collapse = "\n")) %>% 
  mutate(word = str_split(word, "\\+")) %>% 
  unnest(word) %>%
  mutate(word = str_trim(word), 
         word = str_remove(word, "(\nimaje: )(.*)(?=\n)")) %>% 
  mutate(gramatica = str_extract(word, "(?<= -- )(.*)(?=\n)"), 
         word = str_remove(word, "( -- )(.*)(?=\n)"), 
         word = str_remove_all(word, "(\ngramatica: )(.*)(?=\n)")) %>% 
  mutate(capital = str_extract(word, "(?<=\ncapital: )(.*)(?=\n)"), 
         word = str_remove(word, "(\ncapital: )(.*)(?=\n)")) %>% 
  mutate(en = str_extract(word, "(?<=\nen: )(.*)(?=\n)"), 
         word = str_remove_all(word, "(\nen: )(.*)(?=\n)")) %>% 
  mutate(defini = str_extract(word, "(?<=\ndefini: )(.*)(?=\n)"),
         word = str_remove_all(word, "(\ndefini: )(.*)(?=\n)")) %>% 
  mutate(defini = if_else(str_detect(word, "(\n\\(\\d\\))(.*)(?=\n)"), 
                          paste0(defini, " - may be incomplete"), 
                          identity(defini)), 
         word = str_remove_all(word, "(\n\\(\\d\\))(.*)(?=\n)")) %>% 
  mutate(nota = str_extract(word, "(?<=\nnota: )(.*)(?=\n)"), 
         word = str_remove(word, "(\nnota: )(.*)(?=\n)")) %>% 
  mutate(vide = str_extract(word, "(?<=\nvide: )(.*)(?=\n)"), 
         word = str_remove(word, "(\nvide: )(.*)(?=\n)")) %>% 
  mutate(orijina = str_extract(word, "(?<=\norijina: )(.*)(?=\n)"), 
         word = str_remove(word, "(\norijina: )(.*)(?=\n)")) %>%
  mutate(tasonomia = str_extract(word, "(?<=\ntasonomia: )(.*)(?=\n)"), 
         word = str_remove(word, "(\ntasonomia: )(.*)(?=\n)")) %>%
  mutate(pronuncia = str_extract(word, "(?<=\npronuncia: )(.*)(?=\n)"), 
         word = str_remove(word, "(\npronuncia: )(.*)(?=\n)")) %>%
  mutate(simbol = str_extract(word, "(?<=\nsimbol: )(.*)(?=\n)"), 
         word = str_remove(word, "(\nsimbol: )(.*)(?=\n)")) %>%
  mutate(usa = str_extract(word, "(?<=\nusa: )(.*)(?=\n)"), 
         word = str_remove_all(word, "(\nusa: )(.*)(?=\n)")) %>% 
  mutate(an = str_extract(word, "(?<=\nan: )(.*)(?=\n)"), 
         word = str_remove(word, "(\nan: )(.*)(?=\n)")) %>% 
  mutate(da = str_extract(word, "(?<=\nda: )(.*)(?=\n)"), 
         word = str_remove(word, "(\nda: )(.*)(?=\n)")) %>% 
  mutate(de = str_extract(word, "(?<=\nde: )(.*)(?=\n)"), 
         word = str_remove_all(word, "(\nde: )(.*)(?=\n)")) %>% 
  mutate(eo = str_extract(word, "(?<=\neo: )(.*)(?=\n)"), 
         word = str_remove_all(word, "(\neo: )(.*)(?=\n)")) %>% 
  mutate(es = str_extract(word, "(?<=\nes: )(.*)(?=\n)"), 
         word = str_remove_all(word, "(\nes: )(.*)(?=\n)")) %>% 
  mutate(fr = str_extract(word, "(?<=\nfr: )(.*)(?=\n)"), 
         word = str_remove_all(word, "(\nfr: )(.*)(?=\n)")) %>% 
  mutate(gl = str_extract(word, "(?<=\ngl: )(.*)(?=\n)"), 
         word = str_remove(word, "(\ngl: )(.*)(?=\n)")) %>% 
  mutate(he = str_extract(word, "((?<=\nhe: )(.*)(?=\n))|(he: (.*)$)"), 
         word = str_remove_all(word, "((\nhe: )(.*)(?=\n))|(he: (.*)$)")) %>% 
  mutate(mt = str_extract(word, "(?<=\nmt: )(.*)(?=\n)"), 
         word = str_remove(word, "(\nmt: )(.*)(?=\n)")) %>% 
  mutate(ru = str_extract(word, "((?<=\nru: )(.*)(?=\n))|(ru: (.*)$)"), 
         word = str_remove(word, "((\nru: )(.*)(?=\n))|(ru: (.*)$)")) %>% 
  mutate(word = str_remove_all(word, "\n[A-Za-z]+:(.*)"),
         word = str_remove(word, "^\\.")) %>% 
  mutate_all(str_trim) %>% 
  select(word_id, word, gramatica, defini, en, eo, es, fr, everything()) %>% 
  write_csv("elefin_dict.csv") %>% 
  View
