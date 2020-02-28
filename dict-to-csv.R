library(tidyverse)

entries <- read_csv('http://elefen.org/disionario/disionario_completa.txt', 
                    col_names = FALSE)

entries %>% 
  filter(str_detect(X1, "\\.") & str_detect(X1, "--")) %>% 
  View()

entries %>% mutate(word_id = cumsum(str_detect(X1, "\\.") & str_detect(X1, "--"))) %>% 
  group_by(word_id) %>% 
  summarize(word = paste(X1, collapse = "\n")) %>% 
  mutate(gramatica = str_extract(word, "(?<= -- )[A-Za-z]+"), 
         word = str_remove(word, " -- [A-Za-z]+")) %>% 
  mutate(en = str_extract(word, "(?<=\nen: )(.*)(?=\n)"), 
         word = str_remove(word, "(\nen: )(.*)(?=\n)")) %>% 
  mutate(defini = str_extract(word, "(?<=\ndefini: )(.*)(?=\n)"), 
         word = str_remove(word, "(\ndefini: )(.*)(?=\n)")) %>% 
  mutate(nota = str_extract(word, "(?<=\nnota: )(.*)(?=\n)"), 
         word = str_remove(word, "(\nnota: )(.*)(?=\n)")) %>% 
  mutate(usa = str_extract(word, "(?<=\nusa: )(.*)(?=\n)"), 
         word = str_remove(word, "(\nusa: )(.*)(?=\n)")) %>% 
  mutate(an = str_extract(word, "(?<=\nan: )(.*)(?=\n)"), 
         word = str_remove(word, "(\nan: )(.*)(?=\n)")) %>% 
  mutate(da = str_extract(word, "(?<=\nda: )(.*)(?=\n)"), 
         word = str_remove(word, "(\nda: )(.*)(?=\n)")) %>% 
  mutate(de = str_extract(word, "(?<=\nde: )(.*)(?=\n)"), 
         word = str_remove(word, "(\nde: )(.*)(?=\n)")) %>% 
  mutate(eo = str_extract(word, "(?<=\neo: )(.*)(?=\n)"), 
         word = str_remove(word, "(\neo: )(.*)(?=\n)")) %>% 
  mutate(es = str_extract(word, "(?<=\nes: )(.*)(?=\n)"), 
         word = str_remove(word, "(\nes: )(.*)(?=\n)")) %>% 
  mutate(fr = str_extract(word, "(?<=\nfr: )(.*)(?=\n)"), 
         word = str_remove(word, "(\nfr: )(.*)(?=\n)")) %>% 
  mutate(gl = str_extract(word, "(?<=\ngl: )(.*)(?=\n)"), 
         word = str_remove(word, "(\ngl: )(.*)(?=\n)")) %>% 
  mutate(he = str_extract(word, "((?<=\nhe: )(.*)(?=\n))|(he: (.*)$)"), 
         word = str_remove(word, "((\nhe: )(.*)(?=\n))|(he: (.*)$)")) %>% 
  mutate(mt = str_extract(word, "(?<=\nmt: )(.*)(?=\n)"), 
         word = str_remove(word, "(\nmt: )(.*)(?=\n)")) %>% 
  mutate(ru = str_extract(word, "(?<=\nru: )(.*)(?=\n)"), 
         word = str_remove(word, "(\nru: )(.*)(?=\n)")) %>%
  write_csv("elefin_dict.csv")
  
