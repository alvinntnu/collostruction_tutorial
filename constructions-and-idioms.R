library(tidyverse)
library(tidytext)
library(quanteda)
library(stringr)
library(jiebaR)
library(readtext)

apple_corpus <- readtext("demo_data/applenews10000.tar.gz") %>% corpus

# Initialize the segmenter
segmenter <- worker(user="demo_data/dict-ch-user.txt", 
                    bylines = F, 
                    symbol = T)

# Define own tokenization function
word_seg_text <- function(text, jiebar){
    segment(text, jiebar) %>% # vector output
    str_c(collapse=" ")
}

# From `corpus` to `tibble`
apple_df <- apple_corpus %>%
  tidy %>%
  filter(text !="") %>% #remove empty documents
  mutate(doc_id = row_number()) 

# Tokenization
apple_df <- apple_df %>% # create doccument index
  mutate(text_tag = map_chr(text, word_seg_text, segmenter))



# Define regex
pattern_qilai <- "[^\\s]+\\s起來\\b"

# Extract patterns
apple_df %>%
  select(-text) %>%
  unnest_tokens(output = construction, 
                input = text_tag, 
                token = function(x) str_extract_all(x, pattern=pattern_qilai)) -> apple_qilai

# Print
apple_qilai

# word freq
apple_df %>%
  select(-text) %>%
  unnest_tokens(word,
                text_tag,
                token = function(x) str_split(x, "\\s+|\u3000")) %>%
  filter(nzchar(word)) %>%
  count(word, sort = T) -> apple_word

apple_word %>%
  head(100)

# Joint frequency table
apple_qilai %>%
  count(construction, sort=T) %>%
  tidyr::separate(col="construction",
                  into = c("w1","construction"),
                  sep="\\s") %>%
  mutate(w1_freq = apple_word$n[match(w1,apple_word$word)]) -> apple_qilai_table

apple_qilai_table

# prepare for coll analysis
apple_qilai_table %>%
  select(w1, w1_freq, n) %>%
  write_tsv("qilai.tsv")



# corpus information
cat("Corpus Size: ", sum(apple_word$n), "\n")
cat("Construction Size: ", sum(apple_qilai_table$n), "\n")



# save info in a text
sink("qilai_info.txt")
cat("Corpus Size: ", sum(apple_word$n), "\n")
cat("Construction Size: ", sum(apple_qilai_table$n), "\n")
sink()

## # Create new file
## file.create("qilai_results.txt")



## ####################################
## #       WARNING!!!!!!!!!!!!!!!     #
## # The script re-starts a R session #
## ####################################
## source("http://www.stgries.info/teaching/groningen/coll.analysis.r")
## 



# load the output txt
results <-readLines("demo_data/qilai_results.txt", encoding = "utf-8")
# subset lines 
results<-results[-c(1:17, (length(results)-17):length(results))]
# convert into CSV
collo_table<-read_tsv(results)

# auto-print
collo_table %>%
  filter(relation =="attraction") %>%
  arrange(desc(coll.strength)) %>%
  head(100) %>%
  select(words, coll.strength, everything())

# from wide to long
collo_table %>%
  filter(relation == "attraction") %>%
  filter(obs.freq >=5) %>%
  select(words, obs.freq, 
         delta.p.constr.to.word, 
         delta.p.word.to.constr,
         coll.strength) %>%
  pivot_longer(cols=c("obs.freq", 
                      "delta.p.constr.to.word", 
                      "delta.p.word.to.constr",
                      "coll.strength"),
               names_to = "metric",
               values_to = "strength") %>%
  mutate(metric = factor(metric, 
                         levels = c("obs.freq", 
                                   "delta.p.constr.to.word",
                                   "delta.p.word.to.constr",
                                   "coll.strength"))) %>%
  group_by(metric) %>%
  top_n(10, strength) %>%
  #arrange(strength) %>%
  #mutate(strength_rank = row_number()) %>%
  ungroup %>%
  arrange(metric, desc(strength)) -> coll_table_long

# plot

graphs <- list()
for(i in levels(coll_table_long$metric)){
  coll_table_long %>%
    filter(metric %in% i) %>%
    ggplot(aes(reorder(words, strength), strength, fill=strength)) +
    geom_col(show.legend = F) +
    coord_flip() +
    labs(x = "Collexemes", 
         y = "Strength", 
         title = i)+
    theme(text = element_text(family="Arial Unicode MS"))-> graphs[[i]]
}

require(ggpubr)
ggpubr::ggarrange(plotlist = graphs)



## all_idioms <- readLines(con = "demo_data/dict-ch-idiom.txt")
## head(all_idioms)
## tail(all_idioms)
## length(all_idioms)



## Please conduct a collexeme analysis for the aspectual construction "X + 了" in Chinese.

## 
## Extract all tokens of this consturction from the news corpus and identify all words preceding the aspectual marker.

## 
## Based on the distributional information, conduct the collexemes analysis using the `coll.analysis.r` and present the collexemes that significantly co-occur with the construction "X + 了" in the X slot. Rank the collexemes according to the collostrength provided by Stefan Gries' script.


















## Using the same Chinese news corpus---`demo_data/corpus-news-collection.csv`, please create a frequency list of all four-character words/idioms that are included in the four-character idiom dictionary `demo_data/dict-ch-idiom.txt`.

## 
## Please include both the frequency as well as the dispersion of each four-character idiom in the corpus. Dispersion is defined as the number of articles where it is observed.

## 
## Please arrange the four-character idioms according to their dispersion.




## Let's assume that we are particularly interested in the idioms of the schema of `X_X_`, such as "一心一意", "民脂民膏", "滿坑滿谷" (i.e., idioms where the first character is the same as the third character).

## 
## Please find the top 20 frequent idioms of this schema and visualize their frequencies in a bar plot as shown below.




## Continuing the previous exercise, the idioms of the schema `X_X_` may have different types of `X`. Here we refer to the character `X` as the pivot of the idiom.

## 
## Please identify all the pivots for idioms of this schema which have at least two types of constructional variants in the corpus (i.e., its type frequency >= 2) and visualize their type frequencies as shown below.






## Continuing the previous exercise, to further study the semantic uniqueness of each pivot schema, please identify the top 5 idioms of each pivot schema according to the frequencies of the idioms in the corpus.

## 
## Please present the results for schemas whose type frequencies >= 5 (i.e., the pivot schema has at least FIVE different idioms as its constructional instances).

## 
## Please visualize your results as shown below.




## Let's assume that we are interested in how different media may use the four-character words differently.

## 
## Please show the average number of idioms per article by different media and visualize the results in bar plots as shown below.

## 
## The average number of idioms per article can be computed based on token frequency (i.e., on average how many idioms were observed per article?) or type frequency (i.e., on average how many different idiom types were observed per article?).

