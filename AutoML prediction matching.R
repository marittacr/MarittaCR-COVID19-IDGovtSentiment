library(readr)
library(jsonlite)
library(stringr)
library(tidyr)
library(dplyr)

tweets2 <- read_csv("cleaned_tweets_130421.csv")

#Cleaned tweet b.1
ls_json <- list.files("cleaned_tweet/prediction/",pattern = ".jsonl",full.names = T)
ls_json <- lapply(ls_json,read_lines)
ls_json <- unlist(ls_json)
prediction <- lapply(ls_json,fromJSON)
content <- lapply(prediction,"[[","instance")
content <- lapply(content,"[[","content")
sentiment <- lapply(prediction,"[[","prediction")
sentiment <- lapply(sentiment,"[[","sentiment")
prediction <- tibble(content = unlist(content),
                     sentiment = unlist(sentiment))

prediction <- prediction %>% 
  mutate(file = str_sub(content,-9))

ls_source <- list.files("cleaned_tweet/",pattern = ".txt",full.names = T)

source_cleaned_tweet1 <- lapply(ls_source,read_file)
source_cleaned_tweet1 <- tibble(data.text=unlist(source_cleaned_tweet1),file = str_sub(ls_source,-9))
source_cleaned_tweet1 <- left_join(source_cleaned_tweet1,prediction,by="file") %>%  
  drop_na() %>% 
  select(data.text,sentiment,file,content)
sentiment_cleaned_tweet1 <- inner_join(source_cleaned_tweet1,tweets2,by="data.text") %>% 
  select(data.text,date,actor,region,role,sentiment)

#cleaned tweet b.2
ls_json <- list.files("cleaned_tweet_2/prediction/",pattern = ".jsonl",full.names = T)
ls_json <- lapply(ls_json,read_lines)
ls_json <- unlist(ls_json)
prediction <- lapply(ls_json,fromJSON)
content <- lapply(prediction,"[[","instance")
content <- lapply(content,"[[","content")
sentiment <- lapply(prediction,"[[","prediction")
sentiment <- lapply(sentiment,"[[","sentiment")
prediction <- tibble(content = unlist(content),
                     sentiment = unlist(sentiment))

prediction <- prediction %>% 
  mutate(file = str_sub(content,-9))

ls_source <- list.files("cleaned_tweet_2/",pattern = ".txt",full.names = T)

source_cleaned_tweet2 <- lapply(ls_source,read_file)
source_cleaned_tweet2 <- tibble(data.text=unlist(source_cleaned_tweet2),file = str_sub(ls_source,-9))
source_cleaned_tweet2 <- left_join(source_cleaned_tweet2,prediction,by="file") %>%  
  drop_na() %>% 
  select(data.text,sentiment,file,content)
sentiment_cleaned_tweet2 <- inner_join(source_cleaned_tweet2,tweets2,by="data.text") %>% 
  select(data.text,date,actor,region,role,sentiment)

#cleaned tweet b.3
ls_json <- list.files("cleaned_tweet_3/prediction/",pattern = ".jsonl",full.names = T)
ls_json <- lapply(ls_json,read_lines)
ls_json <- unlist(ls_json)
prediction <- lapply(ls_json,fromJSON)
content <- lapply(prediction,"[[","instance")
content <- lapply(content,"[[","content")
sentiment <- lapply(prediction,"[[","prediction")
sentiment <- lapply(sentiment,"[[","sentiment")
prediction <- tibble(content = unlist(content),
                     sentiment = unlist(sentiment))

prediction <- prediction %>% 
  mutate(file = str_sub(content,-9))

ls_source <- list.files("cleaned_tweet_3/",pattern = ".txt",full.names = T)

source_cleaned_tweet3 <- lapply(ls_source,read_file)
source_cleaned_tweet3 <- tibble(data.text=unlist(source_cleaned_tweet3),file = str_sub(ls_source,-9))
source_cleaned_tweet3 <- left_join(source_cleaned_tweet3,prediction,by="file") %>%  
  drop_na() %>% 
  select(data.text,sentiment,file,content)
sentiment_cleaned_tweet3 <- inner_join(source_cleaned_tweet3,tweets2,by="data.text") %>% 
  select(data.text,date,actor,region,role,sentiment)

#cleaned tweet b.4
ls_json <- list.files("cleaned_tweet_4/prediction/",pattern = ".jsonl",full.names = T)
ls_json <- lapply(ls_json,read_lines)
ls_json <- unlist(ls_json)
prediction <- lapply(ls_json,fromJSON)
content <- lapply(prediction,"[[","instance")
content <- lapply(content,"[[","content")
sentiment <- lapply(prediction,"[[","prediction")
sentiment <- lapply(sentiment,"[[","sentiment")
prediction <- tibble(content = unlist(content),
                     sentiment = unlist(sentiment))

prediction <- prediction %>% 
  mutate(file = str_sub(content,-9))

ls_source <- list.files("cleaned_tweet_4/",pattern = ".txt",full.names = T)

source_cleaned_tweet4 <- lapply(ls_source,read_file)
source_cleaned_tweet4 <- tibble(data.text=unlist(source_cleaned_tweet4),file = str_sub(ls_source,-9))
source_cleaned_tweet4 <- left_join(source_cleaned_tweet4,prediction,by="file") %>%  
  drop_na() %>% 
  select(data.text,sentiment,file,content)
sentiment_cleaned_tweet4 <- inner_join(source_cleaned_tweet4,tweets2,by="data.text") %>% 
  select(data.text,date,actor,region,role,sentiment)


#cleaned tweet b.5
ls_json <- list.files("cleaned_tweet_5/prediction/",pattern = ".jsonl",full.names = T)
ls_json <- lapply(ls_json,read_lines)
ls_json <- unlist(ls_json)
prediction <- lapply(ls_json,fromJSON)
content <- lapply(prediction,"[[","instance")
content <- lapply(content,"[[","content")
sentiment <- lapply(prediction,"[[","prediction")
sentiment <- lapply(sentiment,"[[","sentiment")
prediction <- tibble(content = unlist(content),
                     sentiment = unlist(sentiment))

prediction <- prediction %>% 
  mutate(file = str_sub(content,-9))

ls_source <- list.files("cleaned_tweet_5/",pattern = ".txt",full.names = T)

source_cleaned_tweet5 <- lapply(ls_source,read_file)
source_cleaned_tweet5 <- tibble(data.text=unlist(source_cleaned_tweet5),file = str_sub(ls_source,-9))
source_cleaned_tweet5 <- left_join(source_cleaned_tweet5,prediction,by="file") %>%  
  drop_na() %>% 
  select(data.text,sentiment,file,content)
sentiment_cleaned_tweet5 <- inner_join(source_cleaned_tweet5,tweets2,by="data.text") %>% 
  select(data.text,date,actor,region,role,sentiment)


# senti_local <- select(SENTIMENT_LOCAL,-data.text)
# senti_national <- select(sentimet_cleaned_tweet1,-data.text)

senti_1 <-  mutate(sentiment_cleaned_tweet1, sentiment=gsub(0,-1,sentiment)) %>% 
  mutate(sentiment.name=if_else(sentiment>=1,"positive","negative")) %>% 
  mutate(region=str_replace(region, "national", "central"))

senti_2 <-  mutate(sentiment_cleaned_tweet2, sentiment=gsub(0,-1,sentiment)) %>% 
  mutate(sentiment.name=if_else(sentiment>=1,"positive","negative")) %>% 
  mutate(region=str_replace(region, "national", "central"))


senti_3 <-  mutate(sentiment_cleaned_tweet3, sentiment=gsub(0,-1,sentiment)) %>% 
  mutate(sentiment.name=if_else(sentiment>=1,"positive","negative")) %>% 
  mutate(region=str_replace(region, "national", "central"))



senti_4 <- mutate(sentiment_cleaned_tweet4, sentiment=gsub(0,-1,sentiment)) %>% 
  mutate(sentiment.name=if_else(sentiment>=1,"positive","negative")) %>% 
  mutate(region=str_replace(region, "national", "central"))



senti_5 <- mutate(sentiment_cleaned_tweet5, sentiment=gsub(0,-1,sentiment)) %>% 
  mutate(sentiment.name=if_else(sentiment>=1,"positive","negative")) %>% 
  mutate(region=str_replace(region, "national", "central"))




# senti_all_automl <- bind_rows(senti_local,senti_national)
# senti_all_automl <- mutate(senti_all_automl,sentiment=gsub(0,-1,sentiment))

all_senti_NEW <- bind_rows(senti_1,senti_2,senti_3,senti_4,senti_5)



write_csv(senti_1,"senti_autoML_180421(1).csv")
write_csv(senti_2,"senti_autoML_180421(2).csv")
write_csv(senti_3,"senti_autoML_180421(3).csv")
write_csv(senti_4,"senti_autoML_180421(4).csv")
write_csv(senti_5,"senti_autoML_180421(5).csv")
write_csv(all_senti_NEW,"senti_autoML_150421(ALL).csv")




# write_csv(SENTIMENT_LOCAL,"sentiment_local_automl.csv")
# write_csv(sentimet_cleaned_tweet1,"sentimet_cleaned_tweet1_automl.csv")
# write_csv(senti_all_automl,"senti_all_automl.csv")
