# https://www.tidytextmining.com/sentiment.html
library(dplyr)
library(readr)
library(tidytext)
library(stringr)
library(tidyr)
library(uuid)


#Applying unique row id with "uuid"
# master_act = read_csv("master_actor_030421.csv")  
# uuid_ls = sapply( seq_along(1:nrow(master_act)), UUIDgenerate)
# master_act <- master_act %>% mutate(uuid=uuid_ls)
# 
# master_ins = read_csv("master_institusi030421.csv")
# uuid_ls = sapply( seq_along(1:nrow(master_ins)), UUIDgenerate)
# master_ins <- master_ins %>% mutate(uuid=uuid_ls)

tweets2 <- read_csv("cleaned_tweets_130421.csv")
uuid_ls = sapply( seq_along(1:nrow(tweets2)), UUIDgenerate)
tweets2 <- tweets2 %>% mutate(uuid=uuid_ls)


#Uploading the lexicon 
lexicon =  read_csv("modified_full_lexicon.txt")

#select important variables & tokenization
# master_act_w <- master_act %>% select(uuid,data.created_at,data.text,date,actor,region,role) %>% 
#   unnest_tokens(word,data.text)
# 
# master_ins_w <- master_ins %>% select(uuid,data.created_at,data.text,date,actor,region,role) %>% 
#   unnest_tokens(word,data.text)

tweets_w <- tweets2 %>% select(uuid,data.text,date,actor,region,role) %>% 
  unnest_tokens(word,data.text)

#data actor match by word
# act_sentiment_tw = left_join(master_act_w, lexicon,by="word") %>% drop_na() 
# act_sentiment_sum = act_sentiment_tw %>% group_by(uuid) %>% 
#   summarise(sentiment = sum(weight)) %>% left_join(master_act,by="uuid")
# 
# act_sentiment_sum <- mutate(act_sentiment_sum,sentiment.name=if_else(sentiment>=1,"positive",
#                                                                      if_else(sentiment==0,"neutral","negative"))) %>% 
#   mutate(sentiment.norm=if_else(sentiment>=1,1,if_else(sentiment==0,0,-1)))
# 
# table(act_sentiment_sum$sentiment.name)

#data institution match by word
# ins_sentiment_tw = left_join(master_ins_w, lexicon,by="word") %>% drop_na() 
# ins_sentiment_sum = ins_sentiment_tw %>% group_by(uuid,actor) %>% 
#   summarise(sentiment = sum(weight))

tweets2_tw = left_join(tweets_w, lexicon,by="word") %>% drop_na() 

tweets2_sum = tweets2_tw %>% group_by(uuid) %>% 
  mutate(senti.words=if_else(weight>=1,"positive",
                             if_else(weight==0,"neutral","negative")))  
posnegcount=pivot_wider(select(tweets2_sum,uuid,senti.words,number_of_words),names_from = senti.words,values_from = number_of_words) %>% 
  mutate(positive=lapply(positive,sum),negative=lapply(negative,sum))

abc <- left_join(posnegcount,tweets2, by="uuid")

tweets2_sum = tweets2_tw %>% group_by(uuid) %>% 
  summarise(sentiment = sum(weight)) %>% left_join(tweets2,by="uuid")

abc <- mutate(abc, mu=log(as.numeric(positive) + 0)/log(as.numeric(negative) + 0))
abc <- mutate(abc, sigma=((as.numeric(positive) + 0)^-1) + (as.numeric(negative)^-1))

plot(abc$mu, abc$sigma,
     xlim = c(0,2),
     ylim = c(0, 2),
     main = "Model Space: each point represent a Gaussian Model",
     xlab = "mu space",
     ylab = "sigma space")

curve(dnorm(x, sample_mu[1], sample_sigma[1]), from = -50, to = 450,
      main = "The first sampled Gaussian Model",
      xlab = "height(cm)",
      ylab = "Probability Density conditioned to using the first sampled model  ")


tweets2_sum <- mutate(tweets2_sum,sentiment.name=if_else(sentiment>=1,"positive",
                                                                     if_else(sentiment==0,"neutral","negative"))) %>% 
  mutate(sentiment.norm=if_else(sentiment>=1,1,if_else(sentiment==0,0,-1)))


write_csv(tweets2_sum,"all_tweet_140421.csv")

tw_act_viz <- group_by(tweets2_sum,date,actor) %>% count(sentiment.name)
tw_act_viz <- spread(tw_act_viz,actor,n)

tw_act_viz_1 <- group_by(tweets2_sum,date,actor,sentiment.name) %>% summarise(n=sum(sentiment.norm)) 
tw_viz_sum2 <- spread(tw_act_viz_1,sentiment.name,n)

write_csv(tw_viz_sum2,"all_dailysent_140421.csv")
  
tw_act <- select(tw_viz_sum2,-dki,-jabar,-jateng,-jatim,-kemenkes,-sulsel,-kaltim)
write_csv(tw_act,"all_actor_140421.csv")

tw_ins <- select(tw_viz_sum2,-jokowi,-isrannoor,-khofifah,-risma,-ridwankamil,-ganjar,-anies,-nurdinabdullah,-terawan)
write_csv(tw_ins,"all_ins_140421.csv")


#filter actor
all_ins <- filter(tweets2_sum,actor=='dki' | actor == 'jabar' | actor== 'jateng' | actor=='jatim' | actor=='sulsel' | actor=='kaltim' | actor== 'kemenkes')
all_fig <- filter(tweets2_sum,actor=='jokowi' | actor == 'terawan' | actor== 'ridwankamil' | actor=='ganjar' | actor=='isrannoor' | actor=='risma' | actor== 'khofifah' | actor== 'nurdinabdullah' | actor== 'anies')

#filter local - national
all_local <- filter(tweets2_sum,region=='local')
all_central <- filter(tweets2_sum_new,region=='central')


write_csv(all_ins,"senti_tidy_ins_150421.csv")
write_csv(all_fig,"senti_tidy_fig_150421.csv")
write_csv(all_central,"all_central_180421.csv")


# act_visualise <- select(act_sentiment_sum,2,23,24,25,28,30)
# act_visualise <- select(act_visualise,date,actor,region,sentiment.name,sentiment) 
# 
# act_visualise_sum <- group_by(act_visualise,actor) %>% count(sentiment.name)
# act_visualise_sum <- spread(act_visualise_sum,sentiment.name,n)
# write_csv(act_visualise_sum,"data_visual_actor.csv")
# 
# 
# #data sentiment per hari
# act_visualise_sum2 <- group_by(act_visualise,date,actor) %>% summarise(n=sum(sentiment.norm)) 
# act_visualise_sum2 <- spread(act_visualise_sum2,actor,n)
# write_csv(act_visualise_sum2,"visual_senti_daily2.csv")


