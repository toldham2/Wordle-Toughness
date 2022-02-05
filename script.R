#### Library ####
library(rtweet)
library(tidyverse)

#### Keys ####
api_key <- "xfQW7UIdZETukBsRYGKgAvwza"
api_secret_key <- "jNhal8ujHoC0Ecbw8DyF16wZWELYo1hBBH6U9FPWOddL1yOFj7"
access_token <- "1442505297185050627-3Lel9RIa1obeiFXeFDvvm6NmhMBuc6"
access_secret_token <- "oD4wBbFw22ukXrjIEZdzpiXrH7CK9q7h8s7nLXjVdq4vs"

token <- create_token(
  app = "R",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_secret_token)

#### Get Data ####
master <- data.frame()

Sys.sleep(900)

for (i in 223:231){
  query <- paste0('"Wordle ', i, '"')
  
  # if (i %% 5 == 0) {
  #   message("\n", Sys.time(), ": Waiting to avoid timeout...")
  #   Sys.sleep(905)
  # }
  
  message(paste0("\n", Sys.time(), ": Getting scores for game ", i, "..."))
  tweets <- search_tweets(query,
                          n = 1999,
                          include_rts = FALSE,
                          type = "recent")
  
  score <- str_sub(tweets$text, 12, 14)
  actual_score <- str_sub(tweets$text, 12, 12)
  game_id <- str_sub(tweets$text, 8, 11)
  
  scores <- data.frame(tweets$screen_name, score, actual_score, tweets$created_at, game_id, tweets$lang)
  
  scores <- filter(scores, grepl("/", score))
  scores <- filter(scores, !grepl(" ", score))
  scores <- filter(scores, grepl("en", tweets.lang))
  
  master <- rbind(master, scores)
}

#### Clean Data ####
master <- filter(master, grepl("1/6|2/6|3/6|4/6|5/6|6/6|X/6", score))
master$actual_score <- gsub("X", "7", master$actual_score)
master <- filter(master, grepl("223|224|225|226|227|228|229|230|231", game_id))

write.csv(master, "/Users/tyleroldham/Documents/Data Viz Projects/Wordle-Toughness/wordle_scores_223-231.csv")

#### Get Overall Dist ####
score_dist_overall <- master %>%
  count(score)

score_dist_overall %>% 
  ggplot(aes(score, n)) +
  geom_col()

#### Get Average Score by Game
score_avg_game <- master %>% 
  group_by(game_id) %>%
  summarise(mean = mean(as.numeric(actual_score)))

score_avg_game %>% 
  ggplot(aes(game_id, mean)) +
  geom_point() +
  geom_hline(yintercept = mean(score_avg_game$mean)) +
  scale_x_discrete(name="Game ID") +
  scale_y_continuous(name="Average Score", limits=c(1, 6))
