# Borrows heavily from Arthur Holtz, found at R Views:
#   https://rviews.rstudio.com/2022/02/21/wordle-data-analysis/
#   Thanks to him for figuring out the scraping!

suppressMessages({
  library(httr)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(ggthemes)
  library(scales)
  library(tidyr)
  library(magrittr)
})

url_wordle = "https://www.nytimes.com/games/wordle/main.18637ca1.js"
wordle_script_text <- GET(url_wordle) %>%
  content(as = "text", encoding = "UTF-8")

word_df <- substr(
  wordle_script_text,
  # cigar is the first word
  str_locate(wordle_script_text, "cigar")[,"start"],
  # shave is the last word
  str_locate(wordle_script_text, "shave")[,"end"]) %>%
  str_remove_all("\"") %>%
  str_split(",") %>%
  unlist(.) %>%
  tibble(word = .) 

score_one_word <- function(guess, dict, pos_score = 3, let_score = 2) {
  dat <- expand.grid(
    g_pos = 1:nchar(guess),
    d_word = dict
  ) %>%
    mutate(
      g_word = guess,
      g_let = str_sub(guess, start = g_pos, end = g_pos),
      d_let = str_sub(d_word, g_pos, g_pos)
    )
  
  dat %<>% 
    mutate(
      raw_score = dplyr::case_when(
        g_let == d_let ~ pos_score,
        # sum_char > max_char ~ 0, # special case, imagine "eerie"
        str_detect(d_word, g_let) ~ let_score,
        T ~ 0
      )
    ) %>%
    # by arranaging this way we put the position matches first, which is key
    #  to making sure we give the full points for position matches.
    arrange(desc(raw_score))
  
  # Correct the scores so we don't over-award points for repeated guess letters.
  #  For instance "eerie" is probably not a good guess but it would get a very
  #  high score as is with three E's.
  dat %<>%
    group_by(d_let) %>%
    # maximum number of letter matches that will be awarded:
    mutate(max_char = n()) %>%
    ungroup(.) %>%
    # number of raw matches found:
    group_by(g_let) %>%
    mutate(sum_char = 1:n()) %>%
    ungroup(.) %>%
    mutate(
      score = dplyr::case_when(
        sum_char > max_char ~ 0, # special case, imagine "eerie"
        T ~ raw_score
      )
    ) 
  score_out <- dat %>%
    group_by(guess = g_word, dict_word = d_word) %>%
    summarize(score = sum(score), .groups = "drop") %>%
    group_by(guess) %>%
    summarize(score = mean(score), .groups = "drop") %>%
    pull(score)

  return(score_out)
}

# example use:
score_one_word("terra", dict = c("irate", "fishy"))
test <- score_one_word("terra", dict = word_df$word)

# How long will it take to score 10 words?
# bench::mark(
#   iterations = 10,
#   word_df %>%
#     head(100) %>%
#     mutate(score = purrr::map_dbl(.x = word, 
#                                   .f = score_one_word, 
#                                   dict = word_df$word))
# )

# Good enough!

score_df <- word_df %>%
  mutate(score = purrr::map_dbl(.x = word, 
                                .f = score_one_word, 
                                dict = word_df$word))

score_df %>% arrange(desc(score))

