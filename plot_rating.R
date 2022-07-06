# Chargement des library #####
library(rvest) 
library(stringr) 
library("tidyverse")
library(data.table)
library(stringi)
library(RColorBrewer)
library(cowplot)
library(showtext)

# titre de la série
serie_title = 'Smallville'

# nombre de saisons
s = 10

# code de la série à retrouve 
# exemple https://www.imdb.com/title/tt0279600/episodes?ref_=tt_eps_sm
code = 'tt0279600'

table = data.frame()

# web scrap des informations
for(i in 1:s){
  
  url = paste0("https://www.imdb.com/title/",code,"/episodes?season=",i
  )
  
  while(TRUE){
    dl_file <- try(download.file(as.character(url), destfile = "temp.html", quiet=TRUE),
                   silent=TRUE)
    if(!is(dl_file, 'try-error')) break
  }
  
  html_page <- read_html("temp.html")
  
  saison = i
  
  episode <- html_page %>% 
    html_nodes("#episodes_content strong a") %>% 
    html_text() %>% 
    str_replace_all("\t|\n|"  , "")
  
  note <- html_page %>% 
    html_nodes(".ipl-rating-star.small .ipl-rating-star__rating") %>% 
    html_text() %>% 
    str_replace_all("\t|\n|"  , "")
  
  vote <- html_page %>% 
    html_nodes(".ipl-rating-star__total-votes") %>% 
    html_text() %>% 
    str_replace_all("\t|\n|"  , "")
  
  temp_table <- as.data.frame(cbind(saison, episode, note, vote))
  table <- rbind(table,temp_table) 
  
}


df_avg <-
  table %>% 
  group_by(saison) %>% 
  mutate(episode = row_number(),
         saison = as.numeric(saison),
         note = as.numeric(note),
         vote = gsub(',','',vote),
         vote = as.numeric(gsub("[^\\d]+", "", vote, perl=TRUE))
  ) %>% 
  ungroup() %>% 
  arrange(saison, episode) %>% 
  mutate(episode_id = row_number()) %>% 
  group_by(saison) %>% 
  mutate(
    avg = mean(note),
    episode_mod = episode_id + (9 * saison),
    mid = mean(episode_mod)
  ) %>% 
  ungroup() %>% 
  mutate(saison = factor(saison))

df_lines <-
  df_avg %>% 
  group_by(saison) %>% 
  summarize(
    start_x = min(episode_mod) -5,
    end_x = max(episode_mod) + 5,
    y = unique(avg)
  ) %>% 
  pivot_longer(
    cols = c(start_x, end_x),
    names_to = "type",
    values_to = "x"
  ) %>% 
  mutate(
    x_group = if_else(type == "start_x", x + .1, x - .1),
    x_group = if_else(type == "start_x" & x == min(x), x_group - .1, x_group),
    x_group = if_else(type == "end_x" & x == max(x), x_group + .1, x_group)
  )

p = df_avg %>% 
  ggplot(aes(episode_mod, note)) +
  geom_hline(data = tibble(y = 7:11),
             aes(yintercept = y),
             color = "grey82",
             size = .5) +
  geom_segment(aes(xend = episode_mod,
                   yend = avg, 
                   color = saison, 
                   color = after_scale(colorspace::lighten(color, .2)))) +
  geom_line(data = df_lines,
            aes(x, y),
            color = "grey40") +
  geom_line(data = df_lines,
            aes(x_group, y, 
                color = saison, 
                color = after_scale(colorspace::darken(color, .2))),
            size = 2.5) +
  geom_point(aes(size = vote,
                 color = saison)) +
  geom_label(aes(mid, 10.2,
                 label = glue::glue(" Saison {saison} "),
                 color = saison, 
                 color = after_scale(colorspace::darken(color, .2))),
             show.legend = FALSE,
             fill = NA,
             # family = "Special Elite",
             # fontface = "bold",
             label.padding = unit(.2, "lines"),
             label.r = unit(.25, "lines"),
             label.size = .5)  +
  scale_x_continuous(expand = c(.015, .015)) +
  scale_y_continuous(expand = c(.03, .03),
                     limits = c(6.5, 10.5),
                     breaks = seq(6.5, 10, by = .5),
                     sec.axis = dup_axis(name = NULL)) +
  # scale_color_manual(values = c("#486090", "#D7BFA6", "#6078A8", "#9CCCCC", 
  #                               "#7890A8"),
  #                    guide = F) +
  scale_color_brewer(palette = "Paired", guide = "none") +
  scale_size_binned(name = "Votes par épisode",
                    range = c(.3, 6),
                    labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
  ggtitle(serie_title) +
  # xlab("") + ylab("Note") +
  labs(x = "", y = "Note",
       caption = "Visualization by Clément Rieux") +
  guides(size = guide_bins(show.limits = T,
                           direction = "horizontal",
                           title.position = "top",
                           title.hjust = .5)) +
  theme(legend.position = c(.2, .085), 
        legend.key.size = unit(1, 'cm'),
        legend.key.width = unit(2, "lines"),
        legend.text = element_text(angle = 60, vjust = 1, hjust=1),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

ggsave(paste0(gsub(" ","_", serie_title), ".png"), 
       width = 15, height = 9) 

