library(tidyverse)
library(ggplot2)
library(stringr)
library(anchors)
library(ggthemes)
library(ggrepel)

#read csv
chat_stats <- read.csv("slackstats0508.csv")

#clean data
chat_stats_clean <- chat_stats %>% 
  mutate("Active Membership" = (Members.who.posted/Total.membership)*100) %>% 
  mutate("Messages per Member" = Messages.posted.by.members/Total.membership) %>%
  mutate("Type of Channel" = str_extract(Name, "^[^_-]+")) %>%
  mutate("Total Membership" = Total.membership) %>% 
  replace.value("Type of Channel", to="info")

chat_stats_clean$`Type of Channel` <- if_else(chat_stats_clean$`Type of Channel` 
                                              %in% c("all", "eto", "info", "proj", "team"), 
                                              chat_stats_clean$`Type of Channel`, "other")

#visualization
ggplot(chat_stats_clean, aes(x=`Messages per Member`, y=`Active Membership`)) + 
  geom_point(aes(color=`Type of Channel`, size=`Total Membership`), alpha=0.5) +
  geom_text_repel(data=chat_stats_clean %>% filter(`Messages per Member` > 30 & 
                                                     `Active Membership` > 80), 
                  aes(label=Name), size=3.5, fontface = 'bold',
                  box.padding = 1.3) +
  scale_size(range = c(0.5, 12), breaks = c(100, 25, 10, 5)) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  ggtitle("Activity on Different Types of Slack Channels") +
  ylab("Percent of Active Membership") +
  theme_economist_white(gray_bg = FALSE) +
  scale_color_tableau() +
  theme(
    text = element_text(size=8, family="ITC Officina Sans"),
    axis.title.x = element_text(color="black", 
                                size=10,
                                face="italic",
                                vjust=-1),
    axis.title.y = element_text(color="black",
                                size=10,
                                face="italic",
                                vjust=2),
    axis.line.x = element_blank()
  ) 

#save viz
ggsave("Slack Chat Contest Viz.png", device="png", width = 8, height = 6, dpi=300) 