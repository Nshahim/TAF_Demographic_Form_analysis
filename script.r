library(ggplot2)
library(dplyr)
library(openxlsx)
library(plotrix)


data <- read.xlsx("input/TAF Demographic Forms_Final.xlsx")
data[data$Province == "Takhar ", "Province"] = "Takhar"

data %>% 
  group_by(Province) %>% 
  summarize(Participants = sum(length(Name))) %>% 
  ungroup() %>% 
  ggplot(aes(x=Province, y = Participants, fill=Province)) +
  geom_col(show.legend = F) +
  labs(x=NULL, y=NULL) +
  ggtitle("Participants interviewed") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=Participants), nudge_y=1) +
  ylim(c(0,30))




str(data$Age)

table(data$Age)

data %>% 
  group_by(Province) %>% 
  summarize(mean = mean(as.numeric(Age), na.rm = T))

data %>% 
  ggplot(aes(x=as.numeric(Age), fill = Province)) +
  geom_density(alpha = 0.5, show.legend = F) +
  facet_wrap(~Province, ncol = 2) +
  geom_vline(mean(data$Age, na.rm = ))

table(data$Education.level) %>% 
  data.frame() %>% 
  ggplot(aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1)) +
  geom_col() +
  coord_polar(start=0)


data %>% 
  ggplot(aes(x=Education.level, fill=Education.level)) +
  geom_bar() +
  coord_polar(start=0)

data %>%
  mutate(Age_catagory = case_when(
    Age %in% c(17:25) ~ "17-25",
    Age %in% c(26:35) ~ "26-35",
    Age %in% c(36:46) ~ "36-46",
    Age > 46 ~ "Above 46",
    TRUE ~ NA_character_
  )) %>% 
  group_by(Age_catagory) %>% 
  summarize(Frequency = sum(length(Age_catagory))) %>% 
  ungroup() %>% 
  ggplot(aes(x=Age_catagory, y= Frequency, fill=Age_catagory)) +
  geom_col(show.legend = F) +
  theme_bw() +
  labs(x="Age Category") +
  geom_text(aes(label=Frequency), nudge_y=3) +
  ylim(c(0,70))

data %>%  
  group_by(Marital.Status) %>% 
  summarize(Frequency = sum(length(Marital.Status))) %>% 
  ungroup() %>%
  ggplot(aes(x= factor(Marital.Status, levels = c("Single", "Married", "Divorced")), y = Frequency, fill=Marital.Status)) +
  geom_col(show.legend = F) +
  labs(x=NULL) +
  theme_bw() +
  geom_text(aes(label=Frequency), nudge_y=6) +
  ylim(0,160)

data %>%  
  group_by(Marital.Status) %>% 
  summarize(Frequency = sum(length(Marital.Status)))
  

forPie <- table(data$Marital.Status) %>% 
  data.frame() %>% 
  mutate(perc = round(Freq/sum(Freq)*100))
  

pie3D(forPie$perc, labels = forPie$Var1, explode = 0.01, theta = 0.5, main="Distribution of ")
legendg(x=forPie$perc, labels = forPie$Var1)

pie(forPie$perc, labels = paste(forPie$perc, "%", sep = ""), col=rainbow(3), main = "Marital Status")
legend("topright", c("Divorced","Married","Single"), cex = 0.8,
       fill = rainbow(3))


pie(x, labels = piepercent, main = "City pie chart",col = rainbow(length(x)))
legend("topright", c("London","New York","Singapore","Mumbai"), cex = 0.8,
       fill = rainbow(length(x)))


table(data$Marital.Status) %>% 
  data.frame() %>% 
  ggplot(aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1)) +
  geom_col(show.legend = F) +
  coord_polar(start=0) +
  geom_text(aes(label=Freq), nudge_y = 4) +
  labs(x=NULL, y = NULL)


data %>% 
  filter(Education.level != "Illiterate") %>% 
  group_by(Province) %>% 
  summarize(total = sum(length(Education.level)))

data %>% 
  mutate(Age_catagory = case_when(
    Age %in% c(17:25) ~ "17-25",
    Age %in% c(26:35) ~ "26-35",
    Age %in% c(36:46) ~ "36-46",
    Age > 46 ~ "Above 46",
    TRUE ~ NA_character_
  )) %>% 
  group_by(Age_catagory, Marital.Status) %>% 
  summarize(Frequency = sum(length(Marital.Status))) %>% 
  ungroup() %>% 
  ggplot(aes(x= factor(Marital.Status, levels = c("Single", "Married", "Divorced")), y = Frequency, fill=Marital.Status)) +
  geom_col(show.legend = F) +
  labs(x=NULL) +
  theme_bw() +
  facet_wrap(~Age_catagory) +
  geom_text(aes(label=Frequency), nudge_y=4) +
  ylim(c(0,60))

data %>% 
  group_by(Education.level) %>%
  summarize(Participants = sum(length(Education.level))) %>% 
  ungroup() %>% 
  ggplot(aes(x=Education.level, y=Participants, fill=Education.level)) +
  geom_col(show.legend = F) +
  ggtitle("Education Level of Participants") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = -25)) +
  geom_text(aes(label=Participants), nudge_y=2) +
  labs(x=NULL, y = NULL)
  

data %>% 
  group_by(Province, Education.level) %>%
  summarize(Participants = sum(length(Education.level))) %>% 
  ungroup() %>% 
  ggplot(aes(x=Education.level, y=Participants, fill=Education.level)) +
  geom_col() +
  facet_wrap(~Province, ncol = 2) +
  labs(x=NULL, y = NULL, fill=NULL) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom") +
  geom_text(aes(label=Participants), nudge_y=4) +
  ylim(c(0,20))

data %>% 
  group_by(Education.level, `Do.you.have.job?`) %>% 
  summarize(Number_of_Jobs = sum(length(`Do.you.have.job?`))) %>% 
  ungroup() %>%
  ggplot(aes(x=factor(`Do.you.have.job?`, levels = c("Part-Time", "Full-Time", "No")), y = Number_of_Jobs, fill=`Do.you.have.job?`)) +
  geom_col() +
  facet_wrap(~Education.level) +
  labs(x=NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(label=Number_of_Jobs), nudge_y=6) +
  ylim(c(0,60))

