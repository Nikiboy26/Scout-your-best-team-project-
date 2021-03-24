# Load the tidyverse package
library(tidyverse)

# Import the full dataset
data <- read_csv("datasets/athletics.csv")


# Select the results of interest: women's javelin
javelin <- data%>%
  filter(Event == "Javelin", Male_Female == "Female")%>%
  select(- c(Male_Female, Event))

# Give yourself a snapshot of your data 
head(javelin)
summary(javelin)


# Assign the tidy data to javelin_long
javelin_long <- javelin%>%
  gather(Flight1:Flight6, key = "Flight", value = "Distance")

# Make Flight a numeric
javelin_long$Flight <- as.numeric(gsub("Flight", "", javelin_long$Flight))

# Examine the first 6 rows
head(javelin_long)

javelin_totals <- javelin_long %>%
  filter(Distance > 0)%>%
  group_by(Athlete, EventID)%>%
  summarise(TotalDistance = sum(Distance),
            StandardDev = round(sd(Distance),3),
            Success = n())
javelin_totals

javelin <- javelin%>%
  mutate(early = Flight1 + Flight2 +Flight3,
         late = Flight4 + Flight5 + Flight6,
         diff = late - early)
# Examine the last ten rows
lr <- nrow(javelin)
javelin[169:178,]


javelin_totals <- left_join(
  javelin, javelin_totals, by = c("EventID", "Athlete"))
javelin_totals <- javelin_totals%>%
  select(Athlete, TotalDistance, StandardDev, Success, diff)

# Examine the first ten rows
head(javelin_totals, n = 10)

norm <- function(result) {
  (result - min(result)) / (max(result) - min(result))
}
aggstats <- c("TotalDistance", "StandardDev", "Success", "diff")
javelin_norm <- javelin_totals %>%
  ungroup() %>%
  mutate_at(aggstats, norm)%>%
  group_by(Athlete)%>%
  summarize_all(mean)

head(javelin_norm)


weights <- c(2.1, 4.9, .5, 2.5)
javelin_team <- javelin_norm %>%
  mutate(TotalScore = TotalDistance * weights[1] + StandardDev*weights[2] + Success*weights[3] + diff*weights[4]) %>%
  arrange(desc(TotalScore)) %>%
  slice(1:5) %>%
  select(Athlete, TotalScore)

javelin_team


team_stats <- javelin_totals %>% 
  filter(Athlete %in% c("Dominique Ouellette", "Maggie Malone", 
                        "Alicia DeShasier", "Heather Bergmann",
                        "Haley Crouser"))



team_stats <- javelin_totals %>% 
  filter(Athlete %in% javelin_team$Athlete)%>%
  group_by(Athlete)%>%
  summarize_all(mean)

team_stats

pool_stats <- data.frame(do.call('cbind', sapply(javelin_totals, function(x) if(is.numeric(x)) c(max(x), mean(x)))))
pool_stats$MaxAve <- c("Maximum", "Average")
pool_stats <- pool_stats %>%
  gather(key="Statistic", value="Aggregate", -MaxAve)



p <- team_stats %>%
  gather(TotalDistance:diff, key = "Statistic", value = "Aggregate")%>%
  ggplot(aes(x = Athlete, y = Aggregate, fill = Athlete)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(vars(Statistic), scales = "free_y") +
  geom_hline(data=pool_stats, aes(yintercept=Aggregate, group=Statistic, color=MaxAve), size=1) +
  labs(title="Your best team", color="Athlete pool maximum / average") +
  scale_fill_hue(l=70) +
  scale_color_hue(l=20) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

p



home <- c(2,4,5)
away <- sample(1:nrow(javelin_totals), 3, replace=FALSE)

HomeTeam <- round(sum(team_stats$TotalDistance[home]),2)
AwayTeam <- round(sum(javelin_totals$TotalDistance[away]),2)

print(paste0("Javelin match, Final Score: ", HomeTeam, " - ", AwayTeam))
ifelse(HomeTeam > AwayTeam, print("Win!"), print("Sometimes you just have to take the L."))