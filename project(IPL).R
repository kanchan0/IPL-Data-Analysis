############# Load required packages #############
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

library(treemap)
library(RColorBrewer)
library(caret)        # for predictive modeling
library(randomForest) # for prediction
library(scales)       # axis formatting
library(viridis)      # high-quality color palettes
library(igraph)       # for partnership network
library(reshape2)     # for confusion matrix plotting

# Create directory to save plots
if(!dir.exists("Plots")) dir.create("Plots")

############# Load data #############
deliveries <- read.csv("./IPL/deliveries.csv")
matches <- read.csv("./IPL/matches.csv")
matches <- matches[matches$result == "normal", ]

############# Matches played in different cities #############
p1 <- ggplot(matches %>% filter(!is.na(city)), aes(city, fill = city)) +
  geom_bar() +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "City", y = "Number of Matches Played", title = "Matches Played in Different Cities") +
  guides(fill = FALSE) +
  scale_fill_viridis(discrete = TRUE)
ggsave("Plots/Matches_By_City.png", plot = p1, width = 10, height = 6, dpi = 300, bg="white")

############# Toss advantage analysis #############
matches$toss_match <- ifelse(as.character(matches$toss_winner) == as.character(matches$winner), "Won", "Lost")
p2 <- ggplot(matches %>% filter(!is.na(toss_match)), aes(toss_match, fill = toss_match)) +
  geom_bar() +
  theme_classic(base_size = 14) +
  labs(x = "Toss Result", y = "Number of Matches Won", title = "Advantage of Winning Toss") +
  scale_fill_brewer(palette = "Set1")
ggsave("Plots/Toss_Advantage.png", plot = p2, width = 6, height = 6, dpi = 300, bg="white")

############# Matches played by each team #############
matches_played <- as.data.frame(table(matches$team2) + table(matches$team1))
p3 <- ggplot(matches_played, aes(reorder(Var1, -Freq), Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Team", y = "Number of Matches Played", title = "Matches Played by Each Team") +
  guides(fill = FALSE) +
  scale_fill_viridis(discrete = TRUE)
ggsave("Plots/Matches_By_Team.png", plot = p3, width = 10, height = 6, dpi = 300, bg="white")

############# Matches won by each team #############
p4 <- ggplot(matches, aes(winner)) +
  geom_bar(fill = "#0072B2") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Team", y = "Matches Won", title = "Matches Won by Each Team")
ggsave("Plots/Matches_Won.png", plot = p4, width = 10, height = 6, dpi = 300, bg="white")

############# Home advantage analysis #############
Data <- matches %>% filter(season != 2009)
Data$date <- as.Date(Data$date)
Data1 <- Data %>% filter(date < as.Date("2014-04-16") | date > as.Date("2014-04-30"))

# Assign home teams based on city
home_map <- c(
  "Bangalore" = "Royal Challengers Bangalore", "Chennai" = "Chennai Super Kings",
  "Delhi" = "Delhi Daredevils", "Chandigarh" = "Kings XI Punjab", "Jaipur" = "Rajasthan Royals",
  "Mumbai" = "Mumbai Indians", "Kolkata" = "Kolkata Knight Riders", "Kochi" = "Kochi Tuskers Kerala",
  "Ahmedabad" = "Rajasthan Royals", "Dharamsala" = "Kings XI Punjab", "Rajkot" = "Gujarat Lions",
  "Kanpur" = "Gujarat Lions", "Raipur" = "Delhi Daredevils", "Nagpur" = "Deccan Chargers",
  "Indore" = "Kochi Tuskers Kerala"
)
Data1$home_team <- home_map[Data1$city]
Data1$home_team[Data1$city == "Hyderabad" & Data1$season <= 2012] <- "Deccan Chargers"
Data1$home_team[Data1$city == "Hyderabad" & Data1$season > 2012] <- "Sunrisers Hyderabad"
Data1$home_team[Data1$city == "Visakhapatnam" & Data1$season == 2015] <- "Sunrisers Hyderabad"
Data1$home_team[Data1$city == "Ranchi" & Data1$season == 2013] <- "Kolkata Knight Riders"
Data1$home_team[Data1$city == "Ranchi" & Data1$season > 2013] <- "Chennai Super Kings"
Data1$home_team[Data1$city == "Pune" & Data1$season != 2016] <- "Pune Warriors"
Data1$home_team[Data1$city == "Pune" & Data1$season == 2016] <- "Rising Pune Supergiants"
Data1 <- Data1 %>% filter(!is.na(home_team))
Data1$win_host <- ifelse(as.character(Data1$winner) == as.character(Data1$home_team), "Home", "Away")

p5 <- ggplot(Data1, aes(win_host, fill = win_host)) +
  geom_bar() +
  theme_classic(base_size = 14) +
  labs(x = "Team", y = "Number of Matches Won", title = "Is Home Advantage Real in IPL?") +
  scale_fill_brewer(palette = "Set2")
ggsave("Plots/Home_Advantage.png", plot = p5, width = 6, height = 6, dpi = 300, bg="white")

############# Winning percentage of each team #############
matches_won <- as.data.frame(table(matches$winner))
colnames(matches_won)[2] <- "Won"
matches_played <- as.data.frame(table(matches$team2) + table(matches$team1))
colnames(matches_played)[2] <- "Played"

p6 <- ggplot(left_join(matches_played, matches_won), aes(reorder(Var1, -Won/Played), Won*100/Played, fill = Var1)) +
  geom_bar(stat = "identity") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Team", y = "Win Percentage", title = "Winning Percentage of Teams") +
  guides(fill = FALSE) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_viridis(discrete = TRUE)
ggsave("Plots/Win_Percentage.png", plot = p6, width = 10, height = 6, dpi = 300, bg="white")

############# Top batsmen #############
top_batsmen <- deliveries %>% group_by(batsman) %>% summarise(runs = sum(batsman_runs)) %>% arrange(desc(runs)) %>% filter(runs > 3000)
p7 <- ggplot(top_batsmen, aes(reorder(batsman, -runs), runs, fill = batsman)) +
  geom_bar(stat = "identity") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Batsman", y = "Runs", title = "Top Batsmen in IPL") +
  guides(fill = FALSE) +
  scale_fill_viridis(discrete = TRUE)
ggsave("Plots/Top_Batsmen.png", plot = p7, width = 10, height = 6, dpi = 300, bg="white")

############# Top bowlers #############
top_bowlers <- deliveries %>% group_by(bowler) %>% filter(player_dismissed != "") %>% summarise(wickets = n()) %>% top_n(10, wt = wickets)
p8 <- ggplot(top_bowlers, aes(reorder(bowler, -wickets), wickets, fill = bowler)) +
  geom_bar(stat = "identity") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Bowler", y = "Wickets", title = "Top Bowlers in IPL") +
  guides(fill = FALSE) +
  scale_fill_viridis(discrete = TRUE)
ggsave("Plots/Top_Bowlers.png", plot = p8, width = 10, height = 6, dpi = 300, bg="white")

############# Treemap of runs against teams #############
top_players <- c("V Kohli","SK Raina","RG Sharma","G Gambhir")
df_treemap <- deliveries %>% filter(batsman %in% top_players) %>% group_by(batsman, bowling_team) %>% summarise(runs = sum(batsman_runs)) %>% filter(runs > 100)
png("Plots/TopRuns_Treemap.png", width=1200, height=800, bg="white")
treemap(df_treemap,
        index = c("batsman", "bowling_team"),
        vSize = "runs",
        vColor = "bowling_team",
        type = "categorical",
        palette = brewer.pal(12,"Set3"),
        fontsize.title = 15,
        fontfamily.title = "serif",
        title = "Runs Against Different Teams")
dev.off()

############# Dismissal types treemap #############
df_dismiss <- deliveries %>% filter(player_dismissed %in% top_players) %>% group_by(player_dismissed, dismissal_kind) %>% summarise(type = n())
png("Plots/Dismissal_Treemap.png", width=1200, height=800, bg="white")
treemap(df_dismiss,
        index = c("player_dismissed", "dismissal_kind"),
        vSize = "type",
        vColor = "dismissal_kind",
        type = "categorical",
        palette = brewer.pal(6,"Set2"),
        fontsize.title = 15,
        fontfamily.title = "serif",
        title = "Type of Dismissals")
dev.off()

############# Strike rate by over #############
df_strike <- deliveries %>%
  filter(batsman %in% top_players) %>%
  group_by(batsman, over) %>%
  summarise(strike = mean(batsman_runs, na.rm = TRUE)*100, .groups="drop") %>%
  tidyr::complete(batsman, over = 1:20, fill = list(strike = 0))  # fill missing overs with 0

p9 <- ggplot(df_strike, aes(over, strike, col = batsman)) +
  geom_line(size = 1.5) +
  theme_classic(base_size = 14) +
  labs(x = "Over", y = "Strike Rate", title = "Strike Rate of Top Batsmen Over Overs") +
  scale_x_continuous(breaks = 1:20) +
  scale_color_viridis(discrete = TRUE)

ggsave("Plots/Strike_Rate.png", plot = p9, width = 10, height = 6, dpi = 300, bg="white")




############# Season-wise runs comparison #############
df_season <- deliveries %>%
  left_join(matches %>% select(id, season), by=c("match_id"="id")) %>%
  filter(batsman %in% top_players) %>%
  group_by(batsman, season) %>%
  summarise(runs = sum(batsman_runs, na.rm = TRUE), .groups="drop") %>%
  tidyr::complete(batsman, season = 2008:2016, fill = list(runs = 0))

p10 <- ggplot(df_season, aes(season, runs, col = batsman)) +
  geom_line(size = 1.5) +
  theme_classic(base_size = 14) +
  scale_x_continuous(breaks = 2008:2016) +
  labs(x = "Season", y = "Runs", title = "Season-wise Runs Comparison of Top Batsmen") +
  scale_color_viridis(discrete = TRUE)

ggsave("Plots/Season_Runs.png", plot = p10, width = 10, height = 6, dpi = 300, bg="white")




############# Ball vs Run progression #############
df_prog <- deliveries %>% filter(batsman %in% top_players) %>%
  group_by(match_id, batsman) %>%
  mutate(cum_run = cumsum(batsman_runs), cum_ball = 1:n())
p11 <- ggplot(df_prog, aes(cum_ball, cum_run, col = batsman)) +
  geom_point(alpha = 0.6) +
  theme_classic(base_size = 14) +
  labs(x = "Balls Faced", y = "Cumulative Runs", title = "Runs vs Balls Faced in Matches") +
  scale_color_viridis(discrete = TRUE)
ggsave("Plots/Run_Progression.png", plot = p11, width = 10, height = 6, dpi = 300, bg="white")

############# Predictive model: Match winner based on toss and city #############
matches_model <- matches %>% select(winner, toss_winner, city) %>% filter(!is.na(city))
matches_model$winner <- as.factor(matches_model$winner)
matches_model$toss_winner <- as.factor(matches_model$toss_winner)
matches_model$city <- as.factor(matches_model$city)

set.seed(123)
trainIndex <- createDataPartition(matches_model$winner, p = 0.8, list = FALSE)
train <- matches_model[trainIndex, ]
test <- matches_model[-trainIndex, ]

rf_model <- randomForest(winner ~ toss_winner + city, data = train, ntree = 200)
pred <- predict(rf_model, test)
accuracy <- mean(pred == test$winner)
print(paste("Prediction Accuracy (Toss + City Model):", round(accuracy*100,2), "%"))

############# Predictive model: Match winner (full features) #############
matches_model <- matches %>%
  select(team1, team2, toss_winner, toss_decision, season, city, winner) %>%
  filter(!is.na(city))

# Encode all character columns as factors
matches_model <- matches_model %>% mutate(across(where(is.character), as.factor))

set.seed(123)
train_index <- createDataPartition(matches_model$winner, p = 0.8, list = FALSE)
train_data <- matches_model[train_index, ]
test_data <- matches_model[-train_index, ]

rf_model_full <- randomForest(
  winner ~ team1 + team2 + toss_winner + toss_decision + season + city,
  data = train_data,
  ntree = 500
)
rf_pred <- predict(rf_model_full, test_data)

############# Confusion matrix heatmap with diagonal highlight #############
conf_matrix <- table(rf_pred, test_data$winner)
conf_df <- as.data.frame(conf_matrix)
colnames(conf_df) <- c("Predicted", "Actual", "Count")
conf_df$Correct <- ifelse(conf_df$Predicted == conf_df$Actual, "Yes", "No")

p_conf <- ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile(color = "grey80", size = 0.3) +              # plain tiles with border
  geom_text(aes(label = Count), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_classic(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks = element_line(color = "black"),
    axis.line = element_line(color = "black"),
    legend.position = "right"
  ) +
  labs(title = "Confusion Matrix Heatmap",
       x = "Actual Winner", y = "Predicted Winner")

ggsave("Plots/Confusion_Matrix.png", plot = p_conf, width = 12, height = 8, dpi = 300, bg="white")

############# Predict runs scored by batsmen #############
deliveries_model <- deliveries %>%
  inner_join(matches %>% select(id, season), by = c("match_id" = "id")) %>%
  filter(!is.na(batsman_runs), !is.na(over), !is.na(season),
         !is.na(batting_team), !is.na(bowling_team), !is.na(bowler))

# randomForest only allows factors with <= 53 levels: keep the top 50 bowlers, lump the rest
top_b <- names(sort(table(deliveries_model$bowler), decreasing = TRUE))[1:50]
deliveries_model$bowler <- ifelse(deliveries_model$bowler %in% top_b,
                                  as.character(deliveries_model$bowler), "Other")

# Sample for speed (the full ~150k rows is slow for a first run)
set.seed(123)
deliveries_model <- deliveries_model[sample(nrow(deliveries_model), 50000), ]

factor_cols <- c("batting_team", "bowling_team", "bowler", "season")
deliveries_model <- deliveries_model %>%
  mutate(across(all_of(factor_cols), as.factor))

train_idx <- createDataPartition(deliveries_model$batsman_runs, p = 0.8, list = FALSE)
train_del <- deliveries_model[train_idx, ]
test_del  <- deliveries_model[-train_idx, ]

for (col in factor_cols) {
  test_del[[col]] <- factor(test_del[[col]], levels = levels(train_del[[col]]))
}
test_del <- test_del %>% filter(if_all(all_of(factor_cols), ~ !is.na(.x)))

rf_runs_model <- randomForest(
  batsman_runs ~ batting_team + bowling_team + over + bowler + season,
  data = train_del,
  ntree = 100
)

runs_pred <- predict(rf_runs_model, test_del)
test_del$runs_pred <- runs_pred

p_pred <- ggplot(test_del, aes(runs_pred, batsman_runs)) +
  geom_point(alpha = 0.5) +
  geom_abline(color = "red") +
  theme_classic(base_size = 14) +
  labs(x = "Predicted Runs", y = "Actual Runs", title = "Random Forest Prediction of Batsman Runs")

ggsave("Plots/Predicted_vs_Actual_Runs.png", plot = p_pred, width = 8, height = 6, dpi = 300, bg = "white")
############# Top partnerships network #############
partnerships <- deliveries %>%
  filter(!is.na(player_dismissed)) %>%
  group_by(batsman, non_striker) %>%
  summarise(runs = sum(batsman_runs), .groups = "drop") %>%
  filter(runs > 100)

g <- graph_from_data_frame(partnerships, directed = FALSE)
png("Plots/Partnership_Network.png", width = 1000, height = 800, bg="white")
plot(g, vertex.size = 15, vertex.label.cex = 0.8, edge.width = E(g)$runs / 20,
     main = "Top Batsman Partnerships")
dev.off()

############# Strike rate clustering #############
strike_data <- deliveries %>%
  filter(batsman %in% top_players) %>%
  group_by(batsman, bowler) %>%
  summarise(strike_rate = mean(batsman_runs, na.rm = TRUE) * 100, .groups = "drop")

strike_matrix <- spread(strike_data, key = bowler, value = strike_rate, fill = 0)
row.names(strike_matrix) <- strike_matrix$batsman
strike_matrix <- strike_matrix[, -1]

d <- dist(strike_matrix)
hc <- hclust(d)
png("Plots/Strike_Rate_Clustering.png", width = 800, height = 600, bg = "white")
plot(hc, main = "Hierarchical Clustering of Strike Rate vs Bowlers")
dev.off()
