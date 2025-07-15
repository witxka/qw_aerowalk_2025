library("jsonlite")
library("ggplot2")
library("corrplot")
library("reshape2")  # For melt()


split_key_value_pairs <- function(input_string) {
  # Remove leading/trailing whitespace and quotes
  input_string <- trimws(input_string)
  input_string <- gsub('\\"', '', input_string)
  input_string <- gsub("[\r\n]", "", input_string)
  # Split the string by double backslashes
  parts <- strsplit(input_string, "\\", fixed = TRUE)[[1]]
  
  # Remove empty strings that might result from splitting
  parts <- parts[parts != ""]

  # Check if we have an even number of elements
  if (length(parts) %% 2 != 0) {
    warning("Input string doesn't contain an even number of key-value pairs")
  }
  
  # Create a data frame with keys and values
  keys <- parts[seq(1, length(parts), by = 2)]
  values <- parts[seq(2, length(parts), by = 2)]
  
  # Combine into a named vector
  result <- setNames(values, keys)
  
  # Convert to data frame for better display
  data.frame(Key = keys, Value = values, stringsAsFactors = FALSE)
}


data <- fromJSON("AERO2025_DIV1.duels.json", simplifyDataFrame = FALSE)

# Extract LG data for each player across all entries
lg_data_ga <- list()
lg_data_ra <- list()
lg_data_ya <- list()
lg_data_mh <- list()
lg_data_lg <- list()
lg_data_damage_gvn <- list()
lg_data_frags <- list()
lg_data_rank <- list()
lg_data_skill_rl <- list()
lg_data_speed_avr <- list()
lg_data_spawnfrags <- list()
lg_data_version <- list()
lg_data_client <- list()
lg_data_avg_ping <- list()
lg_data_avg_packetloss <- list()
lg_data_sv_antilag <- list()
lg_data_ktxver <- list()
lg_data_epoch <- list()

for (entry in data) {
  playerId <- 1
  if (entry$players[[1]]$scores$frags > entry$players[[2]]$scores$frags ) {
    playerId <- 1
  } else {
    playerId <- 2
  }
  name <- entry$players[[playerId]]$name
  lg_ga <- entry$players[[playerId]]$armors_and_megas$ga
  lg_ya <- entry$players[[playerId]]$armors_and_megas$ya
  lg_ra <- entry$players[[playerId]]$armors_and_megas$ra
  lg_mh <- entry$players[[playerId]]$armors_and_megas$mh
  lg_lg <- entry$players[[playerId]]$weapons_efficiency$lg
  lg_damage_gvn <- entry$players[[playerId]]$damage$gvn
  lg_frags <- entry$players[[playerId]]$scores$frags
  lg_rank <- entry$players[[playerId]]$scores$rank
  lg_spawnfrags <- entry$players[[playerId]]$spawnfrags
  lg_skill_rl <- entry$players[[playerId]]$skill_RL$ad
  lg_speed_avr <- entry$players[[playerId]]$speed$average
  lg_version <- entry$version

  server_info <-split_key_value_pairs(entry$serverinfo)
  lg_sv_antilag <- server_info$Value[server_info$Key == "sv_antilag" ]
  lg_ktxver <- server_info$Value[server_info$Key == "ktxver" ]
  lg_epoch <- server_info$Value[server_info$Key == "epoch" ]
  
  if ( gsub("_", "",  entry$players_mvd_info[[playerId]]$name_sanatized) == name ) {
    lg_client <- entry$players_mvd_info[[playerId]]$client
    lg_avg_ping <- entry$players_mvd_info[[playerId]]$avg_ping
    lg_avg_packetloss <- entry$players_mvd_info[[playerId]]$avg_packetloss
  } else {
    lg_client <- "unknown"
  }
  lg_data_ga[[name]] <- c(lg_data_ga[[name]], lg_ga)
  lg_data_ra[[name]] <- c(lg_data_ra[[name]], lg_ra)
  lg_data_ya[[name]] <- c(lg_data_ya[[name]], lg_ya)
  lg_data_mh[[name]] <- c(lg_data_mh[[name]], lg_mh)
  lg_data_lg[[name]] <- c(lg_data_lg[[name]], lg_lg)
  lg_data_frags[[name]] <- c(lg_data_frags[[name]], lg_frags)
  lg_data_rank[[name]] <- c(lg_data_rank[[name]], lg_rank)
  lg_data_spawnfrags[[name]] <- c(lg_data_spawnfrags[[name]], lg_spawnfrags)
  lg_data_skill_rl[[name]] <- c(lg_data_skill_rl[[name]], lg_skill_rl)
  lg_data_speed_avr[[name]] <- c(lg_data_speed_avr[[name]], lg_speed_avr)
  lg_data_damage_gvn[[name]]  <- c(lg_data_damage_gvn[[name]], lg_damage_gvn)
  lg_data_version[[name]] <- c(lg_data_version[[name]], lg_version)
  lg_data_client[[name]] <- c(lg_data_client[[name]], lg_client)
  lg_data_avg_ping[[name]] <- c(lg_data_avg_ping[[name]], lg_avg_ping)
  lg_data_avg_packetloss[[name]] <- c(lg_data_avg_packetloss[[name]], lg_avg_packetloss)
  lg_data_sv_antilag[[name]] <- c(lg_data_sv_antilag[[name]], lg_sv_antilag)
  lg_data_ktxver[[name]] <- c(lg_data_ktxver[[name]], lg_ktxver)
  lg_data_epoch[[name]] <- c(lg_data_epoch[[name]], lg_epoch)
 }


# Convert to a data frame
lg_df_epoch <- data.frame(
  player = rep(names(lg_data_epoch), lengths(lg_data_epoch)),
  epoch = unlist(lg_data_epoch),
  match_num = unlist(lapply(lengths(lg_data_epoch), seq_len)))
lg_df_ktxver <- data.frame(
  player = rep(names(lg_data_ktxver), lengths(lg_data_ktxver)),
  ktxver = unlist(lg_data_ktxver),
  match_num = unlist(lapply(lengths(lg_data_ktxver), seq_len)))
lg_df_sv_antilag <- data.frame(
  player = rep(names(lg_data_sv_antilag), lengths(lg_data_sv_antilag)),
  sv_antilag = unlist(lg_data_sv_antilag),
  match_num = unlist(lapply(lengths(lg_data_sv_antilag), seq_len)))
lg_df_avg_ping <- data.frame(
  player = rep(names(lg_data_avg_ping), lengths(lg_data_avg_ping)),
  avg_ping = unlist(lg_data_avg_ping),
  match_num = unlist(lapply(lengths(lg_data_avg_ping), seq_len)))
lg_df_avg_packetloss <- data.frame(
  player = rep(names(lg_data_avg_packetloss), lengths(lg_data_avg_packetloss)),
  avg_packetloss = unlist(lg_data_avg_packetloss),
  match_num = unlist(lapply(lengths(lg_data_avg_packetloss), seq_len)))
lg_df_client <- data.frame(
  player = rep(names(lg_data_client), lengths(lg_data_client)),
  client = unlist(lg_data_client),
  match_num = unlist(lapply(lengths(lg_data_client), seq_len)))

lg_df_version <- data.frame(
  player = rep(names(lg_data_version), lengths(lg_data_version)),
  version = unlist(lg_data_version),
  match_num = unlist(lapply(lengths(lg_data_version), seq_len)))
lg_df_spawnfrags <- data.frame(
  player = rep(names(lg_data_spawnfrags), lengths(lg_data_spawnfrags)),
  spawnfrags = unlist(lg_data_spawnfrags),
  match_num = unlist(lapply(lengths(lg_data_spawnfrags), seq_len)))
lg_df_frags <- data.frame(
  player = rep(names(lg_data_frags), lengths(lg_data_frags)),
  frags = unlist(lg_data_frags),
  match_num = unlist(lapply(lengths(lg_data_frags), seq_len)))

lg_df_rank <- data.frame(
  player = rep(names(lg_data_rank), lengths(lg_data_rank)),
  rank = unlist(lg_data_rank),
  match_num = unlist(lapply(lengths(lg_data_rank), seq_len)))
lg_df_rl_ad <- data.frame(
  player = rep(names(lg_data_skill_rl), lengths(lg_data_skill_rl)),
  rl_ad = unlist(lg_data_skill_rl),
  match_num = unlist(lapply(lengths(lg_data_skill_rl), seq_len)))

lg_df_speed_avr <- data.frame(
  player = rep(names(lg_data_speed_avr), lengths(lg_data_speed_avr)),
  speed_avr = unlist(lg_data_speed_avr),
  match_num = unlist(lapply(lengths(lg_data_speed_avr), seq_len)))

lg_df_damage_gvn <- data.frame(
  player = rep(names(lg_data_damage_gvn), lengths(lg_data_damage_gvn)),
  damage_gvn = unlist(lg_data_damage_gvn),
  match_num = unlist(lapply(lengths(lg_data_damage_gvn), seq_len)))

lg_df_lg <- data.frame(
  player = rep(names(lg_data_lg), lengths(lg_data_lg)),
  lg = unlist(lg_data_lg),
  match_num = unlist(lapply(lengths(lg_data_lg), seq_len)))

lg_df_ga <- data.frame(
  player = rep(names(lg_data_ga), lengths(lg_data_ga)),
  ga = unlist(lg_data_ga),
  match_num = unlist(lapply(lengths(lg_data_ga), seq_len)))
  
lg_df_ra <- data.frame(
  player = rep(names(lg_data_ra), lengths(lg_data_ra)),
  ra = unlist(lg_data_ra),
  match_num = unlist(lapply(lengths(lg_data_ra), seq_len)))
lg_df_ya <- data.frame(
  player = rep(names(lg_data_ya), lengths(lg_data_ya)),
  ya = unlist(lg_data_ya),
  match_num = unlist(lapply(lengths(lg_data_ya), seq_len)))
lg_df_mh <- data.frame(
  player = rep(names(lg_data_mh), lengths(lg_data_mh)),
  mh = unlist(lg_data_mh),
  match_num = unlist(lapply(lengths(lg_data_mh), seq_len)))
  # Show the result
#print(lg_df_ga, row.names = FALSE)
#print(lg_df_ya, row.names = FALSE)
#print(lg_df_ra, row.names = FALSE)
#print(lg_df_mh, row.names = FALSE)

merged_df  <- merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(
  lg_df_ga, lg_df_ya),lg_df_ra),lg_df_mh),lg_df_lg),lg_df_rl_ad),lg_df_frags),
  lg_df_rank),lg_df_spawnfrags),lg_df_damage_gvn),lg_df_speed_avr),lg_df_version),lg_df_ktxver),
  lg_df_client),lg_df_avg_ping),lg_df_avg_packetloss),lg_df_sv_antilag),lg_df_epoch)


write.csv(merged_df,"winners.csv")
#write.csv(merged_df,"loosers.csv")

# stop here with ERROR
exit(0)


merged_df <- read.csv("winners.csv")
merged_df <- read.csv("loosers.csv")
#merged_df_numeric <- merged_df[, sapply(merged_df, is.numeric)]
merged_df_numeric <- merged_df[,4:14]

print (merged_df_numeric)
cor_matrix <- cor(merged_df_numeric)
#print(cor_matrix)
corrplot(cor_matrix, method = "color")

cor_matrix <- cor(merged_df_numeric, method = "spearman")

cor_matrix <- cor(merged_df_numeric, method = "kendall")

cor_test_result <- cor.test(merged_df_numeric$ga,merged_df_numeric$ra)
print(cor_test_result)


melted_cor <- melt(cor_matrix)
ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0)

#write.csv(merged_df, file = "winners.csv")
#write.csv(merged_df, file = "loosers.csv")
#View (merged_df)

ggplot(lg_df, aes(x = lg)) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 0.5, 
                 fill = "lightblue", 
                 color = "black") +
  geom_density(alpha = 0.2, fill = "blue") +
  ggtitle("Distribution of LG Values") +
  xlab("LG Values") +
  ylab("Density")

hist(lg_df$lg, 
     main = "Distribution of LG Values",
     xlab = "LG Values",
     col = "lightblue",
     border = "black")

plot(density(lg_df$lg, na.rm = TRUE),
     main = "Density Plot of LG Values",
     xlab = "LG Values",
     col = "blue",
     lwd = 2)

summary(lg_df$lg)  # For quick quartiles and min/max
mean(lg_df$lg)     # Mean
median(lg_df$lg)   # Median
sd(lg_df$lg)       # Standard deviation
  
