library(pacman)
p_load("dplyr", "ggplot2", "this.path")
p_load("glue", "treemap", "devtools")
p_load("stringr","tidyr")

#devtools::install_github("timelyportfolio/d3treeR")
library("d3treeR")

setwd(dirname(this.path()))
getwd()

data_ori <- read.csv("data.csv")


# --- preprocessing ---

data <- data_ori
names(data) <- c("date", "sex", "age", "education", "salary", "job",
                     "phone", "music_style", "music_time",
                     "music_free_platform", "music_paid_platform", "music_device",
                     "music_per_day", "wish_style", "music_with_things",
                     "ads_acceptance_level", "ads_remove_and_screen_off_listening_accepted_price",
                     "guitar_preference", "guitar_listening", "guitar_playing",
                     "guitar_interest", "know_lofi", "lofi_perference", "lofi_listening",
                     "guitar_avatar_appeal", "guitar_avatar_streaming_appeal", 
                     "guitar_avatar_song_request_appeal",
                     "lyrics_generation_increase_song_request",
                     "paid_song_request_appeal", "pay_for_guitar_tabs_generation", 
                     "pay_for_guitar_tabs_playing", "service_increase_guitar_interest",
                     "one_month_free_trial_increase_service_interest",
                     "service_accepted_price_per_month", "other_thoughts_for_service")

str(data)

# integrate moblie OS
data <- data %>%
  mutate(phone = ifelse(phone != "沒手機" & phone != "安卓" & phone != "ios", "安卓", phone))

# count keyword of music_style
keywords_list <- strsplit(as.character(data$music_style), ", ")

keyword_counts <- table(unlist(keywords_list))
keyword_data <- data.frame(keyword = names(keyword_counts), count = as.numeric(keyword_counts))
keyword_data <- data.frame(keyword = rep(keyword_data$keyword, times = keyword_data$count))
keyword_data

count_keyword <- function(data) {
  keywords_list <- strsplit(as.character(data), ", ")
  keyword_counts <- table(unlist(keywords_list))
  keyword_data <- data.frame(keyword = names(keyword_counts), count = as.numeric(keyword_counts))
  keyword_data <- data.frame(keyword = rep(keyword_data$keyword, times = keyword_data$count))
  return(keyword_data)
}
# use count_keyword(data)

# handle Lofi issue
lofi_perference_know_3 <- data[data$know_lofi>2, ]$lofi_perference
lofi_listening_know_3 <- data[data$know_lofi>2, ]$lofi_listening


# --- bar plot ---

draw_bar_plot <- function (data, x_lab = "", y_lab = "數量", cumulative = F, prob = T) {
  data <- as.data.frame(table(data))
  
  names(data)[1] <- c("Var1") 
  
  total <- sum(data$Freq)
  
  if (cumulative == T) {
    data <- data %>% 
      mutate(Freq = cumsum(Freq))
  }
  
  if (prob == T) {
    data <- data %>% 
      mutate(Freq_text = sprintf("%d (%.1f%%)", Freq, (Freq / total) * 100))
  }
  
  ggplot(data=data, aes(x = Var1, y = Freq)) +
    geom_bar(stat="identity", fill="steelblue") +
    labs(x = x_lab, y = y_lab) +
    geom_text(aes(label = Freq_text), vjust=1.3, color="white", size=3.5) +
    theme_minimal()
}

draw_bar_plot(data$guitar_interest, x_lab = "您有興趣學彈吉他", y_lab = "", cumulative = T)
draw_bar_plot(data$guitar_listening, x_lab = "你平常有聽他人彈吉他", y_lab = "", cumulative = T)

draw_bar_plot(lofi_perference_know_3, x_lab = "喜歡 Lofi 的程度", y_lab = "", cumulative = T)
draw_bar_plot(lofi_listening_know_3, x_lab = "平常播放 Lofi 的習慣", y_lab = "", cumulative = T)


# ---  stacked bar plot ---

service_1 <- as.data.frame(table(data$guitar_avatar_song_request_appeal))
service_2 <- as.data.frame(table(data$pay_for_guitar_tabs_generation))
service_3 <- as.data.frame(table(data$pay_for_guitar_tabs_playing))

combined_data <- data.frame(
  Var1 = rep(1:5, 3),
  Freq = c(service_1$Freq, service_2$Freq, service_3$Freq),
  Service = rep(c("直播點曲", "歌曲生成吉他譜", "AI虛擬人物吉他教學"), each = 5)
)

ggplot(combined_data, aes(x = Service, y = Freq, fill = as.factor(Var1))) +
  geom_col() +
  labs(title = "服務興趣比較",
       x = "服務種類",
       y = "",
       fill = "興趣程度") +
  scale_fill_manual(values = c("1" = "#9dbdb8",
                               "2" = "#db9c84",
                               "3" = "#7091db",
                               "4" = "#f578c4",
                               "5" = "#c9fa78")) +
  theme_minimal()

# --- pie chart ---

draw_pie_chart <- function(data, lab_name="") {
  
  data <- as.data.frame(table(data))
  names(data)[1] <- c("Var1") 
  
  data <- data %>% 
    arrange(desc(Var1)) %>%
    mutate(prop = Freq / sum(data$Freq) *100)
    # mutate(ypos = cumsum(prop)- 0.5*prop )
  
  chart <- ggplot(data, aes(x="", y=prop, fill=Var1)) +
    geom_bar(stat="identity", width=1, color="white") +
    geom_text(aes(label = glue("{round(prop)}%")),
              position = position_stack(vjust = 0.5), color = "white", size=4) +
    coord_polar(theta = "y") +
    guides(fill=guide_legend(title=lab_name)) +
    theme_void()
  
  return(chart)
}


# --- tree map ---

get_tree_data <- function (vars, vars_name) {
  tree_data <- data.frame()
  
  for (i in seq(1:length(vars))) {
    temp <- as.data.frame(table(vars[i]))
    names(temp) <- c("subgroup", "value") 
    temp$group <- vars_name[i]
    
    tree_data <- rbind(tree_data, temp)
  }
  
  return(tree_data)
}

draw_tree_map <- function(map_name, vars, vars_name, interactive = TRUE) {
  
  tree_data <- get_tree_data(vars, vars_name)
  
  basic_tree_map <- treemap(tree_data,
                            index=c("group","subgroup"),
                            vSize="value",
                            type="index",
                            palette = "Set2",
                            bg.labels=c("white"),
                            align.labels=list(
                              c("center", "center"), 
                              c("right", "bottom")
                            )
  )            
  
  if (interactive == FALSE){
    return(basic_tree_map)
  }
  
  interactive_tree_map <- d3tree2(basic_tree_map, rootname = map_name)
  return(interactive_tree_map)
}

# tree_map - basic background data
vars <- c(data %>% select(sex:phone))
vars_name <- c("性別", "年齡", "教育程度", "薪資", "職業", "手機系統")
basic_data_tree_map <- draw_tree_map("基本資料", vars, vars_name)
basic_data_tree_map

# tree_map - music survey v1
vars <- c(data %>% select(music_style:music_per_day))
vars_name <- c("喜好風格", "聆聽時刻", "偏好平台(免費)", "偏好平台(付費)", "聆聽方式", "聆聽時長(每日)")
music_data_tree_map <- draw_tree_map("音樂調查", vars, vars_name)
music_data_tree_map

# tree_map - music survey v2
vars <- c(count_keyword(data$music_style),
          count_keyword(data$music_time),
          count_keyword(data$music_free_platform),
          count_keyword(data$music_paid_platform),
          count_keyword(data$music_device),
          data%>%select(music_per_day))
vars_name <- c("喜好風格", "聆聽時刻", "偏好平台(免費)", "偏好平台(付費)", "聆聽方式", "聆聽時長(每日)")
music_data_tree_map <- draw_tree_map("音樂調查", vars, vars_name)
music_data_tree_map

