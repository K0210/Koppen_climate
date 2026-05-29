library(dplyr)

# 関数定義：気候区の判定
get_kubun <- function(df) {
  # df は '月', '月平均気温平年値', '月平均降水量平年値' を持つデータフレームと仮定
  
  max_temp <- max(df$月平均気温平年値)
  min_temp <- min(df$月平均気温平年値)
  ave_temp <- mean(df$月平均気温平年値)
  max_rain <- max(df$月平均降水量平年値)
  min_rain <- min(df$月平均降水量平年値)
  ave_rain <- mean(df$月平均降水量平年値)
  months_above_10 <- sum(df$月平均気温平年値 > 10)
  
  N_sm_list <- c(4, 5, 6, 7, 8, 9)
  S_sm_list <- c(1, 2, 3, 10, 11, 12)
  
  N_sm <- df %>% filter(月 %in% N_sm_list)
  S_sm <- df %>% filter(月 %in% S_sm_list)
  
  # 夏の月の判定
  if (max_temp == max(N_sm$月平均気温平年値)) {
    df_sm <- N_sm
    df_wn <- S_sm
  } else if (max_temp > max(N_sm$月平均気温平年値)) {
    df_sm <- S_sm
    df_wn <- N_sm
  } else {
    stop('夏季が特定できませんでした。')
  }
  
  # 乾燥限界・季節判定用パラメータ
  rain_year <- sum(df$月平均降水量平年値)
  rain_sm <- sum(df_sm$月平均降水量平年値)
  df_sm_min_rain <- min(df_sm$月平均降水量平年値)
  df_wn_min_rain <- min(df_wn$月平均降水量平年値)
  df_sm_max_rain <- max(df_sm$月平均降水量平年値)
  df_wn_max_rain <- max(df_wn$月平均降水量平年値) 

  # 乾燥限界判定の前に必要
  ratio <- rain_year / rain_summer
  if (ratio > 0.3) { 
    dry_pattern == 's'
  } else if (ratio < 0.7){
    dry_pattern == 'w'
  } else {
    dry_pattern == 'f'
  }

  
  # 乾燥限界の計算 (dry_patternに依存)
  r <- case_when(
    dry_pattern == 's' ~ 20 * ave_temp,
    dry_pattern == 'f' ~ 20 * (ave_temp + 7),
    dry_pattern == 'w' ~ 20 * (ave_temp + 14),
    TRUE ~ stop('乾燥限界の判定パターンが不正です')
  )

    # 温帯と冷帯の乾季の判定 
  if ((df_sm_min_rain < df_wn_min_rain) & (3 * df_sm_min_rain < df_wn_max_rain) & (min_rain < 40)) {
    dry_season <- 's'
  } else if ((df_sm_min_rain > df_wn_min_rain) & (10 * df_wn_min_rain < df_sm_max_rain)) {
    dry_season <- 'w'
  } else {
    dry_season <- 'f'
  }
  
  # 気温による小分類
  temp_pattern <- if (max_temp >= 22) 'a'
  else if (months_above_10 >= 4) 'b'
  else if (min_temp >= -38) 'c'
  else 'd'
  
  # 気候帯の判定
  zone <- if (max_temp < 10) 'E'
  else if (r > rain_year) 'B'
  else if (min_temp >= 18) 'A'
  else if (min_temp > -3) 'C'
  else 'D'
  
  # 気候区分の生成
  kubun <- case_when(
    zone == 'A' ~ {
      if (min_rain >= 60) 'Af'
      else if (min_rain > (100 - 0.4 * ave_rain)) 'Am'
      else if (min_rain == df_wn_min_rain) 'Aw'
      else 'As'
    },
    zone == 'B' ~ {
      base <- if (0.5 * r > rain_year) 'BW' else 'BS'
      paste0(base, if (ave_temp >= 18) 'h' else 'k')
    },
    zone %in% c('C', 'D') ~ paste0(zone, dry_season, temp_pattern),
    zone == 'E' ~ if (max_temp >= 0) 'ET' else 'EF'
  )
  
  return(kubun)
}

# 関数定義：気候区の日本語名
get_kubun_name <- function(kubun) {
  first <- substr(kubun, 1, 1)
  second <- substr(kubun, 2, 2)
  
  if (first == 'A') {
    if (second == 'f') '熱帯雨林気候'
    else if (second == 'm') '熱帯モンスーン気候'
    else 'サバナ気候'
  } else if (first == 'B') {
    if (second == 'W') '砂漠気候' else 'ステップ気候'
  } else if (first == 'C') {
    if (second == 's') '地中海性気候'
    else if (second == 'w') '温帯夏雨気候'
    else if (substr(kubun, 3, 3) == 'a') '温暖湿潤気候'
    else '西岸海洋性気候'
  } else if (first == 'D') {
    if (second == 's') '高地地中海性気候'
    else if (second == 'w') '冷帯夏雨気候'
    else '冷帯湿潤気候'
  } else if (first == 'E') {
    if (second == 'T') 'ツンドラ気候' else '氷雪気候'
  }
}

