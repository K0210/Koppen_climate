import pandas as pd
# 関数定義：気候区の判定
def get_kubun(df):
  # 気温と降水量から気候区分を判定
  # 必要情報の取得
  max_temp = df['月平均気温平年値'].max()
  min_temp = df['月平均気温平年値'].min()
  ave_temp = df['月平均気温平年値'].mean()
  max_rain = df['月平均降水量平年値'].max()
  min_rain = df['月平均降水量平年値'].min()
  ave_rain = df['月平均降水量平年値'].mean()  # 乾燥限界の計算
  months_above_10 = (df['月平均気温平年値'] > 10).sum()
  N_sm_list = [4,5,6,7,8,9]
  S_sm_list = [1,2,3,10,11,12]
  N_sm = df[df['月'].isin(N_sm_list)]
  S_sm = df[df['月'].isin(S_sm_list)]

  # 夏の月の判定
  if (max_temp==N_sm['月平均気温平年値'].max()):
    df_sm = N_sm
    df_wn = S_sm
  elif (max_temp>N_sm['月平均気温平年値'].max()):
    df_sm = S_sm
    df_wn = N_sm
  else:
     raise ValueError('夏季が特定できませんでした。')

  #f型,s型,w型の決定
  rain_year = df['月平均降水量平年値'].sum()
  rain_sm = df_sm['月平均降水量平年値'].sum()  # 年降水量に占める夏季降水量の割合ごとにパターン分類
  df_sm_min_rain = df_sm['月平均降水量平年値'].min()
  df_wn_min_rain = df_wn['月平均降水量平年値'].min()
  df_sm_max_rain = df_sm['月平均降水量平年値'].max()
  df_wn_max_rain = df_wn['月平均降水量平年値'].max()
  ratio = rain_sm/rain_year
  if (ratio>=0.7):
    # 冬季乾燥
    dry_pattern = 'w'
  elif (ratio>0.3):
    # 年中同程度
    dry_pattern = 'f'
  elif(ratio>0):
    # 夏季乾燥
    dry_pattern = 's'
  else:
    raise ValueError('年降水量に占める夏季降水量の割合ratioが不正値です。')

  # パターンごとに乾燥限界の判定
  if (dry_pattern=='s'):
    r = 20*ave_temp
  elif (dry_pattern=='f'):
    r = 20*(ave_temp + 7)
  elif (dry_pattern=='w'):
    r = 20*(ave_temp + 14)
  else:
    raise ValueError('乾燥限界の判定パターンdry_patternが不正値です。')

  # 温帯と冷帯の乾季の判定
  if (df_sm_min_rain < df_wn_min_rain)&(3*df_sm_min_rain < df_wn_max_rain)&(min_rain < 40):
    dry_season = 's'
  elif (df_sm_min_rain > df_wn_min_rain)&(10*df_wn_min_rain < df_sm_max_rain):
    dry_season = 'w'
  else:
    dry_season = 'f'
  # 温帯と冷帯の気温による小分類
  if (max_temp >= 22):
    temp_pattern = 'a'
  elif (months_above_10 >= 4):
    temp_pattern = 'b'
  elif (min_temp >= -38):
    temp_pattern = 'c'
  elif (min_temp < -38):
    temp_pattern = 'd'
  else:
    raise ValueError('気温による小分類temp_patternが不正値です。')

  # 気候帯の判定
  if (max_temp<10):
    # 最暖月平均気温が10℃未満
    zone = 'E' # 寒帯
  elif (r>rain_year):
    # 年平均降水量が乾燥限界未満
    zone = 'B' # 乾燥帯
  elif (min_temp>=18):
    # 最寒月平均気温18℃以上
    zone = 'A' # 熱帯
  elif (min_temp>-3):
    # 最寒月平均気温18℃未満-3℃超え
    zone = 'C' # 温帯
  elif (min_temp<=-3):
    # 最寒月平均気温-3℃以下
    zone = 'D' # 亜寒帯（冷帯）
  else:
    raise ValueError('気候帯の判定に失敗しました。')

  # 気候区分の判定
  if (zone=='A'):
    # 熱帯
    if (min_rain>=60):
      kubun = 'Af' # 熱帯雨林気候
    elif (min_rain>(100-0.4*ave_rain)):
      kubun = 'Am' # 熱帯モンスーン気候
    elif (min_rain==df_wn_min_rain):
      kubun = 'Aw' # サバナ気候
    elif (min_rain==df_sm_min_rain):
      kubun = 'As' # 熱帯夏季少雨気候
  elif (zone=='B'):
    # 乾燥帯
    if (0.5*r>rain_year):
      kubun = 'BW' # 砂漠気候
    else:
      kubun = 'BS' # ステップ気候
    if (ave_temp>=18):
      kubun = kubun + 'h'
    else:
      kubun = kubun + 'k'
  elif (zone=='C')|(zone=='D'):
    # 温帯または亜寒帯（冷帯）
    kubun = zone + dry_season + temp_pattern # 温帯または冷帯の気候区
  elif (zone=='E'):
    # 寒帯
    if (max_temp>=0):
      kubun = 'ET' # ツンドラ気候
    else:
      kubun = 'EF' # 氷雪気候
  return kubun
# 関数定義：気候区の日本語名
def kubun_name(kubun):
  if (kubun[:1] == 'A'):
    if (kubun[1] == 'f'):
      A = '熱帯雨林気候'
    elif (kubun[1] == 'm'):
      # 弱い乾季のある熱帯雨林性気候ともいう。
      A = '熱帯モンスーン気候'
    elif (kubun[1] == 's')|(kubun[1] == 'w'):
      # Asは厳密には熱帯夏季少雨気候だが、サバナ気候とあまり違いがない。
      A = 'サバナ気候'
  elif (kubun[:1] == 'B'):
    if (kubun[1] == 'W'):
      A = '砂漠気候'
    elif (kubun[1] == 'S'):
      A = 'ステップ気候'
  elif (kubun[:1] == 'C'):
    if (kubun[1] == 's'):
      A = '地中海性気候'
    elif (kubun[1] == 'w'):
      A = '温帯夏雨気候'
    elif (kubun[1] == 'f'):
      if (kubun[2] == 'a'):
        A = '温暖湿潤気候'
      elif (kubun[2] == 'b')|(kubun[2] == 'c'):
        A = '西岸海洋性気候'
  elif (kubun[:1] == 'D'):
    if (kubun[1] == 's'):
      A = '高地地中海性気候'
    elif (kubun[1] == 'w'):
      A = '冷帯夏雨気候'
    elif (kubun[1] == 'f'):
      A = '冷帯湿潤気候'
  elif (kubun[:1] == 'E'):
    if (kubun[1] == 'T'):
      A = 'ツンドラ気候'
    elif (kubun[1] == 'F'):
      A = '氷雪気候'
  return A
# サンプルDataFrameの作成（実際のデータに合わせて調整してください）
sample_data = {
    '月': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12],
    '月平均気温平年値': [-5.0, -3.0, 2.0, 8.0, 15.0, 20.0, 25.0, 24.0, 18.0, 10.0, 4.0, -1.0],
    '月平均降水量平年値':[10,20,30,40,50,60,70,80,90,50,30,10]
}
sample_df = pd.DataFrame(sample_data)

kikou = get_kubun(sample_df)
kikou_name = kubun_name(kikou)
print(kikou,kikou_name)
