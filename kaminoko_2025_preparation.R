# パッケージ読み込み----
  
library(tidyverse)
library(patchwork)
library(corrplot) # 相関行列を求める
library(janitor) # 変数名をきれいにするためのパッケージ


# データインポートと結合----

## 結合前データ読み込み
df1 <- read_csv("kaminoko2025_0608.csv") 
df2 <- read_csv("kaminoko2025_0614-15.csv")
df3 <- read_csv("kaminoko2025_0719-20.csv")


## 変数名つけなおし（結合する変数のみ）----
df1 <- df1 |> clean_names() # rename()時にエラーが出ないよう変数名をきれいにしておく

df1 <- df1 |> rename(
  questionnaire_no = anketono_shi_bie_sono1_tongshi_fan_hao, 
  date = diao_zha_ri,
  v1_sex = q1_xing_bie,
  v2_gen = q2_nian_ling,
  v3_hokkaido = q3_o_zhumai,
  v3_hokkaido_resi = q3_1_shi_ting_cun_ming_bei_hai_dao_nei,
  v3_not_hokkaido_resi = q3_2_dou_dao_fu_xian_ming_bei_hai_dao_wai ,
  v4_hokkaido_visits = q4_koremadeno_bei_hai_daoheno_lu_xing_fang_wen_chu_zhangmo_hanmete,
  v5_companions_cnt = q5_tong_xing_zheno_ren_shu_anata_zi_shenwo_hanmeta_ren_shu,
  v6_kaminoko_visits = q6_shenno_zi_chiheno_fang_wen,
  v7 = q7_shenno_zi_chiwo_fang_wenshitakikkake, # ここはあとでダミー変数に分解する
  v8_scenery_expectation = q8_1_shenno_zi_chiheno_qi_dai_du_jing_seno_meishisaya_zi_ran_nitsuite,
  v8_general_expectation = q8_2_shenno_zi_chiheno_qi_dai_du_guan_guang_detoshiteno_shenno_zi_chiheno_zong_he_de_man_zu_du_nitsuite,
  v9_scenery_satisfaction =  q9_1_shenno_zi_chiheno_man_zu_du_jing_seno_meishisaya_zi_ran_ti_yan_nitsuite,
  v9_access_satisfaction = q9_2_shenno_zi_chiheno_man_zu_du_chimadenoakusesu_nitsuite,
  v9_facility_satisfaction = q9_3_shenno_zi_chiheno_man_zu_du_toire_zhu_che_changnadono_she_bei_nitsuite,
  v9_tourist_spot_satisfaction = q9_4_shenno_zi_chiheno_man_zu_du_zhou_bianno_guan_guangsupottono_chong_shi_du_nitsuite,
  v9_general_satisfaction = q9_5_shenno_zi_chiheno_man_zu_du_shenno_zi_chini_duisuru_zong_he_dena_man_zu_du_nitsuite,
  v11_revisit_intention = q10_shenno_zi_chiwomata_fang_wenshitaito_siimasuka,
  v12 = q11_qing_li_tingno_ren_to_jiao_liushitaito_siimasuka, # ここはあとでダミー変数に分解する
  v13_citizen_consumer = q12_2_zhi_wennotaipu_shuinitotteno_si_zhiwo_weniteiruka,
  v14_bid1_wtp = q12_1_shenno_zi_chi_no_guan_guang_zi_yuantoshiteno_si_zhiwo_ping_sisurutameno_zhi_wen_ti_shi_e, # あとでここからv14_bid2_yes_wtpとv14_bid2_no_wtpをつくる
  v14_bid1 = q13_1_di1hui_zhi_fani_yi_si,
  v14_bid2_yes = q13_2_di2hui_zhi_fani_yi_si_di1hui_zhi_fanu,
  v14_bid2_no = q13_3_di2hui_zhi_fani_yi_si_di1hui_zhi_fanwanai,
  v14_reason = q13_4_zhi_fanitakunai_li_you # あとでダミー変数に分解
)

df2 <- df2 |> clean_names() # rename()時にエラーが出ないよう変数名をきれいにしておく

df2 <- df2 |> rename(
  questionnaire_no = anketono_shi_bie_sono1_tongshi_fan_hao, 
  date = diao_zha_ri,
  v1_sex = q1_xing_bie,
  v2_gen = q2_nian_ling,
  v3_hokkaido = q3_o_zhumai,
  v3_hokkaido_resi = q3_1_shi_ting_cun_ming_bei_hai_dao_nei,
  v3_not_hokkaido_resi = q3_2_dou_dao_fu_xian_ming_bei_hai_dao_wai,
  v4_hokkaido_visits = q4_koremadeno_bei_hai_daoheno_lu_xing_fang_wen_chu_zhangmo_hanmete,
  v5_companions_cnt = q5_tong_xing_zheno_ren_shu_anata_zi_shenwo_hanmeta_ren_shu,
  v6_kaminoko_visits = q6_shenno_zi_chiheno_fang_wen,
  v7 = q7_shenno_zi_chiwo_fang_wenshitakikkake, # ここはあとでダミー変数に分解する
  v8_scenery_expectation = q8_1_shenno_zi_chiheno_qi_dai_du_jing_seno_meishisaya_zi_ran_nitsuite,
  v8_general_expectation = q8_2_shenno_zi_chiheno_qi_dai_du_guan_guang_detoshiteno_shenno_zi_chiheno_zong_he_de_man_zu_du_nitsuite,
  v9_scenery_satisfaction =  q9_1_shenno_zi_chiheno_man_zu_du_jing_seno_meishisaya_zi_ran_ti_yan_nitsuite,
  v9_access_satisfaction = q9_2_shenno_zi_chiheno_man_zu_du_chimadenoakusesu_nitsuite,
  v9_facility_satisfaction = q9_3_shenno_zi_chiheno_man_zu_du_toire_zhu_che_changnadono_she_bei_nitsuite,
  v9_tourist_spot_satisfaction = q9_4_shenno_zi_chiheno_man_zu_du_zhou_bianno_guan_guangsupottono_chong_shi_du_nitsuite,
  v9_general_satisfaction = q9_5_shenno_zi_chiheno_man_zu_du_shenno_zi_chini_duisuru_zong_he_dena_man_zu_du_nitsuite,
  v11_revisit_intention = q10_shenno_zi_chiwomata_fang_wenshitaito_siimasuka,
  v12 = q11_qing_li_tingno_ren_to_jiao_liushitaito_siimasuka, # ここはあとでダミー変数に分解する
  v13_citizen_consumer = q12_2_zhi_wennotaipu_shuinitotteno_si_zhiwo_weniteiruka,
  v14_bid1_wtp = q12_1_shenno_zi_chi_no_guan_guang_zi_yuantoshiteno_si_zhiwo_ping_sisurutameno_zhi_wen_ti_shi_e, # あとでここからv14_bid2_yes_wtpとv14_bid2_no_wtpをつくる
  v14_bid1 = q13_1_di1hui_zhi_fani_yi_si,
  v14_bid2_yes = q13_2_di2hui_zhi_fani_yi_si_di1hui_zhi_fanu,
  v14_bid2_no = q13_3_di2hui_zhi_fani_yi_si_di1hui_zhi_fanwanai,
  v14_reason = q13_4_zhi_fanitakunai_li_you # あとでダミー変数に分解
)


df3 <- df3 |> clean_names() # rename()時にエラーが出ないよう変数名をきれいにしておく

df3 <- df3 |> rename(
  questionnaire_no = anketono_shi_bie_sono1_tongshi_fan_hao, 
  date = diao_zha_ri,
  v1_sex = q1_xing_bie,
  v2_gen = q2_nian_ling,
  v3_hokkaido = q3_o_zhumai,
  v3_hokkaido_resi = q3_1_shi_ting_cun_ming_bei_hai_dao_nei,
  v3_not_hokkaido_resi = q3_2_dou_dao_fu_xian_ming_bei_hai_dao_wai,
  v4_hokkaido_visits = q4_koremadeno_bei_hai_daoheno_lu_xing_fang_wen_chu_zhangmo_hanmete,
  v5_companions_cnt = q5_tong_xing_zheno_ren_shu_anata_zi_shenwo_hanmeta_ren_shu,
  v6_kaminoko_visits = q6_shenno_zi_chiheno_fang_wen,
  v9_scenery_satisfaction = q8_1_shenno_zi_chiheno_man_zu_du_jing_seno_meishisaya_zi_ran_ti_yan_nitsuite,
  v9_access_satisfaction = q8_2_shenno_zi_chiheno_man_zu_du_chimadenoakusesu_nitsuite,
  v9_facility_satisfaction = q8_3_shenno_zi_chiheno_man_zu_du_toire_zhu_che_changnadono_she_bei_nitsuite,
  v9_tourist_spot_satisfaction = q8_4_shenno_zi_chiheno_man_zu_du_zhou_bianno_guan_guangsupottono_chong_shi_du_nitsuite,
  v9_general_satisfaction = q8_5_shenno_zi_chiheno_man_zu_du_shenno_zi_chini_duisuru_zong_he_dena_man_zu_du_nitsuite,
  v10_kaminoko_scenery_satis = q9_1_ge_yao_suno_man_zu_du_chi_zi_tino_meishisa_nitsuite,
  v10_kaminoko_silence_satis = q9_2_ge_yao_suno_man_zu_du_jingkesa_nitsuite,
  v10_kaminoko_forest_satis = q9_3_ge_yao_suno_man_zu_du_senno_likasa_nitsuite,
  v10_kaminoko_achievement_satis = q9_4_ge_yao_suno_man_zu_du_tadori_zheita_da_cheng_gan_nitsuite,
  v13_citizen_consumer = q7_zhi_wennotaipu_shuinitotteno_si_zhiwo_weniteiruka,
  v14_bid1_wtp = q7_chu_hui_ti_shi_e, # あとでここからv14_bid2_yes_wtpとv14_bid2_no_wtpをつくる
  v14_bid1 = q7_1_di1hui_zhi_fani_yi_si,
  v14_bid2_yes = q7_2_di2hui_zhi_fani_yi_si_di1hui_zhi_fanu,
  v14_bid2_no = q7_3_di2hui_zhi_fani_yi_si_di1hui_zhi_fanwanai,
  v14_reason = q7_4_zhi_fanitakunai_li_you,  # あとでダミー変数に分解
  v15_critical_access_attitude = q10_1_zhi_zaiwo_fanggeruka_guangi_dao_luno_zheng_bei,
  v15_critical_parking_attitude = q10_2_zhi_zaiwo_fanggeruka_zhu_che_changno_zheng_bei,
  v15_critical_crowd_attitude = q10_3_zhi_zaiwo_fanggeruka_da_shino_tano_fang_wen_keni_chu_huiu,
  v15_critical_shop_attitude = q10_4_zhi_zaiwo_fanggeruka_mai_diangaaru,
  v15_critical_photospot_attitude = q10_5_zhi_zaiwo_fanggeruka_xie_zhensupottono_kan_bangaaru
)

# dateに日付未記入のものがあるが、実施タイミングはわかるので識別変数を入れておく
df1 <- df1 |> 
  mutate(survey_timing = "6月上旬") 

df2 <- df2 |> 
  mutate(survey_timing = "6月中旬")

df3 <- df3 |> 
  mutate(survey_timing = "7月下旬") 


# 結合前にすべて文字列型に変換（型が同じでないと結合できない→満足度などはあとで数値型に直す）
df1 <- df1 |> mutate(across(everything(), as.character))
df2 <- df2 |> mutate(across(everything(), as.character))
df3 <- df3 |> mutate(across(everything(), as.character))

df <- bind_rows(df1, df2, df3)



# 再コーディング----

## 欠損をNAに----


# 欠損をすべてNAに置換
library(stringr) # str_trim()を使うために読み込む
df <- df |>
  mutate(
    across(
      everything(),
      ~ {
        x <- str_trim(.) # 文字列前後の空白を削除
        ifelse(x %in% c("未回答", "【未回答】", "【未回答」", "不明", "NA", "", "その他（未回答）", "【未回答】;", "未回答;"), NA, x)
      }
    )
  )




## id----



# 現在のデータの行番号でidを上書き
df <- df |> 
  mutate(
    id = row_number()
  ) |> 
  relocate(id)  # idを先頭列に移動


## date, date_detail----


# date 日付をファクタ化してレベル設定
df <- df |> 
  mutate(
    date = case_when(
      survey_timing == "6月上旬"  ~ "6月8日" ,
      survey_timing == "6月中旬" ~ "6月14-15日",
      survey_timing == "7月下旬" ~ "7月20-21日" ,
      TRUE ~ NA_character_
    ),
    date = factor(date, levels =c("6月8日", "6月14-15日", "7月20-21日"))
  )


# 午前午後の区別もいれたdate_detail変数をつくる
df <- df |> 
  mutate(
    date_detail = factor(date, levels = c("6月8日午前", "6月8日午後", "6月14日午前",  "6月14日午後", "6月15日午後", "7月20日午後", "7月21日午前", "7月21日午後"))
  ) 





## v3----



# v3 北海道内（外）と道内（外）が混じっていたので道内（外）に統一
df <- df |> 
  mutate(
    v3_hokkaido = case_when(
      v3_hokkaido == "北海道内" ~ "道内",
      v3_hokkaido == "北海道外" ~ "道外",
      TRUE ~ v3_hokkaido
    )
  ) 


## v4----


# v4 値がばらばらなので揃える（最大4回目以上）
df <- df |>
  mutate(
    v4_hokkaido_visits = if_else(
      v4_hokkaido_visits %in% c("10回", "１０回", "10回以上", "10回位", "10数回", "13回目くらい", "15年行き来している", "４回", "4回目", "5回目", "6", "6回", "6回目", "６回目", "7回目", "たくさん", "多数回", "沢山"),
      "4回目以上",
      v4_hokkaido_visits
    )
  )

# 回答が回数でないもの、特定できないものはNAに
df <- df |> 
  mutate(
    v4_hokkaido_visits = if_else(
      v4_hokkaido_visits %in% c("なし", "在住", "実家が札幌", "複数回目", "道産子"),
      NA_character_,
      v4_hokkaido_visits
    )
  )

# 3～5回は低めに見積もって3回とする
df <- df |> 
  mutate(
    v4_hokkaido_visits = if_else(
      v4_hokkaido_visits %in% c("3～5回", "3") ,  
      "3回目",
      v4_hokkaido_visits
    )
  )

# 最後にレベルをつける
df <- df |> 
  mutate(
    v4_hokkaido_visits = factor(v4_hokkaido_visits, level = c("はじめて", "2回目", "3回目", "4回目以上"))
  ) 




## v5----



# v5 「1人（同行者はいない）」が冗長なので「1人」に変換。また、最大5人以上に統一しよう
df <- df |> 
  mutate(
    v5_companions_cnt = case_when(
      v5_companions_cnt == "1人（同行者はいない）" ~ "1人",
      TRUE ~ v5_companions_cnt
    )
  )

# 5人以上に統一
df <- df |>
  mutate(
    v5_companions_cnt = if_else(
      v5_companions_cnt %in% c("5人", "6", "7人", "７人"),
      "5人以上",
      v5_companions_cnt
    )
  )


# 最後にレベルをつける
df <- df |> 
  mutate(
    v5_companions_cnt = factor(v5_companions_cnt, level = c("1人", "2人", "3人", "4人", "5人以上"))
  ) 




## v6----


# v6 神の子訪問回数が3回目以上ならすべて「3回目以上」に統一
df <- df |>
  mutate(
    v6_kaminoko_visits = if_else(
      v6_kaminoko_visits %in% c("3回目", "4回以上", "4回目以上", "５回", "何十回も"),
      "3回目以上",
      v6_kaminoko_visits
    )
  )


# 最後にレベルをつける
df <- df |> 
  mutate(
    v6_kaminoko_visits = factor(v6_kaminoko_visits, level = c("はじめて", "2回目", "3回目以上"))
  ) 





## v7----



others_in_v7 <- c("Facebook", "通りがかり", "お客様案内", "その他(ネット)", "とおりがかりで", "もともと知っていた",
                  "グーグルマップを見て以前から気になっていた", "グーグルマップ", "前々から知っていた。初めての人をドライブに誘った",
                  "前回美しかったから", "友人から", "友人の紹介", "同伴", "友人", "摩周湖で看板を見て", "道外からの親戚のアランド",
                  "道路の看板を見て", "２０年前に来た", "SNS")

pat <- str_c(str_escape(others_in_v7), collapse = "|")

# v7から自由記述を取り除き、v7_otherをつくる
df <- df |>
  mutate(
    v7_other = if_else(
      str_detect(replace_na(v7, ""), regex(pat)),
      str_extract(replace_na(v7, ""), regex(pat)),
      NA_character_
    ),
    v7_new = v7 |>
      replace_na("") |>
      str_remove_all(regex(pat)) |>
      str_replace_all("\\s*;\\s*", ";") |>
      str_replace_all(";{2,}", ";") |>
      str_replace_all("^;|;$", "") |>
      na_if(""),
    v7_new = str_replace_all(v7_new, 
                             c("同行者以外からの話をいい手" = "同行者以外から話を聞いて",
                               "同行者以外からの話を聞いて" = "同行者以外から話を聞いて",
                               "旅行雑誌やガイドブックを見て" = "旅行雑誌・ガイドブックを見て"))
  )



# v7をダミー変数に
v7_dummy <- df |> 
  separate_rows(v7_new, sep = ";") |> 
  mutate(v7_new = trimws(v7_new)) |> 
  filter(!is.na(v7_new), v7_new != "") |> 
  mutate(dummy = 1) |> 
  pivot_wider(
    id_cols = id,
    names_from = v7_new,
    values_from = dummy,
    values_fill = 0
  ) 

# 元のdfに結合
df <- df |> 
  left_join(v7_dummy, by = "id")


# ダミー変数の名前を変更
df <- df |> 
  rename(
    "v7_web" = "清里町・観光協会のWebサイトを見て",
    "v7_other_web" = "清里町・観光協会以外のWebサイトを見て",
    "v7_web_reviews" = "インターネットの口コミ（Xなど）を見て",
    "v7_tv" = "テレビの旅番組・情報番組を見て",
    "v7_travel_agency" = "旅行会社の紹介（パンフレットも含む）",
    "v7_magazine" = "旅行雑誌・ガイドブックを見て",
    "v7_companion_suggestion" = "今回の同行者の提案",
    "v7_non_companion_info" = "同行者以外から話を聞いて"
  )



## v8, v9, v10----

# 神の子池への期待度、満足度を数値型に変換
df <- df |> 
  mutate(
    v8_scenery_expectation = as.numeric(v8_scenery_expectation),
    v8_general_expectation = as.numeric(v8_general_expectation),
    v9_scenery_satisfaction = as.numeric(v9_scenery_satisfaction),
    v9_access_satisfaction = as.numeric(v9_access_satisfaction),
    v9_facility_satisfaction = as.numeric(v9_facility_satisfaction),
    v9_tourist_spot_satisfaction = as.numeric(v9_tourist_spot_satisfaction),
    v9_general_satisfaction = as.numeric(v9_general_satisfaction),
    v10_kaminoko_scenery_satis = as.numeric(v10_kaminoko_scenery_satis),
    v10_kaminoko_silence_satis = as.numeric(v10_kaminoko_silence_satis),
    v10_kaminoko_forest_satis = as.numeric(v10_kaminoko_forest_satis),
    v10_kaminoko_achievement_satis = as.numeric(v10_kaminoko_achievement_satis)
  ) 



## v12----


# 自由回答はない（ほとんどない）ので処理はしない
# v12のおかしな値（入力時に入れたメモみたいなの）を取り除く
freaks_in_v12 <- c("その他に丸はされてるが理由は未回答", "日数の？がない", "なかなか来られないので", "近くに来た時に何かあれば")

pat <- str_c(str_escape(freaks_in_v12), collapse = "|")
df <- df |>
  mutate(
    v12_new = v12 |>
      replace_na("") |>
      str_remove_all(regex(pat)) |>
      str_replace_all("\\s*;\\s*", ";") |>
      str_replace_all(";{2,}", ";") |>
      str_replace_all("^;|;$", "") |>
      na_if("")
  )


# v12_newをダミー変数に
v12_dummy <- df |> 
  separate_rows(v12_new, sep = ";") |> 
  mutate(v12_new = trimws(v12_new)) |> 
  filter(!is.na(v12_new), v12_new != "") |> 
  mutate(dummy = 1) |> 
  pivot_wider(
    id_cols = id,
    names_from = v12_new,
    values_from = dummy,
    values_fill = 0
  )

# 元のdfに結合
df <- df |> 
  left_join(v12_dummy, by = "id")

# ダミー変数の名前を変更
df <- df |> 
  rename(
    "v12_interact_by_event" = "町のイベントがあれば参加したい",
    "v12_interact_by_market" = "直売の場があれば商品を見てみたい",
    "v12_interact_by_guesthouse" = "民宿があれば泊まってみたい",
    "v12_unwilling_to_interact" = "特に交流したいとは思わない",
    "v12_unaware_of_location" = "神の子池が清里町にあると意識していなかった"
  )




## v13----


# v13 1,2のままだと使いにくいので、カテゴリー名をかえる。
df <- df |> 
  mutate(
    v13_citizen_consumer = case_when(
      v13_citizen_consumer == "あなた+他の訪問者+町の住民にとっての価値" ~ "市民選好",
      v13_citizen_consumer == "あなたにとっての価値" ~ "消費者選好",
      TRUE ~ NA_character_
    )
  )



## v14----


# 初回提示額から「円」を取り除き数値型に変換
df$v14_bid1_wtp <-  str_remove(df$v14_bid1_wtp,"円") |> as.numeric()


# v14_bid1_wtpからv14_bid2_yes_wtpとv14_bid2_no_wtpをつくる
df <- df |> 
  mutate(
    v14_bid2_yes_wtp = case_when(
      v14_bid1_wtp == 100 ~ 300,
      v14_bid1_wtp == 300 ~ 500,
      v14_bid1_wtp == 500 ~ 1000,
      v14_bid1_wtp == 1000 ~ 1500,
      TRUE ~ v14_bid1_wtp,
    ),
    v14_bid2_no_wtp = case_when(
      v14_bid1_wtp == 100 ~ 50,
      v14_bid1_wtp == 300 ~ 100,
      v14_bid1_wtp == 500 ~ 300,
      v14_bid1_wtp == 1000 ~ 500,
      TRUE ~ v14_bid1_wtp
    ) 
  )




## v14_reason----


# 回答が40くらいしかなく、全体的にばらけているので、ダミー変数にはしない。
# 分析に使う必要が出てきたら自由記述をカテゴライスするなどする
df$v14_reason |> 
  unique()





## v14回答整合性チェック----




# 整合性チェック
table(df$v14_bid1,
      is.na(df$v14_bid2_yes),
      is.na(df$v14_bid2_no))




# CVMの分岐質問であり得ない回答パターンをしているサンプルを抽出
bidmiss_id_1 <- df |> 
  filter(v14_bid1 == "支払う" & !is.na(v14_bid2_no)) |> 
  select(id) 

bidmiss_id_2 <- df |> 
  filter(v14_bid1 == "支払わない" & !is.na(v14_bid2_yes) & !is.na(v14_bid2_no)) |> 
  select(id) 

# これは1回目だけ回答して2回め無回答のパターン  
bidmiss_id_3 <- df |> 
  filter(v14_bid1 == "支払う" & is.na(v14_bid2_yes) & is.na(v14_bid2_no)) |> 
  select(id) 

bidmiss_id_4 <- df |> 
  filter(v14_bid1 == "支払わない" & is.na(v14_bid2_no)) |> 
  select(id) 

bidmiss_id <- rbind(bidmiss_id_1, bidmiss_id_2, bidmiss_id_3, bidmiss_id_4)

# ありえない回答パターンのサンプル抽出
df |> 
  semi_join(bidmiss_id, by = "id") |> 
  select(id, date, questionnaire_no, v14_bid1, v14_bid2_yes, v14_bid2_no)



## v14回答パターン変数作成----

# v14 CVM質問についてyy, yn, ny, nnのパターンを表す変数をつくる
df <- df |> 
  mutate(
    yes_no_pattern = case_when(
      v14_bid1 == "支払う" & v14_bid2_yes == "支払う"  & is.na(v14_bid2_no) ~ "yy",
      v14_bid1 == "支払う" & v14_bid2_yes == "支払わない"  & is.na(v14_bid2_no) ~ "yn",
      v14_bid1 == "支払わない" & v14_bid2_no == "支払う" ~ "ny",
      v14_bid1 == "支払わない" & v14_bid2_no == "支払わない" ~ "nn",
      TRUE ~ NA_character_
    )
  )




## v15----



# 7件法の選択肢を数値に変換する。妨げ度合いを示す変数とする。

score_map <- c(
  "強く妨げる" = 7,
  "妨げる" = 6,
  "少し妨げる" = 5,
  "どちらでもない" = 4,
  "あまり妨げない" = 3,
  "妨げない" = 2,
  "全く妨げない" = 1
)

convert_7scale <- function(x){
  as.integer(score_map[x])
}


df <- df |> mutate(
  across(
    c("v15_critical_access_attitude", "v15_critical_parking_attitude", "v15_critical_crowd_attitude", "v15_critical_shop_attitude", "v15_critical_photospot_attitude"), convert_7scale
  )
)



