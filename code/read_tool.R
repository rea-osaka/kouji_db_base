library(tidyverse)

############################################################
# 公示csvファイルを入れてあるディレクトリから読み込む
############################################################
make_kouji_base_DB <- function(target_dir, pattern = "^L01-.*\\.csv$"){
  
  # 読み込みファイルリスト
  files <- dir(target_dir, pattern = pattern, full.names = TRUE)
  
  # ファイル読み込み
  ans <- lapply(files, read_chikakouji_file) %>% bind_rows()
  
  # 並び変え
  ans <- ans %>% arrange(date, 所在地コード, 用途, 連番)
  
  return(ans)
}


############################################################
# 公示csvファイル一つを読み込むルーチン
# データの作成年度により列がことなるので
# 抜き出す列を確定する必要あり
############################################################
read_chikakouji_file <- function(file){

  # 共通処理
  # ファイルからのデータを読み込む共通処理
  tmp_data <- 
    read_csv(file,
             locale = locale(encoding = "cp932"),
             col_types = cols(.default = col_character(),
                              `年次` = col_integer(),
                              `地積` = col_integer(),
                              `建ぺい率` = col_integer(),
                              `建蔽率` = col_integer(),
                              `容積率` = col_integer()
                              )) %>%
    filter(!is.na(所在地コード))

  ##########################################
  # 列整理の前処理
  ##########################################
  
  # 当該データファイルの作成年
  data_year <- tmp_data$`年次`[1]

  # 変更点データ
  change_info_vector <- tmp_data[[search_change_property_col_index(tmp_data)]]
  if(is.null(change_info_vector)){
    # S58年のデータ列が無い場合の処理
    change_info_vector = rep(NA,nrow(tmp_data))
  }
  
  # 建蔽率列を統一
  if("建蔽率" %in% names(tmp_data)){
    tmp_data$`建ぺい率` <- tmp_data$`建蔽率`
  }

  # 列に駅名が無い場合
  if( ! "駅名" %in% names(tmp_data)){
    tmp_data$`駅名` <- ""
  }
  
  
  
  ##########################################
  # 用途地域列の処理をして整理して
  # 統一したものを結合
  ##########################################
  tmp_data <- tmp_data %>% bind_cols(extract_youtokubun(tmp_data))
    
  ##########################################
  # 統一列の作成
  ##########################################
  tmp_data <- tmp_data %>% 
    mutate(
      
      # 当該年度価格
      price = tmp_data[[search_price_col_index(tmp_data)]] %>% as.integer(),
      
      # 標準地番号
      std_number = make_stdnumber_string(`用途`,`連番`,`市区町村名`),
      
      # 選定替えビットデータ
      sentei_bit = tmp_data[[search_sentei_col_index(tmp_data)]],
      
      # 変更点選定替え情報具体データ
      point_status = extract_point_status(change_info_vector, data_year),
      
      # 変更点具体的データ
      change_contents = extract_change_contents(change_info_vector, data_year),
      
      # 利用の現況具体データ
      genkyo = extract_genkyo(`利用の現況`),
      
      # 施設の具体データ
      sisetu = extract_infra(`施設`),
      
      # 価格時点データ
      date = as.Date(sprintf("%d-01-01",data_year)),
      
      # 価格時点和暦データ
      wareki = `年次` %>% sapply(make_wareki), 
      
      # 位置情報
      long = as.double(`経度`) / 3600,
      lat = as.double(`緯度`) / 3600, 
      
      # 読み込み元ファイル名
      sorce_file = file
           
    )#end of mutate
    
  ans <- tmp_data %>% 
    select(date,
           wareki,
           std_number,
           price,
           `所在地コード`:`地積`,
           genkyo,
           `利用状況表示`,
           `建物構造`,
           sisetu,
           youto,
           bouka,
           tokei,
           sonota,
           建ぺい率,
           容積率,
           `駅名`,
           `駅距離`,
           long, lat,
           sentei_bit,
           point_status,
           change_contents,
           sorce_file) %>% 
    
    arrange(date,所在地コード,用途,連番)
  
  
 return(ans) 
}

############################################################
# 公示の取り込みdata.frameから
# 各種列のインデックスを探すルーチン
############################################################

search_price_col_index <- function(df){
  base <- which(stringr::str_detect(names(df), "選定年次ビット"))
  current_year <- as.integer(df$`年次`[1])
  offset <- current_year - 1982
  
  return(base + offset)
}

search_change_property_col_index <- function(df){
  base <- which(stringr::str_detect(names(df),"選定年次ビット"))
  current_year <- as.integer(df$`年次`[1])
  offset <- current_year - 1982

  ans <- ifelse(current_year == 1983, NA, base + offset * 2 -1)
  
  return(ans)
}

search_sentei_col_index <- function(df){
  base <- which(stringr::str_detect(names(df),"選定年次ビット"))
  return(base)
}

###############################################################
# 標準宅地番号を作る
###############################################################
make_stdnumber_string <- function(youto,renban,name){
  youto <- as.numeric(youto)
  renban <- as.numeric(renban)
  ifelse(youto == 0,
         sprintf("%s-%s",name,renban),
         sprintf("%s%s-%s",name,youto,renban))
}


###############################################################
# bit構文解析ルーチン
###############################################################

####################################
# bit文字列変換の汎用ルーチン
####################################
extract_item_from_bitdata <- function(bit_data, item_vector){
  split_bit_vector <- strsplit(bit_data, "")[[1]]
  logi_vecotr <- ifelse(split_bit_vector == "1", TRUE, FALSE)
  return(item_vector[logi_vecotr])
}  

####################################
# 施設
####################################
extract_infra <- function(bitdata_list){
  
  item <- c("水道","ガス","下水")
  
  ans <- 
    lapply(bitdata_list, extract_item_from_bitdata, item) %>% 
    lapply(str_flatten, "・") 
  
  ans <- ifelse(sapply(ans, length) == 0, "", ans) %>% unlist()
    
  return(ans)
}

####################################
# 利用の現況
####################################
extract_genkyo <- function(bitdata_list){
  
  item <- c("住宅", "店舗", "事務所", "銀行", "旅館", "給油所", "工場", "倉庫", "農地",
                   "山林", "医院", "空地", "作業場", "原野", "その他", "用材", "雑木" )
  
  ans <- 
    lapply(bitdata_list, extract_item_from_bitdata, item) %>% 
    lapply(str_flatten, "・") 
  ans <- ifelse(sapply(ans, length) == 0, "", ans) %>% unlist()
  return(ans)
}

####################################
# 用途地域解析
# 
# パターン１ S58-H19(v1.0)
# パターン２ H20-H25(v2.0)、H26(v2.2)、H27-H29(v2.3)
# パターン３ H30(v2.4)、H31(v2.5)
####################################

extract_youtokubun <- function(df){
  year <- as.numeric(df$年次[[1]])
  
  ans <- NULL
  
  if(year %in% 1983:2007){
    ans <- extract_youtokubun_p1(df)
    
  }else if(year %in% 2008:2017){
    ans <- extract_youtokubun_p2(df)
    
  }else{
    ans <- extract_youtokubun_p3(df)
    
  }
  
  return(ans)
  
}


####################################
# パターン３
####################################

extract_youtokubun_p3 <- function(df){
  
   ans_youto <- df$用途区分
   ans_bouka <- df$防火区分
   ans_tokei <- df$都市計画区分
   ans_sonota <- df$公園区分
  
   ans <- tibble::tibble(youto = ifelse(ans_youto == "_" , "", ans_youto),
                         bouka = ifelse(ans_bouka == "_" , "", ans_bouka),
                         tokei = ifelse(ans_tokei == "_" , "", ans_tokei),
                         sonota = ifelse(ans_sonota == "_" , "", ans_sonota))
  return(ans)
  
}
####################################
# パターン２
####################################
extract_youtokubun_p2 <- function(df){
  bitdata_list <- df$法規制
  youto <- bitdata_list %>% substring(1,12)
  
  bouka <- bitdata_list %>% substring(13,14)
  
  tokei <- bitdata_list %>% substring(15,18)
  
  sonota <- bitdata_list %>% substring(19,25)
  
  
  item_youto  <- c("１低専", "２低専", "１中専", "２中専", "１住居", "２住居", "準住居", "近商", "商業", 
                   "準工", "工業", "工専")
  
  item_bouka <- c("防火", "準防")
  
  item_tokei <- c("調区", "非線引", "都計外", "準都計")
  
  item_sonota <- c("地森計", "国立公（普通）", "国立公（２種）", "国立公（３種）", "国定公（普通）", "国定公（２種）", "国定公（３種）")
  
  
   ans_youto <- lapply(youto, extract_item_from_bitdata, item_youto) %>% lapply(str_flatten, "・") 
   ans_bouka <- lapply(bouka, extract_item_from_bitdata, item_bouka) %>% lapply(str_flatten, "・") 
   ans_tokei <- lapply(tokei, extract_item_from_bitdata, item_tokei) %>% lapply(str_flatten, "・") 
   ans_sonota <- lapply(sonota, extract_item_from_bitdata, item_sonota) %>% lapply(str_flatten, "・") 
  
   ans <- tibble::tibble(youto = ifelse(sapply(ans_youto, length) == 0, "", ans_youto) %>% unlist(),
                         bouka = ifelse(sapply(ans_bouka, length) == 0, "", ans_bouka) %>% unlist(),
                         tokei = ifelse(sapply(ans_tokei, length) == 0, "市街化", ans_tokei) %>% unlist(),
                         sonota = ifelse(sapply(ans_sonota, length) == 0, "", ans_sonota) %>% unlist())
  return(ans)
  
}


####################################
# パターン１
####################################
extract_youtokubun_p1 <- function(df){
  
  bitdata_list <- df$用途地域
  
  youto <- bitdata_list %>% substring(1,12)
  youto2 <- bitdata_list %>% substring(19,22)
  youto <- str_c(youto,youto2)
  
  bouka <- bitdata_list %>% substring(13,14)
  
  tokei <- bitdata_list %>% substring(15,16)
  
  sonota <- bitdata_list %>% substring(17,18)
  
  
  item_youto  <- c("１低専", "２低専", "１中専", "２中専", "１住居", "２住居", "準住居", "近商", "商業", 
                   "準工", "工業", "工専", "１住専", "２住専", "住居", "住居専用")
  
  item_bouka <- c("防火", "準防")
  
  item_tokei <- c("調区", "都市")
  
  item_sonota <- c("地森計","国定公")
  
  
   ans_youto <- lapply(youto, extract_item_from_bitdata, item_youto) %>% lapply(str_flatten, "・") 
   ans_bouka <- lapply(bouka, extract_item_from_bitdata, item_bouka) %>% lapply(str_flatten, "・") 
   ans_tokei <- lapply(tokei, extract_item_from_bitdata, item_tokei) %>% lapply(str_flatten, "・") 
   ans_sonota <- lapply(sonota, extract_item_from_bitdata, item_sonota) %>% lapply(str_flatten, "・") 
  
   ans <- tibble::tibble(youto = ifelse(sapply(ans_youto, length) == 0, "", ans_youto) %>% unlist(),
                         bouka = ifelse(sapply(ans_bouka, length) == 0, "", ans_bouka) %>% unlist(),
                         tokei = ifelse(sapply(ans_tokei, length) == 0, "市街化", ans_tokei) %>% unlist(),
                         sonota = ifelse(sapply(ans_sonota, length) == 0, "", ans_sonota) %>% unlist())
  return(ans)
  
}



####################################
# 属性移動col解析
# その１
# 最左ビットで新規・継続の区分を解析
# 仕様から以下の場合分けが必要
# パターン１ 1984-2014
# パターン２ 2015-2019
####################################
extract_point_status <- function(bitdata_list, year = 1984){

  ans_list <- numeric(length(bitdata_list))
  
  for(i in seq_along(bitdata_list)){
    target <- substring(bitdata_list[i],1,1)
    
    ans <- ""
    
    if(year %in% c(1984:2014)){
      if(target == "1"){
        ans <- "継続"
      }else if(target == "2"){
        ans <- "基準地・標準地番号変更"
      }else if(target == "3"){
        ans <- "選定替えで当該の選定なし"
      }else if(target == "4"){
        ans <- "選定替えで当該年追加"
      }else if(target == "5"){
        ans <- "新設"
      }else if(target == "6"){
        ans <- "廃止"
      }
    }else if(year %in% c(2015:2019)){
      if(target == "1"){
        ans <- "継続"
      }else if(target == "2"){
        ans <- "基準地・標準地番号変更"
      }else if(target == "4"){
        ans <- "新設・選定替えで当該年追加"
      }
    }
    ans_list[i] <- ans
  }#end of for
  
 return(ans_list) 
}

####################################
# 属性移動col解析
# その２
# 最左ビットを除く部分で
# 何が変化したかを表す
#
# 仕様から以下の場合分けが必要
# bit長も異なる
# パターン１ 1984-2014
# パターン２ 2015-2019
####################################
extract_change_contents <- function(bitdata_list, year = 1984){
  size <- length(bitdata_list)
  ans_list <- numeric(length(bitdata_list))
  
  for(i in seq_along(bitdata_list)){
    ans <- ""
    
    if(year %in% c(1984:2014)){
      
      # bit長は10
      target <- substring(bitdata_list[i], 2, 10)
      
      item <- c("住所漢字",
                "地積",
                "利用の現況",
                "建物構造",
                "供給施設",
                "駅からの距離",
                "用途地域",
                "建ペイ率",
                "容積率")
      
      ans <- extract_item_from_bitdata(target, item)
      
    }else if(year %in% c(2015:2019)){
      
      # bit長は14
      target <- substring(bitdata_list[i], 2, 14)
      
      item <- c("住所漢字",
                "地積",
                "利用の現況",
                "建物構造",
                "供給施設",
                "駅からの距離",
                "用途区分",
                "防火区分",
                "都市計画区分",
                "森林区分",
                "公園区分",
                "建ペイ率",
                "容積率")
      
      ans <- extract_item_from_bitdata(target,item)
      
    }
    
    
    tmp <- str_flatten(ans, collapse = "・") 
    ans_list[i] <- ifelse(length(tmp) != 0, tmp, "")
    
  }
  
  return(ans_list)
}

###############################################################
# 汎用ルーチン
###############################################################

make_wareki <- function(int_year){
  int_year <- as.numeric(int_year)
  
  if( int_year < 1873){
    ans <- int_year
    
  }else if(int_year %in% 1873:1911){
    ans <- sprintf("M%02d", int_year - 1873 + 6)
    
  }else if(int_year %in% 1912:1925){
    if(int_year == 1912){
      ans <- "M45/T01"
    }else{
      ans <- sprintf("T%02d", int_year - 1911)
    }
    
  }else if(int_year %in% 1926:1988){
    if(int_year == 1926){
      ans <- "T15/S01"
    }else{
      ans <- sprintf("S%02d", int_year - 1925)
    }
    
  }else if(int_year %in% 1989:2018){
    if(int_year == 1989){
      ans <- "S64/H01"
    }else{
      ans <- sprintf("H%02d", int_year - 1988)
    }
    
  }else{
    if(int_year == 2019){
      ans <- "H31/R01"
    }else{
      ans <- sprintf("R%02d", int_year - 2018)
    }
    
  }
 return(ans) 
}
