###########################################
### 【地理情報講座#3】Rでの距離計算入門 ###
###########################################


### 1. 2つの店舗間の距離計算

# geosphereというライブラリ
install.packages("geosphere")
library("geosphere")

# distGeoというコマンドで距離計算ができる。単位はm。（）内で距離を測りたい2組の緯度経度を指定します。
# 他にもコマンドがありますが、distGEOが一番正確みたいです。
# gas_naha[1, c("longitude", "latitude")]は1行目の"longitude", "latitude"という列に含まれている値を示している。

distGeo(gas_naha[1, c("longitude", "latitude")], gas_naha[2, c("longitude", "latitude")])

# Imapというライブラリでも出来るみたいです。
install.packages("Imap")
library(Imap)

# gdistというコマンドで距離計算ができる。単位は指定可能
gdist(lon.1 = gas_naha[1, "longitude"], lon.2 =gas_naha[2, "longitude"] , lat.1 = gas_naha[1, "latitude"], lat.2 = gas_naha[2, "latitude"], units  = "m")

# ちなみに、どっちでも値は等しいです。個人的にはgeosphereの方が書きやすいです。Imapは距離の指定ができるのが嬉しいですね。


### 2. 複数の店舗間の距離を一気に計算する ###

## 2.1. 1行目の店舗との距離を各店舗に対して算出
for (i in 1:48){
  gas_naha[i,"1" ] <- distGeo(gas_naha[1, c("longitude", "latitude")], gas_naha[i, c("longitude", "latitude")])
}


## 2.2.全店舗に対して同じことをやってみます。これぞ自動化。

# エラーが出たので、店舗名をFactorから文字列にしています。
gas_naha$store <- as.character(gas_naha$store)

# i行目の店舗との距離を各店舗j（1~48行目）に対して測っている。
for (i in 1:48){
  for (j in 1:48){
    colname <- gas_naha[i, "store"]
    gas_naha[j,colname ] <- distGeo(gas_naha[i, c("longitude", "latitude")], gas_naha[j, c("longitude", "latitude")])
  }
}

## 2.3. 競合店舗の数を集計する

for (i in 1:48){
  for (j in 1:48){
    colname <- gas_naha[i, "store"]
    gas_naha[j,colname ] <- distGeo(gas_naha[i, c("longitude", "latitude")], gas_naha[j, c("longitude", "latitude")])
    gas_naha[j,colname ] <- ifelse(gas_naha[j,colname ] <= 2000, 1, 0)
  }
}


for (i in 1:48){
  gas_naha[i, "compe"] <- sum(gas_naha[i, 10:57]) -1
}


### 3. 競合の店舗数と価格の関係を分析する
## 3.1. 価格データを結合してデータを整える
gas_compe <- gas_naha[, c(1, 58)]
price <- read.csv("price.csv")
gas_compe <- merge(gas_compe, price, by = 'store')
gas_compe <- na.omit(gas_compe)

## 3.2. 回帰分析で検証
out <- lm(data = gas_compe, Price ~ compe)
summary(out)