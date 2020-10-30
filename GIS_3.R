###################################################
### 【地理情報講座#1】Rでのジオコーディング入門 ###
###################################################


### 今回使用するデータの読み込み
# Windowsの人はfileEncoding = "utf8"とする必要があるかもしれません。
gas_naha <- read.csv("gas_naha_new.csv", fileEncoding = "shift-jis")


## データを整える（エクセルで作業可能です）
library(tidyverse)
gas_naha <- gas_naha %>% 
  select(store, address,company, fX, fY) %>% # 必要な変数だけ取得
  rename(longitude = fX, latitude = fY) # 変数の名前を変更


### GISプロット 
## leafletというライブラリを使用します。 
library(leaflet)

## 地図をRstudio上に用意します。
map <- leaflet(gas_naha) %>% 
  addTiles()

map

map %>% 
  addCircles(lng = ~ longitude, lat = ~latitude)


#####################################################
### 【地理情報講座#2】Rでのメッシュ統計データ入門 ###
#####################################################


### 2. 緯度経度をメッシュコードに変換する ###

# メッシュコード変換に使用するライブラリ。少し時間がかかります。
install.packages("jpmesh")
library(jpmesh)

# 1km四方のメッシュを示す3次メッシュコードを算出。coords_to_meshというコマンドを使用します。
# （）内では、経度、緯度の順番に座標を指定します。
gas_naha$meshcode3 <- coords_to_mesh(gas_naha$longitude, gas_naha$latitude)

# 同じ様にして、2次メッシュを算出してみる。2次メッシュは10km四方である。mesh_sizeでメッシュの大きさを指定。
gas_naha$meshcode2 <- coords_to_mesh(gas_naha$longitude, gas_naha$latitude, mesh_size = 10)

# つまり、500m四方の4次メッシュだと…
gas_naha$meshcode4 <- coords_to_mesh(gas_naha$longitude, gas_naha$latitude, mesh_size = 0.5)


### Groupwork1 ###
gas_mesh3 <- gas_naha %>% 
  group_by(meshcode3) %>% 
  summarise(number_store = n(), competitor = sum(compe)- number_store)
            # 行の数をカウント、つまり店舗数をカウントしている。


### 3. メッシュ統計データを組み合わせて分析してみる ###

### Groupwork2 ###

## 沖縄県那覇市の3次メッシュにくっつけていく
okinawa_mesh3 <- read.csv('47.csv', fileEncoding = "shift-jis")

# 那覇市だけのデータにします
naha_mesh3 <- subset(okinawa_mesh3, 市区町村名 == "那覇市", 基準メッシュコード)
colnames(naha_mesh3) <-"meshcode3"

## 別解
naha_mesh3 <- okinawa_mesh3 %>% 
  filter(市区町村名 == "那覇市") %>% 
  select(基準メッシュコード) %>% 
  rename(meshcode3 = 基準メッシュコード)

### Groupwork3 ###

# 結合
gas_naha_3 <- merge(naha_mesh3, gas_mesh3, by= "meshcode3", all.x = TRUE)
gas_naha_3$number_store[is.na(gas_naha_3$number_store)] <- 0
gas_naha_3$entry <- ifelse(gas_naha_3$number_store > 0, 1, 0)

# sepの指定に注意する
pop <- read.csv("tblT000846S3927.txt", sep = ",", fileEncoding = "shift-jis")
# 必要なデータだけに絞る。必要な行は1行目以外。列は1列目と5列目だけ。
pop <- pop[-1,c(1, 5)]
# 変数名を変える
colnames(pop) <- c("meshcode3", "pop")

### Groupwork4 ###

# データの型を確認
sapply(gas_naha_3, class)
sapply(pop, class)
# 人口を数値データに変換する
pop$pop<- as.numeric(pop$pop)
# 結合
gas_naha_3 <- merge(gas_naha_3, pop, by="meshcode3", all.x = TRUE)
# 欠損値を含むデータは人が住めない地域として削除します。
gas_naha_3 <- na.omit(gas_naha_3)


# 推定
out <- glm(entry ~ pop, family = binomial(link = "probit"), data = gas_naha_3)
install.packages("margins")
library(margins)
summary(margins(out))



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
