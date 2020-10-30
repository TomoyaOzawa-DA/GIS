#################################
### Rでのジオコーディング入門 ###
#################################


### データの読み込み
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







