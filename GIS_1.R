
##################################
### RでのWebスクレイピング入門 ###
##################################

### 準備
install.packages("rvest")
library("rvest")
library(tidyverse)

### htmlの指定：文字化けした場合は、ここでencodingを指定(encoding = "UTF-8")
html <- read_html("https://gogo.gs/search/?kw=&ac=47201")


### データの取得
gas_name_1 <- html_nodes(html, xpath = "//h5[@class = 'shop-name font-weight-bold']") %>% 
  html_text()
gas_name_1

store_1 <- data.frame(store = gas_name_1)

gas_address_1 <- html_nodes(html, xpath = "//p[@class = 'shop-address']") %>% 
  html_text()
gas_address_1

address_1 <- data.frame(address = gas_address_1)

### 結合
gas_1 <- cbind(store_1, address_1)


### 2ページから5ページのお店名と住所もゲットする。
## 同じことの繰り返しなので繰り返し処理を行うfor構文を使ってみます。

for (i in 2:5){
  # ページごとのURLを作成
  URL <- paste("https://gogo.gs/search/?kw=&ac=47201&page=",i, sep = "")
  # そのURLを読み込む
  html <- read_html(URL)
  # 店名の取得
  gas_name <-  html_nodes(html, xpath = "//h5[@class = 'shop-name font-weight-bold']") %>% 
    html_text()
  store <- data.frame(store = gas_name)
  # 同様に住所
  gas_address <- html_nodes(html, xpath = "//p[@class = 'shop-address']") %>% 
    html_text()
  address <- data.frame(address = gas_address)
  # 店名と住所を結合
  gas <- cbind(store, address)
  # 前ページまでのデータと結合
  gas_1 <- rbind(gas_1, gas)
}

gas_1


### おまけ："/"の後ろが会社名になっているので、抽出してみる

## とりあえず、店舗名を2つに分割
gas_store2 <- str_split_fixed(gas_1$store, pattern = "/", n = 2)
## くっつける
gas_new <- cbind(gas_1, gas_store2)
## 整える
gas_new <- gas_new %>% 
  select(store, address, "2") %>% 
  rename(company = "2")

## 追加
gas_new$company <- sub(" ", "", gas_new$company)

## 試しに会社名で店舗数を集計してみる
gas_sum <- gas_new %>% 
  group_by(company) %>% 
  summarise(number = n())

### csvファイルで出力する。
write.csv(gas_new, "gas_naha.csv", fileEncoding = "CP932", row.names = FALSE)



