library(RSQLite) #RSQLite
library(dplyr)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname ="wnjpn.db") #DB読み込み(wnjpn.db)


checkwords <- function(x){

  hit_words <- dbGetQuery(con, "select * from word where lemma = (?)",x) #wordテーブルから一致するワードを取得

  if( nrow(hit_words)==0 ){  #一致するワードがなければErrorを返す
    return("Error")
  }

  #print(hit_words)
  hit_words <- subset(hit_words, pos=="n")

  if( nrow(hit_words)==0 ){  #一致するワードがなければErrorを返す
    return("Error")
  }

  #print("一致したワード")
  #print(hit_words)

  hit_words_synset <- dbGetQuery(con, "select * from sense where wordid = (?)",hit_words$wordid) #同義語をsenseテーブルから取得

  if( nrow(hit_words_synset)==0 ){  #一致する同義語シーンセットがなければErrorを返す
    return("取得した同義語リストはありません。")
  }

  #print(hit_words_synset$synset)

  counter <- nrow(hit_words_synset) #シーンセットの数をカウント
  #print("シーンセット数")
  #print(counter)


  n=1
  while(n <= counter){
    if(n==1){
      syn_word <- dbGetQuery(con, "select * from synset where synset = (?)",hit_words_synset$synset[n])
    }else{
      syn_word[n,] <- dbGetQuery(con, "select * from synset where synset = (?)",hit_words_synset$synset[n]) #同義語をsenseテーブルから取得
    }
    n <- n + 1
  }

  #print("取得した同義語定義リスト")
  #print(syn_word)

  counter2 <- nrow(syn_word) #同義語定義リスト数をカウント
  #print("同義語定義リスト数")
  #print(counter2)

  m=1
  y=1
  while(m <= counter2){
    syn_word_keep <- dbGetQuery(con, "select * from sense where  synset = (?)",syn_word$synset[m]) #sense から synset = syn_word の synset の sense を取得
    #print("何回目かのループ")
    #print(syn_word_keep)

    counter4 = 0
    counter4 <- nrow(syn_word_keep)
    #print("何行あったか")
    #print(counter4)
    z=1
    while(z <= counter4){
      #取得した sense を並べる
      if(y==1){
        syn_word_export <- syn_word_keep[c(1),]
      }else{
        syn_word_export[y,] <- syn_word_keep[c(z),]
      }
      z <- z + 1
      y <- y + 1
    }
    m <- m + 1
  }
  #print("取得したシーン")
  #print(syn_word_export)

  counter3 <- nrow(syn_word_export) #同義語シーン数をカウント
  #print("同義語定義シーン数")
  #print(counter3)

  x=1
  while(x <= counter3){
    #word から wordid = syn_word_export の wordid を取得、代入
    if(x == 1){
      syn_word_output <- dbGetQuery(con, "select * from word where wordid = (?)",syn_word_export$wordid[1])
    }else{
      syn_word_output[x,] <- dbGetQuery(con, "select * from word where wordid = (?)",syn_word_export$wordid[x])
    }
    x <- x + 1
  }
  #print("取得したワード")
  #print(syn_word_output)


  #print("最終出力：同義語")
  final_export <- syn_word_output$lemma
  return(final_export) #最終的な同義語を返す

}



# ----新規ワード追加時に編集すべき箇所
# wordに追加したいワードを追加
# senseに追加したい意味のsense_set_IDを追加
# もし新しいsensesetなら意味を追加する。
