library(RMeCab)

server = function(input, output, session) {
    observeEvent(input$file, {
        csv_file = reactive(read.csv(input$file$datapath))
        output$table = renderTable(csv_file())
    })
    
    output$value <- renderText({ input$text })
    
    observeEvent(input$submit, {
        text = input$text
        write(text, file="data.txt")
        if(nchar(text)<10) {
            output$words_list <- renderTable("10文字以上の文章を入力する必要があります")
        } else {
            word <- RMeCabFreq("data.txt")
            #名詞のみ取得
            word <- subset(word, Info1 == "名詞")
            #数・非自立・接尾の除外
            type <- c("数", "非自立", "接尾")
            word <- subset(word, !Info2 %in% type)
            word <- word[order(word$Freq, decreasing=T),]
            #無駄な単語の削除
            word <- subset(word, Term!="ー"&Term!="("&Term!=")"&Term!="!!"&Term!="!")
            output$words_list <- renderTable(head(word,n=100))
            
            i=1	#単語の番号i
            export = as.list(NULL)
            source("same-mean.r") #類義語探索プログラムsame-mean.rを取得
            print(nrow(word))
            while(i<=nrow(word)){ #単語の番号i
                print(word[i,1])
                same_words <- checkwords(word[i,1]) #類義語検索(same-mean.rを使用)
                
                if(same_words[1] != "Error"){ #類義語が辞書にある場合
                    j=1 #類義語のリスト番号j
                    while( j <= length(same_words) ){
                        k=1
                        while(k<=N){ #検索中の単語の番号k
                            if(word[i,1]!=same_words[j]){ #同義語と元が同じ時を覗く
                                if(word[k,1]==same_words[j]){ #j番目の類義語がリスト(k番目)から発見できた時
                                    print("類義語！！！")
                                    export[i] <- word[k,1]
                                    word[i,4]<-word[i,4]+word[k,4] #同義語の場合最初に現れた言語にFreqを合算
                                    word[k,]<-NA	#同義語（後者）を削除
                                    word<-na.omit(word)	#同義語（後者）を削除
                                }
                            }
                            k <- k + 1
                        }
                        j <- j + 1
                    }
                }
                i <- i + 1
            }
            
            word <- head(word, n=N) #最終確認（全プロセス終了）
            output$result <- renderTable(export.table)
        }
    })
}