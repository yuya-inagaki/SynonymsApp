library(RMeCab)
library(stringr)

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
            words_list <- word
            
            i=1	#単語の番号i
            export <- data.frame(matrix(rep(NA, 4), nrow=1))[numeric(0), ]
            colnames(export) <- c("Word", "Count", "SynonymsCount" ,"Synonyms")
            #類義語探索プログラムsame-mean.rを取得
            source("same-mean.r")
            words_length = nrow(word)
            while(i<=words_length){ #単語の番号i
                check_word <- word[i,1]
                #類義語検索(same-mean.rを使用)
                same_words <- checkwords(check_word)
                print(same_words)
                synonyms_count = 1
                synonyms_list = list()
                if(same_words[1] != "Error"){ #類義語が辞書にある場合
                    j=1 #類義語のリスト番号j
                    while( j <= length(same_words) ){
                        k=i
                        #検索中の単語の番号k
                        while(k<=words_length){
                            #同義語と元が同じ時を覗く
                            if(check_word!=same_words[j]){
                                #j番目の類義語がリスト(k番目)から発見できた時
                                if(word[k,1]==same_words[j]){
                                    if(synonyms_count==1){
                                        synonyms_list[1] <- check_word
                                    }
                                    synonyms_list[synonyms_count+1] <- word[k,1]
                                    synonyms_count <- synonyms_count + 1
                                    #同義語の場合最初に現れた言語にFreqを合算
                                    word[i,4]<-word[i,4]+word[k,4]
                                    #同義語（後者）を削除
                                    word[k,]<-NA
                                    word<-na.omit(word)
                                    words_length <- words_length-1
                                }
                            }
                            k <- k + 1
                        }
                        j <- j + 1
                    }
                }
                # 類義語のexport
                export[i,]=c(check_word, word[i,4], synonyms_count, str_c(synonyms_list, collapse = ", "))
                i <- i + 1
            }
            
            word <- head(word, n=N) #最終確認（全プロセス終了）
            output$result <- DT::renderDataTable(export, 
                extensions = c('Buttons'), 
                options = list(lengthMenu = c(10, 25, 50, 100),
                    dom = 'Blfrtip',
                    pageLength = 10, 
                    buttons = c('csv', 'excel', 'pdf')
                )
            )
            output$words_list <- DT::renderDataTable(words_list, 
                extensions = c('Buttons'), 
                options = list(lengthMenu = c(10, 25, 50, 100),
                    dom = 'Blfrtip',
                    pageLength = 10, 
                    buttons = c('csv', 'excel', 'pdf')
                )
            )
        }
    })
}