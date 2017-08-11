
#importing the data
crimson_text_corpus <- read.csv("C:\\Users\\prathyusha\\Desktop\\cleaned_crimson_data_2.csv", header = T, sep=",")


#library(tm)
#getwd()
#setwd('/home/fcbanalytics/Prathy_Practice')
crimson_text_corpus <- data.frame(crimson_text_corpus$Contents,crimson_text_corpus$doc_id,crimson_text_corpus$Name)

# To asssign a new colum with 0 values
#crimson_text_corpus <- as.data.frame(append(crimson_text_corpus, 0, after = 4))
crimson_text_corpus <- setNames(crimson_text_corpus,c( "contents" ,"Post_id","Name"))

#to convert all the variables into characters 
crimson_text_corpus <- data.frame(lapply(crimson_text_corpus, as.character), stringsAsFactors=FALSE)

# or another way to change the variable to character
#crimson_text_corpus$contents <- as.character(crimson_text_corpus$contents)
#crimson_text_corpus$Post_id <- as.character(crimson_text_corpus$Post_id)
#crimson_text_corpus$Name <- as.character(crimson_text_corpus$Name)

# To find out the names which starts with Dr

Dr <- grepl("Dr.", crimson_text_corpus$Name)
summary(Dr)

# row number
find1 <- grep("Dr.", crimson_text_corpus$Name)
find1

# To print the names
find <- crimson_text_corpus$Name[grep("Dr.", crimson_text_corpus$Name)]
find

# to find out the names with Dr. and Dr 

crimson_text_corpus$is_doc <- grepl("Dr\\Q.\\E",crimson_text_corpus$Name) # to know the dr. terms

crimson_text_corpus$is_doc2 <- grepl("Dr\\Q \\E",crimson_text_corpus$Name) # to know the dr terms

summary(crimson_text_corpus$is_doc)

summary(crimson_text_corpus$is_doc2)

#ifelse statement to assign the 0 and 1 values to new code column

crimson_text_corpus$code <- ifelse(crimson_text_corpus$is_doc %in% c(TRUE)| crimson_text_corpus$is_doc2 %in% c(TRUE), "Doctor","Consumer")

#Run after creating ifelse statement
crimson_text_corpus$code <- as.character(crimson_text_corpus$code)

# Reference ifelse statement 
#hcp$IS_GYN <- ifelse(hcp$IS_GYN1 %in% c(TRUE) & hcp$is_texas %in% c(1) & hcp$target_zips %in% 'target' & 
# hcp$GENDER_CODE_3017 %in% "F", TRUE, FALSE)

#hcp$is_hcp <- ifelse(is_sermo %in% c(TRUE)| is_crimson_and_dr_in_name %in% c(TRUE),1,0)



crimson_text_corpus <- data.frame(crimson_text_corpus)

# to drop Name, is_doc and is_doc2 columns

crimson_text_corpus <- crimson_text_corpus[,-(3:5),drop=FALSE]


crimson <- data.frame(crimson_text_corpus)

TextContent = crimson$contents
# create term frequency matrix using functions from tm library
doc_corpus <- Corpus( VectorSource(TextContent) )
control_list <- list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE)
tdm <- TermDocumentMatrix(doc_corpus, control = control_list)

tf <- as.matrix(tdm)


#The next Step is to Create the Inverse Term frequency Matrix with the above inputs

# idf
idf <- log( ncol(tf) / ( 1 + rowSums(tf != 0) ) ) 

# diagonal matrix
idf <- diag(idf)

tf_idf <- crossprod(tf, idf)
colnames(tf_idf) <- rownames(tf)
tf_idf


# Note that normalization is computed "row-wise"
tf_idf / sqrt( rowSums( tf_idf^2 ) )

TextContent

crimson$dmeta1 <- 1:8891
crimson$dmeta2 <- letters[1:8891]
#crimson

# corpus 
crimson_docs <- Corpus(DataframeSource(crimson))

inspect(crimson_docs) #showing error to overcome the error file encoding = latin1 is added at the data importing step

meta(crimson_docs)

#crimson_docs

writeLines(as.character(crimson_docs[[200]]))

getTransformations()
toSpace <- content_transformer(function (x, pattern) {return (gsub(pattern, " ", x))})

crimson_docs <- tm_map(crimson_docs, toSpace, "-")

crimson_docs <- tm_map(crimson_docs, toSpace, ":")

crimson_docs <- tm_map(crimson_docs, removePunctuation)

crimson_docs <- tm_map(crimson_docs,content_transformer(tolower))

crimson_docs <- tm_map(crimson_docs, removeNumbers)

crimson_docs <- tm_map(crimson_docs, removeWords, stopwords("english"))
#crimson_docs <- tm_map(crimson_docs,content_transformer(function(x) iconv(x, to='UTF8', sub='byte')))

crimson_docs <- tm_map(crimson_docs, stripWhitespace)

writeLines(as.character(crimson_docs[[200]]))

RemoveURL <- function(x){
  gsub("http[a-z]*","",x)
}

crimson_docs <- tm_map(crimson_docs, content_transformer(RemoveURL))

#install.packages("SnowballC")

library(SnowballC)

#install.packages("textstem")
#install.packages('*textstem', dependencies = TRUE)
#library(textstem)

# stem document 

#crimson_docs <- stemDocument(crimson_docs, language = "english") # try

crimson_docs <- tm_map(crimson_docs, stemDocument)

writeLines(as.character(crimson_docs[[200]]))
crimson_docs <- tm_map(crimson_docs, content_transformer(gsub), pattern = 'someth', replacement = 'something', fixed = T)
crimson_docs <- tm_map(crimson_docs, content_transformer(gsub), pattern = 'peopl', replacement = 'people', fixed = T)
crimson_docs <- tm_map(crimson_docs, content_transformer(gsub), pattern = 'consum', replacement = 'consumer', fixed = T)
crimson_docs <- tm_map(crimson_docs, content_transformer(gsub), pattern = 'realli', replacement = 'realise', fixed = T)
crimson_docs <- tm_map(crimson_docs, content_transformer(gsub), pattern = 'someon', replacement = 'someone', fixed = T)
crimson_docs <- tm_map(crimson_docs, content_transformer(gsub), pattern = 'lllive', replacement = 'live', fixed = T)
crimson_docs <- tm_map(crimson_docs, content_transformer(gsub), pattern = 'chanc', replacement = 'chance', fixed = T)
crimson_docs <- tm_map(crimson_docs, content_transformer(gsub), pattern = 'mani', replacement = 'many', fixed = T)
crimson_docs <- tm_map(crimson_docs, content_transformer(gsub), pattern = 'alway', replacement = 'always', fixed = T)
crimson_docs <- tm_map(crimson_docs, content_transformer(gsub), pattern = 'llive', replacement = 'live', fixed = T)
crimson_docs <- tm_map(crimson_docs, content_transformer(gsub), pattern = 'fargo', replacement = 'far', fixed = T)

# To remove specific terms

crimson_docs <- tm_map(crimson_docs,removeWords,c("let","that","zwu","zyra","NA","e", "rt","get","can", "also", "may"))

#str(crimson_docs)

# Document Term matrix

corpus <- Corpus(VectorSource(crimson_docs)) # change class 
matrix_term <- DocumentTermMatrix(corpus)

dtm <- TermDocumentMatrix(matrix_term, control = control_list)



# To get top 100 most frequent words
#m <- as.matrix(dtm)
#v <- sort(rowSums(m),decreasing=TRUE)
#d <- data.frame(word = names(v),freq=v)
#head(d, 100)


# to get all the terms 

dtm$dimnames$Terms

writeLines(as.character(crimson_docs[[200]]))

inspect(dtm[1:5,1:5])

# Mining the corpus

freq_words <- colSums(as.matrix(dtm)) # to know the frequency of the words

length(freq_words) # total no of terms

#create sort order (descending)
ord <- order(freq_words,decreasing=TRUE)
#ord

#inspect most and least frequently occurring terms
freq_words[head(ord)]

# to get top 100 frequent words
freq_words[(ord)][1:100]


# to remove the unqualified terms we need to rerun from the ftansformation step
#crimson_docs <- tm_map(crimson_docs,removeWords,c("zwu","zyra","NA","e", "rt","get","will","just","dont"))

# to get least frequent words
freq_words[tail(ord)]

# removing any additional words by limiting the word length
#dtmr <-DocumentTermMatrix(crimson_docs, control=list(wordLengths=c(4, 20),
#                                                    bounds = list(global = c(1,50))))
#dtmr
#str(dtmr)


findFreqTerms(dtm,lowfreq=306) # to find the terms which appears more than 1000 times 

findAssocs(dtm,'chase',0.5) # to check the correlation between the specific term amd other terms in the corpus
findAssocs(dtm,'ancient',0.6)

# graphics

crimson_graphics =data.frame(term=names(freq_words),occurrences=freq_words)

library(ggplot2)
crimson_plot <- ggplot(subset(crimson_graphics, freq_words>1000), aes(term, occurrences))
crimson_plot <- crimson_plot + geom_bar(stat='identity')
crimson_plot <- crimson_plot + theme(axis.text.x=element_text(angle=45, hjust=1))
crimson_plot

#wordcloud
#install.packages("RcolorBrewer")
library(wordcloud)

#setting the same seed each time ensures consistent look across clouds
set.seed(42)

#limit words by specifying min frequency, word cloud without any colors 
wordcloud(names(freq_words),freq_words, min.freq=1000)

# To add colors

wc <- wordcloud(names(freq_words),freq_words,min.freq=1000,colors=brewer.pal(6,'Dark2'))
wc <- wordcloud(names(freq_words),freq_words,min.freq=1000,colors=brewer.pal(8,'Dark2'))




# To get the tf-idf

review_crimson_corpus = Corpus(VectorSource(crimson_text_corpus$contents))
review_crimson_corpus = tm_map(review_crimson_corpus, content_transformer(tolower))
review_crimson_corpus = tm_map(review_crimson_corpus, removeNumbers)
review_crimson_corpus = tm_map(review_crimson_corpus, removePunctuation)
review_crimson_corpus =  tm_map(review_crimson_corpus, stripWhitespace)
review_crimson_corpus = tm_map(review_crimson_corpus, removeWords, stopwords("english"))

RemoveURL <- function(x){
  gsub("http[a-z]*","",x)
}

review_crimson_corpus <- tm_map(review_crimson_corpus, content_transformer(RemoveURL))

inspect(review_crimson_corpus[1])

review_crimson_corpus<- DocumentTermMatrix(review_crimson_corpus)
review_crimson_corpus

review_crimson_corpus = removeSparseTerms(review_crimson_corpus, 0.99)

review_crimson_corpus

inspect(review_crimson_corpus[1,1:5])

x <- findFreqTerms(review_crimson_corpus,80)
x

wc = data.frame(sort(colSums(as.matrix(review_crimson_corpus)), decreasing=TRUE))
wordcloud(rownames(wc), wc[,1], max.words=10, colors=brewer.pal(3, "Dark2"))



tfidf <- TermDocumentMatrix(crimson_text_corpus$contents, control = list(weighting = weightTfIdf))
tfidf = removeSparseTerms(tfidf, 0.95)
tfidf
