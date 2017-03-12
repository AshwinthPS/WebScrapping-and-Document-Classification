getwd()

#checkForServer()

library(rvest)
library(RSelenium)
library(EBImage)
library(tm)
library(SnowballC)
options(EBImage.display = "raster")  
RSelenium::startServer()

z <- remoteDriver()
z$open()

z$navigate("http://www.google.com")

a=z$findElement(using="css selector","#lst-ib")

key<-readline("Enter the Keyword to search")

a$sendKeysToElement(list(key))
page <- z$findElement(using = "css selector",".sbico")
news_page<-page$clickElement()
currenturl<-z$getCurrentUrl()
url<-paste(currenturl,"&tbm=nws")
z$navigate(url)


head_latest <- z$findElement(using = "css selector",".g:nth-child(5) ._HId")
headers<-head_latest$clickElement()
Sys.sleep(10)
title<-head_latest$getTitle()
title

head_latest <- z$findElement(using = "css selector",".nested") 
text<-head_latest$getElementText()
text

## or ##

url<-z$getCurrentUrl()
url
webpage <- read_html(url[[1]])
webpage

img <- webpage %>%
  html_nodes(".wp-post-image") %>%
  html_attr("src")

ref_img <- readImage(img)
display(ref_img)


#### other options to remove text. 

sb_text <- html_nodes(webpage, 'p')
sb <- html_text(sb_text)

text<-NULL
for(i in 1:length(sb))
{
  text<- paste0(text,sb[i])
  
}

text

final_text<-paste(title,"       ",text)

i=1

dir.create(paste0("article",i))
write.table(final_text,paste0(getwd(),"/",paste0("article",i),"/","output.txt"))
output_image<-writeImage(ref_img,paste0(getwd(),"/",paste0("article",i),"/","Outputimg.jpg"))

###Image Extraction


###############################################################################################

queryvec<-readline("Enter Query Vectors")

for(i in 1:2)
{
  inputdata[i]<-read.table(paste0(getwd(),"/","article",i,"/","output.txt"))
  
}

inputdata[2]
ip<-read.table(file.choose())

query<-readline("Enter the Keyword to search / Query / Filter")

my.docs <- VectorSource(c(inputdata[1],query))
inspect(VCorpus(my.docs))



#my.docs$Names <- c(names(doc.list), "query")
my.corpus <- Corpus(my.docs)
inspect(my.corpus)
strwrap(my.corpus[[2]])


my.corpus <- tm_map(my.corpus, removePunctuation)
strwrap(my.corpus[[1]])
my.corpus <- tm_map(my.corpus, removeNumbers)  #optional - need dates (PosiXct)
strwrap(my.corpus[[1]])
my.corpus <- tm_map(my.corpus, tolower)
strwrap(my.corpus[[1]])
my.corpus <- tm_map(my.corpus, stripWhitespace)
strwrap(my.corpus[[1]])
my.corpus <- tm_map(my.corpus, PlainTextDocument)
term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)
inspect(term.doc.matrix.stm)

nc<-ncol(term.doc.matrix.stm)

#a<-NULL
#final_link<-NULL
#options(useFancyQuotes=FALSE)


#for(i in 1:nc){a=paste(paste0(dQuote(paste0("doc",i))),",")print(a)}

colnames(term.doc.matrix.stm)<-c("doc1","doc2","doc3","doc4")

inspect(term.doc.matrix.stm[0:14, ])
inspect(term.doc.matrix.stm)

term.doc.matrix <- as.matrix(term.doc.matrix.stm)

cat("Dense matrix representation size", object.size(term.doc.matrix), "bytes.\n", 
    "Simple matrix size", object.size(term.doc.matrix.stm), "bytes.\n")

N.docs<-nc
  
get.tf.idf.weights <- function(tf.vec, df) {
  weight = rep(0, length(tf.vec))
  weight[tf.vec > 0] = (1 + log2(tf.vec[tf.vec > 0])) * log2(N.docs/df)
  weight
}

get.weights.per.term.vec <- function(tfidf.row) {
  term.df <- sum(tfidf.row[1:N.docs] > 0)
  tf.idf.vec <- get.tf.idf.weights(tfidf.row, term.df)
  return(tf.idf.vec)
}

tfidf.matrix <- t(apply(term.doc.matrix, c(1), FUN = get.weights.per.term.vec))
colnames(tfidf.matrix) <- colnames(term.doc.matrix)

term.doc.matrix[0:3, ]
tfidf.matrix[0:3, ]

tfidf.matrix <- scale(tfidf.matrix, center = FALSE, scale = sqrt(colSums(tfidf.matrix^2)))
tfidf.matrix[,6]

query.vector <- tfidf.matrix[, (N.docs)]
tfidf.matrix <- tfidf.matrix[, 1:N.docs]

doc.scores <- t(query.vector) %*% tfidf.matrix
doc.scores


index<-NULL

for(i in 1:2)
{
  a<-doc.scores[doc.scores==max(doc.scores)]
  index[i]<-which(doc.scores == max(doc.scores))
  doc.scores<-doc.scores[!doc.scores==a]
}

index

a<-new_data[index][-1]

write.table(a,"habileop")
getwd()
