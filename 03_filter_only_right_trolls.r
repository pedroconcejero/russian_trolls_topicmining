

#install.packages("quanteda") 
#install.packages("topicmodels")

library(quanteda)
library(topicmodels)
library(data.table)
# For producing the wordclouds
library(RColorBrewer)
library(wordcloud)

setwd("D:/trolls/russian-troll-tweets-master")

load("todos_los_trolls.rda")

table(trollstot$account_category, trollstot$retweet)

righttrolls <- trollstot[trollstot$account_category == "RightTroll" 
                         & trollstot$retweet == "0"
                         & trollstot$language == "English",]
dim(righttrolls)

# Creamos un corpus con el contenido de los tuits
trolls <- corpus(righttrolls$content)

dim(trollstot)
table(trollstot$language)
length(trolls)
summary(trolls)


#####
# kwic keyword in context
kwic(trolls, pattern = "obama")

# make a dfm, removing stopwords and applying stemming
additional_stopwords <- c("rt",
                          "lol",
                          "well")
mycorpus <- corpus(additional_stopwords)
mystopwords <- c(stopwords("en"), additional_stopwords)
mytoks <- 
  tokens(trolls) %>%
  tokens_remove(mystopwords, padding = TRUE)
#mytoks
myStemMat <- dfm(mytoks,
                 ngrams = 2, concatenator = "-",
#                 remove = mystopwords,
                 stem = FALSE, 
                 remove_punct = TRUE)

topfeatures(myStemMat, 100)  # 100 top bigrams

# Es facilísimo hacer un worcloud
set.seed(100)
textplot_wordcloud(myStemMat, 
                   min_count = 100, 
                   random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

# quanteda makes it very easy to fit topic models as well, e.g.:

time1 <- Sys.time()

quant_dfm <- dfm_trim(myStemMat, min_termfreq = 100)

set.seed(100)
if (require(topicmodels)) {
   my_lda_fit20 <- LDA(convert(quant_dfm, to = "topicmodels"), k = 20)
   get_terms(my_lda_fit20, 5)
}

time2 <- Sys.time()
print(time2-time1)


# PLOTTING WORDCLOUD MATRIX

kk <- my_lda_fit20@beta
class(kk)
dim(kk)
colnames(kk) <- my_lda_fit20@terms
head(kk)

# We define the matrix of plots
# How to do this well explained here
# http://www.statmethods.net/advgraphs/layout.html
# 6 colums * 600 = 3000 width
# 5 rows * 600 = 3600 height

png(file="RIGHTTROLLS_hi_res_def_def.png",
    width=3600,
    height=3000,
    res = 300,
    bg = "black")

par(mfrow=c(4,5))

for (k in 1:length(kk[,1])) {
  
  topic1 <- kk[k,]
  
  v <- topic1
  
  d <- data.frame(word = names(v),rank= rank(v))
  
  d <- d[order(-d$rank),]
  
  d$freq <- d$rank - max(d$rank) + 100
  # Now with a prettier layout
  # baed on code published in
  # http://onertipaday.blogspot.com.es/2011/07/word-cloud-in-r.html
  #plot.new()
  
  pal2 <- brewer.pal(11,"Spectral")
  wordcloud(d$word,d$freq, 
            scale=c(0.7,0.001),
            max.words=70, 
            random.order=FALSE, 
            rot.per= 0, 
            colors=pal2,
            random.clor = TRUE)
  title(main = paste(k),
        font = 10,
        col.main = "yellow")
}


dev.off()


save.image(file = "wkspace_righttrolls.rda")
