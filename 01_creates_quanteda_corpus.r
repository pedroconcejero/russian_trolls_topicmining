

#install.packages("quanteda") 
#install.packages("topicmodels")

library(quanteda)
library(topicmodels)

# Creamos un corpus con el contenido de los tuits
trolls <- corpus(trollstot$content[trollstot$language == "English"])

dim(trollstot)
table(trollstot$language)
length(trolls)
summary(trolls)

# Vemos como crear metadato de tipo de tuit, con quanteda::docvars
# Recordemos que tipo de troll es account_category

docvars(trolls, "Category") <- trollstot$account_category[trollstot$language == "English"]
docvars(trolls, "Retweet") <- trollstot$retweet[trollstot$language == "English"]
summary(trolls)


#####
# kwic keyword in context
kwic(trolls, pattern = "terror")


# make a dfm, removing stopwords and applying stemming
myStemMat <- dfm(trolls, remove = stopwords("english"), stem = FALSE, remove_punct = TRUE)

topfeatures(myStemMat, 20)  # 20 top words
topfeatures(myStemMat, 100)  # 100 top words

# Es facilísimo hacer un worcloud
set.seed(100)
textplot_wordcloud(myStemMat, 
                   min_count = 100, 
                   random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

# quanteda makes it very easy to fit topic models as well, e.g.:
  
quant_dfm <- dfm(trolls, 
                 remove_punct = TRUE, 
                 remove_numbers = TRUE, 
                 remove = stopwords("english"))
quant_dfm <- dfm_trim(quant_dfm, min_termfreq = 100)
quant_dfm

## Document-feature matrix of: 14 documents, 1,263 features (64.5% sparse).

# set.seed(100)
# if (require(topicmodels)) {
#   my_lda_fit20 <- LDA(convert(quant_dfm, to = "topicmodels"), k = 20)
#   get_terms(my_lda_fit20, 5)
# }

save.image(file = "wkspace.rda")
