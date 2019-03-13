#  Authors Pedro Concejero pedro.concejerocerezo@gmail.com
#  Inés Merino Lobo i.merinolo@gmail.com
#  11/03/2019 

#  Purpose of program, inputs, and outputs

#  Purpose: topicmodels de 2 y pico millones de trolls rusos

#  Librerías necesarias

# esencial para SNA
library(igraph)

# Para la manipulación de los mensajes
library(stringr)
library(tm)

# Y por supuesto topicmodels
library(topicmodels)

# Used for filtering out categories with 0
library("slam")
# For producing the wordclouds
library(RColorBrewer)
library(wordcloud)


# Pon tu directorio de trabajo

setwd("D:/trolls/russian-troll-tweets-master")


load("wkspace.rda")

dim(quant_dfm)

dtm <- quant_dfm

dtm.redux.for.topicmodel <- removeSparseTerms(dtm, 0.999)

dim(dtm.redux.for.topicmodel)

head(quant_dfm@Dimnames$features, n = 100)


# The mean term frequency-inverse document frequency (tf-idf) over documents containing
# this term is used to select the vocabulary. This measure allows to omit terms which have low
# frequency as well as those occurring in many documents. We only include terms which have
# a tf-idf value of at least 0.1 which is a bit less than the median and ensures that the very
# frequent terms are omitted.

# After this pre-processing we have the following document-term matrix with a reduced vocabulary
# which we can use to fit topic models.

# To keep the code exactly as in topicmodels vignette

JSS_dtm <- quant_dfm

# CAVEAT Error en LDA(JSS_dtm, k = k, control = list(seed = SEED)) : 
# Each row of the input matrix needs to contain at least one non-zero entry

#library("slam")
summary(row_sums(JSS_dtm))

JSS_dtm <- JSS_dtm[row_sums(JSS_dtm) > 0,]
summary(col_sums(JSS_dtm))


dim(JSS_dtm)

inspect(JSS_dtm[1:10,1:10])


## STARTING ("topicmodels")

time1 <- Sys.time()

k <- 10
SEED <- 2010
jss_TM <-
	list(VEM = LDA(JSS_dtm, k = k, control = list(seed = SEED)),
			 VEM_fixed = LDA(JSS_dtm, k = k,
			 								control = list(estimate.alpha = FALSE, seed = SEED)),
			 Gibbs = LDA(JSS_dtm, k = k, method = "Gibbs",
			 						control = list(seed = SEED, burnin = 1000,
			 													 thin = 100, iter = 1000)),
			 CTM = CTM(JSS_dtm, k = k,
			 					control = list(seed = SEED,
			 												 var = list(tol = 10^-4), em = list(tol = 10^-3))))

sapply(jss_TM[1:2], slot, "alpha")

# The most likely topic for each document is obtained by

Topic <- topics(jss_TM[["VEM"]], 1)

#The 20 most frequent terms for each topic are obtained by

Terms <- terms(jss_TM[["VEM"]], 20)
Terms[,1:20]


time2 <- Sys.time()

print(time2-time1)


## Stopped here

# The most likely topic of the papers which appeared in Volume 24 called
# Statistical Modeling of Social Networks with `statnet"' is given by


(topics_v24 <-
	topics(jss_TM[["VEM"]])[grep("/v24/", JSS_papers[, "identifier"])])

most_frequent_v24 <- which.max(tabulate(topics_v24))

# The similarity between these papers is indicated by the fact that the majority of the papers
# have the same topic as their most likely topic. The ten most likely terms for topic 28 are given # by

terms(jss_TM[["VEM"]], 10)[, most_frequent_v24]

### Trying to draw a wordcloud of each top20 terms per topic


# For extracting the terms (columns) to topics (rows) we use beta

kk <- jss_TM[["VEM"]]@beta
class(kk)
dim(kk)
colnames(kk) <- jss_TM[["VEM"]]@terms
head(kk)

# We define the matrix of plots
# How to do this well explained here
# http://www.statmethods.net/advgraphs/layout.html
# 6 colums * 600 = 3000 width
# 5 rows * 600 = 3600 height

png(file="mygraphic_hi_res_def_def.png",
		width=3600,
		height=3000,
		res = 300,
		bg = "black")

par(mfrow=c(6,5))

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
	wordcloud(d$word,d$freq^3, scale=c(2.2,0.001),
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


# We define the matrix of plots
# How to do this well explained here
# http://www.statmethods.net/advgraphs/layout.html
# 6 colums * 600 = 3000 width
# 5 rows * 600 = 3600 height



save.image(out.wkspace)
