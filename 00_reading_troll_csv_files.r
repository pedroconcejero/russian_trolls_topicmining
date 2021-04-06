# Leer los tuits trolls 
# Cortesia de FiveThirtyEight
# https://fivethirtyeight.com/features/why-were-sharing-3-million-russian-troll-tweets/
# Inés Merino, Pedro Concejero, 2019

# Cambia a tu dir de trabajo. Aquí habrás descargado PREVIAMENTE el .zip de aqui
# (MEJOR DESDE UN NAVEGADOR)
# y luego descomprime el .zip
# https://github.com/fivethirtyeight/russian-troll-tweets/archive/master.zip

library(data.table)
library(stringr)

setwd("/home/pedro/Escritorio/UTAD_2021_Q2/BUSQUEDA Y ANALISIS INFORMACION/russian_trolls_topicmining/russian-troll-tweets-master")


# Lectura y carga de cada una de las 13 piezas por separado

trollfiles <- list.files(pattern = glob2rx("*.csv"))
trollfiles

colclasses <- c(rep("character", 7),
                rep("integer", 3),
                rep("factor", 5),
                rep("character", 6))
#1

trolls1 <- fread(trollfiles[1],
                 colClasses = colclasses,
                 encoding = "UTF-8")
dim(trolls1)

trolls1 <- trolls1[, c(1:16)]
dim(trolls1)
str(trolls1)

#2

trolls2 <- fread(trollfiles[2], 
                 colClasses = colclasses,
                 encoding = "UTF-8")
dim(trolls2)

trolls2 <- trolls2[, c(1:16)]
dim(trolls2)

trollstot <- rbind(trolls1, trolls2)
dim(trollstot)

summary(trollstot)

#3

trolls3 <- fread(trollfiles[3], 
                 colClasses = colclasses,
                 encoding = "UTF-8")
dim(trolls3)

trolls3 <- trolls3[, c(1:16)]
dim(trolls3)

trollstot <- rbind(trollstot, trolls3)
dim(trollstot)

summary(trollstot)

#4

trolls4 <- fread(trollfiles[4], 
                 colClasses = colclasses,
                 encoding = "UTF-8")
dim(trolls4)

trolls4 <- trolls4[, c(1:16)]
dim(trolls4)

trollstot <- rbind(trollstot, trolls4)
dim(trollstot)

# 75% of tweets are in english
# 20% of all tweets in russian
# Only 5% of all tweets in other language

summary(trollstot)

#5

trolls5 <- fread(trollfiles[5], 
                 colClasses = colclasses,
                 encoding = "UTF-8")
dim(trolls5)

trolls5 <- trolls5[, c(1:16)]
dim(trolls5)

trollstot <- rbind(trollstot, trolls5,
                   encoding = "UTF-8")
dim(trollstot)

summary(trollstot)

#6

trolls6 <- fread(trollfiles[6], 
                 colClasses = colclasses,
                 encoding = "UTF-8")
dim(trolls6)

trolls6 <- trolls6[, c(1:16)]
dim(trolls6)

trollstot <- rbind(trollstot, trolls6)
dim(trollstot)

summary(trollstot)


#7

trolls7 <- fread(trollfiles[7], 
                 colClasses = colclasses,
                 encoding = "UTF-8")
dim(trolls7)

trolls7 <- trolls7[, c(1:16)]
dim(trolls7)

trollstot <- rbind(trollstot, trolls7)
dim(trollstot)

summary(trollstot)

#8

trolls8 <- fread(trollfiles[8], 
                 colClasses = colclasses,
                 encoding = "UTF-8")
dim(trolls8)

trolls8 <- trolls8[, c(1:16)]
dim(trolls8)

trollstot <- rbind(trollstot, trolls8)
dim(trollstot)

summary(trollstot)
str(trollstot)

#9

trolls9 <- fread(trollfiles[9], 
                 colClasses = colclasses,
                 encoding = "UTF-8")
dim(trolls9)

trolls9 <- trolls9[, c(1:16)]
dim(trolls9)

trollstot <- rbind(trollstot, trolls9)
dim(trollstot)

summary(trollstot)

#10

trolls10 <- fread(trollfiles[10], 
                 colClasses = colclasses,
                 encoding = "UTF-8")
dim(trolls10)

trolls10 <- trolls10[, c(1:16)]
dim(trolls10)

trollstot <- rbind(trollstot, trolls10)
dim(trollstot)

summary(trollstot)

#11

trolls11 <- fread(trollfiles[11], 
                  colClasses = colclasses,
                  encoding = "UTF-8")
dim(trolls11)

trolls11 <- trolls11[, c(1:16)]
dim(trolls11)

trollstot <- rbind(trollstot, trolls11)
dim(trollstot)

summary(trollstot)

#12

trolls12 <- fread(trollfiles[12], 
                  colClasses = colclasses,
                  encoding = "UTF-8")
dim(trolls12)

trolls12 <- trolls12[, c(1:16)]
dim(trolls12)

trollstot <- rbind(trollstot, trolls12)
dim(trollstot)

summary(trollstot)

#13

trolls13 <- fread(trollfiles[13], 
                  colClasses = colclasses,
                  encoding = "UTF-8")
dim(trolls13)

trolls13 <- trolls13[, c(1:16)]
dim(trolls13)

trollstot <- rbind(trollstot, trolls13)
dim(trollstot)

summary(trollstot)

# Limpiamos http y caracteres extraños

head(trollstot$content, n = 100)
trollstot$content <- str_replace_all(trollstot$content, "\"\"", "" )
trollstot$content <- str_replace_all(trollstot$content, pattern = "https?://([^/\\s]++)\\S*+", "" )
trollstot$content <- str_replace_all(trollstot$content, pattern = "&amp;", "" )
trollstot$content <- str_replace_all(trollstot$content, pattern = "@^", "" )

# Guardamos objeto R con todos

save(trollstot, file = "todos_los_trolls.rda")
