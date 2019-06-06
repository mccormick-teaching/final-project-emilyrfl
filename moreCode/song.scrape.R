##### WARNING- takes long time to run. Can just use the pre-made objects that I stored in Github
require(geniusr)
require(genius)
require(tidyverse)
require(rvest)

### Plan: we will give ourselves a 2 month buffer on either side of Trump's election.
### We will take 1 year worth of data on either side
### So "pretrump" will be 09-01-2015 to 09-01-2016
#### Post trump will be 01/01/2017-01/01/2018
#### 52 weeks for each

### Generate ALL dates from 2015-2017 to have a bank to choose from
months <- c("01", "02", "03", "04", "05", "06", "07", "08","09","10","11","12")
monthdays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
days <- unlist(sapply(1:12, function(u) rep(months[u], monthdays[u])))
alldays <- unlist(sapply(monthdays, function(u) 1:u))
strdays <- sapply(alldays, function(u) ifelse(u<10, paste0("0",u), as.character(u)))
dates1 <- paste0("2015-", days, "-", strdays)
dates2 <- paste0("2016-", days, "-", strdays)
dates3 <- paste0("2017-", days, "-", strdays)
alldates <- c(dates1, dates2, dates3)

### Now, from bank of ALL dates, select one date every 7 days for each of our age ranges
pre_trump_scrape_dates <- alldates[seq(which(alldates=="2015-09-01"), which(alldates=="2015-09-01")+365, by=7)]
post_trump_scrape_dates <- alldates[seq(which(alldates=="2017-01-01"), which(alldates=="2017-01-01")+365, by=7)]

### Scrape Billboard Top 40 Pop Song/Artist for all of these dates
### Note: due to ties or something some weeks have 41-44 songs in the top 40. 
### That is why I am giving a little bit of extra capacity to this empty dataframe
allSongs <- data.frame(matrix(0, nrow=2*52*44, ncol=3))
names(allSongs) <- c("Title", "Artist", "PostTrump")
i=1
for (date in pre_trump_scrape_dates) {
  webpage <- read_html(paste0('https://www.billboard.com/charts/pop-songs/', date))
  badranks <- webpage %>% rvest::html_nodes("div") %>% rvest::html_attr("data-rank")
  badtitles <- webpage %>% rvest::html_nodes("div") %>% rvest::html_attr("data-title")
  badartists <- webpage %>% rvest::html_nodes("div") %>% rvest::html_attr("data-artist")
  ranks <- badranks[!is.na(badranks)]
  titles <- badtitles[!is.na(badranks)]
  artists <- badartists[!is.na(badranks)]
  n <- length(ranks)
  allSongs[i:(i+n-1),] = cbind(titles, artists, 0)
  i=i+n
}
for (date in post_trump_scrape_dates) {
  webpage <- read_html(paste0('https://www.billboard.com/charts/pop-songs/', date))
  badranks <- webpage %>% rvest::html_nodes("div") %>% rvest::html_attr("data-rank")
  badtitles <- webpage %>% rvest::html_nodes("div") %>% rvest::html_attr("data-title")
  badartists <- webpage %>% rvest::html_nodes("div") %>% rvest::html_attr("data-artist")
  ranks <- badranks[!is.na(badranks)]
  titles <- badtitles[!is.na(badranks)]
  artists <- badartists[!is.na(badranks)]
  n <- length(ranks)
  allSongs[i:(i+n-1),] = cbind(titles, artists, 1)
  i=i+n
}
### Get rid of extra capacity; only keep spots we filled
allSongs <- allSongs[1:(i-1),]

### Some songs could have been on list multiple weeks in a row. We should only have unique songs
### justification for "minimum". If it was on list both before and after trump (there are only like 5
### of these) it was written and gained popularity pre-trump, so it makes more sense to put it with
### pre trump
uniqueSongs <- allSongs[!duplicated(allSongs[,1:2]),]
  
#allSongs %>% group_by(Title, Artist) %>% summarise(PostTrump = min(PostTrump))
n <- NROW(uniqueSongs)
uniqueSongs$ID <- 1:n


### Now for each uniquesong we need lyrics
### Handling "featuring" separately seemed to adequately get rid of a good number 
### of errors. 
full_lyrics <- vector("list", n)
for (i in 1:n) {
  test = try( {
  try_lyrics <- paste(genius_lyrics(artist = uniqueSongs$Artist[i], song = uniqueSongs$Title[i])$lyric, collapse=" ")
  full_lyrics[i] <- try_lyrics
  }, silent=TRUE)
  if (class(test) == 'try-error') {
    artist <- str_split(uniqueSongs$Artist[i], " ")[[1]]
    if (length(which(artist=="Featuring")) > 0) {
      newartist <- artist[1:(which(artist=="Featuring")-1)]
      test2 = try({
      try_lyrics <- paste(genius_lyrics(artist = paste(newartist, collapse=" "), song = uniqueSongs$Title[i])$lyric, collapse=" ")
      full_lyrics[i] <- try_lyrics
    }, silent=TRUE)
    if (class(test2) == "try-error") {
      print(i)
      print(uniqueSongs$Artist[i])
      print(uniqueSongs$Title[i])
  }
    } else {
      if (length(which(artist=="&")) > 0) {
        newartist <- artist[1:(which(artist=="&")-1)]
        artist[artist=="&"] <- "and"
        test3 = try({
          try_lyrics <- paste(genius_lyrics(artist = paste(newartist, collapse=" "), song = uniqueSongs$Title[i])$lyric, collapse=" ")
          full_lyrics[i] <- try_lyrics
        }, silent=TRUE)
        if (class(test3) == "try-error") {
        test4 = try({
          try_lyrics <- paste(genius_lyrics(artist = artist, song = uniqueSongs$Title[i])$lyric, collapse=" ")
          full_lyrics[i] <- try_lyrics
        }, silent=TRUE)
        if (class(test4)=="try-error") {
          print(i)
          print(uniqueSongs$Artist[i])
          print(uniqueSongs$Title[i])
        }
        }
      } else {
      print(i)
      print(uniqueSongs$Artist[i])
      print(uniqueSongs$Title[i])
    }
    }
  }
}

### Just a few cases to do separately. 
### These were only ones that failed above
full_lyrics[227] <- paste(genius_lyrics(artist ="Dram", song="broccoli")$lyric, collapse=" ")
full_lyrics[280] <- paste(genius_lyrics(artist ="Luis Fonsi and Daddy Yankee", song="Despacito remix")$lyric, collapse=" ")
full_lyrics[264] <- paste(genius_lyrics(artist ="Axwell Ingrosso", song="I Love You")$lyric, collapse=" ")
full_lyrics[6] <- paste(genius_lyrics(artist ="Jack u and Justin Bieber", song="Where Are U Now")$lyric, collapse=" ")
full_lyrics[99] <- paste(genius_lyrics(artist ="G-Eazy", song="Me Myself and I")$lyric, collapse=" ")
full_lyrics[183] <- paste(genius_lyrics(artist ="Nicky Jam", song="With You Tonight hasta el amanecer")$lyric, collapse=" ")
full_lyrics[199] <- paste(genius_lyrics(artist ="Machine Gun Kelly", song="Bad Things")$lyric, collapse=" ")
full_lyrics[204] <- paste(genius_lyrics(artist ="Zayn", song="I Don't Wanna Live Forever")$lyric, collapse=" ")
full_lyrics[246] <- paste(genius_lyrics(artist ="Kygo", song="It Ain't Me")$lyric, collapse=" ")
full_lyrics[321] <- paste(genius_lyrics(artist ="Justin Bieber", song="Friends")$lyric, collapse=" ")
#full_lyrics[341] <- paste(genius_lyrics(artist ="Ed Sheeran", song="Perfect")$lyric, collapse=" ")
#full_lyrics[351] <- paste(genius_lyrics(artist ="Lauv", song="I Like Me Better")$lyric, collapse=" ")
full_lyrics[355] <- paste(genius_lyrics(artist ="Selena Gomez", song="Wolves")$lyric, collapse=" ")
#full_lyrics[357] <- paste(genius_lyrics(artist ="Bebe Rexha", song="Meant To Be")$lyric, collapse=" ")
#full_lyrics[375] <- paste(genius_lyrics(artist ="Camila Cabello", song="Never Be the Same")$lyric, collapse=" ")
full_lyrics[337] <- paste(genius_lyrics(artist ="Cardi B", song="Bodak Yellow")$lyric, collapse=" ")

#### REMOVE DUPLICATES: must be removed both from full_lyrics and uniqueSongs
NROW(full_lyrics)
NROW(uniqueSongs)

#### FIRST: remove these rows that have no lyrics (they were duplicates)
uniqueSongs <- uniqueSongs[!sapply(full_lyrics, is.null),]
full_lyrics <- full_lyrics[!sapply(full_lyrics, is.null)]
NROW(full_lyrics)
NROW(uniqueSongs)

#### Now some regular cleaning
require(tm)
full_lyrics <- lapply(full_lyrics, function(u) tolower(u))

library(textcat)
english <- sapply(full_lyrics, function(u) textcat(u))
indices <- which(english=="spanish")

uniqueSongs <- uniqueSongs[-indices,]
full_lyrics <- full_lyrics[-indices]
NROW(uniqueSongs)
NROW(full_lyrics)
uniqueSongs$ID <- 1:NROW(uniqueSongs)

corpus <- VCorpus(VectorSource(unlist(full_lyrics)))
cleaning_phase = tm_map(corpus,removePunctuation)
cleaning_phase = tm_map(cleaning_phase,removeNumbers)
cleaning_phase = tm_map(cleaning_phase, stripWhitespace)




### Need to make it remove stopwords
### But idk how many we want
### The precomputed lists look too BIG to me
mystopwords <- c("i", "the", "a", "am", "if", "of", "do", "be", "was", "is", "if", "what",
                 "who", "will", "were", "and", "all", "that", "this", "i", "you", "we",
                 "it", "oh", "can", "can't", "he", "she", "they", "give", "thing", "with", "ive", "got",
                 "there", "are", "from", "been", "im", "ill", "to", "in","that")
#### NOTE-- change this so that words_we_want is like cleaning_phase but with
#### stopwords removed. Easy to do once we decide which words we want removed
words_we_want <- tm_map(cleaning_phase, removeWords, mystopwords)





save(full_lyrics, file="full_lyrics.Rdata")
save(uniqueSongs, file="uniqueSongs.Rdata")
save(words_we_want, file="words_we_want.Rdata")

