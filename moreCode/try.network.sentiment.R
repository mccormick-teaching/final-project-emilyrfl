#### Try making network models, where first we truncate so that we are only including
#### words rated with high sentiment from the affin library.
### Less dimensions. 
load("words_we_want.Rdata")
load("full_lyrics.Rdata")
load("unique_songs.Rdata")
library(tm)

## Make matrices
words_we_want2 <- tm_map(words_we_want, stemDocument)
tdm <- TermDocumentMatrix(words_we_want2, control=list(bounds = list(global = c(2, 200))))
tdm.idf <- weightTfIdf(tdm, normalize = TRUE)
tdm.full <- TermDocumentMatrix(words_we_want2)
tdm.full.mat <- as.matrix(tdm.full)

### Get only the sentiment words. 
affin_score <- get_sentiments("afin")
affin_score$word <- stemDocument(affin_score$word)
affin_score <- affin_score[!duplicated(affin_score$word),]
affin_strong_score <- affin_score[abs(affin_score$score)>1,]

### Reduce the tdm mat to only include these. Leaves us with a vocab of only 382
tdm.sent.mat <- tdm.full.mat[rownames(tdm.full.mat) %in% affin_strong_score$word, ]

### Adjacecy matrix:: doc-doc sim
adj <- t(tdm.sent.mat)%*%tdm.sent.mat

edge_cutoff_low <- 49
edge_cutoff_high <- 50
adj[adj<edge_cutoff_low] <- 0
adj[adj>edge_cutoff_high] <- edge_cutoff_high

require(network)
network1 <- network(x = adj, matrix.type="adjacency", directed=FALSE)

library(VBLPCM)
v.start<-vblpcmstart(network1,G=2,d=10,LSTEPS=1e3)
v.fit<-vblpcmfit(v.start,STEPS=20)
vblpcmgroups(v.fit)
plot(v.fit)

### Okay- so it didn't make us any meaningful clusters. Big bummer.
### But let's still compute pairwise distances and see if there is any pre/post
### trump information in the latent positions
NROW(v.fit$V_z)

ergmm(network1~euclidean(d=2, G=2), control = control.ergmm(sample.size=1000, burnin=200))



