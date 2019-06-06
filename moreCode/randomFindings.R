#### Pre-Post Trump: How many songs used "LOVE" is ODDLY consistent
table(uniqueSongs[which(dtm_mat["love",]>0),]$PostTrump)/table(uniqueSongs$PostTrump)

### Proportion using beauty increased
table(uniqueSongs[which(dtm_mat["beauti",]>0),]$PostTrump)/table(uniqueSongs$PostTrump)


#### Bad words went up too tho. For some kind of a lot. 
table(uniqueSongs[which(dtm_mat["fuck",]>0),]$PostTrump)/table(uniqueSongs$PostTrump)
table(uniqueSongs[which(dtm_mat["shit",]>0),]$PostTrump)/table(uniqueSongs$PostTrump)

