rm(list = ls())

setwd("E:/Graduate Studies/Misc/RM/Marshall")

##### Just replace the data file for the read and write code, line 7 and and 71 respectively 

dat_messy <- read.csv(file = "Texas General Elections 2020 (Raw).csv")
dat_messy <- dat_messy[,-1]

dat <- data.frame(matrix(ncol = 4))

for(county in 1:(length(dat_messy)/4)){
  dat <- data.frame(mapply(c, dat, subset(dat_messy, select = (1 +((county-1)*4)):(4 +((county-1)*4))))) 
  print(county)
}

dat <- dat[-1,]
dat[is.na(dat)] <- ""
rownames(dat) <- 1:nrow(clean_dat)

clean_dat <- data.frame()
clean_dat <- data.frame(matrix(ncol = 8, nrow = nrow(dat)))
colnames(clean_dat) <- c("Total Votes", "County", "County Office", "Candidate", "Party", "Candidate Type", "Election Votes", "Percent")

vote_total <- 1
candidate <- 1
row <- 1


for(row in 1:nrow(dat)){
  
  
  if(dat[row,2] == "D" | dat[row,2] == "R"){clean_dat$Party[candidate] <- dat[row,2]}
  if(dat[row,3] == "N" | dat[row,3] == "I"){clean_dat$`Candidate Type`[candidate] <- dat[row,3]}
  if(dat[row,2] == "D" | dat[row,2] == "R"){clean_dat$`Election Votes`[candidate] <- dat[row,4]}
  if(dat[row,2] == "D" | dat[row,2] == "R"){clean_dat$Candidate[candidate] <- dat[row,1]}
  
  if(dat[row,2] == "D" | dat[row,2] == "R"){
    
    increment <- 1
    
    while(is.na(clean_dat$`Total Votes`[candidate])){
      
      if(dat[row+increment,1] == ""){clean_dat$`Total Votes`[candidate] <- dat[row+increment,4]}
      increment <- increment + 1
    }
  }
  
  if(dat[row,2] == "D" | dat[row,2] == "R"){
    
    increment <- 1
    
    while(is.na(clean_dat$`County Office`[candidate])){
      
      if(dat[row - increment,1] == "Candidate"){clean_dat$`County Office`[candidate] <- dat[row-(increment + 1),1]}
      increment <- increment + 1
    }
  }
  
  clean_dat$Percent[candidate] <- (as.numeric(gsub(",","",clean_dat$`Election Votes`[candidate]))/as.numeric(gsub(",","",clean_dat$`Total Votes`[candidate])))*100
  
  county <- (row %/% nrow(dat_messy)) + 1
  clean_dat$County[candidate] <- dat[(county-1)*nrow(dat_messy) + 3, 1]
  
  if(dat[row,2] == "D" | dat[row,2] == "R"){candidate <- candidate + 1}
}


clean_dat <- clean_dat[1:which(is.na(clean_dat$`Total Votes`))[1]-1,]

write.csv(clean_dat, file = "Texas General Election 2020 (Cleaned).csv")








