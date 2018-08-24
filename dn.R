###### Daily ninja project 

rm(list=ls())

getwd()

setwd("D:/projects/Daily ninja")

data=read.csv("houseelf_earlength_dna_data.csv",header = T)

library(stringr)

str(data)

GC_content <- function(dna){
  str_to_upper(dna)
  Gs <- str_count(dna, "G")
  Cs <- str_count(dna, "C")
  gc_content <- (Gs + Cs) / str_length(dna) * 100
  return(gc_content)
}

get_ear_length <- function(seq){
  #Determine the ear length category
  ear_lengths <- ifelse(seq > 10, "large", "small")
  return(ear_lengths)
}

dna_obs <- nrow(data)
dna_seq <- data[,3]
dna_out <- data.frame(id=numeric(length=dna_obs), earlength_class=numeric(length=dna_obs), gc_content=numeric(length=dna_obs))

for(n in 1:dna_obs){
  id <- data[n,1]
  earlength <- data[n,2]
  earlength_class <- get_ear_length(earlength)
  dna <- dna_seq[n]
  gc_result <- GC_content(dna)
  dna_out[n,1] <- id
  dna_out[n,2] <- earlength_class
  dna_out[n,3] <- gc_result
}

small=dna_gc[which(dna_gc$earlength_class=="small"),]

small

mean(small$gc_content)

large=dna_gc[which(dna_gc$earlength_class=="large"),]

mean(large$gc_content)

large

write.csv(dna_out,"grinert_analysis.csv",row.names =T)
