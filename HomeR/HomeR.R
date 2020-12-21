### Necessary Packages:
library(dplyr, warn.conflicts = F)
library(tidytext, warn.conflicts = F)
library(stringr, warn.conflicts = F)
library(tibble, warn.conflicts = F)
library(ggplot2, warn.conflicts = F)
library(R.utils, warn.conflicts = F)

### For Draft 4
setwd("/cloud/project/Mining Homer/Full Miner/Books")

### ### Cleans Accents ### ###
### (To be nested in the for loop)
CleanText <- function(Book){
  Book <- gsub("(ἀ|ἁ|ά|ὰ|ᾶ|ἄ|ἂ|ἆ|ἅ|ἃ|ἇ|ᾷ)",
               "α",
               Book,
               ignore.case = T)
  Book <- gsub("(ἠ|ἡ|ή|ὴ|ῆ|ἤ|ἢ|ἦ|ἥ|ἣ|ἧ|ῃ|ῂ|ῇ|ᾔ|ᾗ)",
               "η",
               Book,
               ignore.case = T)
  Book <- gsub("(ἰ|ἱ|ί|ὶ|ῖ|ἴ|ἲ|ἶ|ἵ|ἳ|ἷ)",
               "ι",
               Book,
               ignore.case = T)
  Book <- gsub("(ϊ|ΐ|ῒ)",
               "ϊ",
               Book,
               ignore.case = T)
  Book <- gsub("(ὠ|ὡ|ώ|ὼ|ῶ|ὤ|ὢ|ὦ|ὥ|ὣ|ὧ|ῳ|ῷ|ᾤ|ᾧ|ᾠ|ῴ)",
               "ω",
               Book,
               ignore.case = T)
  Book <- gsub("(ὐ|ὑ|ύ|ὺ|ῦ|ὔ|ὒ|ὖ|ὕ|ὓ|ὗ)",
               "υ",
               Book,
               ignore.case = T)
  Book <- gsub("(ΰ|ῢ|ϋ)",
               "ϋ",
               Book,
               ignore.case = T)
  Book <- gsub("(ἐ|ἑ|έ|ὲ|ἔ|ἒ|ἕ|ἓ)",
               "ε",
               Book,
               ignore.case = T)
  Book <- gsub("(ὀ|ὁ|ό|ὸ|ὄ|ὂ|ὅ|ὃ)",
               "ο",
               Book,
               ignore.case = T)
  Book <- gsub("(ῤ|ῥ)",
               "ρ",
               Book,
               ignore.case = T)
  
  return(Book)
}

### ### Creates Tibbles ### ###

IliadTibble <- tibble()

for (BookNum in c(1:24)) {
  
  ### Read Book ###
  assign( # Assigns the variable Text_n
    paste("Text", BookNum, sep = ""), # Choses the variable name
    readLines(paste("IliadBook", BookNum, sep="")) %>% # Reads the text into the variable
      CleanText() # Cleans the text before assignment
  )
  
  ### Tibblize ###
  
  # Tibbles the variable Text_n as Tibble_n
  assign(paste("Tibble", BookNum, sep = ""),  
         tibble(
           Book = BookNum,
           RelativeLine = 1:countLines(
             paste("IliadBook", BookNum, sep = "")),
           text = get(paste("Text", BookNum, sep = ""))
          )
         ) #%>%
           #mutate(Book = BookNum))
  
  # Binds outputs into one Tibble
  IliadTibble <- rbind(IliadTibble, get(paste("Tibble", BookNum, sep = "")))
}

### Global Line numbers
IliadTibble <- IliadTibble %>%
  add_column(GlobalLine = c(1:nrow(IliadTibble)), .before = "Book")

### Unnest
IliadTibble <- IliadTibble  %>%
  unnest_tokens(Word, text)

### Clean Values
rm(list = ls(pattern = "(^Text|^Tibble)\\d"))