source("/cloud/project/Mining Homer/Full Miner/HomeR.R")
source("/cloud/project/Mining Homer/Full Miner/SCA/ThRax.R")


# It can be typed manually faster
# than it can be automated
### NEVERMIND
ReconList <- IliadTibble %>%
  filter(str_detect(Book, "^6$") == T) %>%
  filter(str_detect(RelativeLine,
                    "(39\\d|40\\d|41[012]|46[6789]|47\\d|48[01])")
         == T) #%>%
  #write.table(file = "/cloud/project/Mining Homer/Full Miner/Recon.csv")
  

HiatusList <- ReconList %>%
  filter(str_detect(Word, "\\w*[αηιωυεο][αηιωυεοϊϋ]{1,3}[αηιωυεοϊϋ]{0,3}\\w*\\b") == T) %>%
  filter(!str_detect(Word, "\\w*[αηιωυεο][αηωεοϊϋ]{1,3}\\w*\\b") == F) #%>%
  #write.table(file = "/cloud/project/Mining Homer/Full Miner/Hiatus.csv")



Romanize <- function(Greek) {
  
  Greek <- gsub("ϊ",
                "ï",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("ϋ",
                "ü",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("αι",
                "aĭ",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("ει",
                "e:",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("ηι",
                "ɛ:ĭ",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("οι",
                "oĭ",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("υι",
                "yĭ",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("ωι",
                "ɔ:ĭ",
                Greek,
                ignore.case = T)
  
  Greek <-  gsub("αυ",
                 "aŭ",
                 Greek,
                 ignore.case = T)
  
  Greek <- gsub("ευ",
                "eŭ",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("ηυ",
                "ɛ:ŭ",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("ου",
                "o:",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("ωυ",
                "ɔ:ŭ",
                Greek,
                ignore.case = T)
  
  
  Greek <- gsub("α",
               "a",
               Greek,
               ignore.case = T)
  
  Greek <- gsub("ε",
                "e",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("η",
                "ɛ:",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("ι",
                "i",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("ο",
                "o",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("υ",
                "y",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("ω",
                "ɔ:",
                Greek,
                ignore.case = T)
  
  

  Greek <- gsub("β",
                "b",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("γ",
                "g",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("δ",
                "d",
                Greek,
                ignore.case = T)
  
  
  Greek <- gsub("ζ",
                "z",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("θ",
                "q",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("κ",
                "k",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("λ",
                "l",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("μ",
                "m",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("ν",
                "n",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("ξ",
                "h",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("π",
                "p",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("ρ",
                "r",
                Greek,
                ignore.case = T)

  Greek <- gsub("[σς]",
                "s",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("τ",
                "t",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("φ",
                "f",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("χ",
                "x",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("ψ",
                "c",
                Greek,
                ignore.case = T)
  
  Greek <- gsub("ϝ",
                "w",
                Greek,
                ignore.case = T)
  
}

### Lookarounds need 'perl = T'
WList <- Romanize(HiatusList$Word)
  WList <- c(
    gsub(
        "(a|a:|e|e:|ɛ:|i|i:|o|o:|ɔ:|y|y:)(a|a:|e|e:|ɛ:|i|i:|o|o:|ɔ:|y|y:|ï|ü)↵
        (?![aeɛioɔyïüĭŭ])","\\1w\\2", WList, perl = T)) # 2 - V.V doesn't cap SVs
  WList <- c(
    gsub(
        "(a|a:|e|e:|ɛ:|i|i:|o|o:|ɔ:|y|y:)([ĭŭ])(a|a:|e|e:|ɛ:|i|i:|o|o:|ɔ:|y|y:)",
        "\\1\\2w\\3", WList, perl = T))             # 3 - VS.V 
  WList <- c(
    gsub(
        "(a|a:|e|e:|ɛ:|i|i:|o|o:|ɔ:|y|y:)(a|a:|e|e:|ɛ:|i|i:|o|o:|ɔ:|y|y:)([ĭŭ])",
        "\\1w\\2\\3", WList, perl = T))             # 3 - V.VS
HiatusList %>%
  add_column(WList) %>%
  add_column(applysoundchange(WList)) %>%
  View(title = "Digamma Results")