#NLP training script from LinkedIn Learning

usethis::use_git_config(user.name="Harriet Larvin", user.email="h.larvin@qmul.ac.uk")
usethis::create_github_token()

# install.packages("tm")

library(dplyr)

#set training files as working directory
setwd(
  "C:/Users/wpw244/OneDrive - Queen Mary, University of London/Projects/Practice/NLP training/Exercise Files/"
)

# corpus: R object that contains docs in a simplified structure for analysing the text

#### Use baseR and the text mining (tm) package
library(tm)

#Identify the most frequent words used in a folder of text files
topPoetryTerms_tm <- Corpus(DirSource(directory = "poetry",
                                      pattern = "*.txt")) %>%
  DocumentTermMatrix(
    control = list(
      tolower = TRUE,
      removePunctuation = TRUE,
      stopwords = TRUE,
      removeNumbers = TRUE
    )
  ) %>%
  removeSparseTerms(sparse = .1) %>%
  as.matrix() %>%
  colSums() %>% #N times a term appears in the corpus
  sort(decreasing = T) %>%
  head(n = 10)

#### Use baseR and the text mining (quanteda) package
library(quanteda)
library(readtext)

topPoetryTerms_quanteda <-
  readtext("poetry/*.txt", docvarsfrom = "filenames") %>%
  corpus() %>%
  tokens(remove_numbers = T, remove_punct = T) %>%
  tokens_remove(pattern = stopwords("english")) %>% 
  tokens_tolower() %>% 
  dfm() %>% 
  topfeatures()

#### Use baseR and the text mining (tidytext) package
library(tidyverse)
library(tidytext)

topPoetryTerms_tidytext <-
  list.files(path = "poetry", pattern = "*txt",
             full.names = T) %>% 
  map_df(~data_frame(txt = read_file(.x))) %>% 
  unnest_tokens(word,txt) %>% #strips punctuation and converts to lowercase
  filter(is.na(as.numeric(word))) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = T) %>% 
  head(n=10)

########################## Corpora ##############################
# Create new corpus
newCorpus <- Corpus(DirSource(directory = "poetry",
                              pattern = "*.txt"))
# Show structure of corpus
str(newCorpus)
class(newCorpus)

#Volatile corpus
list.files()
newVCorpus <- VCorpus(DirSource(directory = "poetry",
                                pattern = "*.txt"))
str(newVCorpus)
class(newVCorpus)

#simple corpus
newSimpleCorpus <- SimpleCorpus(DirSource(directory = "poetry",
                                          pattern = "*.txt"))
summary(newSimpleCorpus )


########################## creating Corpus from diff sources ##############################
getSources()

#data frame source
#Create dataframe with txt file names + contents
aDataframe <- readtext(list.files(path = "poetry",
                                  patter = "*txt",
                                  full.names = T))
#Create corpus from this dataframe
aCorpus <- Corpus(DataframeSource(aDataframe))
summary(aCorpus)

#directory Source
newCorpus <- Corpus(DirSource(directory = "poetry",
                              pattern = "*.txt"))
summary(newCorpus)

#vector source
VectorOfText <- c("And they lived happily ever after",
                  "Three little piggies",
                  "He huffed and he puffed")
names(VectorOfText) <- c("firstline","secondline","thirdline")

aCorpus <- VCorpus(VectorSource(VectorOfText))
summary(aCorpus)

########################## combining and subsetting corpora ##############################
simpCorp <- readRDS("poetCorpus.RDS")
newVCorpus <- VCorpus(DirSource(directory = "poetry",
                                pattern = "*.txt"))

#Combine
combinedCorpora <- c(simpCorp,newVCorpus) # uses the class of the first element in the combine function
combinedCorpora <- c(newVCorpus,simpCorp)

#filtering with tm
filteredCorpus <- tm_filter(simpCorp,
                            FUN = function(x) grepl("Will not",x))
summary(filteredCorpus)

#indexing with tm (numbers with a match)
tm_index(simpCorp, FUN = function(x) grepl("Will not", x))

########################################## Part2 ###############################
