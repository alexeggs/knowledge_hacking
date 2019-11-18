runTextAnalysis <- function(textFile, ud_model, word_type) {

  # Load text file splitting words with space
  bookText <- paste(
    readLines(textFile)
    , collapse = " ")
  
  # Transform text into Corpus (collection of text documents)
  
  bookCorpus <- Corpus(VectorSource(bookText))
  
  # Transform using text mining transformations
  bookCorpus <- tm_map(bookCorpus, PlainTextDocument) %>% #map to plain text
    tm_map(content_transformer(tolower)) %>% #transform all words to lower case
    tm_map(removePunctuation) %>% #remove punctuation
    tm_map(removeNumbers) %>% #remove numbers
    tm_map(stripWhitespace) %>% #remove extra whitespace
    tm_map(removeWords, stopwords('spanish')) #remove common words
  
  dtm <- TermDocumentMatrix(bookCorpus) # turn remaining words into matrix to perform counts
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v) # create list of words ordered by frequency
  
  cat(paste0("Done summary...\n"))
  
  # The code below goes through the text and categorises words and phrases by the types of word
  ## Commented below to save the output of udpipe analysis for speed of loading
  
  #ud_model <- udpipe_load_model("spanish-gsd-ud-2.3-181115.udpipe")
  #bookWordAnalysis <- udpipe_annotate(ud_model, x = bookText, trace = TRUE)
  #bookWordAnalysis <- as.data.frame(bookWordAnalysis)
  #saveRDS(bookWordAnalysis, "data/sherlockWordAnalysis_scarlet.rds")
  bookWordAnalysis <- readRDS("data/sherlockWordAnalysis_scarlet.rds")
  
  # Cleaning of output from udpipe to get stemmed words
  stemmedWords <- bookWordAnalysis %>% select(token, lemma, upos, feats)
  stemmedWords <- stemmedWords %>%
    mutate(token = gsub(pattern = '[[:punct:] ]+(_)',replacement = '', token),
           lemma = gsub(pattern = '[[:punct:] ]+(_)',replacement = '', lemma)
           ) %>%
    mutate(token = gsub('[[:digit:]]+', '', token) %>% tolower(),
           lemma = gsub('[[:digit:]]+', '', lemma) %>% tolower()
           ) %>%
    filter(lemma != '') %>% unique()
  
  d_stemmed <- d %>%
    inner_join(stemmedWords,
               by = c("word"="token")) %>%
    unique()
  
  pronouns <- d_stemmed %>% #Get list of the prounouns in the text
    filter(upos == 'PROPN') %>%
    select(lemma) %>% pull()
  
  
  phrases <- bookWordAnalysis %>% #Get reference of phrases and related words
    select(lemma, sentence) %>% unique()
  
  cat(paste0("Got pronouns and phrase...\n"))
  
  
  ## Filtering for words based on type:
   ## Verb
  if (word_type == "VERB") {
    
    bookAnalysisExtract <- d_stemmed %>% 
      filter(upos %in% word_type & grepl("VerbForm=Inf",feats)) %>% #Remove all the pronouns from our current list
      select(word,lemma) %>% unique() %>%
      filter(!word %in% pronouns)
    
   ## Adjective
  } else if (word_type == "ADJ") {
    
    bookAnalysisExtract <- d_stemmed %>% 
      filter(upos %in% word_type & grepl("Number=Sing",feats)) %>% #Remove all the pronouns from our current list
      select(word,lemma) %>% unique() %>%
      filter(!word %in% pronouns)

    ## Adverb    
  } else if (word_type == "ADV") {
    
    bookAnalysisExtract <- d_stemmed %>% 
      filter(upos %in% word_type) %>% #Remove all the pronouns from our current list
      select(word,lemma) %>% unique() %>%
      filter(!word %in% pronouns)
  
    ## Noun  
  } else if (word_type == "NOUN") {
    
    bookAnalysisExtract <- d_stemmed %>% 
      filter(upos %in% word_type & grepl("Number=Sing",feats)) %>% #Remove all the pronouns from our current list
      select(word,lemma) %>% unique() %>%
      filter(!word %in% pronouns)
    
  }
  

  ## There are still quite a few common words which we're familiar with
  # We can do some research to get rid of these
  # The link below reads from the top 1000 used words in from the Spanish Royal Academy (RAE)
  
  topWords <- read.table("http://corpus.rae.es/frec/1000_formas.TXT",
                         encoding = "latin1",
                         header = T)
  
  ## Filtering the list of words from the book with the top 1000 spanish words
  bookAnalysisExtract <- bookAnalysisExtract %>%
    filter(!word %in% topWords$Orden)
  
  
  ## Filtering more words with the ones learnt from Duolingo
  cat(paste0("Getting my vocab...\n"))
  
  ## Duolingo url, use your own account ID
  ## the duolingo_creds file contains two lines with the duolingo account ID and the Duolingo API bearer token
  duolingo_creds <- readLines("src/tools/duolingo_creds.txt")
  url = paste0("https://www.duolingo.com/vocabulary/overview?_=",duolingo_creds[1])
  bearerToken = duolingo_creds[2]

  # Call API to get vocab list
  token <- paste("Bearer", bearerToken)
  req <- httr::GET(url, httr::add_headers("Authorization" = token))
  
  duolingoWordJson <- httr::content(req)
  myDuolingoVocabInfo <- duolingoWordJson$vocab_overview
  
  myDuolingoVocabWords <- do.call(c,
                          lapply(1:length(myDuolingoVocabInfo),
                                 function(i) {
                                   
                                   tmp <- myDuolingoVocabInfo[i][[1]]$word_string
                                   
                                   return(tmp)
                                   
                                 }))
  
  ## The following commented part is the parsing of the udpipe model for the Duolingo words
  
  # myDuolingoWords <- udpipe_annotate(ud_model, x = myDuolingoVocabWords, trace = TRUE) %>%
  #   as.data.frame() %>%
  #   filter(upos %in% c("VERB")) %>% #Remove all the pronouns from our current list
  #   select(token,lemma) %>% unique()
  
  #saveRDS(myDuolingoWords, "data/myDuolingoWords.rds")
  myDuolingoWords <- readRDS("data/myDuolingoWords.rds")
  
  ## Remove Duolingo words and limit to words which appear more than twice
  wordsToLearn <- bookAnalysisExtract[!(bookAnalysisExtract$lemma %in% myDuolingoWords$lemma),] %>%
    inner_join(d, by = c("word"="word")) %>%
    filter(freq > 2) %>%
    select(lemma, freq) %>% unique()
  
  cat(paste0("Generating wordcloud...\n"))
  
  ## Generate wordcloud
  printWordCloud <- wordcloud2(wordsToLearn, 
            color=brewer.pal(8, "Dark2"))
  
  return(list(printWordCloud,
              phrases))
  
}
