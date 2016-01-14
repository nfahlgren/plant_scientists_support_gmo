library(ggplot2)
library(tm)
library(tm.plugin.webmining)
library(wordcloud)
library(SnowballC)
library(grid)
library(parallel)

## Title word frequencies ##
titles = read.table(file='petitioner_publication_analysis_results.txt.titles.txt', sep='\t',
                    header=TRUE, encoding='UTF-8', stringsAsFactors = FALSE,
                    quote = "\"")

# Run a series of text-mining steps on the input titles
# The stem completion step can be run in parallel
title_tm = function(docs, cpu = 1, minWordLen = 4) {
  docs.tm = Corpus(VectorSource(docs))
  docs.tm = tm_map(docs.tm, content_transformer(tolower))
  docs.tm = tm_map(docs.tm, removeNumbers)
  docs.tm = tm_map(docs.tm, removeWords, stopwords("english"))
  docs.tm = tm_map(docs.tm, removePunctuation)
  docs.tm = tm_map(docs.tm, stripWhitespace)
  docs.tm = tm_map(docs.tm, PlainTextDocument)
  
  ref.docs = docs.tm
  
  docs.tm = tm_map(docs.tm, stemDocument)
  
  stemCompletion_mod <- function(x,dict) {
    PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=dict, type="shortest"),sep="", collapse=" ")))
  }
  
  c1 = makeForkCluster(cpu)
  docs.tm = parSapply(c1, docs.tm, stemCompletion_mod, dict=ref.docs)
  stopCluster(c1)
  
  return(docs.tm)
}

# Convert a corpus to a data frame of words and word frequencies
tm2dm = function(tmdocs, minWordLen=4) {
  tmdocs = Corpus(VectorSource(tmdocs))
  tdm = TermDocumentMatrix(tmdocs, control = list(wordLengths=c(minWordLen, Inf)))
  tdm.mat = as.matrix(tdm)
  wfq = sort(rowSums(tdm.mat), decreasing = TRUE)
  dm = data.frame(word=names(wfq), freq=wfq, stringsAsFactors=FALSE)
  
  # For some reason the first 9 rows are junk that gets added during the text-mining process, so remove them
  #dm = dm[10:nrow(dm),]
  
  return(dm)
}

tm.ss = title_tm(titles, cpu = 4)
dm.ss = tm2dm(tm.ss)

removed = c(
  'scientificsupport', 'arabidopsis', 'maize', 'thaliana', 'tomato', 'rice', 'wheat', 'tobacco', 'populus', 'chlamydomonas',
  'phytophthora', 'barley', 'potato', 'pseudomonas', 'salina', 'soybean', 'human', 'yeast', 'agrobacterium', 'cucumber',
  'reinhardtii', 'drosophila', 'brassica', 'nicotiana', 'sativa', 'oryza', 'coli', 'infestans', 'elongata', 'polymorpha',
  'escherichia', 'solanum', 'tumefaciens', 'rhizobium', 'indica', 'poplar', 'turnip', 'cauliflower', 'sativum', 'xanthomonas',
  'pisum', 'domestica', 'physcomitrella', 'corn', 'brachypodium', 'lepidoptera', 'mouse', 'potyvirus', 'patens', 'candida',
  'bacillus', 'cotton', 'mice', 'saccharomyces', 'sorghum', 'sugarcane', 'thlaspi', 'cyanobacteria', 'poaceae', 'radiata',
  'carrot', 'distachyon', 'hedgehog', 'sinorhizobium', 'fusarium', 'neurospora', 'grape', 'sojae', 'strawberries', 'thuringiensis',
  'vitis', 'petunia', 'caerulescens', 'cerevisiae', 'onion', 'haberlea', 'sunflower', 'cabbage', 'napus', 'vicia', 'crassa',
  'cucumis', 'reinhardi', 'rhodopensis', 'alfalfa', 'aspen', 'cladosporium', 'medicago', 'panicum', 'vaccinia', 'chenopodiaceae',
  'cassava', 'fulvum', 'spinach', 'triticum', 'benthamiana', 'plutellidae', 'banana', 'copi', 'switchgrass', 'elegans', 'setaria'
)

dm.filtered = dm.ss[!dm.ss$word %in% removed,]

# Manually fix a few incorrectly stem-completed words
dm.filtered[dm.filtered$word == 'respons',]$word = 'response'
dm.filtered[dm.filtered$word == 'regulon',]$word = 'regulate'

c.scale = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

png(filename = 'sci.support.wordcloud.png', width = 2500, height = 2500)
wordcloud(dm.filtered$word, dm.filtered$freq, random.order=FALSE, 
          colors=c.scale, scale = c(20,1))
dev.off()

