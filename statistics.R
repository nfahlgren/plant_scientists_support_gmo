library(ggplot2)
library(tm)
library(tm.plugin.webmining)
library(wordcloud)
library(SnowballC)
library(shiny)
library(grid)
library(parallel)

## Citation statistics ##
stats = read.table(file='network.edges2.txt.stats.txt', sep='\t', header=TRUE)

stats$active_years = stats$last_year - stats$start_year + 1

stats$citations_per_year = stats$citations / stats$active_years

stats$group = as.character(stats$group)
stats[grep('scientific_support', stats$group),]$group = 'Scientific support'
stats[grep('no_consensus', stats$group),]$group = 'No consensus'
stats$group = factor(stats$group)

stats.sorted = stats[order(stats$citations_per_year),]

# Total citations
plot.citations = ggplot(stats, aes(x = citations, group = group, fill = group)) + 
  geom_density(alpha = 0.5) + theme_bw() + scale_x_log10('log(citations, base = 10)') +
  theme(legend.position = 'bottom')

# First publication year
plot.start = ggplot(stats, aes(x = start_year, group = group, fill = group)) +
  geom_density(alpha = 0.5) + theme_bw() + scale_x_continuous('First publication year') +
  theme(legend.position = 'bottom')

# Active publication years
plot.career = ggplot(stats, aes(x = active_years, group = group, fill = group)) +
  geom_density(alpha = 0.5) + theme_bw() + scale_x_continuous('Active publication years') +
  theme(legend.position = 'bottom')

plot.cpy = ggplot(stats,
                  aes(x = citations_per_year, group = group, fill = group)) +
  geom_density(alpha = 0.5) + theme_bw() + 
  scale_x_log10('log(citations / year, base = 10)') +
  theme(legend.position = 'bottom')

# Multiple plot function modified from the Cookbook for R by Winston Chang: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols),byrow=TRUE)
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(plot.citations, plot.cpy, plot.start, plot.career, cols = 2)
################################################################################

## Title word frequencies ##
titles = read.table(file='network.edges2.txt.titles.txt', sep='\t',
                    header=TRUE, encoding='UTF-8', stringsAsFactors = FALSE,
                    quote = "\"")

no.consensus = titles[titles$group == './data/no_consensus',]$title
sci.support = titles[titles$group == './data/scientific_support',]$title

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

tm.nc = title_tm(no.consensus, cpu = 1)
tm.ss = title_tm(sci.support, cpu = 1)

tm2dm = function(tmdocs, minWordLen=4) {
  tmdocs = Corpus(VectorSource(tmdocs))
  tdm = TermDocumentMatrix(tmdocs, control = list(wordLengths=c(minWordLen, Inf)))
  tdm.mat = as.matrix(tdm)
  wfq = sort(rowSums(tdm.mat), decreasing = TRUE)
  dm = data.frame(word=names(wfq), freq=wfq, stringsAsFactors=FALSE)
  
  # For some reason the first 9 rows are junk that gets added during the text-mining process, so remove them
  dm = dm[10:nrow(dm),]
  
  return(dm)
}

dm.nc = tm2dm(tm.nc)
dm.ss = tm2dm(tm.ss)



c.scale = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# ggplot(data = dm.nc, aes(x=c(1:length(dm.nc$freq)), y=log10(freq))) +
#   geom_point()
# 
# ggplot(data = dm.ss, aes(x=c(1:length(dm.ss$freq)), y=log10(freq))) +
#   geom_point()

png(filename = 'no.consensus.wordcloud.png', width = 2500, height = 2500)
wordcloud(dm.nc$word, dm.nc$freq, random.order=FALSE, 
          colors=c.scale, scale = c(20,1))
dev.off()

png(filename = 'sci.support.wordcloud.png', width = 2500, height = 2500)
wordcloud(dm.ss$word, dm.ss$freq, random.order=FALSE, 
          colors=c.scale, scale = c(20,1))
dev.off()

# no.consensus.p = no.consensus.dm
# no.consensus.p$freq = no.consensus.dm$freq / sum(no.consensus.dm$freq)
# 
# sci.support.p = sci.support.dm
# sci.support.p$freq = sci.support.dm$freq / sum(sci.support.dm$freq)
# 
# f.table = merge(x = no.consensus.p, y = sci.support.p, by = 'word', all = TRUE)
# colnames(f.table) = c('word', 'f.nc', 'f.ss')
# f.table[is.na(f.table$f.nc),]$f.nc = 0
# f.table[is.na(f.table$f.ss),]$f.ss = 0
# f.table$ave = (f.table$f.nc + f.table$f.ss) / 2
# 
# ggplot(f.table, aes(x = log(ave, 2), y = log(f.nc / f.ss, 2))) + geom_point()

f.table = merge(x = dm.nc, y = dm.ss, by = 'word', all = TRUE)
colnames(f.table) = c('word', 'f.nc', 'f.ss')
f.table[is.na(f.table$f.nc),]$f.nc = 0
f.table[is.na(f.table$f.ss),]$f.ss = 0
f.table$ave = (f.table$f.nc + f.table$f.ss) / 2
f.table$sum = f.table$f.nc + f.table$f.ss

f.filter = f.table[f.table$sum > 3,]

ggplot(f.filter, aes(x = log(ave, 2), y = log(f.nc / f.ss, 2))) +
  geom_text(aes(label = word), size=2) + theme_bw() +
  scale_x_continuous(limits=c(0,12), "Average count (log2)") +
  scale_y_continuous(limits=c(-10,10), "Group ratio (log2)")
################################################################################

ui <- fluidPage(
  fluidRow(
    column(width = 12,
           plotOutput("plot1", height = 600, width = 600, hover = hoverOpts(id ="plot_hover"))
    )
  ),
  fluidRow(
    column(width = 5,
           verbatimTextOutput("hover_info")
    )
  )
)

server <- function(input, output) {
  
  
  output$plot1 <- renderPlot({
    
    ggplot(f.filter, aes(x = log(ave, 2), y = log(f.nc / f.ss, 2))) + geom_point() +
      theme_bw() +
      scale_x_continuous(limits=c(0,12)) + scale_y_continuous(limits=c(-10,10))
    #ggplot(mtcars, aes(x=mpg,y=disp,color=factor(cyl))) + geom_point()
    
  })
  
  output$hover_info <- renderPrint({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      dist=sqrt((hover$x-log(f.filter$ave, 2))^2+(hover$y-log(f.filter$f.nc / f.filter$f.ss, 2))^2)
      #cat("Weight (lb/1000)\n")
      if(min(dist) < 3)
        f.filter$word[which.min(dist)]
    }
    
    
  })
}
shinyApp(ui, server)
