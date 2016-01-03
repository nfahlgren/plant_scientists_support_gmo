library(ggplot2)
library(tm)
library(tm.plugin.webmining)
library(wordcloud)
library(SnowballC)
library(shiny)

## Citation statistics ##
stats = read.table(file='network.edges2.txt.stats.txt', sep='\t', header=TRUE)

stats$active_years = stats$last_year - stats$start_year + 1

stats$citations_per_year = stats$citations / stats$active_years

stats.sorted = stats[order(stats$citations_per_year),]

ggplot(stats,
       aes(x = log(citations_per_year), group = group, fill = group)) +
  geom_density(alpha = 0.2) + theme_bw()
################################################################################

## Title word frequencies ##
titles = read.table(file='network.edges2.txt.titles.txt', sep='\t',
                    header=TRUE, encoding='UTF-8', stringsAsFactors = FALSE,
                    quote = "\"")

no.consensus = titles[titles$group == './data/no_consensus',]$title
sci.support = titles[titles$group == './data/scientific_support',]$title

tm.nc = Corpus(VectorSource(no.consensus))
tm.ss = Corpus(VectorSource(sci.support))

tm.nc = tm_map(tm.nc, content_transformer(tolower))
tm.ss = tm_map(tm.ss, content_transformer(tolower))

tm.nc = tm_map(tm.nc, removeNumbers)
tm.ss = tm_map(tm.ss, removeNumbers)

tm.nc = tm_map(tm.nc, removePunctuation)
tm.ss = tm_map(tm.ss, removePunctuation)

tm.nc = tm_map(tm.nc, removeWords, stopwords("english"))
tm.ss = tm_map(tm.ss, removeWords, stopwords("english"))

tm.nc = tm_map(tm.nc, stripWhitespace)
tm.ss = tm_map(tm.ss, stripWhitespace)

tm.nc = tm_map(tm.nc, stemDocument)
tm.ss = tm_map(tm.ss, stemDocument)

tdm.nc = TermDocumentMatrix(tm.nc, control = list(wordLengths=c(5, Inf)))
tdm.ss = TermDocumentMatrix(tm.ss, control = list(wordLengths=c(5, Inf)))

mat.nc = as.matrix(tdm.nc)
mat.ss = as.matrix(tdm.ss)

wfq.nc = sort(rowSums(mat.nc), decreasing = TRUE)
wfq.ss = sort(rowSums(mat.ss), decreasing = TRUE)

dm.nc = data.frame(word=names(wfq.nc), freq=wfq.nc, stringsAsFactors=FALSE)
dm.ss = data.frame(word=names(wfq.ss), freq=wfq.ss, stringsAsFactors=FALSE)

c.scale = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# ggplot(data = dm.nc, aes(x=c(1:length(dm.nc$freq)), y=log10(freq))) +
#   geom_point()
# 
# ggplot(data = dm.ss, aes(x=c(1:length(dm.ss$freq)), y=log10(freq))) +
#   geom_point()

png(filename = 'no.consensus.wordcloud.png', width = 2500, height = 2500)
wordcloud(no.consensus.dm$word, no.consensus.dm$freq, random.order=FALSE, 
          colors=c.scale, scale = c(20,1))
dev.off()

png(filename = 'sci.support.wordcloud.png', width = 2500, height = 2500)
wordcloud(sci.support.dm$word, sci.support.dm$freq, random.order=FALSE, 
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
