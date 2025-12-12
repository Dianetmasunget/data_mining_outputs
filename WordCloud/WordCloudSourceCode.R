# Set working directory
setwd("C:/rprogramming")

# Load required libraries
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# Read feedback.txt
text <- readLines("feedback.txt", encoding = "UTF-8", warn = FALSE)

# Create VCorpus
corpus <- VCorpus(VectorSource(text))

# Cleaning the text
corpus <- tm_map(corpus, content_transformer(tolower))                         # lowercase
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^a-z ]", " ", x))) # remove punctuation/special chars
corpus <- tm_map(corpus, removeNumbers)                                        # remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("english"))                    # remove English stopwords
corpus <- tm_map(corpus, stripWhitespace)                                      # strip extra whitespace
corpus <- tm_map(corpus, stemDocument)                                         # stemming

tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)

# Calculate word frequencies
word_freqs <- sort(rowSums(m), decreasing = TRUE)
df <- data.frame(word = names(word_freqs), freq = word_freqs)

# Display top 10 words
top10 <- head(df, 10)
print(top10)

# The top words represent common topics in customer feedback.
# Frequent words show recurring themes such as service, staff, office, and food.
# This indicates customers mostly comment on staff behavior, waiting times, and overall experience.
# Restaurant feedback focuses on food quality and ambiance.
# Government office feedback highlights process efficiency and document handling.

png("wordcloud_exam.png", width = 800, height = 600)  # save as PNG

set.seed(1234)
wordcloud(words = df$word,
          freq = df$freq,
          min.freq = 2,
          max.words = 1000,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

dev.off()  # close PNG device

rare_df <- subset(df, freq == 1)  # select words with frequency 1
# If fewer than 5 rare words exist, take least frequent 5 words
if(nrow(rare_df) < 5){
  rare_df <- tail(df, 5)
}

png("wordcloud_rare.png", width = 800, height = 600)

# Select 5 least frequent words
least5 <- tail(df[order(df$freq), ], 5)
print(rare_df)

set.seed(2025)
wordcloud(words = rare_df$word,
          freq = rare_df$freq,
          min.freq = 1,
          max.words = 500,
          random.order = FALSE,
          rot.per = 0,
          scale = c(3, 0.7),
          colors = brewer.pal(8, "Dark2"))

dev.off()  # close PNG device

cat("\nDONE! Images generated:\n")
cat("- wordcloud_exam.png\n")
cat("- wordcloud_rare.png\n")

