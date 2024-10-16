# Set the working directory
# setwd("~/path/to/directory")

# Turn off scientific notation
options(scipen=999)

# Load packages
library(ggplot2)
library(tidytext)
library(tidyr)
library(lubridate)
library(textclean)
library(stopwords)
library(dplyr)
library(textdata)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(readr)
library(syuzhet)
library(rtweet)
library(magrittr)
library(gridExtra)
library(sf)
library(countrycode)
library(scales)
library(rnaturalearth)
library(rnaturalearthdata)

# Upload CSV files 
tweets <- read_csv("~/path/to/csv/file")

# Convert the 't' column to character format and assign to a new column in the same dataframe
tweets$text <- as.character(tweets$t)

str(tweets)

# DESCRIPTIVE ANALYSIS

# Examine the structure of the data
str(tweets)

# Examine the summary statistics of the data
summary(tweets)

# Calculate the number of tweets and unique users
tweets %>%
  summarize(
    n_tweets = n(),
    n_unique_users = n_distinct(sn)
  )

# Calculate the mean, median, and standard deviation of tweet lengths
tweets %>%
  mutate(tweet_length = stringr::str_count(t)) %>%
  filter(!is.na(tweet_length)) %>%
  summarize(
    mean_tweet_length = mean(tweet_length, na.rm = TRUE),
    median_tweet_length = median(tweet_length, na.rm = TRUE),
    sd_tweet_length = sd(tweet_length, na.rm = TRUE)
  )

# Create a scatter plot of tweet length versus number of followers
tweets %>%
  mutate(tln = str_count(text)) %>%
  ggplot(aes(x = tln, y = flrs)) +
  geom_point(alpha = 0.5) +
  labs(x = "Tweet length", y = "Number of likes", title = "Tweet length versus number of followers")

# Create a histogram of the distribution of tweet lengths
tweets %>%
  mutate(tweet_length = str_count(text)) %>%
  ggplot(aes(x = tweet_length)) +
  geom_histogram(fill = "steelblue", binwidth = 10) +
  labs(x = "Tweet length", y = "Count", title = "Distribution of tweet lengths")

# Summary statistics of 'flrs' (followers)
summary(tweets$flrs)

# Plot distribution of followers
hist(tweets$flrs, breaks = 50, main = "Distribution of Followers", xlab = "Number of Followers")

# SPATIOTEMPORAL ANALYSIS

# Filter out rows with missing country codes
filtered_tweets <- tweets[complete.cases(tweets$cc), ]

# Convert country codes to country names
filtered_tweets$country <- countrycode::countrycode(
  sourcevar = filtered_tweets$cc,
  origin = "iso2c",
  destination = "country.name"
)

# Group data by country, count the number of tweets, and calculate average lat/lng
tweet_counts <- filtered_tweets %>%
  group_by(country) %>%
  summarise(
    total_tweets = n(),
    avg_lat = mean(tlt, na.rm = TRUE),
    avg_lng = mean(tln, na.rm = TRUE)
  ) %>%
  arrange(desc(total_tweets))

# Filter out rows with missing latitude or longitude
tweet_counts_clean <- tweet_counts %>%
  filter(!is.na(avg_lat) & !is.na(avg_lng))

# Get the world map from Natural Earth
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create an sf object from the cleaned tweet counts data
tweet_counts_sf <- st_as_sf(tweet_counts_clean, coords = c("avg_lng", "avg_lat"), 
                            crs = 4326, agr = "constant")

# Create the plot using ggplot2
tweet_world_map <- ggplot(data = world) +
  geom_sf(fill = "gray90", color = "gray50") +  # Use the Natural Earth base map
  geom_sf(data = tweet_counts_sf, aes(size = total_tweets), color = "blue", alpha = 0.7) +  # Add the tweet dots
  scale_size_continuous(labels = scales::comma, range = c(2, 12)) +  # Adjust the size of dots
  labs(
    title = "",
    subtitle = "Each dot represents the number of posts from a country",
    size = "Total posts",
    caption = "Data: Natural Earth (public domain); post counts data provided by Boston Children's Hospital"
  ) +  # Include data source in the caption
  theme_void() +  # Remove axes and grid lines for a clean map
  theme(
    plot.background = element_rect(fill = "aliceblue"),  # Set background color
    legend.position = "bottom",
    legend.title = element_text(size = 14, face = "bold"),  # Make legend title bold
    legend.text = element_text(size = 12),  # Increase size of legend text
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 10))  # Center subtitle
  )

# Save the map as a high-resolution JPEG file
ggsave("tweet_world_map_high_quality.jpeg", plot = tweet_world_map, 
       dpi = 300, width = 10, height = 6, units = "in")

# PLOT CASES AND TWEETS

# Create a data frame with your data
year <- 2012:2022
cases <- c(4490, 4680, 7774, 4535, 7102, 8819, 16911, 22986, 10137, 8638, 5856)
posts <- c(4, 76, 108, 181, 95, 91, 197, 336, 350, 858, 153)
data <- data.frame(year, cases, posts)

# Calculate the maximum scales for cases and posts
max_cases <- max(cases)
max_posts <- max(posts)

# Calculate the ratio to adjust the secondary axis
ratio <- max_cases / max_posts

# Plot
ggplot(data) + 
  geom_bar(aes(x = year, y = posts), stat = "identity", fill = "grey", color = "black", size = 0.2) +
  geom_line(aes(x = year, y = cases / ratio), colour = "darkblue", size = 1.5) +
  geom_point(aes(x = year, y = cases / ratio), colour = "darkblue", size = 3) +
  scale_x_continuous(breaks = 2012:2022, minor_breaks = NULL) +
  scale_y_continuous(
    name = "Number of posts (grey bars)", 
    labels = label_comma(),
    sec.axis = sec_axis(~ . * ratio, name = "Number of cases (blue line)", labels = label_comma())
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(margin = margin(t = 20), size = 14), # Increase size for x axis title
    axis.title.y = element_text(margin = margin(r = 20), size = 14), # Increase size for y axis title
    axis.title.y.right = element_text(margin = margin(l = 20), size = 14), # Increase size for secondary y axis title
    axis.text.x = element_text(size = 12), # Increase size for x axis tick labels
    axis.text.y = element_text(size = 12), # Increase size for y axis tick labels
    plot.title = element_text(size = 16) # Increase size for plot title
  ) +
  labs(title = "Posts vs. Cases per Year", x = "Year", y = "Number of Posts (Grey Bars)")

# SPEARMAN'S CORRELATION TEST

cor_test_result <- cor.test(cases, posts, method = "spearman")

# Print the test result
print(cor_test_result)

# DATA PREPROCESSING

# Build a corpus and specify the source to be character vectors
tweets.corpus.raw <- tweets$text %>% VectorSource() %>% Corpus()
tweets.corpus.raw <- Corpus(VectorSource(tweets$text)) 

# Convert to lower case 
tweets.corpus.raw <- tm_map(tweets.corpus.raw, content_transformer(stringi::stri_trans_tolower))

# Remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)  
tweets.corpus.raw <- tm_map(tweets.corpus.raw, content_transformer(removeURL))

# Remove the @ (usernames)
removeUsername <- function(x) gsub("@[^[:space:]]*", "", x)  
tweets.corpus.raw <- tm_map(tweets.corpus.raw, content_transformer(removeUsername))

# Remove words between < >
removeUsername <- function(x) gsub("<[^[:space:]]*", "", x)  
tweets.corpus.raw <- tm_map(tweets.corpus.raw, content_transformer(removeUsername))

# Remove anything except the English language and space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
tweets.corpus.raw <- tm_map(tweets.corpus.raw, content_transformer(removeNumPunct))

# Remove stopwords and keywords used for the search of tweets
myStopWords<- c((stopwords('english')),c("diphtheria", "diphterie", "diphtérie", "difteri", "like", "got", "people", "rt", "en", "use", "used", "via", "amp", "retweet", 'those', 'on', 'own', 'yourselves', 'ie', 'around', 'between', 'four', 'been', 'alone', 'off', 'am', 'then', 'other', 'can', 'cry', 'hereafter', 'front', 'dont', 'just', 'pas', 'too', 'wherein', 'everything', 'up', 'onto', 'never', 'either', 'how', 'before', 'anyway', 'since', 'through', 'amount', 'now', 'he', 'cant', 'was', 'con', 'have', 'into', 'because', 'inc', 'not', 'therefore', 'they', 'even', 'whom', 'it', 'see', 'somewhere', 'interest', 'thereupon', 'nothing', 'thick', 'whereas', 'much', 'whenever', 'find', 'seem', 'until', 'whereby', 'at', 'ltd', 'fire', 'also', 'some', 'last', 'than', 'get', 'already', 'our', 'once', 'will', 'noone', 'that', 'what', 'thus', 'no', 'myself', 'out', 'next', 'whatever', 'although', 'though', 'etc', 'which', 'would', 'therein', 'nor', 'somehow', 'whereupon', 'besides', 'whoever', 'thin', 'ourselves', 'few', 'third', 'without', 'anything', 'twelve', 'against', 'while', 'twenty', 'if', 'however', 'found', 'herself', 'when', 'may', 'ours', 'six', 'done', 'seems', 'else', 'call', 'perhaps', 'had', 'nevertheless', 'fill', 'where', 'otherwise', 'still', 'within', 'its', 'for', 'together', 'elsewhere', 'throughout', 'of', 'eg', 'others', 'show', 'sincere', 'anywhere', 'anyhow', 'as', 'are', 'the', 'hence', 'something', 'hereby', 'nowhere', 'de', 'latterly', 'neither', 'his', 'go', 'forty', 'put', 'their', 'by', 'namely', 'could', 'five', 'itself', 'is', 'nine', 'whereafter', 'down', 'bottom', 'thereby', 'such', 'both', 'she', 'become', 'whole', 'who', 'yourself', 'every', 'thru', 'except', 'very', 'several', 'among', 'being', 'be', 'mine', 'further', 'here', 'during', 'why', 'with', 'becomes', 'about', 'a', 'co', 'seeming', 'due', 'wherever', 'beforehand', 'detail', 'fifty', 'becoming', 'might', 'amongst', 'my', 'empty', 'thence', 'thereafter', 'almost', 'least', 'someone', 'often', 'from', 'keep', 'him', 'or', 'top', 'her', 'nobody', 'sometime', 'across', 'hundred', 'only', 'via', 'name', 'eight', 'three', 'back', 'to', 'all', 'became', 'move', 'me', 'we', 'formerly', 'so', 'i', 'whence', 'describe', 'under', 'always', 'himself', 'in', 'herein', 'more', 'after', 'themselves', 'you', 'above', 'sixty', 'them', 'hasnt', 'your', 'made', 'indeed', 'most', 'everywhere', 'fifteen', 'but', 'must', 'along', 'beside', 'hers', 'side', 'former', 'anyone', 'full', 'has', 'yours', 'whose', 'behind', 'please', 'amoungst', 'mill', 'ten', 'seemed', 'sometimes', 'should', 'over', 'take', 'each', 'same', 'rather', 'latter', 'and', 'hereupon', 'part', 'per', 'eleven', 'ever', 'enough', 'again', 'us', 'yet', 'moreover', 'mostly', 'one', 'meanwhile', 'whither', 'there', 'toward', 'give', 'system', 'do', 'an', 'these', 'everyone', 'towards', 'this', 'bill', 'cannot', 'un', 'afterwards', 'beyond', 'were', 'whether', 'well', 'another', 'below', 'first', 'upon', 'any', 'none', 'many', 'serious', 're', 'two', 'couldnt', 'les', 'que', 'less', 'h', 'la', 'et' ))
tweets.corpus.raw <- tm_map(tweets.corpus.raw, removeWords, myStopWords)

# Remove single letter words
removeSingle <- function(x) gsub(" . ", " ", x)   
tweets.corpus.raw <- tm_map(tweets.corpus.raw, content_transformer(removeSingle))

# Remove extra whitespaces
tweets.corpus.raw <- tm_map(tweets.corpus.raw, stripWhitespace)

# Keep a copy of “tweets.corpus.raw” for stem completion later
tweets.corpus.cleaned <- tweets.corpus.raw

## Stem words
tweets.corpus.stemmed <- tweets.corpus.cleaned %>% tm_map(stemDocument)

## Correct / complete text after stemming
stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary) 
  x <- paste(x, sep="", collapse=" ") 
  stripWhitespace(x)
}

tweets.corpus.completed <- tweets.corpus.stemmed %>%
  lapply(stemCompletion2, dictionary=tweets.corpus.cleaned) %>%
  VectorSource() %>% Corpus()

# Compare text before/after cleaning
# Original text
tweets.corpus.raw[[1]]$content %>% strwrap(60) %>% writeLines()

# After basic cleaning
tweets.corpus.cleaned[[1]]$content %>% strwrap(60) %>% writeLines()

# Stemmed text
tweets.corpus.stemmed[[1]]$content %>% strwrap(60) %>% writeLines()

# Create a term document matrix (tdm)
tdm <- TermDocumentMatrix(tweets.corpus.completed, control= list(wordLengths= c(1, Inf)))
tdm

# TERM FREQUENCY ANALYSIS

# Find the terms used most frequently (lowfreq = x -> indicates the minimum number of times a word appears in the TDM)
freq.terms <- tdm %>% findFreqTerms(lowfreq = 100)
# Get the frequency count for each term
term.freq <- tdm %>% as.matrix() %>% rowSums()
# Create a data frame with terms and their corresponding frequency counts
df <- data.frame(term = names(term.freq), freq = term.freq)
# Sort the data frame by frequency count in descending order
df <- df %>% arrange(desc(freq))
# Print the resulting data frame with terms and their frequency counts
print(df)

# Arrange the terms by frequency and get the top 40
top_terms_df <- df %>%
  arrange(desc(freq)) %>%
  top_n(40, freq)

# Plot frequent words, now using the top_terms_df
theme_set(theme_bw())
plot1 <- ggplot(top_terms_df, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") +
  coord_flip() +
  theme(axis.text = element_text(size = 12))

# Print the plot
print(plot1)

# Save high-resolution version of the bar chart
ggsave("plot1_high_res.png", plot1, dpi = 300, width = 8, height = 6)

# Create a word cloud
m <- tdm %>% as.matrix
word.freq <- m %>% rowSums() %>% sort(decreasing = T) # calculate the frequency of words and sort it by frequency 
pal <- brewer.pal(9, "BuGn")[-(1:4)] # colors
plot2 <- wordcloud(words = names(word.freq), freq = word.freq, min.freq = 1,
                   random.order = F, colors = pal) # Plot word cloud

# Save word cloud as a high-resolution PNG file
png("wordcloud_high_res.png", width = 8, height = 6, units = 'in', res = 300)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 1,
          random.order = FALSE, colors = pal)
dev.off()  # Close the PNG device

# HIERARCHICAL CLUSTERING

# Remove sparse terms (i.e., absent in over 95% of the tweets.) from the tdm. Convert the resulting tdm into a matrix format 
m2 <- tdm %>% removeSparseTerms(sparse = 0.95) %>% as.matrix() 
# Calculate distance matrix to measure dissimilarity between each pair of terms in the matrix
dist.matrix <- m2 %>% scale() %>% dist()
# Perform hierarchical clustering on the distance matrix using the 'ward' method 
fit <- dist.matrix %>% hclust(method = "ward")
plot(fit) # Plot a dendrogram to represent the hierarchical clustering
groups <- fit %>% cutree(k = 3) # Assign each term to one of the 3 clusters.
table(groups) # Print the number of observations in each cluster