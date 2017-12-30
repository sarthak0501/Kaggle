
user_data <- read.csv("gender-classifier-DFE-791531.csv", stringsAsFactors = F)
user_data <- user_data[1:20000,]

user_data <- na.omit(user_data)
user_data <- unique(user_data)

cleaning <- function(s){
  s <- tolower(s)
  s = gsub("\\W",' ', s)
  s = trimws(s)
  s = gsub("\\s+", " ", s)
  s = gsub("/", " ", s)
  s = gsub("@", " ", s)
  s = gsub("\\|", " ", s)
  return(s)
}

user_data$description <- cleaning(user_data$description)
user_data$text <- cleaning(user_data$text)

## taking only unit_id, gender, description and text and considering only male and female 

user_data = user_data[user_data$gender %in% c('male', 'female'),]
user_data = user_data[,c(1, 6, 11, 20)]
head(user_data)

## Variable transformation

user_data$gender = ifelse(user_data$gender == 'male', 1, 0)

trn = sample(nrow(user_data), nrow(user_data)*0.8)
train = user_data[trn,]
test = user_data[-trn,]

### Creating a common column for all the text
### description + text

train$all_text = paste(train$description, train$text)
head(train)

library(tm)
library(RColorBrewer)
library(wordcloud)

WordCloud <- function(d){
  d = Corpus(VectorSource(d))
  d = tm_map(d, removePunctuation)
  d = tm_map(d, removeNumbers)
  d <- tm_map(d, removeWords,c('http', 'https',stopwords('english')))
  wordcloud(d, max.words = 20, random.order = FALSE)
}

WordFreq <- function(d){
  d = Corpus(VectorSource(d))
  d = tm_map(d, removePunctuation)
  d = tm_map(d, removeNumbers)
  d <- tm_map(d, removeWords,c('https','http',stopwords('english')))
  tdm <- TermDocumentMatrix(d)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  return(d)
}


male_train <- train[train$gender == 1,]
female_train <- train[train$gender == 0,]

male_words = WordFreq(male_train$all_text)
female_words = WordFreq(female_train$all_text)

print("Male words")
head(male_words)
print("Female words")
head(female_words)

print("Male words")
WordCloud(male_train$all_text)

print("Female words")
WordCloud(female_train$all_text)

all_words = merge(x = male_words, y = female_words, by = "word", all = TRUE)

colnames(all_words) <- c("word", "freq_m", "freq_f")

all_words[is.na(all_words)] <- 0

all_words$sum = all_words$freq_m + all_words$freq_f

all_words$male_prob = all_words$freq_m/all_words$sum

## Removing words which are only in male words or only in female words

all_words = all_words[!(all_words$male_prob %in% c(0,1)),]

all_words = all_words[order(-all_words$male_prob),]

head(all_words)

nrow(all_words)

d = Corpus(VectorSource(train$all_text))
d = tm_map(d, removePunctuation)
d = tm_map(d, removeNumbers)
d <- tm_map(d, removeWords,c('https','http',stopwords('english')))
tdm <- TermDocumentMatrix(d)

library(tidytext)
DF <- tidy(tdm)

max(as.numeric(DF$document))

head(DF)

sapply(DF, function(e){length(unique(e))})

nrow(DF)

DF <- DF[DF$term %in% all_words$word,]

merged_set <- merge(x = DF, y = all_words, by.x = "term", by.y = "word")
merged_set <- merged_set[order(-merged_set$document),]

sapply(merged_set, class)

merged_set$document <- as.numeric(merged_set$document)

merged_set = merged_set[order(order(merged_set$document)),]

head(merged_set)

max(merged_set$document)

aggr = aggregate(cbind(freq_m, sum) ~ document, data = merged_set, sum)

head(aggr)

aggr$male_prob = aggr$freq_m / aggr$sum

max(aggr$document)

train$ID = 1:nrow(train)

train_op= merge(train, aggr, by.x = "ID", by.y = "document", all.x = T)

train_op$gender_predicted = ifelse(train_op$male_prob >= 0.5, 1, 0)

head(train_op, 3)

mean(train_op$gender == train_op$gender_predicted, na.rm = T)

sum(is.na(train_op$gender_predicted))

test$all_text = paste(test$description, test$text)

d = Corpus(VectorSource(test$all_text))
d = tm_map(d, removePunctuation)
d = tm_map(d, removeNumbers)
d <- tm_map(d, removeWords,c('https','http',stopwords('english')))
tdm <- TermDocumentMatrix(d)

DF <- tidy(tdm)

DF <- DF[DF$term %in% all_words$word,]

merged_set <- merge(x = DF, y = all_words, by.x = "term", by.y = "word")

merged_set$document <- as.numeric(merged_set$document)

merged_set <- merged_set[order(-merged_set$document),]

merged_set = merged_set[order(order(merged_set$document)),]

aggr = aggregate(cbind(freq_m, sum) ~ document, data = merged_set, sum)

aggr$male_prob = aggr$freq_m / aggr$sum

test$ID = 1:nrow(test)

test_op= merge(test, aggr, by.x = "ID", by.y = "document", all.x = T)

test_op$gender_predicted = ifelse(test_op$male_prob >= 0.5, 1, 0)

mean(test_op$gender == test_op$gender_predicted, na.rm = T)

sum(is.na(test_op$gender_predicted))
