#######################################
# Article #
#######################################

## Load packages
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}
# Acquire stmBrowser package from github
if(!require(devtools)) install.packages("devtools")
library(devtools)
install_github("mroberts/stmBrowser",dependencies=TRUE)

lapply(c("tidyverse",
         "guardianapi", # for working with the Guardian's API
         "quanteda", # for QTA
         "quanteda.textstats", # more Quanteda!
         "quanteda.textplots", # even more Quanteda!
         "readtext", # for reading in text data
         "stringi", # for working with character strings
         "textstem", # an alternative method for lemmatizing
        "lubridate",
        "stm",
        "wordcloud",
        "stmBrowser",
        "LDAvis"
), pkgTest)



### A. Using the Guardian API with R
gu_api_key() # run this interactive function

####2023
# We want to query the API on articles
china23 <- gu_content(query = "China", from_date = "2023-01-01", to_date = "2024-01-01") # making a tibble

# We'll save this data
saveRDS(china23, "data/df23")

####2022
# We want to query the API on articles
china22 <- gu_content(query = "China", from_date = "2022-01-01", to_date = "2023-01-01") # making a tibble

# We'll save this data
saveRDS(china22, "data/df22")

####2021
# We want to query the API on articles
china21 <- gu_content(query = "China", from_date = "2021-01-01", to_date = "2022-01-01") # making a tibble

#画图查看观点和新闻的数量
# 创建数据框
data <- data.frame(
  Corpus = c(rep("china21", 1), 
             rep("china22", 1), 
             rep("china23", 1)),
  Size = c(nrow(china21), 
           nrow(china22), 
           nrow(china23))
)

# 绘制柱状图
ggplot(data, aes(x = Corpus, y = Size, fill = Corpus)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Size), vjust = -0.5) +  # 添加文本标签
  labs(x = "Corpus", y = "Size", title = "Size of Each Corpus") +
  theme_minimal()

# We'll save this data
saveRDS(china23, "data/df23")
saveRDS(china22, "data/df22")
saveRDS(china21, "data/df21")


# First, we'll subset again on the kind of articles we're interested in:
china21 <- china21[china21$type == "article" & china21$section_id == "world",]
china22 <- china22[china22$type == "article" & china22$section_id == "world",]
china23 <- china23[china23$type == "article" & china23$section_id == "world",]

# First, we'll tidy up our initial dataframes.
tidy21 <- china21 %>%
  select(headline,
         byline,
         date = web_publication_date, # Rename date variable
         section_name,
         standfirst,
         body_text
  ) %>%
  mutate(date = as_datetime(date)) # parse date

tidy22 <- china22 %>%
  select(headline,
         byline,
         date = web_publication_date, # Rename date variable
         section_name,
         standfirst,
         body_text
  ) %>%
  mutate(date = as_datetime(date)) # parse date

tidy23 <- china23 %>%
  select(headline,
         byline,
         date = web_publication_date, # Rename date variable
         section_name,
         standfirst,
         body_text
  ) %>%
  mutate(date = as_datetime(date)) # parse date

# Next, we'll remove the initial (large) dataframes from memory
rm(china21)
rm(china22)
rm(china23)

# Now we have a more wieldy tibble. 
head(tidy21)
head(tidy22)
head(tidy23)

# Let's check for duplicates again:
which(duplicated(tidy21$headline))
which(duplicated(tidy22$headline))
which(duplicated(tidy23$headline))

# We can use the same code to drop duplicated headlines:
tidy21 <- tidy21[-which(duplicated(tidy21$headline)),]
tidy22 <- tidy22[-which(duplicated(tidy22$headline)),]
tidy23 <- tidy23[-which(duplicated(tidy23$headline)),]

# Let's also tidy the body_text column before we transform into a corpus
tidy21$body_text <- str_replace(tidy21$body_text, "\u2022.+$", "")
tidy22$body_text <- str_replace(tidy22$body_text, "\u2022.+$", "")
tidy23$body_text <- str_replace(tidy23$body_text, "\u2022.+$", "")

# Creating a corpus object
corp21 <- corpus(tidy21, 
                 docid_field = "headline",
                 text_field = "body_text")

corp22 <- corpus(tidy22, 
                 docid_field = "headline",
                 text_field = "body_text")

corp23 <- corpus(tidy23, 
                 docid_field = "headline",
                 text_field = "body_text")

# Creating a useful summary object of our corpus
corpSum21 <- summary(corp21, 
                     n = nrow(docvars(corp21)) #note: the default is n=100
) 

corpSum22 <- summary(corp22, 
                     n = nrow(docvars(corp22)) #note: the default is n=100
)

corpSum23 <- summary(corp23, 
                     n = nrow(docvars(corp23)) #note: the default is n=100
)

head(corpSum21[,-8])
head(corpSum22[,-8])
head(corpSum23[,-8])

## 2. Corpus statistics
#  We can use the corpus summary object to start creating statistical 
#  plots of our text data, for instance a histogram of articles over time:
corpSum21 %>%
  ggplot(aes(date)) +
  geom_histogram() # note, we can use geom_density() to give the pdf

corpSum22 %>%
  ggplot(aes(date)) +
  geom_histogram() # note, we can use geom_density() to give the pdf

corpSum23 %>%
  ggplot(aes(date)) +
  geom_histogram() # note, we can use geom_density() to give the pdf

# We can also plot these simultaneously using lubridate
ggplot(data = NULL) +
  geom_density(aes(yday(corpSum21$date)), color = "red") +
  geom_density(aes(yday(corpSum22$date)), color = "blue") +
  geom_density(aes(yday(corpSum23$date)), color = "green")+
  labs(color = "Data Source")

# We can calculate additional statistics using the summary object. 
# For example, the TTR is the ratio of types to tokens:
corpSum21$ttr <- corpSum21$Types / corpSum21$Tokens
corpSum22$ttr <- corpSum22$Types / corpSum22$Tokens
corpSum23$ttr <- corpSum23$Types / corpSum23$Tokens

# We can plot this over time as well:
ggplot(data = NULL) +
  geom_point(aes(yday(corpSum21$date), corpSum21$ttr), col = "red") +
  geom_point(aes(yday(corpSum22$date), corpSum22$ttr), col = "blue") +
  geom_point(aes(yday(corpSum23$date), corpSum23$ttr), col = "green") +
  geom_smooth(aes(yday(corpSum21$date), corpSum21$ttr), col = "red") +
  geom_smooth(aes(yday(corpSum22$date), corpSum22$ttr), col = "blue") +
  geom_smooth(aes(yday(corpSum23$date), corpSum23$ttr), col = "green")

## 3. Creating the tokens list and the dfm
# Let's move on to creating our other data objects: the tokens list and 
# the dfm. Here are the steps we followed last week:
toks21 <- quanteda::tokens(corp21, 
                           include_docvars = TRUE,
                           remove_numbers = TRUE,
                           remove_punct = TRUE,
                           remove_symbols = TRUE,
                           remove_separators = TRUE,
                           remove_url = TRUE) %>% #Create tokens object
  tokens_tolower() %>% # Transform to lower case
  tokens_remove(stopwords("english")) # Remove stopwords

toks22 <- quanteda::tokens(corp22, 
                           include_docvars = TRUE,
                           remove_numbers = TRUE,
                           remove_punct = TRUE,
                           remove_symbols = TRUE,
                           remove_separators = TRUE,
                           remove_url = TRUE) %>% #Create tokens object
  tokens_tolower() %>% # Transform to lower case
  tokens_remove(stopwords("english")) # Remove stopwords

toks23 <- quanteda::tokens(corp23, 
                           include_docvars = TRUE,
                           remove_numbers = TRUE,
                           remove_punct = TRUE,
                           remove_symbols = TRUE,
                           remove_separators = TRUE,
                           remove_url = TRUE) %>% #Create tokens object
  tokens_tolower() %>% # Transform to lower case
  tokens_remove(stopwords("english")) # Remove stopwords

# Find collocations
colc21 <- textstat_collocations(toks21, size = 2, min_count = 10) 
colc22 <- textstat_collocations(toks22, size = 2, min_count = 10) 
colc23 <- textstat_collocations(toks23, size = 2, min_count = 10)

# This time, let's look at the z scores to see what cut-off to use
?textstat_collocations

toks21 <- tokens_compound(toks21, pattern = colc21[colc21$z > 17,])
toks22 <- tokens_compound(toks22, pattern = colc22[colc22$z > 17,])
toks23 <- tokens_compound(toks23, pattern = colc23[colc23$z > 17,])

# Remove whitespace
toks21 <- tokens_remove(quanteda::tokens(toks21), "") 
toks22 <- tokens_remove(quanteda::tokens(toks22), "") 
toks23 <- tokens_remove(quanteda::tokens(toks23), "") 

# Stem tokens?
toks21 <- tokens_wordstem(toks21)
toks22 <- tokens_wordstem(toks22)
toks23 <- tokens_wordstem(toks23)

# Removing extra stopwords
dfm21 <- dfm(toks21)
dfm22 <- dfm(toks22)
dfm23 <- dfm(toks23)

topfeatures(dfm21)
topfeatures(dfm22)
topfeatures(dfm23)

super_stops <- c("said","use","case","can","now","first","call",
                 "say","peopl","like","day","sinc",
                 "also","new","countri","people","year","time","includ","nation",
                 "country","one","report","china","need","take","claim","see","show")

toks21 <- tokens_remove(toks21, super_stops,
                        valuetype = "glob")

toks22 <- tokens_remove(toks22, super_stops,
                        valuetype = "glob")

toks23 <- tokens_remove(toks23, super_stops,
                        valuetype = "glob")
# run again
dfm21 <- dfm(toks21)
dfm22 <- dfm(toks22)
dfm23 <- dfm(toks23)

# show
topfeatures(dfm21)
topfeatures(dfm22)
topfeatures(dfm23)

# save our data for next time
saveRDS(dfm21, "data/dfm21")
saveRDS(dfm22, "data/dfm22")
saveRDS(dfm23, "data/dfm23")

####### ML
## 1. Read in and wrangle data
#     a) In the data folder you'll find a large data.frame object called 
#        ukr_h1_2022. Read it in, and check the type of articles it contains.
dat <- df

#     b) Pre-process the data.frame.
dat$body_text <- str_replace(dat$body, "\u2022.+$", "")
dat <- dat[-which(grepl("briefing", dat$web_title) == TRUE),]

corp23 <- corpus(dat, 
               docid_field = "web_title",
               text_field = "body")

toks23 <- quanteda::tokens(corp23, 
                           include_docvars = TRUE,
                           remove_numbers = TRUE,
                           remove_punct = TRUE,
                           remove_symbols = TRUE,
                           remove_separators = TRUE,
                           remove_url = TRUE) %>% #Create tokens object
  tokens_tolower() %>% # Transform to lower case
  tokens_remove(stopwords("english")) # Remove stopwords

# Find collocations
colc23 <- textstat_collocations(toks21, size = 2, min_count = 10) 


toks23 <- tokens_compound(toks23, pattern = colc23[colc23$z > 17,])

# Remove whitespace
toks23 <- tokens_remove(quanteda::tokens(toks23), "") 

# Stem tokens?
toks23 <- tokens_wordstem(toks23)

super_stops <- c("said","use","case","can","now","first","call",
                 "say","peopl","like","day","sinc",
                 "also","new","countri","people","year","time","includ","nation",
                 "country","one","report","china","need","take","claim","see","show")

toks23 <- tokens_remove(toks23, super_stops,
                        valuetype = "glob")

dfm <- dfm(toks23)
dfm <- dfm_trim(dfm, min_docfreq = 20) # this can be a *very* important step

## 2. Perform STM 
# Convert dfm to stm
stmdfm <- convert(dfm, to = "stm")

# Set k
K <- 8

# Run STM algorithm
modelFit <- stm(documents = stmdfm$documents,
                vocab = stmdfm$vocab,
                K = K,
                prevalence = ~ source + s(as.numeric(date)), 
                data = stmdfm$meta,
                max.em.its = 500,
                init.type = "Spectral",
                seed = 2023,
                verbose = TRUE)

# Save your model!
saveRDS(modelFit, "data/modelFit")
