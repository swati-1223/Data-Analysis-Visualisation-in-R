
# Load necessary libraries
library(reticulate)
library(tuber)
library(tidyverse)
library(opencv)
library(stm)
library(tm)
library(RCurl)
library(imager)
library(magick)
library(image.libfacedetection)
library(vosonSML)
library(sentimentr)
library(data.table)
library(dplyr)


# Add YouTube API Authrorization

yt_oauth("******************",
         token = "")


###############################################################################################################################################################################################################################

# SECTION 1 - DATA COLLECTION  & PREPARATION #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Import data from a given Channel id

# Channel 1 = Zalando
channelid_zalando = "UCH8Kxr6Z5SQ8mj8HiF5syLw"
channelstats_zalando <- get_channel_stats(channel_id = channelid_zalando)
allvideostats_zalando <- get_all_channel_video_stats(channel_id = channelid_zalando)

# Checking the data types of all the columns in the dataframe
str(allvideostats_zalando)

# Assigning suitable datatypes to the columns
allvideostats_zalando$viewCount <- as.numeric(allvideostats_zalando$viewCount)
allvideostats_zalando$likeCount <- as.numeric(allvideostats_zalando$likeCount)
allvideostats_zalando$commentCount <- as.numeric(allvideostats_zalando$commentCount)
allvideostats_zalando$publication_date <- as.Date(allvideostats_zalando$publication_date)


# As per the sampling criteria shortlisting most popular videos that are showcased on the channel page under videos tab and
# after applying the filter of popular. It was observed that these videos appear in an decreasing order of views. So we
# will sort all the videos on the basis of views (decreasing order), select the videos posted within the last 3 years and 
# select Top 100 of the popular videos. 


# Filtering on the basis of recency i.e. videos posted within last 3 years
relevant_videos_zalando <- allvideostats_zalando[format(allvideostats_zalando$publication_date,
                                                        format = "%Y") >= as.POSIXlt(Sys.Date())$year + 1900 -3,]

# Filtering Top 100 videos by views
relevant_videos_zalando <- relevant_videos_zalando[order(relevant_videos_zalando$viewCount , decreasing = T),]
relevant_videos_zalando_Top100 <- head(relevant_videos_zalando,100)

# Adding a calculated field called total interaction as sum of views, likes and comments

relevant_videos_zalando$total_interaction <- relevant_videos_zalando$viewCount + relevant_videos_zalando$likeCount + relevant_videos_zalando$commentCount

# Getting Thumbnail picture url and duration of each video and adding it to the relevant videos dataframe
# Initiating a counter i for FOR loop
i=1

for(videoid_zalando in relevant_videos_zalando_Top100$id) {
  videodetails_zalando <- get_video_details(video_id = videoid_zalando)
  thumbnail_url_zalando <- videodetails_zalando$items[[1]]$snippet$thumbnails$high$url
  relevant_videos_zalando_Top100$thumbnail[i] <- thumbnail_url_zalando
  
  contentdetails_zalando <- get_video_details(video_id = videoid_zalando, part = "contentDetails" )
  duration_zalando <- contentdetails_zalando$items[[1]]$contentDetails$duration
  relevant_videos_zalando_Top100$duration[i] <- duration_zalando
  
  i=i+1
}

# Converting duration into hours, minutes and seconds
relevant_videos_zalando_Top100$duration <- str_replace_all(relevant_videos_zalando_Top100$duration,
                                                           c("PT"="","H"= " hours ","M"=" minutes ","S"=" seconds"))


# In order to segregate SHORTS videos from usual long form videos we create a SHORTS URL for each video
# and check if it is a valid SHORTS url or not using URL response.

# Create shorts url for all videos
relevant_videos_zalando_Top100$ShortsURL <- paste("https://www.youtube.com/shorts/",
                                                  relevant_videos_zalando_Top100$id,
                                                  sep = "")

#if the url exists then it is a shorts indicated by 1 and if not then it is indicated by 0 under column SHORTS

relevant_videos_zalando_Top100$SHORTS <- NA

for (s1 in relevant_videos_zalando_Top100$id) {
  URL_response <- getURL(relevant_videos_zalando_Top100[relevant_videos_zalando_Top100$id == s1,]$ShortsURL)
  relevant_videos_zalando_Top100[relevant_videos_zalando_Top100$id == s1,]$SHORTS <- ifelse(URL_response == "",0,1)
}

###############################################################################################################################################################################################################################

# SECTION 2 - VISUAL ANALYSIS #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# Assessing Visual elements of a Thumbnail through Colorfulness, Brightness and Face Detection

#1. COLOURFULNESS
# The colorfulness of an image is a measure of how vivid or saturated its colors are. 
#In R Studio, you can use the imager package to compute the colorfulness of an image based 
# on the standard deviation of its RGB values.

#2. BRIGHTNESS
# In R Studio, you can use the imager package to compute the brightness of an image 
# based on its luminance values. The luminance values represent the brightness of 
# each pixel in the image, and are computed as a weighted sum of the RGB values. 
# The img[,,1], img[,,2], and img[,,3] expressions extract the red, green, 
# and blue color channels of the image, respectively. The luminance variable 
# computes the weighted sum of the RGB values using the standard luminance 
# coefficients for the sRGB color space. The mean() function then takes the average
# of all luminance values to obtain the overall brightness of the image.
# The coefficient values used in the formula Y = 0.2126 * R + 0.7152 * G + 0.0722 * B 
# are the standard coefficients for converting RGB values to luminance in the sRGB color space.
# 0.2126 for the red channel (R)
# 0.7152 for the green channel (G)
# 0.0722 for the blue channel (B)
# These values are based on the relative sensitivity of the human eye to different wavelengths of light. 
# The coefficients are designed to give a weighted sum of the R, G, and B channels that closely 
# approximates the perceived brightness of the color.

#3. FACE DETECTION

# Detect human faces in thumbnails to be used as a assessment criteria for the performance of a video
# using image.libfacedetetction package
# install.packages("magick")
# install.packages("image.libfacedetection", repos = "https://bnosac.github.io/drat")



# Initialising the counter
z=1

for(vid_z in relevant_videos_zalando_Top100$id) {
  # loading the thumbnail images
  image_zalando <- load.image(relevant_videos_zalando_Top100[relevant_videos_zalando_Top100$id == vid_z,]$thumbnail)
  
  # calculating RGB values of the image
  r_zalando <- image_zalando[,,1]
  g_zalando <- image_zalando[,,2]
  b_zalando <- image_zalando[,,3]
  
  # calculating colorfulness 
  colorfulness_z <- sqrt(sd(r_zalando)^2 + sd(g_zalando)^2 + sd(b_zalando)^2) / sqrt(mean(r_zalando)^2 + mean(g_zalando)^2 + mean(b_zalando)^2)
  relevant_videos_zalando_Top100$colorfulness[z] <- round(colorfulness_z,2)
  
  # calculating brightness
  luminance_z <- 0.2126 * r_zalando + 0.7152 * g_zalando + 0.0722 * b_zalando
  brightness_z <- mean(luminance_z)
  relevant_videos_zalando_Top100$brightness[z] <- round(brightness_z,2)
  
  # calculating number of faces detected
  image_z <- image_read(relevant_videos_zalando_Top100[relevant_videos_zalando_Top100$id == vid_z,]$thumbnail)
  faces_z <- image_detect_faces(image_z)
  relevant_videos_zalando_Top100$faces[z] <- faces_z$nr
  
  # increasing the counter
  z=z+1
}


round(mean(relevant_videos_zalando_Top100$colorfulness),2)
round(mean(relevant_videos_zalando_Top100$brightness),2)
round(mean(relevant_videos_zalando_Top100$faces),2)

###############################################################################################################################################################################################################################

# SECTION 3 - CONTENT ANALYSIS / TOPIC MODELLING #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Data Cleaning
# We will apply few preprocessing steps to our sample, including:
# • Convert to lower case
# • Remove Stop-Words
# • Remove numbers
# • Remove punctuation
# • Words shorter than the minimum word length are discarded
# • Stemming
# • Replace all non-alphanumeric characters
# • Create output

# 1. Zalando Topics across all videos

relevant_videos_zalando_Top100$text <- paste(relevant_videos_zalando_Top100$title, 
                                             relevant_videos_zalando_Top100$description,
                                             sep = " ")

relevant_videos_zalando_Top100_metadata <-
  cbind.data.frame(
    relevant_videos_zalando_Top100$viewCount,
    relevant_videos_zalando_Top100$likeCount,
    relevant_videos_zalando_Top100$commentCount,
    relevant_videos_zalando_Top100$total_interaction,
    relevant_videos_zalando_Top100$colorfulness,
    relevant_videos_zalando_Top100$brightness,
    relevant_videos_zalando_Top100$faces
  )

colnames(relevant_videos_zalando_Top100_metadata) <- c("viewCount", 
                                                       "likeCount", 
                                                       "commentCount",
                                                       "total_interaction",
                                                       "colorfulness",
                                                       "brightness",
                                                       "faces")

# Verify data 
head(relevant_videos_zalando_Top100_metadata,1)
head(relevant_videos_zalando_Top100,1)



# Replacing all NA values with 0
relevant_videos_zalando_Top100_metadata[is.na(relevant_videos_zalando_Top100_metadata)] <- 0
relevant_videos_zalando_Top100[is.na(relevant_videos_zalando_Top100)] <- 0

processed_zalando <-
  textProcessor(
    relevant_videos_zalando_Top100$text,
    metadata = relevant_videos_zalando_Top100_metadata,
    lowercase = TRUE,
    #*
    removestopwords = TRUE,
    #*
    removenumbers = TRUE,
    #*
    removepunctuation = TRUE,
    #*
    stem = FALSE,
    #*
    wordLengths = c(3, Inf),
    #*
    sparselevel = 1,
    #*
    language = "en",
    #*
    verbose = TRUE,
    #*
    onlycharacter = TRUE,
    # not def
    striphtml = FALSE,
    #*
    customstopwords = c("www","zalando","https","com",
                        "https", "comzalando", "shkitchen",
                        "instagram","facebook","iouri","quedarse",
                        "one","man", "rksv","din","måte","make","garrn",
                        "scoormetstijl","sterksel","nike",
                        "herrekampanje","zijn","geeft","aan","van","kinda",
                        "moda","een","que","diversidad","con","aqui","han",
                        "toni","kampanje","hos","egil","podladtchikov",
                        "som","para","het","havner"), #*
    v1 = FALSE
  ) #*


processed_zalando$vocab

# Filter out terms that don’t appear in more than 10 documents.
out_z <- prepDocuments(processed_zalando$documents, 
                       processed_zalando$vocab, 
                       processed_zalando$meta, 
                       lower.thresh=10)

docs_z <- out_z$documents
vocab_z <- out_z$vocab
meta_z <-out_z$meta


# Applying Structural Topic Model (STM) Algorithm 
set.seed(831)
system.time({
  First_STM <- stm(docs_z, vocab_z, 8,
                   data = meta_z,
                   seed = 100, max.em.its = 100
  )
})

# Plot first Topic Model
plot(First_STM, 
     type = "labels",
     n = 5,
     labeltype = "prob")

# Top5_Topics_Zalando
plot(First_STM,
     n=5)

# See all generated topics
labelTopics(First_STM)


# 2.Topics across all LONG FORMAT videos on Zalando's channel

# Separate Long format videos from all videos and store in a different dataframe

relevant_videos_long <- relevant_videos_all_300[relevant_videos_all_300$SHORTS == 0,]

relevant_videos_long_metadata <- cbind.data.frame(
  relevant_videos_long$viewCount,
  relevant_videos_long$likeCount,
  relevant_videos_long$commentCount,
  relevant_videos_long$total_interaction,
  relevant_videos_long$colorfulness,
  relevant_videos_long$brightness,
  relevant_videos_long$faces
)

# Text Processing
processed_long <-
  textProcessor(
    relevant_videos_long$text,
    metadata = relevant_videos_long_metadata,
    lowercase = TRUE,
    #*
    removestopwords = TRUE,
    #*
    removenumbers = TRUE,
    #*
    removepunctuation = TRUE,
    #*
    stem = FALSE,
    #*
    wordLengths = c(3, Inf),
    #*
    sparselevel = 1,
    #*
    language = "en",
    #*
    verbose = TRUE,
    #*
    onlycharacter = TRUE,
    # not def
    striphtml = FALSE,
    #*
    customstopwords = c("plwsdwa","comnew","www","prettylittlething",
                        "https","twitter","plwsdwa","nella",
                        "comprettylittlething","html","just",
                        "bit","rose","love","mae",
                        "com","http","plt", "plwsdwa","complaylist",
                        "molly","doja","cat","plwsdwa",
                        "youtube","comofficialplt","facebook",
                        "instagram","demi",
                        "www","shein","https","com","http",
                        "every","game","will",
                        "www","zalando","https","com",
                        "https", "comzalando", "shkitchen",
                        "instagram","facebook","iouri","quedarse",
                        "one","man", "rksv","din","måte","make","garrn",
                        "scoormetstijl","sterksel","nike",
                        "herrekampanje","zijn","geeft","aan","van","kinda",
                        "moda","een","que","diversidad","con","aqui","han",
                        "toni","kampanje","hos","egil","podladtchikov",
                        "som","para","het","havner",
                        "youtube","comofficialplt","facebook","instagram","demi",
                        "sheintryonhaul","sheingoodfinds","sheinmusicfest",
                        "brand","togetheriamstrong","supportervansporters",
                        "heretostay","zalandochallenge"),
    v1 = FALSE
  ) #*

processed_long$vocab


# Filter out terms that don’t appear in more than 10 documents.
out_long <- prepDocuments(processed_long$documents, 
                          processed_long$vocab, 
                          processed_long$meta, 
                          lower.thresh=10)

docs_long <- out_long$documents
vocab_long <- out_long$vocab
meta_long <-out_long$meta


# Applying Structural Topic Model (STM) Algorithm 
set.seed(831)
system.time({
  Long_STM <- stm(docs_long, vocab_long, 5,
                  data = meta_long,
                  seed = 10, max.em.its = 100
  )
})


# Plot LONG FORMAT videos' Topic Model
plot(Long_STM, 
     type = "labels",
     n = 5,
     labeltype = "prob")

plot(Long_STM, 
     n = 8)



# 3.Topics across all SHORT FORMAT videos on Zalando's channel

# Separate Short format videos from all videos and store in a different dataframe

relevant_videos_shorts <- relevant_videos_all_300[relevant_videos_all_300$SHORTS == 1,]


relevant_videos_shorts_metadata <- cbind.data.frame(
  relevant_videos_shorts$viewCount,
  relevant_videos_shorts$likeCount,
  relevant_videos_shorts$commentCount,
  relevant_videos_shorts$total_interaction,
  relevant_videos_shorts$colorfulness,
  relevant_videos_shorts$brightness,
  relevant_videos_shorts$faces
)

# Text Processing

processed_shorts <-
  textProcessor(
    relevant_videos_shorts$text,
    metadata = relevant_videos_shorts_metadata,
    lowercase = TRUE,
    #*
    removestopwords = TRUE,
    #*
    removenumbers = TRUE,
    #*
    removepunctuation = TRUE,
    #*
    stem = FALSE,
    #*
    wordLengths = c(3, Inf),
    #*
    sparselevel = 1,
    #*
    language = "en",
    #*
    verbose = TRUE,
    #*
    onlycharacter = TRUE,
    # not def
    striphtml = FALSE,
    #*
    customstopwords = c("www","prettylittlething","https","twitter","plwsdwa","nella",
                        "comprettylittlething","html","just","bit","rose","love","mae",
                        "com","http","plt", "plwsdwa","complaylist","molly","doja","cat",
                        "youtube","comofficialplt","facebook","instagram","demi",
                        "shein", "shorts",
                        "zalando","comzalando", "shkitchen",
                        "one","man", "rksv","din","måte","make","garrn",
                        "scoormetstijl","sterksel",
                        "toni","kampanje",
                        "sheingoodfinds", "nike",
                        "herrekampanje","sheintryonhaul",
                        "zalandochallenge", "game","cleaning",
                        "moda","home","heretostay","must",
                        "kitchen","give","thing","need",
                        "organization","organize",
                        "lyksntf","aquí","diversidad","quedarse"
    ), #*
    v1 = FALSE
  ) #*

processed_shorts$vocab


# Filter out terms that don’t appear in more than 10 documents.
out_shorts <- prepDocuments(processed_shorts$documents, 
                            processed_shorts$vocab, 
                            processed_shorts$meta, 
                            lower.thresh=10)

docs_shorts <- out_shorts$documents
vocab_shorts <- out_shorts$vocab
meta_shorts <-out_shorts$meta


# Applying Structural Topic Model (STM) Algorithm 
set.seed(831)
system.time({
  Shorts_STM <- stm(docs_shorts, vocab_shorts, 5,
                    data = meta_shorts,
                    max.em.its = 100
  )
})


# Plot Short format videos' Topic Model
plot(Shorts_STM, 
     type = "labels",
     n = 5,
     labeltype = "prob")

vocab_shorts

plot(Shorts_STM, 
     n = 8)


###############################################################################################################################################################################################################################

# SECTION 4 - SENTIMENT ANALYSIS #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# AS there is a limitation in the number of API calls allowed for collecting comments 
# we will shortlist top 10 videos for sentiment analysis


# Selecting only those videos from ZALANDO's channel which have 1 or more comments

df_z_withcomments <- relevant_videos_zalando_Top100[relevant_videos_zalando_Top100$commentCount>0,]

# Only 77 videos had one or more comments
# We will take only comments from Top 10 videos only due to API call limitations

df_z_withcomments <-  df_z_withcomments %>% slice_max(df_z_withcomments$viewCount, n = 50)

# Creating a temporary list (comments_list) for holding comments from a single video at a time 
# and then appending it to the final comment list (comments_list_z)
comments_list <- data.frame()
comments_list_z <- data.frame()

# Initialising counter
vz = 1

for (vz in 1:nrow(df_z_withcomments)) {
  comments_list <- get_all_comments(video_id = df_z_withcomments$id[vz])
  comments_list_z <- rbind(comments_list_z,comments_list)
  vz = vz+1
}

sentiment_zalando = sentiment(comments_list_z$textOriginal)

sentiment_dz <- setDF(sentiment_zalando)

# Creating a Custom Function that generates a sentiment class based on sentiment score

get_sentiment_class <- function(sentiment_score) {
  
  sentiment_class = "Positive"
  
  if ( sentiment_score < 0) {
    sentiment_class = "Negative"
  } 
  
  else if (sentiment_score == 0) {
    sentiment_class = "Neutral"
  }
  
  sentiment_class
}


# Take average of sentiment score for each sentence in a single comment to give avg_sentiment
sentiment_dz <- sentiment_dz %>% group_by(element_id) %>% mutate(avg_sentiment = mean(sentiment))

sentiment_dz_avg <- aggregate(sentiment_dz$sentiment, list(sentiment_dz$element_id), FUN=mean)


colnames(sentiment_dz_avg) <- c("Comment","Sentiment Score")

# Add a sentiment_class attribute
sentiment_dz_avg$sentiment_class <- sapply(sentiment_dz_avg$sentiment_score,
                                           get_sentiment_class)

# Draw a pie chart showing percentage distribution of sentiment across comments

sentiment_summary <- count(sentiment_dz_avg,"sentiment_class")
sentiment_summary_total <- sum(sentiment_summary$freq)

sentiment_summary$percentage <- round((sentiment_summary$freq/sentiment_summary_total)*100,0)
sentiment_summary$percentage <- paste(sentiment_summary$percentage,"%", sep = "")


pie(sentiment_summary$freq, 
    paste(sentiment_summary$sentiment_class,
          " (", sentiment_summary$percentage, ")",
          sep = ""),
    col=c("#EC6B56","#FFC154","#47B39C")
    )


comment_sentiment_list_z <- cbind.data.frame(comments_list_z,sentiment_dz_avg)

###############################################################################################################################################################################################################################

# SECTION 5 - VIDEO PERFORMANCE ANALYSIS #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#Channel Level video stats

# Assign 0 to NA values

relevant_videos_zalando_Top100[is.na(relevant_videos_zalando_Top100)] <- 0

# Create a summary dataframe
summary_relevant_videos_zalando_Top100 <- cbind.data.frame(sum(relevant_videos_zalando_Top100$viewCount),
                                                           sum(relevant_videos_zalando_Top100$likeCount),
                                                           sum(relevant_videos_zalando_Top100$commentCount),
                                                           mean(relevant_videos_zalando_Top100$viewCount),
                                                           mean(relevant_videos_zalando_Top100$likeCount),
                                                           mean(relevant_videos_zalando_Top100$commentCount),
                                                           mean(relevant_videos_zalando_Top100$colorfulness),
                                                           mean(relevant_videos_zalando_Top100$brightness),
                                                           max(relevant_videos_zalando_Top100$faces),
                                                           min(relevant_videos_zalando_Top100$faces),
                                                           mean(relevant_videos_zalando_Top100$faces),
                                                           sum(relevant_videos_zalando_Top100$SHORTS)
                                                           )


colnames(summary_relevant_videos_zalando_Top100) <- c("Total_viewCount",
                                                      "Total_likeCount",
                                                      "Total_commentCount",
                                                      "Avg_viewCount",
                                                      "Avg_likeCount",
                                                      "Avg_commentCount",
                                                      "Avg_colorfulness",
                                                      "Avg_brightness",
                                                      "Max_faces",
                                                      "Min_faces",
                                                      "Avg_faces",
                                                      "Number_of_short_videos")

# Add calculated number of long videos by subtracting the number of short videos from total videos
summary_relevant_videos_zalando_Top100$Number_of_long_videos <- 100 - summary_relevant_videos_zalando_Top100$Number_of_short_videos

# Calculate derived video stats like "Likes gained per view" and "Comments gained per view"

relevant_videos_zalando_Top100$LikesPerView <- round((relevant_videos_zalando_Top100$likeCount / relevant_videos_zalando_Top100$viewCount)*100,2)
relevant_videos_zalando_Top100$CommentsPerView <- round((relevant_videos_zalando_Top100$commentCount / relevant_videos_zalando_Top100$viewCount)*100,2)


# Check correlation across all the mterics

All_videos_cor <- select(relevant_videos_zalando_Top100,
                         viewCount,
                         likeCount,
                         commentCount,
                         total_interaction,
                         colorfulness,
                         brightness,
                         faces,
                         LikesPerView,
                         CommentsPerView
                         )

Overall_correlation <- cor(All_videos_cor)
Overall_correlation

# Correlation interpretation
# Strong relationship => 0.7 - 1
# Moderate relationship => 0.45 - 0.7
# Weak relationship => 0.20 - 0.45
# No relationship => 0.00 - 0.20

########################################################---THE END---#######################################################################################################################################################################
