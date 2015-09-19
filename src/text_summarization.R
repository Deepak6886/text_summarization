library(plyr)
library(dplyr)
library(data.table)
library(Matrix)
library(stringr)
library(syuzhet)

library(tm)
library(openNLP)
library(openNLPmodels.en)
library(rvest)
library(Rfacebook)

require(doMC)
registerDoMC(20)
options(scipen = 8)

# retrieve techcrunch and new york times articles
# through facebook posts on their pages respectively
token <- "CAACEdEose0cBAHZA3v9flW4RzvAzHWr2xM08MNLXTsd0MzZCKVez7XSc0Yg2ZCHdXHLmNti2vVUvGZCZBolHQixSayaZAuZBT4toq50DY6UOFSGVzQeJBqFMkhh9VVfPZBnTksZAcwoiIZCJG9FuFE3A8wOH1FUm0wEzWfnE10s2Yr0dr7Oq385AQau5vS59DkNWtHQXkz4SuXJwfrHwIoBVgP"
tc <- getPage("techcrunch", token, n = 100)
tc_web <- html(tc[1,6])
tc_title <- tc_web %>% html_node(".tweet-title , p") %>% html_text()
tc_article <- tc_web %>% html_nodes("p") %>% html_text()
tc_article <- gsub("\n", "", tc_article)
tc_article <- tc_article[tc_article != ""]

ny <- getPage("nytimes", token, n = 100)
ny_web <- html(ny[1,6])
ny_title <- ny_web %>% html_nodes("#story-heading") %>% html_text()
ny_article <- ny_web %>% html_nodes(".story-content") %>% html_text()
ny_article <- gsub("\n", "", ny_article)
ny_article <- ny_article[ny_article != ""]


# naive article summarization algorithm
naive_sumly <- function(text, method = "bing")
{
  agg_txt <- paste(text, collapse = "\n\n")
  txt_string <- as.String(agg_txt)
  word_ann <- Maxent_Word_Token_Annotator()
  sent_ann <- Maxent_Sent_Token_Annotator()
  txt_annotations <- annotate(txt_string, list(sent_ann, word_ann))
  txt_doc <- AnnotatedPlainTextDocument(agg_txt, txt_annotations)
  
  st <- sents(txt_doc)
  wt <- words(txt_doc)
  
  sens <- get_sentences(agg_txt)
  words_per_sens <- llply(st, function(x) {
    temp <- x[!(tolower(x) %in% stopwords("SMART"))]
    temp <- str_replace_all(temp, "[^[:alnum:]]", "")
    return(temp[temp != ""])
  })
  
  if (length(sens) > 3)
  {
    sim_mat <- foreach(a = words_per_sens, .combine = "cbind") %:%
      foreach(b = words_per_sens, .combine = "c") %dopar% {
        length(intersect(a, b))/((length(a) + length(b))/2)
      }
    spars_sim_mat <- Matrix(sim_mat, sparse = T)
    diag(spars_sim_mat) <- 1
    sens_sim <- colSums(spars_sim_mat)-1
    dat <- data.table(sentence = sens, total_sim = colSums(spars_sim_mat)-1)
    
    paras <- unlist(strsplit(agg_txt, split = "\n\n", fixed = T))
    cat(identical(paras, text))
    cat("\n\n")
    
    best_sens <- rep(NA, length(paras))
    max_para_vals <- rep(NA, length(paras))
    i <- 1
    for (p in paras)
    {
      best_sen <- NA
      max_para_val <- 0
      para_sens <- get_sentences(p)
      for (s in para_sens)
      {
        m <- dat[grepl(s, sentence, fixed = TRUE),]$total_sim
        if (m > max_para_val)
        {
          max_para_val <- m
          best_sen <- as.character(s)
        }
        else
        {
          max_para_val <- max_para_val
          best_sen <- best_sen
        }
      }
      best_sens[i] <- best_sen
      max_para_vals[i] <- max_para_val
      i <- i + 1
      cat(best_sen)
      cat("\n")
      cat(max_para_val)
      cat("\n\n")
    }
    best_sens <- best_sens[which(max_para_vals != 0)]
    max_para_vals <- max_para_vals[which(max_para_vals != 0)]
    #best_sens <- best_sens[which(max_para_vals > mean(dat$total_sim))]
    #max_para_vals <- max_para_vals[which(max_para_vals > mean(dat$total_sim))]
    return(list(summary = best_sens, max_sim_values = max_para_vals))
  }
  else
  {
    return(text)
  }
}

# summary example
trunc_article <- naive_sumly(text = tc_article)
