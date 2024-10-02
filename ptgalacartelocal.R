library(tidyverse)
library(freqtables)
library(hablar)
library(psych)
library(sjmisc)
library(skimr)
library(correlation)
library(parameters)
library(jtools)
library(Hmisc)
library(tidytext)

library(paletteer)
library(extrafont)
library(gridExtra)
library(cowplot)

library(conText)
library(quanteda)
library(data.table)
library(textdata)

library(fastText)
library(text2vec)

library(lubridate)
library(ggthemes)


#prepping data

# ptg /pol/ data
ptg <- read_csv("presidenttrumpgeneral.csv")

# Script parameters
WINDOW_SIZE <- 6
DIM <- 300
ITERS <- 5

#where data is saved
destination_path <- "Data/stuff/"

#simply number of obs in ptg
total_size <- nrow(ptg)

#cleaning text
#removing cases with NA in 'body' (text var)
ptg <- ptg %>%
  filter(!is.na(body))

#creating new date variables from timestamp
ptg <- ptg %>%
  mutate(
    date = as.Date(timestamp),          # Convert timestamp to date
    yearwk = as.Date(cut(date, "week"))   # Extract year and week from date
  )

#obtain corpus of text from body
ptg_corpus <- corpus(ptg, text_field = "body")

#tokenizinbg all words and removing stop words, punctuation, symbols from corpus
toks <- tokens(ptg_corpus, 
               remove_punct = TRUE, 
               remove_symbols = TRUE, 
               remove_numbers = TRUE, 
               remove_url = TRUE,
               tolower = TRUE)

#remove stopwords
toks_nostop <- tokens_select(toks, pattern = stopwords("en"), selection = "remove")

#convert tokens back to text (restore original form minus stopwords, punctuation, etc.)
cleaned_ptg <- sapply(toks_nostop, paste, collapse = " ")

#obtain corpus of text from body
cleaned_ptg_corpus <- corpus(cleaned_ptg)

#retokenizing (tedious, i know)
toks <- tokens(cleaned_ptg_corpus)

#saving all tokens 
saveRDS(toks, file = paste0(destination_path, "toks", total_size, "30k.rds"))


#getting top k features
cleaned_ptg_dfm <- dfm(toks, verbose = TRUE)
#top 30k feats
top_feats <- featnames(cleaned_ptg_dfm)[order(-colSums(cleaned_ptg_dfm))[1:30000]]

# leave the pads so that non-adjacent words will not become adjacent
toks_feats <- tokens_select(toks, top_feats, padding = TRUE)
saveRDS(toks_feats, file = paste0(destination_path, "top_toks_feats", total_size, "30k.rds"))

# Construct the feature co-occurrence matrix
toks_fcm <- fcm(
  toks_feats,
  context = "window",
  window = WINDOW_SIZE,
  count = "frequency",
  tri = FALSE,
  weights = rep(1, WINDOW_SIZE)
)

#saving fcm
saveRDS(toks_fcm, file = paste0(destination_path, "combined_fcm", total_size, "30k.rds"))


# Estimate GloVe model using text2vec
#define glove
glove <- GlobalVectors$new(rank = DIM, x_max = 100, learning_rate = 0.05)

#fitting glove model on my fcm
wv_main <- glove$fit_transform(
  toks_fcm,
  n_iter = ITERS,
  convergence_tol = 1e-3,
  n_threads = parallel::detectCores()
)

#getting glove components
wv_context <- glove$components
#idk what this does
local_glove <- wv_main + t(wv_context)
#saving local glove embeddings (glove trained on our data)
saveRDS(local_glove, file = paste0(destination_path, "combined_local_glove", total_size, "30k.rds"))

#computing transformation matrix
local_transform <- compute_transform(x = toks_fcm, pre_trained = local_glove, weighting = 100)
#saving transformation matrix
saveRDS(local_transform, file = paste0(destination_path, "combined_local_transform", total_size, "30k.rds"))


#making a list of variations of DONALD TRUMP - my target word
trump_variants <- c("donald", "trump", "donald trump", "the donald", "drumpf", "trunp", 
                    "president trump", "the president", "potus")


#making function to replace Trump variants with TARGETWORD in the corpus
# Function to replace Trump variants with TARGETWORD
process_trump <- function(text_data) {
  for (trump_var in trump_variants) {
    text_data <- gsub(trump_var, "TARGETWORD", text_data, ignore.case = TRUE)
  }
  return(text_data)
}

#applying process_trump to 'cleaned_ptg' 
cleaned_ptg_targeted <- sapply(cleaned_ptg, process_trump)

#converting back to a corpus after replacing Trump variants
cleaned_ptg_corpus_targeted <- corpus(cleaned_ptg_targeted)

#saving the corpus with Trump variants replaced
saveRDS(cleaned_ptg_corpus_targeted, file = paste0(destination_path, "modified_ptg_corpus.rds"))

#_nws_corpus_leadertarg.rds is now equivalent to modified_ptg_corpus.rds

#binding yearwk from ptg to cleaned ptg corpus targeted to get dates for text
#tiny chunk by tiny chunk
for (i in seq(1, nrow(ptg), by = 1000)) {
  start_idx <- i
  end_idx <- min(i + 999, nrow(ptg)) # Process 1000 rows at a time
  docvars(cleaned_ptg_corpus_targeted, "yearwk")[start_idx:end_idx] <- ptg$yearwk[start_idx:end_idx]
}

#saving
saveRDS(cleaned_ptg_corpus_targeted, file = paste0(destination_path, "modified_ptg_corpus_with_yearwk.rds"))


#defining the English words for 'support' and 'opposition' to establish the axis
first_en <- "opposition" # Corresponds to 'المعارضة'
second_en <- "support"   # Corresponds to 'الدعم'


#Creating the get_similarity_scores function 
get_similarity_scores <- function(x, 
                                  target = "TARGETWORD", 
                                  first_vec, 
                                  second_vec, 
                                  pre_trained, 
                                  transform_matrix,
                                  group_var,
                                  window = 12L,
                                  norm = "l2",
                                  remove_punct = FALSE, 
                                  remove_symbols = FALSE, 
                                  remove_numbers = FALSE, 
                                  remove_separators = FALSE,
                                  valuetype = "fixed",
                                  hard_cut = FALSE,
                                  case_insensitive = TRUE) {
  
  # Tokenize corpus
  toks <- tokens(x, remove_punct = remove_punct, remove_symbols = remove_symbols, 
                 remove_numbers = remove_numbers, remove_separators = remove_separators)
  
  # Build tokenized corpus of contexts surrounding the target word
  target_toks <- tokens_context(x = toks, pattern = target, 
                                valuetype = valuetype, window = window, 
                                hard_cut = hard_cut, case_insensitive = case_insensitive)
  
  # Compute ALC embeddings
  target_dfm <- dfm(target_toks)
  target_dem <- dem(x = target_dfm, pre_trained = pre_trained, 
                    transform = TRUE, transform_matrix = transform_matrix, 
                    verbose = TRUE)
  
  # Aggregate embeddings over the grouping variable (yearwk)
  target_dem_grouped <- dem_group(target_dem, 
                                  groups = target_dem@docvars[[group_var]]) 
  
  # Cosine similarity for first vector of terms (support)
  if (length(first_vec) > 1) {
    y_matrix = as.matrix(pre_trained[intersect(first_vec, rownames(pre_trained)),])
  } else {
    y_matrix = t(as.matrix(pre_trained[intersect(first_vec, rownames(pre_trained)),]))
  }
  
  group_first_val <- sim2(target_dem_grouped, 
                          y = y_matrix, 
                          method = 'cosine', norm = norm)
  
  group_first_val <- rowMeans(group_first_val) 
  group_first_val <- tibble(group = factor(names(group_first_val)), 
                            first_val = unname(group_first_val))
  
  # Cosine similarity for second vector of terms (opposition)
  if (length(second_vec) > 1) {
    y_matrix = as.matrix(pre_trained[intersect(second_vec, rownames(pre_trained)),])
  } else {
    y_matrix = t(as.matrix(pre_trained[intersect(second_vec, rownames(pre_trained)),]))
  }
  
  group_sec_val <- sim2(target_dem_grouped, 
                        y = y_matrix, 
                        method = 'cosine', norm = norm)
  
  group_sec_val <- rowMeans(group_sec_val)
  group_sec_val <- tibble(group = factor(names(group_sec_val)), 
                          sec_val = unname(group_sec_val))
  
  # Combine results and calculate final similarity score (support - opposition)
  result <- left_join(group_first_val, group_sec_val, by = "group") %>% 
    mutate(val = first_val - sec_val) %>%
    select(group, val)
  
  return(result)
}



# Define the function to process cosine similarities between TARGETWORD and each end of the axis
process_cos_sim <- function(local_glove, local_transform) {
  
  # Load the modified corpus with Trump variants replaced by 'TARGETWORD'
  ptg_corpus <- readRDS("Data/stuff/modified_ptg_corpus_with_yearwk.rds")
  
  # Print a message indicating that cosine similarity calculations have started
  cat("Calculating cosine similarities...\n")
  
  # Calculate cosine similarities using `get_similarity_scores`
  cos_simsdf_all <- get_similarity_scores(
    x = ptg_corpus, 
    target = "TARGETWORD",           # Target word is 'TARGETWORD'
    first_vec = first_en,            # Opposition axis ('opposition')
    second_vec = second_en,          # Support axis ('support')
    pre_trained = local_glove,       # Pre-trained GloVe embeddings
    transform_matrix = local_transform, # Transformation matrix from earlier
    group_var = "yearwk",            # Grouped by the year-week variable
    window = 12L,                    # The context window (6 words per side of target)
    norm = "l2"                      # Normalization option
  )
  
  # Return the result of the cosine similarity computation
  return(cos_simsdf_all)
}

#apply the cosine similarity analysis 
cos_sim_results <- process_cos_sim(local_glove, local_transform)

#got warning: "Warning message:
#In asMethod(object) :
#  sparse->dense coercion: allocating vector of size 1.2 GiB""

#save the result 
saveRDS(cos_sim_results, file = paste0("Data/stuff/", "cos_sim_results.rds"))

#load the cosine similarity results
cos_sim_results <- readRDS("Data/stuff/cos_sim_results.rds")

#ensure 'yearwk' is in Date format 
cos_sim_results <- cos_sim_results %>%
  rename(yearwk = group) %>%
  mutate(yearwk = as.Date(yearwk)) %>%
  arrange(yearwk)

#plotting the cosine similarity results
ptgsupport <- ggplot(cos_sim_results, aes(x = yearwk, y = val)) +
  geom_point(alpha = 0.1, size = 1) +  
  geom_smooth(method = "loess", size = 1, span = 0.5, se = FALSE) +  
  theme_tufte(base_family = "Bahnschrift") +
  labs(x = "Year-week", 
       y = "Cosine similarity (Support - Opposition Index)") +  #y axis labels
  ylim(-0.35, 0.1) +  
  theme(legend.position = "none",  
        axis.text.x = element_text(size = 10),  
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 8),  
        axis.title.y = element_text(size = 8),  
        panel.border = element_rect(colour = "black", fill = NA, size = 1),  
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(size = 0.1, linetype = "solid"),  
        panel.grid.minor = element_line(size = 0.1, linetype = "solid"))  

#exporting plot
png("Plots/ptgsupport.png", units="in", 
    width = 6, height=4, res=400)
ptgsupport
dev.off()



#END
########################

#hillary lcinton analysisss

# Making a list of variations for Hillary Clinton
hillary_variants <- c("hillary", "hilldog", "hildog", "hillry", "hillery", "hilary", "hilary clinton", 
                      "hillary cliton", "crooked hillary", "hilllary", "lyin hillary", "crazy hillary", 
                      "beautiful hillary", "crookd hillary", "hillry clinton", "hilry", "illary clinton")

# Function to replace Hillary Clinton variations with TARGETWORD in the corpus
process_hillary <- function(text_data) {
  for (hillary_var in hillary_variants) {
    text_data <- gsub(hillary_var, "TARGETWORD", text_data, ignore.case = TRUE)
  }
  return(text_data)
}

# Applying process_hillary to 'cleaned_ptg' 
cleaned_ptg_targeted_hc <- sapply(cleaned_ptg, process_hillary)

# Converting back to a corpus after replacing Hillary Clinton variations
cleaned_ptg_corpus_targeted_hc <- corpus(cleaned_ptg_targeted_hc)

# Saving the corpus with Hillary Clinton variations replaced
saveRDS(cleaned_ptg_corpus_targeted_hc, file = paste0(destination_path, "modified_ptg_corpus_hc.rds"))


# Define a function to handle smaller chunks and save progress
process_in_chunks <- function(corpus, ptg_data, chunk_size = 100000) {
  
  total_docs <- nrow(ptg_data)
  num_chunks <- ceiling(total_docs / chunk_size)
  
  for (i in seq(1, total_docs, by = chunk_size)) {
    
    # Define start and end indices for each chunk
    start_idx <- i
    end_idx <- min(i + chunk_size - 1, total_docs)
    
    cat("Processing documents", start_idx, "to", end_idx, "\n")
    
    # Assign yearwk for the chunk
    docvars(corpus, "yearwk")[start_idx:end_idx] <- ptg_data$yearwk[start_idx:end_idx]
    
    # Save the intermediate result for the chunk
    saveRDS(corpus, file = paste0(destination_path, "modified_ptg_corpus_with_yearwk_chunk_", i, ".rds"))
    
    # Clear memory after each chunk is processed
    gc()
  }
  
  return(corpus)
}

# Run the batch processing function
cleaned_ptg_corpus_targeted_hc <- process_in_chunks(cleaned_ptg_corpus_targeted_hc, ptg)


# Saving the corpus with yearwk added
saveRDS(cleaned_ptg_corpus_targeted_hc, file = paste0(destination_path, "modified_ptg_corpus_with_yearwk_hc.rds"))



#creating the cosine similarity function for Hillary Clinton
process_cos_sim_hc <- function(local_glove, local_transform) {
  # Load the modified corpus with Hillary Clinton variants replaced by 'TARGETWORD'
  ptg_corpus_hc <- readRDS("Data/stuff/modified_ptg_corpus_with_yearwk_hc.rds")
  
  # Print a message indicating that cosine similarity calculations have started
  cat("Calculating cosine similarities for Hillary Clinton...\n")
  
  # Calculate cosine similarities using `get_similarity_scores`
  cos_simsdf_all_hc <- get_similarity_scores(
    x = ptg_corpus_hc, 
    target = "TARGETWORD",           # Target word is 'TARGETWORD'
    first_vec = first_en,            # Opposition axis ('opposition')
    second_vec = second_en,          # Support axis ('support')
    pre_trained = local_glove,       # Pre-trained GloVe embeddings
    transform_matrix = local_transform, # Transformation matrix from earlier
    group_var = "yearwk",            # Grouped by the year-week variable
    window = 12L,                    # The context window (6 words per side of target)
    norm = "l2"                      # Normalization option
  )
  
  # Return the result of the cosine similarity computation
  return(cos_simsdf_all_hc)
}


# Apply the cosine similarity analysis for Hillary Clinton
cos_sim_results_hc <- process_cos_sim_hc(local_glove, local_transform)

# Save the Hillary Clinton results to a new file
saveRDS(cos_sim_results_hc, file = paste0("Data/stuff/", "cos_sim_results_hc.rds"))

# Load the cosine similarity results for Hillary Clinton
cos_sim_results_hc <- readRDS("Data/stuff/cos_sim_results_hc.rds")


# Ensure 'yearwk' is in Date format 
cos_sim_results_hc <- cos_sim_results_hc %>%
  rename(yearwk = group) %>%
  mutate(yearwk = as.Date(as.numeric(as.character(yearwk)), origin = "1970-01-01")) %>%
  arrange(yearwk)


#take a peek at the cosine similarity results for Hillary Clinton
hc_support <- ggplot(cos_sim_results_hc, aes(x = yearwk, y = val)) +
  geom_point(alpha = 0.1, size = 1) +  
  geom_smooth(method = "loess", size = 1, span = 0.5, se = FALSE) +  
  theme_tufte(base_family = "Bahnschrift") +
  labs(x = "Year-week", 
       y = "Cosine similarity (Support - Opposition Index)") +  # y axis labels
  ylim(-0.35, 0.1) +  
  theme(legend.position = "none",  
        axis.text.x = element_text(size = 10),  
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 8),  
        axis.title.y = element_text(size = 8),  
        panel.border = element_rect(colour = "black", fill = NA, size = 1),  
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(size = 0.1, linetype = "solid"),  
        panel.grid.minor = element_line(size = 0.1, linetype = "solid"))

hc_support

#now to plot hc and dt together
# Add a var for target word: donald trump
cos_sim_results <- cos_sim_results %>%
  mutate(target_word = "Donald Trump")

#add var for hillary clinton as target
cos_sim_results_hc <- cos_sim_results_hc %>%
  mutate(target_word = "Hillary Clinton")

#merging the two dataframes
combined_cos_sim <- bind_rows(cos_sim_results, cos_sim_results_hc)

# Plotting the combined cosine similarity results
combined_plot <- ggplot(combined_cos_sim, aes(x = yearwk, y = val, color = target_word)) +
  geom_point(alpha = 0.1, size = 1) +  
  geom_smooth(method = "loess", size = 1, span = 0.5, se = FALSE) +  
  theme_tufte(base_family = "Bahnschrift") +
  labs(x = "Year-week", 
       y = "Cosine similarity (Support - Opposition Index)",
       color = "Target Word") +  # Adding a legend for target_word
  ylim(-0.35, 0.1) +  
  theme(legend.position = "right",  
        axis.text.x = element_text(size = 10),  
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 8),  
        axis.title.y = element_text(size = 8),  
        panel.border = element_rect(colour = "black", fill = NA, size = 1),  
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(size = 0.1, linetype = "solid"),  
        panel.grid.minor = element_line(size = 0.1, linetype = "solid"))  

combined_plot

# Exporting the Hillary Clinton plot
png("Plots/hcdtsupportplot.png", units="in", width = 6, height=4, res=400)
combined_plot
dev.off()
