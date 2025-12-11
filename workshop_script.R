# Workshop Script: Text Analysis with LLMs using ellmer
# ======================================================
# This script demonstrates how to use LLMs for qualitative text analysis
# using the ellmer R package with Groq's free API.
#
# SETUP INSTRUCTIONS:
# 1. Install required packages:
#    install.packages(c("tidyverse", "readxl", "ellmer", "jsonlite"))
#
# 2. Get a free Groq API key:
#    - Visit https://console.groq.com/
#    - Sign up for a free account (no credit card required)
#    - Go to API Keys section and create a new key
#    - Copy your API key
#
# 3. Set your API key in R (choose one method):
#    Method A: Set environment variable in R
#      Sys.setenv(GROQ_API_KEY = "your-api-key-here")
#    Method B: Create a .Renviron file in your project directory with:
#      GROQ_API_KEY=your-api-key-here
#    Method C: Pass directly to chat_groq() using api_key parameter (less secure)

library(tidyverse)
library(readxl)
library(ellmer)

# ===================== Load Data =============================================
# Read the Excel file with free-text responses
# This file contains responses from participants about concealing infectious illness

# Update this path to where you have the Excel file
data_file <- "data/study_1_qualitative_coding_disc_con_xlsx"

# Read data from the "self-other" sheet
# You can also try "harm", "stigma", or "miss" sheets
data_df <- read_excel(data_file, sheet = "self-other") |>
  select(id = ResponseId, free_text = motivation_fr, wg = WG, ra = RA)

# Preview the data
head(data_df)

# Let's work with just a few examples for the workshop
# Select 5 responses to analyze
sample_texts <- data_df |>
  slice(1:5) |>
  pull(free_text)

# Display the sample texts
cat("Sample texts we'll analyze:\n")
for (i in seq_along(sample_texts)) {
  cat("\n--- Text", i, "---\n")
  cat(str_wrap(sample_texts[i], width = 80), "\n")
}

# ===================== Define Instructions ===================================
# These instructions tell the LLM how to rate the text responses
# WORKSHOP PARTICIPANTS: You can edit these instructions!

instructions <- '
We have collected free response data from students at the University of Michigan
and healthcare workers within Michigan Medicine.

Your job is to read through these free responses about why participants said they
were motivated to hide signs of infectious illness and indicate where they fall on
a number of different variables.

Does the participant mention motivations for concealment that were more related to
the self or more related to others?

Examples of self motivation include:
- Not wanting to miss out on in-person things like work or class
- Not wanting others to judge them
- Not wanting others to avoid them

Examples of other motivation include:
- Not wanting to worry other people
- Not wanting to burden others by missing a work shift

Coding scheme:
- Put "1" if it is a self motivation
- Put "2" if it is an other motivation
- Put "0" if it is neither/unclear
- It is possible for a response to mention both self and other reasons,
  so it is ok for there to be both a 1 and a 2 for the same response, i.e. "1,2"

Before answering, explain your reasoning step by step, using example phrases or words.
Then provide the final answer.

Your final answer should be either "0" or "1" or "2" or "1,2".
'

# ===================== Set Up LLM Client =====================================
# Create a client to interact with the Groq API
# Using moonshotai/kimi-k2-instruct-0905 which supports structured outputs
# Note: llama-3.3-70b-versatile does NOT support structured outputs

# Method 1: If you set GROQ_API_KEY as environment variable
client <- chat_groq(
  model = "moonshotai/kimi-k2-instruct-0905",
  system_prompt = instructions
)

# Method 2: If you want to pass the API key directly (less secure, for workshop only)
# client <- chat_groq(
#   model = "moonshotai/kimi-k2-instruct-0905",
#   system_prompt = instructions,
#   api_key = "your-api-key-here"
# )

# For simple chat without structured output, you can use llama-3.3-70b-versatile:
# client_llama <- chat_groq(model = "llama-3.3-70b-versatile", system_prompt = instructions)

# ===================== Analyze Texts =========================================
# Now let's analyze each text

# EXAMPLE 1: Simple chat with free text response
# -----------------------------------------------
cat("\n\n=== EXAMPLE 1: Simple Free Text Response ===\n")
simple_response <- client$chat(sample_texts[1])
cat("\nText:\n", str_wrap(sample_texts[1], width = 70), "\n")
cat("\nLLM Response:\n", str_wrap(simple_response, width = 70), "\n")

# EXAMPLE 2: Structured output using chat_structured()
# -----------------------------------------------
# Define the expected output structure using type_() functions
type_analysis <- type_object(
  "Analysis of motivation for concealment",
  reasoning = type_string(
    "Step by step reasoning using phrases or words from the text"
  ),
  answer = type_string("The numerical score: 0, 1, 2, or 1,2")
)

cat("\n\n=== EXAMPLE 2: Structured Output ===\n")

results <- list()

for (i in seq_along(sample_texts)) {
  cat("\n\n========================================\n")
  cat("Analyzing text", i, "of", length(sample_texts), "\n")
  cat("========================================\n")

  # Create a fresh client for each structured call to avoid chat history issues
  # (structured output responses can cause problems when reused in Groq provider)
  fresh_client <- chat_groq(
    model = "moonshotai/kimi-k2-instruct-0905",
    system_prompt = instructions
  )

  # Send the text to the LLM with structured output
  # This returns a named list with reasoning and answer fields
  data <- fresh_client$chat_structured(sample_texts[i], type = type_analysis)

  cat("\nText:", str_wrap(sample_texts[i], width = 70), "\n\n")
  cat("Reasoning:", str_wrap(data$reasoning, width = 70), "\n\n")
  cat("Answer:", data$answer, "\n")

  # Store result
  results[[i]] <- list(
    text_id = i,
    text = sample_texts[i],
    reasoning = data$reasoning,
    answer = data$answer
  )

  # Small pause to avoid hitting rate limits
  Sys.sleep(1)
}

# ===================== View Results ==========================================
# Convert results to a data frame for easier viewing

results_df <- map_df(results, function(x) {
  tibble(
    text_id = x$text_id,
    answer = x$answer,
    reasoning = x$reasoning
  )
})

print(results_df)

# ===================== Compare with Human Ratings ============================
# Compare LLM ratings with the two human raters (wg and ra)

comparison <- data_df |>
  slice(1:5) |>
  mutate(text_id = row_number()) |>
  left_join(results_df, by = "text_id") |>
  select(text_id, llm_answer = answer, human_rater_wg = wg, human_rater_ra = ra)

print(comparison)

# Calculate agreement
comparison <- comparison |>
  mutate(
    agrees_with_wg = llm_answer == as.character(human_rater_wg),
    agrees_with_ra = llm_answer == as.character(human_rater_ra)
  )

cat("\n\nAgreement with human raters:\n")
cat(
  "Agreement with rater WG:",
  mean(comparison$agrees_with_wg, na.rm = TRUE) * 100,
  "%\n"
)
cat(
  "Agreement with rater RA:",
  mean(comparison$agrees_with_ra, na.rm = TRUE) * 100,
  "%\n"
)
