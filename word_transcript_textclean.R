# Sample character vector
text <- c("Speaker 1:", "This is a sample", "line of text with", "Speaker 2:", "multiple line breaks.", "Speaker 3:", "We want to keep these lines.", "Speaker 4:", "But remove line breaks in others.")

# Define a function to process the text
process_text <- function(text_vector) {
  # Initialize an empty character vector to store the result
  result <- character(0)
  
  # Initialize a variable to keep track of whether we are inside a "Speaker" block
  inside_speaker_block <- FALSE
  
  # Iterate through each line of the input text
  for (line in text_vector) {
    # Check if the line contains the word "Speaker"
    if (grepl("Speaker", line)) {
      # If it does, add the line to the result vector and set inside_speaker_block to TRUE
      result <- c(result, line)
      inside_speaker_block <- TRUE
    } else {
      # If it doesn't, remove line breaks and add the line to the result vector
      line <- gsub("\n", " ", line)
      result <- c(result, line)
    }
  }
  
  return(result)
}

# Call the function to process the text
processed_text <- process_text(text)

# Print the processed text
cat(processed_text, sep = "\n")

transk <- readLines('../../Transcript.txt')
transk_flat <- stringr::str_flatten(transk)
processed_transk <- process_text(transk)

insert_line_breaks <- function(input_text) {
  # Use regular expression to insert line breaks before "Speaker"
  modified_text <- gsub("00", "\n00", input_text)
  
  return(modified_text)
}

# Call the function to insert line breaks
modified_text <- insert_line_breaks(transk_flat)
cat(modified_text)
write(modified_text, file = "transk_modified.txt")
