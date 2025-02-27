#### ExtractPDF tool 2025#########
###### ExtractPDF tool is developed in R to automate information
####extraction from unstructured portable document format (pdf) files.

library("dplyr")
library("stringr") # packages for handling stings
library("pdftools") #Utilities for extracting text, fonts, attachments, metadata from a pdf 

#update packages and poppler
update.packages()
poppler_config()

###create a vector of pdf file names 
files <- list.files(pattern = "pdf$")
#check if your pdf files have fonts, format/layout errors when rendered into text
files_I <- lapply(files, pdf_text)
str(files_I)


###if there are errors run this part of the code, where you change the format
##and fonts of the pdf files you recreate them and save them in a different directory


# Function to correct fonts, preserving layout using pdf_data for the pdfs
library(grid)
correct_fonts_pdfs <- function(inputDir, outputDir, font_family = "sans", font_size = 12, left_shift = 0.01) {
  # Create the output directory if it doesn't exist
  if (!dir.exists(outputDir)) {
    dir.create(outputDir, recursive = TRUE)
  }
  pdf_files <- list.files(inputDir, pattern = "\\.pdf$", full.names = TRUE) # List the pdf files in the input directory
  if (length(pdf_files) == 0) stop("No PDF files found in the input directory.")
  # Process each pdf file in the list
  for (pdf_file in pdf_files) {
    output_pdf <- file.path(outputDir, paste0("corrected_", basename(pdf_file)))# Generate the output file path
    # Extract text data with position information from the input pdf
    pdf_info <- pdf_data(pdf_file)
    pdf(file = output_pdf, width = 8.5, height = 11, family = font_family) # Create a new pdf with the specified font family
    # Loop through each page of the pdf
    for (page_num in seq_along(pdf_info)) {
      page_data <- pdf_info[[page_num]] # Extract the data for the current page
      # Check if the page_data is empty or has missing coordinates
      if (nrow(page_data) == 0) next
      # Safely get the maximum x and y coordinates, avoiding zero or missing values
      max_x <- max(page_data$x, na.rm = TRUE)
      max_y <- max(page_data$y, na.rm = TRUE)
      # Avoid division by zero issues
      if (max_x == 0 || max_y == 0) next
      grid.newpage() # Create a blank plot (using grid) for each page with proper viewports and scaling
      pushViewport(viewport(width = unit(1, "npc"), height = unit(1, "npc")))
      # Add each text element to the plot using the extracted coordinates
      apply(page_data, 1, function(row) {
        # Extract x, y positions, and text from the current row
        x <- (as.numeric(row["x"]) / max_x) - left_shift  # Shift x-coordinate to the left
        y <- 1 - (as.numeric(row["y"]) / max_y)  # Normalized y-coordinate (0 to 1 scale)
        # Check if text content is not empty and draw the text at appropriate coordinates
        if (nzchar(row["text"]) && !is.na(row["text"])) {
          tryCatch({
            # Draw the text at the appropriate coordinates with specified font and size
            grid.text(row["text"], x = unit(x, "npc"), y = unit(y, "npc"),
          gp = gpar(fontfamily = font_family, fontsize = font_size), just = "left")
          }, error = function(e) {
            # If drawing the text fails, print a warning and continue
            message(sprintf("Failed to draw text on page %d at (%f, %f): %s", page_num, x, y, e$message))
          })
        }
      })
    }
    # Close the device only if it is still open
    if (dev.cur() != 1) dev.off()
    message(sprintf("Processed and saved pdf with 'sans' font: %s", output_pdf))
  }
}
# Create a folder with the pdfs that cannot render & and a Newfolder with the correct fonta and layout  
input_dir <- "C:\\Users\\.......\\folder"
output_dir <- "C:\\Users\\\\Newfolder"

# Run the generalized function for all PDFs in the input directory
correct_fonts_pdfs(input_dir, output_dir, font_size = 8, left_shift = 0.04)


########
#########If there are No errors rendering your pdfs into text 
########you can run the rest of the code to Extract data
#########
#The execution time of the software is measured
system.time({ 
  files <- list.files(pattern = "pdf$") #create a vector of pdf file names
View(as.data.frame(files)) # check the numbering of the pdf files

###function to get the most frequent space values for each of  pdfs pages 
#and slice it using that value.
read_text <- function(text) {
  result <- ''
  columns_format <- 2
  lstops <- str_locate_all(text, " ") #Get all index of " " from page.
  #Puts the index of the most frequents ' ' in a vector.
  stops <- as.integer(names(sort(table(unlist(lstops)),decreasing=TRUE)[1:2]))
  for(i in seq(1, columns_format, by=1)) #Slice based in the specified number of columns 
  {
    temp_result <- sapply(text, function(x){
      start <- 1
      stop <-stops[i] 
      if(i > 1)            
        start <- stops[i-1] + 1
      if(i == columns_format)#last column, read until end.
        stop <- nchar(x)+1
      substr(x, start=start, stop=stop)
    }, USE.NAMES=FALSE)
    temp_result <- str_trim(temp_result)
    result <- append(result, temp_result)
  }
  result
}
### function to bind the text in a continuous format
correct_format <- function(text){    
  result <- ''
  for (i in 1:length(text)) {
    page <- text[i]
    t1 <- unlist(str_split(page, "\n"))      
    maxSize <- max(nchar(t1))
    t1 <- paste0(t1,strrep(" ", maxSize-nchar(t1)))
    result = append(result,read_text(t1))
  }
  result
} 


###Fucntion for multiple processing of the pdf, convert it into text, make text lower case, clean the text data,
#replace digit, punctuation, trim white space and use the functions above for formatting
multiple.func <- function(x) {
  pdftools::pdf_text(x) %>%
    str_to_lower() %>%
    str_replace_all ("[\\r\\n\\t]+", " ") %>%
    str_replace_all( "[:digit:]","") %>% 
    str_replace_all("[:punct:]", " ") %>% 
    str_trim() %>% 
    read_text() %>% 
    correct_format()
}


## apply the multiple.func function to all the pdfs
filesI <- lapply(files, multiple.func)

#######make every pdf file one big vector
filesI <- lapply(filesI, function(x) (print(paste(x,collapse=' '))))

#apply the str_split function, split the text at the "references" and remove the second part.
my_list_f <- lapply(filesI, function(vec) {
  sapply(str_split(vec, "\\breferences\\b"), function(x) (x[1]))
})


#################Customizable part of the code######

####Create a list of vectors with the data extraction labels, 
#containing  words of interest for extraction 
my_list_labels <- list(exp_route = c ("oral", "inhalation", "dermal", "ingestion"), 
                       chemical_class = c("pesticide", "metals", "persistent", "non-persistent", "pharmaceuticals", "emissions",
                                          "food additive", "polycyclic", "hydrocarbons", "cosmetics", "cosmetic", "mycotoxins","vocs", "parabens", "svocs","voc", 
                                          "tvoc", "phthalates", "pfass","bisphenols", "bisphenol", "radiation", "aerosol", "flame", "particulate matter", 
                                          "pm", "inorganic", "pops", "pop", "food contaminant"), 
                       exp_model_new = c("mathematical model","model", "script", "software","toolbox","empirical","equations", "equation", 
                                         "formulas", "formula", "regression"), 
                       known_models = c("mcra", "merlin-expo", "sheds", "deem", "calendex ", "cares", "consexpo", "integra",
                                        "qsar","raidar", "expocast", "usetox", "abicam", "pacem", "bprisc4", "pca", "bream", "hapem", 
                                        "caltox", "sprayexpo", "pangea", "icecrm","fhx", "protege", "gexframe", "efast"),
                       analysis_method = c("deterministic","probabilistic","statistical","probability", "uncertainity",
                                           "simulations"),
                       prog_stat_anal = c("r version", "r code", "version",  "python", "julia", "sas", "stata", "spss", "excel", "crystal ball", 
                                          "spreadsheet", "matlab", "python", "winbugs", "creme global", "genstat","visual basic", "openbugs"),
                       data_used = c("concentration", "frequency", "rate", "rates",  "gis", "consumption", "amount", "volume",
                                     "time", "dose", "duration", "area", "factor", "activity", "efficiency", "body weight"),
                       popul_study = c("individual", "population", "subpopulations", "cohort", "age groups", "age",
                                       "children", "adults"),
                       scope_study = c("human exposure", "exposure estimates", "brain", "risk",
                                       "liver", "kidney", "health", "risk assessment", "biomonitoring"))
names(my_list_labels)


### function to pass the regex expression to detect only the whole words and not part of words
correct_reg = function(vectorI) {
  vectorII <- paste("\\b", vectorI, "\\b", sep = "")
  vectorII
}
###apply the function to the  list to extract whole words 
my_list_labels_reg <- lapply(my_list_labels, correct_reg)
names(my_list_labels_reg)


#################
###########   Nested loop between the list of pdfs and the list of data extraction labels

library(purrr) 
my_list_2 <- vector("list", length = length(my_list_f)*length(my_list_labels_reg))
data_fr <- expand.grid(i = names(my_list_labels_reg), j = seq_along(my_list_f))[2:1]
data_fr <- apply(data_fr, 1, paste, collapse = "_")
names(my_list_2) <- data_fr


for (j in seq_along(my_list_f)) {
  for(i in names(my_list_labels_reg)) {
    labels_extr <- paste(j, i, sep = "_")
    my_list_2[[labels_extr]] <- my_list_labels_reg[[i]] %>%
      map (function (x)  str_extract_all(my_list_f[[j]], regex(x))) %>% 
      unlist() %>% 
      discard(is.na)
  }
}

# Convert my list of vectors of unequal length to a data frame
my_list_2_df<- do.call(cbind,lapply(1:length(my_list_2),
function(i) c(my_list_2[[i]], rep(NA, (max(lengths(my_list_2))-lengths(my_list_2)[i])))))
colnames(my_list_2_df) <- names(my_list_2)

View(my_list_2_df)
})


install.packages('xlsx')
library(openxlsx)
write.xlsx(as.data.frame(my_list_2_df), 
  'C:\\Users\\MARK\\ONTOX\\Data_extraction_R\\DataExtraction_PDFs_CutReferences_DataExtrProject\\DataExtraction_SysRevProject\\my_list_2_df.xlsx')

