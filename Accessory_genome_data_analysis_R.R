####Accessory genome Data analysis####

###Packages and libraries used in this script that need to be loaded###

install.packages("Rtools")
install.packages("tidyverse")
install.packages("RVAideMemorie")
install.packages("multcomp")
install.packages("rstatix")
install.packages("rlist")
library("tools")
library("RColorBrewer")
library("tidyverse")
library("dplyr")
library("data.table")
library("ggplot2")
library("rcompanion")
library("multcomp")
library("rstatix")
library("stats")
library("rlist")


## Load the metadata and data (i.e., matrix of '0' and '1') and transform to a data frame ##
  # In this case, metadata and data are in the same file

metadata_data <-read.csv("directory_path/file_name.csv", header = TRUE, sep = ";", row.names = 1)
metadata_data_df <- data.frame(metadata_data)


# Create a list of the gene names to be analysed

list_genes <- data.table(colnames(metadata_data_df)[-(1:7)]) # Metadata is organized in the first 7 columns, from column 8 on there is the gene names, output matrix from the genomic database (e.g., Resfinder)
rf <- list_genes$V1

# Check your data as an overall summary: group your data according the metadata you desire
num_rows_metadata1 <- data.frame(metadata_data_df %>% group_by(metadata1) %>% summarise(no_rows = length(metadata1)))
print(num_rows_metadata1)


# Create the list and levels with the name of the METADATA information to be used in the grouping (headers of the table) #
  # In this file 'metadata1' and 'metadata2' (e.g., 'Serotype', 'Matrix', 'Year', ...)

metadata1_list <- factor(x=metadata_data_df$metadata1)
metadata1_levels <- levels(metadata1_list)

metadata2_list <- factor(x=metadata_data_df$metadata2)
metadata2_levels <- levels(metadata2_list)


## Split and group the data according to the 'metadata' selected in the previous step (i.e., 'metadata1' and 'metadata2')
my_splits <- split(metadata_data_df, list(metadata_data_df$metadata1, metadata_data_df$metadata2))


## Create tables according to the previous selected metadata 
for (i in 1:length(my_splits)) {
  assign(serovars[i], my_splits[[i]])
}

## Create and calculate the tables of genetic presence according to the selected metadata as an object class while counting all the genomes that have a gene (i.e., '1')
presence <- lapply(my_splits, function(x) {
  colSums(x[, rf])})

presence_metadata1_metadata2 <- data.frame()
presence_metadata1_metadata2 <- rbind(presence, presence_metadata1_metadata2)
row.names(presence_metadata1_metadata2) <- list_data$V1

# Write and save the table of genetic presence as a .csv file
write.table(presence_metadata1_metadata2, "directory_path/presence_metadata1_metadata2.csv", sep=";")


## Create and calculate the tables of genetic prevalence according to the selected metadata as an object class while counting all the genomes that have a gene (i.e., '1')
prevalence <- lapply(my_splits, function(x){(colSums(x[, rf]))*100/nrow(x)})

prevalence_metadata1_metadata2 <- data.frame()
prevalence_metadata1_metadata2 <- rbind(prevalence, prevalence_metadata1_metadata2)
row.names(prevalence_metadata1_metadata2) <- list_resfinder$V1

# Write and save the table of genetic presence as a .csv file
write.table(prevalence_metadata1_metadata2, "directory_path/prevalence_metadata1_metadata2.csv", sep=";")
