#!/usr/bin/env Rscript

# Author: David M. Louis
# Contact: dmlouis@stanford.edui
# Contact: dmlouis87@gmail.com

############################### Libraries ###############################
# install.packages("plyr")
library(plyr)

############################### Arguments ###############################
# expected format of input file is
cmd_args = commandArgs(trailingOnly = TRUE);
# midread_table_file = cmd_args[6] ???????????

input_file=read.csv(cmd_args, header=T)
# tableLength=length(midread_table[,1])
# print(cmd_args) #prints file name
print(paste0("input length: ", length(rownames(input_file))))
#midread_matrix=data.matrix(midread_table)

############################################################################################
# Analysis Functions
############################################################################################
#creating specificity sheets
#inputs:dataframe #2nd counted column #2nd columns merge files (needs 2)
#ex: specificty_table(df, "split_column", "split_col_item_1", "split_col_item_2")
specificity_table <- function(input_file, count_col, count_data_1, count_data_2){
  #creating specificity tables
  data_count <- count(input_file, vars = c("pattern", count_col))
  spec_table <- merge(subset(data_count, disease == count_data_1), subset(data_count, disease == count_data_2), by = 'pattern', all = T)
  colnames(spec_table)[colnames(spec_table) == 'freq.x'] <- count_data_1
  colnames(spec_table)[colnames(spec_table) == 'freq.y'] <- count_data_2
  spec_table[[paste0(count_col,'.x')]] <- NULL
  spec_table[[paste0(count_col,'.y')]] <- NULL
  spec_table[is.na(spec_table)] <- 0
  #advanced frequency
  spec_table[[paste0(count_data_1, "Freq")]] <- spec_table[[count_data_1]]/sum(spec_table[[count_data_1]])*100
  spec_table[[paste0(count_data_2, "Freq")]] <- spec_table[[count_data_2]]/sum(spec_table[[count_data_2]])*100
  #creating advanced specificity
  spec_table[[paste0(count_data_1, "Spec")]] <- spec_table[[count_data_1]]/(rowSums(spec_table[,c(count_data_1, count_data_2)]))*100
  spec_table[[paste0(count_data_2, "Spec")]] <- spec_table[[count_data_2]]/(rowSums(spec_table[,c(count_data_1, count_data_2)]))*100
  spec_table[['sum']] <- rowSums(spec_table[,c(count_data_1, count_data_2)])
  return(spec_table)
}

#Function ends
############################### Analysis  #################################

data_specificity  <- specificity_table(input_file, "disease", unique(input_file$disease)[1], unique(input_file$disease)[2])

#################################
############################### Outputs #################################

# print(gsub("^.*/", "", cmd_args))
write.csv(data_specificity, file = paste0("specificity_table_", gsub("^.*/", "", cmd_args)), quote = F, row.names = F)

