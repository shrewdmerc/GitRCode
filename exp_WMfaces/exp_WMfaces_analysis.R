# Name: Clay Gibson
# Date: 21 January 2015
# Description: Directed Research statistical analyses

path <- "/Users/cgibson/Dropbox/Senior Year/Term 1/Directed Research/FaceSequences/Data"
length(list.files(path))

setwd(path)
data_files = list.files(pattern="*.csv")
all_data = lapply(data_files, read.delim)

all_data = lapply(all_data, 
                  colnames(x) <- c("Participant_Number", "Initial_Face_Index", "Initial_Face_Angle",
                                   "Chosen_Face_Index", "Chosen_Face_Angle", "Reaction_Time", "Clicks", 
                                   "Initial_Face_Name", "Chosen_Face_Name")
