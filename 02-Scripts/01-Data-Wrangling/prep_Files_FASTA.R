# Script to write fasta files for each subtype

## load data
load("./01-Data/01-Processed-Data/metadata.rds")

## packages
library(dplyr)


## helper functions



## fastafiles

fastafiles <- list.files("./01-Data/00-Raw-Data/GISAID Sequences", full.names = T, pattern = "[.]fasta$")


if(!dir.exists("./01-Data/00-Raw-Data/GISAID Sequences/subtypes")){
  dir.create("./01-Data/00-Raw-Data/GISAID Sequences/subtypes")
}


seqs.df.list <- list()


for(i in 1:length(fastafiles)){
  
  seqs <- readLines(fastafiles[i])
  
  seqs.df <- data.frame(label = seqs[which(substr(seqs, 1,1)==">")]) %>%
    mutate(ID = sub(">", "", label))
  
  start.indices <- which(substr(seqs, 1,1)==">")+1
  stop.indices <- c(which(substr(seqs, 1,1)==">")[-1]-1, length(seqs))
  
  seqs.df$seq <- NA
  
  for(k in seq_along(start.indices)){
    seqs.df$seq[k] <- paste0(seqs[start.indices[k]:stop.indices[k]], collapse = "")
  }
  
  seqs.df.list[[i]] <- seqs.df
  
}



## join
seqs.df <- seqs.df.list %>% 
  bind_rows() %>% 
  filter(ID %in% meta$Isolate_Id)


seqs.df <- full_join(seqs.df, 
                     meta, 
                     by = c("ID"="Isolate_Id"))



seqs.df <- seqs.df %>% 
  mutate(seqlength = nchar(seq)) %>% 
  filter(seqlength>1600) %>% 
  filter(!(ID%in%c("EPI_ISL_90552") & substr(seq,1,3)=="aaa")) %>% #makes alignment gappy
  filter(!ID%in%c("EPI_ISL_187393", "EPI_ISL_187398", "EPI_ISL_187396", "EPI_ISL_187403", #insertion just before stop codon makes nonsense
                  "EPI_ISL_187400", "EPI_ISL_187406", "EPI_ISL_187407", "EPI_ISL_187404", 
                  "EPI_ISL_187405", "EPI_ISL_187408", "EPI_ISL_187409", "EPI_ISL_187418", 
                  "EPI_ISL_187438", "EPI_ISL_187437", "EPI_ISL_187440")) 





# h1insert948956 <- c("EPI_ISL_158603", "EPI_ISL_167605", "EPI_ISL_167620", "EPI_ISL_167653", 
#                     "EPI_ISL_167657", "EPI_ISL_167666", "EPI_ISL_167667", "EPI_ISL_167665", 
#                     "EPI_ISL_167670", "EPI_ISL_167676", "EPI_ISL_167681", "EPI_ISL_379883", 
#                     "EPI_ISL_90127", "EPI_ISL_90128", "EPI_ISL_90129", "EPI_ISL_99940", 
#                     "EPI_ISL_132789", "EPI_ISL_78121", "EPI_ISL_78122", "EPI_ISL_78123", 
#                     "EPI_ISL_78124", "EPI_ISL_78125", "EPI_ISL_78126", "EPI_ISL_78127", 
#                     "EPI_ISL_78128", "EPI_ISL_78129", "EPI_ISL_78130", "EPI_ISL_78131", 
#                     "EPI_ISL_78132", "EPI_ISL_78133", "EPI_ISL_78134", "EPI_ISL_78135", 
#                     "EPI_ISL_78136", "EPI_ISL_78496", "EPI_ISL_78497", "EPI_ISL_78498", 
#                     "EPI_ISL_78499", "EPI_ISL_78500", "EPI_ISL_78501", "EPI_ISL_78514", 
#                     "EPI_ISL_78987", "EPI_ISL_78988", "EPI_ISL_78989", "EPI_ISL_78990", 
#                     "EPI_ISL_78991", "EPI_ISL_78992", "EPI_ISL_78993", "EPI_ISL_78994", 
#                     "EPI_ISL_78995", "EPI_ISL_78996", "EPI_ISL_78997", "EPI_ISL_78998", 
#                     "EPI_ISL_79008", "EPI_ISL_79019", "EPI_ISL_79020", "EPI_ISL_79021", 
#                     "EPI_ISL_79022", "EPI_ISL_79023", "EPI_ISL_79044", "EPI_ISL_79057", 
#                     "EPI_ISL_79086", "EPI_ISL_79087", "EPI_ISL_79088", "EPI_ISL_79089", 
#                     "EPI_ISL_79090", "EPI_ISL_87685", "EPI_ISL_87686", "EPI_ISL_87687", 
#                     "EPI_ISL_87688", "EPI_ISL_87689", "EPI_ISL_87696", "EPI_ISL_87697", 
#                     "EPI_ISL_87698", "EPI_ISL_87699", "EPI_ISL_87700", "EPI_ISL_87701", 
#                     "EPI_ISL_87702", "EPI_ISL_87711", "EPI_ISL_87712", "EPI_ISL_87713", 
#                     "EPI_ISL_87714", "EPI_ISL_87715", "EPI_ISL_87716", "EPI_ISL_87717", 
#                     "EPI_ISL_87718", "EPI_ISL_87719", "EPI_ISL_87720", "EPI_ISL_87721", 
#                     "EPI_ISL_87722", "EPI_ISL_202165", "EPI_ISL_80033", "EPI_ISL_80034", 
#                     "EPI_ISL_80035", "EPI_ISL_80036", "EPI_ISL_80037", "EPI_ISL_80038", 
#                     "EPI_ISL_80039", "EPI_ISL_80040", "EPI_ISL_80041", "EPI_ISL_80042", 
#                     "EPI_ISL_80043", "EPI_ISL_80044", "EPI_ISL_80045", "EPI_ISL_80046", 
#                     "EPI_ISL_80047", "EPI_ISL_80048", "EPI_ISL_80049", "EPI_ISL_80050", 
#                     "EPI_ISL_80051", "EPI_ISL_80052", "EPI_ISL_80053", "EPI_ISL_80054", 
#                     "EPI_ISL_80055", "EPI_ISL_80056", "EPI_ISL_121733", "EPI_ISL_121734", 
#                     "EPI_ISL_121735", "EPI_ISL_121736", "EPI_ISL_121737", "EPI_ISL_121738", 
#                     "EPI_ISL_121739", "EPI_ISL_121740", "EPI_ISL_121741", "EPI_ISL_121742", 
#                     "EPI_ISL_121743", "EPI_ISL_121744", "EPI_ISL_121745", "EPI_ISL_121746", 
#                     "EPI_ISL_121747", "EPI_ISL_121748", "EPI_ISL_121749", "EPI_ISL_121750", 
#                     "EPI_ISL_121751", "EPI_ISL_121752", "EPI_ISL_121753", "EPI_ISL_121754", 
#                     "EPI_ISL_121755", "EPI_ISL_121757", "EPI_ISL_121758", "EPI_ISL_121759", 
#                     "EPI_ISL_121760", "EPI_ISL_121761", "EPI_ISL_121762", "EPI_ISL_121763", 
#                     "EPI_ISL_121764", "EPI_ISL_121765", "EPI_ISL_121766", "EPI_ISL_121767", 
#                     "EPI_ISL_121768", "EPI_ISL_121769", "EPI_ISL_121770", "EPI_ISL_121771", 
#                     "EPI_ISL_121772", "EPI_ISL_121773", "EPI_ISL_121774", "EPI_ISL_130378", 
#                     "EPI_ISL_130377", "EPI_ISL_130373", "EPI_ISL_130372", "EPI_ISL_130371", 
#                     "EPI_ISL_130369", "EPI_ISL_130368", "EPI_ISL_130367", "EPI_ISL_130366", 
#                     "EPI_ISL_130365", "EPI_ISL_130364", "EPI_ISL_114145", "EPI_ISL_114144", 
#                     "EPI_ISL_114143", "EPI_ISL_114142", "EPI_ISL_114141", "EPI_ISL_114140", 
#                     "EPI_ISL_114139", "EPI_ISL_114138", "EPI_ISL_114137", "EPI_ISL_114136", 
#                     "EPI_ISL_114135", "EPI_ISL_114133", "EPI_ISL_114042", "EPI_ISL_114041", 
#                     "EPI_ISL_114040", "EPI_ISL_114039", "EPI_ISL_114038", "EPI_ISL_114037", 
#                     "EPI_ISL_114036", "EPI_ISL_114035", "EPI_ISL_114034", "EPI_ISL_114033", 
#                     "EPI_ISL_114032", "EPI_ISL_156768", "EPI_ISL_170672", "EPI_ISL_270942", 
#                     "EPI_ISL_293359", "EPI_ISL_270926", "EPI_ISL_270927", "EPI_ISL_270948", 
#                     "EPI_ISL_270949", "EPI_ISL_294442", "EPI_ISL_294441", "EPI_ISL_270929", 
#                     "EPI_ISL_270976", "EPI_ISL_4070975")




# N = 49725 +4





## write fastas

subtypes <- unique(seqs.df$subtype)


for(ii in 1:length(subtypes)){
  
  these.seqs <- seqs.df %>% 
    filter(subtype%in%subtypes[ii]) %>%
    select(label, seq)
  
  if(nrow(these.seqs)>0){
    write.table(these.seqs, 
                file=paste0("./01-Data/00-Raw-Data/GISAID Sequences/subtypes/", 
                            subtypes[ii],
                            "_n", nrow(these.seqs), ".fasta"), 
                sep="\n", 
                col.names = FALSE, 
                row.names = FALSE, 
                quote=FALSE)
  }
  
  
}





# fastafiles <- list.files("./01-Data/00-Raw-Data/GISAID Sequences/subtypes", full.names = T)
# 
# alignedfiles <- paste0(fastafiles, ".mafft")
# 
# system(paste0(paste0('mafft --auto "', fastafiles, '" > "', alignedfiles, '"'), collapse = ";"))






## save


## clean environment
rm(list = ls())
gc()






