extract_positions <- function(file,ind,a,pos_table) {
    i=1
    unique_tags = data.frame()    
    count_table = data.frame()
    
    count_table[i,1] = 0
    count_table[i,2] = 0
    count_table[i,3] = 0

    empty = 0
    
    # open stream, we'll go through each file using chunks of 2 million reads, lower this number if you experience problems with RAM (e.g. computer freezes)
    fs <- FastqStreamer(file,n=5e6)

    stream <- open(fs)
    on.exit(close(stream))
    repeat {
      ## input chunk
      fq <- yield(stream)
      if (length(fq) == 0)
        break
      
      seq1 <- sread(fq)
      count_table[i,1] = count_table[i,1]  + length(seq1)
      
      # find all sequences containing the end of the vector
      pattern = stri_detect_regex(seq1,'[CG]AT[CG]CGTTGGA')
      fq <- fq[pattern]
      
      seq2 <- sread(fq)
      count_table[i,2] = count_table[i,2]  + length(seq2)
      
      # extract genomic tag (i. e. 15 basepairs before the GATC or CATG)
      tags = stri_extract_first(seq2,regex="[ATCG]{15}[CG]AT[GC]CGTTGGA")
      tags = as.data.frame(tags)
      vindex = stri_extract_first(seq2,regex="[CG]AT[CG]CGTTGGA[ATCG]{12}")
      vindex1 = substring(vindex, 18)
      
      tags1 = as.data.frame(cbind(tags,vindex1))
      tags1 = left_join(tags1,ind,by="vindex1")
      
      # discard tags with undefined index
      tags1 = filter(tags1,!(is.na(lib)))
      tags1 = filter(tags1,! is.na(tags))
      
      # reads with tag
      count_table[i,3] = count_table[i,3] + dim(tags1)[1]
      
      # count occurance of each tag
      tag_counts = dplyr::count(tags1, tags,lib)
      colnames(tag_counts) = c("tag","lib","n")
      tag_counts= ungroup(tag_counts)
      if(empty == 0){
        unique_tags = tag_counts
        empty = 1
      }else{
        unique_tags = rbind(unique_tags,tag_counts)
      }
      unique_tags1 = group_by(unique_tags,tag,lib)
      
    }
    tag_counts = unique_tags1
    tag_counts$tag = substring(tag_counts$tag, 1,19)
    
    # number of different tags
    count_table[i,4] = dim(tag_counts)[1]

    # Is the tag in the table?
    a$V1 = toupper(a$V1)
    tag_counts_found = left_join(tag_counts,a,by=c("tag" = "V1"))
    tag_counts_found = filter(tag_counts_found,!(is.na(V2)))
    tag_counts_found1 = left_join(tag_counts_found,pos_table,by=c("V2" = "name"))
    
    # number of tags found in hash
    count_table[i,5] = dim(tag_counts_found)[1]
    
    # Is the tag unique at least on one side?
    tag_counts_found1 = mutate(tag_counts_found1,left = ifelse(count_tag1 == 1,ifelse(tag == toupper(tag1),"keep","no"),"more"))
    tag_counts_found1 = mutate(tag_counts_found1,right = ifelse(count_tag2 == 1,ifelse(tag == rev_tag2,"keep","no"),"more"))
    tag_counts_found1 = mutate(tag_counts_found1,note = ifelse(left == "more" | right=="more","multiple",ifelse(chr=="DDB0232429" & ((pos > 2249563 & pos < 3002134) | (pos> 3002337 & pos < 3755085)),"inverted repeat","unique")))
    
    #tag_counts_found1 = mutate(tag_counts_found1,orient = ifelse((side == "up" & tag == toupper(tag1)) | (side == "down" & tag == rev_tag2),"pos","neg"))
    tag_counts_found1 = filter(tag_counts_found1,note=="inverted repeat" | note == "unique")
    
    # combine counts of different sides
    final_counts = group_by(tag_counts_found1,V2,lib) %>% summarize(counts = sum(n))
    final_counts = filter(final_counts,counts > 10)
    count_table[i,6] = dim(final_counts)[1]
    # add annotations
    tag_counts_found1 = ungroup(tag_counts_found1)
    aa = select(tag_counts_found1,V2,note,lib)
    aa = unique(aa)
    final_counts1 = left_join(final_counts,aa,by=c("V2","lib"))
    filename = strsplit(file, "\\/")
    file1 = filename[[1]][lengths(filename)]
    file1 = gsub('.fastq.gz', '', file1)
    if(dim(final_counts1)[1] > 0){
      final_counts1$file = file1
    }
    # write position file
    filename = strsplit(file, "\\.")[[1]][1]
    write.table(final_counts1,file=paste(filename,"positions",sep="_"),row.names=F,sep="\t")
    rm(fq)
    
    test = group_by(final_counts1, lib) %>% mutate(count = n())
    test1 = unique(select(test,lib,count))
    count_table[i,7] = 0
    count_table[i,8] = 0
    count_table[i,9] = 0
    count_table[i,10] = 0
    count_table[i,11] = 0
    count_table[i,12] = 0
    count_table[i,13] = 0

    names(count_table) = c("reads","tags","tags with index","unique tags","identified tags","filtered tags","G1","G2","G3","C4","C6","C7","C8")

    if("G1" %in% test1$lib){count_table$G1 = filter(test1,lib == "G1")$count}
    if("G2" %in% test1$lib){count_table$G2 = filter(test1,lib == "G2")$count}
    if("G3" %in% test1$lib){count_table$G3 = filter(test1,lib == "G3")$count}
    if("C4" %in% test1$lib){count_table$C4 = filter(test1,lib == "C4")$count}
    if("C6" %in% test1$lib){count_table$C6 = filter(test1,lib == "C6")$count}
    if("C7" %in% test1$lib){count_table$C7 = filter(test1,lib == "C7")$count}
    if("C8" %in% test1$lib){count_table$C8 = filter(test1,lib == "C8")$count}
    
    newList <- list("table" = count_table, "positions" = final_counts1)
  return(newList)
}