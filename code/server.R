#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(ShortRead)
library(stringi)
library(gdata)

source('extract_positions.R', encoding = 'UTF-8')

shinyServer(function(input, output) {
	value <- reactiveVal(0)
	value1 <- reactiveVal(0)
	
	a = seq(1,30,1)
  b = seq(31,60,1)

  lay1 = rbind(c(1,2),c(3,4),c(5,6))
  lay2 = rbind(c(1,2,NA,3,4),c(5,6,NA,7,8),c(9,10,NA,11,12))
  lay3 = rbind(c(1,2,NA,3,4,NA,5,6),c(7,8,NA,9,10,NA,11,12),c(13,14,NA,15,16,NA,17,18))
  lay4 = rbind(c(1,2,NA,3,4,NA,5,6,NA,7,8),c(9,10,NA,11,12,NA,13,14,NA,15,16),c(17,18,NA,19,20,NA,21,22,NA,23,24))
  lay5 = rbind(c(1,2,NA,3,4,NA,5,6,NA,7,8,NA,9,10),c(11,12,NA,13,14,NA,15,16,NA,17,18,NA,19,20),c(21,22,NA,23,24,NA,25,26,NA,27,28,NA,29,30))

    indices_grid1 = reactive({
      inFile <- input$file1
      f=read.table(inFile$datapath,header=T)
      f=as.data.frame(f)
      a=unique(filter(f,grid==1)$index)
      a
    })
    
    indices_grid2 = reactive({
      inFile <- input$file1
      f=read.table(inFile$datapath,header=T)
      f=as.data.frame(f)
      a=unique(filter(f,grid==2)$index)
      a
    })
    
    output$plot1 = renderPlot({
      
      inFile <- input$file1
      if (is.null(inFile)){
        a = data.frame(a=seq(1,30,1),il=seq(1,30,1))
      }else{
        f=read.table(inFile$datapath,header=T)
        f=as.data.frame(f)
        a=paste(filter(f,grid==1)$plate,filter(f,grid==1)$index,filter(f,grid==1)$enzyme,sep=" ")
        a=as.data.frame(a)
        switch(input$lgrid1,
          {il=c(1,2,3,4,5,6)},
          {il=c(1,2,5,6,9,10,3,4,7,8,11,12)},
          {il=c(1,2,7,8,13,14,3,4,9,10,15,16,5,6,11,12,17,18)},
          {il=c(1,2,9,10,17,18,3,4,11,12,19,20,5,6,13,14,21,22,7,8,15,16,23,24)},
          {il=c(1,2,11,12,21,22,3,4,13,14,23,24,5,6,15,16,25,26,7,8,17,18,27,28,9,10,19,20,29,30)}
        )     
        il=as.numeric(il)
        
        a$il = il
        a = arrange(a,il)
      }
      switch(input$lgrid1,
             grid.arrange(grobs=lapply(1:6, function(ii) grobTree(rectGrob(gp=gpar(fill="darkgrey")), textGrob(a[ii,1]))), layout_matrix = lay1,widths = c(1,1)),
             grid.arrange(grobs=lapply(1:12, function(ii) grobTree(rectGrob(gp=gpar(fill="darkgrey")), textGrob(a[ii,1]))), layout_matrix = lay2,widths = c(1,1,0.5,1,1)),
             grid.arrange(grobs=lapply(1:18, function(ii) grobTree(rectGrob(gp=gpar(fill="darkgrey")), textGrob(a[ii,1]))), layout_matrix = lay3,widths = c(1,1,0.5,1,1,0.5,1,1)),
             grid.arrange(grobs=lapply(1:24, function(ii) grobTree(rectGrob(gp=gpar(fill="darkgrey")), textGrob(a[ii,1]))), layout_matrix = lay4,widths = c(1,1,0.5,1,1,0.5,1,1,0.5,1,1)),
             grid.arrange(grobs=lapply(1:30, function(ii) grobTree(rectGrob(gp=gpar(fill="darkgrey")), textGrob(a[ii,1]))), layout_matrix = lay5,widths = c(1,1,0.5,1,1,0.5,1,1,0.5,1,1,0.5,1,1)))
    })
  
    output$plot2 = renderPlot({
      inFile1 <- input$file1
      if (is.null(inFile1)){
        b = data.frame(b=seq(31,60,1),bb=seq(1,30,1))
      }else{
        f1=read.table(inFile1$datapath,header=T)
        f1=as.data.frame(f1)
        b=paste(filter(f1,grid==2)$plate,filter(f1,grid==2)$index,filter(f1,grid==2)$enzyme,sep=" ")
    
        switch(input$lgrid2,
               {bb=c(1,2,3,4,5,6)},
               {bb=c(1,2,5,6,9,10,3,4,7,8,11,12)},
               {bb=c(1,2,7,8,13,14,3,4,9,10,15,16,5,6,11,12,17,18)},
               {bb=c(1,2,9,10,17,18,3,4,11,12,19,20,5,6,13,14,21,22,7,8,15,16,23,24)},
               {bb=c(1,2,11,12,21,22,3,4,13,14,23,24,5,6,15,16,25,26,7,8,17,18,27,28,9,10,19,20,29,30)}
        )     
        bb=as.numeric(bb)
        b=as.data.frame(b)
        b$bb = bb
        b = arrange(b,bb)
      }
      switch(input$lgrid2,
             grid.arrange(grobs=lapply(1:6, function(ii) grobTree(rectGrob(gp=gpar(fill="darkgrey")), textGrob(b[ii,1]))), layout_matrix = lay1,widths = c(1,1)),
             grid.arrange(grobs=lapply(1:12, function(ii) grobTree(rectGrob(gp=gpar(fill="darkgrey")), textGrob(b[ii,1]))), layout_matrix = lay2,widths = c(1,1,0.5,1,1)),
             grid.arrange(grobs=lapply(1:18, function(ii) grobTree(rectGrob(gp=gpar(fill="darkgrey")), textGrob(b[ii,1]))), layout_matrix = lay3,widths = c(1,1,0.5,1,1,0.5,1,1)),
             grid.arrange(grobs=lapply(1:24, function(ii) grobTree(rectGrob(gp=gpar(fill="darkgrey")), textGrob(b[ii,1]))), layout_matrix = lay4,widths = c(1,1,0.5,1,1,0.5,1,1,0.5,1,1)),
             grid.arrange(grobs=lapply(1:30, function(ii) grobTree(rectGrob(gp=gpar(fill="darkgrey")), textGrob(b[ii,1]))), layout_matrix = lay5,widths = c(1,1,0.5,1,1,0.5,1,1,0.5,1,1,0.5,1,1)))
    })
    
    observeEvent(input$read_list, {
    	value1(0)
    	value(1)             
    })
    
    observeEvent(input$read_list1, {
    	value(0)
    	value1(1)            
    })
    # ntext <- eventReactive(input$read_list, {
    # 	b1 = 1
    # 	print(b1)
    # 	b2 = 0
    # 	input$directory
    # })
    # ntext1 <- eventReactive(input$read_list1, {
    # 	b1 = 0
    # 	b2 = 1
    # 	input$directory
    # })
    
    
    DExpTable_data <- reactive({
      both=data.frame()
    	#D_files <- list.files(path = ntext(),pattern = "*.fastq.gz" ,recursive = F, full.names = T)
    	#print(length(D_files))
    	print(value())
    	print(value1())
      if(value() == 1){
      	print("test")
      	# find all files
      	D_files <- list.files(path = input$directory,pattern = "*.fastq.gz" ,recursive = F, full.names = T)
      	D = as.data.frame(D_files)
      	names(D) = c("name")
      	# read all possible insertion points and their annotations
      	pos_table <- read.delim("../help_files/pos_table1",stringsAsFactors = F)
      	# create the lookup table
      	a = as.data.frame(cbind(c(pos_table$tag1,pos_table$rev_tag2),c(pos_table$name,pos_table$name)))
      	# read indices
      	ind = read.table("../help_files/indices_0mm",header=T)
      	ind = as.data.frame(ind)
      	
      	count_table = data.frame()
      	results=data.frame()
      	
      	# Create a Progress object
      	progress <- Progress$new(min=1, max=length(D_files))
      	# Make sure it closes when we exit this reactive, even if there's an error
      	on.exit(progress$close())
      	
      	progress$set(message = "Analysing files", value = 1)
      	
      	n = length(D_files)
      	# go through each fastq file
      	for(i in 1:n) {
      		filename = strsplit(D_files[i], "\\.")[[1]][1]
      		filename = strsplit(filename, "\\/")
      		file1 = filename[[1]][lengths(filename)]
      		g1 = indices_grid1()
      		g2 = indices_grid2()
      		
      		if(startsWith(file1, "C") | startsWith(file1, "R")){
      			ind1 = filter(ind,lib %in% g1)
      		}else{
      			ind1 = filter(ind,lib %in% g2)
      		}
      		count_table1 = data.frame()
      		if(i == 1){
      			allList = extract_positions(D_files[i],ind1,a,pos_table)
      			results=allList$positions 
      			count_table = allList$table
      		}else{
      			allList = extract_positions(D_files[i],ind1,a,pos_table)
      			count_table = rbind(count_table,allList$table)
      			results=rbind(results,allList$positions) 
      		}
      		print(count_table)
      		progress$set(value = i)
      	}
      	count_table = mutate(count_table,percent_tags = round(100*tags/reads,digits=2))
      	count_table$file=D$name
      	count_table = select(count_table,file,1,2,3,percent_tags,4,5,6,7,8,9,10,11,12,13)
      	write.table(count_table,file=paste0(input$directory,"/stats"),row.names=F,sep="\t")
      	names(results) = c("name","lib","counts","note","file")
      	write.table(results,file=paste0(input$directory,"/all_counts"),row.names=F,sep="\t")
      	both = list("table" = count_table,"positions" = results)
      }else if(value1() == 1){
      	count_table = read.table(file=paste0(input$directory,"/stats"),sep="\t",header=T)
        results = read.table(file=paste0(input$directory,"/all_counts"),sep="\t",header=T)
        both = list("table" = count_table,"positions" = results)
        print(both)
      }
      
      both
    })
    
    output_DExpTable_data <- reactive({
      DExpTable_data()$table
    })
    
    output$DExp <- DT::renderDataTable({
      DT::datatable(output_DExpTable_data(), extensions = c('ColReorder'), rownames = FALSE, options = list(scrollX = T, dom = 'rltip', colReorder = TRUE,fixedHeader = TRUE),escape = F)
    })
    
    output$usable = renderPlot({
      data = output_DExpTable_data()
      data = select(data,1,5)
      names(data) = c("file","perc")
      data$file <- gsub("/Volumes/Pukeko/REMI-seq/Nextseq17/Nicole_18_10032017/", "", data$file)
      data$file <- gsub(".fastq.gz", "", data$file)
      ggplot(data, aes(x = factor(file), y = perc)) + geom_bar(stat = "identity") + labs(title = "Percentage of reads with tags",x = "Samples",y = "Percentage of reads") + coord_flip() + geom_hline(yintercept = 50,color="red")
    }, height = 1000, width = 1000)
    
    output$numb_mut = renderPlot({
      data = output_DExpTable_data()
      data = select(data,1,8)
      names(data) = c("file","mutants")
      data$file <- gsub("/Volumes/Pukeko/REMI-seq/Nextseq17/Nicole_18_10032017/", "", data$file)
      data$file <- gsub(".fastq.gz", "", data$file)
      C = input$lgrid1 * 24
      V = input$lgrid1 * 24
      ggplot(data, aes(x = factor(file), y = mutants)) + geom_bar(stat = "identity") + labs(title = "Number of mutants per sample",x = "Samples",y = "Number of mutants") + coord_flip() + geom_hline(yintercept = c(C,V),color="red")
    }, height = 1000, width = 1000)
    
    output$numb_mut_index = renderPlot({
      data = output_DExpTable_data()
      data = select(data,1,9:15)
      print(data)
      data$file <- gsub("/Volumes/Pukeko/REMI-seq/Nextseq17/Nicole_18_10032017/", "", data$file)
      data$file <- gsub(".fastq.gz", "", data$file)
      data1 = gather(data,lib,count,2:8)
      data1 = filter(data1,count>0)
      data1 = drop.levels(data1)
      ggplot(data1, aes(x = factor(file), y = count,fill=lib)) + geom_bar(stat = "identity") + labs(title = "Number of mutants per sample and index",x = "Samples",y = "Number of mutants") + coord_flip() + facet_wrap(~lib)
    }, height = 1000, width = 1000)
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("table",".csv", sep="")
      },
      
      content = function(file) {
        
        # Write to a file specified by the 'file' argument
        write.table(output_DExpTable_data(), file, sep = "\t",row.names = FALSE)
      }
    )
    
    output_positions_data <- reactive({
        DExpTable_data()$positions
    })
    
    output$pos <- DT::renderDataTable({
      DT::datatable(output_positions_data(), extensions = c('ColReorder'), rownames = FALSE, options = list(scrollX = T, dom = 'rltip', colReorder = TRUE,fixedHeader = TRUE),escape = F)
    })
    
    overlap_table_data <- reactive({
      
      all1 = output_positions_data()
      all1 = all1 %>% rowwise() %>% mutate(group=ifelse(grepl("C", file),"C",ifelse(grepl("V", file),"V",ifelse(grepl("h", file),"h",ifelse(grepl("R", file),"R",NA)))))
      all1 = all1 %>% group_by(file) %>% mutate(total = sum(counts))
      all1 = mutate(all1,total=sum(counts))
      all1 = all1 %>% rowwise() %>% mutate(norm = round(1000000 * (counts/total),2))
      all2 = filter(all1,norm>100)
      
      grid1 = filter(all2,group=="C" | group=="R")
      grid1 = group_by(grid1,name,lib)
      grid1 = mutate(grid1,s_max = max(norm)) 
      grid1 = grid1 %>% rowwise() %>% mutate(filter = ifelse(norm/s_max > 0.05,"y","n"))
      grid1 = filter(grid1,filter=="y")
      grid1 = grid1 %>% rowwise() %>% mutate(name_all=paste(name,group,lib,sep="_"))
      counts=plyr::count(grid1,"name_all")
      grid1 = left_join(grid1,counts,by="name_all")
      
      grid2 = filter(all2,group=="V" | group=="h")
      grid2 = group_by(grid2,name,lib)
      grid2 = mutate(grid2,s_max = max(norm)) 
      grid2 = grid2 %>% rowwise() %>% mutate(filter = ifelse(norm/s_max > 0.05,"y","n"))
      grid2 = filter(grid2,filter=="y")
      grid2 = grid2 %>% rowwise() %>% mutate(name_all=paste(name,group,lib,sep="_"))
      counts=plyr::count(grid2,"name_all")
      grid2 = left_join(grid2,counts,by="name_all")
      
      grids = data.frame()
      
      for(i in 1:length(indices_grid1())){
        a=filter(grid1,grepl("R",file),lib==indices_grid1()[i])
        b=filter(grid1,grepl("C",file),lib==indices_grid1()[i])
        d=inner_join(a,b,by=c("name","lib","filter"))
        
        grid1_ind = select(d,-filter,-name_all.x,-name_all.y)
        grid1_ind = grid1_ind %>% rowwise() %>% mutate(pos=paste(file.x,file.y,sep="_"))
        counts=plyr::count(grid1_ind,"pos")
        colnames(counts)=c("pos","mutants")
        grid1_ind = left_join(grid1_ind,counts,by="pos")
        
        if(dim(grid1_ind)[1] > 0){
          grid1_ind$index = indices_grid1()[i]
          if(dim(grids)[1] == 0){
            grids = grid1_ind
          }else{
            grids = rbind(grids,grid1_ind)
          }
        }
        
      }
      
      for(i in 1:length(indices_grid2())){
        a=filter(grid2,grepl("h",file),lib==indices_grid2()[i])
        b=filter(grid2,grepl("V",file),lib==indices_grid2()[i])
        d=inner_join(a,b,by=c("name","lib","filter"))
        
        grid2_ind = select(d,-filter,-name_all.x,-name_all.y)
        grid2_ind = grid2_ind %>% rowwise() %>% mutate(pos=paste(file.x,file.y,sep="_"))
        counts=plyr::count(grid2_ind,"pos")
        colnames(counts)=c("pos","mutants")
        grid2_ind = left_join(grid2_ind,counts,by="pos")
        
        if(dim(grid2_ind)[1] > 0){
          grid2_ind$index = indices_grid2()[i]
          if(dim(grids)[1] == 0){
            grids = grid2_ind
          }else{
            grids = rbind(grids,grid2_ind)
          }
        }
        
      }
      print(grids)
      
      grids
    })
    
    o_overlap_table_data <- reactive({
      overlap_table_data()
    })
    
    output$grid <- DT::renderDataTable({
      DT::datatable(o_overlap_table_data(), extensions = c('ColReorder'), rownames = FALSE, options = list(scrollX = T, dom = 'rltip', colReorder = TRUE,fixedHeader = TRUE),escape = F)
    })
    
    output$download_overlapData <- downloadHandler(
      filename = function() {
        paste("table",".csv", sep="")
      },
      
      content = function(file) {
        
        # Write to a file specified by the 'file' argument
        write.table(o_overlap_table_data(), file, sep = "\t",row.names = FALSE)
      }
    )
    
    overlap_plate_data <- reactive({
      
      stamps = read.table("../help_files/stamps_complete",header=T)
      stamps$plate <- factor(stamps$plate)
      data_long <- gather(stamps, column, pheno, X1:X12, factor_key=TRUE)
      data_long$column = gsub("X", "", data_long$column)
      data_long$plate = as.numeric(data_long$plate)
      data_long$column = as.numeric(data_long$column)
      
      pos_table <- read.delim("../help_files/pos_table1",stringsAsFactors = F)
      
      all = o_overlap_table_data()
      inFile = input$file1
      f=read.table(inFile$datapath,header=T)
      f=as.data.frame(f)
      
      #f = mutate(f,rstart = ifelse(pos==1 | pos==2,1,ifelse(pos==3 | pos == 4,9,ifelse(pos==5 | pos==6,17,0))),rstop = rstart + 8,cstart = ifelse(pos==1 | pos==3 | pos== 5,1,13),cstop = cstart + 11)
      all = mutate(all,grid = ifelse(group.x == "R",1,2))
      
      all$file.x = gsub('R', '', all$file.x)
      all$file.x = gsub('h', '', all$file.x)
      all$file.y = gsub('C', '', all$file.y)
      all$file.y = gsub('V', '', all$file.y)

      all = mutate(all, pos1 = ifelse(file.x <= 8, ifelse(file.y <= 12,1,2),ifelse(file.x <= 16,ifelse(file.y <= 12,3,4),ifelse(file.y <= 12,5,6)) ))
      all = left_join(all,f,by=c("index"="index","pos1" = "pos","grid"="grid"))
      all = mutate(all,row = ifelse(file.x == 1 | file.x == 9 | file.x == 17,"A",ifelse(file.x==2| file.x == 10 | file.x == 18,"B",ifelse(file.x==3| file.x == 11 | file.x == 19,"C",ifelse(file.x==4| file.x == 12 | file.x == 20,"D",ifelse(file.x==5| file.x == 13 | file.x == 21,"E",ifelse(file.x==6| file.x == 14 | file.x == 22,"F",ifelse(file.x==7| file.x == 15 | file.x == 23,"G",ifelse(file.x==8| file.x == 16 | file.x == 24,"H","NA")))))))))
      all = mutate(all,column = ifelse(file.y==1 | file.y == 13,1,ifelse(file.y==2 | file.y == 14,2,ifelse(file.y==3 | file.y == 15,3,ifelse(file.y==4 | file.y == 16,4,ifelse(file.y==5 | file.y == 17,5,
                                ifelse(file.y==6 | file.y == 18,6,ifelse(file.y==7 | file.y == 19,7,ifelse(file.y==8 | file.y == 20,8,ifelse(file.y==9 | file.y == 21,9,ifelse(file.y==10 | file.y == 22,10,
                                ifelse(file.y==11 | file.y == 23,11,ifelse(file.y==12 | file.y == 24,12,0)))))))))))))
      all = select(all,name,lib,note.x,mutants,pos,grid,pos1,layer,plate,row,column,enzyme,file.x,group.x,file.y,group.y)
      
      
      combinations = select(all,grid,lib)
      combinations = unique(combinations)
      print(all)
      grids_s = data.frame()
      print(combinations)
      for(i in 1:dim(combinations)[1]){
        aa = filter(all,grid == combinations[i,1],lib == combinations[i,2])
        print(aa)
        write.table(aa,file="temp",sep="\t",row.names=F,col.names=F)
        cmd = "perl sudoku.pl temp > temp_s"
        system(cmd)
        aa_s = read.table("temp_s",header=F,stringsAsFactors = F)
        if(dim(aa_s)[2] == 16){
          aa_s$mutants_s = aa_s$V4
        }
        colnames(aa_s) = c(colnames(aa),"mutants_s")
        
        if(dim(grids_s)[1] == 0){
          grids_s = aa_s
        }else{
          grids_s = rbind(grids_s,aa_s)
        }
      }
      print(grids_s)
      grids_s = left_join(grids_s,data_long,by = c("plate" = "plate","row" = "row","column" = "column"))
      grids_s = mutate(grids_s,pheno = ifelse(plate > 394,"NS",pheno))
      
      grids_s = left_join(grids_s,pos_table,by = "name")
      
      grids_s
    })
    
    overlap_plate <- reactive({
      overlap_plate_data()
    })
    
    output$plate <- DT::renderDataTable({
      DT::datatable(overlap_plate(), extensions = c('ColReorder'), rownames = FALSE, options = list(scrollX = T, dom = 'rltip', colReorder = TRUE,fixedHeader = TRUE),escape = F)
    })
    
    output$download_plateData <- downloadHandler(
      filename = function() {
        paste("table",".csv", sep="")
      },
      
      content = function(file) {
        
        # Write to a file specified by the 'file' argument
        write.table(overlap_plate(), file, sep = "\t",row.names = FALSE)
      }
    )
    ###########statistics output###############
    output$stats <- DT::renderDataTable({
    	DT::datatable(stats_all(), extensions = c('ColReorder'), rownames = FALSE, options = list(scrollX = T, dom = 'rltip', colReorder = TRUE,fixedHeader = TRUE,columnDefs = list(list(className = 'dt-center', targets = 0:5))),escape = F)
    })
    
    stats_all <- reactive({
    	stats_data()
    })
    
    stats_data <- reactive({
    	s=data.frame()
    	plate = overlap_plate()
    	inFile <- input$file1
    	f=as.data.frame(read.table(inFile$datapath,header=T))
			pos_muts = dim(f)[1] * 96
			s[1,1] = pos_muts
			muts = dim(plate)[1]
			dif_muts = length(unique(plate$name))
			cells = length(unique(plate$pos.x))
			plate = group_by(plate, pos.x)
			occ_cell = summarise(plate,count=n())
			single = dim(filter(occ_cell,count==1))[1]
			more = dim(filter(occ_cell,count>1))[1]
			empty = pos_muts - cells
			genic = dim(filter(plate,!is.na(gene)))[1]
			intergenic = dim(filter(plate,is.na(gene)))[1]
			genes = length(unique(plate$gene))-1
			
			s[1,2] = muts
			s[1,3] = dif_muts
			s[1,4] = paste0(paste(cells,round(100 * cells/pos_muts,digits=0),sep = " / ")," %")
			s[1,5] = paste0(paste(empty,round(100 * empty/pos_muts,digits=0),sep = " / ")," %")
			s[1,6] = paste0(paste(single,round(100 * single/pos_muts,digits=0),sep = " / ")," %")
			s[1,7] = more
			s[1,8] = genic
			s[1,9] = genes
			s[1,10] = intergenic
			names(s) = c("pos. mutants","identified mutants","different mutants","occupied cells","empty cells","cells with one mutant","cells with more mutants","genic sites","different genes","intergenic sites")
			s
    })
    
    output$download_stats <- downloadHandler(
    	filename = function() {
    		paste("table",".csv", sep="")
    	},
    	
    	content = function(file) {
    		
    		# Write to a file specified by the 'file' argument
    		write.table(stats_all(), file, sep = "\t",row.names = FALSE)
    	}
    )
})
