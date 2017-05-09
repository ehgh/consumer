library(igraph)
library(plyr)
library(poweRlaw)
library(grid)
library(Rmisc)
library(lattice)
library(ggplot2)


#############################################################################################################
#importin the files
for (i in 2014:2014){
  address <- paste("~/Desktop/research/consumer data/KiltsPanelData/nielsen_extracts 5/HMS/", i, "/Annual_Files/panelists_" , i, ".tsv", sep = "")
  assign(paste("panelists_",i, sep = ""), read.table(address, header = TRUE, sep = "\t"))
}
for (i in 2014:2014){
  address <- paste("~/Desktop/research/consumer data/KiltsPanelData/nielsen_extracts 5/HMS/", i, "/Annual_Files/trips_" , i, ".tsv", sep = "")
  assign(paste("trips_",i, sep = ""), read.table(address, header = TRUE, sep = "\t"))
}
for (i in 2014:2014){
  address <- paste("~/Desktop/research/consumer data/KiltsPanelData/nielsen_extracts 5/HMS/", i, "/Annual_Files/purchases_" , i, ".tsv", sep = "")
  assign(paste("purchases_",i, sep = ""), read.table(address, header = TRUE, sep = "\t"))
}

#products1 <- read.table("~/Desktop/research/consumer data/KiltsPanelData/nielsen_extracts 4/HMS/Master_Files/Latest/products.tsv" , sep = "\t", quote = "", as.is = !StringsAsFactor, skipNul = TRUE)
#products1 <- read.table("~/Desktop/research/consumer data/nielsen_extracts/RMS/Master_Files/Latest/products.tsv" , header = TRUE, sep = "\t", quote = "", as.is = !StringsAsFactor, skipNul = TRUE)
#############################################################################################################
#combining trips and purchases
for (i in 2014:2014){
  #merging lists (Note: some trips may not include any purchases in the purchases list and here we exclude them to create products graph)
  assign(paste("trips_purchases_",i, sep = ""), merge(eval(parse(text = paste("trips_", i, sep = ""))), eval(parse(text = paste("purchases_", i, sep = ""))), by.x = "trip_code_uc", by.y = "trip_code_uc", all.x = FALSE, all.y = TRUE))
  #combining upc+upc_ver_uc into upc_unique
  assign(paste("trips_purchases_",i, sep = ""), transform(eval(parse(text = paste("trips_purchases_", i, sep = ""))), upc_unique = paste(upc, upc_ver_uc, sep = "")))
  assign(paste("trips_purchases_",i, sep = ""), transform(eval(parse(text = paste("trips_purchases_", i, sep = ""))), upc_unique = as.character(upc_unique)))
  assign(paste("trips_purchases_",i, sep = ""), transform(eval(parse(text = paste("trips_purchases_", i, sep = ""))), upc_unique = as.numeric(upc_unique)))
  assign(paste("trips_purchases_data_",i, sep = ""), eval(parse(text = paste("trips_purchases_",i, "[,c(1:9,12:16)]", sep = ""))))
  assign(paste("trips_purchases_data_",i, sep = ""), eval(parse(text = paste("trips_purchases_",i, "[,c(1:2,5,7,16)]", sep = ""))))
}
#############################################################################################################
#calculating number of edges in products graph
temp <- trips_purchases_2014$trip_code_uc
#temp <- trips_purchases_zip_956_2014$trip_code_uc
temp = table(temp)
temp = data.frame(temp)
temp<- transform(temp, temp = as.numeric(temp))
temp<- list(unlist(temp$Freq)*(unlist(temp$Freq)-1)/2)
product_graph_edge_count <- sum(temp[[1]])
average_productPerTrip <- mean(temp$Freq)
#############################################################################################################
#creating products graph
#remove NA store_zip3 rows
listname <- paste("trips_purchases_zip_", i, sep = "")
assign(listname, eval(parse(text = paste("trips_purchases_data_",i,"[!is.na(trips_purchases_data_",i,"$store_zip3),]", sep = ""))))
#filter a specific zipcode
for (zip in 956){
  listname <- paste("trips_purchases_zip_",zip,"_", i, sep = "")
  assign(listname, eval(parse(text = paste("trips_purchases_zip_",i,"[trips_purchases_zip_",i,"$store_zip3 == ", zip, ",]", sep = ""))))
}
#############################################################################################################
#separating list based on store_code_uc
listname <- paste("trips_purchases_zip_",zip,"_", i,"$store_code_uc", sep = "")
store_code_list <- unique(eval(parse(text = listname)))
for (store_code in store_code_list[1]){
  if (store_code%%10 == 0){
    print(store_code)
  }
  #filtering the store_code data
  listname <- paste("trips_purchases_zip_",zip,"_store_",store_code,"_", i, sep = "")
  assign(listname, eval(parse(text = paste("trips_purchases_zip_",i,"[trips_purchases_zip_",i,"$store_code_uc == ", store_code, ",]", sep = ""))))
  #create product graph edgelist
  #this line is not required!
  #trip_code_list <- unique(eval(parse(text = paste(listname, "$trip_code_uc", sep = ""))))
  #parse by trip_code and then add edges from the parsed list  
  temp1 <- matrix(nrow = 0, ncol = 2)
  #split datasets based on trip_code_uc
  temp <- split(eval(parse(text = listname)), f = eval(parse(text = paste(listname, "$trip_code_uc", sep = ""))))
  #add edges to edgelist
  for (trip in 1:length(temp)){
      temp2 <- temp[[trip]]$upc_unique
      if (length(temp2) > 1) {
        temp1 <- rbind(temp1, t(combn(temp2, 2)))
      }
  }
  listname <- paste(listname, "_edgelist", sep = "" )
  #convert to data frame
  assign(listname, as.data.frame(temp1))
  #counting the number of duplicate rows as weight
  temp <- ddply(eval(parse(text = listname)),.(V1,V2),nrow)
  #rename column names to Source and Target to use in Gephi
  eval(parse(text = paste("names(", listname, ") <- c('Source','Target') ", sep = "" )))
  #create the graph
  assign(paste(listname, "_graph", sep = "" ), graph.data.frame(eval(parse(text = listname)), directed = FALSE))
  #assign weights of 1 to edges
  eval(parse(text = paste("E(", listname, "_graph)$weight <- 1", sep = "" )))
  #remove self edges and merge duplicate edges into weights
  assign(paste(listname, "_graph", sep = "" ), simplify(eval(parse(text = paste(listname, "_graph", sep = "" ))), remove.loops = TRUE, edge.attr.comb=list(weight="sum")))
  #convert graph into edgelist with weights
  assign(listname, cbind(as_edgelist(eval(parse(text = paste(listname, "_graph", sep = "" ))), names = TRUE), E(eval(parse(text = paste(listname, "_graph", sep = "" ))))$weight))
  temp10 <- cbind(as_edgelist(eval(parse(text = paste(listname, "_graph", sep = "" ))), names = TRUE), E(eval(parse(text = paste(listname, "_graph", sep = "" ))))$weight)
  #address for the table to be written at
  address <- paste("~/Desktop/research/consumer data/R files/zip 956 store resulotion graphs/", listname, ".tsv", sep = "")
  write.table(eval(parse(text = listname)), address, row.names = FALSE, sep = ",")
  #remove the created objects to free memory
  rm(temp1)
  rm(temp2)
  rm(temp)
  rm(listname)
  listname <- paste("trips_purchases_zip_",zip,"_store_",store_code,"_", i, sep = "")
  rm(listname)
  listname <- paste("trips_purchases_zip_",zip,"_store_",store_code,"_", i, "_graph", sep = "" )
  rm(listname)
  
}


#############################################################################################################
