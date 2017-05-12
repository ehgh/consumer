library(igraph)
library(plyr)
library(poweRlaw)
library(grid)
library(Rmisc)
library(lattice)
library(ggplot2)
library(data.table)

#############################################################################################################
#importin the files
for (i in 2011:2013){
  address <- paste("~/Desktop/research/consumer data/KiltsPanelData/nielsen_extracts 5/HMS/", i, "/Annual_Files/panelists_" , i, ".tsv", sep = "")
  assign(paste("panelists_",i, sep = ""), read.table(address, header = TRUE, sep = "\t"))
}
for (i in 2013:2013){
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
  if (which(store_code_list == store_code)%%10 == 0){
    print(store_code)
  }
  #filtering the store_code data
  listname <- paste("trips_purchases_zip_",zip,"_store_",store_code,"_", i, sep = "")
  assign(listname, eval(parse(text = paste("trips_purchases_zip_",i,"[trips_purchases_zip_",i,"$store_code_uc == ", store_code, ",]", sep = ""))))
  #check if there is only one purchase with one product to continue to next store
  if (length(eval(parse(text = listname)) == 1))
    next
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
  #create the graph
  assign(paste(listname, "_graph", sep = "" ), graph.data.frame(eval(parse(text = listname)), directed = FALSE))
  #assign weights of 1 to edges
  eval(parse(text = paste("E(", listname, "_graph)$weight <- 1", sep = "" )))
  #remove self edges and merge duplicate edges into weights
  assign(paste(listname, "_graph", sep = "" ), simplify(eval(parse(text = paste(listname, "_graph", sep = "" ))), remove.loops = TRUE, edge.attr.comb=list(weight="sum")))
  #convert graph into edgelist with weights
  assign(listname, cbind(as_edgelist(eval(parse(text = paste(listname, "_graph", sep = "" ))), names = TRUE), E(eval(parse(text = paste(listname, "_graph", sep = "" ))))$weight, "Undirected"))
  #rename column names to Source and Target to use in Gephi
  eval(parse(text = paste("colnames(", listname, ") <- c('Source','Target','Weight','Type') ", sep = "" )))
  #address for the table to be written at
  address <- paste("~/Desktop/research/consumer data/R files/zip 956 store resulotion graphs/", listname, ".tsv", sep = "")
  write.table(eval(parse(text = listname)), address, row.names = FALSE, sep = ",")
  #remove the created objects to free memory
  rm(temp1)
  rm(temp2)
  rm(temp)
  listname <- paste("trips_purchases_zip_",zip,"_store_",store_code,"_", i, "_edgelist", sep = "")
  rm(list = listname)
  listname <- paste("trips_purchases_zip_",zip,"_store_",store_code,"_", i, sep = "")
  rm(list = listname)
  listname <- paste("trips_purchases_zip_",zip,"_store_",store_code,"_", i, "_edgelist_graph", sep = "" )
  rm(list = listname)
}
#############################################################################################################
#combining products and purchases
for (i in 2014:2014){
  #merging lists (Note: some trips may not include any purchases in the purchases list and here we exclude them to create products graph)
  assign(paste("products_purchases_",i, sep = ""), merge(products, eval(parse(text = paste("purchases_", i, sep = ""))), by.x = c("upc","upc_ver_uc"), by.y = c("upc","upc_ver_uc"), all.x = FALSE, all.y = TRUE))
  #counting the purchases in each product category/department/module and sort them
  assign(paste("departments_",i, sep = ""), data.frame(sort(table(eval(parse(text = paste("products_purchases_",i, "$department_descr", sep = "")))), decreasing = TRUE)))
  assign(paste("groups_",i, sep = ""), data.frame(sort(table(eval(parse(text = paste("products_purchases_",i, "$product_group_descr", sep = "")))), decreasing = TRUE)))
  assign(paste("modules_",i, sep = ""), data.frame(sort(table(eval(parse(text = paste("products_purchases_",i, "$product_module_descr", sep = "")))), decreasing = TRUE)))
  assign(paste("groups_",i, sep = ""), table(eval(parse(text = paste("products_purchases_",i, "$product_group_descr", sep = "")))))
  assign(paste("modules_",i, sep = ""), table(eval(parse(text = paste("products_purchases_",i, "$product_module_descr", sep = "")))))
  #plot ecdf of purchases per product department/module/group
  department_plot =  
    ggplot(eval(parse(text = paste("departments_",i, sep = ""))), aes(y = cumsum(Freq/sum(Freq)), x = seq(1:length(Var1)))) +
    geom_step() +
    theme_bw() +
    theme(axis.text.x = element_text(size = 7.5, angle = 45,  vjust=1, hjust=1)) +
    theme(axis.title.y = element_text(size = 10)) +
    scale_y_continuous(limits = c(0,1)) +
    scale_x_continuous(breaks = seq(1:length(eval(parse(text = paste("departments_",i,"$Var1", sep = ""))))),  labels = eval(parse(text = paste("departments_",i,"$Var1", sep = "")))) +
    xlab("product department") +
    ylab("CDF of purchases\n per product department") +
    ggtitle(paste("CDF of purchases per product department\n ordered by highest contribution first for year ", i, sep = ""))
  address <- paste("~/Desktop/research/consumer data/plots/CDF_department_", i, ".pdf", sep = "")
  pdf(address, width=6, height=6)
  print(department_plot)
  dev.off()
  
  L <- length(eval(parse(text = paste("modules_",i,"$Var1", sep = ""))))
  #Number of the points to be shown on x axis
  N <- 30
  modules_plot =  
    ggplot(eval(parse(text = paste("modules_",i, sep = ""))), aes(y = cumsum(Freq/sum(Freq)), x = seq(1:length(Var1)))) +
    geom_step() +
    theme_bw() +
    theme(axis.text.x = element_text(size = 7.5, angle = 45,  vjust=1, hjust=1)) +
    theme(axis.title.y = element_text(size = 10)) +
    scale_y_continuous(limits = c(0,1)) +
    scale_x_continuous(breaks = eval(parse(text = "seq(from = 1, to = L , length.out = N)")),  labels = eval(parse(text = paste("modules_",i,"$Var1[seq(from = 1, to = L, length.out = N)]", sep = "")))) +
    xlab("product modules") +
    ylab("CDF of purchases\n per product modules") +
    ggtitle(paste("CDF of purchases per product modules\n ordered by highest contribution first for year ", i, sep = ""))
  address <- paste("~/Desktop/research/consumer data/plots/CDF_modules_", i, ".pdf", sep = "")
  pdf(address, width=6, height=6)
  print(modules_plot)
  dev.off()
  
  L <- length(eval(parse(text = paste("groups_",i,"$Var1", sep = ""))))
  #Number of the points to be shown on x axis
  N <- 30
  groups_plot =  
    ggplot(eval(parse(text = paste("groups_",i, sep = ""))), aes(y = cumsum(Freq/sum(Freq)), x = seq(1:length(Var1)))) +
    geom_step() +
    #ggplot(eval(parse(text = paste("products_purchases_",i, sep = ""))), aes(department_code)) +
    #stat_ecdf() + 
    # scale_x_log10() +
    theme_bw() +
    theme(axis.text.x = element_text(size = 7.5, angle = 45,  vjust=1, hjust=1)) +
    theme(axis.title.y = element_text(size = 10)) +
    scale_y_continuous(limits = c(0,1)) +
    scale_x_continuous(breaks = eval(parse(text = "seq(from = 1, to = L , length.out = N)")),  labels = eval(parse(text = paste("groups_",i,"$Var1[seq(from = 1, to = L, length.out = N)]", sep = "")))) +
    #theme(legend.justification=c(1,0), legend.position=c(1,0)) +
    # theme(legend.position = c(.25, .8)) +
    xlab("product groups") +
    ylab("CDF of purchases\n per product groups") +
    ggtitle(paste("CDF of purchases per product groups\n ordered by highest contribution first for year ", i, sep = ""))
  address <- paste("~/Desktop/research/consumer data/plots/CDF_groups_", i, ".pdf", sep = "")
  pdf(address, width=6, height=6)
  print(groups_plot)
  dev.off()
}
#############################################################################################################
#find common panelists throughout years
common_panelists <- panelists_2011$household_code
for (i in 2012:2014){
  common_panelists <- intersect(common_panelists, eval(parse(text = paste("panelists_", i, "$household_code", sep = ""))))  
}
#filtering panelists according to common panelists
for (i in 2011:2014){
  assign(paste("panelists_",i, sep = ""), eval(parse(text = paste("panelists_", i, "[panelists_", i, "$household_code %in% common_panelists,]", sep = ""))))
}
#filtering trips according to common panelists
for (i in 2011:2014){
  assign(paste("trips_",i, sep = ""), eval(parse(text = paste("as.data.table(trips_", i, "[trips_", i, "$household_code %in% common_panelists,])", sep = ""))))
}
#count the number of trips for each household and the amount of spendature per year for that household
for (i in 2011:2014){
  assign(paste("trips_sum_",i, sep = ""), eval(parse(text = paste("trips_", i, "[,list(total = sum(total_spent), num = .N), by = household_code]", sep = ""))))
}
#merge trip_sums to calculate change in spendature and employment status













#############################################################################################################


gc()














