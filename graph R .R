library(igraph)
library(plyr)
library(poweRlaw)
library(grid)
library(lattice)
library(Rmisc)
library(ggplot2)
library(data.table)
library(MASS)
library(survival)
library(fitdistrplus)
library(logspline)

#############################################################################################################
#importing the files
for (i in 2014:2014){
  address <- paste("~/Desktop/research/consumer data/KiltsPanelData/nielsen_extracts 5/HMS/", i, "/Annual_Files/panelists_" , i, ".tsv", sep = "")
  assign(paste("panelists_",i, sep = ""), read.table(address, header = TRUE, sep = "\t"))
}
for (i in 2014:2014){
  address <- paste("~/Desktop/research/consumer data/KiltsPanelData/nielsen_extracts 5/HMS/", i, "/Annual_Files/trips_" , i, ".tsv", sep = "")
  assign(paste("trips_",i, sep = ""), read.table(address, header = TRUE, sep = "\t"))
}
for (i in 2013:2013){
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
  #assign(paste("trips_purchases_data_",i, sep = ""), eval(parse(text = paste("trips_purchases_",i, "[,c(1:9,12:16)]", sep = ""))))
  #assign(paste("trips_purchases_data_",i, sep = ""), eval(parse(text = paste("trips_purchases_",i, "[,c(1:2,5,7,16)]", sep = ""))))
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
    geom_segment(aes(x = 0.2*10, y = 0.1, xend = 0.2*10, yend = 0.9), color = "red") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10, angle = 45,  vjust=1, hjust=1),axis.text.y = element_text(size = 13)) +
    theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,length.out = 6)) +
    scale_x_continuous(breaks = seq(1:length(eval(parse(text = paste("departments_",i,"$Var1", sep = ""))))),  labels = eval(parse(text = paste("departments_",i,"$Var1", sep = "")))) +
    #ggtitle(paste("Cumulative distribution of purchases per product departments\n ordered by contribution in year ", i, sep = ""))
    xlab("Product departments ordered by highest contribution first") +
    ylab("Cumulative distribution of purchases\n per product departments")
  address <- paste("~/Desktop/research/consumer data/plots/CDF_department_", i, ".pdf", sep = "")
  pdf(address, width=6.5, height=6)
  print(department_plot)
  dev.off()
  
  L <- length(eval(parse(text = paste("modules_",i,"$Var1", sep = ""))))
  #Number of the points to be shown on x axis
  N <- 30
  modules_plot =  
    ggplot(eval(parse(text = paste("modules_",i, sep = ""))), aes(y = cumsum(Freq/sum(Freq)), x = seq(1:length(Var1)))) +
    geom_step() +
    geom_segment(aes(x = 0.2*L, y = 0.1, xend = 0.2*L, yend = 0.9), color = "red") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10, angle = 45,  vjust=1, hjust=1),axis.text.y = element_text(size = 13)) +
    theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,length.out = 6)) +
    #labeling the x-axis by sampled modules
    #scale_x_continuous(breaks = eval(parse(text = "seq(from = 1, to = L , length.out = N)")),  labels = eval(parse(text = paste("modules_",i,"$Var1[seq(from = 1, to = L, length.out = N)]", sep = "")))) +
    #labeling the x-axis by normalized interval [0,1]
    scale_x_continuous(breaks = eval(parse(text = "seq(from = 1, to = L , length.out = N)")),  labels = round(seq(0,L,length.out = N), digits = 0)) +
    #ggtitle(paste("Cumulative distribution of purchases per product modules\n ordered by contribution in year ", i, sep = ""))
    xlab("Product modules ordered by highest contribution first") +
    ylab("Cumulative distribution of purchases per product modules")
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
    geom_segment(aes(x = 0.2*L, y = 0.1, xend = 0.2*L, yend = 0.9), color = "red") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10, angle = 45,  vjust=1, hjust=1),axis.text.y = element_text(size = 13)) +
    theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,length.out = 6)) +
    #labeling the x-axis by sampled groups
    #scale_x_continuous(breaks = eval(parse(text = "seq(from = 1, to = L , length.out = N)")),  labels = eval(parse(text = paste("groups_",i,"$Var1[seq(from = 1, to = L, length.out = N)]", sep = "")))) +
    #labeling the x-axis by normalized interval [0,1]
    scale_x_continuous(breaks = eval(parse(text = "seq(from = 1, to = L , length.out = N)")),  labels = round(seq(0,L,length.out = N), digits = 0)) +
    #ggtitle(paste("Cumulative distribution of purchases per product groups\n ordered by contribution in year ", i, sep = ""))
    xlab("Product groups ordered by highest contribution first") +
    ylab("Cumulative distribution of purchases per product groups")
  address <- paste("~/Desktop/research/consumer data/plots/CDF_groups_", i, ".pdf", sep = "")
  pdf(address, width=6, height=6)
  print(groups_plot)
  dev.off()
}
#############################################################################################################
#merge all different hours employment into 1 and remove families without male or female head
for (i in 2011:2014){
  assign(paste("panelists_",i, sep = ""), eval(parse(text = paste("panelists_", i, "[!(panelists_", i, "$male_head_employment == 0),]", sep = ""))))
  eval(parse(text = paste("panelists_", i, "$male_head_employment[which(panelists_", i, "$male_head_employment == 2 | panelists_", i, "$male_head_employment == 3)] <- 1", sep = "")))
  assign(paste("panelists_",i, sep = ""), eval(parse(text = paste("panelists_", i, "[!(panelists_", i, "$female_head_employment == 0),]", sep = ""))))
  eval(parse(text = paste("panelists_", i, "$female_head_employment[which(panelists_", i, "$female_head_employment == 2 | panelists_", i, "$female_head_employment == 3)] <- 1", sep = "")))
}
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
#filtering trips_purchases according to common panelists
for (i in 2011:2012){
  assign(paste("trips_purchases_",i, sep = ""), eval(parse(text = paste("as.data.table(trips_purchases_", i, "[trips_purchases_", i, "$household_code %in% common_panelists,])", sep = ""))))
}
#count the number of trips for each household and the amount of spendature per year for that household
for (i in 2011:2014){
  assign(paste("trips_sum_",i, sep = ""), eval(parse(text = paste("trips_", i, "[,list(total = sum(total_spent), num = .N), by = household_code]", sep = ""))))
}
#count the average of quantities for each household and the amount of total coupon per year for that household
for (i in 2011:2012){
  assign(paste("trips_purchases_sum_",i, sep = ""), eval(parse(text = paste("trips_purchases_", i, "[,list(coupon = sum(coupon_value), quantity = mean(quantity)), by = household_code]", sep = ""))))
}

#merge trip_sums to calculate change in spendature
assign("trips_sum_11_12", merge(eval(parse(text = paste("trips_sum_", "2011", sep = ""))), eval(parse(text = paste("trips_sum_", "2012", sep = ""))), by = "household_code",suffixes = c("2011","2012")))
trips_sum_11_12$total_spent_change <- (trips_sum_11_12$total2012/trips_sum_11_12$num2012) - (trips_sum_11_12$total2011/trips_sum_11_12$num2011)
#merge trip_purchases_sums to calculate change in coupon and quantity
assign("trips_purchases_sum_11_12", merge(eval(parse(text = paste("trips_purchases_sum_", "2011", sep = ""))), eval(parse(text = paste("trips_purchases_sum_", "2012", sep = ""))), by = "household_code",suffixes = c("2011","2012")))
trips_purchases_sum_11_12$quantity_change <- trips_purchases_sum_11_12$quantity2012 - trips_purchases_sum_11_12$quantity2011
trips_purchases_sum_11_12$coupon_change <- trips_purchases_sum_11_12$coupon2012 - trips_purchases_sum_11_12$coupon2011
#merge panelists to calculate change in employment status
assign("panelists_13_14", merge(eval(parse(text = paste("panelists_", "2013", sep = ""))), eval(parse(text = paste("panelists_", "2014", sep = ""))), by = "household_code",suffixes = c("2013","2014")))
assign("panelists_13_14", transform(eval(parse(text = "panelists_13_14")), employment = paste(male_head_employment2013, male_head_employment2014, female_head_employment2013, female_head_employment2014, sep = "")))
assign("panelists_13_14", transform(eval(parse(text = "panelists_13_14")), employment = as.character(employment)))
assign("panelists_13_14", transform(eval(parse(text = "panelists_13_14")), employment = as.numeric(employment)))
assign("panelists_13_14", eval(parse(text = paste("panelists_13_14[,c(1,116)]", sep = ""))))
assign("trips_sum_11_12", eval(parse(text = paste("trips_sum_11_12[,c(1,6)]", sep = ""))))
assign("trips_purchases_sum_11_12", eval(parse(text = paste("trips_purchases_sum_11_12[,c(1,6,7)]", sep = ""))))
#merge change in spendature and change in employment into one dataframe
assign("panelists_13_14", merge(eval(parse(text = "panelists_13_14")), eval(parse(text = "trips_sum_11_12")), by = "household_code"))
assign("panelists_13_14", merge(eval(parse(text = "panelists_13_14")), eval(parse(text = "trips_purchases_sum_11_12")), by = "household_code"))
#separate files for change in employment of only male/female head
apanelists_13_14 <-  panelists_13_14[which(panelists_13_14$employment %in% c("1111","1911","1999","9111","9199")),]
apanelists_13_14 <-  panelists_13_14[which(panelists_13_14$employment %in% c("1111","1119","9919","1191","9991")),]
table(apanelists_13_14$employment)

apanelists_13_14 <- as.table(apanelists_13_14)
#plot change in purchasin quantity for change in employment groups
employment_plot =  
  ggplot(apanelists_13_14, aes(x = quantity_change, color = household_code)) +
  stat_ecdf() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7.5, angle = 45,  vjust=1, hjust=1)) +
  theme(axis.title.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_continuous(limits = c(-0.25,0.25)) +
  xlab("Average quantity change from 2011 to 2012") +
  ylab("CDF") +
  ggtitle(paste("CDF of average quantity change from 2011 to 2012\n grouped by change in male head emplyment\n status", "", sep = ""))
employment_plot
address <- paste("~/Desktop/research/consumer data/plots/CDF_employment_quantity_male_change1_", i, ".pdf", sep = "")
pdf(address, width=6, height=6)
print(employment_plot)
dev.off()
rm(employment_plot)

#plot change in expenditure for change in employment groups
employment_plot =  
  ggplot(panelists_13_14, aes(total_spent_change, color = employment)) +
  stat_ecdf() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7.5, angle = 45,  vjust=1, hjust=1)) +
  theme(axis.title.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  #scale_x_continuous(limits = c(-0.25,0.25)) +
  xlab("Total expenditure change from 2011 to 2012") +
  ylab("CDF") +
  ggtitle(paste("CDF of total expenditure change from 2011 to 2012\n grouped by change in male head emplyment\n status", "", sep = ""))
employment_plot
address <- paste("~/Desktop/research/consumer data/plots/CDF_employment_total_spent_male_change1_", i, ".pdf", sep = "")
pdf(address, width=6, height=6)
print(employment_plot)
dev.off()
rm(employment_plot)
panelists_2014_weka <- panelists_2014[,c(5,8,10:17,20,23,26)]
panelists_2014_weka <- panelists_2014[,c(1,6,9,10,11,12,13,14,15)]
panelists_2013_weka <- panelists_2013[,c(1,6,9,10,11,12,13,14,15)]
write.csv(panelists_2013_weka, "~/Desktop/research/consumer data/weka files/panelists_2013_weka.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.csv(panelists_2014_weka, "~/Desktop/research/consumer data/weka files/panelists_2014_weka.csv", sep = ",", col.names = TRUE, row.names = FALSE)
w = cor(panelists_2014_weka, use = 'pairwise.complete.obs')
w = cor(panelists_2014_weka)
w[abs(w[,9]) > 0.3,9]
w[abs(w) > 0.5 & w < 1]
w = cor(panelists_2013_weka, use = 'pairwise.complete.obs')
w = cor(panelists_2013_weka)
w[abs(w[,9]) > 0.1,9]
w[,c(9,10)]
#############################################################################################################
#finding difference in product graph for different stores
#importing the graphs
#importing the files
store_list_graph_stat <- as.data.frame(store_code_list[1:215])
#for (i in store_code_list[1:215]){
for (i in c(2073128)){
  address <- paste("~/Desktop/research/consumer data/R files/zip 956 store resulotion graphs/trips_purchases_zip_956_store_", i, "_2014_edgelist.tsv" , sep = "")
  assign(paste("trips_purchases_zip_956_store_", i, "_2014_edgelist", sep = ""), read.table(address, header = TRUE, sep = ","))
  assign(paste("trips_purchases_zip_956_store_", i, "_2014_edgelist", sep = ""), as.data.frame(eval(parse(text = paste("trips_purchases_zip_956_store_", i, "_2014_edgelist[,1:3]", sep = "")))))
  assign(paste("trips_purchases_zip_956_store_", i, "_2014_graph", sep = ""), graph.data.frame(eval(parse(text = paste("trips_purchases_zip_956_store_", i, "_2014_edgelist", sep = ""))), directed = FALSE))
  rm(list = paste("trips_purchases_zip_956_store_", i, "_2014_edgelist", sep = ""))
  store_list_graph_stat$vertices[store_list_graph_stat$store_code_list == i] = vcount(eval(parse(text = paste("trips_purchases_zip_956_store_", i, "_2014_graph", sep = ""))))
  store_list_graph_stat$edges[store_list_graph_stat$store_code_list == i] = ecount(eval(parse(text = paste("trips_purchases_zip_956_store_", i, "_2014_graph", sep = ""))))
  store_list_graph_stat$diff[store_list_graph_stat$store_code_list == i] = ecount(difference(trips_purchases_zip_956_store_821690_2014_graph,eval(parse(text = paste("trips_purchases_zip_956_store_", i, "_2014_graph", sep = "")))))/17283
}

store_list_graph_stat_all <- matrix(nrow = 215, ncol = 215)
for(i in 1:215){
  for(j in c(1:215)){
    store_list_graph_stat_all[i,j] = ecount(difference(eval(parse(text = paste("trips_purchases_zip_956_store_", store_code_list[i], "_2014_graph", sep = ""))),eval(parse(text = paste("trips_purchases_zip_956_store_", store_code_list[j], "_2014_graph", sep = "")))))/store_list_graph_stat$edges[i]
  }
  print(i)
}
store_list_graph_CC <- matrix(nrow = 215, ncol = 1)

for(j in c(1:215)){
  store_list_graph_CC[j] = transitivity(eval(parse(text = paste("trips_purchases_zip_956_store_", store_code_list[i], "_2014_graph", sep = ""))))
}
ggplot(data = as.data.frame(table(store_list_graph_stat_all1)), aes(x = Var1)) + geom_histogram(aes(y = ..count..))
ggplot(data = as.data.frame(table(store_list_graph_CC)), aes(x = Var1)) + geom_histogram(aes(y = ..count..))

#############################################################################################################
#combining upc and upc)ver)uc in products list
products1 <-products
assign("products1", transform(products1, upc_unique = paste(upc, upc_ver_uc, sep = "")))
assign("products1", transform(products1, upc_unique = as.character(upc_unique)))
assign("products1", transform(products1, upc_unique = as.numeric(upc_unique)))
#measuring the centrality of nodes in product graph
graph <- eval(parse(text = paste("trips_purchases_zip_956_store_", i, "_2014_graph", sep = "")))
store_betweenness <- betweenness(graph, v = V(graph), directed = FALSE, weights = E(graph)$weight)
top_between <- products1[products1$upc_unique %in% names(tail(sort(store_betweenness),10)),]
store_degree <- strength(graph, v = V(graph), weights = E(graph)$weight)
top_degree <- products1[products1$upc_unique %in% names(tail(sort(store_degree),10)),]
store_closeness <- closeness(graph, v = V(graph), weights = E(graph)$weight)
top_closeness <- products1[products1$upc_unique %in% names(tail(sort(store_closeness),10)),]
store_page_rank <- page_rank(graph, v = V(graph), directed = FALSE, weights = E(graph)$weight)
top_page_rank <- products1[products1$upc_unique %in% names(tail(sort(store_page_rank$vector),10)),]
#store_ <- (graph, v = V(graph), weights = E(graph)$weight)
#top_ <- products1[products1$upc_unique %in% names(tail(sort(store_),10)),]


#############################################################################################################
#creating the bipartite household product graph
#############################################################################################################
#applying pareto rule..keeping top 20% of modules
assign(paste("modules_",i, sep = ""), data.frame(sort(table(eval(parse(text = paste("products_purchases_",i, "$product_module_code", sep = "")))), decreasing = TRUE)))
L <- length(eval(parse(text = paste("modules_",i,"$Freq", sep = ""))))
assign(paste("modules_",i,"_20P", sep = ""), eval(parse(text = paste("modules_",i, "[floor(0.2*L):floor(0.21*L),]", sep = ""))))
assign(paste("Products_",i,"_20P", sep = ""), eval(parse(text = paste("products[products$product_module_code %in% modules_",i,"_20P$Var1 ,c(1:3,5)]", sep = ""))))
assign(paste("Products_",i,"_20P", sep = ""), transform(eval(parse(text = paste("Products_",i,"_20P", sep = ""))), upc_unique = paste(upc, upc_ver_uc, sep = "")))
assign(paste("Products_",i,"_20P", sep = ""), transform(eval(parse(text = paste("Products_",i,"_20P", sep = ""))), upc_unique = as.character(upc_unique)))
assign(paste("Products_",i,"_20P", sep = ""), transform(eval(parse(text = paste("Products_",i,"_20P", sep = ""))), upc_unique = as.numeric(upc_unique)))
assign(paste("trips_purchases_",i,"_20P", sep = ""), eval(parse(text = paste("trips_purchases_",i,"[trips_purchases_",i,"$upc_unique %in% Products_",i,"_20P$upc_unique ,]", sep = ""))))

household_rand <- unique(trips_purchases_2014_20P$household_code)
household_rand <- household_rand[1:3]
assign(paste("trips_purchases_",i,"_20P1", sep = ""), eval(parse(text = paste("trips_purchases_",i,"_20P[trips_purchases_",i,"_20P$household_code %in% household_rand,]", sep = ""))))


assign(paste("household_product_",i, sep = ""), eval(parse(text = paste("trips_purchases_",i,"_20P1[,c(2,16)]", sep = ""))))
assign(paste("household_product_",i, sep = ""), eval(parse(text = paste("ddply(household_product_",i,",.(household_code, upc_unique),nrow)", sep = ""))))
#assign(paste("household_product_",i, sep = ""), eval(parse(text = paste("sapply(household_product_",i,",as.character)", sep = ""))))
assign(paste("household_store_",i, sep = ""), eval(parse(text = paste("trips_purchases_",i,"_20P1[,c(2,5)]", sep = ""))))
assign(paste("household_store_",i, sep = ""), eval(parse(text = paste("ddply(household_store_",i,",.(household_code, store_code_uc),nrow)", sep = ""))))
assign(paste("household_store_",i, sep = ""), eval(parse(text = paste("household_store_",i,"[!(household_store_",i,"$store_code_uc==0),]", sep = ""))))
#assign(paste("household_store_",i, sep = ""), eval(parse(text = paste("sapply(household_store_",i,",as.character)", sep = ""))))
assign(paste("household_product_graph",i, sep = ""), eval(parse(text = paste("rbind(household_product_",i,",household_store_",i,")", sep = ""))))

write.csv(household_product_2014, "~/Desktop/research/consumer data/R files/household_product_2014.csv", sep = ",", col.names = c("household_code","upc","weight"), row.names = FALSE)
write.csv(household_store_2014, "~/Desktop/research/consumer data/R files/household_store_2014.csv", sep = ",", col.names = TRUE, row.names = FALSE)


graph <- graph.data.frame(household_store_2014, directed = FALSE)
E(graph)$weight <- household_store_2014$V1
V(graph)$type <- V(graph)$name %in% household_store_2014[,1]

graph <- graph.data.frame(household_product_2014, directed = FALSE)
E(graph)$weight <- household_product_2014$V1
V(graph)$type <- V(graph)$name %in% household_product_2014[,1]

l <- layout.bipartite(graph)
plot(graph, layout = l[, c(2,1)], vertex.color="red",vertex.size = 1,
     edge.width = 2.5, vertex.label = NA)

#############################################################################################################
#filtering pareto products
PP <- read.table('~/Desktop/research/consumer data/R files/zip 956 store resulotion graphs/trips_purchases_zip_956_store_8376197_2014_edgelist.tsv', header = TRUE, sep = ",")
assign("PP", eval(parse(text = paste("PP[PP$Source %in% Products_",i,"_20P$upc_unique & PP$Target %in% Products_",i,"_20P$upc_unique,]", sep = ""))))
PP$SourceName <- Products_2014_20P[match(PP$Source, Products_2014_20P$upc_unique),3]
PP$TargetName <- Products_2014_20P[match(PP$Target, Products_2014_20P$upc_unique),3]
PP$SourceName1 <- Products_2014_20P[match(PP$Source, Products_2014_20P$upc_unique),4]
PP$TargetName1 <- Products_2014_20P[match(PP$Target, Products_2014_20P$upc_unique),4]
PP <- PP[,c(5,6,3,4)]
PP <- ddply(PP,.(SourceName1,TargetName1),summarise, Weight = sum(Weight))
PP <- PP[!(PP$SourceName1==PP$TargetName1),]
PP$Type <- "Undirected"
colnames(PP) <- c('Source','Target','Weight','Type')
write.csv(PP, "~/Desktop/research/consumer data/R files/PP1.csv", sep = ",", row.names = FALSE)
#############################################################################################################
#Finding the statistics of purchasing in trips_purchases file
#Basket size distribution
temp <- as.data.table(trips_purchases_2014[,c(1,2,3,4,5,7,16)])
temp2 <- temp[ , basket := .N, by = trip_code_uc]
temp3 <- temp2[!duplicated(temp2[,1]),]
#this will do the same as above but only keeps counts and not rows
temp4 <- count(temp, c('trip_code_uc'))
#plotting teh distribution
all_stores_basket_plot =  
  ggplot(temp3, aes(x = basket, color = household_code)) +
  stat_ecdf() +
  theme_bw() +
  #theme(axis.text.x = element_text(size = 7.5, angle = 45,  vjust=1, hjust=1)) +
  #theme(axis.title.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_continuous(limits = c(1, 100)) +
  xlab("Basket Size") +
  ylab("CDF") +
  ggtitle("Basket Size Distribution Over All Stores in 2014")
all_stores_basket_plot
address <- "~/Desktop/research/consumer data/plots/Basket Size Distribution Over All Stores in 2014.pdf"
pdf(address, width=6, height=6)
print(all_stores_basket_plot)
dev.off()
rm(all_stores_basket_plot)
#check what distribution fits the data most
temp5 <- temp3$basket[temp3$basket<100]
descdist(temp5)
fit.lognorm <- fitdist(temp5 , "lnorm")
plot(fit.lognorm)
fit.weibull <- fitdist(temp5 , "weibull")
plot(fit.weibull)
#############################################################################################################
#Finding inter-trip time
household_list <- unique(trips_2014$household_code)
temp3 <- matrix(nrow = 0, ncol = 1)
for (i in 1:length(household_list)){
  if (i%%1000 == 0){
    print(i)
  }
  temp <- trips_2014$purchase_date[trips_2014$household_code == household_list[i]]
  temp2 <- strptime(sort(temp), format = "%Y-%m-%d")
  temp3 <- rbind(temp3, as.matrix(round(diff(temp2)/86400, digits = 0)))
}
inter_trip_time <- as.data.frame(temp3)
#similar code as above but faster..needs some fixing
temp <- as.data.table(trips_2014[,c(2,3)])
temp2 <- temp[ , inter_trip_time := round(diff(strptime(sort(purchase_date), format = "%Y-%m-%d"))/86400, digits = 0), by = household_code]
V1 <- temp2$inter_trip_time
inter_trip_time <- as.data.frame(V1)
#plotting distribution
inter_trip_time_plot =  
  ggplot(inter_trip_time, aes(x = V1)) +
  stat_ecdf() +
  theme_bw() +
  #theme(axis.text.x = element_text(size = 7.5, angle = 45,  vjust=1, hjust=1)) +
  #theme(axis.title.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_continuous(limits = c(1, 122)) +
  xlab("Inter Trip Time") +
  ylab("CDF") +
  ggtitle("Inter Trip Time Distribution Over All Stores in 2014")
inter_trip_time_plot
address <- "~/Desktop/research/consumer data/plots/Inter Trip Time Distribution Over All Stores in 2014.pdf"
pdf(address, width=6, height=6)
print(inter_trip_time_plot)
dev.off()
rm(inter_trip_time_plot)
#fit distribution to inter trip time
inter_trip_time <- as.numeric(inter_trip_time$V1)
descdist(inter_trip_time)
fit.lognorm <- fitdist(inter_trip_time , "lnorm")
plot(fit.lognorm)
fit.weibull <- fitdist(inter_trip_time , "weibull")
plot(fit.weibull)
#number of costumers per store
temp <- as.data.table(trips_2014[trips_2014$store_code_uc != 0,])
temp2 <- temp[ , count := .N, by = list(store_code_uc, purchase_date)]
temp3 <- temp2[,c(3,5,10)]
temp4 <- temp3[!duplicated(temp3),]
count <- temp4$count
costumers_day_store <- as.data.frame(count)
#plotting distribution
costumers_day_store_plot =  
  ggplot(costumers_day_store, aes(x = count)) +
  stat_ecdf() +
  theme_bw() +
  #theme(axis.text.x = element_text(size = 7.5, angle = 45,  vjust=1, hjust=1)) +
  #theme(axis.title.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_continuous(limits = c(1, 20)) +
  xlab("Constumers Per Day Per Store") +
  ylab("CDF") +
  ggtitle("Constumers Per Day Per Store Distribution Over All Stores in 2014")
costumers_day_store_plot
address <- "~/Desktop/research/consumer data/plots/costumers day store Distribution Over All Stores in 2014.pdf"
pdf(address, width=6, height=6)
print(costumers_day_store_plot)
dev.off()
rm(costumers_day_store_plot)
#fit distribution to inter trip time
descdist(costumers_day_store$count)
fit.lognorm <- fitdist(costumers_day_store , "lnorm")
plot(fit.lognorm)
fit.weibull <- fitdist(costumers_day_store$count , "weibull")
plot(fit.weibull)
#############################################################################################################
#product entropy for users
temp <- as.data.table(trips_purchases_2014)
#temp$C <- unique(temp, by = c("household_code", "upc_unique"))
temp2 <- temp[ , count := .N, by = list(household_code, upc_unique)]
temp3 <- temp2[ , trip := .N, by = list(household_code, trip_code_uc)]
temp3$frac <- temp3$count/temp3$trip
temp4 <- temp3[!duplicated(temp3[,c(2,16)]),]
temp5 <- temp4[ , C := .N, by = household_code]
temp6 <- temp5[ , entropy := -sum(frac*log(frac))/log(C), by = household_code]
temp6$entropy[is.nan(temp6$entropy)] <- 1
temp7 <- unique(temp6, by ="household_code")
entropy <- temp7$entropy
costumer_product_entropy <- as.data.frame(entropy)
#plotting distribution
costumer_product_entropy_plot =  
  ggplot(costumer_product_entropy, aes(x = entropy)) +
  stat_ecdf() +
  theme_bw() +
  #theme(axis.text.x = element_text(size = 7.5, angle = 45,  vjust=1, hjust=1)) +
  #theme(axis.title.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_continuous(limits = c(1, 20)) +
  xlab("Product Entropy") +
  ylab("CDF") +
  ggtitle("Constumers Product Entropy Distribution Over All Stores in 2014")
costumer_product_entropy_plot
address <- "~/Desktop/research/consumer data/plots/Entropy Distribution Over All Stores in 2014.pdf"
pdf(address, width=6, height=6)
print(costumer_product_entropy_plot)
dev.off()
rm(costumer_product_entropy_plot)
#fit distribution to inter trip time
descdist(costumer_product_entropy$entropy)
fit.lognorm <- fitdist(costumer_product_entropy$entropy , "lnorm")
plot(fit.lognorm)
fit.weibull <- fitdist(costumer_product_entropy$entropy , "weibull")
plot(fit.weibull)



#############################################################################################################
#############################################################################################################

gc()






