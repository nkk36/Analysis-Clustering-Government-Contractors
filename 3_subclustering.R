# Setup ====

# Load packages
library(data.table)
library(dplyr)
library(magrittr)
library(stringi)
library(Matrix)
library(irlba)
library(ggplot2)
library(matrixStats)
library(MASS)

# Load functions
source("R/Transform_to_Percentage.R")
source("R/Transform_by_Capping.R")
source("R/Transform_Negative_Values.R")

# Load data
load("data/NAICS_2D_Clusters_2_50.RData")
naics2D = read.csv("data/lookup_naics2D.csv")
naics2D_vendors = read.csv("data/total_dollars_obligated_naics2D_duns_vendor_names.csv",
                           colClasses = "character")

# Set parameters to subset initial clustering on ====
Industry = "Professional_Services" #"Professional_Services"
N_Clusters = 13
#Cluster = unique(naics2D$Column[naics2D$Desc == Industry])
Subcategory = "NAICS"
N_Clusters_subcategory = 15

# Data Processing ====

# Assign vendor to cluster
naics2D_vendors$cluster = Cluster_KMeans[[N_Clusters - 1]]$cluster
centers = as.data.frame(Cluster_KMeans[[N_Clusters - 1]]$centers)

# Get all vendors classified into given industry
cluster = which.max(centers[,Industry])
companies = naics2D_vendors[naics2D_vendors$cluster == cluster,]
rownames(companies) = 1:nrow(companies)

n = fread("data/dobl_actions_by_duns.csv", colClasses = "character")
companies = left_join(x = companies,
                      y = n,
                      by = "parentdunsnumber")

companies$total_dobl = as.numeric(companies$total_dobl)
companies$total_actions = as.numeric(companies$total_actions)

if (Subcategory == "NAICS"){
  
  # Read pre-processed data
  df = read.csv("data/percentages_naics6D_indices_values.csv")

  # Make into matrix and convert to data frame
  matrix = sparseMatrix(i = df$row, j = df$col, x = df$values)
  matrix = as.matrix(matrix)
  df_for_cp = as.data.frame(matrix)
  
  # Read in duns and vendor names lookup table for naics6D
  vendors = read.csv("data/total_dollars_obligated_naics6D_duns_vendor_names.csv",
                     colClasses = "character")
  df_for_cp = cbind(vendors, df_for_cp )
  df_for_cp  = as.data.frame(df_for_cp )
  df_for_cp  = df_for_cp [df_for_cp $parentdunsnumber %in% companies$parentdunsnumber,]
  temp = fread("data/total_dollars_obligated_naics6D_clustering_data.csv",
               colClasses = "character")
  colnames(df_for_cp)[3:ncol(df_for_cp)] = colnames(temp)
  df_for_cp = df_for_cp[,3:ncol(df_for_cp)]
  
  Clusters = 2:50
  Cluster_KMeans_naics6D = list()

  for (i in Clusters) {
    # Set seed
    set.seed(123)

    # Perform K-Means clustering with 13 groups
    temp_kmeans = KMeans_rcpp(data = df_for_cp,
                              clusters = i)
    #temp_kmeans = kmeans(x = df_for_cp, centers = i, nstart=50, iter.max = 15)
    Cluster_KMeans_naics6D[[i - 1]] = temp_kmeans
  }
  
  Filename = paste("data/NAICS_6D_", Industry, ".RData", sep = "")
  load(Filename)
  
  clusters = Cluster_KMeans_naics6D[[N_Clusters_subcategory - 1]]$cluster
  companies$subcluster = clusters
  companies[which(substr(companies$vendorname,1,4) == "BOOZ"),]
  companies[which(companies$vendorname == "ANALYTIC SERVICES INC"),]
  Subcluster = 10 # Choose subcluster based on the two above lines
  c = Cluster_KMeans_naics6D[[N_Clusters_subcategory-1]]
  t = as.data.frame(c$centers)
  t2  = t[Subcluster,which(t[Subcluster,] != 0)]
  plot(1:length(t2), sort(t2))
  t3 = melt(t2)
  t3 = arrange(t3, desc(value))
  
  remove(df, matrix, temp, vendors, centers, Cluster_KMeans, naics2D, naics2D_vendors)
  
  
  # Clusters = 2:50
  # Cluster_KMeans_naics6D = list()
  # 
  # for (i in Clusters) {
  #   # Set seed
  #   set.seed(123)
  #   
  #   # Perform K-Means clustering with 13 groups
  #   temp_kmeans = kmeans(x = df_for_cp, centers = i, nstart=50, iter.max = 15)
  #   Cluster_KMeans_naics6D[[i - 1]] = temp_kmeans
  # }
  # 
  # remove(temp_kmeans)
  
}else if (Subcategory == "PSC") {
  # Read pre-processed data
  df = read.csv("data/percentages_psc_indices_values.csv")
  
  # Make into matrix and convert to data frame
  matrix = sparseMatrix(i = df$row, j = df$col, x = df$values)
  matrix = as.matrix(matrix)
  df_for_cp = as.data.frame(matrix)
  
  # Read in duns and vendor names lookup table for naics6D
  vendors = read.csv("data/total_dollars_obligated_psc_duns_vendor_names.csv",
                     colClasses = "character")
  df_for_cp = cbind(vendors, df_for_cp )
  df_for_cp  = as.data.frame(df_for_cp )
  df_for_cp  = df_for_cp [df_for_cp $parentdunsnumber %in% companies$parentdunsnumber,]
  temp = fread("data/total_dollars_obligated_psc_clustering_data.csv",
               colClasses = "character")
  colnames(df_for_cp)[3:ncol(df_for_cp)] = colnames(temp)
  df_for_cp = df_for_cp[,3:ncol(df_for_cp)]
  
  remove(df, matrix, temp, vendors, centers, Cluster_KMeans, naics2D, naics2D_vendors)
  
  # Filename = paste("data/NAICS_6D_", Industry, ".RData", sep = "")
  # load(Filename)
  
  # Clusters = 2:50
  # Cluster_KMeans_naics6D = list()
  # 
  # for (i in Clusters) {
  #   # Set seed
  #   set.seed(123)
  #   
  #   # Perform K-Means clustering with 13 groups
  #   temp_kmeans = kmeans(x = df_for_cp, centers = i, nstart=50, iter.max = 15)
  #   Cluster_KMeans_naics6D[[i - 1]] = temp_kmeans
  # }
  # 
  # remove(temp_kmeans) 
}


# # Other Code ====
# # Setup ################################
# 
# # Load packages
# library(clValid)
# library(corrplot)
# library(data.table)
# library(DBI)
# library(dbplyr)
# library(dplyr)
# library(fossil)
# library(ggplot2)
# library(knitr)
# library(magrittr)
# library(Matrix)
# library(plotly)
# library(RColorBrewer)
# library(RMySQL)
# 
# # Load functions
# source("R/Transform_to_Percentage.R")
# source("R/Transform_by_Capping.R")
# source("R/Min_Max_Scaling.R")
# source("R/Transform_Negative_Values.R")
# source("R/set_environment_variables.R")
# 
# # Load data
# set_environment_variables(TRUE)
# con = dbConnect(drv = MySQL(),
#                 db = "kmeans_2D_naics",
#                 host = Sys.getenv("Host"),
#                 user = Sys.getenv("username"),
#                 password = Sys.getenv("password"))
# df_original = con %>% 
#   tbl("indices_values_dobl_2D_naics") %>%
#   collect()
# companies  = con %>% 
#   tbl("indices_values_duns_vendornames_dobl_2D_naics") %>%
#   collect()
# naics_2D  = con %>% 
#   tbl("indices_values_naics_2D_Desc_Code") %>%
#   collect()
# 
# # Pre-Process ################################
# 
# # Create copy of original data set
# df = df_original
# 
# # Step 1: Make all values positive
# df = Transform_Negative_Values(df, 
#                                type = "absolute value")
# # Step 2: Divide each value by the sum of numbers in its row
# df = Transform_to_Percentage(df)
# # Step 3: Set values below 0.1 to zero
# df = Transform_by_Capping(df = df, 
#                           lower_bound = 0.1, 
#                           upper_bound = 0.9, 
#                           type = "Below")
# 
# # Step 4: Convert data to a data frame
# matrix = sparseMatrix(i = df$row, j = df$col, x = df$values)
# matrix = as.matrix(matrix)
# df_for_cp = as.data.frame(matrix)
# 
# remove(matrix, df)
# 
# cluster13 = con %>% 
#   tbl("cluster13") %>%
#   collect()
# 
# df_for_cp$cluster = cluster13$cluster
# 
# # Clustering ################################
# 
# Clusters = 2:50
# Cluster_KMeans = list()
# 
# for (i in Clusters) {
#   # Set seed
#   set.seed(123)
#   
#   # Perform K-Means clustering with 13 groups
#   temp_kmeans = kmeans(x = df_for_cp, centers = i, nstart=50, iter.max = 15)
#   Cluster_KMeans[[i - 1]] = temp_kmeans
# }
# 
# remove(temp_kmeans)
# 
# # Write CSVs ################################
# i = 2
# for (element in Cluster_KMeans){
#   temp = data.frame(cluster = element$cluster)
#   write.csv(temp, paste("data/clusters/cluster", as.character(i), ".csv",sep = ""), row.names = FALSE)
#   i = i + 1
# }
# 
# i = 2
# for (element in Cluster_KMeans){
#   temp = data.frame(cluster = element$centers)
#   write.csv(temp, paste("data/clusters/centers", as.character(i), ".csv",sep = ""), row.names = FALSE)
#   i = i + 1
# }
# 
# for (i in 2:50){
#   temp = read.csv(paste("data/clusters/centers", as.character(i), ".csv", sep = ""))
#   colnames(temp) = c("V1","V2", "V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15", 
#                      "V16","V17","V18","V19","V20","V21","V22","V23","V24")
#   write.csv(temp, paste("data/clusters/centers", as.character(i), ".csv",sep = ""), row.names = FALSE)
# }
# # Write to database ################################
# for (i in 2:50){
#   c = read.csv(paste("data/clusters/cluster", as.character(i), ".csv", sep = ""))
#   center = read.csv(paste("data/clusters/centers", as.character(i), ".csv", sep = ""))
#   dbWriteTable(conn = con, name = paste("cluster", as.character(i), sep = ""), value = c)
#   dbWriteTable(conn = con, name = paste("center", as.character(i), sep = ""), value = center)
# }
# 
# # Visualize ################################
# load("data/cluster_kmeans.RData")
# 
# 
# # Between WSS/Total WSS
# t = data.frame(wss = sapply(X = Cluster_KMeans, 
#                             FUN = function(x){
#                               
#                               sum(x$betweenss)/x$totss*100
#                               
#                             }))
# t$clusters = 2:50
# 
# g_wss = ggplot(data = t) +
#   geom_point(mapping = aes(x = clusters, y = wss)) + 
#   geom_line(mapping = aes(x = clusters, y = wss)) + 
#   labs(title = "Variance Explained using K-Means",
#        subtitle = "Number of clusters from 2 - 50",
#        x = "Number of Clusters",
#        y = "Percentage of Variance Explained")
# 
# p_wss = plot_ly(t, 
#                 x = ~clusters, 
#                 y = ~wss, 
#                 type = "scatter", 
#                 mode = "lines+markers+text",
#                 hoverinfo = "x+y") %>%
#   layout(title = "Variance Explained using K-Means",
#          xaxis = list(title = "Number of Clusters"),
#          yaxis = list (title = "Percentage of Variance Explained"),
#          hovermode = "x")
# 
# # Centers Heatmap
# 
# t2 = sapply(X = Cluster_KMeans, 
#             FUN = function(x){
#               
#               x$centers
#               
#             })
# 
# vals <- unique(scales::rescale(c(volcano)))
# o <- order(vals, decreasing = FALSE)
# cols <- scales::col_numeric("Reds", domain = NULL)(vals)
# colz <- setNames(data.frame(vals[o], cols[o]), NULL)
# 
# 
# 
# plot_ly(x = 1:49,
#         text = ~matrix(rep(paste("Major NAICS Category: ", naics_2D$Sector, sep = ""), 49), ncol = 49, nrow = 24),
#         z = t(t2[[49]]), 
#         colorscale = colz,
#         type = "heatmap",
#         hoverinfo = "x+text+z") %>%
#   layout(title = "Centers of Clusters",
#          xaxis = list(title = "Clusters"),
#          yaxis = list (title = "Major NAICS Code", showticklabels = FALSE, ticks = ""),
#          margin = list(
#            r = 10, 
#            t = 25, 
#            b = 40, 
#            l = 50
#          ))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# test = con %>%
#   tbl("center13") %>%
#   collect()
# test = test[,colnames(test) != "row_names"]
# 
# testmatrix = as.matrix(test)
# plot_ly(z = testmatrix, type = "heatmap")
# 
# test$cluster = rownames(test)
# test2 = melt(test, id.vars = "cluster")
# test2$cluster = factor(test2$cluster, levels = 1:13)
# test2 = left_join(x = test2, 
#                   y = naics_2D,
#                   by = c("variable" = "Column_Code"))
# 
# ggplot(test2, aes(cluster, Sector)) + 
#   geom_tile(aes(fill = value), colour = "white") + 
#   scale_fill_gradient(low = "white", high = "steelblue") + 
#   theme(axis.text.y = element_text(angle = 0, size = 8))
# 
# # FROM LOCAL RSTUDIO ====
# 
# reduced_data = SVD$u[,1:355]%*%diag(SVD$d[1:355])
# reduced_data = data.frame(reduced_data)
# 
# # Elbow Method for finding the optimal number of clusters ====
# set.seed(123)
# # Compute and plot wss for k = 2 to k = 15.
# k.max <- 15
# wss <- sapply(X = 1:k.max, 
#               FUN = function(k){
#                 
#                 kmeans(reduced_data, k, nstart=50,iter.max = 15 )$tot.withinss
#                 
#               })
# 
# 
# 
# 
# duns_vendornames = read.csv("../../data/9_vendor_names_duns_total_dollars_obligated.csv")
# 
# kmeans30 = kmeans(reduced_data, centers = 100, nstart = 50, iter.max = 15)
# 
# duns_vendornames$cluster = kmeans30$cluster
# 
# # NAICS2D per DUNS #################################
# 
# library(data.table)
# library(dplyr)
# source("R/int_hist.R", chdir = T)
# 
# df = fread("data/1a_clustering_data_before_dcast.csv")
# duns = fread("data/9_vendor_names_duns_total_dollars_obligated.csv")
# 
# df = df[df$parentdunsnumber %in% duns$parentdunsnumber,]
# df$naics2D = substr(df$naics_code,1,2)
# 
# naics2d_per_duns = df %>% 
#   filter(naics2D != "") %>%
#   group_by(parentdunsnumber) %>%
#   summarise(n = n_distinct(naics2D))
# 
# companies_with_multiple = naics2d_per_duns %>% 
#   group_by(n) %>% 
#   summarise(n2 = n())
# 
# companies_with_multiple$cum_perc = cumsum(companies_with_multiple$n2)/116328*100
# 
# t2 = int_hist(naics2d_per_duns$n)
# t3 = data.frame(t2$counts)
# colnames(t3) = c("Number of Major NAICS Codes", "Count")
# 
# t = duns[which(duns$parentdunsnumber %in% naics2d_per_duns$parentdunsnumber[which(naics2d_per_duns$n == 10)]),] %>%
#   arrange(vendorname)
# 
# 
# 
# # Correlation Plot ################################
# 
# # Load packages
# library(clValid)
# library(corrplot)
# library(data.table)
# library(dplyr)
# library(fossil)
# library(ggplot2)
# library(knitr)
# library(magrittr)
# library(Matrix)
# library(RColorBrewer)
# 
# # Load functions
# source("R/Transform_to_Percentage.R")
# source("R/Transform_by_Capping.R")
# source("R/Min_Max_Scaling.R")
# source("R/Transform_Negative_Values.R")
# 
# # Load data
# df_original = fread("data/indices_values_dollars_obligated_2d_naics.csv")
# naics_2D = fread("data/naics_2d_desc.csv")
# companies = fread("data/duns_vendor_names_total_obligated_2D_naics.csv")
# 
# # Create copy of original data set
# df = df_original
# 
# # Step 1: Make all values positive
# df = Transform_Negative_Values(df, 
#                                type = "absolute value")
# # Step 2: Divide each value by the sum of numbers in its row
# df = Transform_to_Percentage(df)
# # Step 3: Set values below 0.1 to zero
# df = Transform_by_Capping(df = df, 
#                           lower_bound = 0.1, 
#                           upper_bound = 0.9, 
#                           type = "Below")
# 
# # Step 4: Convert data to a data frame
# matrix = sparseMatrix(i = df$row, j = df$col, x = df$values)
# matrix = as.matrix(matrix)
# df_for_cp = as.data.frame(matrix)
# 
# # Set seed
# set.seed(123)
# 
# # Perform K-Means clustering with 13 groups
# kmeans13 = kmeans(x = df_for_cp, centers = 13, nstart=50, iter.max = 15)
# 
# res = cor(df_for_cp, method = "s")
# 
# corrplot(res, type = "upper", order = "hclust", 
#          tl.col = "black", tl.srt = 45)
# 
# clusters = df_for_cp
# clusters$c = kmeans13$cluster
# 
# d = data.frame(naics = names(df_for_cp))
# 
# for (i in 1:13){
#   
#   t = clusters[clusters$c == i, -c(25)]
#   
#   d[,paste("c",as.character(i), sep = "")] = colSums(t)
#   
# }
# 
# d = melt(d, id.vars = "naics")
# 
# 
# ggplot(data = d) + 
#   geom_bar(mapping = aes(x = naics, y = value),
#            stat = "identity") + 
#   facet_wrap(~variable, nrow = 6, ncol = 4)
# 
# # Clustering with more than NAICS 2D ====
# 
# # Load packages
# library(bit64)
# library(data.table)
# library(dplyr)
# library(magrittr)
# library(stringi)
# 
# # Load data
# companies = fread("data/duns_vendor_names_total_obligated_2D_naics.csv")
# clusters = fread("data/cluster13.csv", header = FALSE)
# colnames(clusters) = c("row", "cluster")
# companies$cluster = clusters$cluster
# df = fread("data/cluster13_1_raw_data.csv", sep = "\t", colClasses = "character")
# 
# # Get companies from cluster 9
# c9 = companies[companies$cluster == 9,]
# 
# # Subset on companies from cluster 9
# df = df[df$parentdunsnumber %in% c9$parentdunsnumber,]
# 
# # Join vendorname from c9 with df
# df = left_join(x = df, 
#                y = c9,
#                by = "parentdunsnumber")
# 
# df = df[,c("dollarsobligated", "contractactiontype", "typeofcontractpricing", "dunsnumber", "parentdunsnumber", 
#            "psc_cat", "productorservicecode", "principalnaicscode", "piid", "modnumber", "fundingrequestingagencyid", 
#            "vendorname.y")]
# colnames(df)[12] = "vendorname"
# 
# # Split NAICS codes and NAICS descriptions
# naics_split = unlist(stri_split(str = df$principalnaicscode, fixed = ":", n = 2))
# naics_split = data.frame(naics_split = naics_split)
# 
# naics_code = data.frame(naics_code = naics_split$naics_split[seq(1, nrow(naics_split), 2)])
# naics_desc = data.frame(naics_desc = naics_split$naics_split[seq(2, nrow(naics_split), 2)])
# 
# naics = cbind(naics_code, naics_desc)
# remove(naics_split, naics_code, naics_desc)
# 
# # Split PSC codes and PSC descriptions
# psc_split = unlist(stri_split(str = df$productorservicecode, fixed = ":", n = 2))
# psc_split = data.frame(psc_split = psc_split)
# 
# psc_code = data.frame(psc_code = psc_split$psc_split[seq(1, nrow(psc_split), 2)])
# psc_desc = data.frame(psc_desc = psc_split$psc_split[seq(2, nrow(psc_split), 2)])
# 
# psc = cbind(psc_code, psc_desc)
# remove(psc_split, psc_code, psc_desc)
# 
# # Split funding agency codes and funding agency names
# fund_split = unlist(stri_split(str = df$fundingrequestingagencyid, fixed = ":", n = 2))
# fund_split = data.frame(fund_split = fund_split)
# 
# fund_code = data.frame(fund_code = fund_split$fund_split[seq(1, nrow(fund_split), 2)])
# fund_desc = data.frame(fund_desc = fund_split$fund_split[seq(2, nrow(fund_split), 2)])
# 
# fund = cbind(fund_code, fund_desc)
# remove(fund_split, fund_code, fund_desc)
# 
# # Combine data 
# df2 = cbind(parentdunsnumber = df$parentdunsnumber, 
#             vendorname = df$vendorname, 
#             piid = df$piid, 
#             dollarsobligated = df$dollarsobligated, 
#             psc_cat = df$psc_cat, 
#             naics, 
#             psc, 
#             fund
# )
# 
# write.csv(df2, "data/cluster13_2_clustering_data_before_dcast.csv", row.names = FALSE)
# df2 = fread("data/cluster13_2_clustering_data_before_dcast.csv")
# 
# #################################################################
# #                                                               #
# #     DOLLARS OBLIGATED PER NAICS, PSC, FUND AGENCY: TOTAL      #
# #                                                               #
# #################################################################
# 
# # Convert dollars obligated to double
# df2$dollarsobligated = as.double(df2$dollarsobligated)
# 
# # Sum dollars obligated for each parent DUNS number and for each NAICS code
# Total_Dollars_Obligated_NAICS  = df2 %>% 
#   group_by(parentdunsnumber, naics_code) %>% 
#   summarise(total = sum(dollarsobligated))
# 
# Total_Dollars_Obligated_NAICS = Total_Dollars_Obligated_NAICS[Total_Dollars_Obligated_NAICS$naics_code != "",]
# 
# Total_Dollars_Obligated_NAICS$code = sapply(X = Total_Dollars_Obligated_NAICS$naics_code,
#                                             FUN = function(x){
#                                               
#                                               x_temp = as.character(x)
#                                               
#                                               return(paste("n",x_temp, sep = "_"))
#                                               
#                                             })
# 
# # Sum dollars obligated for each parent DUNS number and for each PSC code
# Total_Dollars_Obligated_PSC  = df2 %>% 
#   group_by(parentdunsnumber, psc_code) %>% 
#   summarise(total = sum(dollarsobligated))
# 
# Total_Dollars_Obligated_PSC = Total_Dollars_Obligated_PSC[Total_Dollars_Obligated_PSC$psc_code != "",]
# 
# Total_Dollars_Obligated_PSC$code = sapply(X = Total_Dollars_Obligated_PSC$psc_code,
#                                           FUN = function(x){
#                                             
#                                             x_temp = as.character(x)
#                                             
#                                             return(paste("p",x_temp, sep = "_"))
#                                             
#                                           })
# 
# # Sum dollars obligated for each parent DUNS number and for each funding agency code
# Total_Dollars_Obligated_FUND  = df2 %>% 
#   group_by(parentdunsnumber, fund_code) %>% 
#   summarise(total = sum(dollarsobligated))
# 
# Total_Dollars_Obligated_FUND = Total_Dollars_Obligated_FUND[Total_Dollars_Obligated_FUND$fund_code != "",]
# 
# Total_Dollars_Obligated_FUND$code = sapply(X = Total_Dollars_Obligated_FUND$fund_code,
#                                            FUN = function(x){
#                                              
#                                              x_temp = as.character(x)
#                                              
#                                              return(paste("f",x_temp, sep = "_"))
#                                              
#                                            })
# 
# Total_Dollars_Obligated_NAICS = Total_Dollars_Obligated_NAICS[,c("parentdunsnumber", "code", "total")]
# Total_Dollars_Obligated_PSC = Total_Dollars_Obligated_PSC[,c("parentdunsnumber", "code", "total")]
# Total_Dollars_Obligated_FUND = Total_Dollars_Obligated_FUND[,c("parentdunsnumber", "code", "total")]
# 
# 
# Total = rbind.data.frame(Total_Dollars_Obligated_NAICS, Total_Dollars_Obligated_PSC, Total_Dollars_Obligated_FUND)
# 
# # Cast the data into long form using the sum of dollars obligated as the value. If no value is known, fill with 0
# Total = data.table::dcast(data = Total, 
#                           formula = parentdunsnumber ~ code, 
#                           fill = 0, 
#                           value.var = "total")
# write.csv(Total, "data/cluster13_2_clustering_data.csv", row.names = FALSE)
# Total = fread("data/cluster13_2_clustering_data.csv")
# 
# # Cluster
# # Elbow Method for finding the optimal number of clusters ====
# set.seed(123)
# 
# psc = read.csv("psc_total_dollars_obligate.csv")
# 
# # Step 2: Divide each value by the sum of numbers in its row
# psc = Transform_to_Percentage(psc)
# # Step 3: Set values below 0.1 to zero
# psc = Transform_by_Capping(df = psc, 
#                            lower_bound = 0.1, 
#                            upper_bound = 0.9, 
#                            type = "Below")
# 
# # Step 4: Convert data to a data frame
# matrix = sparseMatrix(i = psc$row, j = psc$col, x = psc$values)
# matrix = as.matrix(matrix)
# df_for_cp = as.data.frame(matrix)
# 
# # Compute and plot wss for k = 2 to k = 15.
# k.max <- 20
# wss2 <- sapply(X = 11:k.max, 
#                FUN = function(k){
#                  
#                  kmeans(as.matrix(df_for_cp), k, nstart=50,iter.max = 15 )$tot.withinss
#                  
#                })
