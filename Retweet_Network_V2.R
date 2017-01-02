# Build a Retweet network, apply community detection algorithms
# Using stringr library to extract strings

require(stringr)
require(igraph)
require(RPostgreSQL)
require(plyr)
require(data.table)



# drv  <-  dbDriver("PostgreSQL")
# 
# con  <- dbConnect(drv, dbname="new_test1",
#                   host="localhost",
#                   user="postgres",password="6235$$wa",port=5432)
# 
# tweets_text  <- dbGetQuery(con, "SELECT * from cairo_table;")
# tweets_text <- readRDS("R Objects/tweets_agg_cairo.rds")
# tweets_text  <- unique(tweets_text)
# tweets_text$RT_Logical  <- grepl("RT @.+:",tweets_text$body, perl=TRUE) 
# # Only alphanumeric plus underscore allowed in twitter usernames
# tweets_text  <- tweets_text[tweets_text$RT_Logical,]
# tweets_text$RTs  <- ifelse(tweets_text$RT_Logical,str_extract(tweets_text$body,"@[A-Za-z0-9_]+:? "),"")
# tweets_text$RTs  <- gsub("[@:]","",tweets_text$RTs,perl=TRUE)
# #tweets_RTs_only  <- count(tweets_text,vars=c("username","RTs")) # get the number of times one user RTs another, valued edges
# tweets_text <- as.data.table(tweets_text)
# tweets_text <- tweets_text[,count_RTs:=length(body),by=c("username","RTs")]
# # vertices_attr  <- tweets_text[c("username","location")]
# # vertices_attr  <- vertices_attr[!duplicated(vertices_attr$username),]
# 
# #I want only the raw numbers of choices at the level of user aggregated over time
# setkey(tweets_text,username,RTs)
# tweets_agg <- unique(tweets_text)
#Still have 200k+ edges for the Egypt data alone
#igraph has a handy function converting the RT network as an edgelist and every extra
#column is an attribute on the edges.
#vertices attributes must be added as an option
#vertices attributes must include all usernames mentioned in the edge list
tweets_agg <- readRDS("R Objects/tweets_agg_cairo.rds")
small_agg <- tweets_agg[count_RTs>10,.(username,RTs,count_RTs)]
RT_network  <- graph.data.frame(small_agg)
#Only want the large, connected component
RT_network_clust  <- clusters(RT_network)
#There is no mode functoin in R, oddly enough

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

V(RT_network)$clust  <- membership(RT_network_clust)
RT_network_comp1  <- delete.vertices(RT_network,V(RT_network)[V(RT_network)[clust!=Mode(RT_network_clust$membership)]])
RT_edgelist <- as_edgelist(RT_network_comp1,names=FALSE)
getit <- get.edge.attribute(RT_network_comp1)
RT_edgelist <- cbind(RT_edgelist,as.numeric(getit$count_RTs))
write.table(RT_edgelist,file="Clustering Model/mixderg/mixderg/data/RT_network/RT_network.dat",sep="\t",row.names=FALSE,
            col.names=FALSE)
detach("package:igraph",unload=TRUE)
require(intergraph)
require(VBLPCM)

RT_network_obj  <- asNetwork(RT_network_comp1)
#RT_comms  <- ergmm(RT_network_obj ~ euclidean(d=2,G=2),family="Poisson",response="freq",verbose=TRUE)
#The standard MCMC method is way too slow

RT_start  <- vblpcmstart(RT_network_obj,G = 2,d=2,model="rsender",edgecovs=as.vector(as.matrix(RT_network_obj,matrix.type="adjacency",attrname="count_RTs")))
RT_finish_d1  <- vblpcmfit(RT_start,STEPS=1000,maxiter=200)
saveRDS(RT_finish_d1,"R Objects/cairo_network_all_10RTs_d2.rds")
RT_posterior <- RT_finish_d1$V_lambda
saveRDS(RT_posterior,"R Objects/cairo_posterior_10RTs_d2.rds")
rm(RT_start)
rm(RT_finish_d1)
RT_start  <- vblpcmstart(RT_network_obj,G = 2,d=1,model="rsender",edgecovs=as.vector(as.matrix(RT_network_obj,matrix.type="adjacency",attrname="count_RTs")))
RT_finish_d2 <- vblpcmfit(RT_start2,STEPS=200,maxiter=200)
saveRDS(RT_finish_d2,"R Objects/cairo_network_all_10RTs_d1.rds")
RT_posterior <- RT_finish_d2$V_lambda
saveRDS(RT_posterior,"R Objects/cairo_posterior_10RTs_d1.rds")
rm(RT_start)
rm(RT_finish_d2)
RT_start3  <- vblpcmstart(RT_network_obj,G = 1,d=1,model="rsender",edgecovs=as.vector(as.matrix(RT_network_obj,matrix.type="adjacency",attrname="count_RTs")))
RT_finish_g1 <- vblpcmfit(RT_start3,STEPS=200,maxiter=200)
saveRDS(RT_finish_g1,"R Objects/cairo_network_all_10RTs_g1.rds")
RT_posterior <- RT_finish_g1$V_lambda
saveRDS(RT_posterior,"R Objects/cairo_posterior_10RTs_g1.rds")


RT_change  <- function(x) {
  if(x[1]>x[2]){
    group  <- 1
  }
  if(x[1]<x[2]){
    group  <- 2
  }
  group
}



RT_membership <- apply(RT_posterior, 2,FUN=RT_change)
#RT_probs <- predict.vblpcm(RT_finish)
#boxplot(split(RT_probs,RT_finish$Y),ylab=expression(paste("P(",Y[i][j],"=1)")),xlab=expression(paste(Y[i][j])))
#plot.igraph(RT_network_comp1,vertex.size=1,edge.arrow.size=.1)

# Now import the community membership vector back into igraph
detach("package:VBLPCM",unload=TRUE)
require(igraph)
latent_comm  <- create.communities(RT_membership,algorithm="Latent Position Model")
V(RT_network_comp1)$community <- RT_membership
plot(latent_comm,RT_network_comp1,vertex.label=NA,vertex.label.cex=.5,edge.arrow.size=.1,vertex.size=5)

group1 <- V(RT_network_comp1)[V(RT_network_comp1)$community==1]$name
group2 <- V(RT_network_comp1)[V(RT_network_comp1)$community==2]$name
group1 <- data.frame(group1)
group2 <- data.frame(group2)