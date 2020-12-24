library(rtweet)
consumerKey="YfosItKbjjyJ4eemqcKM9FgVw"
consumerSecret="4NOohalv98NjkT8e7MvmhTSiBqgvGLiROp9l4PjamxMWVoKCz4"
accessToken="1330224754498957315-O69Zbs7kxmP5eP8HRgqegvmrL7juJm"
accessTokenSecret= "oHO3xWPcEVnCGdZLmkGLfx6iPZAoMSUhqwBnzIMaF3VTm"
twitter_token <- create_token(
  app = "Hyson",
  consumer_key = consumerKey,
  consumer_secret = consumerSecret,
  access_token = accessToken,
  access_secret = accessTokenSecret)

OOTD <- search_tweets("HongKongProtests", n = 1000, include_rts = TRUE)
OOTD_1 <- OOTD[OOTD$is_retweet==T,]

range(OOTD_1$retweet_created_at)

OOTD_user <- OOTD_1[, c("user_id","retweet_user_id")]

names(OOTD_user$user_id)<- "Source"
names(OOTD_user$retweet_user_id)<- "Target"
Output <- data.frame(lapply(OOTD_user, as.character), stringsAsFactors=FALSE)

write_csv(df1, "userid_new.csv")

matrix <- as.matrix(OOTD_user)

library(igraph)
nw_rtweet <- graph_from_edgelist(el = matrix, directed = TRUE)
print.igraph(nw_rtweet)


label <- data.frame(count=OOTD_1$followers_count)

mean(OOTD_1$followers_count)
sd(OOTD_1$followers_count)

label$count_label <- ifelse(label$count>3000, "1","0")

V(nw_rtweet)$followers <- label$count_label
vertex_attr(nw_rtweet)

# Set the vertex colors for the plot
sub_color <- c("lightblue","yellow")
set.seed(1234)

retweet_plot_label <- plot(nw_rtweet, asp = 9/16,
                     vertex.size = 5,
                     vertex.color = sub_color[as.factor(vertex_attr(nw_rtweet,"followers"))],
                     edge.arrow.size = 0.5,
                     edge.color = "black",
                     vertex.label = NA,
                     main = "Retweet Network of #HongKongProtests based on follower")

legend("bottomleft", legend=c(">3000 followers (38)", "<3000 followers (848)"),
       fill=c("yellow", "lightblue"))

nrow(label[label$count_label==1,])


# Influential users network with betweeness
influential_users <- OOTD_1[OOTD_1$followers_count>3000,]

influential_users_1 <- influential_users[, c("user_id","retweet_user_id")]

names(influential_users_1$user_id)<- "Source"
names(influential_users_1$retweet_user_id)<- "Target"
Output_1 <- data.frame(lapply(influential_users_1, as.character), stringsAsFactors=FALSE)

matrix_1 <- as.matrix(influential_users_1)



library(igraph)
nw_rtweet_1 <- graph_from_edgelist(el = matrix_1, directed = TRUE)
print.igraph(nw_rtweet_1)

deg_out <- degree(nw_rtweet_1, mode=c("out"))
deg_out
vert_size <- (deg_out*5)+5

set.seed(1234)

retweet_plot_label <- plot(nw_rtweet_1, asp = 9/16,
                           vertex.size = vert_size,
                           edge.arrow.size = 0.5,
                           edge.color = "black",
                           vertex.label = influential_users$screen_name,
                           vertex.label.cex = 0.6,
                           vertex.label.colour = "black",
                           vertex.color = "lightblue",
                           main = "Retweet Network of Influential Users")

  
  
  