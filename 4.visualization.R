library(jsonlite)
library(tidyr)
library(widyr)
library(psych)
library(lubridate)
library(scales)
library(ggalluvial)
library(alluvial)

cov_fed_20 <- readRDS(file = "intermediate_data/cov_fed_20_210619")

labelTopics(cov_fed_20, n = 10, frexweight = 0.75)

topicNames <- c('Sex education', 'Nurture','School', 'Family policy',
                'Regional politics', 'Sex','(Homo)sexuality',  
                'Internet regulation',
                'World changes', 'Law', 'Officials about sex education', 'Personal strategies',
                'Crime against minors',
                'Western countires and a threat to moral-spiritual values',
                'Reproductive health and the reproduction of nation',
                'Church', 'Juvenile justice', 'Protect children from harmful information',
                'Germany', 'Politcs')

plot(cov_fed_20, custom.labels = topicNames)

# topic correlation
topic_corr_simple <- topicCorr(cov_fed_20)
plot(topic_corr_simple)

topic_corr_huge <- topicCorr(cov_fed_20, method = "huge")
plot(topic_corr_huge)

# ggplot significant estimate effect
predict_topics <- estimateEffect(1:20 ~fed, cov_fed_20,
                                 meta = out$meta,
                                 uncertainty = "Global")

sumdf <- summary(predict_topics)

tblsum <- sumdf$tables
sign_topics <- as.data.frame(cbind(topic = topicNames,
                                   polit_true_sign = rep(NA, 15),
                                   estimate = rep(NA, 15)))
sign_topics$polit_true_sign = as.numeric(sign_topics$polit_true_sign)
sign_topics$estimate = as.numeric(sign_topics$estimate)
for (i in c(1:15)){
  sign_topics$polit_true_sign[[i]] = tblsum[[i]][2,4]
}
for (i in c(1:15)){
  sign_topics$estimate[[i]] = tblsum[[i]][2,1]
}
sign_topics$Group<-ifelse(sign_topics$polit_true_sign<=0.05,"Significant","Insignificant")
sign_topics$polit <- ifelse(sign_topics$estimate<0, "Non-fed", "Fed")
sign_topics <- sign_topics %>% filter(Group != "Insignificant")
cols <- c("Non-fed"="#FF1B1C", "Fed"="#202C59")

ggplot(sign_topics, aes(x=topic,y=estimate,fill=polit)) +
  geom_bar(stat="identity", alpha=0.7) +
  ylab("Estimate") +
  xlab("Topic") +
  ggtitle("Significant estimated effect of covariates on topics") +
  coord_flip() +
  scale_fill_manual(name="", values=cols, breaks=c("Non-fed", "Fed")) +
  theme_minimal() +
  theme(text=element_text(family="Times New Roman", size=12), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

# plot text examples with findThoughts
tmpdf2 = tmpdf2 %>% mutate(text_200 = substr(text, 1, 200))
thought <- findThoughts(cov_fed_20, texts=tmpdf2$text_1000, topics = 9, n=1000)

thought
plot(thought)
plotQuote(thought$docs[[1]])

# 20 topics stm igraph (correlation)
str(topic_corr_simple)

adj_cor_topic <- as.matrix(topic_corr_huge$posadj)
adj_cor_topic_cor <- as.matrix(topic_corr_huge$cor)
# topic names
adj_cor_topic <- as.matrix.data.frame(adj_cor_topic) 
colnames(adj_cor_topic) <- topicNames 
rownames(adj_cor_topic) <- topicNames
#  igraph from matrix
cor_topics <- graph.adjacency(adjmatrix = adj_cor_topic, mode = "undirected", diag = F)
# only correlated vertices
cor_topics <- delete.vertices(cor_topics,
                              V(cor_topics)[ degree(cor_topics) < 1] )
fastgreedy_topic <- fastgreedy.community(cor_topics)

# clusters
table_fastgreedy_topic <- cbind(fastgreedy_topic$membership, fastgreedy_topic$names) 
table_fastgreedy_topic = as.data.frame(table_fastgreedy_topic)
table_fastgreedy_topic$V1 = as.character(table_fastgreedy_topic$V1)
table_fastgreedy_topic$V2 = as.character(table_fastgreedy_topic$V2)
V(cor_topics)$Clusters = as.character(table_fastgreedy_topic$V1[match(V(cor_topics)$name, table_fastgreedy_topic$V2)])

col_topics = c("1"="#2FBF71", "2"="#EF2D56", "3"="#59C3C3",  "4"="#DFCC74")
ggraph(cor_topics, layout = "fr") +
  geom_edge_link(show.legend = FALSE) +
  geom_node_point(aes(color = Clusters), alpha = 0.7, size = 5, palette = "Set2") +
  geom_node_text(aes(label = name), repel = TRUE, size=5) +
  theme_void() +
  scale_color_manual(values=col_topics) +
  theme(legend.position="none", text=element_text(family="Times New Roman"))

ggsave("~/Desktop/network_of_topics.png", dpi = 300, width = 12, height = 9)


