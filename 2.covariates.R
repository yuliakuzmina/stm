# creating dataframe with  covariate and messages > 200 characters and cutting big messages to 1000 words
# plots with wordfreq
integrum <- readRDS(file = "/Users/yuliyakuzmina/Documents/R/integrum_sex_ed/intermediate_data/int_lem12286.RDS")
str(integrum)

# covariate federal - not a federal media
integrum$fed <- ifelse(grepl("МК|АиФ|Гарант|Rbk|kp.|Российская г|РИА Н|Независимая г|iz.ru|
                             РИА Н|Комсомольская|ИА REGNUM|Советская Россия|РИФ|Гарант|
                             Газета.Ru|Lenta.ru", integrum$source ), 1 ,0)

View(table(integrum$fed))

# are there NA observations after lemmatization?
View(table(integrum$text[nchar(integrum$text)<50]))

View(integrum[integrum$text=="как это делается"])

integrum <- integrum[!integrum$text=="как это делается"]

integrum <- integrum[integrum$year<2019]

# check if I can cut the messages to 1000 words
tmpdf <-  integrum %>% mutate(word_cnt = sapply(strsplit(text_lemm, " "), length))

tmpdf2 <-  tmpdf %>% mutate(text_lemm_1000 = ifelse(word_cnt > 1000, 
                                                    word(text_lemm, 1, 1000), 
                                                    text_lemm)) %>% filter(word_cnt > 200)

tmpdf2 <-  tmpdf2 %>% mutate(word_cnt_1000 = sapply(strsplit(text_lemm_1000, " "), length))

# plots freq fed / no-fed
ggplot(tmpdf, aes(x=date )) +
  geom_histogram(color="black", fill="white", binwidth = 100)+
  ggtitle("The number of texts on sex education in the Russian media")+ 
  geom_vline(xintercept = as.Date("2012-01-01")) +
  ggplot2::annotate("text", x = as.Date("2010-09-15"), y = 600, label = "Law “On the protection \nof children from information \nharmful to their health and development”", size=4)+
  geom_vline(xintercept = as.Date("2013-07-01")) +
  ggplot2::annotate("text", x = as.Date("2014-07-15"), y = 740, label = "Law on the Prohibition of the Promotion \nof Homosexuality among Minors",
                    size=4)+
  labs(x="Year",
       y="The number of messages",
       caption = "\n12282 texts  on sex education in the Russian media 2008 - 2018")+ 
  geom_blank() + 
  theme_bw()+
  theme(plot.caption = element_text(size = 12, hjust = 0), 
        axis.title = element_text(size = 14),
        title = element_text(size = 16))

ggsave("~/Desktop/freq_messages.png", dpi = 300, width = 12, height = 10)


ggplot(tmpdf, aes(x=date)) +
  geom_histogram(color="black", fill="white", binwidth = 100)+
  facet_wrap(fed ~ ., scales = "free_y", ncol = 1, 
             labeller = as_labeller(c("0" = "Regional media", 
                                      "1" = "Federal media") ))+
  ggtitle("The number of texts on sex education \nin  federal / regional Media")+
  labs(x="Year", y="The number of texts", 
       caption = "\n12282 texts  on sex education in the Russian media 2008 - 2018")+ 
  geom_blank() + 
  theme_bw()+
  theme(plot.caption = element_text(size = 12, hjust = 0), 
        axis.title = element_text(size = 14),
        title = element_text(size = 16))

ggsave("~/Desktop/freq_messages_fed.png", dpi = 300, width = 12, height = 9) 



# plots with wordcount
ggplot(tmpdf, aes(word_cnt)) +
  geom_histogram(binwidth = 10)+
  scale_x_continuous(breaks=c(seq(0,3000, 200)))+
  geom_vline(xintercept=200)+
  labs(title= "The number of words in texts",
       y = "The number of texts", x = "The number of words",
       caption = "\n12282 texts  on sex education in the Russian media 2008 - 2018")+ 
  geom_blank() + 
  theme_bw()+
  theme(plot.caption = element_text(size = 12, hjust = 0), 
        axis.title = element_text(size = 14),
        title = element_text(size = 16))

ggsave("~/Desktop/freq_words.png", dpi = 300, width = 12, height = 9)


# when messages > 200 characters and  big messages are cut to 1000 words
ggplot(tmpdf2, aes(x=date)) +
  geom_histogram(color="black", fill="white", binwidth = 100)+
  facet_wrap(fed ~ ., scales = "free_y", ncol = 1, 
             labeller = as_labeller(c("0" = "Regional media", 
                                      "1" = "Federal media") ))+
  ggtitle("The number of texts on sex education \nin  federal / regional Media")+
  labs(x="Year", y="The number of texts", 
       caption = "\n12282 texts  on sex education in the Russian media 2008 - 2018")+ 
  geom_blank() + 
  theme_bw()+
  theme(plot.caption = element_text(size = 12, hjust = 0), 
        axis.title = element_text(size = 14),
        title = element_text(size = 16))

ggsave("~/Desktop/freq_messages_fed_9186.png", dpi = 300, width = 12, height = 9) 


ggplot(tmpdf, aes(word_cnt)) +
  geom_histogram(binwidth = 10)+
  scale_x_continuous(breaks=c(seq(0,3000, 200)))+
  geom_vline(xintercept=200)+
  labs(title= "The number of words in texts after data processing",
       y = "The number of texts", x = "The number of words",
       caption = "\n12282 texts  on sexual education in the Russian media 2008 - 2018")+ 
  geom_blank() + 
  theme_bw()+
  theme(plot.caption = element_text(size = 12, hjust = 0), 
        axis.title = element_text(size = 14),
        title = element_text(size = 16))

ggsave("~/Desktop/freq_words2.png", dpi = 300, width = 12, height = 9)

