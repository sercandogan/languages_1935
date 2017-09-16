library(tidyverse)
library(ggthemes)



#LOAD DATA ----------------------
lang <- as_tibble(read_csv("~/Desktop/Projects/language/data.csv"))
#Kaynak: Yeni Türk Mecmuası 1939 / 1935 nüfus sayımı

#TIDY DATA ----------------------
lang %<>%
  select(-toplam) %>%
  gather(kadin,erkek, key = cinsiyet, value = frekans)

lang <- transform(lang, cinsiyet = as.factor(cinsiyet))

head(lang)

#VISUALISING DATA ---------------------
lang %>% count(sum(frekans))
# [1] 741015 kişi

lang %>%
  group_by(cinsiyet) %>%
  summarise(n = sum(frekans))
#>  cinsiyet      n
#>1 erkek       380781
#>2 kadin       360294



# Barchart
lang %>%
  group_by(anadil) %>%
  summarise(n = sum(frekans)) %>%
  ggplot() +
  geom_bar(mapping = aes(anadil,n), stat = 'identity') +
  coord_flip()

# Barchart except Turkish (outlier)
lang %>%
  filter(anadil != 'Türkçe') %>%
  group_by(anadil) %>%
  summarise(n = sum(frekans)) %>%
  ggplot() +
  geom_bar(mapping = aes(anadil,n), stat = 'identity') +
  coord_flip()

# Look for details
lang %>%
  filter(!(anadil %in% c("Türkçe",'Yahudi İspanyolcası','Rumca','Ermenice'))) %>%
  group_by(anadil) %>%
  summarise(n = sum(frekans)) %>%
  ggplot() +
  geom_bar(mapping = aes(reorder(anadil,n),n), stat = 'identity') +
  coord_flip()

# Look for top 10 languages
lang %>%
  filter(!(anadil %in% c("Türkçe",'Yahudi İspanyolcası','Rumca','Ermenice'))) %>%
  group_by(anadil) %>%
  summarise(n = sum(frekans)) %>%
  top_n(10) %>%
  ggplot() +
  geom_bar(mapping = aes(reorder(anadil,n),n), stat = 'identity') + 
  coord_flip()  # + coord_polar() 

#reorder ****

# Look for speaken top 10 languages among women
lang %>%
  filter(cinsiyet == 'kadin' & !(anadil %in% c("Türkçe",'Yahudi İspanyolcası','Rumca','Ermenice'))) %>%
  top_n(10) %>% 
  ggplot() +
  geom_bar(mapping = aes(reorder(anadil,frekans),frekans), stat = 'identity', fill = "#ACACAC") + 
  coord_flip() + theme_fivethirtyeight()


# main graph

lang %>%
  group_by(anadil) %>%
  mutate(toplam = sum(frekans)) %>%
  filter(!(anadil %in% c("Türkçe",'Yahudi İspanyolcası','Rumca','Ermenice'))) %>%
  ggplot() + 
  geom_bar(mapping = aes(reorder(anadil,toplam),toplam, fill = cinsiyet), 
           stat='identity',
           inherit.aes = F,
           width = 0.8,
           alpha = 0.9) +
  coord_flip() + 
  labs(x = "Anadil",
       y = "Sayı",
       title = "1935 Yılındaki İstanbul'da Yaşayan İnsanların Dilleri",
       caption = "(Veri:1939 Yılı Yeni Türk Mecmuası Dergisi)")



# polar cordinate

lang %>%
  filter(!(anadil %in% c("Türkçe",'Yahudi İspanyolcası','Rumca','Ermenice'))) %>%
  group_by(anadil) %>%
  summarise(n = sum(frekans)) %>%
  top_n(10) %>%
  ggplot() +
  geom_bar(mapping = aes(reorder(anadil,n),n), stat = 'identity') + 
  coord_flip() + coord_polar() 


# Comparing between Turkish, Ladino, Armenian and Romaic

lang %>%
  filter(anadil %in% c("Türkçe",'Yahudi İspanyolcası','Rumca','Ermenice')) %>%
  group_by(anadil) %>%
  summarise(n = sum(frekans)) %>%
  ggplot() +
  geom_bar(mapping = aes(reorder(anadil,n),log(n)), stat = 'identity') + 
  theme_gdocs() +
  scale_y_continuous(limits = c(0,15)) +
  labs(x = "En çok konuşulan dört anadil",
        y = "log(frekans)")
  

