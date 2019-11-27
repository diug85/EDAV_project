#################
#####READ ME#####
#################

# Que contiene este codigo

#1. Data Extraction Dialogues: Crea base de dialogos
#2. Data Extraction Ratings IMDb: Crea base de ratings de IMDb (nota las bases originales estan en el repo de Ivan en Gdrive)
#3. Boxplot Ratings: Primera version de Boxplot de ratings
#4. Useful Databases: Create 2 useful data bases for analysis
#5. K-means:  Primera version de analisis de K-means para personajes



##############################################
######1. Data Extraction Dialogues############
##############################################

library(rvest)
library(robotstxt)
library(tidyverse)

url <- "https://fangj.github.io/friends/"
paths_allowed(url)


html <- read_html(url)
episodes_list <- html %>% html_nodes("li")  %>% html_nodes("a")
link <- episodes_list %>% html_attr("href")
long_name <- episodes_list %>% html_text
df <- data.frame(long_name,link) %>%
  mutate(numbers = str_extract(long_name,"(\\d+-\\d+)|\\d+") ) %>%
  mutate(season=ifelse(str_length(str_match(numbers,"\\d+"))==3, substr(numbers,0,1),substr(numbers,0,2) ) ) %>%
  mutate(number=str_replace(str_replace(numbers,season,""), paste("-",season,sep = "")  ,"-") ) %>%
  mutate(name = trimws(str_replace(long_name,"(\\d+-\\d+)|\\d+","")) )%>%
  mutate(link = paste(url,link,sep="")) %>%
  mutate(id=paste(season,":",number))

df <- df[colnames(df)!="numbers"]



read_dialogues <- function(link,ep_id) {
  html <- read_html(link)
  #### Special case for episode 9 : 15
  if (ep_id == "9 : 15"){
    df_1 <- read.csv("episode_9_15.csv")
    df_2 <- df_1 %>%
      mutate(rows2= trimws(str_replace(rows,"\\(([^\\)]+)\\)","") )) %>%
      mutate(is_scene = ifelse(substr(rows2,0,1)=="[",1,0))
  }else{
    if(length(html %>% html_nodes("p,font p"))<10 ){
      rows <- html %>% html_nodes("font[size='3']") %>% 
        gsub(pattern = '<.*?>', replacement = "|")
      if(length(rows)==0){
        rows <- html %>%
          gsub(pattern = '<br\\s*/?>', replacement = "|")
      }
      
      df_1 <-data.frame()
      for (i in 1:length(rows)) {
        rows_2 <- strsplit(rows[i],split="[|]")
        df_temp <- data.frame(rows_2)
        names(df_temp) <- c("rows")
        df_1 <- rbind(df_1, df_temp)
      }
      
      df_2 <- df_1 %>%    
        mutate(rows2= trimws(rows)) %>%
        mutate(rows2 = str_replace_all(rows2, "[\n]" , " ")) %>%
        mutate(rows2 = str_replace_all(rows2, "<b>" , "")) %>%
        mutate(rows2 = str_replace_all(rows2, "</b>" , "")) %>%
        mutate(is_scene = ifelse(substr(rows2,0,1)=="[" | substr(rows2,0,1)=="(",1,0)) %>%
        mutate(rows2= trimws(ifelse(is_scene==1,rows2,str_replace(rows2,"\\[([^\\)]+)\\]",""))   ))
      
    }else{
      rows <- html %>% html_nodes("p,font p") %>% html_text()  
      df_1 <- data.frame(rows)
      df_2 <- df_1 %>%
        mutate(rows2= trimws(str_replace(rows,"\\(([^\\)]+)\\)","") )) %>%
        mutate(is_scene = ifelse(substr(rows2,0,1)=="[",1,0))
    } 
  }
  
  df_2$split <- str_locate(df_2[,"rows2"],  "\\w+:|\\w+.\\w+:|\\w+.\\s\\w+:" )[,2]
  
  df_3 <- df_2 %>%
    mutate(character=ifelse(is.na(split),"",toupper(trimws(substr(rows2,0,split - 1 ))))) %>%
    mutate(line=ifelse(is.na(split),"",trimws(substr(rows2,split + 1,1000000)))) %>%
    mutate(episode_id = ep_id) %>%
    mutate(line=trimws(str_remove(line,"\\(([^\\)]+)\\)")) ) %>%
    filter(character != "WRITTEN BY")
  
  
  df_4 <- data.frame(df_3, scene=cumsum(df_3$is_scene))
  
  df_5 <- df_4 %>%
    filter(rows2!="",!is_scene,!is.na(character),character!="")
  
  df_5$line_num <- seq.int(nrow(df_5))
  df_6<- df_5 %>%
    dplyr::select(c("episode_id","line_num","scene",'character','line'))
  
  return(df_6)
}

df2<-read_dialogues(df[203,"link"],df[203,"id"])



dialogues <- data.frame()
for (i in 1:nrow(df)) {
  n <- read_dialogues(df[i,"link"],df[i,"id"])
  #print(i)
  #print(nrow(n))
  dialogues <- rbind(dialogues, n)
  
}


dialogues <- dialogues %>%
  mutate(words = sapply(strsplit(line, " "), length))


unique(filter(dialogues, grepl("-",episode_id))$episode_id)


dialogues <- dialogues %>%
  mutate(episode_id = if_else(episode_id == "2 : 12-13" & scene < 36,"2 : 12",episode_id)) %>%
  mutate(episode_id = if_else(episode_id == "2 : 12-13" & scene >= 36,"2 : 13",episode_id)) %>%
  mutate(episode_id = if_else(episode_id == "6 : 15-16" & scene < 15,"6 : 15",episode_id)) %>%
  mutate(episode_id = if_else(episode_id == "6 : 15-16" & scene >= 15,"6 : 16",episode_id)) %>%
  mutate(episode_id = if_else(episode_id == "9 : 23-24" & scene < 16,"9 : 23",episode_id)) %>%
  mutate(episode_id = if_else(episode_id == "9 : 23-24" & scene >= 16,"9 : 24",episode_id)) %>%
  mutate(episode_id = if_else(episode_id == "10 : 17-18" ,"10 : 17",episode_id))


dialogues <- dialogues %>%
  mutate(season =  trimws(str_extract(episode_id,".+\\:"))) %>%
  mutate(episode =trimws(str_extract(episode_id,"\\:.+")))

dialogues <- dialogues %>%
  mutate(season =  as.integer( trimws(str_sub(season, end=-2)))) %>%
  mutate(episode = as.integer(trimws(str_sub(episode, start=2))))



dialogues <- dialogues %>%
  mutate(character = if_else(character == "RACH" ,"RACHEL", character)) %>%
  mutate(character = if_else(character == "MNCA" ,"MONICA", character)) %>%
  mutate(character = if_else(character == "CHAN" ,"CHANDLER", character)) %>%
  mutate(character = if_else(character == "PHOE" ,"PHOEBE", character)) 


## added this line to eliminate auxiliary data bases and objects that are not going to be useful

rm(df2, episodes_list, df, n, html, i, link, long_name, url)




##############################################
######2. Data Extraction Ratings IMDb#########
##############################################


##### Estas bases las puse en el repositorio de Google Drive ya que algunas pesan mas de 100Mb #####
##### Con esto cualquiera puede regenerar las BD que deberia ser ya nuestra base principal ya definitiva


load(file = "C:/Users/mamun/Documents/EDVA Fall 2019/Info Project/dfratings.RData")
load(file = "C:/Users/mamun/Documents/EDVA Fall 2019/Info Project/dfTitle.RData")
load(file = "C:/Users/mamun/Documents/EDVA Fall 2019/Info Project/dfepisode.RData")

IMDbFriends=filter(dfTitle, primaryTitle=='Friends' & startYear=='1994' & endYear=='2004') # Friends vector identifier in IMDBd

# rename columns to avoid duplicate column name duplicated with dfratings and dfepisode

colnames(IMDbFriends)=c('parentTconst', 'titleType', 'primaryTitle', 'originalTitle', 'isAdult', 'startYear', 'endYear', 'runtimeMinutes', 'genre')

# Get the ID from for each episode

IMDbEpisodes=inner_join(IMDbFriends[,1:3],dfepisode, by=c("parentTconst"="parentTconst"))

# Get the rating for each episode

IMDbRatings=left_join(IMDbEpisodes,dfratings, by=c("tconst"="tconst"))

rm(dfratings,dfTitle, dfepisode)


#Create key to join IMDb information with dialogues data base

IMDbRatings=IMDbRatings %>%
  mutate(episode_id=paste(as.character(IMDbRatings$seasonNumber), ":",
                          ifelse(as.numeric(as.character(IMDbRatings$episodeNumber))>=10,
                                 as.character(IMDbRatings$episodeNumber),
                                 paste0(as.character("0"),as.character(IMDbRatings$episodeNumber)))))


#Join to the dialogues DB information from IMDb like ratings, votes, and keys.

dialogues=inner_join(dialogues,IMDbRatings[,c(1,4,7,8,9)], by= c("episode_id"="episode_id"))


###################################################################
###################3. Box Plot Ratings ############################
###################################################################

library(ggplot2)
library(ggthemes)


ggplot(data=IMDbRatings,aes(x=reorder(seasonNumber, -averageRating, FUN = median),y=averageRating))+
  geom_boxplot()+
  ggtitle("IMDb Average Rating Distribution")+
  xlab("Season") + ylab("Average Rating")+
  theme_hc() + theme(plot.title = element_text(hjust=0.5))


###################################################################
###################4. Useful DB ###################################
###################################################################

DF_Character_Episode=dialogues%>%
  group_by(episode, season, scene, character, episode_id)%>%
  summarize(num_lines= n(), totwords=sum(words,na.rm = T), avgRating=mean(averageRating,na.rm = T) )%>%
  arrange(scene ,-num_lines, -totwords)%>%
  group_by(character, episode, season, episode_id)%>%
  summarize(Total_scene= n(), Total_lines=sum(num_lines,na.rm = T),Tot_words=sum(totwords,na.rm = T),Avg_Rating=mean(avgRating,na.rm = T) )%>%
  arrange(-Total_scene ,-Total_lines, -Tot_words)

DF_Character=dialogues%>%
  group_by(episode, season, scene, character, episode_id)%>%
  summarize(num_lines= n(), totwords=sum(words,na.rm = T))%>%
  arrange(scene ,-num_lines, -totwords)%>%
  group_by(character)%>%
  summarize(Total_scene= n(), Total_lines=sum(num_lines,na.rm = T),Total_words=sum(totwords,na.rm = T))%>%
  arrange(-Total_scene ,-Total_lines, -Total_words)

###################################################################
###################5. K-means ###################################
###################################################################

library(cluster)
library(factoextra)


ClusterCharacter=kmeans(DF_Character[,2:4], 3, nstart = 100)
vcluster=seq.int(1,3,1)
Centers=arrange(data.frame(ClusterCharacter$centers, vcluster),-Total_scene, -Total_lines, -Total_words)
DF_kmeans=cbind(DF_Character, as.data.frame(ClusterCharacter$cluster))
colnames(DF_kmeans)[5]=c('Cluster')
