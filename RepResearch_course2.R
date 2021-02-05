###################################
# Load libraries
###################################
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)

###################################
# Load data from File in the WWW
###################################
my_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
URLdecode(my_url)
# The file is the last section of the URL. Lets split URL by '/'
split_URL <- strsplit(URLdecode(my_url),c("/"))
my_file <- split_URL[[1]][length(split_URL[[1]])]   # extract the file name

if (!dir.exists("data/")){
      dir.create("data")                  # create a "data/" folder
}

dest_file <- paste0("data/",my_file)      # destination file
if(!file.exists(dest_file)){
      download.file(my_url,dest_file)           # dowload`
}

###################################
# Read data and basic transformations
###################################
      
data <- read.csv(dest_file)               # 'read.csv' can read .bz2!!
str(data)                                 # view structure

data$BGN_DATE <- mdy_hms(data$BGN_DATE)   # Convert BGN into Date Class Object

data <- data %>% select(                  # Subset - Select variables 
      BGN_DATE,
      STATE,
      EVTYPE,
      FATALITIES,
      INJURIES,
      PROPDMG,
      PROPDMGEXP,
      CROPDMG,
      CROPDMGEXP
)


###################################
# Process Property and Crop Damage Exponents
###################################
data <- data %>% mutate(
            BGN_YEAR = year(BGN_DATE),
            prop_exp = case_when(
                  PROPDMGEXP %in% c("H", "h") ~ 1e+2,
                  PROPDMGEXP %in% c("K", "k") ~ 1e+3,
                  PROPDMGEXP %in% c("M", "m") ~ 1e+6,
                  PROPDMGEXP %in% c("B", "b") ~ 1e+9,
                  PROPDMGEXP %in% c("", "-", "+", "?") ~ 1,
                  str_detect(PROPDMGEXP, "\\d") ~ 10 ^ as.numeric(PROPDMGEXP)
            ),
            crop_exp = case_when(
                  CROPDMGEXP %in% c("H", "h") ~ 10 ^ 2,
                  CROPDMGEXP %in% c("K", "k") ~ 10 ^ 3,
                  CROPDMGEXP %in% c("M", "m") ~ 10 ^ 6,
                  CROPDMGEXP %in% c("B", "b") ~ 10 ^ 9,
                  CROPDMGEXP %in% c("", "-", "+", "?") ~ 1,
                  str_detect(CROPDMGEXP, "\\d") ~ 10 ^ as.numeric(CROPDMGEXP)
            ),
            property_damage = PROPDMG * prop_exp,
            crop_damage = CROPDMG * crop_exp,
            total_damage = property_damage + crop_damage,
            total_health = FATALITIES + INJURIES
)


###################################
# Analysis
###################################

health_rate_by_evtype <- data %>%  group_by(EVTYPE) %>% 
      summarise(freq_impact = sum(ifelse(FATALITIES + INJURIES > 0, 1, 0), na.rm = T),
                freq = n(), 
                ratio = freq_impact / freq) %>% 
      mutate(decil_freq = )
      arrange(desc(ratio), desc(freq))

summary(health_rate_by_evtype[,c("freq","ratio")])


## Create Decade Breaks
max_year = max(data$BGN_YEAR)
decade_breaks <- seq(from=1950,to=max_year, by = 10)
decade_labels <-
      paste0("[",decade_breaks,"-", 
             formatC(decade_breaks %% 100 + 9, width = 2, flag = "0"),"]")
len_decade <- length(decade_labels)
## adjust last label to the max year
decade_labels[len_decade] <-
      paste0("[",decade_breaks[len_decade],"-",
             formatC(max_year %% 100, width = 2, flag = "0"), "]")
## Concatenate the last break
decade_breaks <- c(decade_breaks,max_year+1)

## Create lustrum Breaks
lustrum_breaks <- seq(from=1950,to=max_year, by = 5)
lustrum_labels <-
      paste0("[", lustrum_breaks, "-",
             formatC(lustrum_breaks %% 100 + 4, width = 2, flag = "0"), "]")

## adjust last label to match the last year of data
len_lustrum <- length(decade_labels)
lustrum_labels[length(lustrum_labels)] <-
      paste0("[", lustrum_breaks[length(lustrum_labels)], "-",
             formatC(max_year %% 100, width = 2, flag = "0"), "]")
lustrum_breaks <- c(lustrum_breaks,max_year+1)

## Breaks and labels in a log-scale squeme for Health impact
health_cut_breaks <- c(0, 1, 3, 10, 30, 100, 300, 1000, 3000,10000)
health_cut_labels <- c("[0,1)", "[1,3)", "[3,10)", "[10,30)", "[30,100)", 
                       "[100,300)", "[300,1K)", "[1K,3K)","[3K,10K)")

## Breaks and labels in a log-scale squeme for Damage
damage_cut_breaks <- c(0, 1, 1e+3, 30e+3, 1e+6, 30e+6, 1e+9, 30e+9, 1e+12)
damage_cut_labels <- c("[0,1)", "[1,1K)","[1K,30K)", "[30K,1M)", "[1M,30M)", "[30M,1B)", 
                       "[1B,30B)", "[30B,1000B)")

data <- data %>% mutate(
      decade = factor(paste0(BGN_YEAR %/%10*10,"s"),ordered = T),
      fatal_intervals = cut(FATALITIES, breaks = health_cut_breaks, labels = health_cut_labels,
          righ = F, ordered_result = T),
      injur_intervals = cut(INJURIES, breaks = health_cut_breaks, labels = health_cut_labels,
                      righ = F, ordered_result = T),
      prop_dmg_intervals = cut(property_damage, breaks = damage_cut_breaks, labels = damage_cut_labels,
          righ = F, ordered_result = T),
      crop_dmg_intervals = cut(crop_damage, breaks = damage_cut_breaks, labels = damage_cut_labels,
          righ = F, ordered_result = T)
)


event_type_table <- read.csv("data/EventTable.scv")
no_match <- data$EVTYPE[!(data$EVTYPE %in% toupper(event_type_table$Event.Name))]
length(no_match)

sorted_events <- sort(table(data$EVTYPE),decreasing = T)
head(sorted_events,40)        # List TOP 40 Event Types
top_freq_event <- names(head(sorted_events,40)) # Store the Names
# The name of the Events that missmatch Table and belong to TOP 25 Events occur
top_freq_event[!(top_freq_event %in% toupper(event_type_table$Event.Name))]


###########################
## FIX TYPE EVENTS
###########################
#------
data$EVTYPE2 <- data$EVTYPE         # copy EVTYPE to EVTYPE2

# Fix Typing errors
data$EVTYPE2 <- str_replace(data$EVTYPE2,"AVALANCE","AVALANCHE")  # typing error
# Fix Plural
data$EVTYPE2 <- str_replace(data$EVTYPE2,"WINDS","WIND")
data$EVTYPE2 <- str_replace(data$EVTYPE2,"CURRENTS","CURRENT")    
# Replace Abbreviations
data$EVTYPE2 <- str_replace(data$EVTYPE2,"FLD","FLOOD")           
data$EVTYPE2 <- str_replace(data$EVTYPE2,"TSTM","THUNDERSTORM")
# Fix Gerund instead noun
data$EVTYPE2 <- str_replace(data$EVTYPE2,"FLOODING","FLOOD")
# Fix some events - no other valid choice
data$EVTYPE2 <- str_replace(data$EVTYPE2,"URBAN/SML STREAM FLOOD","FLOOD")
data$EVTYPE2 <- str_replace(data$EVTYPE2,"WILD/FOREST FIRE","WILDFIRE")
data$EVTYPE2 <- str_replace(data$EVTYPE2,"WINTER WEATHER/MIX","WINTER WEATHER")
data$EVTYPE2 <- str_replace(data$EVTYPE2,"EXTREME COLD","EXTREME COLD/WIND CHILL")
data$EVTYPE2 <- str_replace(data$EVTYPE2,"EXTREME WINDCHILL","EXTREME COLD/WIND CHILL")

data$EVTYPE2[str_detect(data$EVTYPE2,"BLIZZARD")] <- "BLIZZARD"

data$EVTYPE2[str_detect(data$EVTYPE2,"HURRICANE")] <- "HURRICANE (TYPHOON)"
data$EVTYPE2[str_detect(data$EVTYPE2,"TYPHOON")] <- "HURRICANE (TYPHOON)"
data$EVTYPE2[str_detect(data$EVTYPE2,"FLASH FLOOD")] <- "FLASH FLOOD"
data$EVTYPE2 <- str_replace(toupper(data$EVTYPE2),"CSTL","COASTAL")
data$EVTYPE2[str_detect(toupper(data$EVTYPE2),"COASTAL FLOOD")] <- "COASTAL FLOOD"
data$EVTYPE2[str_detect(toupper(data$EVTYPE2),"MAJOR FLOOD")] <- "FLOOD"
data$EVTYPE2[str_detect(toupper(data$EVTYPE2),"RIVER F")] <- "FLOOD"
data$EVTYPE2[str_detect(toupper(data$EVTYPE2),"STORM SURGE")] <- "STORM SURGE/TIDE"
data$EVTYPE2[str_detect(toupper(data$EVTYPE2),"^FOG$")] <- "DENSE FOG"
data$EVTYPE2[str_detect(toupper(data$EVTYPE2),"HEAVY SURF")] <- "HIGH SURF"
data$EVTYPE2[str_detect(toupper(data$EVTYPE2),"HEAT WAVE")] <- "EXCESSIVE HEAT"
data$EVTYPE2[str_detect(toupper(data$EVTYPE2),"HIGH WIND/COLD")] <- "COLD/WIND CHILL"
data$EVTYPE2[str_detect(toupper(data$EVTYPE2),"RECORD COLD")] <- "EXTREME COLD/WIND CHILL"
data$EVTYPE2[str_detect(toupper(data$EVTYPE2),"EXTREME COLD/WIND")] <- "EXTREME COLD/WIND CHILL"



#----
no_match2 <- data$EVTYPE2[!(data$EVTYPE2 %in% toupper(event_type_table$Event.Name))]
length(no_match2)

sorted_events2 <- sort(table(data$EVTYPE2),decreasing = T)
head(sorted_events2,40)        # List TOP 40 Event Types
top_freq_event2 <- names(head(sorted_events,40)) # Store the Names
# The name of the Events that missmatch Table and belong to TOP 25 Events occur
top_freq_event2[!(top_freq_event %in% toupper(event_type_table$Event.Name))]





fatalities_type_decade <- data %>%  #filter(decade == "2000s") %>%
      group_by(EVTYPE2, decade) %>% summarise(sum = sum(FATALITIES, na.rm = F)) %>%
      arrange(decade, desc(sum)) %>% pivot_wider(names_from = decade,
                                                 values_from = sum,
                                                 values_fill = 0) %>%
      mutate(total = sum(`1950s` + `1960s` + `1970s` + `1980s` + `1990s` +
                               `2000s` + `2010s`)) %>%
      arrange(desc(total))

 fatalities_decade <- data %>%  #filter(decade == "2000s") %>%
      group_by(decade) %>% summarise(sum = sum(FATALITIES, na.rm = T)) %>%
      pivot_wider(names_from = decade,
                  values_from = sum,
                  values_fill = 0) %>%
      mutate(total = sum(`1950s` + `1960s` + `1970s` + `1980s` + `1990s` +
                               `2000s` + `2010s`)) %>%
      arrange(desc(total))


injuries_type_decade <- data %>%  #filter(decade == "2000s") %>%
      group_by(EVTYPE2, decade) %>% summarise(sum = sum(INJURIES, na.rm = F)) %>%
      arrange(decade, desc(sum)) %>% pivot_wider(names_from = decade,
                                                 values_from = sum,
                                                 values_fill = 0) %>%
      mutate(total = sum(`1950s` + `1960s` + `1970s` + `1980s` + `1990s` +
                               `2000s` + `2010s`)) %>%
      arrange(desc(total))

injuries_decade <- data %>%  #filter(decade == "2000s") %>%
      group_by(decade) %>% summarise(sum = sum(INJURIES, na.rm = T)) %>%
      pivot_wider(names_from = decade,
                  values_from = sum,
                  values_fill = 0) %>%
      mutate(total = sum(`1950s` + `1960s` + `1970s` + `1980s` + `1990s` +
                               `2000s` + `2010s`)) %>%
      arrange(desc(total))

names = injuries_type_decade$EVTYPE2
injuries_type_decade <- injuries_type_decade[,-1]
colSums(head(scale(injuries_type_decade, center = F, scale = injuries_decade),10))


scale(injuries_type_decade,center = F, scale = injuries_decade)

data %>%  group_by(EVTYPE2, injur_intervals) %>% summarise(freq = n()) %>% 
      arrange(desc(freq)) %>% 
      pivot_wider(names_from = injur_intervals, values_from = freq)

## Conteo total de casos fatales por rangos y décadas
fatal_dec <- with(data, table(
      cut(FATALITIES, breaks = health_cut_breaks, labels = health_cut_labels,
            righ = F, ordered_result = T),
      cut(BGN_YEAR, breaks = decade_breaks,labels = decade_labels,
            right = F, ordered_result = T)
      )
)

fatal_dec

data[which.max(data$FATALITIES),]


fatal_lust <- with(data, table(
      cut(FATALITIES, breaks = health_cut_breaks, labels = health_cut_labels,
          righ = F, ordered_result = T),
      cut(BGN_YEAR, breaks = lustrum_breaks,labels = lustrum_labels,
          right = F, ordered_result = T)
))

fatal_lust


## % Casos por volumen de eventos Fatales
round(scale(
      fatal_dec,
      center = F,
      scale = as.vector(total_events / 100)),5)


## Conteo de heridos por rango y década
injur_dec <- with(data, table(
      cut(INJURIES, breaks = health_cut_breaks, labels = health_cut_labels,
          righ = F, ordered_result = T),
      cut(BGN_YEAR, breaks = decade_breaks,labels = decade_labels,
          right = F, ordered_result = T)
))
injur_dec

data[which.max(data$INJURIES),]


## % Casos por volumen de eventos con heridos
round(scale(
      injur_dec,
      center = F,
      scale = as.vector(total_events / 100)),5)


## Number of events splitted by PROPERTY damage range and decade
prop_damage <- with(data, table(
      cut(property_damage, breaks = damage_cut_breaks, labels = damage_cut_labels,
          righ = F, ordered_result = T),
      cut(BGN_YEAR, breaks = decade_breaks,labels = decade_labels,
          right = F, ordered_result = T)
))
prop_damage

## Number of events splitted by CROP damage range and decade
crop_damage <- with(data, table(
      cut(crop_damage, breaks = damage_cut_breaks, labels = damage_cut_labels,
          righ = F, ordered_result = T),
      cut(BGN_YEAR, breaks = decade_breaks,labels = decade_labels,
          right = F, ordered_result = T)
))
crop_damage

## % Casos por volumen de eventos con heridos
round(scale(
      prop_damage,
      center = F,
      scale = as.vector(total_events / 100)),5)

## % Casos por volumen de eventos con heridos
round(scale(
      crop_damage,
      center = F,
      scale = as.vector(total_events / 100)),5)


                
health_by_evtype <-data %>% group_by(EVTYPE) %>% 
      summarise(fatal = sum(FATALITIES, na.rm = T),
                inju = sum(INJURIES, na.rm = T),
                total = fatal + inju) %>% 
      arrange(desc(total)) %>% filter(total >0)

health_by_state <-data %>% group_by(STATE, COUNTYNAME) %>% 
      summarise(fatal = sum(FATALITIES, na.rm = T),
                inju = sum(INJURIES, na.rm = T),
                total = fatal + inju) %>% 
      arrange(desc(total)) %>% filter(total >0)

health_data <- data %>% #select(BGN_DATE,STATE,FATALITIES,INJURIES) %>% 
      pivot_longer(cols = c("FATALITIES","INJURIES"),
                   names_to = "TYPE_HEALTH_AFFECT", 
                   values_to = ("HEALTH_IMPACT"))

## BoxPlot number of health effect (log10 scale) by type of impact
health_data %>% mutate(decade = factor(year(BGN_DATE) %/% 10*10)) %>% ggplot() +
      geom_boxplot(aes(x = decade, y = HEALTH_IMPACT), alpha = 0.5)+
      facet_wrap(.~ TYPE_HEALTH_AFFECT) +
      scale_y_log10()

## Evolution total health impacts per year
health_data %>% mutate(year = year(BGN_DATE)) %>% 
      group_by(year, TYPE_HEALTH_AFFECT) %>% 
      summarise(sum = sum(HEALTH_IMPACT, na.rm = T)) %>% 
      ggplot() +
      geom_col(aes(x = year, y = sum, group = decade),
               alpha = 0.5,
               position = "stack") +
      geom_smooth(
            method = "loess",
            aes(x = year, y = sum),
            color = "red",
            size = .5,
            linetype = "dashed"
      ) +
      facet_grid(.~TYPE_HEALTH_AFFECT) +
      theme(axis.text.x = element_text(angle = 90,vjust = .5,size = 7))

## Mapa de calor Año vs Estado vs HEalth
health_data %>% mutate(decade = factor(year(BGN_DATE))) %>% 
      group_by(decade, STATE,TYPE_HEALTH_AFFECT) %>% 
      filter(TYPE_HEALTH_AFFECT=="FATALITIES") %>% 
      summarise(sum = sum(HEALTH_IMPACT, na.rm = T)) %>%
      ggplot(aes(x = decade, y = STATE,fill = sum, label = sum) +
      geom_tile() +
      geom_text(aes(label=sum), size=1, color="white") +
#       facet_grid(.~TYPE_HEALTH_AFFECT, scales = "free", shrink = F) +
      theme(axis.text.x = element_text(angle = 90,vjust = .5,size = 7),
            axis.text.y = element_text(size = 7))+
      scale_fill_distiller(palette = "Reds", direction = 1)


data %>% ggplot()+
      geom_line(aes(x= year(BGN_DATE), y = FATALITIES))



## Tablas de resumen 
table(nchar(data$BGN_TIME))
head(data$BGN_TIME,10)
tail(data$BGN_TIME,10)

Sys.timezone()
OlsonNames()

head(as.data.frame(sort(table(data$EVTYPE), decreasing = T)),15)

count_evtype <- with(data,table(FATALITIES, EVTYPE, useNA = "no"))
head(as.data.frame(sort(with(data,table(FATALITIES, EVTYPE)), decreasing = T)),15)
head(as.data.frame(sort(table(data$INJURIES), decreasing = T)),15)
head(as.data.frame(sort(table(data$PROPDMG), decreasing = T)),15)
head(as.data.frame(sort(table(data$PROPDMGEXP), decreasing = T)),15)
head(as.data.frame(sort(table(data$CROPDMG), decreasing = T)),15)
head(as.data.frame(sort(table(data$CROPDMGEXP), decreasing = T)),15)

summary(data$FATALITIES[data$FATALITIES>0])
with(data,hist(log10(FATALITIES[FATALITIES >0]),freq = T, breaks = 20))
with(data,hist(log10(INJURIES[INJURIES >0]),freq = T, breaks = 20))
hist((data$FATALITIES[data$FATALITIES>0]), freq = F,breaks = 30)
hist((data$FATALITIES[data$INJURIES>0]), breaks = 20)

# Map USA
ggplot(my_state, aes(long, lat, group = group)) +
      geom_polygon(fill = "white", colour = "grey50") + 
      coord_quickmap()
