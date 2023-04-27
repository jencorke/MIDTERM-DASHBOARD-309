##
## Midterm 
## Jennifer Corke

library(tidyverse)
library(readr)
library(ggthemes)


math_achievement <- 
  read_csv("https://www2.ed.gov/about/inits/ed/edfacts/data-files/math-achievement-lea-sy2020-21.csv")
rla_achievemet <- 
  read_csv("https://www2.ed.gov/about/inits/ed/edfacts/data-files/rla-achievement-lea-sy2020-21.csv")

## I begin by doing some data manipulation.
filter_math <- math_achievement %>%
  filter(GRADE == "All Grades", CATEGORY == "ALL", NUMVALID >= 1000)

filter_rla <- rla_achievemet %>%
  filter(GRADE == "All Grades", CATEGORY == "ALL", NUMVALID >= 1000)

math_st_avg <- filter_math %>%
  group_by(STNAM) %>%
  summarise(avg_math_proficiency = mean(PCTPROF, na.rm = TRUE))

rla_st_avg <- filter_rla %>%
  group_by(STNAM) %>%
  summarise(avg_rla_proficiency = mean(PCTPROF, na.rm = TRUE))

top_3_math <- math_st_avg %>%
  arrange(desc(avg_math_proficiency)) %>%
  head(3)


bottom_3_math <- math_st_avg %>%
  arrange(avg_math_proficiency) %>%
  head(3)

top_3_rla <- rla_st_avg %>%
  arrange(desc(avg_rla_proficiency)) %>%
  head(3)

bottom_3_rla <- rla_st_avg %>%
  arrange(avg_rla_proficiency) %>%
  head(3)

## For my first plot, I will filter the RLA data and find the mean proficiency scores for each state.
rla_rec_filt <- rla_achievemet %>%
  filter(CATEGORY == "ALL", GRADE=="00", NUMVALID >=1000) %>%
  select(CATEGORY, GRADE , NUMVALID, STNAM, PCTPROF) %>%
  group_by(STNAM) %>%
  mutate(PCTPROF = as.numeric(PCTPROF)) %>%
  summarize(rla_prof = mean(PCTPROF, na.rm = TRUE))
view(rla_rec_filt)  

##Now, I will make a data frame for RLA scores that are 35 or over and merge the map and RLA data.
rla_rec_filt_2 <- rla_rec_filt %>%
  mutate(High_scores=rla_prof>=35)

view(rla_rec_filt_2)

us_states <- map_data("state") 
glimpse(us_states)

country_map_scores_rla <- rla_rec_filt_2 %>%
  mutate(state = str_to_lower(STNAM)) %>%
  left_join(us_states, by=c("state" = "region"))
view(country_map_scores_rla)

## Finally, I create a map that shows the states that have a mean RLA proficiency above or equal to 35. 
ggplot(country_map_scores_rla) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=`High_scores`), color="gray7", size=0.1) +
  scale_fill_manual(values=c("lightblue1", "firebrick1")) +
  coord_map() +
  theme_map() +
  labs(title = "Reading and Language Arts Proficiency in the US in 2020-2021",
       subtitle = "States scoring a 35 or above are highlighted in Red, below in Blue",
       caption = "The state of Washington is not included in this map as they did not submit data.") +
  theme(legend.position = "none",
        plot.title = element_text(face= "bold"))


## For my next plot, I am filtering Michigan and Ohio in the RLA data to compare the RLA proficiency scores.


ohmi_map <- country_map_scores_rla%>%
  filter(STNAM %in% c("OHIO", "MICHIGAN")) %>%
  mutate(State = str_to_title(STNAM)) ##str to title capitalizes

view(ohmi_map)

## Now I create a box-plot to compare the RLA score between Michigan and Ohio.
str(ohmi_map)
boxplot(rla_prof~STNAM,
        data=ohmi_map,
        main="Michigan vs Ohio Reading and Language Arts Proficiency Score in 2020-2021",
        xlab="State",
        ylab=" RLA Proficiency",
        col="orange",
        border="brown"
) 




## For my next plot, I am filtering the math record data and once again calculating the individual 
## states mean proficiency scores.
math_record <- math_achievement %>%
  filter(CATEGORY == "ALL", GRADE=="00", NUMVALID >=1000) %>%
  select(CATEGORY, GRADE , NUMVALID, STNAM, PCTPROF) %>%
  group_by(STNAM) %>%
  mutate(PCTPROF = as.numeric(PCTPROF)) %>%
  summarize(Math_profic = mean(PCTPROF, na.rm = TRUE))
view(math_record)  

##Now, I filter the math data to be scores of 35 and over.
math_rec_filt2 <- math_record %>%
  mutate(High_scores=Math_profic>=35)

view(math_rec_filt2)

## I now will mutate the math and country data together to be able to make a map.
us_states <- map_data("state") 
glimpse(us_states)

map_math <- math_rec_filt2 %>%
  mutate(state = str_to_lower(STNAM)) %>%
  left_join(us_states, by=c("state" = "region"))
view(map_math)

## Creating a map that shows the states that have a mean math proficiency above or equal to 35. 
ggplot(map_math) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=`High_scores`), color="gray7", size=0.1) +
  scale_fill_manual(values=c("lightblue1", "firebrick1")) +
  coord_map() +
  theme_map() +
  labs(title = "Math Proficiency in the US in 2020-2021",
       subtitle = "States scoring a 35 or above are highlighted in Red, below in Blue.",
       caption = "Washington is not included in this map as they did not submit data.") +
  theme(legend.position = "none",
        plot.title = element_text(face= "bold"))
 


















