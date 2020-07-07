
#### NEW DATA ####
#This time I don't want overall numbers but the average for each state. So 
#Read in the data again. I have the scores and the demo graphics so after reading in the 
#data I will merge them together to get one complete data set. 
va <- read_dta("C:/Users/Jeff/Documents/Jeff/HEAL_lab/Summer/HEAL/va_overall.dta")
df_new <- read_dta("C:/Users/Jeff/Documents/Jeff/HEAL_lab/Summer/HEAL/new_data.dta")
va_merge <- merge(df_new,va, by="therapist_id",all.x = T)
summary(va_merge$wgt_therapist_effect)
# I'm going to create new variables using summarize in dplyr that take the mean 
# and variance of the therapist effect. They are saved as avg_va and var_va respectfully

va_use <- va_merge %>% 
  group_by(client_demo_state)%>%
  summarize(avg_va = mean(wgt_therapist_effect, na.rm = TRUE),
            var_va = var(wgt_therapist_effect, na.rm = TRUE))%>%
  print(var_va)%>%
  ungroup()
summary(va_merge$wgt_therapist_effect)

# To be able to merge the data to the map data set that has zip codes I need all
# the state demographics as a lower case name and not the abreviations. I found
# a function online that will help me to do that. 
stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}

#Now I'm applying the function onto my data set and saving it as a new variable 'data'
data <- stateFromLower(va_use$client_demo_state)
data <- as.data.frame(data)
data <- cbind.data.frame(data,va_use$avg_gains,va_use$var_gains)
summary(data$`va_use$avg_va`)
data$region <- tolower(data$data)
#AS I'm doing the heat map I'll neep these packages to get the map of the US. 
library(mapproj)
library(evaluate)
library(ggplot2)
library(maps)
states <- map_data("state")
map.df <- merge(states,data, by="region", all.x=T)
map.df <- map.df[order(map.df$order),]
# this 'b' helps to get a range that I can adjust for the heatmap values. 
#b = c(-.15,.6)
b = c(-.1,.4)

#Here I'm graphing the heat map of the VA across the US and saving the graph as a variable. 
blue_va_graph <-ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill = `va_use$avg_va`))+
  geom_path()+
  #geom_text(data= va_use, aes( group=NA, label=avg_va), 
  #size=2.5, vjust=0.5, hjust=0.5)+
  scale_fill_gradientn(name = "Average Therapist Value Added by State",
                       colours=blues9,
                       limits = b,
                       na.value="grey90")+
  coord_map()

blue_va_graph

# Again plotting the var of the value added but this time with a violin graph
# I think they look good.
violin_va_graph <-ggplot(va_merge, aes(x = client_demo_state, y= wgt_therapist_effect,fill = client_demo_state)) +
  geom_violin()+
  ggtitle("Ranges of Value Added by State")+
  xlab("State")+
  ylab("Value Added")
violin_va_graph



####
####
# attempts to add text inside the states 
m3 <- blue_va_graph + geom_text(aes(x=long, y=lat, label=avg_va), data=va_use, col="yellow", cex=3)
m3
blue_va_graph

# Here I have graphed the var of the value added with a box plot
var_va_graph <-ggplot(va_merge, aes(x = client_demo_state, y= wgt_therapist_effect,fill = client_demo_state)) +
  geom_boxplot()+
  ggtitle("Ranges of Value Added by State")+
  xlab("State")+
  ylab("Value Added")
var_va_graph
print(map.df$`va_use$var_va`)





#### RAW DATA ####
# I'm doing the same thing as before, but with this code I am looking at raw gains
#added by therapist, and not just value added. 
va <- read_dta("C:/Users/Jeff/Documents/Jeff/HEAL_lab/Summer/HEAL/va_overall_raw.dta")
df_new <- read_dta("C:/Users/Jeff/Documents/Jeff/HEAL_lab/Summer/HEAL/new_data.dta")
va_merge <- merge(df_new,va, by="therapist_id",all.x = T)
summary(va_merge$wgt_therapist_effect)
#Here I create the variables again but from different variables ("zmeangains")

va_use <- va_merge %>% 
  group_by(client_demo_state)%>%
  summarize(avg_gains = mean(zmeangains, na.rm = TRUE),
            var_gains = var(zmeangains, na.rm = TRUE))%>%
  print(var_va)%>%
  ungroup()
summary(va_merge$wgt_therapist_effect)

# I included the function again in case I split up this file at some point, 
# and need it as an object again. 
stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}

#Just to remind byself that I need these put the data into a way that I can use 
#it to make a heat map. 
data <- stateFromLower(va_use$client_demo_state)
data <- as.data.frame(data)
data <- cbind.data.frame(data,va_use$avg_va,va_use$var_va)
summary(data$`va_use$avg_va`)
data$region <- tolower(data$data)
library(mapproj)
library(evaluate)
library(ggplot2)
library(maps)
states <- map_data("state")
map.df <- merge(states,data, by="region", all.x=T)
map.df <- map.df[order(map.df$order),]

b = c(-.1,.4)
blue_va_graph <-ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill = `va_use$avg_gains`))+
  geom_path()+
  #geom_text(data= va_use, aes( group=NA, label=avg_va), 
  #size=2.5, vjust=0.5, hjust=0.5)+
  scale_fill_gradientn(name = "Average Therapist Standardized Gains by State",
                       colours=blues9,
                       limits = b,
                       na.value="grey90")+
  coord_map()

blue_va_graph

# Here I graph the variance of the gains with a bar plot

var_va_graph <-ggplot(va_merge, aes(x = client_demo_state, y= zmeangains ,fill = client_demo_state)) +
  geom_boxplot()+
  ggtitle("Ranges of Standardized Gains by State")+
  xlab("State")+
  ylab("Value Added")
var_va_graph
print(map.df$`va_use$var_va`)

#Here are the graphs for standardized gains by state in a violin

violin_va_graph <-ggplot(va_merge, aes(x = client_demo_state, y= zmeangains,fill = client_demo_state)) +
  geom_violin()+
  ggtitle("Ranges of Standardized Gains by State")+
  xlab("State")+
  ylab("Value Added")
violin_va_graph





#These are attempts at adding the number of VA inside the state. 
install.packages("fifystater")

library(ggplot2)
library(fiftystater)

data("fifty_states") 
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
p <- ggplot(crimes, aes(map_id = state)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = Assault), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  
  
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") + theme(legend.position = "bottom", 
                               panel.background = element_blank())
