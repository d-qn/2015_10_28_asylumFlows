library(dplyr)
library(tidyr)
library(magrittr)
library(parsetR)
library(htmlwidgets)
library(swiRcharts)

###		SETTINGS   ###

citizenAgg <- c("Total", "European Union (28 countries)", "Extra EU-28")
iso2agg <- c("EU28", "TOTAL")
data_2015.file <- "data/data_2015only.csv"

###		Load data
data.all <- read.csv(data_2015.file)


###		Aggregate data by geo & citizen
data <- data.all %>% filter(!citizen %in% citizenAgg, !iso2 %in% iso2agg) %>%
  group_by(iso2, geo, citizen) %>% dplyr::summarise(tot = sum(values, na.rm = T )) %>% ungroup()

# remove coutry of origin always 0
citToRemove <- as.character(unlist(data %>% group_by(citizen) %>% dplyr::summarise(grandTot = sum(tot, na.rm = T)) %>% filter(grandTot == 0) %>% select(citizen)))
data %<>% filter(!citizen %in% citToRemove)


### Get the top country of origins and destinations

# find the n largest countries during the last year
ntop <- 6

sumByGeo <- data %>% group_by(iso2) %>% dplyr::summarise(sumByGeo = sum(tot, na.rm = T)) %>% ungroup()
iso2.top <- as.character(unlist(head(as.data.frame(sumByGeo[order(sumByGeo$sumByGeo, decreasing = T),'iso2']), ntop)))
iso2.sub <- c('DE', 'CH', 'SE', 'HU', 'IT', 'FR', 'AT', 'UK')


sumByCit <- data %>% group_by(citizen) %>% dplyr::summarise(sumByCit = sum(tot, na.rm = T)) %>% ungroup()
cit.top <- as.character(unlist(head(as.data.frame(sumByCit[order(sumByCit$sumByCit, decreasing = T),'citizen']), ntop)))


### Merge not top countries
df <- data
df$iso2 <- ifelse(as.character(df$iso2) %in% iso2.sub, as.character(df$iso2), 'other')
df$citizen <- ifelse(as.character(df$citizen) %in% cit.top, as.character(df$citizen), 'other')

df %<>% group_by(iso2, citizen) %>% dplyr::summarise (values = sum(tot, na.rm = T)) %>% ungroup()


# add back the country names to merged iso2
df$geo <- as.character(unlist(data[match(df$iso2, data$iso2),'geo']))
df[which(df$iso2 == "other"), 'geo'] <- "other"


# drop unused levels
df$citizen <- factor(df$citizen)
df$iso2 <- factor(df$iso2)

citLength <- unlist(unique(df %>% group_by(iso2) %>% dplyr::summarise(nelem = length(values)) %>% select(nelem)))
geoLength <- unlist(unique(df %>% group_by(citizen) %>% dplyr::summarise(nelem = length(values)) %>% select(nelem)))

stopifnot(geoLength == length(iso2.sub) + 1, citLength == ntop + 1)

## tmp hack for long country names
df$geo <- gsub("\\(.*\\)", "", df$geo)
df$citizen <- gsub("\\(.*\\)", "", df$citizen)


# arr <- as.table(array(df$values, dim = c(citLength, geoLength),
#   dimnames = list(Origins = as.character(unique(df$citizen)),
#   Destinations = as.character(unique(df$geo)))))

# tooltipParset <- htmlwidgets::JS('function(d){
#   var count = d.count, path = [];
#   while (d.parent) {
#     if (d.name) path.unshift(d.name);
#     d = d.parent;
#   }
#   return path.join(" → ") + "<br>" + comma(count) + " (" + percent(count / d.count) + ")";
# }')
# categoryTooltip <- htmlwidgets::JS(
#   'function(d) {
#   return d.name + "<br>" + comma(d.count) * 100+ " (" + percent(d.count / d.dimension.count) + ")";
#   }'
# )

ps.chart <- parset(arr / 10, dimensions = c("Origins", "Destinations"),
  tension = 0.6, width = "100%", height = 550,
  spacing = 5, duration = 300, tooltip = tooltipParset)

saveWidget(ps.chart, file = "parsetR_test.html",
  selfcontained = FALSE, libdir = "js")

swi_widget("parsetR_test.html")


ps.chart2 <- parset(select(df, values, citizen, geo), dimensions = c("citizen", "geo"),
  # use some JavaScript to inform parset that Freq has the value
  value = htmlwidgets::JS("function(d){return d.values}"),
  tension = 0.6, width = "100%", height = 550,
  spacing = 5, duration = 300)


saveWidget(ps.chart2, file = "parsetR_test2.html",
           selfcontained = FALSE, libdir = "js")

swi_widget("parsetR_test2.html")


# test <- parset(Titanic * 1000, tension = 0.5, width = "100%", height = 500)
# saveWidget(test, file = "parsetR_test2.html", selfcontained = FALSE, libdir = "js")

