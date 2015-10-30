library(dplyr)
library(parsetR)
library(htmlwidgets)
library(swiRcharts)

###		SETTINGS   ###
data.sub.file <- "input/data.csv"
translation.file <- "input/translation_asylumFlowsEU.csv"


df <- read.csv(data.sub.file)
txt <- read.csv(translation.file, row.names = 1, stringsAsFactors = F)


for(lang in colnames(txt)) {
  dd <- df
  tmp_html <- paste0("flow_EU_tmp_", lang, ".html")
  output.html <- paste0("asylumApplicationsFlow_EU_", lang, ".html")

  colnames(dd)[match(c("citizen", "geo"), colnames(dd))] <- txt[c("origins", "destinations"), lang]

  colNums <- match(c("values", txt[c("origins", "destinations"), lang]),colnames(dd))

  ps.chart <- parset(select(dd, colNums),
    dimensions =  txt[c("origins", "destinations"), lang],
    # use some JavaScript to inform parset that Freq has the value
    # http://www.buildingwidgets.com/blog/2015/9/17/week-37-parsetr
    value = htmlwidgets::JS("function(d){return d.values}"),
    tension = 0.7, width = "100%", height = 500,
    spacing = 15, duration = 300)

  # overwite default padding
  ps.chart$sizingPolicy$browser$padding <- 5
  saveWidget(ps.chart, file = tmp_html, selfcontained = FALSE, libdir = "js")

  swi_widget(tmp_html, output.html,
    h2 = txt["title",lang],
    descr = txt["description",lang], 
    h3 = txt["subtitle",lang],
    source = paste0(txt["source",lang], ": ", htmlLink("http://ec.europa.eu/eurostat/en/web/products-datasets/-/MIGR_ASYAPPCTZM", txt["eurostat",lang])),
    author = paste0("Duc-Quang Nguyen | ", htmlLink("http://www.swissinfo.ch", "swissinfo.ch")),
    footer = txt["description2",lang]
  )
}



