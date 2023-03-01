library(shiny)
pacman::p_load(shiny, sf, tmap, tidyverse)

mpsz <- st_read(dsn = "data/geospatial", 
                layer = "MP14_SUBZONE_WEB_PL")

popdata <- read_csv("data/aspatial/respopagesextod2011to2020.csv")

popdata2020 <- popdata %>%
  filter(Time == 2019) %>%
  group_by(PA, SZ, AG) %>%
  summarise(`POP` = sum(`Pop`)) %>%
  ungroup()%>%
  pivot_wider(names_from=AG, 
              values_from=POP) %>%
  mutate(YOUNG = rowSums(.[3:6])
         +rowSums(.[12])) %>%
  mutate(`ECONOMY ACTIVE` = rowSums(.[9:13])+
           rowSums(.[15:17]))%>%
  mutate(`AGED`=rowSums(.[18:22])) %>%
  mutate(`TOTAL`=rowSums(.[5:22])) %>%  
  mutate(`DEPENDENCY` = (`YOUNG` + `AGED`)
         /`ECONOMY ACTIVE`) %>%
  mutate_at(.vars = vars(PA, SZ),
            .funs = funs(toupper)) %>%
  select(`PA`, `SZ`, `YOUNG`, 
         `ECONOMY ACTIVE`, `AGED`, 
         `TOTAL`, `DEPENDENCY`)

mpsz_pop2019 <- left_join(mpsz, popdata2020,
                          by = c("SUBZONE_N" = "SZ"))

ui <- fluidPage(
  titlePanel("Choropleth Mapping System"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "variable",
                  label = "Mapping Variable",
                  choices = c("Young" = "YOUNG",
                              "Economy Active" = "ECONOMY ACTIVE",
                              "Aged" = "AGED",
                              "dependency" = "DEPENDENCY"),
                  selected = "DEPENDENCY"),
      selectInput(inputId = "classification",
                  label = "Classification Method",
                  choices = list("pretty" = "pretty",
                              "quantile" = "quantile",
                              "equal" = "equal",
                              "jenks" = "jenks",
                              "sd" = "sd",
                              "kmeans" = "kmeans",
                              "hclust" = "hclust",
                              "bclust" = "bclust",
                              "fisher" = "fisher"),
                  selected = "pretty"),
      sliderInput(inputId = "classes",
                  label = "Number of Classes",
                  min = 6,
                  max = 12,
                  value = c(6)),
      selectInput(inputId = "colour",
                  label = "Colour Scheme",
                  choices = list("reds" = "Reds",
                              "blues" = "Blues",
                              "greens" = "Greens",
                              "Yellow-Orange-Red" = "YlOrRd"),
                  selected = "YlOrRd")
        ),
    mainPanel(
      tmapOutput("mapPlot",
                 width = "100%",
                 height = 400)
    )
  )
)


server <- function(input, output) {
  output$mapPlot <- renderTmap({
    tmap_options(check.and.fix = TRUE) +
      tm_shape(mpsz_pop2019) + 
      tm_fill(input$variable, 
              n = input$classes,
              style = input$classification,
              palette = input$colour) +
      tm_borders(lwd = 0.1, alpha = 1) +
      tm_view(set.zoom.limits = c(11, 14)
              )
      })
}
    
    shinyApp(ui = ui, server = server)
    