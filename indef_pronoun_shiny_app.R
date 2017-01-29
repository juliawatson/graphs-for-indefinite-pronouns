library(ggplot2)
library(shiny)

language.labs = c('ar','bg','bs','cz','da','de','el','en','es','et','fi',
                  'fr', 'he', 'hr', 'hu', 'id', 'it', 'ja', 'nl', 'no', 'pl', 'pr', 'ro',
                  'ru', 'sl','sr','sv','tr','vi','zh')

ui <- pageWithSidebar(
  
  headerPanel('OCMDS by Language'),
  sidebarPanel(
    
    selectInput('graph_labels', 'Label Language', language.labs, selected=language.labs[8]),
    selectInput('onto_cat', 'Ontological Category', c("thing", "body"), selected="thing")
  ),
  mainPanel(
    plotOutput('plot1')
  )
)

server <- function(input, output, session) {
  
  name <- 'oc_SPLIT'
  folder <- '.'
  fp <- sprintf('%s/%s.csv',folder, name)
  
  ##############
  # parameters #
  ##############
  onto = reactive(input$onto_cat)
  # ontological category to consider
  ndims = 2
  # minimum number of situations a term must be applicable to
  parameters = reactive(sprintf('onto=%s_dim=%d', onto(), ndims))

  #############
  # read data #
  #############
  original.data = read.csv(sprintf('%s_gold.csv', name))
  selected.sits = reactive(original.data$ontological == onto())

  
  
  full.data = read.csv(fp,header=TRUE)

  coordinates.filename = reactive(sprintf('%s/%s_%s.csv', folder, name, parameters()))
  coordinates = reactive(read.csv(coordinates.filename(), sep = ',', header = TRUE))
  labels.fp = sprintf('%s/%s_labels.csv', folder, name)
  labels = read.csv(labels.fp, encoding = "UTF-8")
  sit_labels = reactive(labels[selected.sits(),])
  

  
  non_blank_sits = reactive({sit_labels()[, which(language.labs %in% input$graph_labels) + 1]} != '')

  language_labels <- reactive(sit_labels()[, which(language.labs %in% input$graph_labels) + 1][non_blank_sits()])
  
  x_coords <- reactive({coordinates()$dim.1[non_blank_sits()]})
  y_coords <- reactive({coordinates()$dim.2[non_blank_sits()]})

  
  output$plot1 <- renderPlot({
    print(length(language_labels()))
    print(length(x_coords()))
    qplot(x_coords(), y_coords(), color = language_labels(), label = language_labels()) +
            theme(axis.title.x = element_blank(), axis.title.y = element_blank())
    })
  
}

shinyApp(ui, server)

