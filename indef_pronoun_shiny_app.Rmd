---
runtime: shiny
output: html_document
---

# Something more about indefinite pronouns
Code and data for the CogSci 2017 conference paper on indefinite pronouns

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r echo=FALSE}
library(ggplot2)
library(shiny)

language.labs = c('ar','bg','bs','cz','da','de','el','en','es','et','fi',
                  'fr', 'he', 'hr', 'hu', 'id', 'it', 'ja', 'nl', 'no', 'pl', 'pr', 'ro',
                  'ru', 'sl','sr','sv','tr','vi','zh')

ui <- pageWithSidebar(
  
  headerPanel('OCMDS of Indefinite Pronouns by Language'),
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

shinyApp(ui, server, options = list(width="100%", height = 550))


```

Breakdown of scripts and how to use them by section of paper:

* Method
	- create_dev_test_split.py
		- usage: python create_dev_test_split.py
		- description: Splits exemplars in data/full_set.csv into a dev_set and test_set of equal sizes and writes them to data/dev_set.tsv and data/test_set.csv.

* Are the functions at the right level of granularity?
	- analyze_clustering.py
		- usage: python analyze_clustering.py data/test_set.tsv data/stemming_dictionary.csv
		- dependencies: data.py
		- description: Prints a clustering summary for each parameter setting (see description of parameters in the class docstring for the data class in data.py). Each summary consists of the Adjusted Rand Score with the Haspelmath gold labels followed by a confusion matrix between Haspelmath functions and clusters found by the clustering algorithm.

* The perspective of a similarity space
	- data.py
		- usage: python data.py data/test_set.tsv data/stemming_dictionary.csv
		- description: restructures data as input for oc.r, and writes them to files oc_SPLIT_labels.csv, oc_SPLIT_gold.csv, and oc_SPLIT.csv.
	- oc.r
		- usage: Rscript oc.r
		- description: Generates OCMDS plots based on info in oc_SPLIT_labels.csv, oc_SPLIT_gold.csv and oc_SPLIT.csv.

Other scripts used in our research

* Semantic maps

	- graph_inferring.py
		- usage: python graph_inferring.py
		- dependencies: data.py, graph_parameters.py (see graph_parameters.py file to modify behavior)
		- description: Infers a graph and writes it to files sample_edges.csv and sample_labels.csv, which are inputs for draw_regier_graph.R. It also prints a summary of how much each edge increased the angluin score.
	- draw_regier_graph.R
		- usage: Rscript draw_regier_graph.R
		- description: Running this script creates file sample_graph.pdf from sample_edges.csv and sample_labels.csv. Before running this script, run graph_inferring.py.




