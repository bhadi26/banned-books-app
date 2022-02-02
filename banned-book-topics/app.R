# Banned Books Shiny App 
library(gutenbergr)
library(tidytext)
library(topicmodels)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(ggthemes)

# set up functionality 
run_lda <- function(titles, n_topics) {
books <- gutenberg_works(title %in% titles) %>% 
    gutenberg_download(meta_fields = "title")

# by chapter
by_chapter = books %>% 
    group_by(title) %>% 
    mutate(chapter = cumsum(str_detect(text, regex("^chapter", ignore_case=TRUE)))) %>% 
    ungroup() %>% 
    filter(chapter > 0) %>% 
    unite(document, title, chapter)

# separate charactres into words 
by_chapter_word = by_chapter %>% 
    unnest_tokens(word, text)

# find document-word counts by chapter 
word_counts = by_chapter_word %>% 
    anti_join(stop_words) %>% 
    count(document, word, sort = TRUE) %>% 
    ungroup() 

# create document-term matrix of word count 
chapters_dtm <- word_counts %>% 
    cast_dtm(document, word, n)

# apply LDA 
chapters_lda <- LDA(chapters_dtm, k = n_topics, control=list(seed=1234))

# per topic word probailities 
chapter_topics <- tidy(chapters_lda, matrix="beta")

# find top 10 terms for each topic 
top_terms <- chapter_topics %>% 
    group_by(topic) %>% 
    top_n(10, beta) %>%
    ungroup() %>% 
    arrange(topic, -beta)

return(top_terms)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Topics for Banned Books - Using Project Gutenberg"),
    h5("Enter a title of a book (banned or not banned) and return a list of topics"),
    h6("Topic modeling using  Latent Dirichlet Allocation (LDA)"),
    h6("Using code from: https://www.andreaperlato.com/mlpost/extract-the-main-topics-from-books/"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("titles", "Title", value = "", width = NULL, placeholder = "Adventures of Huckleberry Finn"),
            numericInput("n_topics",
                        "Number of Topics",
                        4, 
                        min = 2,
                        max = 5,
                        ),
            actionButton("run", "Get Topics"), 
            p(" "),
            p("If error, make sure title is spelled correctly or may not be available on Project Gutenberg"), 
            p("Topics may contain sensitive or culturally explicit language"),
            p("The topics & terms may provide insight into why the book was banned or what aspects of history are trying to be supressed."),
            p("https://www.gutenberg.org")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("topicPlot"), 
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
    observeEvent(input$run, {
        top_terms <- reactive({run_lda(input$titles, input$n_topics)})
        plot_data <- reactive({top_terms() %>% 
                mutate(term=reorder(term,beta))})
        
        # PLOT 
        output$topicPlot <- renderPlot({
            # generate bins based on input$bins from ui.R
            ggplot(plot_data(), aes(term, beta, fill=factor(topic))) + 
                geom_col(show.legend=FALSE) + 
                facet_wrap(~topic, scales="free") + 
                coord_flip() + 
                theme_classic() + 
                ggtitle(paste("Topics for ",input$titles))
        })
        
    })
        
   
}
    

# Run the application 
shinyApp(ui = ui, server = server)



