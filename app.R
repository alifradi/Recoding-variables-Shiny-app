library(shiny)
library(stringr)
library(dplyr)
library(tidyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  fileInput('file0', 'Upload your  RDS File here'),
  uiOutput('vars'),
  uiOutput("levelsInput"),
  tableOutput('show_inputs'),
  
  splitLayout(tableOutput('table'),
              tableOutput('tableEcoded'))
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  Data <- reactive({
    inFile <- input$file0
    if (is.null(inFile)) return(NULL)
    a<-readRDS(inFile$datapath)
    a<-as.data.frame(a)
    return(a)
  })
  output$vars <- renderUI({
    data <- Data()
    selectInput(label = 'Variables', inputId = 'toRec',choices = names(data))
  })
  output$levelsInput <- renderUI({
    x<-Data()
    x<-x[,input$toRec]
    # Section get factor's levels ------------------------------
    
    out <- "<table><tbody>"
    if (is.factor(x) )
    {levs <- levels(x)}else
    { levs <- stats::na.omit(unique(x))}
    if (any(is.na(x))) {levs <- c(levs, NA)}
    
    # Section Generate table ------------------------------
    
    for (l in levs) {
      out <- paste0(out, "<tr>")
      out <- paste0(out, "<td class=\"right vertical-align\">",
                    htmltools::htmlEscape(l),
                    "&nbsp;<span class=\"glyphicon glyphicon-arrow-right left-sep\" aria-hidden=\"true\"></span> &nbsp;</td>")
      label <- l
      l <- gsub(":", "_", l)
      id <- paste0("ireclev_", l)
      if (id == "ireclev_NA") {
        label <- "NA"
      }
      if (id == "ireclev_") {
        label <- ""
      }
      id=paste(id,'_',input$toRec,'_')
      out <- paste0(out, "<td class=\"vertical-align\">",
                    textInput(id, "", label), "</td>")
      out <- paste0(out, "</tr>")
    }
    out <- paste0(out, "</tbody></table>")
    HTML(out)
  })
  output$table <- renderTable({
    data<-Data()
    if (is.null(data)) return(NULL)
    data
  })
  output$tableEcoded <- renderTable({
    data<-Data()
    x <- data[,input$toRec]
    y<- input$toRec
    
    choices <- AllInputs()
    
    
    txt <- paste('data$',y, ' <- recode(data$',y, sep = '')
    for (lev in 1:nrow(choices)) {
      n <-choices[lev,'New']
      o <-choices[lev,'Old']
      txt <- paste(txt,',' , o,' = ',"'",n,"'",sep = '')
    }
    txt <- paste(txt, ')',sep = '')
    eval(parse(text=txt))
    return(data)
  })
  AllInputs <- reactive({
    Inputs <- c()
    InputValue <-c()
    for(i in 1:length(names(input))){
      if(str_detect(names(input)[i],'irec')){
        Inputs[i] <- names(input)[i]
        InputValue[i] <- input[[names(input)[i]]]
      }
      
      #InputValue[i] <- input[[names(input)[i]]]
      
    }
    df <- data.frame(In = Inputs, Val = InputValue)
    df<-df %>% filter(!is.na(In)) %>%
      mutate(Feature = str_detect(In,input$toRec)) %>%
      filter(Feature==TRUE)
    p <- separate(df,In, into =c('A','OldVal','B'), sep = '_' )
    
    
    return(data.frame(Input = df$In,Old = p$OldVal,New = df$Val, selected = df$Feature))
  })
  output$show_inputs <- renderTable({
    data<-AllInputs()
    data
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)