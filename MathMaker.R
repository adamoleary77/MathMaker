library(shiny)

### Server ###

server <- function(input, output, session) {
  #render header output from main text box
  output$display <- renderUI({ 
    withMathJax(
        h2('> ',input$mainBox, style= 'font-size:500%')
      )
    })
  
  #Events for non-symbol-input buttons
  #cat MathJax into main text
  observeEvent(input$b1, {updateTextInput(session, 'mainBox', value = paste0(input$mainBox, '\\(\\theta\\)'))})
  observeEvent(input$b2, {updateTextInput(session, 'mainBox', value = paste0(input$mainBox, '\\(\\pi\\)'))})
  observeEvent(input$b3, {updateTextInput(session, 'mainBox', value = paste0(input$mainBox, '\\(\\epsilon\\)'))})
  observeEvent(input$b4, {updateTextInput(session, 'mainBox', value = paste0(input$mainBox, '\\(\\delta\\)'))})
  observeEvent(input$b5, {updateTextInput(session, 'mainBox', value = paste0(input$mainBox, '\\(\\ge\\)'))})
  observeEvent(input$b6, {updateTextInput(session, 'mainBox', value = paste0(input$mainBox, '\\(\\le\\)'))})
  observeEvent(input$b7, {updateTextInput(session, 'mainBox', value = paste0(input$mainBox, '\\(\\infty\\)'))})
  
  #Events for symbol-input buttons
  #cat MathJax into main & clear symbol input box
  observeEvent(input$b8, { #sqare root
    x <- input$tempBox
    if(!x=='' && !x=='syntax error'){
      updateTextInput(session,'mainBox', value = paste0(input$mainBox, '\\(\\sqrt{',x,'}\\)'))
      updateTextInput(session,'tempBox', value = '')}
    else{
      updateTextInput(session,'tempBox', value = 'syntax error')
    }
    })
  observeEvent(input$b9, { #exponent
    x <- input$tempBox
    if(!x=='' && !x=='syntax error'){
      updateTextInput(session,'mainBox', value = paste0(input$mainBox, '\\(^',x,'\\)'))
      updateTextInput(session,'tempBox', value = '')}
    else{
      updateTextInput(session,'tempBox', value = 'syntax error')
    }
  })
  observeEvent(input$b10, { #fraction
    x <- unlist(strsplit(input$tempBox, ','))
    if(length(x) == 2){
      updateTextInput(session,'mainBox', value = paste0(input$mainBox, '\\(\\frac{',x[1],'}{',x[2],'}\\)'))
      updateTextInput(session,'tempBox', value = '')}
    else{
      updateTextInput(session,'tempBox', value = 'syntax error')
    }
  })
}

### User Interface ###

ui <- fluidPage(
  #themeing and title
  theme = "bootstrap_theme.css",
  h1("Math Maker"),
  #active text output
  withMathJax(),
  uiOutput('display'),

  fluidRow(
    #Input boxes column and helptext
    column(4,
      textInput("mainBox", "Main", width = '500px'),
      textInput("tempBox", "Symbol Input", width = '100px'),
      helpText("Type in the Main text box and your text will be translated to the output header."),
      helpText("Use the Symbol Input text box for symbols that require values."),
      helpText("ex) type 2 in the Symbol Input box then click the \\(\\sqrt{x}\\) button."),
      helpText("type numerator then denominator separated by a comma for fractions.")
    ),
    
    #Buttons column
    column(8,
      #Symbol input buttons     
      actionButton('b8', '\\(\\sqrt{x}\\)', width = '50px', style="padding:0px; font-size:200%"),
      actionButton('b9', '$$x^e$$', width = '40px', style="padding:0px; font-size:200%"),
      actionButton('b10', '\\(\\frac{x}{y}\\)', width = '40px', style="padding:0px; font-size:200%"),
      
      #No symbol input buttons
      actionButton('b1', '\\(\\theta\\)', width = '50px', style="padding:0px; font-size:200%"),
      actionButton('b2', '\\(\\pi\\)', width = '40px', style="padding:0px; font-size:200%"),
      actionButton('b3', '\\(\\epsilon\\)', width = '40px', style="padding:0px; font-size:200%"),
      actionButton('b4', '\\(\\delta\\)', width = '40px', style="padding:0px; font-size:200%"),
      actionButton('b5', '\\(\\ge\\)', width = '40px', style="padding:0px; font-size:200%"),
      actionButton('b6', '\\(\\le\\)', width = '40px', style="padding:0px; font-size:200%"),
      actionButton('b7', '\\(\\infty\\)', width = '40px', style="padding:0px; font-size:200%")
    )
))
shinyApp(ui, server)