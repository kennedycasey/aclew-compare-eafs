library(shiny)
source("1-CompareToGS.R")

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Compare ACLEW Annotation Scheme eafs"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Annotation file ----
      fileInput("file1", "Choose YOUR annotation file",
                accept = ".eaf"),
      
      # Input: Annotation file ----
      fileInput("file2", "Choose the GOLD-STANDARD annotation file",
                accept = ".eaf"),

      # Input: Annotated minute ----
      textInput("minute", "Which minute do you want to test? Leave blank to compare full 5min file",
                placeholder = 'Enter a number'),
      
      # Input: Optional ignore dependent tiers ----
      checkboxInput("ignore.dep", "Ignore all dependent tiers (xds, vcm, lex, mwu)?"),

      # Submit button:
      actionButton("submit", "Update")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      uiOutput("report"),
      uiOutput("downloadErrors")
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  report <- eventReactive(input$submit, {
    req(input$file1, input$file2)
    compare.files(input$file1$datapath,
                  input$file2$datapath,
                  as.numeric(input$minute),
                  input$ignore.dep)
  })

  output$report <- renderUI({
    req(report())
    
    tagList(
      tags$h1("Set up for evaluation"),
      tags$div("We matched the following GS tiers to your tiers:"),
      renderTable(report()$tier.equiv),
      renderText(as.character(report()$tier.incons)),
      tags$div("The following tiers had speech in the GS during the test minute:"),
      renderTable(report()$tiers.w.spch),
      tags$h1("Accuracy reports"),
      tags$div("Your accuracy for the test minute is as follows (limited to GS-matching tiers):"),
      renderTable(report()$gs.tiers.print),
      tags$div("Your weighted scores for non-CHI diarization and xds are as follows:"),
      renderText(as.character(report()$nch.diar)),
      renderText(as.character(report()$xds.acc)),
      tags$h1("Summary"),
      renderText(as.character(report()$summ.weighted.score)),
      renderText(as.character(report()$summ.bad.tiers)),
      tags$h4(renderText(as.character(report()$pass.message))),
      tags$div("To pass you needed:"),
      renderText(as.character(report()$req.wscore)),
      renderText(as.character(report()$req.tiers.univ)),
      renderText(as.character(report()$req.tiers.lgsp)),
      tags$br()
    )
  })
  
  output$downloadErrors <- renderUI({
    # Output file name
    time.now <- gsub('-|:', '', as.character(Sys.time()))
    time.now <- gsub(' ', '_', time.now)
    
    errors <- report()$errors.tbl
  
    output$downloadErrorsHandler <- downloadHandler(
      filename = paste0("GS_comparison-",time.now,"-detected_errors-",
                        "-minute_", input$minute,
             "-by_", input$coder, "_from_", input$PI, ".csv"),
      content = function(file) {
        write.csv(errors, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    downloadButton("downloadErrorsHandler", "Download your error spreadsheet")
  })
}

# Create Shiny app ----
shinyApp(ui, server)