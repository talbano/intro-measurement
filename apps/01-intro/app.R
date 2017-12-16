library("shiny")
library("ggplot2")
library("epmr")
load("data/pisa-subset.rda")

# Run once
#vars <- c("memor", "elab", "cstrat", "atschl", "undrem", "metasum")
#names(vars) <- c("memor - memorization strategies", "elab - elaboration strategies",
#  "cstrat - control strategies", "atschl - attitude toward school",
#  "undrem - metacognition understanding and remembering",
#  "metasum - metacognition summarizing")
#set.seed(161227)
#pisa <- PISA09[sample(nrow(PISA09), 10000), c("cnt", vars)]
#pisa <- pisa[complete.cases(pisa), ]
#save(pisa, vars, file = "apps/01-intro/data/pisa-subset.rda")

ui <- fluidPage(
  titlePanel("Exploring PISA09 Student Survey Scales by Country"),
  sidebarPanel(
    selectInput("variable", "Select variable", vars),
    tableOutput("table"), width = 5
  ),
  mainPanel(
    plotOutput("plot"), width = 7
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    selectedData <- reactive({
      pisa[, c("cnt", input$variable)]
    })
    ggplot(selectedData(), aes_(as.name("cnt"), as.name(input$variable))) +
      geom_boxplot() + ylim(-3, 3)
  })
  output$table <- renderTable({
    selectedData <- reactive({
      pisa[, c("cnt", input$variable)]
    })
    tab <- tapply(selectedData()[, 2], selectedData()[, 1], dstudy)
    rnames <- names(tab)
    cnames <- names(tab[[1]])
    mtab <- round(matrix(unlist(tab, use.names = FALSE), ncol = length(cnames),
      byrow = TRUE), 2)
    rownames(mtab) <- rnames
    colnames(mtab) <- cnames
    mtab <- rbind(mtab, Total = dstudy(selectedData()[, 2]))
    mtab[, c(1, 3:5)]
  }, rownames = TRUE, align = "lrrrr")
}

shinyApp(ui = ui, server = server)