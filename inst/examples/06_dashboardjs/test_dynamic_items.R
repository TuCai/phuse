library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Dropdowns 2.0",
                  dropdownMenuOutput("menu")
  ),
  dashboardSidebar(
    helpText("Add another item to the dropdown menu by clicking ",
             "on the button below"),
    actionButton("addItem", "Add another item")
  ),
  dashboardBody()
)

server <- function(input, output, session) {
  tasks <-  reactiveValues(
    code = list(id = "code", value = 15, color = "aqua",
                text = "Refactor code"),
    layout = list(id = "layout", value = 40, color = "green",
                  text = "Design new layout"),
    docs = list(id = "docs", value = 25, color = "red",
                text = "Write documentation")
  )

  # actually render the dropdownMenu
  output$menu <- renderMenu({
    items <- lapply(tasks, function(el) {
      taskItem(value = el$value, color = el$color, text = el$text)
    })
    dropdownMenu(
      type = "tasks", badgeStatus = "danger",
      .list = items
    )
  })

  observeEvent(input$addItem, {
    showModal(modalDialog(title = "Add new task",
                          textInput(paste0("id", input$addItem), "Task ID"),
                          numericInput(paste0("val", input$addItem), "Task value", 0),
                          selectInput(paste0("col", input$addItem), "Task color",
                                      choices = c("red", "yellow", "aqua", "blue",
                                                  "light-blue", "green", "navy", "teal",
                                                  "olive", "lime", "orange", "fuchsia",
                                                  "purple", "maroon", "black")
                          ),
                          textInput(paste0("text", input$addItem), "Task text"),
                          actionButton(paste0("go", input$addItem), "Add item"),
                          easyClose = TRUE, footer = NULL
    ))

    observeEvent(input[[paste0("go", input$addItem)]], {
      tasks[[paste0("id", input$addItem)]] <- list(
        id = input[[paste0("id", input$addItem)]],
        value = input[[paste0("val", input$addItem)]],
        color = input[[paste0("col", input$addItem)]],
        text = input[[paste0("text", input$addItem)]]
      )
      removeModal()
    })
  })
}

shinyApp(ui, server)
