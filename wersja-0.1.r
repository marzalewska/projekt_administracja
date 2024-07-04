library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)

csv_file_path <- "dane.txt"

# Funkcja do wczytywania danych
load_account_data <- function(file_path) {
  read_delim(file_path, delim = ";", col_types = cols(
    Data = col_date(format = "%Y-%m-%d"),
    Kwota = col_double()
  ))
}

# Funkcja do wczytywania najnowszych danych
load_latest_data <- function(account_data) {
  latest_row <- account_data %>%
    arrange(desc(Data)) %>%
    slice(1)
  
  latest_row$Kwota
}

# Funkcja do obliczania zmian w stanie konta
calculate_changes <- function(account_data) {
  account_data <- account_data %>%
    arrange(Data) %>%
    mutate(Change = Kwota - lag(Kwota, default = first(Kwota)))
  
  account_data <- account_data[-1, ]
  account_data
}

# Funkcja do obliczania wydatków i zarobków
calculate_expenses_income <- function(account_data) {
  changes <- account_data$Change
  expenses <- sum(changes[changes < 0])
  income <- sum(changes[changes > 0])
  list(expenses = abs(expenses), income = income)
}

# Definicja UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  "My first app",
                  tabPanel("Informacje",
                           sidebarPanel(
                             tags$h3("Akcje:"),
                             actionButton("transfer", "Zrób przelew"),
                             actionButton("withdraw", "Wyciągnij pieniądze z lokaty")
                           ), # sidebarPanel
                           mainPanel(
                             h1("Saldo"),
                             verbatimTextOutput("account_balance"),
                             br(), br(), br(),
                             h1("Stan konta w ostatnich dniach"),
                             plotOutput("balance_plot"), 
                             br(), br(),
                             h1("Zależność między wydatkami a zarobkami"),
                             plotOutput("pie_chart")
                           ) # mainPanel
                  ),
                  tabPanel("Historia przelewów",
                           dataTableOutput("transactions_table")
                  ),
                  tabPanel("Możliwe pomoce", "Możliwe kredyty na podstawie zdolności kredytowej")
                ) # navbarPage
                
                # Modal dialog for making a transfer
                , tags$div(id = "transferModal", class = "modal", style = "display:none;",
                           tags$div(class = "modal-dialog",
                                    tags$div(class = "modal-content",
                                             tags$div(class = "modal-header",
                                                      tags$button(type = "button", class = "close", `data-dismiss` = "modal", "×"),
                                                      tags$h4(class = "modal-title", "Zrób przelew")
                                             ),
                                             tags$div(class = "modal-body",
                                                      numericInput("transferAmount", "Kwota przelewu:", min = 0, value = 0)
                                             ),
                                             tags$div(class = "modal-footer",
                                                      tags$button(type = "button", class = "btn btn-default", `data-dismiss` = "modal", "Zamknij"),
                                                      actionButton("submitTransfer", "Wyślij", class = "btn btn-primary")
                                             )
                                    )
                           )
                )
) # fluidPage

# Definicja funkcji serwera
server <- function(input, output, session) {
  account_data <- load_account_data(csv_file_path)
  
  latest_balance <- reactiveVal(load_latest_data(account_data))
  
  account_data_with_changes <- reactive({
    calculate_changes(account_data)
  })
  
  expenses_income <- reactive({
    calculate_expenses_income(account_data_with_changes())
  })
  
  output$account_balance <- renderText({
    paste("Na twoim koncie jest kwota:", latest_balance())
  })
  
  output$balance_plot <- renderPlot({
    ggplot(account_data, aes(x = Data, y = Kwota)) +
      geom_line() +
      geom_point() +
      labs(x = "Data", y = "Kwota") +
      theme_minimal()
  })
  
  output$pie_chart <- renderPlot({
    data <- data.frame(
      category = c("Wydatki", "Zarobki"),
      amount = c(expenses_income()$expenses, expenses_income()$income)
    )
    
    ggplot(data, aes(x = "", y = amount, fill = category)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") +
      theme_void() +
      theme(legend.position = "right")
  })
  
  output$transactions_table <- renderDataTable({
    datatable(account_data_with_changes(), 
              options = list(pageLength = 10, autoWidth = TRUE),
              rownames = FALSE) %>%
      formatStyle(
        'Change',
        color = styleInterval(0, c('red', 'green'))
      )
  })
  
  observeEvent(input$transfer, {
    showModal(modalDialog(
      title = "Zrób przelew",
      numericInput("transferAmount", "Kwota przelewu:", min = 0, value = 0),
      footer = tagList(
        modalButton("Zamknij"),
        actionButton("submitTransfer", "Wyślij")
      )
    ))
  })
  
  observeEvent(input$submitTransfer, {
    new_balance <- latest_balance() - input$transferAmount
    new_row <- data.frame(
      Data = Sys.Date(),
      Kwota = new_balance
    )
    account_data <<- rbind(account_data, new_row)
    
    latest_balance(new_balance)
    
    # Zapis danych do pliku
    write_delim(account_data, csv_file_path, delim = ";")
    
    # Aktualizacja tabeli z danymi
    account_data_with_changes <- calculate_changes(account_data)
    expenses_income <- calculate_expenses_income(account_data_with_changes)
    
    output$transactions_table <- renderDataTable({
      datatable(account_data_with_changes, 
                options = list(pageLength = 10, autoWidth = TRUE),
                rownames = FALSE) %>%
        formatStyle(
          'Change',
          color = styleInterval(0, c('red', 'green'))
        )
    })
    
    # Aktualizacja wykresów
    output$balance_plot <- renderPlot({
      ggplot(account_data, aes(x = Data, y = Kwota)) +
        geom_line() +
        geom_point() +
        labs(x = "Data", y = "Kwota") +
        theme_minimal()
    })
    
    output$pie_chart <- renderPlot({
      data <- data.frame(
        category = c("Wydatki", "Zarobki"),
        amount = c(expenses_income$expenses, expenses_income$income)
      )
      
      ggplot(data, aes(x = "", y = amount, fill = category)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y") +
        theme_void() +
        theme(legend.position = "right")
    })
    
    removeModal()
  })
  
  observeEvent(input$withdraw, {
  })
}

# Utwórz obiekt Shiny
shinyApp(ui = ui, server = server)
