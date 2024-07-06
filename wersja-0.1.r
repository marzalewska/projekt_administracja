# Dodatkowe biblioteki
library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)

csv_file_path <- "dane.txt"

# Funkcja do wczytywania danych
load_account_data <- function(file_path) {
  read_delim(file_path, delim = ";", col_types = cols(
    Data = col_date(format = "%Y-%m-%d"),
    Kwota = col_double(),
    Kategoria = col_character()
  ))
}

# Funkcja do wczytywania stanu konta jako sumy wszystkich kwot
load_account_balance <- function(account_data) {
  sum(account_data$Kwota)
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
  expenses <- sum(account_data$Kwota[account_data$Kwota < 0])
  income <- sum(account_data$Kwota[account_data$Kwota > 0])
  # Wyklucz pierwszy wiersz przy obliczaniu zarobków
  income <- sum(account_data$Kwota[-1][account_data$Kwota[-1] > 0])
  list(expenses = abs(expenses), income = income)
}

# Funkcja do obliczania wydatków według kategorii
calculate_category_expenses <- function(account_data) {
  category_expenses <- account_data %>%
    group_by(Kategoria) %>%
    summarise(total_expense = sum(Kwota[Kwota < 0]))

  category_expenses
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
                             plotOutput("pie_chart"),
                             br(), br(),
                             h1("Wydatki według kategorii"),
                             plotOutput("category_plot")
                           ) # mainPanel
                  ),
                  tabPanel("Historia przelewów",
                           DT::dataTableOutput("transactions_table")
                  ),
                  tabPanel("Możliwe pomoce", "Możliwe kredyty na podstawie zdolności kredytowej")
                ) # navbarPage
)

# Definicja funkcji serwera
server <- function(input, output, session) {
  account_data <- load_account_data(csv_file_path)

  latest_balance <- reactiveVal(load_account_balance(account_data))

  account_data_with_changes <- reactive({
    calculate_changes(account_data)
  })

  expenses_income <- reactive({
    calculate_expenses_income(account_data)
  })

  category_expenses <- reactive({
    calculate_category_expenses(account_data)
  })

  output$account_balance <- renderText({
    paste("Na twoim koncie jest kwota:", latest_balance())
  })

  output$balance_plot <- renderPlot({
    account_data <- account_data %>%
      arrange(Data) %>%
      mutate(Saldo = cumsum(Kwota))

    ggplot(account_data, aes(x = Data, y = Saldo)) +
      geom_line() +
      geom_point() +
      labs(x = "Data", y = "Saldo") +
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

  output$category_plot <- renderPlot({
    ggplot(category_expenses(), aes(x = reorder(Kategoria, -total_expense), y = total_expense, fill = Kategoria)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(labels = comma) +  # Formatowanie wartości osi Y
      labs(x = "Kategoria", y = "Suma wydatków", fill = "Kategoria") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Obrót etykiet na osi X
  })

  output$transactions_table <- DT::renderDataTable({
    DT::datatable(account_data_with_changes(),
                  options = list(pageLength = 10, autoWidth = TRUE),
                  rownames = FALSE) %>%
      DT::formatStyle(
        'Change',
        color = DT::styleInterval(0, c('red', 'green'))
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
      Kwota = new_balance,
      Kategoria = "Przelew" # przykładowa kategoria dla przelewu
    )
    account_data <<- rbind(account_data, new_row)

    latest_balance(load_account_balance(account_data))

    # Zapis danych do pliku
    write_delim(account_data, csv_file_path, delim = ";")

    # Aktualizacja tabeli z danymi
    account_data_with_changes <- calculate_changes(account_data)
    expenses_income <- calculate_expenses_income(account_data_with_changes)
    category_expenses <- calculate_category_expenses(account_data_with_changes)

    output$transactions_table <- DT::renderDataTable({
      DT::datatable(account_data_with_changes,
                    options = list(pageLength = 10, autoWidth = TRUE),
                    rownames = FALSE) %>%
        DT::formatStyle(
          'Change',
          color = DT::styleInterval(0, c('red', 'green'))
        )
    })

    # Aktualizacja wykresów
    output$balance_plot <- renderPlot({
      account_data <- account_data %>%
        arrange(Data) %>%
        mutate(Saldo = cumsum(Kwota))

      ggplot(account_data, aes(x = Data, y = Saldo)) +
        geom_line() +
        geom_point() +
        labs(x = "Data", y = "Saldo") +
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

    output$category_plot <- renderPlot({
      ggplot(category_expenses(), aes(x = reorder(Kategoria, -total_expense), y = total_expense, fill = Kategoria)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = comma) +
        labs(x = "Kategoria", y = "Suma wydatków", fill = "Kategoria") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    removeModal()
  })

  observeEvent(input$withdraw, {
  })
}

# Utwórz obiekt Shiny
shinyApp(ui = ui, server = server)
