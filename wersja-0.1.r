library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)

csv_file_path <- "dane.txt"
deposit_file_path <- "lokaty.txt"

# Funkcja do wczytywania danych
load_account_data <- function(file_path) {
  read_delim(file_path, delim = ";", col_types = cols(
    Data = col_date(format = "%Y-%m-%d"),
    Kwota = col_double(),
    Kategoria = col_character()
  ))
}

# Funkcja do wczytywania danych o lokatach
load_deposit_data <- function(file_path) {
  read_delim(file_path, delim = ";", col_types = cols(
    `Nazwa lokaty` = col_character(),
    `Data założenia` = col_date(format = "%Y-%m-%d"),
    `Kwota` = col_double()
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
                  "Bank PinkyPromise",
                  tabPanel("Informacje",
                           sidebarPanel(
                             tags$h3(icon("dollar"),"Akcje"),
                             actionButton("transfer", "Zrób przelew"), br(), br(),
                             actionButton("withdraw", "Wyciągnij pieniądze z lokaty"),
                             tags$h3(icon("envelope"), "Wiadomości"),
                             tags$div(
                               br(),
                               actionButton("new_promotion", label = tags$span(icon("exclamation-circle", style = "color: red;"), "Nowa promocja")),
                               br(),br(),
                               actionButton("ask_question", label = tags$span(icon("question-circle", style = "color: red;"), "Zadaj pytanie"))
                             )
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
                  tabPanel("Lokaty",
                           fluidRow(
                             column(12, DT::dataTableOutput("deposits_table"))
                           )
                  ) # tabPanel Lokaty
                ) # navbarPage
)

# Definicja funkcji serwera
server <- function(input, output, session) {
  account_data <- load_account_data(csv_file_path)
  deposit_data <- load_deposit_data(deposit_file_path)
  
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
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Kategoria", y = "Suma wydatków", fill = "Kategoria") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
  
  output$deposits_table <- DT::renderDataTable({
    DT::datatable(deposit_data,
                  options = list(pageLength = 10, autoWidth = TRUE),
                  rownames = FALSE)
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
      Kwota = -input$transferAmount,
      Kategoria = "Przelew" 
    )
    account_data <<- rbind(account_data, new_row)
    
    latest_balance(load_account_balance(account_data))
    
    write_delim(account_data, csv_file_path, delim = ";")
    
    account_data_with_changes <<- calculate_changes(account_data)
    expenses_income <<- calculate_expenses_income(account_data_with_changes())
    category_expenses <<- calculate_category_expenses(account_data_with_changes())
    
    output$transactions_table <- DT::renderDataTable({
      DT::datatable(account_data_with_changes(),
                    options = list(pageLength = 10, autoWidth = TRUE),
                    rownames = FALSE) %>%
        DT::formatStyle(
          'Change',
          color = DT::styleInterval(0, c('red', 'green'))
        )
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
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Kategoria", y = "Suma wydatków", fill = "Kategoria") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    removeModal()
  })
  
  observeEvent(input$withdraw, {
    if (nrow(deposit_data) == 0) {
      showModal(modalDialog(
        title = "Brak lokat",
        "Nie posiadasz żadnych lokat. Nie można wykonać operacji wypłaty.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else if (nrow(deposit_data) == 1) {
      selected_deposit_row <- 1 
      withdraw_amount <- deposit_data$Kwota[selected_deposit_row]
      new_balance <- latest_balance() + withdraw_amount
      
      deposit_data <<- deposit_data[-selected_deposit_row, ]
      
      write_delim(deposit_data, deposit_file_path, delim = ";")
      
      latest_balance(new_balance)
      
      output$deposits_table <- DT::renderDataTable({
        DT::datatable(deposit_data,
                      options = list(pageLength = 10, autoWidth = TRUE),
                      rownames = FALSE)
      })
      
      removeModal()
    } else {
      showModal(modalDialog(
        title = "Wybierz lokatę do wypłaty",
        selectInput("selected_deposit", "Wybierz lokatę:", choices = deposit_data$`Nazwa lokaty`),
        footer = tagList(
          modalButton("Anuluj"),
          actionButton("submitWithdraw", "Wypłać")
        )
      ))
    }
  })
  
  observeEvent(input$submitWithdraw, {
    selected_deposit <- input$selected_deposit
    withdraw_amount <- deposit_data[deposit_data$`Nazwa lokaty` == selected_deposit, "Kwota"]
    new_balance <- latest_balance() + withdraw_amount
    
    transaction <- data.frame(
      Data = Sys.Date(),
      Kwota = withdraw_amount,
      Kategoria = "Inne"
    )
    
    write_delim(transaction, csv_file_path, delim = ";", append = TRUE)
    
    deposit_data <<- deposit_data[deposit_data$`Nazwa lokaty` != selected_deposit, ]
    
    write_delim(deposit_data, deposit_file_path, delim = ";")
    
    latest_balance(new_balance)
    
    output$deposits_table <- DT::renderDataTable({
      DT::datatable(deposit_data,
                    options = list(pageLength = 10, autoWidth = TRUE),
                    rownames = FALSE)
    })
    
    removeModal()
  })
  
  
  observeEvent(input$new_promotion, {
    showModal(modalDialog(
      title = "Nowa Promocja",
      HTML("Ciesz się latem z wyjątkowymi nagrodami! Skorzystaj z naszej najnowszej promocji i zyskaj jeszcze więcej korzyści!

Jak skorzystać z promocji?<br/>
           <ul>
             <li>Zaloguj się do naszej aplikacji mobilnej.</li>
             <li>Dokonaj co najmniej 3 płatności kartą w ciągu miesiąca.</li>
             <li>Wykonaj co najmniej 2 przelewy za pomocą aplikacji mobilnej.</li>
           </ul><br/>
           Nagrody:<br/>
           🎁 Bon na zakupy o wartości 50 PLN w wybranych sklepach partnerskich.<br/><br/>
           🎁 Cashback 5% na zakupy online do 100 PLN miesięcznie.<br/><br/>
           🎁 0% prowizji na przelewy zagraniczne przez 3 miesiące.<br/><br/>
           Dodatkowo:<br/>
Każda płatność kartą debetową lub kredytową bierze udział w losowaniu smartfona!
<br/>
Promocja trwa od 15 lipca do 31 sierpnia.
<br/>
Nie czekaj! Zaloguj się do aplikacji, korzystaj z bankowości mobilnej i wygrywaj wspaniałe nagrody tego lata!

Pobierz aplikację już teraz i bądź na bieżąco z naszymi najnowszymi ofertami!"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$ask_question, {
    showModal(modalDialog(
      title = "Zadaj pytanie",
      textInput("question", "Twoje pytanie:", ""),
      footer = tagList(
        modalButton("Zamknij"),
        actionButton("submitQuestion", "Wyślij")
      )
    ))
  })
  
  observeEvent(input$submitQuestion, {
    showModal(modalDialog(
      title = "Pytanie wysłane",
      "Dziękujemy za zadanie pytania. Odpowiemy na nie najszybciej jak to możliwe.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

shinyApp(ui = ui, server = server)
