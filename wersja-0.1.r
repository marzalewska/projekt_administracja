library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)


csv_file_path <- "dane.txt"
deposit_file_path <- "lokaty.txt"
recipients_file_path <- "Odbiorcy.txt"

bank_locations <- data.frame(
  type = c("Oddział 1 w Krakowie", "Oddział 4 w Krakowie", "Oddział 13 w Krakowie", "Oddział 11 w Krakowie", "Oddział 9 w Krakowie"),
  adres = c("31-548 Kraków, Al. Pokoju 5", "30-518 Kraków, ul. Rynek Podgórski 12", "31-072 Kraków, ul. Wielopole 17", "30-314 Kraków, ul. Dworska 1c", "30-707 Kraków, ul. Powstańców Wielkopolskich 18")
)

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
    'Nazwa lokaty' = col_character(),
    'Data założenia' = col_date(format = "%Y-%m-%d"),
    Kwota = col_double()
  ))
}

# Funkcja do wczytywania danych odbiorców
load_recipients_data <- function(file_path) {
  read_delim(file_path, delim = ";", col_types = cols(
    Nazwa = col_character(),
    Numer = col_character()
  ))
}

# Function to calculate cumulative balance
calculate_cumulative_balance <- function(account_data) {
  account_data <- account_data %>%
    arrange(Data) %>%
    group_by(Data) %>%
    summarise(Saldo = sum(Kwota)) %>%
    mutate(Saldo_cumulative = cumsum(Saldo)) %>%
    ungroup()

  account_data
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
                                                         actionButton("new_deposit", "Utwórz nową lokatę"),
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
                  ), # tabPanel Lokaty
                                  tabPanel("Placówki i Bankomaty",
                           fluidRow(
                             dataTableOutput("bank_locations_table")
                           )
                  )
                ) # navbarPage
)

# Definicja funkcji serwera
server <- function(input, output, session) {
  account_data <- load_account_data(csv_file_path)
  deposit_data <- load_deposit_data(deposit_file_path)
  recipients_data <- load_recipients_data(recipients_file_path)

  latest_balance <- reactiveVal(load_account_balance(account_data))

    cumulative_balance <- reactive({
    account_data %>%
      calculate_cumulative_balance()
  })

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

  output$bank_locations_table <- renderDataTable({
    datatable(bank_locations,
              options = list(pageLength = 10, autoWidth = TRUE),
              rownames = FALSE)
  })

  output$balance_plot <- renderPlot({
    ggplot(cumulative_balance(), aes(x = Data, y = Saldo_cumulative)) +
      geom_line() +
      geom_point() +
      labs(x = "Data", y = "Saldo") +
      theme_minimal()
  })

  output$pie_chart <- renderPlot({
  data <- data.frame(
    Kategoria = c("Wydatki", "Zarobki"),
    amount = c(expenses_income()$expenses, expenses_income()$income)
  )

  ggplot(data, aes(x = "", y = amount, fill = Kategoria)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y") +
    scale_fill_manual(values = c("#0033CC", "#3366CC")) +  
    theme_void() +
    theme(legend.position = "right")
})


  my_colors<-c("#0066CC", "#003366", "#6699CC", "#006699", "#0099CC", "#336699", "#3399FF", "#3366CC", "#0033CC")


  output$category_plot <- renderPlot({
  ggplot(category_expenses(), aes(x = reorder(Kategoria, -total_expense), y = total_expense, fill = Kategoria)) +
    geom_bar(stat = "identity", color = "black", fill = my_colors) +  # Specify colors here
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
      selectizeInput("recipient_name", "Nazwa odbiorcy:", choices = c("", recipients_data$Nazwa), selected = NULL, options = list(create = TRUE)),
      textInput("recipient_account", "Numer konta odbiorcy:"),
      numericInput("transferAmount", "Kwota przelewu:", min = 0, value = 0),
      footer = tagList(
        actionButton("saveRecipient", "Zapisz nowego odbiorcę i wykonaj przelew"),
        actionButton("submitTransfer", "Wykonaj przelew"),
        modalButton("Zamknij")
      )
    ))
  })

  observe({
  recipient_name <- input$recipient_name
  if (!is.null(recipient_name) && recipient_name != "") {
    recipient <- recipients_data %>% filter(Nazwa == recipient_name)
    if (nrow(recipient) > 0) {
      updateTextInput(session, "recipient_account", value = recipient$Numer)
    }
  }
})


  observeEvent(input$saveRecipient, {
    new_recipient_name <- input$recipient_name
    new_recipient_account <- input$recipient_account
    if(!new_recipient_name %in% recipients_data$Nazwa) {
      new_recipient <- data.frame(
        Nazwa = new_recipient_name,
        Numer = new_recipient_account
      )
      recipients_data <<- rbind(recipients_data, new_recipient)
      write_delim(recipients_data, recipients_file_path, delim = ";")
      updateSelectizeInput(session, "recipient_name", choices = c("", recipients_data$Nazwa), selected = new_recipient_name)
           new_balance <- latest_balance() - input$transferAmount
    account_data <<- rbind(account_data, data.frame(Data = Sys.Date(), Kwota = -input$transferAmount, Kategoria = "Przelew"))
    write_delim(account_data, csv_file_path, delim = ";")
    latest_balance(new_balance)
    output$account_balance <- renderText({
      paste("Na twoim koncie jest kwota:", latest_balance())
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
    removeModal()

      showModal(modalDialog(
        title = "Sukces",
        "Wykonano przelew i zapisano nowego odbiorcę!",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      showModal(modalDialog(
        title = "Błąd",
        "Odbiorca o takiej nazwie już istnieje.",
        easyClose = TRUE,
        footer = NULL
      ))
    }

  })

  observeEvent(input$submitTransfer, {
    new_balance <- latest_balance() - input$transferAmount
    account_data <<- rbind(account_data, data.frame(Data = Sys.Date(), Kwota = -input$transferAmount, Kategoria = "Przelew"))
    write_delim(account_data, csv_file_path, delim = ";")
    latest_balance(new_balance)
    output$account_balance <- renderText({
      paste("Na twoim koncie jest kwota:", latest_balance())
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
    removeModal()
    showModal(modalDialog(
       title = "Sukces",
       "Wykonano przelew!",
       easyClose = TRUE,
       footer = NULL
     ))
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
        selectInput("selected_deposit", "Wybierz lokatę:", choices = deposit_data$'Nazwa lokaty'),
        footer = tagList(
          modalButton("Anuluj"),
          actionButton("submitWithdraw", "Wypłać")
        )
      ))
    }
  })

  observeEvent(input$submitWithdraw, {

    selected_deposit <- input$selected_deposit
    withdraw_amount <- deposit_data[deposit_data$'Nazwa lokaty' == selected_deposit, "Kwota"]
    new_balance <- latest_balance() + withdraw_amount

    transaction <- data.frame(
      Data = Sys.Date(),
      Kwota = withdraw_amount,
      Kategoria = "Inne"
    )

    write_delim(transaction, csv_file_path, delim = ";", append = TRUE)

    deposit_data <<- deposit_data[deposit_data$'Nazwa lokaty' != selected_deposit, ]

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

  observeEvent(input$new_deposit, {
  showModal(modalDialog(
    title = "Utwórz nową lokatę",
    textInput("deposit_name", "Nazwa lokaty:"),
    numericInput("deposit_amount", "Kwota lokaty:", min = 0, value = 0),
    footer = tagList(
      actionButton("create_deposit", "Utwórz lokatę"),
      modalButton("Anuluj")
    )
  ))
})

observeEvent(input$create_deposit, {
  deposit_name <- input$deposit_name
  deposit_amount <- input$deposit_amount

  # Update deposit_data dataframe
  new_deposit <- data.frame(
    'Nazwa lokaty' = as.character(deposit_name),
    'Data założenia' = as.Date(Sys.Date()),
    Kwota = as.numeric(deposit_amount),
    check.names = FALSE
  )
  deposit_data <<- rbind(deposit_data, new_deposit)
  write_delim(deposit_data, deposit_file_path, delim = ";")

  # Deduct deposit amount from account_data and update dane.txt
  account_data <<- rbind(account_data, data.frame(
    Data = Sys.Date(),
    Kwota = -deposit_amount,
    Kategoria = "Lokata"
  ))
  write_delim(account_data, csv_file_path, delim = ";")

  # Update latest_balance reactiveVal
  new_balance <- latest_balance() - deposit_amount
  latest_balance(new_balance)

  # Close modal and update deposits_table output
  removeModal()
})


}

# Uruchom aplikację
shinyApp(ui = ui, server = server)
