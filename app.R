library(shiny)
library(shinyjs)
library(bslib)
library(shinycssloaders)

source("summarizer.R")
source("expander.R")

save_user_email <- function(email) saveRDS(email, file = "user_email.rds")
credentials_file <- "users.rds"

load_users <- function() {
  if (file.exists(credentials_file)) readRDS(credentials_file) else list()
}

save_users <- function(users) saveRDS(users, file = credentials_file)

get_chat_file <- function(email) paste0("chat_", gsub("[^a-zA-Z0-9]", "_", email), ".rds")

load_chat_history <- function(email) {
  file <- get_chat_file(email)
  if (file.exists(file)) readRDS(file) else list(data = list(), names = list())
}

save_chat_history <- function(email, history) {
  file <- get_chat_file(email)
  saveRDS(list(data = history$data, names = history$names), file)
}

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("      
      body {
        margin: 0; padding: 0;
        background: linear-gradient(to bottom right, #0f0f0f, #3a0057);
        color: white; font-family: 'Segoe UI', sans-serif;
      }
      .container { display: flex; margin: 0; padding: 0; }
      .sidebar {
        width: 250px; height: 100vh; overflow-y: auto;
        padding: 10px; border-right: 1px solid #333;
      }
      .content {
        flex: 1; padding: 20px; display: flex;
        flex-direction: column; height: 100vh;
      }
      .input-area { margin-top: auto; }
      .output-box {
        flex: 1; white-space: pre-wrap; word-wrap: break-word;
        border: 1px solid; padding: 10px; margin-bottom: 20px;
        overflow-y: auto; background-color: rgba(255, 255, 255, 0.05);
      }
      .chat-title {
        width: 75%; border: none; background: transparent; color: inherit;
      }
      input[type='text'], input[type='password'], textarea {
        background-color: transparent; color: white;
        border: 1px solid #555; border-radius: 5px; padding: 8px;
      }
      .btn-primary, .btn-secondary, .btn-danger, .btn-info, .btn-warning {
        background: linear-gradient(90deg, #7f00ff, #e100ff);
        border: none; color: white;
        box-shadow: 0 0 10px #7f00ff;
      }
      .glow-logo {
        width: 110px; height: 110px;
        background: linear-gradient(135deg, #a64bf4, #ff1ead);
        border-radius: 50%;
        display: flex; align-items: center; justify-content: center;
        font-size: 2.2em; font-weight: bold;
        color: white; text-shadow: 0 0 10px #e100ff, 0 0 20px #7f00ff;
        animation: pulse 2s infinite;
      }
      @keyframes pulse {
        0% { transform: scale(1); box-shadow: 0 0 0 0 rgba(255, 0, 255, 0.7); }
        70% { transform: scale(1.05); box-shadow: 0 0 0 20px rgba(255, 0, 255, 0); }
        100% { transform: scale(1); box-shadow: 0 0 0 0 rgba(255, 0, 255, 0); }
      }
    "))
  ),
  uiOutput("mainUI")
)

server <- function(input, output, session) {
  current_theme <- reactiveVal("dark")
  user_authenticated <- reactiveVal(FALSE)
  user_email <- reactiveVal("")
  chat_history <- reactiveValues(data = list(), names = list())
  selected_tool <- reactiveVal("Summarizer")
  current_output <- reactiveVal("")
  current_chat <- reactiveVal(NULL)
  
  output$mainUI <- renderUI({
    if (!user_authenticated()) {
      tagList(
        div(style = "height: 100vh; display: flex; align-items: center; justify-content: center; text-align: center; flex-direction: column;",
            span("JScribe", class = "glow-logo"),
            h3("Effortless control with JScribe AI"),
            br(),
            textInput("email", "Email"),
            passwordInput("password", "Password"),
            actionButton("login", "Login"),
            actionButton("signup", "Sign Up", style = "margin-left: 10px;"),
            br(), br(),
            actionButton("guest", "Continue as Guest", class = "btn-info")
        )
      )
    } else {
      tagList(
        div(class = "container",
            div(class = "sidebar", 
                actionButton("new_chat", "🆕 New Chat", class = "btn-sm"),
                uiOutput("chat_list"),
                br(),
                actionButton("delete_all", "🗑️ Delete All", class = "btn-danger btn-sm"),
                br(), br(),
                actionButton("logout", "🚪 Logout", class = "btn-warning btn-sm")
            ),
            div(class = "content",
                actionButton("theme_switch", "🌓 Switch Theme", class = "btn btn-secondary", style = "margin-bottom: 10px;"),
                selectInput("tool_select", "Choose Tool:", choices = c("Summarizer", "Expander", "OCR")),
                div(class = "output-box", withSpinner(textOutput("tool_output"), type = 4)),
                div(class = "input-area",
                    conditionalPanel(
                      condition = "input.tool_select == 'Summarizer'",
                      textAreaInput("sum_input", NULL, placeholder = "Type something to summarize...", rows = 4, width = "100%"),
                      textInput("sum_instruction", NULL, placeholder = "Instruction (optional)", width = "100%"),
                      actionButton("run_sum", "Run"),
                      downloadButton("download_sum", "Download")
                    ),
                    conditionalPanel(
                      condition = "input.tool_select == 'Expander'",
                      textAreaInput("expand_input", NULL, placeholder = "Type something to expand...", rows = 4, width = "100%"),
                      textInput("expand_instruction", NULL, placeholder = "Instruction (optional)", width = "100%"),
                      actionButton("run_expand", "Run"),
                      downloadButton("download_expand", "Download")
                    ),
                    conditionalPanel(
                      condition = "input.tool_select == 'OCR'",
                      fileInput("ocr_file", NULL, accept = c('image/png', 'image/jpeg')),
                      actionButton("run_ocr", "Run"),
                      downloadButton("download_ocr", "Download")
                    )
                )
            )
        )
      )
    }
  })
  
  observeEvent(input$theme_switch, {
    if (current_theme() == "dark") {
      runjs("document.body.style.background = 'white'; document.body.style.color = 'black';")
      current_theme("light")
    } else {
      runjs("document.body.style.background = 'linear-gradient(to bottom right, #0f0f0f, #3a0057)'; document.body.style.color = 'white';")
      current_theme("dark")
    }
  })
  
  observeEvent(input$login, {
    req(input$email, input$password)
    users <- load_users()
    if (input$email %in% names(users) && input$password == users[[input$email]]) {
      user_email(input$email)
      save_user_email(input$email)
      user_authenticated(TRUE)
      hist <- load_chat_history(input$email)
      chat_history$data <- hist$data
      chat_history$names <- hist$names
    } else {
      showModal(modalDialog("Incorrect email or password."))
    }
  })
  
  observeEvent(input$signup, {
    req(input$email, input$password)
    users <- load_users()
    if (input$email %in% names(users)) {
      showModal(modalDialog("User already exists."))
    } else if (nchar(input$password) < 4) {
      showModal(modalDialog("Password too short."))
    } else {
      users[[input$email]] <- input$password
      save_users(users)
      user_email(input$email)
      save_user_email(input$email)
      user_authenticated(TRUE)
      chat_history$data <- list()
      chat_history$names <- list()
    }
  })
  
  observeEvent(input$guest, {
    user_email("guest")
    save_user_email("guest")
    user_authenticated(TRUE)
    hist <- load_chat_history("guest")
    chat_history$data <- hist$data
    chat_history$names <- hist$names
  })
  
  observeEvent(input$logout, {
    save_chat_history(user_email(), chat_history)
    user_authenticated(FALSE)
    user_email("")
    chat_history$data <- list()
    chat_history$names <- list()
  })
  
  output$tool_output <- renderText({ current_output() })
  
  output$download_sum <- downloadHandler(
    filename = function() paste0("summary_", Sys.Date(), ".txt"),
    content = function(file) writeLines(current_output(), file)
  )
  output$download_expand <- downloadHandler(
    filename = function() paste0("expansion_", Sys.Date(), ".txt"),
    content = function(file) writeLines(current_output(), file)
  )
  output$download_ocr <- downloadHandler(
    filename = function() paste0("ocr_", Sys.Date(), ".txt"),
    content = function(file) writeLines(current_output(), file)
  )
  
  observeEvent(input$run_sum, {
    req(input$sum_input)
    result <- summarize_text(input$sum_input, input$sum_instruction)
    current_output(result)
    chat_id <- paste0("chat_", length(chat_history$data) + 1)
    chat_history$data[[chat_id]] <- result
    chat_history$names[[chat_id]] <- paste0("Chat ", length(chat_history$data))
    current_chat(chat_id)
    save_chat_history(user_email(), chat_history)
  })
  
  observeEvent(input$run_expand, {
    req(input$expand_input)
    result <- expand_text(input$expand_input, input$expand_instruction)
    current_output(result)
    chat_id <- paste0("chat_", length(chat_history$data) + 1)
    chat_history$data[[chat_id]] <- result
    chat_history$names[[chat_id]] <- paste0("Chat ", length(chat_history$data))
    current_chat(chat_id)
    save_chat_history(user_email(), chat_history)
  })
  
  observeEvent(input$run_ocr, {
    req(input$ocr_file)
    extracted <- tesseract::ocr(input$ocr_file$datapath)
    current_output(extracted)
    chat_id <- paste0("chat_", length(chat_history$data) + 1)
    chat_history$data[[chat_id]] <- extracted
    chat_history$names[[chat_id]] <- paste0("Chat ", length(chat_history$data))
    current_chat(chat_id)
    save_chat_history(user_email(), chat_history)
  })
  
  observeEvent(input$new_chat, {
    current_output("")
    updateTextAreaInput(session, "sum_input", value = "")
    updateTextInput(session, "sum_instruction", value = "")
    updateTextAreaInput(session, "expand_input", value = "")
    updateTextInput(session, "expand_instruction", value = "")
  })
  
  observeEvent(input$delete_all, {
    chat_history$data <- list()
    chat_history$names <- list()
    save_chat_history(user_email(), chat_history)
  })
  
  output$chat_list <- renderUI({
    if (length(chat_history$data) == 0) return(NULL)
    names_vec <- unlist(chat_history$names)
    lapply(seq_along(chat_history$data), function(i) {
      chat_id <- names(chat_history$data)[i]
      fluidRow(
        column(8, tags$input(
          id = paste0("chat_name_", i),
          type = "text",
          value = names_vec[i],
          style = "width: 100%; border: 1px solid #ccc; border-radius: 4px; padding: 5px;"
        )),
        column(4,
               actionButton(paste0("select_chat_", i), "📂", class = "btn btn-sm btn-success", style = "margin-top:5px;"),
               actionButton(paste0("del_chat_", i), "❌", class = "btn-danger btn-sm", style = "margin-top:5px;")
        )
      )
    })
  })
  
  observe({
    lapply(seq_along(chat_history$data), function(i) {
      chat_id <- names(chat_history$data)[i]
      input_id <- paste0("chat_name_", i)
      observeEvent(input[[input_id]], {
        if (!is.null(input[[input_id]]) && chat_history$names[[chat_id]] != input[[input_id]]) {
          chat_history$names[[chat_id]] <- input[[input_id]]
          save_chat_history(user_email(), chat_history)
        }
      }, ignoreInit = TRUE)
    })
  })
  
  observe({
    lapply(seq_along(chat_history$data), function(i) {
      observeEvent(input[[paste0("select_chat_", i)]], {
        chat_id <- names(chat_history$data)[i]
        current_output(chat_history$data[[chat_id]])
        current_chat(chat_id)
      })
    })
  })
  
  observe({
    lapply(seq_along(chat_history$data), function(i) {
      observeEvent(input[[paste0("del_chat_", i)]], {
        chat_id <- names(chat_history$data)[i]
        chat_history$data[[chat_id]] <- NULL
        chat_history$names[[chat_id]] <- NULL
        if (current_chat() == chat_id) current_output("")
        save_chat_history(user_email(), chat_history)
      })
    })
  })
}

shinyApp(ui, server)
