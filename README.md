# JokoScribe-AI
# AI-Powered Text Utility App (Built with R & Shiny)

This project is an AI-powered **web application built using R and Shiny** that provides three core NLP functionalities in one interface:

- **Text Expander** – Generates more detailed versions of brief input text
- **Summarizer** – Condenses lengthy text into short, meaningful summaries
- **OCR (Optical Character Recognition)** – Extracts text from uploaded images

The app integrates open-source AI models and R packages to support academic writing, professional note-taking, and document digitization.

---
Check out [**JokoScribe AI**](https://ye2qsj-temi.shinyapps.io/jokoscribe//)

## Features

### 1. **Text Expansion**
- Converts short prompts into detailed, coherent paragraphs
- Ideal for email drafting, content ideation, and report writing

### 2. **Text Summarization**
- Generates concise summaries from long blocks of text
- Supports quick comprehension of articles, notes, or reports

### 3. **OCR (Image-to-Text)**
- Upload image files (e.g., scanned docs, screenshots)
- Automatically extracts readable text from images

---

## Built With

| Tool / Library     | Role                                |
|--------------------|-------------------------------------|
| **R**              | Core programming language           |
| **Shiny**          | Web app framework                   |
| **text**           | NLP model interface for expansion and summarization |
| **tesseract**      | OCR engine for extracting text from images |
| **shinydashboard** | UI components and layout management |

---

## App Workflow

1. User chooses a function: `Expand`, `Summarize`, or `OCR`
2. Inputs text (or uploads an image for OCR)
3. App processes input using R-based NLP tools
4. Result is rendered interactively and copy-ready

---

## Deployment

- Hosted via `shinyapps.io` or local deployment using `shiny::runApp()`
- User-friendly UI with a modern, minimal design

---

## How to Run Locally

```r
# Install required packages
install.packages(c("shiny", "shinydashboard", "text", "tesseract"))

# Launch the app
shiny::runApp("path_to_app_folder")
