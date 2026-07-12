library(shiny)
library(shinydashboard)

theme_custom <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13, color = "#1a2e4a"),
      plot.subtitle    = element_text(size = 10, color = "#555555"),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = "#dddddd", fill = NA),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = 10),
      axis.title       = element_text(face = "bold", size = 11)
    )
}

PALETTE <- "Set2"

# ==================== UI ====================
ui <- dashboardPage(
  skin = "blue",
  
  # ---- Header ----
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://www.r-project.org/logo/Rlogo.svg",
               height = "28px", style = "margin-right:6px; vertical-align:middle;"),
      "NON-PARAM SOCIETY"
    ),
    titleWidth = 260
  ),
  
  # ---- Sidebar ----
  dashboardSidebar(
    width = 250,
    tags$style(HTML("
      .main-sidebar, .left-side { background-color:#1a2e4a !important; }
      .skin-blue .main-header .logo { background-color:#1a2e4a !important; }
      .skin-blue .main-header .navbar { background-color:#1e3a5f !important; }
      .skin-blue .sidebar-menu>li>a { color:#bdd5f0 !important; }
      .skin-blue .sidebar-menu>li.active>a { background-color:#2c4f7c !important; color:#fff !important; }
      .sidebar-section-title {
        color:#7fb8e8; font-weight:bold; font-size:11px;
        text-transform:uppercase; letter-spacing:1px;
        margin: 12px 15px 4px; padding-bottom:4px;
        border-bottom: 1px solid #2c4f7c;
      }
      .form-group { margin-bottom: 10px; }
    ")),
    
    div(style = "padding:12px 15px 0;",
        
        # --- File Upload ---
        div(class = "sidebar-section-title", "📁 1. Upload Data"),
        fileInput("file", NULL,
                  accept      = c(".csv", ".xlsx", ".xls"),
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"
        ),
        tags$small(style = "color:#7fb8e8; margin-top:-10px; display:block; margin-bottom:10px;", "Format: CSV atau Excel (.xlsx/.xls)"),
        
        hr(style = "border-color:#2c4f7c; margin:12px 0;"),
        
        # --- Test Configuration ---
        div(class = "sidebar-section-title", "🎯 2. Tujuan Analisis"),
        selectInput("tujuan_analisis", "Tujuan Analisis", 
                    choices = c("Uji Perbedaan", "Uji Hubungan/Korelasi")),
        
        div(class = "sidebar-section-title", "📊 3. Jumlah Sampel/Variabel"),
        selectInput("jumlah_sampel", "Jumlah Sampel", 
                    choices = c("1 Sampel", 
                                "2 Sampel Independen", 
                                "2 Sampel Berpasangan", 
                                "K Sampel Independen", 
                                "K Sampel Berpasangan")),
        
        div(class = "sidebar-section-title", "🔬 4. Metode Uji"),
        selectInput("metode_uji", "Metode", choices = NULL),
        
        hr(style = "border-color:#2c4f7c; margin:12px 0;"),
        
        # --- Dynamic Variable Input ---
        div(class = "sidebar-section-title", "📌 Pilih Variabel"),
        uiOutput("dynamic_variables_ui"),
        
        hr(style = "border-color:#2c4f7c; margin:12px 0;"),
        
        # --- Alpha ---
        div(class = "sidebar-section-title", "⚙️ Pengaturan"),
        selectInput("alpha", "Tingkat Signifikansi (α):",
                    choices  = c("0.01", "0.05", "0.10"),
                    selected = "0.05"
        ),
        
        # --- Run Button ---
        actionButton("run_test",
                     label = "▶  5. Eksekusi Analisis",
                     style = paste0(
                       "background-color:#00a65a; color:white; width:100%;",
                       "margin-top:8px; font-weight:bold; border:none;",
                       "padding:10px; border-radius:4px; font-size:14px;"
                     )
        ),
        
        br(), br()
    )
  ),
  
  # ---- Body ----
  dashboardBody(
    tags$head(tags$style(HTML("
      body, .content-wrapper { background-color:#f0f3f7; }
      .nav-tabs-custom>.nav-tabs>li.active>a {
        border-top: 3px solid #1a2e4a; color:#1a2e4a; font-weight:bold;
      }
      .nav-tabs-custom>.nav-tabs>li>a { font-size:14px; }
      .box.box-primary   { border-top-color:#1a2e4a; }
      .box.box-success   { border-top-color:#27ae60; }
      .box.box-warning   { border-top-color:#e67e22; }
      .box.box-info      { border-top-color:#2980b9; }
      .box-header { font-weight:bold; }

      /* Result Cards */
      .card-sig {
        background:#d4edda; border-left:5px solid #28a745;
        padding:16px; margin:12px 0; border-radius:6px;
        box-shadow:0 2px 6px rgba(0,0,0,0.08);
      }
      .card-not {
        background:#f8d7da; border-left:5px solid #dc3545;
        padding:16px; margin:12px 0; border-radius:6px;
        box-shadow:0 2px 6px rgba(0,0,0,0.08);
      }
      .card-info {
        background:#e8f4fd; border-left:5px solid #2980b9;
        padding:16px; margin:12px 0; border-radius:6px;
      }
      .verdict-sig { color:#155724; font-weight:bold; font-size:15px; }
      .verdict-not { color:#721c24; font-weight:bold; font-size:15px; }
      .stat-table  { width:100%; border-collapse:collapse; margin-top:8px; }
      .stat-table th {
        background:#1a2e4a; color:white; padding:8px 12px; text-align:left;
      }
      .stat-table td { padding:7px 12px; border-bottom:1px solid #e0e0e0; }
      .stat-table tr:nth-child(even) { background:#f8f9fa; }
    "))),
    
    tabBox(
      width = 12, id = "main_tabs",
      
      # ========== TAB 1: Data Preview ==========
      tabPanel(
        title = "📊 Data Editor & Preview",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Preview Data yang di Input",
              DTOutput("data_table")
          )
        ),
        fluidRow(
          box(width = 4, status = "info",
              title = "Statistik Deskriptif Variabel Utama",
              tableOutput("tbl_desc")),
          box(width = 4, status = "info",
              title = "Informasi Dataset",
              tableOutput("tbl_info")),
          box(width = 4, status = "warning",
              title = "Distribusi Frekuensi Kategori (Jika Ada)",
              tableOutput("tbl_freq"))
        )
      ),
      
      # ========== TAB 2: Test Results ==========
      tabPanel(
        title = "🔬 Hasil Uji Non-Parametrik",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Ringkasan Hasil Uji Terpilih",
              tableOutput("tbl_summary")
          )
        ),
        fluidRow(
          box(width = 12, status = "primary",
              title = "Detail Analisis Uji",
              uiOutput("ui_test_cards")
          )
        )
      ),
      
      # ========== TAB 3: Visualisasi ==========
      tabPanel(
        title = "📈 Visualisasi",
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Boxplot Perbandingan Kelompok",
              plotOutput("plot_box", height = "340px")),
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Distribusi Rank per Kelompok",
              plotOutput("plot_rank", height = "300px")),
        ),
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Scatter Plot Hubungan Variabel",
              plotOutput("plot_scatter", height = "340px")),
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Distribusi Densitas per Kelompok",
              plotOutput("plot_density", height = "340px"))
        ),
      ),
      
      # ========== TAB 4: Kesimpulan ==========
      tabPanel(
        title = "📋 Kesimpulan Uji",
        uiOutput("ui_conclusions")
      )
    )
  )
)

server <- function(input, output, session){
  
}

shinyApp(ui, server)
