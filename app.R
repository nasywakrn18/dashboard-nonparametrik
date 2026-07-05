library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

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
        
        # --- Dynamic Variable Input (SEKARANG DINAMIS MENGIKUTI SAMPEL) ---
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

server <- function(input, output, session){  # ---- 1. Load Data ----
  raw_data <- reactive({
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    tryCatch({
      if (ext == "csv") {
        read.csv(input$file$datapath, stringsAsFactors = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        as.data.frame(readxl::read_excel(input$file$datapath))
      } else {
        showNotification("Format tidak didukung. Gunakan CSV / Excel.", type = "error")
        NULL
      }
    }, error = function(e) {
      showNotification(paste("Gagal membaca file:", e$message), type = "error")
      NULL
    })
  })
  
  # ---- 2. Pilihan Metode Uji Berdasarkan Tujuan & Sampel ----
  observe({
    req(input$tujuan_analisis, input$jumlah_sampel)
    tujuan <- input$tujuan_analisis
    sampel <- input$jumlah_sampel
    
    uji_choices <- c()
    if (tujuan == "Uji Perbedaan") {
      if (sampel == "1 Sampel") {
        uji_choices <- c("Wilcoxon One Sample", "Runs Test (Wald-Wolfowitz)")
      } else if (sampel == "2 Sampel Independen") {
        uji_choices <- c("Mann-Whitney U Test")
      } else if (sampel == "2 Sampel Berpasangan") {
        uji_choices <- c("Wilcoxon Signed-Rank (Paired)*")
      } else if (sampel == "K Sampel Independen") {
        uji_choices <- c("Kruskal-Wallis Test")
      } else if (sampel == "K Sampel Berpasangan") {
        uji_choices <- c("Friedman Test*")
      }
    } else if (tujuan == "Uji Hubungan/Korelasi") {
      uji_choices <- c("Spearman Rank Correlation", "Kendall's Tau Correlation")
    }
    updateSelectInput(session, "metode_uji", choices = uji_choices)
  })
  
  # ---- 3. Render Input Pemilihan Variabel Secara Dinamis ----
  output$dynamic_variables_ui <- renderUI({
    req(raw_data())
    cols <- names(raw_data())
    num_cols <- cols[sapply(raw_data(), function(x) is.numeric(x) || suppressWarnings(!all(is.na(as.numeric(x)))))]
    
    tujuan <- input$tujuan_analisis
    sampel <- input$jumlah_sampel
    
    if (tujuan == "Uji Hubungan/Korelasi") {
      tagList(
        selectInput("var_x_num", "Variabel X (Numerik):", choices = num_cols, selected = num_cols[1]),
        selectInput("var_y_num", "Variabel Y (Numerik):", choices = num_cols, selected = if(length(num_cols) > 1) num_cols[2] else num_cols[1])
      )
    } else { # Uji Perbedaan
      if (sampel == "1 Sampel") {
        tagList(
          selectInput("var_y_one", "Variabel Uji (Y - Numerik):", choices = num_cols, selected = num_cols[1])
        )
      } else if (sampel %in% c("2 Sampel Independen", "K Sampel Independen")) {
        tagList(
          selectInput("var_group_ind", "Variabel Grup / Kategori (X):", choices = cols, selected = cols[1]),
          selectInput("var_value_ind", "Variabel Nilai / Respon (Y):", choices = num_cols, selected = num_cols[1])
        )
      } else if (sampel == "2 Sampel Berpasangan") {
        tagList(
          selectInput("var_y1_paired", "Variabel Sampel 1 / Sebelum (Y1):", choices = num_cols, selected = num_cols[1]),
          selectInput("var_y2_paired", "Variabel Sampel 2 / Sesudah (Y2):", choices = num_cols, selected = if(length(num_cols) > 1) num_cols[2] else num_cols[1])
        )
      } else if (sampel == "K Sampel Berpasangan") {
        tagList(
          selectizeInput("var_k_paired", "Pilih Komponen/Variabel (Min. 3):", choices = num_cols, multiple = TRUE)
        )
      }
    }
  })
  
  # ---- 4. Centralized Input Catcher ----
  data_inputs <- reactive({
    req(raw_data(), input$tujuan_analisis)
    tujuan <- input$tujuan_analisis
    sampel <- input$jumlah_sampel
    
    res <- list(type = NULL, x_name = NULL, y_name = NULL, y2_name = NULL, k_names = NULL)
    
    if (tujuan == "Uji Hubungan/Korelasi") {
      req(input$var_x_num, input$var_y_num)
      res$type   <- "correlation"
      res$x_name <- input$var_x_num
      res$y_name <- input$var_y_num
    } else {
      if (sampel == "1 Sampel") {
        req(input$var_y_one)
        res$type   <- "1-sample"
        res$y_name <- input$var_y_one
      } else if (sampel %in% c("2 Sampel Independen", "K Sampel Independen")) {
        req(input$var_group_ind, input$var_value_ind)
        res$type   <- "independent"
        res$x_name <- input$var_group_ind
        res$y_name <- input$var_value_ind
      } else if (sampel == "2 Sampel Berpasangan") {
        req(input$var_y1_paired, input$var_y2_paired)
        res$type    <- "paired-2"
        res$y_name  <- input$var_y1_paired
        res$y2_name <- input$var_y2_paired
      } else if (sampel == "K Sampel Berpasangan") {
        req(input$var_k_paired)
        res$type    <- "paired-k"
        res$k_names <- input$var_k_paired
      }
    }
    return(res)
  })
 
  # ---- 5. Data Table Preview ----
  output$data_table <- renderDT({
    req(raw_data())
    datatable(raw_data(),
              editable  = "cell", filter = "top", rownames = FALSE,
              class     = "table table-striped table-bordered table-hover",
              options   = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE)
    )
  })
  
  # ---- 6. Descriptive Stats ----
  output$tbl_desc <- renderTable({
    req(raw_data(), data_inputs())
    vars <- data_inputs()
    y_col <- if(!is.null(vars$y_name)) vars$y_name else vars$k_names[1]
    req(y_col)
    
    y <- suppressWarnings(as.numeric(raw_data()[[y_col]]))
    y <- y[!is.na(y)]
    if(length(y) == 0) return(NULL)
    
    data.frame(
      Statistik = c("Nama Kolom", "N Observasi", "Mean", "Median", "Std. Deviasi", "Minimum", "Maksimum", "IQR"),
      Nilai = c(y_col, length(y), round(mean(y), 4), round(median(y), 4), round(sd(y), 4), round(min(y), 4), round(max(y), 4), round(IQR(y), 4))
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$tbl_info <- renderTable({
    req(raw_data(), data_inputs())
    df <- raw_data()
    vars <- data_inputs()
    y_col <- if(!is.null(vars$y_name)) vars$y_name else vars$k_names[1]
    req(y_col)
    
    data.frame(
      Info  = c("Jumlah Baris", "Jumlah Kolom", "Total NA Data", "Kolom Numerik", "Tipe Var. Utama"),
      Nilai = c(nrow(df), ncol(df), sum(is.na(df)), sum(sapply(df, is.numeric)), class(df[[y_col]]))
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$tbl_freq <- renderTable({
    req(raw_data(), data_inputs())
    vars <- data_inputs()
    if (vars$type == "independent") {
      x <- raw_data()[[vars$x_name]]
      if (length(unique(x)) <= 30) {
        tbl <- as.data.frame(table(Kelompok = x))
        tbl$Persentase <- paste0(round(tbl$Freq / sum(tbl$Freq) * 100, 1), "%")
        names(tbl)[2] <- "Frekuensi"
        return(tbl)
      } else {
        return(data.frame(Keterangan = "Terlalu banyak nilai unik (>30)"))
      }
    }
    data.frame(Keterangan = "Hanya berlaku untuk sampel independen berkategori")
  }, striped = TRUE, hover = TRUE, bordered = TRUE) 
  
  # ---- 7. Run Selected Non-Parametric Test ----
  test_res <- eventReactive(input$run_test, {
    req(raw_data(), data_inputs(), input$metode_uji)
    
    vars   <- data_inputs()
    df     <- raw_data()
    alpha  <- as.numeric(input$alpha)
    metode <- input$metode_uji
    results <- list()
    
    if (vars$type == "1-sample") {
      y <- suppressWarnings(as.numeric(df[[vars$y_name]]))
      y <- y[!is.na(y)]
      
      if (metode == "Wilcoxon One Sample") {
        t <- tryCatch(wilcox.test(y, mu = 0, exact = FALSE), error = function(e) NULL)
        if (!is.null(t)) {
          results[["Wilcoxon One Sample"]] <- list(
            stat = as.numeric(t$statistic), stat_label = paste0("V = ", round(as.numeric(t$statistic), 3)),
            p_value = t$p.value, sig = t$p.value < alpha,
            h0 = "Median populasi = 0", h1 = "Median populasi ≠ 0",
            extra = paste0("n = ", length(y), " | Median sampel = ", round(median(y), 4))
          )
        }
      } else if (metode == "Runs Test (Wald-Wolfowitz)") {
        med <- median(y); above <- as.integer(y >= med)
        n1 <- sum(above == 1); n2 <- sum(above == 0); R <- length(rle(above)$lengths)
        mu_R <- (2 * n1 * n2) / (n1 + n2) + 1
        var_R <- (2*n1*n2*(2*n1*n2 - n1 - n2)) / ((n1+n2)^2 * (n1+n2-1))
        z_stat <- if(var_R > 0) (R - mu_R) / sqrt(var_R) else 0
        p_val <- 2 * pnorm(-abs(z_stat))
        results[["Runs Test (Wald-Wolfowitz)"]] <- list(
          stat = round(z_stat, 4), stat_label = paste0("Z = ", round(z_stat, 3), " | Runs (R) = ", R),
          p_value = p_val, sig = p_val < alpha,
          h0 = "Urutan data bersifat acak (random)", h1 = "Urutan data tidak bersifat acak",
          extra = paste0("n₁ = ", n1, ", n₂ = ", n2, " | E[R] = ", round(mu_R, 2))
        )
      }
    } else if (vars$type == "independent") {
      x_raw <- df[[vars$x_name]]; y_raw <- suppressWarnings(as.numeric(df[[vars$y_name]]))
      ok <- !is.na(y_raw) & !is.na(x_raw); x <- x_raw[ok]; y <- y_raw[ok]
      grp <- as.factor(x); grp_lvls <- levels(grp); grp_list <- split(y, grp)
      
      if (metode == "Mann-Whitney U Test" && length(grp_lvls) == 2) {
        t <- tryCatch(wilcox.test(grp_list[[1]], grp_list[[2]], exact = FALSE), error = function(e) NULL)
        if (!is.null(t)) {
          eff <- abs(qnorm(t$p.value / 2)) / sqrt(length(y))
          results[["Mann-Whitney U"]] <- list(
            stat = as.numeric(t$statistic), stat_label = paste0("W = ", round(as.numeric(t$statistic), 3)),
            p_value = t$p.value, sig = t$p.value < alpha,
            h0 = paste0("Distribusi '", grp_lvls[1], "' = '", grp_lvls[2], "'"),
            h1 = paste0("Distribusi '", grp_lvls[1], "' ≠ '", grp_lvls[2], "'"),
            extra = paste0("n₁=", length(grp_list[[1]]), ", n₂=", length(grp_list[[2]]), " | Effect size r ≈ ", round(eff, 3))
          )
        }
      } else if (metode == "Kruskal-Wallis Test") {
        t <- tryCatch(kruskal.test(y ~ grp), error = function(e) NULL)
        if (!is.null(t)) {
          results[["Kruskal-Wallis"]] <- list(
            stat = as.numeric(t$statistic), stat_label = paste0("H = ", round(as.numeric(t$statistic), 3), ", df = ", t$parameter),
            p_value = t$p.value, sig = t$p.value < alpha,
            h0 = "Semua kelompok berasal dari distribusi yang sama", h1 = "Setidaknya satu kelompok berbeda distribusinya",
            extra = paste0("k = ", length(grp_lvls), " kelompok | n = ", length(y))
          )
        }
      }
    } else if (vars$type == "paired-2") {
      y1 <- suppressWarnings(as.numeric(df[[vars$y_name]])); y2 <- suppressWarnings(as.numeric(df[[vars$y2_name]]))
      ok <- !is.na(y1) & !is.na(y2); y1 <- y1[ok]; y2 <- y2[ok]
      
      if (metode == "Wilcoxon Signed-Rank (Paired)*") {
        t <- tryCatch(wilcox.test(y1, y2, paired = TRUE, exact = FALSE), error = function(e) NULL)
        if (!is.null(t)) {
          results[["Wilcoxon Paired Signed-Rank"]] <- list(
            stat = as.numeric(t$statistic), stat_label = paste0("V = ", round(as.numeric(t$statistic), 3)),
            p_value = t$p.value, sig = t$p.value < alpha,
            h0 = "Tidak ada perbedaan median (Sampel 1 = Sampel 2)", h1 = "Terdapat perbedaan median secara berpasangan",
            extra = paste0("n = ", length(y1), " pasang observasi lengkap")
          )
        }
      }
    } else if (vars$type == "paired-k") {
      if (metode == "Friedman Test*" && length(vars$k_names) >= 3) {
        mat <- do.call(cbind, lapply(vars$k_names, function(nm) suppressWarnings(as.numeric(df[[nm]]))))
        ok <- complete.cases(mat); mat <- mat[ok, ]
        if (nrow(mat) > 1) {
          t <- tryCatch(friedman.test(mat), error = function(e) NULL)
          if (!is.null(t)) {
            results[["Friedman Test"]] <- list(
              stat = as.numeric(t$statistic), stat_label = paste0("Chi-sq = ", round(as.numeric(t$statistic), 3), ", df = ", t$parameter),
              p_value = t$p.value, sig = t$p.value < alpha,
              h0 = "Seluruh kelompok sampel berpasangan memiliki distribusi yang identik", h1 = "Setidaknya satu kelompok berpasangan berbeda",
              extra = paste0("k = ", ncol(mat), " kolom | n = ", nrow(mat), " baris blok lengkap")
            )
          }
        }
      }
    } else if (vars$type == "correlation") {
      x2 <- suppressWarnings(as.numeric(df[[vars$x_name]])); y2 <- suppressWarnings(as.numeric(df[[vars$y_name]]))
      ok <- !is.na(x2) & !is.na(y2); x2 <- x2[ok]; y2 <- y2[ok]
      
      if (metode == "Spearman Rank Correlation") {
        t <- tryCatch(cor.test(x2, y2, method = "spearman", exact = FALSE), error = function(e) NULL)
        if (!is.null(t)) {
          rho <- as.numeric(t$estimate)
          results[["Spearman Correlation"]] <- list(
            stat = rho, stat_label = paste0("ρ = ", round(rho, 4)),
            p_value = t$p.value, sig = t$p.value < alpha,
            h0 = "Tidak ada hubungan monoton (ρ = 0)", h1 = "Terdapat korelasi monoton (ρ ≠ 0)",
            extra = paste0("Arah hubungan: ", if (rho >= 0) "Positif (+)" else "Negatif (−)")
          )
        }
      } else if (metode == "Kendall's Tau Correlation") {
        t <- tryCatch(cor.test(x2, y2, method = "kendall", exact = FALSE), error = function(e) NULL)
        if (!is.null(t)) {
          tau <- as.numeric(t$estimate)
          results[["Kendall's Tau"]] <- list(
            stat = tau, stat_label = paste0("τ = ", round(tau, 4)),
            p_value = t$p.value, sig = t$p.value < alpha,
            h0 = "Tidak ada asosiasi ordinal (τ = 0)", h1 = "Terdapat asosiasi ordinal (τ ≠ 0)",
            extra = paste0("Tipe pasangan data: ", if (tau >= 0) "Concordant (+)" else "Discordant (−)")
          )
        }
      }
    }
    list(results = results, alpha = alpha, vars = vars, metode = metode)
  })
  # ---- 8. Reshape Data for Graphics Dinamis ----
  plot_df <- reactive({
    req(raw_data(), data_inputs())
    df <- raw_data(); vars <- data_inputs()
    out <- data.frame(y_val = numeric(), grp = factor())
    
    if (vars$type == "1-sample") {
      y <- suppressWarnings(as.numeric(df[[vars$y_name]])); ok <- !is.na(y)
      out <- data.frame(y_val = y[ok], grp = factor(rep("Sampel Uji", sum(ok))))
    } else if (vars$type == "independent") {
      y <- suppressWarnings(as.numeric(df[[vars$y_name]])); x <- as.character(df[[vars$x_name]])
      ok <- !is.na(y) & !is.na(x) & x != ""
      out <- data.frame(y_val = y[ok], grp = as.factor(x[ok]))
    } else if (vars$type == "paired-2") {
      y1 <- suppressWarnings(as.numeric(df[[vars$y_name]])); y2 <- suppressWarnings(as.numeric(df[[vars$y2_name]]))
      ok <- !is.na(y1) & !is.na(y2)
      out <- data.frame(y_val = c(y1[ok], y2[ok]), grp = as.factor(c(rep(vars$y_name, sum(ok)), rep(vars$y2_name, sum(ok)))))
    } else if (vars$type == "paired-k") {
      req(length(vars$k_names) >= 2)
      long_list <- lapply(vars$k_names, function(nm) {
        y <- suppressWarnings(as.numeric(df[[nm]]))
        data.frame(y_val = y, grp = nm)
      })
      out <- do.call(rbind, long_list)
      out <- out[!is.na(out$y_val), ]; out$grp <- as.factor(out$grp)
    } else if (vars$type == "correlation") {
      y1 <- suppressWarnings(as.numeric(df[[vars$x_name]])); y2 <- suppressWarnings(as.numeric(df[[vars$y_name]]))
      ok <- !is.na(y1) & !is.na(y2)
      out <- data.frame(y_val = c(y1[ok], y2[ok]), grp = as.factor(c(rep(vars$x_name, sum(ok)), rep(vars$y_name, sum(ok)))))
    }
    return(out)
  })
  
  # ---- 9. PLOTS ----
  output$plot_box <- renderPlot({
    req(plot_df())
    ggplot(plot_df(), aes(x = grp, y = y_val, fill = grp)) +
      geom_boxplot(alpha = 0.75, outlier.colour = "#c0392b", outlier.size = 2.5) +
      scale_fill_brewer(palette = PALETTE) + labs(title = "Boxplot Distribusi", x = "Grup", y = "Nilai") + theme_custom()
  })
  
  output$plot_violin <- renderPlot({
    req(plot_df())
    ggplot(plot_df(), aes(x = grp, y = y_val, fill = grp)) +
      geom_violin(trim = FALSE, alpha = 0.6) + geom_boxplot(width = 0.1, fill = "white", outlier.size = 0) +
      geom_jitter(width = 0.05, alpha = 0.2, size = 1.5) + scale_fill_brewer(palette = "Set1") +
      labs(title = "Violin Plot & Jitter Data", x = "Grup", y = "Nilai") + theme_custom()
  })
  
  output$plot_scatter <- renderPlot({
    req(raw_data(), data_inputs())
    vars <- data_inputs(); df <- raw_data()
    
    if (vars$type == "correlation") { x_c <- vars$x_name; y_c <- vars$y_name
    } else if (vars$type == "paired-2") { x_c <- vars$y_name; y_c <- vars$y2_name
    } else { return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Scatter plot hanya untuk Korelasi / 2 Sampel Berpasangan") + theme_void()) }
    
    df$x_v <- suppressWarnings(as.numeric(df[[x_c]])); df$y_v <- suppressWarnings(as.numeric(df[[y_c]]))
    df <- df[!is.na(df$x_v) & !is.na(df$y_v), ]
    
    ggplot(df, aes(x = x_v, y = y_v)) + geom_point(alpha = 0.6, colour = "#2980b9", size = 2.5) +
      geom_smooth(method = "lm", formula = y ~ x, colour = "#e74c3c", fill = "#ffcdd2", alpha = 0.2) +
      labs(title = "Scatter Plot Hubungan Pasangan Variabel", x = x_c, y = y_c) + theme_custom()
  })
  
  output$plot_density <- renderPlot({
    req(plot_df())
    ggplot(plot_df(), aes(x = y_val, fill = grp, colour = grp)) +
      geom_histogram(aes(y = after_stat(density)), alpha = 0.25, bins = 15, position = "identity") +
      geom_density(alpha = 0.1, linewidth = 1.0) + scale_fill_brewer(palette = PALETTE) + labs(title = "Kurva Densitas", x = "Nilai", y = "Kerapatan") + theme_custom()
  })
  
  output$plot_rank <- renderPlot({
    req(plot_df())
    df <- plot_df(); df$rnk <- rank(df$y_val)
    ggplot(df, aes(x = grp, y = rnk, fill = grp)) + geom_boxplot(alpha = 0.7) +
      scale_fill_brewer(palette = "Pastel1") + labs(title = "Distribusi Nilai Rank (Peringkat)", x = "Grup", y = "Rank") + theme_custom()
  })
  
  output$plot_means <- renderPlot({
    req(plot_df())
    summ <- plot_df() %>% group_by(grp) %>% summarise(mn = mean(y_val, na.rm=T), sd = sd(y_val, na.rm=T), med = median(y_val, na.rm=T), .groups = "drop")
    ggplot(summ, aes(x = grp, y = mn, fill = grp)) + geom_col(alpha = 0.8, width = 0.5) +
      geom_errorbar(aes(ymin = mn - sd, ymax = mn + sd), width = 0.2, linewidth = 0.8) +
      geom_point(aes(y = med), shape = 23, size = 4, fill = "white") +
      scale_fill_brewer(palette = "Set3") + labs(title = "Mean ± 1 SD (♦ = Median)", x = "Grup", y = "Mean / Rata-rata") + theme_custom()
  }) 
  # ---- 10. Summary Table ----
  output$tbl_summary <- renderTable({
    req(test_res())
    res <- test_res()$results; alpha <- test_res()$alpha
    if (length(res) == 0) return(data.frame(Keterangan = "Uji gagal dieksekusi. Periksa kecocokan data dengan syarat uji."))
    
    data.frame(
      `Nama Uji`     = names(res), `Statistik` = sapply(res, function(r) r$stat_label),
      `P-Value`      = sapply(res, function(r) formatC(r$p_value, format = "e", digits = 4)), `α` = alpha,
      `Signifikan?`  = sapply(res, function(r) if (r$sig) "Ya ✅"  else "Tidak ❌"),
      `Keputusan H₀` = sapply(res, function(r) if (r$sig) "Tolak H₀" else "Gagal Tolak H₀"), check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
  # ---- 11. Test Cards & Detailed Conclusions ----
  output$ui_test_cards <- renderUI({
    req(test_res())
    res <- test_res()$results; alpha <- test_res()$alpha
    if (length(res) == 0) return(div(class = "alert alert-danger", "Tidak ada hasil uji statistik yang valid untuk ditampilkan."))
    
    cards <- lapply(names(res), function(nm) {
      r <- res[[nm]]; cls <- if (r$sig) "card-sig" else "card-not"
      verd <- if (r$sig) "✅  SIGNIFIKAN — Tolak Hipotesis Nol (H₀)" else "❌  TIDAK SIGNIFIKAN — Gagal Menolak Hipotesis Nol (H₀)"
      vcls <- if (r$sig) "verdict-sig" else "verdict-not"
      
      div(class = cls, h4(style = "margin-top:0; color:#1a2e4a;", paste0("🔬 ", nm)),
          tags$table(class = "stat-table",
                     tags$tr(tags$th("Parameter"), tags$th("Nilai")),
                     tags$tr(tags$td("Statistik Uji"), tags$td(r$stat_label)),
                     tags$tr(tags$td("P-Value"),       tags$td(formatC(r$p_value, format = "e", digits = 4))),
                     tags$tr(tags$td("Alpha (α)"),     tags$td(alpha)),
                     tags$tr(tags$td("H₀"),            tags$td(r$h0)),
                     tags$tr(tags$td("H₁"),            tags$td(r$h1)),
                     tags$tr(tags$td("Keterangan"),    tags$td(r$extra))
          ),
          div(class = vcls, style = "margin-top:12px; font-size:15px;", verd)
      )
    })
    do.call(tagList, cards)
  })
  
  output$ui_conclusions <- renderUI({
    req(test_res())
    res <- test_res()$results; alpha <- test_res()$alpha; metode <- test_res()$metode
    
    hdr <- div(class = "card-info", h4("ℹ️ Informasi Hasil Akhir Analisis", style = "margin-top:0;"),
               tags$table(class = "stat-table",
                          tags$tr(tags$th("Parameter"), tags$th("Nilai")),
                          tags$tr(tags$td("Metode Uji"), tags$td(metode)),
                          tags$tr(tags$td("Tingkat Signifikansi α"), tags$td(alpha))
               )
    )
    if (length(res) == 0) return(tagList(hdr, div(class = "alert alert-warning", "Belum ada uji statistik yang berhasil dikalkulasi.")))
    
    make_conclusion <- function(nm, r) {
      if (r$sig) {
        switch(nm,
               "Mann-Whitney U" = paste("Terdapat perbedaan distribusi peringkat data yang signifikan antar kedua kelompok independen (p < α)."),
               "Wilcoxon One Sample" = paste("Terdapat bukti signifikan bahwa median sampel populasi tidak sama dengan nol (p < α)."),
               "Wilcoxon Paired Signed-Rank" = paste("Terdapat perbedaan signifikan yang konsisten secara berpasangan antara nilai pengukuran ke-1 dan ke-2 (p < α)."),
               "Friedman Test" = paste("Terdapat perbedaan efek perlakuan berpasangan yang signifikan secara statistik di antara kelompok yang diuji (p < α)."),
               "Spearman Correlation" = paste("Terdapat hubungan korelasi monoton yang bermakna secara statistik (p < α)."),
               "Kruskal-Wallis" = paste("Terdapat perbedaan bermakna pada minimal salah satu kelompok sampel independen yang dibandingkan (p < α)."),
               "Kendall's Tau" = paste("Terdapat tingkat asosiasi peringkat ordinal yang signifikan di dalam pasangan data (p < α)."),
               "Runs Test (Wald-Wolfowitz)" = paste("Urutan susunan data terbukti tidak bersifat acak secara statistik (p < α). Ada indikasi pola sistematik."),
               "Hasil signifikan."
        )
      } else {
        switch(nm,
               "Mann-Whitney U" = paste("Tidak ditemukan perbedaan distribusi bermakna. Kedua kelompok independen dianggap identik (p ≥ α)."),
               "Wilcoxon One Sample" = paste("Data sampel konsisten dengan nilai hipotesis awal. Median populasi dianggap sama dengan 0 (p ≥ α)."),
               "Wilcoxon Paired Signed-Rank" = paste("Tidak ditemukan perbedaan nilai yang berarti secara berpasangan antara kedua kondisi/waktu pengukuran (p ≥ α)."),
               "Friedman Test" = paste("Seluruh variasi kondisi berpasangan terbukti menghasilkan bentuk sebaran nilai yang relatif homogen/sama (p ≥ α)."),
               "Spearman Correlation" = paste("Tidak ada ikatan korelasi monoton yang cukup kuat atau bermakna secara statistik (p ≥ α)."),
               "Kruskal-Wallis" = paste("Seluruh kelompok sampel independen yang diuji dianggap berasal dari sebaran populasi yang setara/sama (p ≥ α)."),
               "Kendall's Tau" = paste("Hubungan kebersesuaian ordinal antar pasangan peringkat tergolong sangat lemah atau tidak nyata (p ≥ α)."),
               "Runs Test (Wald-Wolfowitz)" = paste("Urutan kemunculan data terbukti acak secara statistik (p ≥ α). Asumsi keacakan terpenuhi."),
               "Hasil tidak signifikan."
        )
      }
    }
    
    items <- lapply(names(res), function(nm) {
      r <- res[[nm]]; icon <- if (r$sig) "✅" else "❌"; verd <- if (r$sig) "TOLAK HIPOTESIS NOL (H₀)" else "GAGAL MENOLAK HIPOTESIS NOL (H₀)"
      div(class = if(r$sig) "card-sig" else "card-not",
          h4(paste(icon, nm), style = "margin-top:0; color:#1a2e4a;"),
          p(strong("📝 Kesimpulan Naratif: "), make_conclusion(nm, r)),
          div(style = "font-size:13px; font-weight:bold;", paste0(icon, "  Keputusan: ", verd, " pada α = ", alpha))
      )
    })
    tagList(hdr, br(), do.call(tagList, items))
  })
  
}

shinyApp(ui, server)