library(shiny)
library(dplyr)
library(DT)
library(htmltools)
library(magick)

# ---- LOAD DATA ----
selos <- read.csv("selos_disp.csv")

selos$max_qtd <- 1

selos$valor[selos$porte==1] = "3,85"
selos$valor[selos$porte==2] = "5,40"
selos$valor[selos$porte==3] = "7,45"

# ---- IMAGES ----
selos <- selos %>%
  rowwise() %>%
  mutate(img_html = {
    img <- image_read(imagem)
    dims <- image_info(img)
    
    lado_menor <- min(dims$width, dims$height)
    escala <- 40 / lado_menor
    
    nova_largura <- round(dims$width * escala)
    nova_altura <- round(dims$height * escala)
    
    paste0("<img src='", imagem, "' width='", nova_largura, "' height='", nova_altura, "'/>")
  }) %>%
  ungroup()

# ---- FUNCTION ----
encontrar_combinacoes <- function(selos, alvo, tolerancia = 0, max_selos = 3, minimizar_selos = TRUE) {
  
  selos$valor <- as.numeric(gsub(",", ".", selos$valor))
  selos <- selos[order(selos$valor), ]
  
  solucoes <- list()
  
  buscar <- function(i, atual_v, atual_q, caminho) {
    
    if (atual_v >= alvo && atual_v <= (alvo + tolerancia)) {
      solucoes <<- c(solucoes, list(caminho))
      return()
    }
    
    if (atual_v > (alvo + tolerancia) || atual_q >= max_selos || i > nrow(selos)) {
      return()
    }
    
    qtd_ja_usada <- sum(caminho == i)
    if (qtd_ja_usada < selos$max_qtd[i]) {
      buscar(i, atual_v + selos$valor[i], atual_q + 1, c(caminho, i))
    }
    
    buscar(i + 1, atual_v, atual_q, caminho)
  }
  
  buscar(1, 0, 0, c())
  
  if (length(solucoes) == 0) return(NULL)
  
  resultados_df <- lapply(solucoes, function(indices) {
    contagem <- table(indices)
    nomes_selos <- selos$nome[as.numeric(names(contagem))]
    
    desc <- paste(paste0(contagem, "x ", nomes_selos), collapse = " + ")
    
    imgs_html <- paste(selos$img_html[indices], collapse = "")
    
    data.frame(
      descricao = desc,
      total = round(sum(selos$valor[indices]), 2),
      n_selos = length(indices),
      visual = imgs_html,
      stringsAsFactors = FALSE
    )
  })
  
  final_df <- do.call(rbind, resultados_df) %>%
    distinct(descricao, .keep_all = TRUE)
  
  final_df <- final_df[order(final_df$n_selos, final_df$total), ]
  
  return(final_df)
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("Combinações de Selos"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("alvo", "Valor alvo (R$):", value = 7.45, step = 0.05),
      numericInput("max_selos", "Máximo de selos:", value = 3, min = 1),
      numericInput("tolerancia", "Tolerância:", value = 0, step = 0.05),
      actionButton("calcular", "Mostrar Resultados")
    ),
    
    mainPanel(
      h3("Resultados"),
      DTOutput("tabela")
    )
  )
)

# ---- SERVER ----
server <- function(input, output) {
  
  addResourcePath("selos", "selos")
  
  resultados <- eventReactive(input$calcular, {
    encontrar_combinacoes(
      selos,
      alvo = input$alvo,
      tolerancia = input$tolerancia,
      max_selos = input$max_selos
    )
  })
  
  output$tabela <- renderDT({
    res <- resultados()
    
    if (is.null(res)) return(NULL)
    
    res$visual <- gsub('\\"', '"', res$visual)
    res$visual <- lapply(res$visual, HTML)
    
    datatable(
      res,
      escape = -which(names(res) == "visual"),
      rownames = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)