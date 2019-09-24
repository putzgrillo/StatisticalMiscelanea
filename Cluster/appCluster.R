library(data.table)
library(plotly)
library(tidyverse)
library(cluster)
library(purrr)
library(gridExtra)
library(GGally)
library(ggfortify)
library(dendextend)
library(combinat)


library(reshape2)
library(caret)
library(tidyverse)
library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(shinydashboard)

options(encoding = "UTF-8")
# LISTA CLUSTERS 
load("clusterApp.Rdata")

# CRIAR INPUTS ----
# CRIAR INPUTS: CLUSTERS DISPONÍVEIS ----
clustersDisponiveis <- names(clusters$dfClusterObservacoes)
names(clustersDisponiveis) <- clustersDisponiveis

# CRIAR INPUTS: VARIÁVEIS ----
variaveisClusters <- colnames({clusters$dfUtilizado %>% select_if(is.numeric)})
names(variaveisClusters) <- variaveisClusters


shinyApp(
  # UI ----
  ui = dashboardPage(
    skin = "green",
    # UI: CABEÇALHO ----
    dashboardHeader(title = 'Cluster'),
    # UI: BARRA LATERAL ----
    dashboardSidebar(width = 300,
                     sidebarMenu(
                       tags$hr(" Análise Candidatos"),
                       menuItem("Sumário Candidatos", tabName = "sumarioCandidatos", icon = icon("info"), selected = TRUE),
                       
                       tags$hr(" Visualização Cluster Escolhido"),
                       
                       menuItem("Tabela Não-Paramétrica", tabName = "tabNaoPar", icon = icon("table")),
                       menuItem("Gráfico por Cliente", tabName = "plotClnt", icon = icon("chart-bar")),
                       menuItem("Gráfico por Cluster", tabName = "plotClst", icon = icon("chart-line")),
                       
                       tags$hr(" Filtros"),
                       
                       menuItem(
                         "Filtro Escolher Modelo Cluster", tabName = "selecaoCamp", icon = icon("filter"),
                         selectInput(inputId = "cluster", label = "Cluster:",
                                     choices = clustersDisponiveis, selected = names(clusters$dfClusterObservacoes)[1])
                       ),
                       menuItem(
                         #startExpanded = TRUE,
                         "Filtros por Cliente", tabName = "selecaoPlotCliente", icon = icon("filter"),
                         selectInput("varPlotX", "Eixo X:", variaveisClusters, selected = variaveisClusters[length(variaveisClusters) - 1]),
                         selectInput("varPlotY", "Eixo Y:", variaveisClusters, selected = variaveisClusters[length(variaveisClusters) - 2]),
                         selectInput("varPlotZ", "Eixo Z:", variaveisClusters, selected = variaveisClusters[length(variaveisClusters) - 3]),
                         checkboxInput("transfExpClnt", "Transformação Exponencial")
                       ),
                       menuItem(
                         "Filtros por Cluster", tabName = "selecaoPlotCluster", icon = icon("filter"),
                         selectInput("varPlotBox", "Variável Box Plot:", variaveisClusters),
                         selectInput("clusterX", "Eixo X Cluster:", variaveisClusters, selected = variaveisClusters[length(variaveisClusters) - 1]),
                         selectInput("clusterY", "Eixo Y Cluster:", variaveisClusters, selected = variaveisClusters[length(variaveisClusters) - 2]),
                         selectInput("clusterZ", "Eixo Z Cluster:", variaveisClusters, selected = variaveisClusters[length(variaveisClusters) - 3]),
                         checkboxInput("transfExpClst", "Transformação Exponencial")
                       )
                     )
    ),
    # UI: CORPO ----
    dashboardBody(
      tabItems(
        # UI: CORPO: SUMARIO CANDIDATOS ----
        tabItem(tabName = 'sumarioCandidatos',
                fluidRow(
                  box(title = "Comparação Clusters Candidatos", width = 8,
                      fluidRow(plotlyOutput("plotSil"))
                  ),
                  box(title = "Freq Var Opcns (Q3)", width = 4,
                      fluidRow(DT::dataTableOutput("dfMelhores"), width = 12),
                      fluidRow(DT::dataTableOutput("dfVariaveisEscolhidas"), width = 12)
                  ),
                  box(title = "Tabela Sumário", width = 12,
                      fluidRow(DT::dataTableOutput("dfSumario"), width = 12)
                  )
                )
        ),
        # UI: CORPO: TABELA NÃO-PARAMÉTRICA ----
        tabItem(tabName = 'tabNaoPar',
                # fluidRow(
                #   box(title = "Tabela Dados Não-Paramétricos", width = 12, 
                #       fluidRow(dataTableOutput("dfNaoParm"))
                #   ) 
                # ) 
                fluidRow(dataTableOutput("dfNaoParm")) 
        ),
        # UI: CORPO: GRÁFICO CLIENTE -----
        tabItem(tabName = 'plotClnt',
                fluidRow(
                  box(title = "Scatter 3D por Cliente", width = 12,
                      fluidRow(plotlyOutput("plotCluster"))
                  ),
                  box(title = "Tabela Não-Paramétrica Eixos", width = 12,
                      fluidRow(dataTableOutput("dfNpPorCliente")) 
                  )
                )
        ),
        # UI: CORPO: GRÁFICOS CLUSTER -----
        tabItem(tabName = 'plotClst',
                fluidRow(
                  box(title = "Scatter 3D por Cluster", width = 12,
                      fluidRow(plotlyOutput("plotPorCluster"))
                  ),
                  box(title = "Box-Plot por Cluster", width = 12,
                      fluidRow(plotlyOutput("plotBox"), width = 12)
                  )
                ) 
        )
        # 
      )
    )
  ),
  
  # SERVER ----
  server = function(input, output) {
    # REACTIVE: 
    # REACTIVE: VARIÁVEIS UTILIZADAS NO AGRUPAMENTO ----
    dfVariaveisEscolhidas <- reactive({
      indiceColunas <- input$cluster %>%
        strsplit(., split = "<>") %>%
        {.[[1]][1]} %>%
        strsplit(., split = "\\|") %>%
        unlist %>%
        as.numeric
      
      data.frame(Variaveis = colnames(clusters$dfUtilizado)[indiceColunas], stringsAsFactors = FALSE)
      
    })
      # REACTIVE: DF NÃO-PARAMÉTRICA ----
    dfNaoParm <- reactive({
      baseCluster <- data.frame(
        clusters[["dfUtilizado"]],
        Cluster = clusters[["dfClusterObservacoes"]][[input$cluster]]$Cluster,
        stringsAsFactors = FALSE
      )
      
      baseCluster %>%
        group_by(Cluster) %>%
        select_if(is.numeric) %>%
        summarise_all(
          funs(
            MEDIA = mean,
            MEDIANA = median, 
            Q1 = quantile(., probs = 0.25), 
            Q3 = quantile(., probs = 0.75))
        ) %>%
        merge(., {
          baseCluster %>%
            group_by(Cluster) %>%
            summarise(
              zTamCluster = length(Cluster),
              zPropCluster = length(Cluster) / nrow(baseCluster)
            )
        }) %>%
        t %>%
        data.frame(
          Metrica = row.names(.), ., stringsAsFactors = FALSE
        ) %>%
        arrange(desc(Metrica)) %>%
        as.data.frame
      
    })
    
      # REACTIVE: SCATTER 3D ----
    plotCluster <- reactive({
      dfPlot <- data.frame(
        clusters[["dfUtilizado"]],
        Cluster = clusters[["dfClusterObservacoes"]][[input$cluster]]$Cluster,
        stringsAsFactors = FALSE
      )
      dfPlot <- data.frame(
        .X = dfPlot[, which(colnames(dfPlot) == input$varPlotX)],
        .Y = dfPlot[, which(colnames(dfPlot) == input$varPlotY)],
        .Z = dfPlot[, which(colnames(dfPlot) == input$varPlotZ)],
        .C = dfPlot[, which(colnames(dfPlot) == "Cluster")],
        dfPlot[, which(colnames(dfPlot) %in% variaveisClusters)]
      )
      
      if(input$transfExpClnt) {
        dfPlot <- dfPlot %>%
          mutate(
            .X = log(.X + 1),
            .Y = log(.Y + 1),
            .Z = log(.Z + 1)
          )
      }
      
      plot_ly(dfPlot,
              x = ~.X, 
              y = ~.Y, 
              z = ~.Z, 
              type = "scatter3d", mode = "markers", color = ~.C,
              hoverinfo = 'text', 
              text = ~paste(
                '</br> Cluster: ', .C,
                '</br> Eixo X: ', round(.X, 2),
                '</br> Eixo Y: ', round(.Y, 2),
                '</br> Eixo Z: ', round(.Z, 2)
                # '</br> Receita Total: ', round(RECEITA_TOTAL, 2),
                # '</br> Valor Compras On: ', round(VALOR_COMPRA_ON, 2)
              ) 
      ) %>%
        layout(
          title = "",
          scene = list(
            xaxis = list(title = input$varPlotX),
            yaxis = list(title = input$varPlotY),
            zaxis = list(title = input$varPlotZ)
          )) 
      
    })
    
      # REACTIVE: BOX-PLOT ----
    plotBox <- reactive({
      dfPlotBox <- data.frame(
        clusters[["dfUtilizado"]],
        Cluster = clusters[["dfClusterObservacoes"]][[input$cluster]]$Cluster,
        stringsAsFactors = FALSE
      )
      dfPlotBox <- data.frame(
        .C = as.factor(dfPlotBox[, which(colnames(dfPlotBox) == "Cluster")]),
        .Y = dfPlotBox[, which(colnames(dfPlotBox) == input$varPlotBox)]
      )
      
      if(input$transfExpClst) {
        dfPlotBox <- dfPlotBox %>%
          mutate(.Y = log(.Y + 1))
      }
      
      ggplotBox <- ggplot(dfPlotBox, 
                          aes_string(x = ".C", y = ".Y", fill = ".C")) + 
        geom_boxplot()
      ggplotly(ggplotBox)
    })
    
      # REACTIVE: TABELA INFOS NÃO-PARAMÉTRICAS ABA POR CLIENTE ----
    dfNpPorCliente <- reactive({
      baseCluster <- data.frame(
        clusters[["dfUtilizado"]],
        Cluster = clusters[["dfClusterObservacoes"]][[input$cluster]]$Cluster,
        stringsAsFactors = FALSE
      )
      
      colunasUsar <- c("Cluster", input$varPlotX, input$varPlotY, input$varPlotZ)
      baseCluster <- baseCluster[, colunasUsar]
      
      baseCluster %>%
        group_by(Cluster) %>%
        select_if(is.numeric) %>%
        summarise_all(
          funs(
            MEDIA = mean,
            MDIANA = median, 
            Q1 = quantile(., probs = 0.25), 
            Q3 = quantile(., probs = 0.75))
        ) %>%
        merge(., {
          baseCluster %>%
            group_by(Cluster) %>%
            summarise(
              zTamCluster = length(Cluster),
              zPropCluster = length(Cluster) / nrow(baseCluster)
            )
        }) %>%
        t %>%
        data.frame(
          Metrica = row.names(.), ., stringsAsFactors = FALSE
        ) %>%
        arrange(desc(Metrica)) %>%
        as.data.frame
      
    })
      # REACTIVE: SCATTER POR CLUSTER ----
    plotPorCluster <- reactive({
      baseClust <- data.frame(
        clusters[["dfUtilizado"]],
        Cluster = as.factor(clusters[["dfClusterObservacoes"]][[input$cluster]]$Cluster),
        stringsAsFactors = FALSE
      )
      
      baseClust <- baseClust %>%
        group_by(
          Cluster
        ) %>%
        summarise_if(
          is.numeric, mean, na.rm = TRUE # USAR MÉDIA PARA TODAS VARIÁVEIS NUMÉRICAS
        ) %>% 
        merge(., {
          baseClust %>%
            group_by(Cluster) %>%
            summarise(Frequencia = n()) %>%
            mutate(PropGrupo = Frequencia / sum(Frequencia))
        }) %>%
        as.data.frame
      
      dfPlot <- data.frame(
        .X = baseClust[, which(colnames(baseClust) == input$clusterX)],
        .Y = baseClust[, which(colnames(baseClust) == input$clusterY)],
        .Z = baseClust[, which(colnames(baseClust) == input$clusterZ)],
        .C = baseClust[, which(colnames(baseClust) == "Cluster")],
        .S = (baseClust[, which(colnames(baseClust) == "PropGrupo")])
      )
      
      if(input$transfExpClst) {
        dfPlot <- dfPlot %>%
          mutate(
            .X = log(.X + 1),
            .Y = log(.Y + 1),
            .Z = log(.Z + 1)
          )
      }
      
      
      plot_ly(dfPlot,
              x = ~.X, 
              y = ~.Y, 
              z = ~.Z, 
              type = "scatter3d", mode = "markers", color = ~.C, size = ~.S, sizes = c(400, 4000),
              hoverinfo = 'text', 
              text = ~paste(
                '</br> Cluster: ', .C,
                '</br> X: ', round(.X, 2),
                '</br> Y: ', round(.Y, 2),
                '</br> Z: ', round(.Z, 2)
              )
      ) %>%
        layout(
          title = "Comparacao Clusters",
          scene = list(
            xaxis = list(title = input$clusterX),
            yaxis = list(title = input$clusterY),
            zaxis = list(title = input$clusterZ)
          )) 
      
    })
    
    # NÃO-REACTIVE ----
      # NÃO-REACTIVE: SCATTER + PLANO DOS CLUSTERS CANDIDATOS ----
    comparacaoClusters <- clusters$dfTodosCandidatos
    
    # MODELO PARA PLANO
    lmPlano <- train(
      SilhuetaMax ~ NumClusters + nVariaveis,
      data = comparacaoClusters,
      method = "rf",
      metric = "RMSE",
      trControl = trainControl(method = "repeatedcv", number = 5, repeats = 2),
      preProcess = c('center', 'scale')
    )
    
    # VALORES PARA PLANO
    eixoX <- seq(min(comparacaoClusters$NumClusters), max(comparacaoClusters$NumClusters), by = 1)
    eixoY <- seq(min(comparacaoClusters$nVariaveis), max(comparacaoClusters$nVariaveis), by = 1)
    planoScatter <- expand.grid(NumClusters = eixoX, nVariaveis = eixoY, KEEP.OUT.ATTRS = F)
    planoScatter$SilhuetaMax <- predict(lmPlano, newdata = planoScatter)
    planoScatter <- acast(planoScatter, nVariaveis ~ NumClusters, value.var = "SilhuetaMax")
    # PLOT 
    plotSilhueta <- plot_ly(comparacaoClusters,
                            x = ~NumClusters, 
                            y = ~nVariaveis, 
                            z = ~SilhuetaMax, 
                            type = "scatter3d", mode = "markers", color = ~Cluster, 
                            symbol = ~Escolhido, symbols = c('circle', 'diamond'),
                            hoverinfo = 'text', 
                            text = ~paste('</br> SilhuetaMax: ', round(SilhuetaMax, 3),
                                          '</br> Numero Variaveis: ', round(nVariaveis, 3),
                                          '</br> Numero Clusters: ', round(NumClusters, 3),
                                          '</br> Metodo: ', Cluster,
                                          '</br> Variaveis Usadas: ', Iteracao)
    ) %>%
      layout(
        legend = list(orientation = 'h'),
        title = "MaxSilhueta em funcao de k-clusters e n-variaveis",
        scene = list(
          xaxis = list(title = "Numero Clusters"),
          yaxis = list(title = "Numero Variaveis"),
          zaxis = list(title = "Silhueta Max")
        )) %>% 
      # ADICIONAR PLANO AO GRÁFICO
      add_trace( 
        z = ~planoScatter,
        x = ~eixoX,
        y = ~eixoY, 
        type = 'surface', 
        alpha = 0.2)
    
      # NÃO-REACTIVE: SUMÁRIO CLUSTERS ---- 
    dfSumario <- clusters[["dfSumarioClusters"]]
    
    
    # SAÍDAS APP ----
      # SAÍDAS APP: NR TABELA SUMARIO ----
    output$dfSumario <- renderDataTable({
      datatable(dfSumario,extensions = 'FixedColumns',
                options = list(
                  dom = 't',
                  scrollX = TRUE,
                  fixedColumns = TRUE
                ))
    })
      # SAÍDAS APP: NR GRAFICO SILHUETA ----
    output$plotSil <- renderPlotly({
      plotSilhueta
    })
    
      # SAÍDAS APP: R TABELA NÃO-PARAMPETRICA (NÃO FORMATADA) ----
    output$dfNaoParm <- renderDataTable({
      nomesColunas <- colnames(dfNaoParm())
      datatable(dfNaoParm(), extensions = 'Buttons',
                options = list(
                  dom = 'Bfrtip',
                  scrollX = TRUE,
                  pageLength = 300,
                  fixedColumns = TRUE,
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                )) %>%
        formatRound(nomesColunas, digits = 3)
    })

      # SAÍDAS APP: R GRÁFICO CLUSTER ----
    output$plotCluster <- renderPlotly({
      plotCluster()
    })
    
      # SAÍDAS APP: TABELA INFOS NÃO-PARAMÉTRICAS ABA POR CLIENTE ----
    output$dfNpPorCliente <- renderDataTable({
      nomesColunas <- colnames(dfNpPorCliente())
      datatable(dfNpPorCliente(), extensions = 'Buttons',
                options = list(
                  dom = 'Bfrtip',
                  scrollX = TRUE,
                  pageLength = 300,
                  fixedColumns = TRUE,
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                )) %>%
        formatRound(nomesColunas, digits = 3)
    })
      # SAÍDAS APP: R GRÁFICO BOX-PLOT ----
    output$plotBox <- renderPlotly({
      plotBox()
    })
      # SAÍDAS APP: SCATTER POR CLUSTER ----
    output$plotPorCluster <- renderPlotly({
      plotPorCluster()
    })
    
      # SAÍDAS APP: VARIÁVEIS UTILIZADAS NO CLUSTER ----
    output$dfVariaveisEscolhidas <- renderDataTable({
      datatable(dfVariaveisEscolhidas(), extensions = "FixedColumns",
                options = list(
                  dom = 't',
                  scrollX = TRUE,
                  fixedColumns = TRUE
                ))
    })
      # SAÍDAS APP: VARIÁVEIS RECORRENTES MELHORES CLUSTERS (Q3) ----
    output$dfMelhores <- renderDataTable({
      nomesVariaveis <- colnames(clusters$dfUtilizado)
      melhoresClusters <- clusters$dfMetricaClusters %>%
        filter(SilhuetaMax > quantile(SilhuetaMax, probs = 0.75)) %>%
        mutate(Iteracao = as.character(Iteracao))
      variaveisRcrrnc <- strsplit(melhoresClusters$Iteracao, split = "\\|") %>% unlist
      dfCntgnc <- as.data.frame(xtabs(~variaveisRcrrnc), stringsAsFactors = FALSE)
      dfCntgnc <- data.frame(
        Variavel = nomesVariaveis[as.numeric(dfCntgnc$variaveisRcrrnc)], 
        Frequencia= dfCntgnc$Freq
      ) %>% 
        filter(Frequencia < max(Frequencia)) %>% # TIRAR FIXAS
        arrange(-Frequencia)
      
      datatable(dfCntgnc,extensions = 'FixedColumns',
                options = list(
                  dom = 't',
                  scrollX = TRUE,
                  fixedColumns = TRUE
                ))
      
    })
  }
)
