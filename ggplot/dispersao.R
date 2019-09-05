library(tidyverse)

# SCATTER 2D (COM 6 VARIÁVEIS) ----
  # GERAR BASE ----
dfPlotGeral <- dfClnts %>%
  filter(
    FAIXA_CLASSE != "SEM_CLASSIFICACAO"
    # & FREQUENCIA >= (15 * 12)
  ) %>%
  group_by(SEXO, FAIXA_CLASSE, FAIXA_ETARIA) %>%
  summarise(
    CLNTS = n_distinct(CD_CLNT),
    GASTO_MEDIO = mean(VALOR),
    FREQ_MEDIA = mean(FREQUENCIA),
    AGRUPAMENTOS_DISTINTOS = mean(AGRUPAMENTOS_DISTINTOS),
    MESES_COM_TRANSACAO = mean(MESES_TRANSACAO),
    LIMITE_MEDIO = mean(VL_LIMT_COMP, na.rm = TRUE)
  ) %>%
  mutate(
    MEDIA_GASTO_MES_COMPRA = GASTO_MEDIO / MESES_COM_TRANSACAO,
    MEDIA_GASTO_MENSAL = GASTO_MEDIO / 12
  ) %>%
  na.omit

  # GERAR VALORES MÉDIOS ----
gastoMedio <- weighted.mean(x = dfPlotGeral$GASTO_MEDIO, w = dfPlotGeral$CLNTS)
freqMedia <- weighted.mean(x = dfPlotGeral$FREQ_MEDIA, w = dfPlotGeral$CLNTS)
ticketMedio <- sum(dfPlotGeral$GASTO_MEDIO * dfPlotGeral$CLNTS) / sum(dfPlotGeral$FREQ_MEDIA * dfPlotGeral$CLNTS)

  # PLOT ----
ggplot(dfPlotGeral, aes(y = GASTO_MEDIO, x = FREQ_MEDIA, colour = FAIXA_CLASSE, shape = FAIXA_ETARIA)) + 
  geom_point(aes(fill = FAIXA_CLASSE, alpha = SEXO, size = CLNTS)) +
  geom_point(aes(size = CLNTS)) +
  scale_size(range = c(10, 50)) +
  scale_shape_manual(values = c(21, 22, 23)) +
  scale_alpha_manual(values = c("M" = 0.1, "F" = 0.75)) +
  ylab("Gasto Médio Anual (R$)") + xlab("Frequência Média Anual (#)") +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(labels = scales::number_format()) +
        # LINHAS
  geom_abline(intercept = 0, slope = ticketMedio, linetype = 2, size = 2) + 
  geom_hline(yintercept = gastoMedio, linetype = 3, size = 1.5, colour = "darkblue") +
  geom_vline(xintercept = freqMedia, linetype = 3, size = 1.5, colour = "goldenrod2") +
  theme_bw() +
  guides(
    shape = guide_legend(override.aes = list(size = 18)), 
    colour = guide_legend(override.aes = list(size = 18)),
    alpha = guide_legend(override.aes = list(size = 18)),
    size = FALSE) +
  theme(
    legend.text = element_text(size = 18),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 18)
  )
  
  
 

# SCATER 2D PLOTLY (COM TEXTO) ----
plotTs <- ggplot(data = dfPlotar,
                 mapping = aes(x = ANOMES, y = Valor, shape = TIPO_EP, colour = TIPO_EP, 
                               text = paste("Valor ($)", format(round(Valor, 2), nsmall = 2, big.mark = ","), 
                                            "<br>Qtde (#)", format(round(Qtde, 0), nsmall = 0, big.mark = ","), 
                                            "<br>Clientes (#)", format(round(Clientes, 0), nsmall = 0, big.mark = ","), 
                                            "<br>Taxa Média (%)", round(TaxaMedia * 100, 2), 
                                            "<br>Plano Médio (#)", round(PlanoMedio, 1), 
                                            "<br>Ticket Médio ($)", format(round(TicketMedio, 2), nsmall = 2, big.mark = ","))
                 )) +
  geom_point() +
  geom_line(group = 1) +
  scale_y_continuous(labels = scales::dollar) + 
  facet_grid(facets = TIPO_EP ~ ., scale = "free_y")

ggplotly(plotTs, tooltip = "text")


# SCATTER 3D (COM SUPERFÍCIE) ----
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
    

# SCATTER 3D (AGRUPADO E COM TAMANHO) ----
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
