# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                         #
#         Código R: Mapa coronavirus com dashboard        #
#                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Pacotes:
  # library(tidyverse)
  library(httr)
  library(leaflet)  
  library(readxl)
  library(rgdal)
  library(scales)
  library(plotly)
  library(htmltools)
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#              Para fazer os ultimos mortos 24h           #  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
  
  # tab_ms_sum_1<-tab_ms_sum
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#       Dados do Ministério da Saúde: webscraping         #  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Link das infos por UF
  u_ms<-"https://xx9p7hp1p7.execute-api.us-east-1.amazonaws.com/prod/PortalEstado"
  r_ms<-GET(u_ms)
  # content(r_ms) # deu nao autorizado
  # onde pegar o token?
  # x-parse-application-id: unAFkcaNDeXajurGB7LChj8SgQYS2ptm
  # isto é um header de requisicao, entao tem que colocar ali em cima
  r_ms<-GET(u_ms,
            add_headers("x-parse-application-id" = "unAFkcaNDeXajurGB7LChj8SgQYS2ptm"))
  
# Verificando conteúdo:
  # content(r_ms)[[1]]
  
  # Pegando apenas o results e transformando em dataframe
    tab_ms <- content(r_ms,
                      simplifyDataFrame=T)
  # verificando:
    head(tab_ms)
    tab_ms
    # class(tab_ms$results) # NULL
    class(tab_ms) # data frame
    
    tab_ms<-tab_ms[,c(1:4)]
    
    names(tab_ms)[1:4]<-c("id",
                          "uf",
                          "qtd_confirmado",
                          "qtd_obito")
  
    head(tab_ms)
  # tab_ms$results %>% unnest(qtd_confirmado) 
    
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#             Pegando lat e long para o mapa              #  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #   
lat_long<-read_excel("~/Desktop/R/Mapas/Mapas cloropeticos - referencia/Codigo UF IBGE + latlong.xlsx",)
tab_ms <- left_join(tab_ms,
                    lat_long) 
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#             Mapa casos confirmados (simples)            #  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #    
    
    tab_ms$letalidade_por_caso_conf<-tab_ms$qtd_obito/tab_ms$qtd_confirmado
    
    
# Etiqueta:    
  labels_confirmado<-sprintf("<strong>%s</strong><br/>%s casos confirmados",
                    tab_ms$nome,
                    format(tab_ms$qtd_confirmado,big.mark = ".",decimal.mark = ",")) %>%
                  lapply(htmltools::HTML)
  
  labels_obitos<-sprintf("<strong>%s</strong><br/>%s óbitos confirmados",
                          tab_ms$nome,
                          format(tab_ms$qtd_obito,big.mark = ".",decimal.mark = ",")) %>%
    lapply(htmltools::HTML)
  
  labels_letalidade<-sprintf("<strong>%s</strong><br/> letalidade: %s ",
                         tab_ms$nome,
                         percent(tab_ms$letalidade_por_caso_conf,0.1,
                                 big.mark = ".",decimal.mark = ",")) %>%
    lapply(htmltools::HTML)
  
  titulo<- "Mapa COVID-19</br> Dados: Min. Saúde </br> execução: Priscila F Hohberg"

  head(tab_ms)
  
# Fazendo o total:
  # tab_ms_sum_1 <- tab_ms_sum
  tab_ms_sum <- tab_ms %>% 
                summarise(qtd_confirmado=sum(qtd_confirmado),
                          qtd_obito=sum(qtd_obito))
  tab_ms_sum$letalidade_por_caso_conf<-tab_ms_sum$qtd_obito/tab_ms_sum$qtd_confirmado
  tab_ms_sum
  
  
# Fazendo o mapa:  
  tab_ms %>%
    mutate(latitude=as.numeric(latitude),
           longitude=as.numeric(longitude)) %>%
    leaflet() %>%
    addTiles() %>%
    # addLegend(position = "bottomright",
    #           title = titulo,
    #           opacity = 1,
    #           colors = "red") %>%
    addCircles(radius= ~qtd_confirmado*1,
               color="red",
               label= labels_confirmado,
               group = 'Casos confirmados') %>%
    addCircles(radius= ~qtd_obito*6,
               color="black",
               label= labels_obitos,
               group = 'Óbitos confirmados') %>%
    addCircles(radius= ~letalidade_por_caso_conf*800000,
               color="purple",
               label= labels_letalidade,
               group = 'Letalidade (%)') %>%
    addLayersControl(
      baseGroups = c('Casos confirmados',
                     'Óbitos confirmados',
                     'Letalidade (%)'),
      # overlayGroups = c("Quakes", "Outline"),
      options = layersControlOptions(collapsed = FALSE)) 
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#       Gráfico de barras com as infos do mapa            #  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
# qtd_confirmado  
  tab_ms %>%
    plot_ly(type = "bar", 
            x = ~nome, 
            y = ~qtd_confirmado, 
            # color = "Reds" ,
            marker = list(color="red"),
            text = paste0(tab_ms$nome,": ",
                          format(tab_ms$qtd_confirmado,
                          big.mark=".",
                          decimal.mark=","),
                          " casos confirmados"),
            hoverinfo = "text") %>%
    layout(xaxis = list(showline = F, 
                        showticklabels = F, 
                        fixedrange = T, 
                        title = "Estados"),
           yaxis = list(fixedrange = T, 
                        title = "# casos confirmados"),
           showlegend=F)
  
# qtd_obito  
  tab_ms %>%
    plot_ly(type = "bar", 
            x = ~nome, 
            y = ~qtd_obito, 
            # color = "Reds" ,
            marker = list(color="black"),
            text = paste0(tab_ms$nome,": ",
                          format(tab_ms$qtd_obito,
                                 big.mark=".",
                                 decimal.mark=","),
                          " casos confirmados"),
            hoverinfo = "text") %>%
    layout(xaxis = list(showline = F, 
                        showticklabels = F, 
                        fixedrange = T, 
                        title = "Estados"),
           yaxis = list(fixedrange = T, 
                        title = "# casos confirmados"),
           showlegend=F)
  
# letalidade_por_caso_conf  
  tab_ms %>%
    plot_ly(type = "bar", 
            x = ~nome, 
            y = ~letalidade_por_caso_conf, 
            # color = "Reds" ,
            marker = list(color="purple"),
            text = paste0(tab_ms$nome,": ",
                          format(round(tab_ms$letalidade_por_caso_conf*100,1),
                                 big.mark=".",
                                 decimal.mark=","),
                          "%"),
            hoverinfo = "text") %>%
    layout(xaxis = list(showline = F, 
                        showticklabels = F, 
                        fixedrange = T, 
                        title = "Estados"),
           yaxis = list(fixedrange = T, 
                        title = "# casos confirmados"),
           showlegend=F)
    
  
# com menu  
  plot_ly(tab_ms, x = ~nome) %>%
    add_bars(x = ~nome,
             y = ~qtd_confirmado, name="Casos confirmados",
             marker = list(color="red"),
             text = paste0(tab_ms$nome,": ",
                           format(tab_ms$qtd_confirmado,
                                  big.mark=".",
                                  decimal.mark=","),
                           " casos confirmados"),
             hoverinfo = "text") %>%
    add_bars(x = ~nome,
             y = ~qtd_obito, visible=F, name="Óbitos confirmados",
             marker = list(color="black"),
             text = paste0(tab_ms$nome,": ",
                           format(tab_ms$qtd_obito,
                                  big.mark=".",
                                  decimal.mark=","),
                           " casos confirmados"),
             hoverinfo = "text") %>%
    add_bars(x = ~nome,
             y = ~letalidade_por_caso_conf*100, visible=F, name="Letalidade (%)",
             marker = list(color="purple"),
             text = paste0(tab_ms$nome,": ",
                           format(round(tab_ms$letalidade_por_caso_conf*100,1),
                                  big.mark=".",
                                  decimal.mark=","),
                           "%"),
             hoverinfo = "text") %>%
    layout(xaxis = list(showline = F, 
                        showticklabels = F, 
                        fixedrange = T, 
                        title = "Estados"),
           yaxis = list(fixedrange = T),
           showlegend=F,
           updatemenus = list(
             list(
               x = "Pará",
               buttons = list(
                 list(method = "update",
                      args = list(list(visible = list(TRUE, FALSE,FALSE)),
                                  list(yaxis = list(title = "# casos confirmados"))),
                      label = "Casos confirmados"),
                 
                 list(method = "update",
                      args = list(list(visible = list(F, T,F)),
                                  list(yaxis = list(title = "# óbitos"))),
                      label = "Óbitos confirmados"),
                 
                 list(method = "update",
                      args = list(list(visible = list(F, F,T)),
                                  list(yaxis = list(title = "Letalidade (%)"))),
                      label = "Letalidade (%)")))
           )
    )
  
# # # Exemplo perfeito de grafico plotly com menu: 
#   plot_ly(mtcars, x = ~wt) %>%
#       add_markers(y = ~mpg, name = "mpg") %>%
#       add_markers(y = ~hp, name = "hp", visible = FALSE) %>%
#       layout(
#         title = "Drop down menus - Update",
#         xaxis = list(domain = c(0.1, 1)),
#         yaxis = list(title = "mpg"),
#         showlegend = FALSE,
#         updatemenus = list(
#           list(
#             y = 0.7,
#             buttons = list(
#               list(method = "update",
#                    args = list(list(visible = list(TRUE, FALSE)),
#                                list(yaxis = list(title = "mpg"))),
#                    label = "mpg"),
#               
#               list(method = "update",
#                    args = list(list(visible =  list(FALSE, TRUE)),
#                                list(yaxis = list(title = "hp"))),
#                    label = "hp")))
#         )
#       )
    
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#             Cruzando com dados da população             #  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #    
# Abrindo dados do IBGE:
  # dados_pop<-as.data.frame(read_excel("~/Desktop/R/Mapa coronavirus BR/Bases/Base UF IBGE.xlsx"))
  # verificando:
    # head(dados_pop)
# 
# # Arrumando base:
#   names(dados_pop)<-dados_pop[2,]
#   dados_pop<-dados_pop[-c(1,2),]
#   dados_pop<-dados_pop[c(1:27),]
#   # conferindo:
#     nrow(dados_pop)
#     head(dados_pop)
#   
#   
# # Pegando apenas a população  
#   dados_pop<-dados_pop[,c(1,2,7)]
#   names(dados_pop) <- c("nome",'CD_GEOCUF', "Populacao")
#   
# # Cruzando com dados do min saude:
#   tab_ms<-left_join(tab_ms,
#                     dados_pop,
#                     by='nome')
#   tab_ms<-as.data.frame(tab_ms)
#   # conferindo:
#     head(tab_ms)
#     
# # Calculando as taxas (por 100mil)
#   tab_ms$Populacao<-as.numeric(tab_ms$Populacao)  
#   tab_ms$taxa_confirmados<-(tab_ms$qtd_confirmado/tab_ms$Populacao)*100000
#     head(tab_ms)
#     
#   rm(dados_pop)  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #           Pegando e ajeitando o shapefile               #  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
#     
# # Lendo o shapefile (UF):
#   shape<-readOGR(dsn=path.expand('~/Desktop/R/Mapas cloropeticos - referencia/Shapefiles IBGE/br_unidades_da_federacao/'),
#                    "BRUFE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8")
#   class(shape) # OK, DEU CERTO!!    
# 
# 
# # Para juntar as informações:
#   dados_shape<-merge(shape,
#                      tab_ms,
#                          by.x="CD_GEOCUF",
#                          by.y="CD_GEOCUF")
#   
#   head(dados_shape)
#   class(dados_shape)
#   
# # Passando para lat e long:
#   latlong <- CRS("+proj=longlat +datum=WGS84 +no_defs")
#   dados_shape<-spTransform(dados_shape,latlong)
#   class(dados_shape)
#     
#     # Alterando o enconding nome estado:
#     Encoding(dados_shape$NM_ESTADO) <- "UTF-8"
#     
#     # rm(shape)
#     # rm(r_ms)
#     # rm(latlong)
#     # rm(labels)
#     # rm(tab_ms)
#     
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #             Fazendo mapa cloropético                    #  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 
# # verificar versao 1
#     
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #       Fazendo mapa cloropético com camadas              #  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #   
#     
# # Palhetas:
#   pal_qtd_confirmado <- colorBin("OrRd",
#                                 domain = dados_shape$qtd_confirmado, 
#                                 n=10)
#   
#   pal_taxa_confirmados <- colorBin("OrRd",
#                                  domain = dados_shape$taxa_confirmados, 
#                                  n=10)
#   
#   pal_qtd_obitos <- colorBin("OrRd",
#                                    domain = dados_shape$qtd_obito, 
#                                    n=10)
# 
# # Títulos:  
#   titulo_qtd_confirmado <- "Casos confirmados <br/>"
#   titulo_taxa_confirmados <- "Taxa de casos confirmados <br/> por 100mil habitantes"
#   titulo_qtd_obitos <- "Óbitos confirmados"
#     
# # Etiquetas:  
#   labels_qtd_confirmado <- sprintf("<strong>%s</strong><br/>%s casos confirmados",
#                                     dados_shape$nome,
#                                     format(dados_shape$qtd_confirmado,
#                                            big.mark = ".",
#                                            decimal.mark = ",")) %>% 
#                            lapply(htmltools::HTML)  
#   
#   labels_taxa_confirmados <- sprintf("<strong>%s</strong><br/>%s por 100mil habitantes",
#                                    dados_shape$nome,
#                                    format(round(dados_shape$taxa_confirmados,1),
#                                           big.mark = ".",
#                                           decimal.mark = ",")) %>% 
#                               lapply(htmltools::HTML)  
#   
#   labels_qtd_obito <- sprintf("<strong>%s</strong><br/>%s por 100mil habitantes",
#                                      dados_shape$nome,
#                                      format(dados_shape$qtd_obito,
#                                             big.mark = ".",
#                                             decimal.mark = ",")) %>%  
#                               lapply(htmltools::HTML)  
#   
#   labels_qtd_obito <- sprintf("<strong>%s</strong><br/>%s por 100mil habitantes",
#                               dados_shape$nome,
#                               format(dados_shape$qtd_obito,
#                                      big.mark = ".",
#                                      decimal.mark = ",")) %>%  
#     lapply(htmltools::HTML)  
    
# cinzas bons:  
  # qtd_confirmado 
    # leaflet(dados_shape) %>%
    #                   addPolygons(stroke = T,
    #                               weight = 0.6,
    #                               color = "#B5A8A8",
    #                               fillOpacity = 0.9,
    #                               smoothFactor = 0,
    #                               fill = T,
    #                               group = 'Casos confirmados <br/>(números absolutos)',
    #                               fillColor = ~pal_qtd_confirmado(qtd_confirmado),
    #                               label = labels_qtd_confirmado,
    #                               labelOptions = labelOptions(
    #                                                     style = list("font-weight" = "normal",
    #                                                                  padding = "3px 8px"),
    #                                                     textsize = "13px",
    #                                                     direction = "auto"))  %>%  
    #                   addPolygons(stroke = T,
    #                               weight = 0.6,
    #                               color = "#B5A8A8",
    #                               fillOpacity = 0.9,
    #                               smoothFactor = 0,
    #                               fill = T,
    #                               group = 'Taxa: casos confirmados <br/> a cada 100mil hab',
    #                               fillColor = ~pal_taxa_confirmados(taxa_confirmados),
    #                               label = labels_taxa_confirmados,
    #                               labelOptions = labelOptions(
    #                                 style = list("font-weight" = "normal",
    #                                              padding = "3px 8px"),
    #                                 textsize = "13px",
    #                                 direction = "auto"))  %>% 
    #                 addLegend("bottomright",
    #                             pal=pal_qtd_confirmado,
    #                             values= ~qtd_confirmado,
    #                             group  = 'Casos confirmados <br/>(números absolutos)',
    #                             title = titulo_qtd_confirmado,
    #                             opacity = 0.9) %>%
    #                   addLegend("bottomright",
    #                             pal=pal_taxa_confirmados,
    #                             values= ~taxa_confirmados,
    #                             group  = 'Taxa: casos confirmados <br/> a cada 100mil hab',
    #                             title = titulo_taxa_confirmados,
    #                             opacity = 0.9) %>%
    #                   addLayersControl(
    #                                 baseGroups = c('Casos confirmados <br/>(números absolutos)',
    #                                                'Taxa: casos confirmados <br/> a cada 100mil hab'),
    #                                    # overlayGroups = c("Quakes", "Outline"),
    #                                    options = layersControlOptions(collapsed = FALSE)) 
    # 
    # 
    # 
    
    
    
    
    