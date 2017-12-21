library(dplyr)
library(tidyr)
library(shiny)
library(ggplot2)
library(networkD3)
library(scales)
library(sna)
library(igraph)
ui <- pageWithSidebar(
        titlePanel("Rahul_Kumar_Goyal_SNA"),
        sidebarPanel(
                conditionalPanel(condition = "input.tabselected == 1",
                                 radioButtons("table", "Choose a table:", c("Normal Table" = "NT", "Tall Table" = "TT"))
                ),
                conditionalPanel(condition = "input.tabselected == 2",
                                 radioButtons("text", "Which Information:",
                                              c("Total Calls Placed" = "TC", "People who got received most calls" = "MD",
                                                "People who called the most"="MR"))),
                conditionalPanel(condition = "input.tabselected == 3",
                                 radioButtons("color", "Select color scale: ",
                                              c("Green/Yellow" = "G", "Black/Red" = "R"))
                ),
                conditionalPanel(condition = "input.tabselected == 4",
                                 radioButtons("plots", "Choose a network:",
                                              c("Connection Network" = "CN", "Outdegree/Outbound Calls Network" = "ON",
                                                "Indegree/Inbound Calls Network" = "IN", "Betweenness Network" = "BN",
                                                "Closeness Network" = 'CLN')))
        ),
        mainPanel(
                tabsetPanel(type = "tab",
                            tabPanel("Table", dataTableOutput(outputId = "table"), value = 1),
                            tabPanel("Text",textOutput(outputId = "text"), value = 2),
                            tabPanel("Map", plotOutput(outputId = "map"), value = 3),
                            tabPanel("Network",forceNetworkOutput(outputId = "net"), value = 4,
                                     textOutput(outputId = "abc")),
                            id = "tabselected"
                )
        )
)

server <- function(input,output){
        deal <- read.csv('COCAINE_DEALING.csv')
        colnames(deal)[1] <- "caller"
        new_deal <- gather(deal,called,count,-caller) %>% arrange(desc(count))
        l <- new_deal
        name <- levels(as.factor(l$caller))
        new_deal <- new_deal[-which(new_deal$count == 0),]
        calls_received <- new_deal %>% group_by(called) %>% summarise(calls = sum(count)) %>% arrange(calls)
        max_received_guys <- as.character(as.list(calls_received[which(calls_received[,2] == max(calls_received$calls)),][,1])[[1]])
        calls_dialled <- new_deal %>% group_by(caller) %>% summarise(calls = sum(count)) %>% arrange(calls)
        max_caller_guys <- as.character(as.list(calls_dialled[which(calls_dialled[,2]==max(calls_dialled$calls)),][,1])[[1]])
        total_calls <- sum(new_deal$count, na.rm = TRUE)
        name <- levels(as.factor(l$caller))
        group <- seq(0,length(name)-1)
        l$caller <- factor(l$caller, labels = group, levels = name)
        l$called <- factor(l$called, labels = group, levels = name)
        l <- l[-which(l$count == 0),]
        deal1 <- deal
        for(i in 1:nrow(deal)){
                for(j in 2:ncol(deal)){
                        if(deal[i,j] != 0){
                                deal1[i,j] = 1
                        }
                }
        }
        
        deal1$caller <- NULL
        rownames(deal1) <- colnames(deal1)
        indegree <- rowSums(deal1)
        outdegree <- colSums(deal1)
        deal2 <- deal
        deal2$caller <- NULL
        rownames(deal2) <- colnames(deal2)
        deal2 <- as.matrix(deal2)
        adjmatrix <- graph_from_adjacency_matrix(deal2, mode = 'undirected', weighted = TRUE)
        betweennes <- centralization.betweenness(adjmatrix)$res
        closeness <- centralization.closeness(adjmatrix)$res
        
        d1 = forceNetwork(l, data.frame(name,group = rep(1,28), size = rep(20,28)), Source = 'caller',
                          Target = "called", Value = 'count',
                          NodeID = "name", Group = "group",
                          fontSize = 15, clickAction = TRUE, opacityNoHover = 1, zoom = TRUE,
                          linkDistance = 150, opacity = 0.7, Nodesize = "size",
                          linkWidth = 2)
        
        ncallers <- data.frame(name, group)
        onlycallers <- unique(as.numeric(as.character(l$caller)))
        for(i in 1:nrow(ncallers)){
                if(length(which(onlycallers == ncallers$group[i])) > 0){ncallers$group1[i] <- "More than zero calls placed"}
                else{ncallers$group1[i] <- "Zero Calls Placed"}
        }
        lcallers <- l
        lcallers$value <- rep(0, nrow(lcallers))
        a <- aggregate(l$count, by = list(Category = l$caller), FUN = sum)
        lcallers$value[a$Category] <- a$x
        ncallers$size <- as.numeric(indegree) * 5
        
        d2 = forceNetwork(lcallers,ncallers,Source = "caller", Nodesize = "size",
                          Target = "called", Value = "count",NodeID = "name",
                          Group = "group1",fontSize =17, opacity = 0.8,
                          linkDistance = 150,
                          opacityNoHover = 1, zoom = TRUE,arrows = TRUE, legend = TRUE,
                          linkWidth = JS("function(d) { return d.value/2; }"),
                          colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"))
        nreceivers <- data.frame(name, group)
        onlyreceivers <- unique(as.numeric(as.character(l$called)))
        for(i in 1:nrow(nreceivers)){
                if(length(which(onlyreceivers == ncallers$group[i])) > 0){nreceivers$group1[i] <- 'More than zero calls received'}
                else{nreceivers$group1[i] <- 'Zero Calls received'}
        }
        lreceivers <- l
        lreceivers$value <- rep(0, nrow(lreceivers))
        b <- aggregate(l$count, by = list(Category = l$called), FUN = sum)
        lreceivers$value[b$Category] <- b$x
        nreceivers$size <- exp(as.numeric(outdegree)*2)
        
        d3 = forceNetwork(lreceivers,nreceivers,Source = "called", Nodesize = "size",
                          Target = "caller", Value = "count",
                          NodeID = "name", Group = "group1",fontSize =17, opacity = 0.8,
                          linkDistance = 150,
                          opacityNoHover = 1, zoom = TRUE,arrows = TRUE, legend = TRUE,
                          linkWidth = JS("function(d) { return d.value/2; }"),
                          colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),)
        
        deal1 <- deal
        for(i in 1:nrow(deal)){
                for(j in 2:ncol(deal)){
                        if(deal[i,j] != 0){
                                deal1[i,j] = 1
                        }
                }
        }
        
        group2 <- rep(1,28)
        for(i in 1:length(group2)){
                if(betweennes[i] > 0){group2[i] <- 'Betweenness greater than zero'}
                else{group2[i] <- 'Betweenness equal to zero'}
        }
        d4 = forceNetwork(l, data.frame(name,group = group2, size = betweennes), Source = 'caller',
                          Target = "called", Value = 'count',
                          NodeID = "name", Group = "group",
                          fontSize = 15, clickAction = TRUE, opacityNoHover = 1, zoom = TRUE,
                          linkDistance = 150, opacity = 0.7, Nodesize = "size",
                          linkWidth = JS("function(d) { return d.value/2; }"), legend = TRUE,
                          colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),)
        
        group3 <- rep(1,28)
        for(i in 1:length(group3)){
                if(closeness[i] > 0.5){group3[i] <- 'Closeness greater than 0.5'}
                else{group3[i] <- 'Closeness less than 0.5'}
        }
        d5 = forceNetwork(l, data.frame(name,group = group3, size = exp(closeness*7)), Source = 'caller',
                          Target = "called", Value = 'count',
                          NodeID = "name", Group = "group",
                          fontSize = 15, clickAction = TRUE, opacityNoHover = 1, zoom = TRUE,
                          linkDistance = 150, opacity = 0.7, Nodesize = "size",
                          linkWidth = JS("function(d) { return d.value/2; }"), legend = TRUE,
                          colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),)
        
        
        output$map <- renderPlot({
                par(mar=c(5.1,4.1,4.1,2.1))
                g1 <- ggplot(new_deal, aes(x = called, y = caller, fill = count))+
                        geom_tile() + scale_fill_gradientn(colours = c("black","white","darkred"), 
                                                           values = rescale(c(0,10,20)),
                                                           guide = "colorbar", limits=c(0,20)) +
                        labs(x = "") + labs(y = "")
                g2 <- ggplot(new_deal, aes(x = called, y = caller, fill = count))+
                        geom_tile() + scale_fill_gradientn(colours = c("darkgreen","white","yellow"), 
                                                           values = rescale(c(0,10,20)),
                                                           guide = "colorbar", limits=c(0,20)) + 
                        labs(x = "") + labs(y = "")
                switch(input$color, R = plot(g1), G = plot(g2))
        })
        output$abc <- renderPrint({
                cat('1. Link Width is the number of calls on the edge')
                cat(' ')
                cat('2. Node radius is the centrality measure in that specific graph')
                cat('3. Node color is mentioned in the legend')
        })
        output$table <- renderDataTable({
                switch(input$table, NT = deal, TT = new_deal)
        })
        output$text <- renderPrint({
                switch(input$text, TC = cat(total_calls),
                       MD = cat(max_received_guys) , MR =cat(max_caller_guys))
        })
        output$net <- renderForceNetwork(
                switch(input$plots,
                       CN = d1, ON = d2, IN = d3, BN = d4, CLN = d5
                )
        )
}

shinyApp(ui = ui, server = server)