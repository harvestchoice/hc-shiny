#####################################################################################
# Title: Ethiopia Segmentation Study
# Date: August 2014
# Project: HarvestChoice for the Bill and Melinda Gates Foundation
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

setwd("/home/projects/www/2014_BMGF")

library(data.table)
library(survey)
library(shiny)
library(ggmap)

# Load saved image
load("./temp/eth_bmgf_seg_app.RData")

par(bty="n", family="Helvetica-Narrow", cex.axis=.8, cex.sub=.9, font.main=1, adj=0)

ethMap <- qmap(c(33.00, 3.32, 47.99, 14.90),
    zoom=6, color="bw", source="google", maptype="hybrid", darken=c(.4, "#F1F1F3")) 

theme_opts <- list(theme(
        plot.title=element_text(size=12, hjust=0),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        panel.background=element_blank(),
        plot.background=element_blank(),
        panel.border=element_blank(),
        axis.title=element_blank(),
        axis.line=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position="right",
        legend.direction = "vertical",
        axis.ticks = element_blank()))

pal <- c("#38A700", "#cdfbb7", "#a0ff74", "#ffffda", "#ceab67")
pal <- pal[5:1]

# Define server logic for slider examples
shinyServer(function(input, output, session) {
      
      output$sldelev <- renderUI({ sliderInput("sldelev", "Elevation (meters):",
                min=eth.l3.dt[, min(seg3_elev, na.rm=T)],
                max=eth.l3.dt[, max(seg3_elev, na.rm=T)],
                value=1100, step=100) })
      
      output$sldrain <- renderUI({ sliderInput("sldrain", "Rainfall (mm/year):",
                min=eth.l3.dt[, min(seg3_pre, na.rm=T)],
                max=eth.l3.dt[, max(seg3_pre, na.rm=T)],
                value=c(600, 900), step=100) })
      
      output$sldtt20k <- renderUI({ sliderInput("sldtt20k", "Market access (hrs):",
                min=eth.l3.dt[, min(seg2_tt20k, na.rm=T)],
                max=eth.l3.dt[, max(seg2_tt20k, na.rm=T)],
                value=c(3,5), step=0.5) })
      
      output$sldpopden <- renderUI({ sliderInput("sldpopden", "Population density (sq. km.):",
                min=eth.l3.dt[, min(seg3_popden, na.rm=T)],
                max=eth.l3.dt[, max(seg3_popden, na.rm=T)],
                value=c(90,200), step=10) })    
      
      
      out <- reactive({
            
            if ( is.null(input$sldelev) ) {
              # Set default input values
              sldelev <- 1100
              sldrain1 <- 600
              sldrain2 <- 900
              sldtt20k1 <- 3
              sldtt20k2 <- 5
              sldpopden1 <- 90
              sldpopden2 <- 200              
              
            } else {
              # Dynamic values
              sldelev <- input$sldelev
              sldrain1 <- input$sldrain[1]
              sldrain2 <- input$sldrain[2]
              sldtt20k1 <- input$sldtt20k[1]
              sldtt20k2 <- input$sldtt20k[2]
              sldpopden1 <- input$sldpopden[1]
              sldpopden2 <- input$sldpopden[2]
            }
            
            d <- copy(eth.l3.dt)
            d[, seg3_elev_clas := factor(ifelse(seg3_elev < sldelev, "lowlands", "uplands"))]
            d[seg3_elev_clas=="lowlands", seg3_pre_clas := factor(ifelse(seg3_pre < sldrain1, "dry", "moist"))]
            d[seg3_elev_clas=="uplands", seg3_pre_clas := factor(ifelse(seg3_pre < sldrain2, "dry", "moist"))]
            d[, seg3_aez_clas := factor(paste(seg3_pre_clas, seg3_elev_clas, sep=" "))]
            
            breaks <- c(0, sldtt20k1, sldtt20k2, 36)
            labels <- c("TH", "TM", "TL")
            d[, seg3_tt20k_clas := cut(seg2_tt20k, breaks, labels=labels)]
            
            breaks <- c(0, sldpopden1, sldpopden2, 17500)
            labels <- c("PL", "PM", "PH")
            d[, seg3_popden_clas := cut(seg3_popden, breaks, labels=labels)]
            d[, seg3 := factor(paste(seg3_aez_clas, seg3_tt20k_clas, seg3_popden_clas, sep="."))]            
            
            return(d)
          })
      
      output$table <- renderDataTable(options=list(
              pageLength=100,
              lengthChange="false"), {
            out()[, list(Woreda=ADM3_NAME), by=list(Segment=seg3alt)][order(Woreda)]          
          })
      
      output$stats <- renderTable(include.rownames=F, {
            # Merge all segmentation values back into `hh`
            d <- out()      
            setkey(hh, ADM3_CODE)
            setkey(d, ADM3_CODE)
            d <- d[hh][, .SD, .SDcols=c("EA", "region", "zone", "weight", "seg3",
                    "hhsize", "povclass2")]
            
            # Generate population and poverty counts
            tmp <- svydesign(~EA, strata=~region+zone, data=d, weights=~weight, nest=TRUE)
            out1 <- svyby(~hhsize, ~seg3, tmp, svytotal, na.rm=T, multicore=F)
            out2 <- svyby(~I(povclass2=="below $2/day"), ~seg3, tmp, svymean, na.rm=T, multicore=F)
            out1 <- cbind(out1, out2)
            out1 <- out1[, c(1,2,3,6,8)]
            setnames(out1, c("Segment", "Population", "Std. Err.", "$2/day Poverty HCR", "Std. Err."))
            
          })   
      
      output$sum1 <- renderTable({
            d <- out()
            data.frame(Woreda=summary(d$seg3_aez_clas))
          })
      
      output$sum2 <- renderTable({
            d <- out()
            data.frame(Woreda=summary(d$seg3_tt20k_clas))
          })
      
      output$sum3 <- renderTable({
            d <- out()
            data.frame(Woreda=summary(d$seg3_popden_clas))
          }) 
      
      output$sum4 <- renderTable({
            d <- out()
            data.frame(Woreda=summary(d$seg3))
          })  
      
      output$map <- renderUI({
            if ( is.null(input$sldelev) ) {
              tags$img(src="download.png")
              
            } else {
              renderPlot({
                    d <- out()
                    setkey(d, ADM3_CODE)
                    setkey(tmp, ADM3_CODE)
                    tmp$seg3 <- d[tmp][, seg3]
                    
                    ethMap + 
                        geom_polygon(aes(Lon, Lat, fill=seg3), tmp, color=0, alpha=.5) +  
                        geom_path(aes(x=Lon, y=Lat, group=ID), g.eth, color="white", size=0.01) +
                        scale_fill_manual("Segments\n", values=colorRampPalette(pal)(length(levels(tmp$seg3)))) +      
                        coord_equal() + theme_opts
                    
                  })
            }
          })
    })




