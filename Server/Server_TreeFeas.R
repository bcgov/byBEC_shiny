### Kiri Daust
### Feasibility tab

observeEvent(input$showinstr,{
  shinyalert(title = "Instructions",html = T,text = instr_feas)
})

testCanAdd <- function(){
  if(is.null(input$edaplot_selected)){
    return(TRUE)
  }
  return(FALSE)
}

observeEvent({c(input$map_polygon_click,input$edaplot_selected)},{
  toggle(id = "addspp", condition = testCanAdd())
})

observe({
  toggle(id = "submitdat", condition = input$sppPick != "None")
})

##this is the column name in the database
observeEvent(input$updatedfeas,{
  print("Updating feasibility")
  if(input$updatedfeas){
    globalFeas$dat <- "newfeas"
  }else{
    globalFeas$dat <- "feasible"
  }
  
}, priority = 20)

output$downloadFeas <- downloadHandler(
  filename = "FeasibilityUpdates.csv",
  content = function(file){
    dat <- dbGetQuery(con,"SELECT bgc,ss_nospace,sppsplit,feasible,newfeas,mod FROM feasorig")
    dat <- as.data.table(dat)
    setnames(dat, old = "sppsplit",new = "spp")
    fwrite(dat, file)
  }
)

##download buttons
output$downloadAudit <- downloadHandler(
  filename = "FeasibilityAudit.csv",
  content = function(file){
    dat <- dbGetQuery(con,"SELECT * FROM feas_audit")
    dat <- as.data.table(dat)
    fwrite(dat, file)
  }
)

output$downloadPest <- downloadHandler(
  filename = "PestUpdates.csv",
  content = function(file){
    dat <- dbGetQuery(con,"SELECT * FROM forhealth")
    dat <- as.data.table(dat)
    fwrite(dat, file)
  }
)

##base BGC map -- done
output$map <- renderLeaflet({
  leaflet() %>%
    setView(lng = -122.77222, lat = 51.2665, zoom = 6) %>%
    leaflet::addTiles(
      urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
      attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
      group = "Hillshade",
      options = leaflet::pathOptions(pane = "mapPane")) %>%
    addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "Positron",
                     options = leaflet::pathOptions(pane = "mapPane")) %>%
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite",
                              options = leaflet::pathOptions(pane = "mapPane")) %>%
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap",
                              options = leaflet::pathOptions(pane = "mapPane")) %>%
    leaflet::addTiles(
      urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mblbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
      attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
      group = "Cities",
      options = leaflet::pathOptions(pane = "overlayPane")) %>%
    addBGCTiles() %>%
    leaflet::addLayersControl(
      baseGroups = c("Hillshade","Positron","Satellite", "OpenStreetMap"),
      overlayGroups = c("BGCs","Feasibility","Districts","Cities"),
      position = "topright")
})

##add plot locations
observeEvent({c(input$showtrees,
                input$sppPick,
                input$trials,
                input$trialStart)},{
                  sppName <- substr(input$sppPick,1,2)
                  if(!is.null(input$showtrees)){
                    QRY <- paste0("select spp,plotnum,geometry from plotdata where spp = '",sppName,"' and region in ('",
                                  paste(input$showtrees,collapse = "','"),"')")
                    dat <- st_read(con,query = QRY)
                    if(nrow(dat) > 0){
                      dat <- dat["plotnum"]
                      colnames(dat)[1] <- "label"
                      leafletProxy("map") %>%
                        addGlPoints(data = dat,layerId = "tree_plot",popup = ~ label,
                                    fillColor = "#2DB000",fragmentShaderSource = "point")
                    }
                  }else{
                    leafletProxy("map") %>%
                      removeGlPoints("tree_plot")
                  }
                  if(!is.null(input$trials)){
                    dat2 <- st_read(con,query = paste0("select project_id, plotid, spp, seedlot,assessment, geometry from offsite where spp like '",
                                                       sppName,"%' and project_id in ('",paste(input$trials,collapse = "','"),
                                                       "') and planted > '", input$trialStart[1],"' and planted < '",input$trialStart[2],"'"))
                    if(nrow(dat2) == 0){
                      dat2 <- NULL
                      leafletProxy("map") %>%
                        removeGlPoints("tree_trial")
                    }else{
                      dat <- as.data.table(st_drop_geometry(dat2))
                      dat[assID, ID := i.ID, on = "assessment"]
                      dat <- dat[,.(ID = max(ID)), by = .(plotid,spp)]
                      dat[assCols, Col := i.Col, on = "ID"]
                      dat[,label := paste0("Name: ",plotid)]
                      dat <- dat[,.(plotid,label,Col)]
                      dat[,Col := as.character(Col)]
                      plotLocs <- merge(dat2, dat, by = "plotid")
                      
                      leafletProxy("map") %>%
                        addGlPoints(data = plotLocs,layerId = "tree_trial",popup = ~ label,
                                    fillColor = plotLocs$Col,fragmentShaderSource = "square")
                    }
                  }else{
                    leafletProxy("map") %>%
                      removeGlPoints("tree_trial")
                  }
                })

##Prepare BGC colour table for non-edatopic
prepDatSimple <- reactive({
  QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
                " from feasorig where spp = '",substr(input$sppPick,1,2),
                "' and ",globalFeas$dat," in (1,2,3,4,5)")
  d1 <- tryCatch({
    dbGetQuery(con, QRY)
  },
  error = function(e){
    invisible(lapply(dbListConnections(PostgreSQL()), dbDisconnect))
    con <<- dbConnect(drv, user = "postgres", password = "Kiriliny41", host = "68.183.199.104", 
                      port = 5432, dbname = "spp_feas")
    dat <- dbGetQuery(con, QRY)
    return(dat)
  })
  if(nrow(d1) == 0){
    shinyalert(title = "Oops!",text = "There are no data for that species",
               type = "error",showConfirmButton = T)
    QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
                  " from feasorig where spp = 'Sx' and ",globalFeas$dat," in (1,2,3,4,5)")
    d1 <- dbGetQuery(con, QRY)
  }
  feas <- as.data.table(d1)
  setnames(feas, old = globalFeas$dat, new = "feasible")
  feasMax <- feas[,.(SuitMax = min(feasible)), by = .(bgc,sppsplit)]
  if(input$showFreq){
    feasMax <- prepFreq()
    sppOpts <- unique(feasMax$sppsplit)
    feasMax[,sppsplit := as.numeric(as.factor(sppsplit))]
    feasMax[taxaFreqCols, Col := i.Col, on = c("sppsplit","Freq")]
    feasMax[Freq == "Added",Col := "#fbff00ff"]
    feasMax[Freq == "Removed",Col := "#8300ffff"]
    PALeg <- list(
      labels = c(sppOpts,"Added","Removed"),
      colours = c(taxaCols[1:length(sppOpts)],"#fbff00ff","#8300ffff"),
      title = "Presence/Absence"
    )
    globalLeg$Legend <- PALeg
  }else{
    if(length(unique(feasMax$sppsplit)) > 1){
      temp <- unique(feasMax$sppsplit)
      tempTab <- data.table(sppsplit = temp, Col = taxaCols[1:length(temp)])
      feasMax[tempTab,Col := i.Col, on = "sppsplit"]
      temp <- unique(feasMax[,.(sppsplit,Col)])
      
      PALeg <- list(
        labels = c(tempTab$sppsplit,"Added","Removed"),
        colours = c(tempTab$Col,"#fbff00ff","#8300ffff"),
        title = "Presence/Absence"
      )
      globalLeg$Legend <- PALeg
      
      feasMax[SuitMax == 4,Col := "#fbff00ff"]
      feasMax[SuitMax == 5,Col := "#8300ffff"]
    }else{
      feasMax[,Col := "#443e3dFF"]
      feasMax[SuitMax == 4,Col := "#fbff00ff"]
      feasMax[SuitMax == 5,Col := "#8300ffff"]
      PALeg <- list(
        labels = c(input$sppPick,"Added","Removed"),
        colours = c("#443e3dFF","#fbff00ff","#8300ffff"),
        title = "Presence/Absence"
      )
      globalLeg$Legend <- PALeg
    }
  }
  feasMax[,Lab := bgc]
  feasMax[,.(bgc,Col,Lab)]
})

##prep frequency colours
prepFreq <- reactive({
  QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
                " from feasorig where spp = '",substr(input$sppPick,1,2),
                "' and ",globalFeas$dat," in (1,2,3,4,5)")
  feas <- as.data.table(dbGetQuery(con, QRY))
  setnames(feas, old = globalFeas$dat, new = "feasible")
  minDist <- feas[,.SD[feasible == min(feasible, na.rm = T)],by = .(bgc,sppsplit)]
  tf2 <- minDist[feasible %in% c(4,5),]
  minDist <- minDist[feasible %in% c(1,2,3),]
  abUnits <- minDist[grep("[[:alpha:]] */[[:alpha:]]+$",ss_nospace),]
  noAb <- minDist[!grepl("[[:alpha:]] */[[:alpha:]]+$",ss_nospace),]
  abUnits <- eda[abUnits, on = "ss_nospace"] ##merge
  abUnits <- abUnits[,.(Temp = if(any(grepl("C4",edatopic))) paste0(ss_nospace,"_01") else ss_nospace, feasible = feasible[1]),
                     by = .(bgc,ss_nospace,sppsplit,spp)]
  abUnits[,ss_nospace := NULL]
  setnames(abUnits,old = "Temp",new = "ss_nospace")
  minDist <- rbind(noAb,abUnits)
  minDist[,ID := if(any(grepl("01", ss_nospace)) & feasible[1] == 1) T else F, by = .(bgc,sppsplit)]
  minDist[,Freq := NA_character_]
  minDist[(ID),Freq := "High"]
  
  minDist2 <- minDist[ID == F,]
  minDist2[,ID := if(any(grepl("01", ss_nospace))) T else F, by = .(bgc,sppsplit)]
  minDist2[(ID),Freq := "Moderate"]
  
  minDist3 <- minDist2[ID == F,]
  minEda <- eda[minDist3, on = "ss_nospace"]
  minEda <- minEda[,.(AvgEda = mean(smr)), by = .(bgc,sppsplit,ss_nospace,feasible)]
  minEda[,CentEda := abs(AvgEda - 3.5)]
  minEda <- minEda[,.SD[CentEda == min(CentEda, na.rm = T)], by = .(bgc,sppsplit)]
  lookupTab <- data.table(AvgEda = c(0,2,5,7),Freq = c("Low","Moderate","Low","Low"))
  temp <- lookupTab[minEda, on = "AvgEda", roll = T]
  
  t1 <- minDist[!is.na(Freq),.(Freq = Freq[1]), by = .(bgc,sppsplit)]
  t2 <- minDist2[!is.na(Freq),.(Freq = Freq[1]), by = .(bgc,sppsplit)]
  t3 <- temp[,.(Freq = Freq[1]), by = .(bgc,sppsplit)]
  allFreq <- rbind(t1,t2,t3)
  
  if(nrow(tf2) > 0){
    tf2[feasible == 4,Freq := "Added"]
    tf2[feasible == 5,Freq := "Removed"]
    tf2 <- tf2[,.(Freq = Freq[1]), by = .(bgc,sppsplit)]
    allFreq <- rbind(allFreq, tf2)
  }
  allFreq
})

##Prepare BGC colours for edatopic option
prepEdaDat <- reactive({
  QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
                " from feasorig where spp = '",substr(input$sppPick,1,2),
                "' and ",globalFeas$dat," in (1,2,3,4)")
  feas <- as.data.table(dbGetQuery(con, QRY))
  setnames(feas, old = globalFeas$dat, new = "feasible")        
  globalLeg$Legend <- edaLeg
  id <- as.numeric(input$edaplot_selected)
  idSub <- idDat[ID == id,.(ID,edatopic)]
  edaSub <- eda[idSub, on = "edatopic"]
  feasSub <- feas[ss_nospace %chin% edaSub$ss_nospace,]
  if(nrow(feasSub) == 0){
    return(NULL)
  }
  feasSub[,Lab := paste0(ss_nospace,": ", feasible)]
  feasSum <- feasSub[,.(FeasVal = mean(feasible), Lab = paste(Lab, collapse = "<br>")), by = bgc]
  #tempCol <- grRamp(rescale(feasSum$FeasVal,to = c(0,1)))
  feasSum[,Col := colour_values(rescale(feasSum$FeasVal,to = c(0,1)))]
  feasSum[,.(bgc,Col,Lab)]
})

##render feasibiliy map
observeEvent({c(
  input$sppPick,
  input$edaplot_selected,
  input$updatedfeas,
  input$showFreq)
},{
  if(input$sppPick == "None"){
    session$sendCustomMessage("clearLayer","xxx")
  }else{
    if(is.null(input$edaplot_selected)){
      dat <- prepDatSimple()
    }else{
      dat <- prepEdaDat()
    }
    if(is.null(dat)){
      dat <- NULL
      session$sendCustomMessage("clearLayer", "Delete")
    }else{
      print("Rendering map")
      dat <- dat[subzTransparent, on = "bgc"]
      dat[is.na(Col),Col := Transparent]
      dat[is.na(Lab),Lab := bgc]
      leafletProxy("map") %>%
        invokeMethod(data = dat, method = "addGridTiles", ~bgc, ~Col, ~Lab) %>%
        addLegend(position = "bottomright",
                  labels = globalLeg$Legend$labels,
                  colors = globalLeg$Legend$colours,
                  title = globalLeg$Legend$title,
                  layerId = "bec_feas") 
    }
  }
}, priority = 15)

output$tableBGC <- renderUI({
  unit <- globalSelBEC()
  if(!is.null(unit)){
    tagList(
      h3(paste0("Feasibility for ",unit)),
      br()
    )
  }
})

##prepare suitability table when polygon clicked
prepTable <- reactive({
  unit <- globalSelBEC()
  print(unit)
  idx_row <- NULL
  idx_col <- NULL
  QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
                " from feasorig where bgc = '",unit,"' and ",globalFeas$dat," in (1,2,3,4)")
  feas <- as.data.table(dbGetQuery(con, QRY))
  if(nrow(feas) == 0){
    shinyalert("Oopsies!","There are no species in that subzone :(")
    return(list(dat = feas, rIdx = NULL, cIdx = NULL, sppCol = NULL))
  }
  setnames(feas, old = globalFeas$dat, new = "feasible")   
  if(is.null(input$edaplot_selected)){
    feasSub <- feas[sppsplit != "X",]
    tabOut <- data.table::dcast(feasSub, ss_nospace ~ sppsplit,fun.aggregate = mean, value.var = "feasible")
    tabOut[,lapply(.SD,as.integer),.SDcols = -"ss_nospace"]
  }else{
    id <- as.numeric(input$edaplot_selected)
    idSub <- idDat[ID == id,.(ID,edatopic)]
    edaSub <- eda[idSub, on = "edatopic"]
    edaSub <- edaSub[bgc == unit,]
    dat <- feas[ss_nospace %in% edaSub$ss_nospace & feasible %in% c(1,2,3,4),]
    tabOut <- data.table::dcast(dat, ss_nospace ~ sppsplit, value.var = "feasible", fun.aggregate = mean)
    tabOut[,lapply(.SD,as.integer),.SDcols = -"ss_nospace"]
    if(input$updatedfeas){
      QRY <- paste0("select ss_nospace,sppsplit,feasible from feasorig where bgc = '",
                    unit,"' and feasible in (1,2,3,4)")
      feasOrig <- as.data.table(dbGetQuery(con, QRY))
      dat2 <- feasOrig[ss_nospace %in% edaSub$ss_nospace,]
      setnames(dat2, old = "feasible", new = "FeasOld")
      comp <- merge(dat,dat2,on = c("ss_nospace","sppsplit"),all = T)
      comp[,Same := (feasible == FeasOld) & !is.na(feasible) & !is.na(FeasOld)]
      tabOrig <- data.table::dcast(comp, ss_nospace ~ sppsplit, value.var = "Same",fun.aggregate = function(x){x[1]})
      idx <- which(tabOrig == F, arr.ind = T)
      idx_row <- unname(idx[,1] - 1)
      idx_col <- unname(idx[,2] - 1)
    }
    
  }
  spp <- colnames(tabOut)
  spp[spp %in% c("Se","Sw","Sxw")] <- "Sx"
  spp[spp %in% c("Sxl","Sxs","Ss")] <- "Ss"        
  spp <- substr(spp, 1,2)
  sppCurr <- substr(input$sppPick,1,2)
  if(sppCurr %in% spp){
    sppIdx <- which(spp == sppCurr) - 1
  }else{
    sppIdx <- NULL
  }
  list(dat = tabOut, rIdx = idx_row, cIdx = idx_col, sppCol = sppIdx)
})

observeEvent(input$bgc_click,{
  #updateSelectInput(session, "selectBGC",selected = "None")
  globalSelBEC(input$bgc_click)
})

##render suitability table, colour updated cells
observeEvent({c(input$bgc_click, 
                input$edaplot_selected,
                input$sppPick,
                input$updatedfeas,
                input$selectBGC)},{
                  if(!is.null(globalSelBEC())){
                    output$hot <- renderRHandsontable({
                      temp <- prepTable()
                      dat <- temp$dat
                      #browser()
                      rhandsontable(data = dat,col_highlight = temp$cIdx,
                                    row_highlight = temp$rIdx, spp_highlight = temp$sppCol) %>%
                        hot_cols(format = "0", renderer = "
                function(instance, td, row, col, prop, value, cellProperties) {
                Handsontable.renderers.NumericRenderer.apply(this, arguments);
                if (instance.params) {
                    hcols = instance.params.col_highlight
                    hcols = hcols instanceof Array ? hcols : [hcols]
                    hrows = instance.params.row_highlight
                    hrows = hrows instanceof Array ? hrows : [hrows]
                    hspp = instance.params.spp_highlight
                    hspp = hspp instanceof Array ? hspp : [hspp]
                }
                
                var i;
                for(i = 0; i < 100; i++){
                    if (instance.params && (col === hcols[i] && row === hrows[i])) {
                      td.style.background = 'yellow';
                    }
                    if(instance.params && col === hspp[i]){
                        td.style.background = 'lightgreen';
                    }
                }
                    
            }
                             ")
                    })
                  }
                })

##ask for initials and call sendToDb
observeEvent(input$submitdat,{
  shinyalert("Enter your initials:", type = "input",imageUrl = "images/puppy1.jpg",imageHeight = "100px", inputId = "initials", callbackR = sendToDb)
})

##compile and send updates to database
sendToDb <- function(nme){
  dat <- as.data.table(hot_to_r(input$hot))
  unit <- globalSelBEC()
  QRY <- paste0("select fid,bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
                " from feasorig where bgc = '",unit,"' and ",globalFeas$dat," in (1,2,3,4)")
  datOrig <- as.data.table(dbGetQuery(con, QRY))
  setnames(datOrig,old = globalFeas$dat, new = "feasible")
  dat <- melt(dat, id.vars = "ss_nospace", value.name = "newfeas", variable.name = "sppsplit")
  dat2 <- datOrig[dat, on = c("ss_nospace","sppsplit")]
  dat2[is.na(feasible),feasible := -1]
  dat2 <- dat2[newfeas != feasible,]
  dat2[,mod := nme]
  datAudit <- dat2[,.(fid,ss_nospace,sppsplit,newfeas,mod)]
  datAudit[,date := Sys.Date()]
  dbWriteTable(con,"feas_audit", datAudit, append = T, row.names = F)
  datNew <- dat2[is.na(bgc),]
  datOld <- dat2[!is.na(bgc),]
  if(nrow(datNew) > 0){
    temp <- data.table(bgc = gsub("/.*","",datNew$ss_nospace),ss_nospace = datNew$ss_nospace,
                       sppsplit = datNew$sppsplit,feasible = NA, 
                       spp = substr(datNew$sppsplit,1,2),newfeas = datNew$newfeas,mod = nme)
    dbWriteTable(con,"feasorig",temp, append = T,row.names = F)
  }
  if(nrow(datOld) > 0){
    dbWriteTable(con, "temp_update", datOld, overwrite = T)
    dbExecute(con,"UPDATE feasorig 
                  SET newfeas = temp_update.newfeas,
                  mod = temp_update.mod
                  FROM temp_update
                  WHERE feasorig.ss_nospace = temp_update.ss_nospace
                  AND feasorig.sppsplit = temp_update.sppsplit")
    dbExecute(con,"UPDATE feasorig
                  SET newfeas = 5
                  WHERE newfeas IS NULL
                  AND feasible IS NOT NULL")
  }
  
  shinyalert("Thank you!","Your updates have been recorded", type = "info",
             imageUrl = "images/puppy1.jpg",imageHeight = "100px", inputId = "dbmessage")
  
}
##table to add species
output$hot_add <- renderRHandsontable({
  if(!is.null(globalSelBEC())){
    unit <- globalSelBEC()
    edaSub <- unique(eda[bgc == unit,.(bgc,ss_nospace)])
    temp <- data.table(ss_nospace = edaSub$ss_nospace, newfeas = NA_integer_)
    rhandsontable(data = temp)
  }
})

##modal to add species
observeEvent(input$addspp,{
  shinyalert(html = T,
             text = tagList(
               h4("Select a species, add feasibility, then click submit"),
               pickerInput("sppPickAdd",
                           label = NULL,
                           choices = sppSplitList), 
               fluidRow(column(8,rHandsontableOutput("hot_add")),
                        column(4,textInput("addsppMod",label = "Enter your initials:"))),
               
             ),
             callbackR = addSppToDb,
             showCancelButton = T,
             showConfirmButton = T)
})

addSppToDb <- function(x){
  if(x){
    dat <- hot_to_r(input$hot_add)
    dat <- as.data.table(dat)
    dat2 <- data.table(bgc = gsub("/[[:digit:]]*","", dat$ss_nospace),
                       ss_nospace = dat$ss_nospace,
                       sppsplit = substr(input$sppPickAdd,1,2),
                       feasible = NA,spp = substr(input$sppPickAdd,1,2), newfeas = dat$newfeas, mod = input$addsppMod)
    
    dat2 <- dat2[!is.na(newfeas),]
    dbWriteTable(con, name = "feasorig", value = dat2, append = T,row.names = F)
    shinyalert("Thank you!","Your updates have been recorded", type = "info",
               imageUrl = "images/puppy1.jpg",imageHeight = "100px",inputId = "dbmessage")
  }
}

##render interactive edatopic grid
output$edaplot <- renderGirafe({
  gg <- ggplot()+
    geom_blank()+
    scale_y_discrete(limits = c("7","6","5","4","3","2","1","0"))+
    scale_x_discrete(limits = c("A","B","C","D","E"))+
    geom_rect_interactive(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                                            data_id = ids), 
                          fill = "grey", col = "purple")+
    geom_hline(aes(yintercept = grd1y), linetype = "dashed")+
    geom_vline(aes(xintercept = grd1x), linetype = "dashed")+
    theme_bw(base_size = 16)+
    theme(panel.grid.major = element_blank())+
    labs(x = "SNR", y = "SMR")+
    coord_fixed()
  
  girafe(ggobj = gg,
         options = list(opts_selection(type = "single")),
         width_svg = 4, height_svg = 7)
})