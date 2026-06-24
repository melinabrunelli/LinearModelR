library (shiny)
library (limma)
library (cmapR)
#source(cmapR-io.R)

ui <- shinyUI (fluidPage (
  titlePanel (title = "Linear Models for data analysis"), 
  sidebarLayout (
    
    sidebarPanel (fileInput ('file1', 
                             HTML ('DATA FILE<br>
                                <h6>Normalized multi-median log ratios, with ID in column 1 (.csv or .gct)</h6>'),
                             accept=c('text/csv', 
                                      'text/comma-separated-values, text/plain', 
                                      '.csv',
                                      '.gct',
                                      '.gctx')),
                  numericInput ("contrasts", 
                                HTML ("NUMBER OF CONTRASTS<br>
                                   <h6>Each contrast encodes a comparison of interest.</h6>"),
                                value=0, min = 0, step = 1),
                  downloadButton ('downloadData', HTML ('RUN<br>and download results'))),
    
    mainPanel (fluidRow("SAMPLES"),
               fluidRow(uiOutput("samples")),
               fluidRow(uiOutput("sample_choices")),
               HTML ("<hr>"),
               fluidRow ("MAIN COMPARISONS"), 
               fluidRow(uiOutput("gg")),
               fluidRow(uiOutput("csv_gg")),
               HTML ("<hr>"),
               fluidRow("SECONDARY EFFECTS (optional)"),
               fluidRow(uiOutput("treats")),
               fluidRow(uiOutput("csv_treats")),
               HTML ("<hr>"),
               fluidRow ("CONTRASTS (optional)"),
               fluidRow (uiOutput("cont")),
               HTML ("<hr>"),
               fluidRow("EXPERIMENTAL DESIGN"),
               fluidRow (uiOutput("plo")),
               HTML ("<hr>")
    )
  )
)) # End of shinyUI



server <- function (input, output, session) {
  
  file_type <- reactive({
    # check what the file extension is:
    validate(need(input$file1, ""))
    validate(need(tolower(tools::file_ext((input$file1)$datapath)) %in% c("csv","gct"), "Not a supported data file! csv and gct only."))
    
    inFile <- input$file1
    if (tolower(tools::file_ext(inFile$datapath)) == "csv"){ 
      file_type <- "csv"
    } else if (tolower(tools::file_ext((input$file1)$datapath)) == "gct") {
      file_type <- "gct"
    } 
  })
  
  data_file <- reactive({
    
    #validate(need(input$file1, "No data file uploaded."))
    inFile <- input$file1
    
    if (file_type() == "csv"){ # If .csv file handle this way
      
      csv_data_file <- read.csv(inFile$datapath, header = T, row.names = 1) # get .csv data
      return(csv_data_file)
      
    } else if (file_type() == "gct") {
      gct_data_file <-  parse.gctx(inFile$datapath) # get .gct data object
      return(gct_data_file)
    }
  })
  
  
  # To get columns add another reactive value to select columns (and/or button to select all). Replace uses of data_file() with this reactive
  ###
  ### Samples:
  ###
  
  output$samples <- renderUI ({
    if (file_type() == "csv" | file_type() == "gct") {
      checkboxInput("all_cols", "Use all columns as samples.", TRUE)
    }
  })
  
  all_cols <- reactive({
    validate(need(!is.null(input$all_cols), ""))
    return(input$all_cols)
  })
  
  output$sample_choices <- renderUI ({
    validate(need(file_type(), ""))
    validate(need(all_cols() == FALSE, ""))
    
    if (file_type() == "csv"){
      column (width=5, checkboxGroupInput (inputId = "custom_samples",  
                                           label = "Select Samples",
                                           choices= colnames(data_file()),
                                           selected = colnames(data_file())))
    } else if (file_type() == "gct"){
      column (width=5, checkboxGroupInput (inputId = "custom_samples",  
                                           label = "Select Samples",
                                           choices= colnames(data_file()@mat),
                                           selected = colnames(data_file()@mat)))
    }
  })
  
  custom_samples <- reactive({
    validate(need(input$custom_samples, "Please provide more than 1 sample."))
    validate(need(length(input$custom_samples)>1, "Please provide more than 1 sample."))
    input$custom_samples
  })
  
  data_file_updated <- reactive({
    if (all_cols()){
      return(data_file())
    } else {
      if (file_type() == "csv") {
        validate(need(all_cols() == FALSE, ""))
        data_file_updated <- data_file()[,(custom_samples())]
        return(data_file_updated)
      } else if (file_type() == "gct") {
        validate(need(data_file()@mat[,(custom_samples())], ""))
        data_file_updated <- data_file()
        data_file_updated@mat <- data_file_updated@mat[,(custom_samples())]
        data_file_updated@cdesc <- data_file_updated@cdesc[(custom_samples()),]
        return(data_file_updated)
        
      }
    }
  })
  
  ###
  ### Groups:
  ###
  
  output$gg <- renderUI ({
    
    if (file_type() == "gct"){
      group_treat_choices = colnames(data_file_updated()@cdesc)
      group_treat_choices = group_treat_choices[group_treat_choices != "id"]
      selectizeInput('group', label = "Select main comparisons identifier", choices = group_treat_choices, multiple = F)
    } else if (file_type() == "csv"){
      textInput ("csv_groupNames", 
                 HTML ("<h6>Enter main comparisons separated by commas, with reference group listed last. Eg: MUT, WT</h6>"))
    }
  })
  
  csv_groupNames <- reactive({
    validate(need(!is.null(input$csv_groupNames), ""))
    return(input$csv_groupNames)
  })
  
  output$csv_gg <- renderUI ({
    
    if (file_type() == "csv"){
      validate(need(csv_groupNames(), "Enter at least two main comparisons seperated by commas."))
      csv_groups = strsplit ( gsub ('[ \t]', '', csv_groupNames()), split=",")[[1]]
      if (length(csv_groups) > 1) {
        lapply (seq(length(csv_groups)), 
                function (j) {
                  column (width=5,
                          checkboxGroupInput (inputId = paste0("group_",j),  
                                              label = csv_groups[j],
                                              choices=colnames(data_file_updated())))
                })
      }
    } else return()
  })
  
  group_choice <- reactive({
    if (file_type() == "gct"){
      return(input$group)
    } 
  })
  
  ###
  ###Treatments:
  ###
  
  output$treats <- renderUI({
    
    if (file_type() == "gct"){
      treat_choices = colnames(data_file_updated()@cdesc)
      treat_choices = treat_choices[treat_choices != "id"]
      selectizeInput('tt', label = "Select secondary effects identifier (optional)", choices = c(treat_choices,"none"), multiple = F,
                     options = list(
                       placeholder = 'Please select an option below',
                       onInitialize = I('function() { this.setValue("none"); }')))
    } else if (file_type() == "csv"){
      textInput ("csv_treatments", 
                 HTML ("<h6>Enter secondary effects separated by commas, with reference treatment listed last. Eg: TRT, CTRL</h6>"))
    }
  })
  
  csv_treatments <- reactive({
    validate(need(!is.null(input$csv_treatments), ""))
    return(input$csv_treatments)
  })
  
  treatment_choice <- reactive({
    validate(need(input$tt != "", ""))
    if (file_type() == "gct") {
      return(input$tt)
    } 
  })
  
  output$csv_treats <- renderUI({
    if (file_type() == "csv"){
      validate(need(csv_treatments(), "Enter at least two secondary effects seperated by commas."))
      csv_treatments = strsplit ( gsub ('[ \t]', '', csv_treatments()), split=",")[[1]]
      if (length(csv_treatments) > 1) {
        lapply (seq(length(csv_treatments)), 
                function (j) {
                  column (width=5,
                          checkboxGroupInput (inputId = paste0("treatment_",j),  
                                              label = csv_treatments[j],
                                              choices=colnames(data_file_updated())))
                }
        )
      }
    }
  })
  
  
  ###
  ### Contrasts:
  ###
  in_contrasts <- reactive({
    validate(need(input$contrasts != "", "Please enter the number of contrasts you wish to calculate."))
    input$contrasts
    
  })
  
  output$cont <- renderUI({
    # setup the contrasts:
    if (in_contrasts() > 0) {
      ### CSV:
      if (file_type() == "csv") { 
        csv_treatments = strsplit ( gsub ('[ \t]', '', csv_treatments()), split=",")[[1]]
        validate(need(csv_groupNames(), "Enter at least two group names!"))
        csv_groups = strsplit ( gsub ('[ \t]', '', csv_groupNames()), split=",")[[1]]
        validate(need((length(csv_groups)>1), "Enter at least two group names!"))
        if (length(csv_treatments)>1) {
          items = apply (outer (csv_groups, csv_treatments, FUN=function (x,y) paste (x,y,sep='.')),
                         1, function (z) combn (z, 2, FUN=function (s) paste (s, collapse='-')))
        }else if (length(csv_groups)>1) {
          items = rbind (csv_groups)
        }else return("Optional. If not specified, each input GROUP/TREATMENT combination will be tested.") # Return nothing for contrasts if less than 2 groups given
        ### GCT:  
      } else if (file_type() == "gct" ) { # gct with treatments
        groups = unique(data_file_updated()@cdesc[,group_choice()])
        validate(need(length(groups)>1, "Need more than 1 main comparison represented by the samples."))
        if (treatment_choice() != "none") {
          treatments = unique(data_file_updated()@cdesc[,treatment_choice()])
          items = apply (outer (groups, treatments, FUN=function (x,y) paste (x,y,sep='.')),
                         1, function (z) combn (z, 2, FUN=function (s) paste (s, collapse='-')))
        } else { # gct no treatments
          items = rbind (groups)
        }
      }  #end gct if statement  
      
      ### COMMON CODE:
      # Now we have defined the needed values for the given files/treatments:
      options = apply (rbind(items), 1, 
                       function (z) combn (z, 2, function (x) paste0 ('(',x[1],')-(', x[2],')')))
      contrasts.n = min (in_contrasts(), length(options))
      lapply (seq(contrasts.n), 
              function (j) {
                column (width=12,
                        selectizeInput (inputId = paste0("contrast_",j),  
                                        label = paste("Contrast ",j),
                                        choices=as.list(as.vector (options))))}
      ) # end seq
    } else {"Optional. If not specified, each input MAIN COMPARISON/SECONDARY EFFECTS combination will be tested."}
  }) #end renderUI
  
  ###
  ### LINEAR MODEL:
  ###
  
  
  getDataAndFitModel <- reactive ({
    treatments = "" # set to 0, will be reassigned if it was selected
    ### CSV prep
    if (file_type() == "csv") {
      
      if (csv_treatments() != ""){
        treatments = strsplit ( gsub ('[ \t]', '', csv_treatments()), split=",")[[1]]
      } 
      csv_groups = strsplit ( gsub ('[ \t]', '', csv_groupNames()), split=",")[[1]]
      
      d = data = data_file_updated()
      samples = data.frame (group=character(ncol(data)), treatment=character(ncol(data)),
                            stringsAsFactors=FALSE) # do an if statement for treatment?
      row.names(samples) = colnames(data)
      
      for (j in 1:length(csv_groups)) {
        groupMembers = input[[paste0("group_",j)]]
        samples$group[row.names(samples) %in% (groupMembers)] = csv_groups[j] 
      }
      validate(need(!(any(samples$group=="")),"Dataset does not match experiment design: some samples missing main comparisons values!"))
      
      if (length(treatments) >1 )  {
        for (k in 1:length(treatments)) {
          trtMembers = input[[paste0("treatment_",k)]]
          samples$treatment[row.names(samples) %in% (trtMembers)] = treatments[k] 
        }
        validate(need(!(any(samples$treatment=="")), "Dataset does not match experiment design: some samples missing secondary effects values!"))
      }
    }
    ### GCT prep
    if (file_type() == "gct") {
      d = data = as.data.frame(data_file_updated()@mat)
      groups = unique(data_file_updated()@cdesc[,group_choice()])
      
      if (treatment_choice() != "none") {
        treatments = unique(data_file_updated()@cdesc[,treatment_choice()])
        samples = as.data.frame(data_file_updated()@cdesc[c(group_choice(),treatment_choice())])
      } else samples = as.data.frame(data_file_updated()@cdesc[group_choice()])
    }
    
    sampleGroups = apply (samples, 1, 
                          function(x) paste(x, collapse=ifelse(length(treatments)>1,".","")))
    ### Build model:
    
    colnames (data) = sampleGroups
    sampleGroups = factor (sampleGroups)
    design = model.matrix (~ 0 + sampleGroups)
    colnames (design) = levels (sampleGroups)
    fit = lmFit (data,design)
    
    validate(need(!is.null(in_contrasts()),""))
    if (in_contrasts() > 0) {
      contrasts.n = length (grep ("contrast_[0-9]*$", names (input)))  # count actual contasts
      contrasts = character (contrasts.n)
      for (j in 1:contrasts.n) {
        contrasts[j] = as.character (input[[paste0("contrast_",j)]])
      }
    } else contrasts = unique (sampleGroups)
    
    contrast.matrix = makeContrasts (contrasts=unlist(contrasts),levels=design)
    fitc = contrasts.fit (fit, contrast.matrix)
    fitc = eBayes (fitc, robust=TRUE)
    
    out.cols <- c ('logFC', 'P.Value', 'adj.P.Val')
    out <- NULL
    for (i in 1:ncol(contrast.matrix)) {
      out.i <- topTable (fitc, coef=i, number=nrow(data), adjust.method="BH", p.value=1, sort.by="none")
      out.i.cols <- out.i [, out.cols]
      colnames (out.i.cols) <- paste (colnames (contrast.matrix)[i], out.cols, sep='.')
      if (is.null (out)) out <- out.i.cols
      else out <- cbind (out, out.i.cols)
    }
    
    return (cbind (d, out))
  })
  
  output$plo <- renderUI({
    if (file_type() == "gct" | file_type() == "csv"){
      validate(need(getDataAndFitModel(), "Unable to run linear model with current experimental design."))
      "Linear model experimental design is producible! Hit the 'RUN' button to download and view results."
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0 (gsub(".csv","",input$file1,fixed=TRUE),"_LinearModelFit_results.csv")
    },
    content = function(file) {
      write.csv (getDataAndFitModel(), file)
    })
}

shinyApp(ui,server)
