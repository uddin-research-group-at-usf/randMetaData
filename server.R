#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(gtools)


## Defining the size of file to be accepted. Here it can accept any size.
options(shiny.maxRequestSize= -1) 


# this function will define the level subset used to randomize data
level_subsets <- function(level_table) {
    per.level.subsets <- list()
    for (i in 1:ncol(level_table)) {
        this.name <- names(level_table)[i]
        current.level.subsets <- list()
        for (lev in levels(as.factor(unlist(level_table[,i,with=FALSE])))) {
            current.level.subsets[[paste0(this.name,"@",lev)]] <-
                unlist(level_table[,i,with=FALSE]) == lev
        }
        if (length(per.level.subsets) == 0) {
            per.level.subsets <-
                current.level.subsets
        } else {
            new.level.subsets <- list()
            for (j in names(per.level.subsets)) {
                for (k in names(current.level.subsets)) {
                    if (any(current.level.subsets[[k]] &
                            per.level.subsets[[j]])) {
                        new.level.subsets[[paste0(j,":",k)]] <-
                            current.level.subsets[[k]] &
                            per.level.subsets[[j]]
                    }
                }
            }
            per.level.subsets <- new.level.subsets
        }
    }
    per.level.subsets
}


#' Main program starts from here
#' Define server logic required
shinyServer(function(input, output) {
  
  observe(print(paste("Randomize columns", input$RandomizeCols)))
  
  #' input data file which need to be randomized
  inputfile <- reactive({
    
    if(is.null(input$InputFile)){
      return(NULL)
    }
    
    if(length(input$InputFile[['datapath']]) > 1) {
      # Show a modal when the button is pressed
      shinyalert("Oops!", "Only one file needed", type = "error")
    }else{
      path = input$InputFile[['datapath']]
      data <- read.csv(path)
    }
  })
  
  
  
  #' To display the input file in tabpanel
  output$datafile <- renderDataTable({
    if(input$items == 'analysis'){
      cat("Display of meta-analysis files done...\n")
      infile <- DT::datatable(inputfile(), options = list(scrollX= '300px'), 
                              selection = list(target = 'column'))
    }
  })
  
  
  observe(print(input$datafile_columns_selected))
  
  
  #' Display plot options
  output$Plots <- renderUI({
    if(!is.null(randomized_data()) & input$tabset1 == "Plot"){
      if(input$items == "analysis"){
        box(
          title = "Plots", width = 3, solidHeader = TRUE, status = "primary",
          radioButtons("PlotSettings", label = "", 
                       choices = list("Sunflower Plot" = "sunflower",
                                      "Violin Plot" = "violin",
                                      "Density Plot" = "density", 
                                      "Box Plot" = "box",
                                      "Dot Plot" = "dotplot",
                                      "Histogram" = "histogram"), selected = "sunflower")
        )
      }
    }
  })
  
  
  # Plot labels
  output$PlotLabels <- renderUI({
    
    if(!is.null(randomized_data()) & input$tabset1 == "Plot"){
      box(
        title = "Plot Labels", width = 3, solidHeader = TRUE, status = "primary",
        textInput("maintitle", "Main Title", value = "", placeholder = "Enter label for main title"),
        textInput("xaxis", "Xlab", value = "", placeholder = "Enter label for x-axis"),
        textInput("yaxis", "Ylab", value = "", placeholder = "Enter label for y-axix")
      )
    }
  })
  
  
  
  #' Display the plate design so that the user can select the location for controls
  #' All plates display together ...............
  output$InsertCont <- renderDataTable({
    if(is.null(input$InputFile)){
      return(NULL)
    }
    if(input$Controls){
      df <- NULL
      plates <- ceiling(nrow(inputfile()) / 96)
      remaining <- ceiling(nrow(inputfile()) %% 96 / 8) # get the remaining chips and divide by 8
      print(remaining)
      print(nrow(inputfile()))
      print(plates)
      cont_mat <- list()
      chips <- 12
      for (i in 1:plates) {
        if( i == 1) first <- 1
        print(first)
        print(i)
        if(i == plates){
          print("here==============")
          mat <- matrix(nrow = 8, ncol = remaining)
          colnames(mat) <- paste0("P", i, "_Chip_", first:((chips*i)-(12-remaining))) 
        } else{
          print("here ..............")
          mat <- matrix(nrow = 8, ncol = 12)
          colnames(mat) <- paste0("P", i, "_Chip_", first:(chips*i))
        }
        mat[] <- 0
        row.names(mat) <- paste0("R", 1:8)
        df <- cbind(df, mat)
        first <- (chips*i) + 1
        print(first)
      }
      dt <- DT::datatable(df, options = list(scrollX= '300px'), selection = list(target = 'cell'))
    }
  })
  
  
  output$ControlLoc <- renderPrint({
    # print("here //////////////")
    # print(input$InsertCont_cells_selected)
    if(!is.null(input$InsertCont_cells_selected) & input$Controls){
      cat("\n\bAdd controls by selecting locations")
      cat('\n\nSelected locations:\n\n')
      input$InsertCont_cells_selected
    }
  })
  
  
  final_contorls <- eventReactive(input$Submit,{
    if(input$Controls){
      message("testing here .........................")
      # print(input$InsertCont_cells_selected)
      # print(input$InsertCont_cells_selected[, 1])
      # print(input$InsertCont_cells_selected[, 2])
      if(nrow(input$InsertCont_cells_selected) > 1){
        rows <- paste0("R", input$InsertCont_cells_selected[, 1])
        chips <- paste0("chip_", input$InsertCont_cells_selected[, 2])
        
        #' Create plate information - each plate has 12 chips
        plates <- paste0("plate_", ceiling(input$InsertCont_cells_selected[, 2]/12))
        list(rows, chips, plates)
      }
    }
  })
  
  
  #' Display control design
  #' 
  output$ControlLOC <- renderUI({
    if(input$Controls & !is.null(inputfile())){
      box(
        verbatimTextOutput('ControlLoc'),
        tabPanel("Control Design", DT::dataTableOutput('InsertCont'))
      )
    }
    
  })
  
  
  #' #' Options to display final plating data, one plate at a time
  output$Display_final_design <- renderUI({
    if(!is.null(randomized_data()) & input$tabset1 == "Final Design"){
      plates <- ceiling(nrow(inputfile()) / 96)
      plates <- paste0("Plate_", 1:plates)
      if(input$items == "analysis"){
        box(
          title = "Display Final Data", width = 3, solidHeader = TRUE, status = 'primary',
          selectInput('Finaldesign', "Select plate to display data",
                      c("Please select plate" = '', plates), 
                      selected = plates[1], multiple = FALSE)
        )
      }
    }
    
  })
  
  
  # validate selecting columns to balance randomization
  observeEvent(input$Submit,{
    if(!is.null(inputfile())){
      if(is.null(input$datafile_columns_selected)){
        shinyalert("Oops!", "Please select columns for randomization by clicking on the desired columns",
                   type = "error")
      }
    }
  })
  
  
  #' Randomize data
  #' 
  randomized_data <- eventReactive(input$Submit, {
    withProgress(message = 'Patience, Processing Data...',{
      # print(control_info())
      if(is.null(inputfile()))
        return(NULL)
      
      if( (!is.null(input$datafile_columns_selected)) | 
          (input$Controls && !is.null(input$datafile_columns_selected)) ){ # if insert controls and control info is true
        
        # Convert to data.table
        input_samples <- as.data.table(inputfile())
        
        ### assign samples to lanes, then shuffle within chip, then shuffle the
        ### lanes to obtain an ideal order
        samples.per.chip <- 8
        num.samples <- nrow(input_samples)
        num.chips <- ceiling(num.samples / samples.per.chip)
        
        ### this seed was generated with echo -n $RANDOM; it is set here for
        ### reproducibility
        set.seed(13127)
        
        ## Create list 
        chip.assignments <- list()
        for (chip in 1:num.chips) {
          chip.assignments[[chip]] <- as.character()
        }
        
        # function to get level subsets
        chip.counter <- 1;
        chip.shuffle <- sample(length(chip.assignments))
        
        # Convert ParticipantID id to factor
        input_samples[,`ParticipantID`:=as.factor(`ParticipantID`)]
        
        ### we want to balance by PTSD status, Gender
        bal_by_cols <- input$datafile_columns_selected
        message("-------------------")
        print(bal_by_cols)
        print(head(input_samples))
        col_names <- colnames(input_samples)[bal_by_cols]
        print(col_names)
        message("=======================")
        exp.level.subsets <- level_subsets(input_samples[, col_names, with = FALSE])
        for (level.subset in names(exp.level.subsets)) {
          for (sample.id in input_samples[exp.level.subsets[[level.subset]],
                                          sample(`SampleID`)]) {
            ## assign this sample to a chip
            this.chip <- chip.shuffle[chip.counter]
            this.n <- length(chip.assignments[[this.chip]]) + 1
            chip.assignments[[this.chip]][this.n] <- sample.id
            chip.counter <- chip.counter + 1
            if (chip.counter > num.chips) {
              chip.counter <- 1;
            }
          }
        }
        
        print("testing here --------------------")
        ## now shuffle within the chip
        for (chip in 1:num.chips) {
          chip.assignments[[chip]] <- sample(chip.assignments[[chip]])
        }
        
        ## now put the chip position, chip identification, and plate into the
        ## sample information
        for (this.chip in 1:length(chip.assignments)) {
          ## there are 12 chips per plate
          this.plate <- floor((this.chip-1) / 12) + 1
          for (i in 1:length(chip.assignments[[this.chip]])) {
            sample <- chip.assignments[[this.chip]][i]
            input_samples[SampleID==sample,
                          chip := paste0("chip_",this.chip)]
            input_samples[SampleID==sample,
                          plate := paste0("plate_",this.plate)]
            input_samples[SampleID==sample,
                          chip_position := paste0("R",i)]
          }
        }
        
        print("testing here +++++++++++++++++++++++++++")
        # sorty by plate, chip and chip_pos
        cols <- colnames(input_samples)
        len <- length(cols)-3
        cols <- cols[c(1:len, len+2, len+1, len+3)] # to get plate, chip and chip_position in this order
        core_sample_information <-
          input_samples[order(`plate`,`chip`, `chip_position`), c(cols), with = FALSE]
        
        # View(core_sample_information)
        
        if(input$Controls){
          ## some chips may have only seven rows if not enough samples
          seven_rows <- list()
          chips <- c(1:length(unique(core_sample_information$chip)))
          for (i in seq_along(chips)) {
            data <- core_sample_information[core_sample_information$chip == paste0("chip_", i)]
            if(nrow(data) == 7)
              seven_rows[[length(seven_rows) + 1]] <- data
          }
          
          
          print(seven_rows)
          
          ## Getting chip names which have seven rows
          if(length(seven_rows)){
            chip <- unlist(lapply(seven_rows , function(x){
              unique_chips <- unique(x$chip)
            }))
          }
          
          ## total number of plates
          total_plates <- unique(core_sample_information$plate)
          last_plate_data <- core_sample_information[core_sample_information$plate == total_plates[[length(total_plates)]]]
          last_plate_chips <- unique(last_plate_data$chip)
          print(input$Controls)
          print(input$Chip_num)
          print(input$Row_num)
        
          #' Number of rows we need to insert the controls
          #' We will look for the unique number of rows and creat a df with that many rows
          #' 
          rows_ned <- final_contorls()[[1]]
          print(rows_ned)
          control_df <- data.frame(matrix(0, nrow = length(rows_ned), ncol = ncol(input_samples)))
          colnames(control_df) <- colnames(core_sample_information)
          control_plates <- final_contorls()[[3]]
          control_chips <- final_contorls()[[2]]
          control_rows <- final_contorls()[[1]]
          control_df$plate <- final_contorls()[[3]]
          control_df$chip <- final_contorls()[[2]]
          control_df$chip_position <- final_contorls()[[1]]
          
          
          ## Extract data at control locations of original data, have 11 samples
          control_locations <- list()
          for (i in seq(control_plates)) {
            print(control_plates[i])
            print(control_chips[i])
            print(control_rows[i])
            control_locations[[length(control_locations) + 1]] <-
              core_sample_information[(core_sample_information$plate == control_plates[i] &
                                         core_sample_information$chip == control_chips[i] &
                                         core_sample_information$chip_position == control_rows[i])]
          }
          
          ## rbind list
          replace_data <- do.call(rbind, control_locations)
          print(replace_data)
          print(control_df)
          message("testing here ##########################.........")
          
          # data without controls
          without_controls_data <- core_sample_information[!(core_sample_information$SampleID %in% replace_data$SampleID)]
          
          ## Checking if control location data has been removed from the orignial data
          if(any(without_controls_data$SampleID %in% replace_data$SampleID))
            stop("Room not created for control samples")
          
          ## Checking if plate and chip at control and replaced data match
          if(!(identical(control_df$chip[1: nrow(replace_data)], replace_data$chip) &
               identical(control_df$plate[1: nrow(replace_data)], replace_data$plate)))
            stop("Control and removed data does not match")
          
          ## combine data
          insert_data <- rbind(without_controls_data, control_df)
          # View(insert_data)
          
          ## sort again
          new_data <-
            insert_data[order(`plate`,`chip`, `chip_position`), c(cols), with = FALSE]
          
          ## Change plate, chip and row for the removed data at control places
          replace_copy <- replace_data
          for (i in seq_along(seven_rows)) {
            replace_copy[i, len+1] <- unique(seven_rows[[i]]$plate)
            replace_copy[i, len+2] <- unique(seven_rows[[i]]$chip)
            replace_copy[i, len+3] <- paste0("R", nrow(seven_rows[[i]]) + 1)
          }
          
          print(replace_copy)
          
          #' Note: if inserted controls are less then the number of chips with 7 samples
          #' then there is an error, and I need to fix that
          #' 
          
          #' Assign remaining chips from removed data to new plate, chip  and row
          #' We can assign some removed samples at control locations to the chips which have 7 rows
          #' If no chip has seven rows, then we need to insert all at new chips
          
          lst_plt <- max(sort(insert_data$plate)) # get the last plate
          lst_plt <- as.numeric(unlist(strsplit(lst_plt, "_"))[2])
          
          lst_chip <- mixedsort(insert_data$chip)[nrow(insert_data)] # get last chip
          samp_num <- table(insert_data$chip)
          
          lst_chp_smp <- unname(samp_num[which(names(table(insert_data$chip)) == lst_chip)]) # get the samples on last chip
          lst_chip <- as.numeric(unlist(strsplit(lst_chip, "_"))[2])
          
          chip_count <- table(insert_data$plate) # get chips on last plate
          lst_plt_chps <- unname(chip_count[length(chip_count)])
          
          row_before <- length(seven_rows)
          strt <- row_before + 1
          
          cnt_lst_chp <- unlist(strsplit(tail(control_chips, n = 1), "_"))[2] # get the last control location so that we can skip it
          cont_locs <- unlist(strsplit(control_chips, "_"))
          cont_locs <-  as.numeric(cont_locs[which(!grepl("chip", cont_locs))])
          
          #' get the rows that are already occupied by controls on last chip
          rows_alrdy <- as.numeric(unlist(strsplit
                                          (control_df[control_df$chip
                                                      == paste0("chip_", cnt_lst_chp), ]$chip_position, "R")))
          cat("Rows already ......\n", rows_alrdy)
          
          #' Now we will put the replaced data back on the chips
          #' This data was at control locations, but removed after inserting the controls
          #' we also need to check if any control is already inserted to an empty last chip
          #' If so, then we need to skip that place while inserting 
          #' If some have 7 rows, then some may already have correct information 
          #' and we are starting from that location
          #' If we have room for all the replaced data (on the chips with seven rows), we don't need to
          #' send anything to last chip i,e. when nrow(replaced_data) == length(seven_rows)
          if(nrow(replace_data) > length(seven_rows)){
            for (i in strt:nrow(replace_data)) {
              
              if(lst_plt_chps > 96 ){ # if the chips reach to 96, then we need to start a new plate
                lst_plt <- lst_plt + 1
                # lst_plt <- paste0("plate", "_", lst_plt )
                lst_plt_chps <- 1 # reset chip number on lst plate
              }
              
              if(lst_chp_smp == 8 ){ # if the chips reach to 8, then we need to start a new chip
                lst_chip <- lst_chip + 1
                lst_chp_smp <- 1
              }
              
              lst_plt_chps <- lst_plt_chps + 1
              lst_chp_smp <- lst_plt_chps + 1
              
              
              # rows <- c(1:4,6:8)
              replace_copy[i, len+1] <- paste0("plate", "_", lst_plt)
              replace_copy[i, len+2] <- paste0("chip", "_", lst_chip)
              
              row_num <- i - row_before 
              if(row_num %in% rows_alrdy & cnt_lst_chp == lst_chip){ # if there is a control on last chip
                row_num = row_num + 1
                row_before <- row_before - 1
              }
              replace_copy[i, len+3] <- paste0("R", row_num)
              
            }
          }
            
    
          ## Combine data
          final_data <- rbind(new_data, replace_copy)
          # View(final_data)
        }else{
          final_data <- core_sample_information
        }
        
        ## Sort again
        final_data <-
          final_data[order(`plate`,`chip`, `chip_position`), c(cols), with = FALSE]
      }
    })
  })
  
  
  
  # render randomized data
  output$randomdata <- renderDataTable({
    if(is.null(randomized_data()))
      return(NULL)
    print(head(randomized_data()))
    df <- data.frame(randomized_data(), stringsAsFactors = FALSE)
    sorted <- df[mixedorder(df$chip, decreasing = FALSE), ]
    
    randomdata <- DT::datatable(sorted, options = list(scrollX= '300px'),
                                selection = list(target = 'column'))
  })
  
  
  #' Arrange the chips on plates as final design.
  data_plates <- eventReactive(input$Submit,{
    
    if(is.null(randomized_data()))
      return(NULL)
    
    final_data <- randomized_data()
    # View(data.frame(final_data))
    ## Extract each plate information and arrange on chips
    plate_num = c(1:length(unique(final_data$plate)))
    print(plate_num)
    
    ## creating and naming list
    plate_names <- unique(final_data$plate)
    print(plate_names)
    
    arranged_chip_list <- vector("list", length(plate_names))
    names(arranged_chip_list) <- plate_names
    
    ## Assign bloolid  and chips_pos on plates
    total_chips <- length(unique(final_data$chip))
    
    chip_info <- function(data){
      chips <- c(1:length(unique(data$chip)))
      for (i in seq_along(chips)) {
        
        ## Assig plate number for chip
        assign_plate <- ceiling(i/12)
        
        ## Extract data belonging to single chip
        chip_data <- data[data$chip == paste0("chip_", i)]
        message("Printing chip data...........")
        print(chip_data)
        
        ## Rownames
        if(length(arranged_chip_list[[assign_plate]]) == 0){
          message("Inside here ------------------------")
          print(arranged_chip_list[[assign_plate]])
          print(chip_data$chip_position)
          arranged_chip_list[[assign_plate]] <- data.frame(matrix(0, ncol = 12, nrow = 8))#nrow(chip_data) # it was 8 previously
          rownames(arranged_chip_list[[assign_plate]]) <- paste0("R", 1:8) # chip_data$chip_position
        }
        
        print(arranged_chip_list[[assign_plate]])
        message("I am here >>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
        ## Which column to assign
        if(assign_plate == 1) {
          col_num = i
        }else{
          col_num = i - (12 * (assign_plate - 1))
        }
        message("Now I am here ******************************")
        print(arranged_chip_list[[assign_plate]][, col_num])
        print(chip_data$SampleID)
        
        #' if the some chips have seven samples or less, we need to fill the others with NA
        #' before inserting SampleID into the chip
        #' 
        if(length(arranged_chip_list[[assign_plate]][, col_num]) > length(chip_data$SampleID )){
          rem_len <- length(arranged_chip_list[[assign_plate]][, col_num]) - length(chip_data$SampleID)
          print(rem_len)
          elements <- rep(NA_integer_, rem_len)
          arranged_chip_list[[assign_plate]][, col_num] <- c(chip_data$SampleID, elements)
        }else{
          message("in else part ==========================")
          arranged_chip_list[[assign_plate]][, col_num] <- chip_data$SampleID
        }
        message("Testing here &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&")
      }
      return(arranged_chip_list)
    }
    
    
    final_data <- as.data.table(final_data)
    print(final_data)
    message("here ---------===============---------------")
    arranged_chip_data <- chip_info(final_data)
    print(arranged_chip_data)
    
  
    ## Rename chips
    chips <- 12
    for (i in seq_along(arranged_chip_data)) {
      if(i == 1) first <- 1
      colnames(arranged_chip_data[[i]]) <- paste0("chip_", first:(chips*i))
      first <- (chips*i) + 1
    }
    
    
    # remove columns with all zeros
    arranged_chip_data <-lapply(arranged_chip_data, function(x) x[colSums(x, na.rm = TRUE) > 0])
    
    
    #' if there are less than 8 samples on a chip, duplicated samples may be introducted
    #' we need to replaced duplicated with NA
    arranged_chip_data_final <- lapply(arranged_chip_data, function(x){
      for (i in 1:ncol(x)){
        x[, i][duplicated(x[, i])] <- NA
      }
      x
    })
  })
  
  
  # Display one plate data
  output$finaldata <- renderDataTable({
    message("Displaying plate data---------------")
    print(data_plates())
    message("again ......................")
    print(data_plates())
    if(is.null(data_plates()))
      return(NULL)
    print(input$Finaldesing)
    if(is.null(input$Finaldesign)){ #is.null(input$Finaldesign)
      plate <- 1  
    }else{
      plate <- input$Finaldesign
      plate <- unlist(strsplit(plate, "_"))
      plate <- as.numeric(plate[2])
    }
    message("Displaying data at the end...")
    print(input$Finaldesign)
    print(head(data_plates()[[1]]))
    message("Displaying data at the end")
    randomdata <- DT::datatable(data_plates()[[plate]], options = list(scrollX= '300px')) 
  })
  
  
  # validate selecting columns to balance randomization
  message("Testing above plot ------------")
  observe({
    if(input$tabset1 == "Plot"){
      if(!is.null(randomized_data())){
        if(is.null(input$randomdata_columns_selected)){
          shinyalert("Oops!", "Please select columns from randomized data to plot by clicking",
                     type = "error")
        }
      }
    }
  })
  
  message("Testing here at reactive plot_info ------------")
  #' Render plot in the tab panel
  plot_info <- reactive({
    if(is.null(input$InputFile) | is.null(inputfile()))
      return(NULL)
    if(is.null(randomized_data()))
      return(NULL)
    
    if(input$tabset1 == "Plot"){
      message("I am testing inside the plot tabset------")
      if(!is.null(randomized_data())){
        message("I am testing more inside the plot tabset------")
        if(is.null(input$randomdata_columns_selected)){
          shinyalert("Oops!", "Please select columns from randomized data to plot by clicking",
                     type = "error")
          return(NULL)
        }
        else if(length(input$randomdata_columns_selected) > 2){
          shinyalert("Oops!", "Selecting more than two columns not allowed",
                     type = "error")
          return(NULL)
        }
        else if(length(input$randomdata_columns_selected) == 1){
          shinyalert("Oops!", "Please select two columns to plot",
                     type = "error")
          return(NULL)
        }
      }
    }
    
    message("Now testing here ----000000000000000000000000")
    plot_type <- input$PlotSettings
    print(plot_type)
    print(head(randomized_data()))
    print(input$randomdata_columns_selected)
    xaxis <- colnames(randomized_data())[input$randomdata_columns_selected[1]]
    yaxis <- colnames(randomized_data())[input$randomdata_columns_selected[2]]
    print(xaxis)
    print(yaxis)
    df <- data.frame(randomized_data()[randomized_data()$ParticipantID != 0, ], check.names = FALSE)
    print(head(df))
    print(is.factor(df[[xaxis]]))
    print(is.factor(df[[yaxis]]))
    if(is.null(plot_type))
      return(NULL)
    if(plot_type == 'sunflower'){
      df[[xaxis]] <- as.factor(df[[xaxis]])
      df[[yaxis]] <- as.factor(df[[yaxis]])
      sunflowerplot(df[[xaxis]], df[[yaxis]], xlab = input$xaxis, ylab = input$yaxis,
                    cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
      par(xpd=TRUE)
      title(main = input$maintitle)
      legend(4, -3, c("Unique","Duplicates"), cex=1,
             col=c("black","red"), pch=21:22, lty=1:2)
    }
    else if(plot_type == 'violin'){
      p <- ggplot(df, aes_string(x = xaxis, y = yaxis, fill = xaxis)) + geom_violin() + theme_classic() 
      p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
        theme(plot.title = element_text(hjust = 0.5, size = 22),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 18),
              legend.position = "none") + 
        stat_summary(fun.y = mean, geom = "point", size = 5)
    }
    else if(plot_type == 'density'){
      p <- ggplot(df, aes_string(x = xaxis, color = yaxis)) + geom_density() + theme_classic()
      p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
        theme(plot.title = element_text(hjust = 0.5, size = 22),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 18))
    }
    else if(plot_type == 'box'){
      p <- ggplot(df, aes_string(x = xaxis, y = yaxis, fill = xaxis)) +
        geom_boxplot(outlier.color = "red") + theme_classic()
      p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
        theme(plot.title = element_text(hjust = 0.5, size = 22),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 18),
              legend.position = "none") +
        stat_summary(fun.y = mean, geom = "point", size = 5)
    }
    else if(plot_type == 'dotplot'){
      p <- ggplot(df, aes_string(x = xaxis, y = yaxis, fill = xaxis)) +
        geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.7) + theme_classic()
      p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
        theme(plot.title = element_text(hjust = 0.5, size = 22),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 18),
              legend.position = "none") +
        stat_summary(fun.y = mean, geom = "point", size = 3)
    }
    else if(plot_type == 'histogram'){
      p <- ggplot(df, aes_string(x = xaxis, color = yaxis)) + geom_histogram() + theme_classic()
      p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
        theme(plot.title = element_text(hjust = 0.5, size = 22),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 18))
    }
  })
  
  
  # display plot
  output$PlotSummary <- renderPlot({
    if(is.null(plot_info()))
      return(NULL)
    plot_info()
  })
  
  
  
  #' This function is work around for downloading the plot
  #' It is a bit tricky. if we don't use this function, the plot is not downloaded
  #' 
  plot_info1 <- function(){
    if(is.null(input$InputFile) | is.null(inputfile()))
      return(NULL)
    if(is.null(randomized_data()))
      return(NULL)
    
    if(input$tabset1 == "Plot"){
      message("I am testing inside the plot tabset------")
      if(!is.null(randomized_data())){
        message("I am testing more inside the plot tabset------")
        if(is.null(input$randomdata_columns_selected)){
          shinyalert("Oops!", "Please select columns from randomized data to plot by clicking",
                     type = "error")
          return(NULL)
        }
        else if(length(input$randomdata_columns_selected) > 2){
          shinyalert("Oops!", "Selecting more than two columns not allowed",
                     type = "error")
          return(NULL)
        }
        else if(length(input$randomdata_columns_selected) == 1){
          shinyalert("Oops!", "Please select two columns to plot",
                     type = "error")
          return(NULL)
        }
      }
    }
    
    message("Now testing here ----000000000000000000000000")
    plot_type <- input$PlotSettings
    print(plot_type)
    print(head(randomized_data()))
    print(input$randomdata_columns_selected)
    xaxis <- colnames(randomized_data())[input$randomdata_columns_selected[1]]
    yaxis <- colnames(randomized_data())[input$randomdata_columns_selected[2]]
    print(xaxis)
    print(yaxis)
    df <- data.frame(randomized_data()[randomized_data()$ParticipantID != 0, ], check.names = FALSE)
    print(head(df))
    print(is.factor(df[[xaxis]]))
    print(is.factor(df[[yaxis]]))
    if(is.null(plot_type))
      return(NULL)
    if(plot_type == 'sunflower'){
      df[[xaxis]] <- as.factor(df[[xaxis]])
      df[[yaxis]] <- as.factor(df[[yaxis]])
      sunflowerplot(df[[xaxis]], df[[yaxis]], xlab = input$xaxis, ylab = input$yaxis,
                    cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
      par(xpd=TRUE)
      title(main = input$maintitle)
      legend(4, -3, c("Unique","Duplicates"), cex=1,
             col=c("black","red"), pch=21:22, lty=1:2)
    }
    else if(plot_type == 'violin'){
      p <- ggplot(df, aes_string(x = xaxis, y = yaxis, fill = xaxis)) + geom_violin() + theme_classic() 
      p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
        theme(plot.title = element_text(hjust = 0.5, size = 22),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 18),
              legend.position = "none") + 
        stat_summary(fun.y = mean, geom = "point", size = 5)
    }
    else if(plot_type == 'density'){
      p <- ggplot(df, aes_string(x = xaxis, color = yaxis)) + geom_density() + theme_classic()
      p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
        theme(plot.title = element_text(hjust = 0.5, size = 22),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 18))
    }
    else if(plot_type == 'box'){
      p <- ggplot(df, aes_string(x = xaxis, y = yaxis, fill = xaxis)) +
        geom_boxplot(outlier.color = "red") + theme_classic()
      p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
        theme(plot.title = element_text(hjust = 0.5, size = 22),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 18),
              legend.position = "none") +
        stat_summary(fun.y = mean, geom = "point", size = 5)
    }
    else if(plot_type == 'dotplot'){
      p <- ggplot(df, aes_string(x = xaxis, y = yaxis, fill = xaxis)) +
        geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.7) + theme_classic()
      p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
        theme(plot.title = element_text(hjust = 0.5, size = 22),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 18),
              legend.position = "none") +
        stat_summary(fun.y = mean, geom = "point", size = 3)
    }
    else if(plot_type == 'histogram'){
      p <- ggplot(df, aes_string(x = xaxis, color = yaxis)) + geom_histogram() + theme_classic()
      p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
        theme(plot.title = element_text(hjust = 0.5, size = 22),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 18))
    }
  }
  
  
  
  #' To download summary data
  output$randdownload <- downloadHandler(
    filename <- function() { paste('Randomized_data', '.csv', sep='')},
    content <- function(file) {
      write.csv(randomized_data(), file)
    })
  
  
  
  #' Download sample data
  #' 
  output$sampledata <- downloadHandler(
    filename = "Sample Data.csv",
    content = function(file){
      file.copy("www/Dummydata.csv", file)
    }
  )
  outputOptions(output, "sampledata", suspendWhenHidden = FALSE)
  
  
  
  #' Download tutorial 
  #' 
  output$TutorialDownl <- downloadHandler(
    filename = "Tutorial.pdf",
    content = function(file){
      file.copy("www/Tutorial.pdf", file)
    }
  )
  outputOptions(output, "TutorialDownl", suspendWhenHidden = FALSE)
  
  
  #' To download the final design data
  #' We will creat a zip folder for all the plates and download as zip
  output$finaldownload <- downloadHandler(
    filename <- function() { paste('Final_Design', '.zip', sep='')},
    content <- function(file) {
      files <- NULL
      for (i in 1:length(data_plates())) {
        fileName = paste0("Plate_", i, ".csv")
        write.csv(data_plates()[[i]], fileName)
        files <- c(fileName, files)
        
      }
      zip(file, files)
    })
  
  
  
  ## Download plot
  output$plotdownload <- downloadHandler(
    filename = "plot.png",
    content = function(file){
      if(input$PlotSettings == "sunflower"){
        png(file)
        plot_info1()
        dev.off()
      }else{
        ggsave(file, plot_info())
      }
    })
})
# End ----------------------------------
   