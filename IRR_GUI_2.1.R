## Interrater reliability Cohen's Kappa Caculator GUI
#
# The aim of this piese of script is to build a GUI that can take raw data (required format and structure) from coders
# and output the interrater reliability Cohen's Kappa for each Knowlege category.
#
# Cohen’s Kappa:
# Kappa = (Po - Pe) / (1 - Pe) = 1 - (1 - P0 ) / (1 - Pe)
#
#•	Only data sets in CSV files are acceptable.
#
#•	Data sets must have variables containing information of coders, IT category, Knowledge classification, sentence number, 
#   complementarity ( subclass under what: objective capability ) and problem ( subclass under what: subjective capability ). 
#
#•	The names of those variables mentioned above should be exactly written as "Coder", "IT", "Knowledge", "Sen#.", "Complementarity" 
#   and "Problem". 
#





###############check and load necessary packages#####################


package_needed <- c("gWidgets","tcltk","gWidgetsRGtk2")  # packages used in this script
pack_install_idx <- which(package_needed %in% rownames(installed.packages()) == F)
# find the package or packages needed but not installed

if(length(pack_install_idx) > 0){  
  install.packages(package_needed[pack_install_idx]) 
  # Install the pakage(s) needed but not installed
}

library(tcltk)
library(gWidgets)
options ( guiToolkit="RGtk2" )




###############Functions for data reoranization and calculation####################


mywait <- function( text_display, title ) {  #define a function that pauses the script
  #with a pop-out window. It takes the title of the pop-out window and text display in
  #the window as inputs.
  
  
  tt <- tktoplevel()    #creat a widget
  
  tktitle(tt) <- title   
  labelText <- tclVar(text_display)
  label <- tklabel(tt,text=tclvalue(labelText))
  tkconfigure(label,textvariable=labelText)
  tkgrid(label)
  continue <- tkbutton(tt, text='Continue', command=function()tkdestroy(tt))
  tkgrid(continue)
  tkbind(tt,'<Key>', function()tkdestroy(tt) )
  tkwait.window(tt)
}



inter_reli <- function(num_agree,num_total,coder1_no,coder1_yes
                       ,coder2_no,coder2_yes){  
  #function that calculates interrater reliability Kappa
  #It takes number of sentences in agreetment, total numnber of sents, number of 
  #negative labels in coder 1 and coder2, number of positive labels in coder 1 and 2
  #as inputs.
  
  pra = num_agree/num_total       #from predefined formula
  pre = coder1_no/num_total * coder2_no/num_total + 
    coder1_yes/num_total * coder2_yes/num_total
  
  k = (pra - pre) / (1 - pre)
  kap <- round(k,2)  #round reasults to 2 decimal places
  
  return(kap)
}


renumber_sents <- function(dataof1coder = inputDW ){
  #function to renumber sententces from multiple essays
  #It takes raw data set from one coder as input
  
  idx_sent1 <- which(dataof1coder$Sent.. == 1) #find the idx of first sents indicating 
  #the begining of each essay
  
  len_idx_sent1 <- length(idx_sent1)  #number of elements n 
  
  idx_subtractor <- idx_sent1[-len_idx_sent1]  #first n-1 elements
  
  idx_Subtrahend <- idx_sent1[-1]  #last n-1 elements, its the substractor shift by 1
  
  cut_off_point_idx_idx <- which(idx_Subtrahend - idx_subtractor > 1) #substraction
  
  idx_first_obs_incom <- c (idx_Subtrahend[cut_off_point_idx_idx])
  #idea: the cutoff points were thoses breakpoints. Eg. 1, 2, 3, 18, 19, 32.  18 and 32
  #shift the vector and do substraction then pick out idx with elements > 1
  #does not include the first obs of the first essay
  
  idx_last_obs_incom <- idx_first_obs_incom-1 #find the idx of last sents indicating the end 
  #of each essay. does not include the last sent of last essay
  
  
  max_sent <- as.numeric(as.character(dataof1coder$Sent..[idx_last_obs_incom])) 
  #check max sent of each essay
  
  addend_incom <- cumsum(as.numeric(as.character(max_sent)))
  
  idx_first_obs <- c(1, idx_first_obs_incom)   #add first sent of whole data set
  
  idx_last_obs <- c(idx_last_obs_incom, dim(dataof1coder)[1])  #add last sent of last essay
  
  addend <- c(0, addend_incom)
  
  dataof1coder$Sent.. <- as.numeric(as.character(dataof1coder$Sent..))
  
  for (i in 1:length(addend) ){  #for loop to renumber sents from different essays
    
    dataof1coder$Sent..[idx_first_obs[i]:idx_last_obs[i]] <-
      dataof1coder$Sent..[idx_first_obs[i]:idx_last_obs[i]] + addend[i]
  } 
  
  #sents from each essay are numbered as continuation of labels of sents of 
  #previous essays  
  
  return(dataof1coder)
  
}



Inter_reliability <- function(data1_renum,data2_renum){
  
  #function that takes inputs from two raw data sets and outputs interrater reliability 
  #It takes the renumered data sets from both coders as inputs 
  
  
  input_total <- merge(data1_renum,data2_renum, by = intersect(names(data1_renum), 
                                                               names(data2_renum)),all = T)    
  #merge the two input data sets into one total set by common cols
  
  input <- input_total[,c("Coder","IT","Knowledge","Sent..",
                          "Complementarity","Problem")]
  
  # select variables that will be used later
  
  num_sent_total = max(as.numeric(as.character(input$Sent..)))  
  
  ori_Knowledge_levels <-levels(factor(input$Knowledge)) #original levels of factor Knowledge
  
  input$Knowledge <- as.character(input$Knowledge)
  #convert to characters to get a better handle
  
  idx_what_ob_com <- which( (input$Knowledge == "what: objective capability" 
                             & ( input$Complementarity == "X" | input$Complementarity == "x")) )
  #pick out rows with knowledge "what obj" & complementarity, get those idx
  
  idx_what_ob_nocom <- which( (input$Knowledge == "what: objective capability" 
                               &  input$Complementarity == "") )
  
  idx_what_sub_pro <- which( (input$Knowledge == "what: subjective capability" 
                              & ( input$Problem == "X" | input$Problem == "x")) )
  #pick out rows with knowledge "what subj" & pro, get those idx
  
  idx_what_sub_nopro <- which( (input$Knowledge == "what: subjective capability" 
                                &  input$Problem == ""  ))
  
  #count_in_new_levels <- c(length(idx_what_ob_com),length(idx_what_ob_nocom),
  #                         length(idx_what_sub_pro),length(idx_what_sub_nopro))  #get sent count in the 4 new levels 
  
  
  #levels(input$Knowledge) <- c(ori_Knowledge_levels
  #                             ,possible_new_levels[which(count_in_new_levels>=1)])  
  
  #only add levels appear in the data set
  
  
  #levels(input$Knowledge) <- c(ori_Knowledge_levels,
  #                             "what: objective capability with com", 
  #                             "what: objective capability without com",
  #                             "what: subjective capability with pro",
  #                             "what: subjective capability without pro")
  
  #add 4 new factor levels of nowledge we will use to replace the old ones 
  #(what obj & what subj)
  #Be careful!!! Add new levels at the end NOT the beginig so that it won't 
  #affect the input data right now
  #changing factor levels will result in changing factor values in the data. Avoid this!
  #Handle it as characters would be much easier.
  
  input[idx_what_ob_com,"Knowledge"] <- "what: objective capability with com"
  #replace the old levels with new ones
  
  input[idx_what_ob_nocom,"Knowledge"] <- "what: objective capability without com"
  #replace the old levels with new ones
  
  input[idx_what_sub_pro,"Knowledge"] <- "what: subjective capability with pro"
  
  input[idx_what_sub_nopro,"Knowledge"] <- "what: subjective capability without pro"
  
  input$Knowledge = factor(input$Knowledge)  #delete levels did not appear in data set
  
  know_level_modified <- levels(input$Knowledge) #check to make sure the organized levels are correct now
  
  Knowledge_levels_total<- nlevels(input$Knowledge)
  #get number of levels of knowledge in the total(all coders) data set, will be used in loops later
  
  input_subset <- input[,1:4]   #subset information useful for calculation, don't need com and pro any more
  
  
  ref_Coder <- levels(factor(input_subset$Coder))        #check the levels of Coder
  #use factor() to delete the level "" (come from blank rows)
  ref_Knowledge <- levels(factor(input_subset$Knowledge))      
  #check the levels of Knowledge
  
  
  Sent_by_Knowledge_Yes = list()      
  Sent_agree_by_Knowledge_Yes = list()  
  Sent_disa_by_Knowledge_Yes = list()
  Sent_agree_by_Knowledge_No = list() 
  check_Sent_agree_by_Knowledge_Yes = list()
  
  Sent_by_Know_Yes_DW = list()
  Sent_by_Know_Yes_SM = list()
  Sent_by_Know_No_DW = list()
  Sent_by_Know_No_SM = list()
  #create an empty lists to hold sent vectors by knowledge categories 
  
  inter_k <- numeric(length(know_level_modified))
  
  whole_sent_set = 1:num_sent_total #total set of sents in the essay
  
  
  for (i in 1 : Knowledge_levels_total){  #loop through all knowledge levels
    
    Sent_by_Know_Yes_DW[[i]]<- input_subset[(input_subset$Knowledge == ref_Knowledge[i]
                                             & input_subset$Coder == ref_Coder[1])
                                            , "Sent.."]  
    #find Sent by know from each coder respectively
    Sent_by_Know_Yes_SM[[i]]<- input_subset[(input_subset$Knowledge == ref_Knowledge[i]
                                             & input_subset$Coder == ref_Coder[2])
                                            , "Sent.."]  
    Sent_agree_by_Knowledge_Yes[[i]] <- intersect(Sent_by_Know_Yes_DW[[i]]
                                                  ,Sent_by_Know_Yes_SM[[i]])
    #different method to check sent agreed by Knowledge
    Sent_by_Know_No_DW[[i]]<- setdiff(whole_sent_set, Sent_by_Know_Yes_DW[[i]])
    #Sent of No by knowledge of each coder
    Sent_by_Know_No_SM[[i]]<- setdiff(whole_sent_set, Sent_by_Know_Yes_SM[[i]])
    
    Sent_agree_by_Knowledge_No[[i]] <- intersect(Sent_by_Know_No_DW[[i]]
                                                 ,Sent_by_Know_No_SM[[i]])
    #Sent of both No
    
    Sent_by_Knowledge_Yes[[i]] <- input_subset[input_subset$Knowledge 
                                               == ref_Knowledge[i], "Sent.."] 
    #get Sents by Knowledge from both coders (may including duplicate elements when 
    #two coders made same classification, duplicate element sent are the ones that 
    #two coders were in aggrement  ) 
    
    Sent_disa_by_Knowledge_Yes[[i]] <- setdiff(Sent_by_Knowledge_Yes[[i]]
                                               ,Sent_agree_by_Knowledge_Yes[[i]])
    #find different elements between Yes Sent from both coders (1 and 2 yes) 
    #and Yes Sent both coder agreed on (2 yes) the difference is the Yes sent (1 yes 1 no) 
    
    num_agree <- length(Sent_agree_by_Knowledge_Yes[[i]]) +
      length(Sent_agree_by_Knowledge_No[[i]])  #NoNo and YesYes
    
    coder1_no <- length(Sent_by_Know_No_DW[[i]])
    coder2_no <- length(Sent_by_Know_No_SM[[i]])
    coder1_yes <- length(Sent_by_Know_Yes_DW[[i]])
    coder2_yes <- length(Sent_by_Know_Yes_SM[[i]])
    
    inter_k[i] <- inter_reli(num_agree,num_sent_total,coder1_no,coder1_yes
                             ,coder2_no,coder2_yes)
    # call the formula function to do calculation 
    
  } 
  
  
  know_level_modified[know_level_modified == ""] <- "no code" 
  # replcace knowledge level  "" with "no code
  
  #table_result <- cbind(know_level_modified,inter_k)  #appear as table
  table_result <- data.frame(know_level_modified,inter_k)  #appear as table
  
  colnames(table_result) <- c("Knowledge Category","Kappa")
  
  #result = list(inter_reliabilty = inter_k, know_levels = know_level_modified )
  return(table_result)
  
}


delete_extra_no_code <- function(renumbered_data){
  #function to pick out and delete rows with additional "no code" knowledged
  #num_obs <- dim(testdata)[1]  #number of observations of the data set
  #max_sent <- as.numeric(as.character(testdata[num_obs,"Sent.."])) #the max sent after renumbering
  max_sent <- max(as.numeric(as.character(renumbered_data$Sent..))) #the max sent after renumbering
  
  Know_by_sent <- list()
  idx_to_delete <- list()
  
  for (i in 1:max_sent){
    #loop through sents
    Know_by_sent[[i]]  <- renumbered_data[renumbered_data$Sent.. == i,"Knowledge"]
    #get all the knowledges of each sent
    know_num <- length(Know_by_sent[[i]])  #number of knowledge of the sent
    
    if (know_num >= 2 & "" %in% Know_by_sent[[i]] ){
      #if the sent was classified into more than one knowledges 
      #and "" was one of its knowledges
      
      idx_to_delete[[i]] <- which(renumbered_data$Sent.. == i 
                                  & renumbered_data$Knowledge == "" )
      #find the idx of the rows that meet the condition
      
    }
  }
  
  if (length(idx_to_delete) > 0){  # if idx_to_delete is a blank list, unlist it will get NULL!
    
    renumbered_data <- renumbered_data[-unlist(idx_to_delete),]  
    #delete rows with extra no code knowledge
    
  }
  
  
  
  return(renumbered_data)
}



get_score <- function( mixIT , data1, data2){
  ###main function that calls functions defined above to connect raw input to 
  #final output
  #mixIT is a binary argument represents whether or not different IT levels will 
  #be mixed and considered as the same in the calculation. If F, claculation will
  #be conducted seperately for each IT level.
  
  
  #data1 <- read.csv(file.choose(), colClasses = "factor",header = T, sep = ",")  #data from first coder
  #data2 <- read.csv(file.choose(), colClasses = "factor",header = T, sep = ",")  
  #Set variable type as FACTOR!!! If not, the empty variable (no values) will appear 
  #as logical and filled with NAs. That can cause problem! 
  
  data1 <- data1[(data1$Coder!="" | data1$Date!=""),] #delete blank rows
  data2 <- data2[(data2$Coder!="" | data2$Date!=""), ] #delete blank rows
  
  
  if (mixIT == F){
    
    ref_IT <- levels(factor(data1$IT))   #levels of IT
    n_levels_IT <- length(ref_IT)  #number of levels of IT
    
    data1_sub <- list()
    data2_sub <- list()
    Inter_k_IT <- list()
    
    for (i in 1:n_levels_IT){
      
      #loop through IT levels and renumber sents according to IT levels
      data1_sub[[i]] <- data1[data1$IT == ref_IT[i],]  #subset by IT category
      data1_sub[[i]] <- renumber_sents(data1_sub[[i]]) #renumber the subset
      data1_sub[[i]] <- delete_extra_no_code(data1_sub[[i]])  ##delete rows with extra no code knowledge
      
      data2_sub[[i]] <- data2[data2$IT == ref_IT[i],]
      data2_sub[[i]] <- renumber_sents(data2_sub[[i]])
      data2_sub[[i]] <- delete_extra_no_code(data2_sub[[i]]) 
      
      Inter_k_IT[[i]] <- Inter_reliability(data1_sub[[i]], data2_sub[[i]] )  #calculate kappa
      
    } 
    
    #data1_renum <- do.call("rbind", data1_sub)  #cbind renumbered data sets back to one set
    #data2_renum <- do.call("rbind", data2_sub)
    
    output <- list(IT_level = ref_IT, Inter_k_IT = Inter_k_IT) 
    #output with a list. 1st item contains the IT categories and 2nd item contains 
    #corresponding kappa values
    
    return(output)
    
  }else{
    
    data1 <- renumber_sents(data1)  #renumber data set from coder1
    data1 <- delete_extra_no_code(data1)  
    
    data2 <- renumber_sents(dataof1coder = data2) #renumber data set from coder2
    data2 <- delete_extra_no_code(data2) 
    
    output <- Inter_reliability(data1,data2) #calculate kappa
    
    return(output)
  }
  
  
}




#####################Build GUI##################################################

#library(gWidgetsRGtk2)

w = gwindow("Simple IRR GUI",width = 300 , height = 500)

gg<- ggroup(container = w,spacing = 20, horizontal = F)

g1 <- ggroup(container = gg,spacing = 10, horizontal = T)

lbl_data_name <- glabel(
  "Raw Data1, renamed as: ",
  container = g1
)

addSpring(g1)

txt_data_frame_name1 <- gedit("DataCoder1", cont = g1)



g2 <- ggroup(container = gg,spacing = 10, horizontal = T)

lbl_data_name2 <- glabel(
  "Raw Data2, renamed as: ",
  container = g2
)

addSpring(g2)

txt_data_frame_name2 <- gedit("DataCoder2", cont = g2)


status_bar <- gstatusbar("", container = w)



g3 <- ggroup(container = gg,spacing = 10, horizontal = T)

mix_IT_label <- glabel(
  "Mix all IT categories as one ? ",
  container = g3
)

addSpring(g3)

mixITorNot <- gdroplist(c("Yes", "No"), count = T, cont = g3)  #dropdown list for mixIT


text1 = "1. Change the names of Datasets used on the main interface. (optional)
2. Select if you want do the caluculation with Mix_IT from the dropdown-menu on the main interface.
3. Click on continue button on the pop-out window."

mywait(text1, "Step 1")   
#pop-out window that pauses the script with text displayed in the window


#######################scipt pauses here



mixIT_value <- svalue(mixITorNot) #Yes or No for mixIT


if (mixIT_value == "Yes"){
  status_bar <- gstatusbar("MixIT", container = w)
  mixIT = T
}else{
  status_bar <- gstatusbar("Separate calculation by IT category", container = w)
  mixIT = F
}

assign("mixIT", mixIT, envir = globalenv())


gp1 <-ggroup(container = gg,spacing = 10, horizontal = T)


btn_upload1 <- gbutton(   #push button to upload raw data files (only accept csv)
  text      = "Raw Data Coder1",
  container = gp1,
  handler   = function(h, ...)
  {
    gfile(
      text    = "Upload data from coder 1",
      type    = "open",
      action = "read.csv",
      handler = function(h, ...)
      {
        tryCatch(
          {
            data_frame_name <- make.names(svalue(txt_data_frame_name1))
            the_data <- do.call(h$action, list(h$file, colClasses = "factor"))
            assign(data_frame_name, the_data, envir = globalenv())
            svalue(status_bar) <- "Data from coder 1 uploaded"
          },
          error = function(e) svalue(status_bar) <- "Cannot upload data"
        )
      },
      
      filter = list(
        "Comma delimited" = list(patterns = c("*.csv","*.xls")),
        "All files" = list(patterns = c("*"))
      )
    )
  }
)

addSpring(gp1)

btn_upload2 <- gbutton(   #push button to upload raw data files (only accept csv)
  text      = "Raw Data Coder2",
  container = gp1,
  handler   = function(h, ...)
  {
    gfile(
      text    = "Upload data from coder 2",
      type    = "open",
      action = "read.csv",
      handler = function(h, ...)
      {
        tryCatch(
          {
            data_frame_name <- make.names(svalue(txt_data_frame_name2))
            the_data <- do.call(h$action, list(h$file, colClasses = "factor"))
            assign(data_frame_name, the_data, envir = globalenv())
            svalue(status_bar) <- "Data from coder 2 uploaded"
          },
          error = function(e) svalue(status_bar) <- "Cannot upload data"
        )
      },
      
      filter = list(
        "Comma delimited" = list(patterns = c("*.csv","*.xls")),
        "All files" = list(patterns = c("*"))
      )
    )
  }
)



gp2 <-ggroup(container = gg,spacing = 10, horizontal = F)

Calculate <- gbutton(   #push button to start calculataion
  text      = "Calculate IRR",
  container = gp2,
  expand = T,
  handler = function(data1,data2,names){
    data1<- DataCoder1
    data2<- DataCoder2
    
    tryCatch(
      {
        output<- get_score(mixIT = mixIT,data1,data2)
        svalue(status_bar) <- "Calculation succeeded"
      },
      error = function(e) svalue(status_bar) <- "Cannot calculate"
    )
    
    assign("myOutput", output, envir = globalenv())
    
  }
  
)


#addSpring(gp2)



text2 = "1. Load the first Dataset and check the status bar on the main interface.
2. Load the first Dataset and check the status bar on the main interface.
3. Click on Calculate button on the main interface.
4. Click on continue button on the pop-out window."

mywait(text2, "Step 2")



###############script pauses here 


gp3 <- ggroup(container = gg,spacing = 20, horizontal = F, use.scrollwindow = F)


if( mixIT == T){
  
  table_result <- myOutput
  
  status_bar <- gstatusbar("MixIT", container = w)
  
}else{
  
  table_result <- myOutput[[2]][1]
  
  status_bar <- gstatusbar("Check full result in console", container = w)
  
}


table_re <- gtable(table_result, container = gp3, multiple = TRUE ) #display result with a table in GUI
size(table_re) <- c(250, 250)
#For MixIT, it is the final result
#For non MixIT, it is the result of the first IT level. Full result is in consle


text3 = "1.  If Mix_IT is selected (Yes), check results in the tabel on the main interface.
2.  Click on continue button on the pop-out window.

OR

1*. If Mix_IT is not selected (No),  on the main interface.
2*. Click on continue button on the pop-out window.
3*. Check results in the console of R.
"

mywait(text3, "Step 3")

##########script pauses here 

myOutput

######full result in console


