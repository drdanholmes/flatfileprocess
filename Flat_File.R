########################################################################################
# This source code is released under GNU Public License V3. It is provided 
# free of charge and without any guarantee.
# For details of the GPL license see http://www.gnu.org/licenses/gpl-3.0.en.html
# Use at your own risk.
########################################################################################

library(tcltk2)

#make a radiobutton widget
#source for tk widget modifed from http://www.sciviews.org/recipes/tcltk/TclTk-radiobuttons/
#accessed Feb 10, 2016
#--------------------------
win1 <- tktoplevel()
win1$env$rb1 <- tk2radiobutton(win1)
win1$env$rb2 <- tk2radiobutton(win1)
win1$env$rb3 <- tk2radiobutton(win1)
rbValue <- tclVar("Aldosterone")
tkconfigure(win1$env$rb1, variable = rbValue, value = "Aldosterone")
tkconfigure(win1$env$rb2, variable = rbValue, value = "Cortisol")
tkconfigure(win1$env$rb3, variable = rbValue, value = "Testosterone")
tkgrid(tk2label(win1, text = "Which analyte are you processing?"),
       columnspan = 2, padx = 10, pady = c(15, 5))
tkgrid(tk2label(win1, text = "Aldosterone"), win1$env$rb1,
       padx = 10, pady = c(0, 5))
tkgrid(tk2label(win1,text = "Cortisol"), win1$env$rb2,
       padx = 10, pady = c(0, 5))
tkgrid(tk2label(win1,text = "Testosterone"), win1$env$rb3,
       padx = 10, pady = c(0, 15))

onOK <- function() {
  rbVal <- as.character(tclvalue(rbValue))
  tkdestroy(win1)
}

win1$env$butOK <- tk2button(win1, text = "OK", width = -6, command = onOK)
tkgrid(win1$env$butOK, columnspan = 2, padx = 10, pady = c(5, 15))
tkfocus(win1)
tkwait.window(win1)
#--------------------------

# These are the parameters that one would have to change according to their site 

#set the testcode by test
test.code <- switch(tclvalue(rbValue),
                    "Aldosterone" = "ALD",
                    "Testosterone" = "TES",
                    "Cortisol" = "CORT"
)

#set the LoQ by test
LoQ <- switch(tclvalue(rbValue),
                    "Aldosterone" = "<50",
                    "Testosterone" = "<0.05",
                    "Cortisol" = "<1"
)

#set the MRM names by test
MRM.names <- switch(tclvalue(rbValue),
              "Aldosterone" = c("Aldo 1", "Aldo 2"),
              "Testosterone" = c("Testo 1", "Testo 2"),
              "Cortisol" = c("Cortisol 1", "Cortisol 2")
)

#set the worksheet name for your site
worksheet <- "PAPI"


#choose the flat file to process
tkmessageBox(message="You are about to choose a flat file to process.")
flat.file.path <- tk_choose.files(default = "", caption = "Select File", multi = FALSE, filters = NULL, index = 1)
flat.file.dir <- dirname(flat.file.path)
flat.file.name <- basename(flat.file.path)

#determine which row contains the true header of the data. 
#this will be different for different flat files
#in this case the names of the first two columns are used as the identifier
x <- readLines(flat.file.path)
skip.val <- grep("Sample Name\tSample ID", x, fixed = TRUE) - 1

#if no such row is found, then the wrong file has been chosen
if (length(skip.val)==0){
  problem <- TRUE
} else {
  problem <- FALSE
}

if (!problem){
  #read in the data with the appropriate number of lines skipped.
  my.data <- read.delim(flat.file.path, sep = "\t", strip.white = TRUE, skip = skip.val, header = TRUE, stringsAsFactors = FALSE)
  
  #take care of small SCIEX-related hazard with naming of the Calculated Concentration
  conc.col.num <- grep("Calculated.Concentration..",names(my.data), fixed = TRUE)
  names(my.data)[conc.col.num]<- "Calculated.Concentration"
  
  #pull out the columns of interest
  results <- my.data[,c("Sample.Name", "Analyte.Peak.Name","Calculated.Concentration")]
  names(results) <- c("sampleID", "mrm", "conc")
  
  #handle non numeric results
  quantifiers <- results[grep(MRM.names[1], results$mrm),]
  quantifiers$conc <- as.numeric(quantifiers$conc)
  qualifiers <- results[grep(MRM.names[2], results$mrm),]
  qualifiers$conc <- as.numeric(qualifiers$conc)
  
  #separate out sample types
  standards <- grep("Blank|STD",quantifiers$sampleID)
  qc <- grep("C-", quantifiers$sampleID)
  
  #create a regular expression to identify samples (E followed by 10 digits)
  regexp<-"(^E)([[:digit:]]{10})"
  patients <-grep(pattern=regexp,quantifiers$sampleID)
  output.data <- quantifiers[c(qc,patients),]
  
  #prepare the final data
  num.rows <- length(output.data$sampleID)
  final.output.data <- data.frame(rep(worksheet,num.rows), output.data$sampleID, rep(test.code, num.rows), output.data$conc)
  names(final.output.data) <- c("worksheet","sample","test","conc")
  
  #to put LOQs in, we need to convert to character
  #this assumes that all non numeric results are undetectable
  final.output.data$conc <- as.character(final.output.data$conc)
  final.output.data$conc[is.na(final.output.data$conc)] <- LoQ
  
  #If the data file happens to be empty because you selected the wrong file, abort
  if(nrow(final.output.data)==0){
    tkmessageBox(message="Your flat file contained no patient data. Aborting file output")
  } else if (nrow(final.output.data)>0) {
  
    #create the output directory if it does not exist
    if(!dir.exists(file.path(flat.file.dir, "Processed"))){
      dir.create(file.path(flat.file.dir, "Processed"))
    }
    
    if(!dir.exists(file.path(flat.file.dir, "Raw"))){
      dir.create(file.path(flat.file.dir, "Raw"))
    }
    
    #create a  ISO 8601 compliant timestamp - get rid of spaces and colons
    time.stamp <- gsub(":","", Sys.time(), fixed = TRUE)
    time.stamp <- gsub(" ","T", time.stamp, fixed = TRUE)
    
    #save a copy of the input file
    flat.file.copy.name <- paste(test.code,"_",time.stamp, "_Raw.txt", sep="")
    file.copy(flat.file.path, file.path(flat.file.dir,"Raw", flat.file.copy.name ))
    
    #write the final output file
    final.output.name <-  paste(test.code,"_",time.stamp, ".txt", sep="")
    final.output.path <- file.path(flat.file.dir,"Processed" ,final.output.name)
    write.table(file = final.output.path, final.output.data, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ",")
    
    #check that the file was created as expected
    if(file.exists(final.output.path)){
      tkmessageBox(message="Data successfully processed \n Check Processed directory")
    } else {
      tkmessageBox(message="Your file was not created. There was a problem")
    }
  }
} else {
  tkmessageBox(message="Your file was not created.\nThe chosen file does not have\nthe right format")
}
  
