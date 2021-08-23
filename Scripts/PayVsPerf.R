# Statistics Final Project (FE 541)
# December 7th 2017
# Question (Part 2) - Is there a stronger realtionship between moneny and performance or skill and performance
# Wyatt Marciniak
# Pia Om
# Laramie Regalado

# if(TRUE) blocks used to run chunks of code (named but feel free to explore them, for ease of loading)

if(TRUE) # Install/Load packages and set working directory/admin options
{
  #install.packages("readxl")
  #install.packages("sma")
  #install.packages("reshape")
  #install.packages("ggpubr")
  #library(ggplot2) 
  #install.packages("varhandle")
  library(varhandle) # Use for unfactoring data sets
  library(ggpubr)  # Used for ggqqplot
  library(readxl)  # Used for reading multi-sheet excel sheets
  library(reshape) # Used for data frames graphics (unsued but it would have been cool, for another day...)
  library(broom)   # Used for extrating stats from summary() objects nicely
  setwd("C:/FE541/FinalProject") # Set working Directory
  #options(digits = 5)
}

if(TRUE) # DATA LOADING (ALL Avail)
{
  # Load SuperBowl Data ----------------------------------------------
  superbowl <- read_excel("Sb_winners.xlsx")
  
  # Load Rules data set
  rules <- read_excel("Rules.xlsx")
  
  # Load franchise revenue/year and salary cap/year (+avg)
  rev   <- read_xlsx("revenue_salary_cap.xlsx", sheet = 1); rev <- data.frame(rev)
  rev.rown <- rev[,1]
  rev   <- rev[,-1]; rownames(rev) <- rev.rown
  
  salary <- read_xlsx("revenue_salary_cap.xlsx", sheet = 2); salary <- data.frame(salary)
  salary.rown <- salary[,1]; salary.avg <- salary[,2] 
  salary <- salary[,-1]; salary <- salary[,-1]; rownames(salary) <- salary.rown
  salary <- salary[-length(salary[,1]),]
  
  coln <- colnames(rev); coln <- coln[-1]; coln <- c("49ers",coln)
  colnames(rev) <- coln; colnames(salary) <- coln
  
  
  # Create AFC and NFC categorical lists
  afc_list <- c("Bengals","Bills","Broncos","Chargers","Chiefs","Colts","Dolphins","Jets","Patriots","Raiders","Steelers")
  nfc_list <- c("49ers","Bears","Cardinals","Cowboys","Eagles","Falcons","Giants","Lions","Packers","Rams","Redskins","Saints","Vikings")
  
  # Load first year of data (1968) to create data frames for yearly records -------------
  bulk    <- read_xlsx("Record_data.xlsx", sheet = 41) # Yearly stats
  tnames  <- as.vector(bulk$Team)
  w       <- data.frame(bulk[,2])
  l       <- data.frame(bulk[,3])
  pct     <- data.frame(bulk[,4])
  pf      <- data.frame(bulk[,5])
  pa      <- data.frame(bulk[,6])
  td      <- data.frame(bulk[,7])
  w.h     <- data.frame(bulk[,8])
  l.h     <- data.frame(bulk[,9]) 
  w.r     <- data.frame(bulk[,10])
  l.r     <- data.frame(bulk[,11])
  
  bulk    <- read_xlsx("OffenseData.xlsx", sheet = 41) # Yearly offensive stata
  ppg     <- data.frame(bulk[,2])
  ypg     <- data.frame(bulk[,3])
  ypp     <- data.frame(bulk[,4])
  peny    <- data.frame(bulk[,5])
  
  yr      <- 1976
  yr_list <- c(yr) # Use for all 2016 - 1976 data sets
  
  # Load remaining data into R ----------------------------------------
  for(i in 40:1)
  {
    bulk <- read_xlsx("Record_data.xlsx", sheet = i) 
    w    <- cbind(w,   bulk[,2])
    l    <- cbind(l,   bulk[,3])
    pct  <- cbind(pct, bulk[,4])
    pf   <- cbind(pf,  bulk[,5])
    pa   <- cbind(pa,  bulk[,6])
    td   <- cbind(td,  bulk[,7])
    w.h  <- cbind(w.h, bulk[,8])
    l.h  <- cbind(l.h, bulk[,9])
    w.r  <- cbind(w.r, bulk[,10])
    l.r  <- cbind(l.r, bulk[,11])
    
    bulk <- read_xlsx("OffenseData.xlsx", sheet = i)
    ppg  <- cbind(ppg, bulk[,2])
    ypg  <- cbind(ypg, bulk[,3])
    ypp  <- cbind(ypp, bulk[,4])
    peny <- cbind(peny, bulk[,5])
    
    yr   <- yr+1
    yr_list <- c(yr_list, yr)
  }
  # Set column and row names
  colnames(w)    <- yr_list; rownames(w)    <- tnames
  colnames(l)    <- yr_list; rownames(l)    <- tnames
  colnames(pct)  <- yr_list; rownames(pct)  <- tnames
  colnames(pf)   <- yr_list; rownames(pf)   <- tnames
  colnames(pa)   <- yr_list; rownames(pa)   <- tnames
  colnames(td)   <- yr_list; rownames(td)   <- tnames
  colnames(w.h)  <- yr_list; rownames(w.h)  <- tnames
  colnames(l.h)  <- yr_list; rownames(l.h)  <- tnames
  colnames(w.r)  <- yr_list; rownames(w.r)  <- tnames
  colnames(l.r)  <- yr_list; rownames(l.r)  <- tnames
  colnames(ppg)  <- yr_list; rownames(ppg)  <- tnames
  colnames(ypg)  <- yr_list; rownames(ypg)  <- tnames
  colnames(ypp)  <- yr_list; rownames(ypp)  <- tnames
  colnames(peny) <- yr_list; rownames(peny) <- tnames
  
  rm(bulk) # remove bulk from environment
  
  # Load team leaders in offense and defense by rea data --------------------------------------
  bulk.o    <- read_xlsx("OffDefData.xlsx", sheet = 1)
  bulk.d    <- read_xlsx("OffDefData.xlsx", sheet = 2)
  
  lead.o    <- data.frame(bulk.o[,length(bulk.o)])
  lead.d    <- data.frame(bulk.d[,length(bulk.d)])
  
  for(i in 1:length(bulk.o))
  {
    lead.o   <- cbind(lead.o, bulk.o[,length(bulk.o)-i])
    lead.d   <- cbind(lead.d, bulk.d[,length(bulk.d)-i])
  }
  
  lead.o <- lead.o[,-length(lead.o)]; lead.d <- lead.d[,-length(lead.d)]; # Remove extra column (names)
  colnames(lead.o) <- yr_list; colnames(lead.d) <- yr_list # Name columns
  rownames(lead.o) <- tnames; rownames(lead.d)  <- tnames  # name rows
  rm(bulk.o); rm(bulk.d); # Remove temp variables/data sets
  lead.o[is.na(lead.o)] <- 0
  lead.d[is.na(lead.d)] <- 0
  
  # Calculate the season net points (Points for - Points against) for each team, each year --> create a new data frame
  netpt <- data.frame(tnames)
  for(i in 1:length(pf))
  {
    temp <- c()
    for(j in 1:length(pf[,1]))
    {
      net <- as.numeric(pf[j,i]) - as.numeric(pa[j,i])
      temp <- c(temp, net)
    }
    temp <- data.frame(temp)
    netpt <- cbind(netpt, temp)
  }
  netpt <- netpt[,-1] # Remove extra name column
  colnames(netpt) <- colnames(pf)
  rownames(netpt) <- tnames
  
  rm(temp); rm(i); rm(j); rm(yr); rm(net)
  
  # Transpose data to proper variable/obervation orientation
  w   <- data.frame(t(w));   colnames(w) <- tnames
  l   <- data.frame(t(l));   colnames(l) <- tnames
  pct <- data.frame(t(pct)); colnames(pct) <- tnames
  pf  <- data.frame(t(pf));  colnames(pf) <- tnames
  pa  <- data.frame(t(pa));  colnames(pa) <- tnames
  td  <- data.frame(t(td));  colnames(td) <- tnames
  w.h <- data.frame(t(w.h)); colnames(w.h) <- tnames
  l.h <- data.frame(t(l.h)); colnames(l.h) <- tnames
  w.r <- data.frame(t(w.r)); colnames(w.r) <- tnames
  l.r <- data.frame(t(l.r)); colnames(l.r) <- tnames
  ppg <- data.frame(t(ppg)); colnames(ppg) <- tnames
  ypg <- data.frame(t(ypg)); colnames(ypg) <- tnames
  ypp <- data.frame(t(ypp)); colnames(ypp) <- tnames
  peny   <- data.frame(t(peny));   colnames(peny) <- tnames
  netpt  <- data.frame(t(netpt));  colnames(netpt) <- tnames
  lead.o <- data.frame(t(lead.o)); colnames(lead.o) <- tnames
  lead.d <- data.frame(t(lead.d)); colnames(lead.d) <- tnames
  
  tnames.l <- c("f9ers", tolower(tnames)); tnames.l <- tnames.l[-2]
  
  w <- unfactor(w)
  l <- unfactor(l)
  pct <- unfactor(pct)
  pf <- unfactor(pf)
  pa <- unfactor(pa)
  td <- unfactor(td)
  w.h <- unfactor(w.h)
  l.h <- unfactor(l.h)
  w.r <- unfactor(w.r)
  l.r <- unfactor(w.r)
  ppg <- unfactor(ppg)
  ypg <- unfactor(ypg)
  ypp <- unfactor(ypp)
  peny <- unfactor(peny)
  netpt <- unfactor(netpt)
  lead.o <- unfactor(lead.o)
  lead.d <- unfactor(lead.d)
}

if(TRUE) # Define functions to be used
{
  fiveNumSum    <- function(data) # Crops Summary() into the proper 5 number summary
  {
    temp <- data.frame()
    sum <- summary(data)
    temp <- rbind(temp, as.numeric(sub('.*:','', summary(data)[1])))
    temp <- rbind(temp, as.numeric(sub('.*:','', summary(data)[2])))
    temp <- rbind(temp, as.numeric(sub('.*:','', summary(data)[3])))
    temp <- rbind(temp, as.numeric(sub('.*:','', summary(data)[5])))
    temp <- rbind(temp, as.numeric(sub('.*:','', summary(data)[6])))
    rownames(temp) <- c("Min","1st Quartile","Median","3rd Quartile","Max")
    colnames(temp) <- c("5 number Summary")
    temp
  }
  quick.stats   <- function(data)
  {
    temp <- fiveNumSum(data)
    names5 <- rownames(temp)
    
    temp <- rbind(temp, mean(data))
    temp <- rbind(temp, sd(data))
    
    rownames(temp) <- c(names5, "Mean","Std.dev"); colnames(temp) <- c("Summary")
    temp
  }
  agg.team.data <- function(team) # Creates data table for a team (all variables and observations in 1 place)
  {
    full <- data.frame(w[,team])
    full <- cbind(full, l[,team])
    full <- cbind(full, pf[,team])
    full <- cbind(full, pa[,team])
    full <- cbind(full, netpt[,team])
    full <- cbind(full, td[,team])
    full <- cbind(full, pct[,team])
    full <- cbind(full, w.h[,team])
    full <- cbind(full, l.h[,team])
    full <- cbind(full, w.r[,team])
    full <- cbind(full, l.r[,team])
    full <- cbind(full, peny[,team])
    full <- cbind(full, ppg[,team])
    full <- cbind(full, ypg[,team])
    full <- cbind(full, ypp[,team])
    full <- cbind(full, lead.o[,team])
    full <- cbind(full, lead.d[,team])
    
    params <- c("w","l","pf","pa","netpt","td","pct","w.h","l.h","w.r","l.r","peny","ppg","ypg","ypp","lead.o","lead.d")
    colnames(full) <- c(params)
    full
  }
  cor.team      <- function(team) #Generates a correlation matrix for all 17 paramters for 1 team
  {
    cor.table <- agg.team.data(team)
    cor.matrix <- data.frame(matrix(nrow = 17))
    for(i in 1: length(cor.table))
    {
      temp <- data.frame()
      for(j in 1:length(cor.table))
      {
        temp <- rbind(temp, cor(as.numeric(cor.table[,i]), as.numeric(cor.table[,j])))
      }
      cor.matrix <- cbind(cor.matrix, temp)
    }
    
    params <- c("w","l","pf","pa","netpt","td","pct","w.h","l.h","w.r","l.r","peny","ppg","ypg","ypp","lead.o","lead.d")
    cor.matrix <- cor.matrix[,-1]
    colnames(cor.matrix) <- params; rownames(cor.matrix) <- params
    cor.matrix
    
  }
  agg_data      <- function(data, from, to, omit) # Pool (aggregate) full paramater data into 1 list
  {
    a = length(data[,1]) - (to-from); b = length(data[,1]) - (2016-to) 
    temp <- c()
    for(i in 1: length(data))
    {
      omit.flag = 0;
      for(j in 1:length(omit))
      {
        if(names(data)[i] == omit[j]) {omit.flag = 1;}
      }
      if(omit.flag == 1) { next }
      else { temp <- c(temp, data[a:b,i]) }
    }
    temp
  }
  cor.agg       <- function(data) # Generate a correlation matrix for a full data set (all.data set of all params)
  {
    params <- colnames(data)
    cor.matrix <- data.frame(matrix(ncol = 1))
    for(i in 1: length(data))
    {
      temp <- data.frame()
      for(j in 1:length(data))
      {
        temp <- rbind(temp, cor(as.numeric(data[,i]), as.numeric(data[,j])))
      }
      cor.matrix <- cbind(cor.matrix, temp)
    }
    cor.matrix <- cor.matrix[,-1]
    colnames(cor.matrix) <- params; rownames(cor.matrix) <- params
    cor.matrix
  }
  run.corr      <- function(data, title) # returns a list of 4 data frames (corr table, corr_cat table, number of corr per cat, corr sums) + 2 plots
  {
    levels.c <- c(-1,-0.80,-0.51,-0.10,0,0.10,0.51,0.80,1) # (low,high]
    labels.c <- c("Strong Negative","Negative","Weak Negative","N/A ([-0.10:0.10])", "Weak positive","Positive","Strong Positive")
    
    data.c <- cor.agg(data) # Correlation table (calls to corr.agg)
    
    cnames <- colnames(data.c)
    rnames <- rownames(data.c)
    
    data.cc   <- data.frame(matrix(ncol=1)) ; data.count  <- data.frame(matrix(nrow=1))
    var.count <- data.frame(matrix(ncol=1)) ; table.count <- data.frame()
    sum.data  <- data.frame(matrix(ncol=1)) ; sum.table   <- data.frame(matrix(ncol=1))
    alldata.lst <- c()
      
    # Technical: Iterates through the correlation table to extract data for analyses
    # Layman:    The big loop that does everything important - caution
    for(i in 1: length(data.c))
    {
      cat.col <- c(); sum.col <- c()
      c1 = 0; c2 = 0; c3 = 0; c4 = 0; c5 = 0; c6 = 0; c7 = 0;
      for(j in 1:length(data.c))
      {
        if(i == j) {cat.col <- c(cat.col, "-")}
        else
        {
          sum.col <- c(sum.col, data.c[j,i]); alldata.lst <- c(alldata.lst, data.c[j,i])
          if(data.c[j,i] <= levels.c[2]) {c1 = c1 + 1; cat.col <- c(cat.col, labels.c[1])}
          if(data.c[j,i] >= levels.c[2] & data.c[j,i] < levels.c[3]) {c2 = c2 + 1; cat.col <- c(cat.col, labels.c[2])}
          if(data.c[j,i] >= levels.c[3] & data.c[j,i] < levels.c[4]) {c3 = c3 + 1; cat.col <- c(cat.col, labels.c[3])}
          if(data.c[j,i] >= levels.c[4] & data.c[j,i] < levels.c[5]) {c4 = c4 + 1; cat.col <- c(cat.col, labels.c[4])}
          if(data.c[j,i] >= levels.c[5] & data.c[j,i] < levels.c[6]) {c4 = c4 + 1; cat.col <- c(cat.col, labels.c[4])}
          if(data.c[j,i] >= levels.c[6] & data.c[j,i] < levels.c[7]) {c5 = c5 + 1; cat.col <- c(cat.col, labels.c[5])}
          if(data.c[j,i] >= levels.c[7] & data.c[j,i] < levels.c[8]) {c6 = c6 + 1; cat.col <- c(cat.col, labels.c[6])}
          if(data.c[j,i] >= levels.c[8] & data.c[j,i] <= levels.c[9]) {c7 = c7 + 1; cat.col <- c(cat.col, labels.c[7])}
        }
      }
      var.count   <- cbind(var.count, c(c1,c2,c3,c4,c5,c6,c7))   # Counts per variable
      table.count <- rbind(table.count, c(c1,c2,c3,c4,c5,c6,c7)) # Counts total
      data.cc     <- cbind(data.cc, cat.col)                     # Cat. matrix construction
      sum.data    <- cbind(sum.data, sum.col)                    # Data to calculate Summaries
    }
    
    # Get full data counts 
    #table.count <- table.count[-1,] # Get full type counts for the table
    for(i in 1:length(table.count)) { data.count <- rbind(data.count, sum(table.count[,i])) }
    
    data.count <- data.frame(data.count[-1,]); data.cc <- data.cc[,-1]; var.count <- var.count[,-1] # Remove empty row/col
    
    # Get counts as percentage of total
    table.perc <- c(); sum.t = sum(data.count[,1]); print(sum.t)
    for(i in 1:length(data.count[,1])) { table.perc <- c(table.perc, (data.count[i,1]/sum.t)*100) }
    
    data.count <- cbind(data.count, table.perc, var.count)
    
    # Get aggregate summary data 
    alldata.sum <- quick.stats(alldata.lst)

    sum.table <- cbind(sum.table, alldata.sum)
    
    # Get paramter summary data
    sum.data <- sum.data[,-1]
    for(i in 1:length(sum.data))
    {
      sum.table <- cbind(sum.table, quick.stats(sum.data[,i]))
    }
    sum.table <- sum.table[,-1]
    colnames(sum.table) <- c("All Correlations",cnames)
    
    colnames(data.cc)    <- cnames ; rownames(data.cc) <- rnames;
    colnames(data.count) <- c("Total Count", "Percent of Total", cnames); rownames(data.count) <- labels.c
    
    # Account for self~self correlation counts of individual variables sums
    for(i in 2:length(data.count))
    {
      for(j in 1:length(data.count[,i]))
      {
        if(i == j) { data.count[j,i] = data.count[j,i] - 1}
      }
    }
    
    # Combine 3 final data frames into a vector and return to user
    data.lst <- c()
    data.lst[[1]] <- data.c; data.lst[[2]] <- data.cc; data.lst[[3]] <- data.count; data.lst[[4]] <- sum.table
    
    # Plotting data ---------------------------------------------------------------------
    par(mfrow = c(1,1))
    hist(alldata.lst, main = paste("Histogram of ",title," Correlation Data"), xlab = "Correlations")
    boxplot(data.c, main = paste("Boxplot(s) of ",title," Correlations"))
    
    data.lst
    
  } # End of run.corr()
  reg.agg       <- function(data) # Generates a regression model matrix for an entire data set
  {
    params <- colnames(data)
    reg.matrix <- data.frame(matrix(ncol=1))
    
    for(i in 1: length(data))
    {
      temp <- data.frame()
      for(j in 1:length(data))
      {
        model <- lm(as.numeric(data[,j])~as.numeric(data[,i])) # i pos is predictor, j pos is repsone (obs)
        sum   <- summary(model)
        stats <- glance(sum)
        r     <- stats$r.squared
        temp <- rbind(temp, r)
      }
      reg.matrix <- cbind(reg.matrix, temp)
    }
    reg.matrix <- reg.matrix[,-1]
    colnames(reg.matrix) <- params; rownames(reg.matrix) <- params
    reg.matrix
  }
  run.reg       <- function(data, title) # returns a list of 4 data frames (reg ('r.sq') table, reg_cat table, number of reg per cat, reg sums) + 2 plots
  {
    levels.r <- c(0,0.2,0.4,0.6,0.8,0.9) # (low,high]
    labels.r <- c("Negative","Weak","Semi-Weak","Average","Semi-strong","Strong","Very Strong")
    
    data.r <- reg.agg(data) # Regression table (calls to reg.agg)
    
    cnames <- colnames(data.r)
    print(cnames)
    rnames <- rownames(data.r)
    print(rnames)
    
    data.rc   <- data.frame(matrix(ncol=1)) ; data.count  <- data.frame(matrix(nrow=1))
    var.count <- data.frame(matrix(ncol=1)) ; table.count <- data.frame()
    sum.data  <- data.frame(matrix(ncol=1)) ; sum.table   <- data.frame(matrix(ncol=1))
    alldata.lst <- c()
    
    # Technical: Iterates through the regression table to extract data for analyses
    # Layman:    The big loop that does everything important - caution
    for(i in 1: length(data.r))
    {
      cat.col <- c(); sum.col <- c()
      c1 = 0; c2 = 0; c3 = 0; c4 = 0; c5 = 0; c6 = 0; c7 = 0;
      for(j in 1:length(data.r))
      {
        if(i == j) {cat.col <- c(cat.col, "-")}
        else
        {
          sum.col <- c(sum.col, data.r[j,i]); alldata.lst <- c(alldata.lst, data.r[j,i])
          if(data.r[j,i] <= levels.r[1]) {c1 = c1 + 1; cat.col <- c(cat.col, labels.r[1])}
          if(data.r[j,i] > levels.r[1] & data.r[j,i] < levels.r[2]) {c2 = c2 + 1; cat.col  <- c(cat.col, labels.r[2])}
          if(data.r[j,i] >= levels.r[2] & data.r[j,i] < levels.r[3]) {c3 = c3 + 1; cat.col  <- c(cat.col, labels.r[3])}
          if(data.r[j,i] >= levels.r[3] & data.r[j,i] < levels.r[4]) {c4 = c4 + 1; cat.col  <- c(cat.col, labels.r[4])}
          if(data.r[j,i] >= levels.r[4] & data.r[j,i] < levels.r[5]) {c5 = c5 + 1; cat.col  <- c(cat.col, labels.r[5])}
          if(data.r[j,i] >= levels.r[5] & data.r[j,i] <= levels.r[6]) {c6 = c6 + 1; cat.col <- c(cat.col, labels.r[6])}
          if(data.r[j,i] >  levels.r[6]) {c7 = c7 + 1; cat.col <- c(cat.col, labels.r[7])}
        }
      }
      var.count   <- cbind(var.count, c(c1,c2,c3,c4,c5,c6,c7))      # Counts per variable
      table.count <- rbind(table.count, c(c1,c2,c3,c4,c5,c6,c7))    # Counts total
      data.rc     <- cbind(data.rc, cat.col)                     # Cat. matrix construction
      sum.data    <- cbind(sum.data, sum.col)                    # Data to calculate Summaries
    }
    
    # Get full data counts 
    #table.count <- table.count[-1,] # Get full type counts for the table
    for(i in 1:length(table.count)) { data.count <- rbind(data.count, sum(table.count[,i])) }
    
    data.count <- data.frame(data.count[-1,]); data.rc <- data.rc[,-1]; var.count <- var.count[,-1] # Remove empty row/col
    
    # Get counts as percentage of total
    table.perc <- c(); sum.t = sum(data.count[,1]); print(sum.t)
    for(i in 1:length(data.count[,1])) { table.perc <- c(table.perc, (data.count[i,1]/sum.t)*100) }
    
    data.count <- cbind(data.count, table.perc, var.count)
    
    # Get aggregate summary data 
    alldata.sum <- quick.stats(alldata.lst)
    
    sum.table <- cbind(sum.table, alldata.sum)
    
    # Get paramter summary data
    sum.data <- sum.data[,-1]
    for(i in 1:length(sum.data))
    {
      sum.table <- cbind(sum.table, quick.stats(sum.data[,i]))
    }
    sum.table <- sum.table[,-1]
    colnames(sum.table) <- c("All Regressions",cnames)
    
    print(rnames)
    print(data.rc)
    print(labels.r)
    print(data.count)
    
    colnames(data.rc)    <- cnames ; rownames(data.rc) <- rnames;
    colnames(data.count) <- c("Total Count", "Percent of Total", cnames); rownames(data.count) <- labels.r
    
    # Account for self~self regression counts of individual variables sums
    for(i in 2:length(data.count))
    {
      for(j in 1:length(data.count[,i]))
      {
        if(i == j) { data.count[j,i] = data.count[j,i] - 1}
      }
    }
    
    # Combine 3 final data frames into a vector and return to user
    data.lst <- c()
    data.lst[[1]] <- data.r; data.lst[[2]] <- data.rc; data.lst[[3]] <- data.count; data.lst[[4]] <- sum.table
    
    # Plotting data ---------------------------------------------------------------------
    par(mfrow = c(1,1))
    hist(alldata.lst, main = paste("Histogram of ",title," Regression (r.sq) Data"), xlab = "R-squared")
    boxplot(data.r, main = paste("Boxplot(s) of ",title," Regressions (r.sq)"))
    
    data.lst
  } # End of rin.reg()
}

# Source Start --------------------------------------------------------------------------------------------------------------------------------------

#### Set univers of variables to analyze
obs    <- c("l","netpt","pct","peny","ppg","td","w","ypg","ypp")
pred.l <- c("lead.o","lead.d")
pred.s <- c("salary")
pred.r <- c("rev")
omit   <- c("Chiefs","Rams")


#### PART 1: Get data sets/corr + reg data for predictor subsets (based on time frames of data)
obs.full    <- data.frame(matrix(ncol = 1))
pred.full   <- data.frame(matrix(ncol = 1)) 
salary.full <- data.frame(matrix(ncol = 1)) 
rev.full    <- data.frame(matrix(ncol = 1)) 


# Get full (lead.o and lead.d) data sets for all (1976 - 2016)
start = 1976; end = 2016

for(i in 1:length(obs))
{
  obs.full  <- cbind(obs.full, agg_data(get(obs[i]), start, end, omit))
}
for(i in 1:length(pred.l))
{
  pred.full <- cbind(pred.full, agg_data(get(pred.l[i]), start, end, omit))
}

obs.full <- obs.full[,-1]; pred.full <- pred.full[,-1]
colnames(obs.full) <- c(obs); colnames(pred.full) <- c(pred.l)

full.data <- cbind(obs.full, pred.full)


# Set salary and revenue specific parameters (time)
salary.start = 2011; salary.end = 2016
rev.start = 2001; rev.end = 2016

salary.data.full <- data.frame(matrix(ncol = 1))
rev.data.full    <- data.frame(matrix(ncol = 1))

# Get full salary data set (2011 - 2016)
for(i in 1:length(pred.s))
{
  salary.full  <- cbind(salary.full, agg_data(get(pred.s[i]), salary.start, salary.end, omit))
}
for(i in 1:length(obs))
{
  salary.data.full <- cbind(salary.data.full, agg_data(get(obs[i]), salary.start, salary.end, omit))
}

salary.full <-  salary.full[,-1]; salary.data.full <- salary.data.full[,-1]
salary.data.full <- cbind(salary.data.full, salary.full)
colnames(salary.data.full) <- c(obs,"salary") # Salary in millions

# Get full revenue data set (2001 - 2016)
for(i in 1:length(pred.r))
{
  rev.full    <- cbind(rev.full, agg_data(get(pred.r[i]), rev.start, rev.end, omit))
}
for(i in 1:length(obs))
{
  rev.data.full <- cbind(rev.data.full, agg_data(get(obs[i]), rev.start, rev.end, omit))
}

rev.full <- rev.full[,-1]; rev.data.full <- rev.data.full[,-1]
rev.data.full <- cbind(rev.data.full, rev.full)
colnames(rev.data.full) <- c(obs,"rev") # rev in millions

# Generate Correlation Analyses (Corr matrix, cat matrix, counts, summaries)

# 1976 - 2016 (lead.o, lead.d)
temp <- run.corr(full.data, "Off/Def Leader")
cor.l.c <- data.frame(temp[1]); cor.l.cc.c <- data.frame(temp[2]); cor.l.count.c <- data.frame(temp[3]); cor.l.sum.c <- data.frame(temp[4])

temp <- run.reg(full.data, "Off/Def Leader")
cor.l.r <- data.frame(temp[1]); cor.l.cc.r <- data.frame(temp[2]); cor.l.count.r <- data.frame(temp[3]); cor.l.sum.r <- data.frame(temp[4])


# 2001 - 2016 (revenue)
temp <- run.corr(rev.data.full, "Franchise revenue")
cor.r.c.c <- data.frame(temp[1]); cor.r.cc.c <- data.frame(temp[2]); cor.r.count.c <- data.frame(temp[3]); cor.r.sum.c <- data.frame(temp[4])

temp <- run.reg(rev.data.full, "Franchise revenue")
cor.r.c.r <- data.frame(temp[1]); cor.r.cc.r <- data.frame(temp[2]); cor.r.count.r <- data.frame(temp[3]); cor.r.sum.r <- data.frame(temp[4])


# 2011 - 2016 (salary)
temp <- run.corr(salary.data.full, "Team Salary Cap")
cor.s.c.c <- data.frame(temp[1]); cor.s.cc.c <- data.frame(temp[2]); cor.s.count.c <- data.frame(temp[3]); cor.s.sum.c <- data.frame(temp[4])

temp <- run.reg(salary.data.full, "Team Salary Cap")
cor.s.c.r <- data.frame(temp[1]); cor.s.cc.r <- data.frame(temp[2]); cor.s.count.r <- data.frame(temp[3]); cor.s.sum.r <- data.frame(temp[4])


#### PART 2: Adjust to same periods, run analysis using full universe (pred + obs)
# Generate time-adjusted full data model (2011 - 2016, all predictors and obs)
start = 2011; end = 2016; combine <- data.frame(matrix(ncol = 1))
all.variables <- c(obs, pred.l, pred.r, pred.s)
all.pred <- c(pred.l, pred.r, pred.s)
for(i in 1:length(all.variables))
{
  combine <- cbind(combine, agg_data(get(all.variables[i]), start, end, omit))
}

combine <- combine[,-1]; colnames(combine) <- all.variables

# DO: 6 year full data correlation
temp <- run.corr(combine, "Full Data (6yrs)")
combine.c <- data.frame(temp[1]); combine.cc.c <- data.frame(temp[2]); combine.count.c <- data.frame(temp[3]); combine.sum.c <- data.frame(temp[4])

combine.comp.c <- combine.count.c[,c(1,2,12:15)]; rownames(combine.comp.c) <- rownames(combine.count.c)

# DO: 6 year full data regressions
temp <- run.reg(combine, "Full Data (6yrs)")
combine.r <- data.frame(temp[1]); combine.cc.r <- data.frame(temp[2]); combine.count.r <- data.frame(temp[3]); combine.sum.r <- data.frame(temp[4])

combine.comp.r <- combine.count.c[,c(1,2,12:15)]; rownames(combine.comp.r) <- rownames(combine.count.r)

# Plot distribution of predictors + obs
if(TRUE)
{
  par(mfrow = c(2,2))
  for(i in 13:10)
  {
    hist(combine[,i], main = paste("Predictor: ",names(combine)[i]))
  }
  par(mfrow = c(3,3))
  for(i in 9:1)
  {
    hist(combine[,i], main = paste("Response: ",names(combine)[i]))
  }
 
}

# END OF THIS SOURCE CODE
