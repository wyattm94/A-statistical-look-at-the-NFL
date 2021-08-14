# Statistics Final Project (FE 541)
# December 7th 2017
# Question (Part 3) - Is there a "Home field Advantage" in the NFL
# Wyatt Marciniak
# Pia Om
# Laramie Regalado

# if(TRUE) blocks used to run chunks of code (named but feel free to explore them, for ease of loading)

if(TRUE) # Install/Load packages and set working directory
{
  #install.packages("readxl")
  #install.packages("sma")
  #install.packages("reshape")
  #install.packages("ggpubr")
  library(ggpubr)  # Used for ggqqplot
  library(readxl)  # Used for reading multi-sheet excel sheets
  library(reshape) # Used for data frames graphics (unsued but it would have been cool, for another day...)
  library(broom)   # Used for extrating stats from summary() objects nicely
  setwd("C:/FE541/FinalProject") # Set working Directory
  options(digits = 5)
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
  simple_reg    <- function(datax, datay, team, graph, title, xname, yname) # Generates regression model, (optionally) plots data and regression line, returns regression summary
  {
    x <- as.numeric(datax[which(datax$Team == team),])
    y <- as.numeric(datay[which(datay$Team == team),])
    fit <- lm(y~x)
    if(graph == 1)
    {
      par(mfrow = c(1,1))
      plot(x,y, main = title, xlab = xname, ylab = yname)
      abline(fit, col = "red")
      result <- summary(fit)
      res    <- result$residuals
      par(mfrow = c(2,2))
      plot(res, main = team)
      abline(0,0, col = "red")
      qqnorm(res, main = team)
      hist(res, main = team)
      boxplot(res, main = team)
      print(result)
    }
    else
    {
      result <- summary(fit)
    }
    result
  }
  full_reg      <- function(datax, datay, title, xname, yname)
  {
    scrub_x <- data.frame(datax[,-1])
    scrub_y <- data.frame(datay[,-1])
    full_x  <- c(as.numeric(scrub_x[1,]))
    full_y  <- c(as.numeric(scrub_y[1,]))
    for(i in 2:length(scrub_x[,1]))
    {
      full_x <- c(full_x, as.numeric(scrub_x[i,]))
      full_y <- c(full_y, as.numeric(scrub_y[i,]))
    }
    
    data <- data.frame(full_x, full_y)
    
    par(mfrow = c(1,1))
    plot(data[,1], data[,2], main = title, xlab = xname, ylab = yname)
    fit <- lm(data[,2]~data[,1])
    abline(fit, col = "red")
    hist(data[,1])
    hist(data[,2])
    s <- summary(fit)
    s
  }
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
  agg_data      <- function(data, from, to) # Pool (aggregate) full paramater data into 1 list
  {
    a = length(data[,1]) - (to-from)+1; b = length(data[,1]) - (2016-to) 
    temp <- c()
    for(i in 1: length(data))
    {
      temp <- c(temp, c(data[a:b,i]))
    }
    temp
  }
}

if(TRUE) # Run t-tests by team
{
  hfa.ttest.team <- function(type, print)
  {
    new.table <- data.frame()
    for(i in 1:length(tnames))
    {
      data.h <- w.h[,tnames[i]]
      data.r <- w.r[,tnames[i]]
      test <- t.test(data.h, data.r, alternative = type)
      
      test.ci   <- data.frame(test$conf.int)
      test.p    <- data.frame(test$p.value)
      
      test.mean <- data.frame(test$estimate)
      test.mh   <- test.mean[1,1]
      test.mr   <- test.mean[2,1]
      
      test.medh <- median(data.h)
      test.medr <- median(data.r)
      
      test.sdh  <- sd(data.h)
      test.sdr  <- sd(data.r)
      
      new.table <- rbind(new.table, c(test.ci[1,1], test.ci[2,1], test.p[1,1], test.mh, test.mr, test.medh, test.medr, test.sdh, test.sdr))
    }
    colnames(new.table) <- c("CI.low","CI.high","P.value","Mean (home)","Mean (rode)","Median (home)","Median (rode)","Std.dev (home)","Std.dev (rode)")
    
    if(print == TRUE)
    {
      plot(new.table$P.value, main = paste("Team T-test P-value (",type,")"), ylab = "T-test P-value", xlab = "", xaxt="n")
      axis(1, at=1:24, labels=tnames) 
      abline(0.05,0, col = "red")
      abline(0.025,0, col = "green")
    }
    
    count.sig = 0; count.ssig = 0;
    for(i in 1:length(tnames))
    {
      if(new.table$P.value[i] < 0.05)
      {
        count.sig = count.sig + 1
      }
      if(new.table$P.value[i] < 0.025)
      {
        count.ssig = count.ssig + 1
      }
    }
    
    new.table <- rbind(new.table, c("-","-",paste(count.sig," / 24"),"-","-","-"))
    new.table <- rbind(new.table, c("-","-",paste(count.ssig," / 24"),"-","-","-"))
    rownames(new.table) <- c(tnames, "Sig. at 0.05","Sig. at 0.025")
    
    new.table
  }
  
  par(mfrow = c(1,1))
  test.team.two <- hfa.ttest.team(type = "two.sided", print = TRUE)
  test.team.gr  <- hfa.ttest.team(type = "greater", print = TRUE)
  test.team.lr  <- hfa.ttest.team(type = "less", print = TRUE)
}

if(TRUE) # Run full data t-tests (time intervals)
{
  hfa.ttest.full <- function(type, print)
  {
    timespan  <- c(5,10,15,20,25,30,35,41)
    new.table <- data.frame()
    
    for(i in 1:length(timespan))
    {
      end   = 2016
      start = 2016 - (timespan[i])
      
      print(paste("Running for ",start+1," - ",end," (Years = ", end - start,")"))
      
      data.h    <- agg_data(w.h, start, end);
      data.r    <- agg_data(w.r, start, end);
      
      test      <- t.test(data.h, data.r, alternative = type)
      
      test.ci   <- data.frame(test$conf.int)
      test.p    <- data.frame(test$p.value)
      
      test.mean <- data.frame(test$estimate)
      test.mh   <- test.mean[1,1]
      test.mr   <- test.mean[2,1]
      
      test.medh <- median(data.h)
      test.medr <- median(data.r)
      
      test.sdh  <- sd(data.h)
      test.sdr  <- sd(data.r)
      
      new.table <- rbind(new.table, c(test.ci[1,1], test.ci[2,1], test.p[1,1], test.mh, test.mr, test.medh, test.medr, test.sdh, test.sdr))
      
      colnames(new.table) <- c("CI.low","CI.high","P.value","Mean (home)","Mean (rode)","Median (home)","Median (rode)","Std.dev (home)","Std.dev (rode)")
    }
    
    
    if(print == TRUE)
    {
      plot(new.table$P.value, main = paste("Agg. T-test P-value (",type,")"), ylab = "T-test P-value", xlab = "", xaxt="n")
      abline(0.05,0, col = "red")
      abline(0.025,0, col = "green")
    }
    
    rownames(new.table) <- c("Last 5 years","Last 10 years","Last 15 years","Last 20 years","Last 25 years","Last 30 years","Last 35 years","Last 41 years")
    new.table
  }
  
  par(mfrow = c(1,1))
  test.full.two <- hfa.ttest.full(type = "two.sided", print = TRUE)
  test.full.gr  <- hfa.ttest.full(type = "greater", print = TRUE)
  test.full.lr  <- hfa.ttest.full(type = "less", print = TRUE)
}



