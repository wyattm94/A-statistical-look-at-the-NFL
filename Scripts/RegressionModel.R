# Statistics Final Project (FE 541)
# December 7th 2017
# Question (Part 1) - Full Data Analysis and buidling a multivariate regression model for winning percentage
# Wyatt Marciniak
# Pia Om
# Laramie Regalado

if(TRUE)
{
  #install.packages("readxl")
  #install.packages("sma")
  #install.packages("reshape")
  #install.packages("ggpubr")
  library(varhandle) # Use for unfactoring data sets
  library(ggpubr)    # Used for ggqqplot
  library(readxl)
  library(reshape)
  library(broom)
  library(leaps)
  setwd("C:/FE541/FinalProject") # Set working Directory
  #options("scipen"= 0)
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
  plot_res      <- function(data, name)
  {
    par(mfrow = c(2,2))
    plot(data, main = name)
    abline(0,0, col = "red")
    hist(data, main = name)
    boxplot(data, main = name)
    qqnorm(data, main = name)
  }
  agg_data      <- function(data) # Pool (aggregate) full paramater data into 1 list
  {
    temp <- c()
    for(i in 1:length(data))
    {
      for(j in 1:length(data[,i]))
      {
        temp <- c(temp, data[j,i])
      }
    }
    temp
  }
  agg.team.data <- function(team) # Pool data of a certain team into 1 data frame
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
  cor.team      <- function(team) # Generates a correlation matrix for all 17 paramters for 1 team
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
  cor.agg       <- function(data) # Generate a correlation matrix for a full data set (all.data set of all params)
  {
    cor.matrix <- data.frame(matrix(nrow = 17))
    for(i in 1: length(data))
    {
      temp <- data.frame()
      for(j in 1:length(data))
      {
        temp <- rbind(temp, cor(as.numeric(data[,i]), as.numeric(data[,j])))
      }
      cor.matrix <- cbind(cor.matrix, temp)
    }
    
    params <- c("l","l.h","l.r","lead.o","lead.d","netpt","pa","pct","peny","pf","ppg","td","w","w.h","w.r","ypg","ypp")
    cor.matrix <- cor.matrix[,-1]
    colnames(cor.matrix) <- params; rownames(cor.matrix) <- params
    cor.matrix
  }
  reg.matrix    <- function(data) # Generates a regression model matrix for an entire data set
  {
    reg.matrix <- data.frame(matrix(nrow=17))
    
    for(i in 1: length(data))
    {
      temp <- data.frame()
      for(j in 1:length(data))
      {
        model <- lm(as.numeric(data[,i])~as.numeric(data[,j]))
        sum   <- summary(model)
        stats <- glance(sum)
        r     <- stats$r.squared
        temp <- rbind(temp, r)
      }
      reg.matrix <- cbind(reg.matrix, temp)
    }
    
    reg.matrix
  }
  check_cln     <- function(data, print) # Identifies predictors to check for colinearity
  {
    c     <- data.frame(diag(solve(cor(data), tol = 1e-20))) # Get matrix
    c_sum <- fiveNumSum(c) # Get summary of output
    c_lim <- c_sum[4,1] # Threshold for colinnaearity observations (3rd Q or above)
    
    obs   <- data.frame(); temp_name <- c()
    for(i in 1:length(c[,1]))
    {
      if(c[i,1] >= c_lim)
      {
        temp_name <- c(temp_name, rownames(c)[i])
        temp_val  <- c[i,1]
        obs       <- rbind(obs, temp_val)
      }
    }
    colnames(obs) <- c("Check")
    rownames(obs) <- c(temp_name)
    
    if(print == TRUE)
    {
      print("MAtrix:"); print(c)
      print(paste("Threshold: ", c_lim))
      print(obs)
    }
    
    analyze_cln(data, obs, print)
  }
  analyze_cln   <- function(data, cln, print) # Checks predictors most correlated partner (from watch list, check_cln())
  {
    rows <- rownames(cln)
    ctable <- data.frame()
    
    for(i in 1:length(cln[,1]))
    {
      obj  <- as.character(rownames(cln)[i])
      temp <- cor(data[,obj], data)
      ctable <- rbind(ctable, temp)
    }
    maxval_lst <- c()
    maxname_lst <- c()
    
    for(i in 1:length(ctable[,1]))
    {
      maxval = 0; maxname = "" #Max holder
      for(j in 1:length(ctable[i,]))
      {
        if(ctable[i,j] > maxval)
        {
          if(colnames(ctable)[j] == rows[i])
          {
            next
          }
          maxval = ctable[i,j]
          maxname = as.character(colnames(ctable)[j])
        }
      }
      maxval_lst <- c(maxval_lst, maxval)
      maxname_lst <- c(maxname_lst, maxname)
    }
    # Construct list...
    analysis <- data.frame(maxname_lst, maxval_lst)
    colnames(analysis) <- c("Most Correlated with", "Correlation")
    rownames(analysis) <- c(rows)
    
    if(print == TRUE)
    {
      print(ctable)
    }
    analysis
  }
  track_cln     <- function(data) # Returns relevant stats on colinearity to be tracked
  {
    col.matrix <- data.frame(diag(solve(cor(data), tol = 1e-20)))
    col.5ns    <- fiveNumSum(col.matrix)
    threshold  <- col.5ns[4,1]
    above_med  <- ((threshold-col.5ns[3,1])/col.5ns[3,1])*100
    count = 0
    for(i in 1:length(col.matrix[,1]))
    {
      if(col.matrix[i,1] >= threshold) {count = count + 1}
    }
    col.stats <- c(threshold, above_med, count)
    col.stats
  }
  all.poss.regs <- function(formula, data, best=1,Cp.plot=T,text.cex=0.8, dp=3, cv.rep=10, nvmax=20)
  {
    # calculates model goodness statistics for All Possible Regression due to Alan Lee
    
    X<-model.matrix(formula, data)
    y= model.response(model.frame(formula, data))
    n<-dim(X)[1]
    selection.stuff<-summary(regsubsets(formula, data, nbest=best, nvmax=nvmax))
    Rsq<-selection.stuff$rsq
    rssp<-selection.stuff$rss
    p<- apply(selection.stuff$which, 1, sum)
    sigma2<-rssp/(n-p)
    adjRsq<- selection.stuff$adjr2
    Cp<-selection.stuff$cp
    AIC<-rssp/sigma2[length(p)] + 2*p
    BIC<-rssp/sigma2[length(p)] + log(n)*p
    nmod<-dim(selection.stuff$which)[1]
    k<-dim(selection.stuff$which)[2]
    
    
    pred.error<-numeric(10)
    CV.mat<-matrix(0, nmod,cv.rep)
    m<-n%/%10
    
    for(k in 1:cv.rep){
      
      # randomise order
      rand.order<-order(runif(n))
      
      yr<-y[rand.order]
      Xr<-X[rand.order,]
      
      for(j in 1:nmod){
        sample<-1:m
        for(i in 1:10){
          use.cols = selection.stuff$which[j,]
          use.cols[1]=F
          use.mat<-as.matrix(Xr[-sample,use.cols])
          colnames(use.mat) = colnames(X)[use.cols]
          test.mat<-as.matrix(Xr[sample,use.cols])
          colnames(test.mat) = colnames(use.mat)
          use.data<-data.frame(y=yr[-sample], use.mat)
          new.data<-data.frame(test.mat)
          
          fit<-lm(y~., data=use.data)
          pred.error[i]<-sum((yr[sample]-predict(fit, new.data))^2)
          sample<-sample + m
        }
        CV.mat[j,k]<-mean(pred.error)
      }}
    
    CV=apply(CV.mat, 1,mean)
    result<-round(cbind(rssp,sigma2,adjRsq,Cp,AIC,BIC,CV, selection.stuff$which[,-1]),dp)
    plot(p-1,Cp,xlab="Number of variables",ylab="Cp",main="Cp Plot")
    textvec<-character(length(p))
    for(i in 1:length(p))textvec[i]<-paste((1:(max(p)-1))[selection.stuff$which[i,]],collapse=",")
    text(p-1,Cp,textvec,pos=3, cex=text.cex)
    lines(p-1,p,lty=2)
    result
  }
  run.corr      <- function(data) # returns a list of 3 data frames (corr table, corr_cat table, number of corr per cat)
  {
    levels.c <- c(-1,-0.80,-0.51,-0.10,0,0.10,0.51,0.80,1) # (low,high]
    labels.c <- c("Strong Negative","Negative","Weak Negative","N/A ([-0.10:0.10])", "Weak positive","Positive","Strong Positive")
    
    data.c <- cor.agg(data) # Correlation table (calls to corr.agg)
    
    cnames <- colnames(data.c)
    rnames <- rownames(data.c)
    
    data.cc <- data.frame(matrix(ncol=1)); data.count <- data.frame(matrix(nrow=1)); 
    var.count <- data.frame(matrix(ncol=1)); table.count <- data.frame(matrix(ncol=7))
    for(i in 1: length(data.c))
    {
      cat.col <- c()
      c1 = 0; c2 = 0; c3 = 0; c4 = 0; c5 = 0; c6 = 0; c7 = 0;
      for(j in 1:length(data.c))
      {
        if(i == j) {cat.col <- c(cat.col, "-")}
        else
        {
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
    }
    
    # Organize and Apply names
    table.count <- table.count[-1,] # Get full type counts for the table
    for(i in 1:length(table.count))
    {
      data.count <- rbind(data.count, sum(table.count[,i]))
    }
    
    data.count <- data.frame(data.count[-1,]); data.cc <- data.cc[,-1]; var.count <- var.count[,-1] # Remove empty row/col
    
    table.perc <- c(); sum.t = sum(data.count[,1])
    for(i in 1:length(data.count[,1]))
    {
      table.perc <- c(table.perc, (data.count[i,1]/sum.t)*100)
    }
    
    data.count <- cbind(data.count, table.perc, var.count)
    
    colnames(data.cc)    <- cnames                                  ; rownames(data.cc) <- rnames;
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
    data.lst[[1]] <- data.c; data.lst[[2]] <- data.cc; data.lst[[3]] <- data.count
    data.lst
  }
}

# SOURCE START -------------------------------------------------------------------------------------------------------------------------------

# (1) FIRST ANALYSIS OF DATA:

if(TRUE) # Calculate full team data sets and correlation tables (optional, add regressions here)
{
  # Full team data for regressions (per team)
  f9ers     <- agg.team.data("49ers")
  bears     <- agg.team.data("Bears")
  bengals   <- agg.team.data("Bengals")
  bills     <- agg.team.data("Bills")
  broncos   <- agg.team.data("Broncos")
  cardinals <- agg.team.data("Cardinals")
  chargers  <- agg.team.data("Chargers")
  chiefs    <- agg.team.data("Chiefs")
  colts     <- agg.team.data("Colts")
  cowboys   <- agg.team.data("Cowboys")
  dolphins  <- agg.team.data("Dolphins")
  eagles    <- agg.team.data("Eagles")
  falcons   <- agg.team.data("Falcons")
  giants    <- agg.team.data("Giants")
  jets      <- agg.team.data("Jets")
  lions     <- agg.team.data("Lions")
  packers   <- agg.team.data("Packers")
  patriots  <- agg.team.data("Patriots")
  raiders   <- agg.team.data("Raiders")
  rams      <- agg.team.data("Rams")
  redskins  <- agg.team.data("Redskins")
  saints    <- agg.team.data("Saints")
  steelers  <- agg.team.data("Steelers")
  vikings   <- agg.team.data("Vikings")
  
  # Team correlation tables
  f9ers.c    <- cor.team("49ers")
  bears.c    <- cor.team("Bears")
  bengals.c  <- cor.team("Bengals")
  bills.c    <- cor.team("Bills")
  broncos.c  <- cor.team("Broncos")
  cardinals.c<- cor.team("Cardinals")
  chargers.c <- cor.team("Chargers")
  chiefs.c   <- cor.team("Chiefs")
  colts.c    <- cor.team("Colts")
  cowboys.c  <- cor.team("Cowboys")
  dolphins.c <- cor.team("Dolphins")
  eagles.c   <- cor.team("Eagles")
  falcons.c  <- cor.team("Falcons")
  giants.c   <- cor.team("Giants")
  jets.c     <- cor.team("Jets")
  lions.c    <- cor.team("Lions")
  packers.c  <- cor.team("Packers")
  patriots.c <- cor.team("Patriots")
  raiders.c  <- cor.team("Raiders")
  rams.c     <- cor.team("Rams")
  redskins.c <- cor.team("Redskins")
  saints.c   <- cor.team("Saints")
  steelers.c <- cor.team("Steelers")
  vikings.c  <- cor.team("Vikings")
}

if(TRUE) # Creates full (aggregate) data sets of a correlation table, category table, counts, and regression analysis
{
  # Create an analysis for the full data set
  p.lst <- c("l","l.h","l.r","lead.o","lead.d","netpt","pa","pct","peny","pf","ppg","td","w","w.h","w.r","ypg","ypp")
  all.data <- data.frame(matrix(nrow = 984))
  
  # Generate a full data table (raw)
  for(i in 1:17)
  {
    all.data <- cbind(all.data, agg_data(get(p.lst[i])))
  }
  all.data <- all.data[,-1]
  colnames(all.data) <- p.lst
  
  # Generate a correlation table, catogorized table and counts of the categories
  
  
  x <- run.corr(all.data)
  all.data.c <- data.frame(x[1]); all.data.cc <- data.frame(x[2]); all.data.cnt <- data.frame(x[3])
  
  # Create a regression table of all data
  all.data.r <- reg.matrix(all.data); all.data.r <- all.data.r[,-1]
  colnames(all.data.r) <- p.lst
  thresh <- c(); thresh.lim = 0.5
  
  for(i in 1: length(all.data.r))
  {
    count = 0
    for(j in 1:length(all.data.r))
    {
      if(all.data.r[j,i] > thresh.lim) { count  = count + 1;}
    }
    thresh <- c(thresh, count)
  }
  print(sum(thresh))
  all.data.r <- rbind(all.data.r, thresh)
  rownames(all.data.r) <- c(p.lst, paste("Threshlod at: ", thresh.lim))
}


# (2) Build multivariate models (use step() and all.poss.reg(), check for colinearity and repeat until good model is found)


# Model1 and Model2, full models
coll.track <- data.frame()

base     <- lm(pct~1, data = all.data)
full     <- lm(pct~., data = all.data)
model1   <- step(base, scope = list(lower = base, upper = full), direction = "forward", trace = 0)
model1.s <- summary(model1); print(model1.s)

plot_res(residuals(model1), "Model1")

all.poss.regs(pct~., data = all.data)
model2   <- lm(pct~l+l.h+l.r+lead.o+peny+w+ypp, data = all.data)
model2.s <- summary(model2); print(model2.s)

plot_res(residuals(model2), "Model2")

all.data.colr <- all.data  # use for models
all.data.colc <- within(all.data, rm("pct")) # use for check

coll.track <- rbind(coll.track, track_cln(all.data))
coll.track <- rbind(coll.track, track_cln(all.data))

# model3 and model4
check_cln(all.data.colc, print = TRUE)
  
all.data.colr <- within(all.data.colr, rm("l.r"))
all.data.colc <- within(all.data.colc, rm("l.r"))
  
base     <- lm(pct~1, data = all.data.colr)
full     <- lm(pct~., data = all.data.colr)
  
model3   <- step(base, scope = list(lower = base, upper = full), direction = "forward", trace = 0)
model3.s <- summary(model3); print(model3.s)
  
plot_res(residuals(model3), "Model3")
  
all.poss.regs(pct~., data = all.data.colr)
model4   <- lm(pct~l+l.h+lead.o+peny+w.h+w.r+ypp, data = all.data)
model4.s <- summary(model4); print(model4.s)
  
plot_res(residuals(model4), "Model4")

coll.track <- rbind(coll.track, track_cln(all.data.colc))
coll.track <- rbind(coll.track, track_cln(all.data.colc))
  
# model5 and model6
check_cln(all.data.colc, print = TRUE)

all.data.colr <- within(all.data.colr, rm("w"))
all.data.colc <- within(all.data.colc, rm("w"))

base     <- lm(pct~1, data = all.data.colr)
full     <- lm(pct~., data = all.data.colr)

model5   <- step(base, scope = list(lower = base, upper = full), direction = "forward", trace = 0)
model5.s <- summary(model5); print(model5.s)

plot_res(residuals(model5), "Model5")

all.poss.regs(pct~., data = all.data.colr)
model6   <- lm(pct~l+l.h+lead.o+peny+w.h+w.r+ypp, data = all.data)
model6.s <- summary(model6); print(model6.s)

plot_res(residuals(model6), "Model6")

coll.track <- rbind(coll.track, track_cln(all.data.colc))
coll.track <- rbind(coll.track, track_cln(all.data.colc))

# model7 and model8
check_cln(all.data.colc, print = TRUE)

all.data.colr <- within(all.data.colr, rm("l"))
all.data.colc <- within(all.data.colc, rm("l"))

base     <- lm(pct~1, data = all.data.colr)
full     <- lm(pct~., data = all.data.colr)

model7   <- step(base, scope = list(lower = base, upper = full), direction = "forward", trace = 0)
model7.s <- summary(model7); print(model7.s)

plot_res(residuals(model7), "Model7")

all.poss.regs(pct~., data = all.data.colr)
model8   <- lm(pct~l.h+lead.o+pa+peny+w.h+w.r, data = all.data)
model8.s <- summary(model8); print(model8.s)

plot_res(residuals(model8), "Model8")

coll.track <- rbind(coll.track, track_cln(all.data.colc))
coll.track <- rbind(coll.track, track_cln(all.data.colc))

# model9 and model10
check_cln(all.data.colc, print = TRUE)

all.data.colr <- within(all.data.colr, rm("pf"))
all.data.colc <- within(all.data.colc, rm("pf"))

base     <- lm(pct~1, data = all.data.colr)
full     <- lm(pct~., data = all.data.colr)

model9   <- step(base, scope = list(lower = base, upper = full), direction = "forward", trace = 0)
model9.s <- summary(model9); print(model9.s)

plot_res(residuals(model9), "Model9")

all.poss.regs(pct~., data = all.data.colr)
model10   <- lm(pct~l.h+lead.o+pa+peny+w.h+w.r, data = all.data)
model10.s <- summary(model10); print(model10.s)

plot_res(residuals(model10), "Model10")

coll.track <- rbind(coll.track, track_cln(all.data.colc))
coll.track <- rbind(coll.track, track_cln(all.data.colc))

# model11 and model12
check_cln(all.data.colc, print = TRUE)

all.data.colr <- within(all.data.colr, rm("l.h"))
all.data.colc <- within(all.data.colc, rm("l.h"))

base     <- lm(pct~1, data = all.data.colr)
full     <- lm(pct~., data = all.data.colr)

model11   <- step(base, scope = list(lower = base, upper = full), direction = "forward", trace = 0)
model11.s <- summary(model11); print(model11.s)

plot_res(residuals(model11), "Model11")

all.poss.regs(pct~., data = all.data.colr)
model12   <- lm(pct~lead.o+netpt+pa+peny+ppg+td+w.h+w.r+ypg+ypp, data = all.data)
model12.s <- summary(model12); print(model12.s)

plot_res(residuals(model12), "Model12")

coll.track <- rbind(coll.track, track_cln(all.data.colc))
coll.track <- rbind(coll.track, track_cln(all.data.colc))

# model13 and model14
check_cln(all.data.colc, print = TRUE)

all.data.colr <- within(all.data.colr, rm("netpt"))
all.data.colc <- within(all.data.colc, rm("netpt"))

base     <- lm(pct~1, data = all.data.colr)
full     <- lm(pct~., data = all.data.colr)

model13   <- step(base, scope = list(lower = base, upper = full), direction = "forward", trace = 0)
model13.s <- summary(model13); print(model13.s)

plot_res(residuals(model13), "Model13")

all.poss.regs(pct~., data = all.data.colr)
model14   <- lm(pct~lead.o+pa+peny+ppg+w.h+w.r+ypg+ypp, data = all.data)
model14.s <- summary(model14); print(model14.s)

plot_res(residuals(model14), "Model14")

coll.track <- rbind(coll.track, track_cln(all.data.colc))
coll.track <- rbind(coll.track, track_cln(all.data.colc))

# model15 and model16
check_cln(all.data.colc, print = TRUE)

all.data.colr <- within(all.data.colr, rm("ypg"))
all.data.colc <- within(all.data.colc, rm("ypg"))

base     <- lm(pct~1, data = all.data.colr)
full     <- lm(pct~., data = all.data.colr)

model15   <- step(base, scope = list(lower = base, upper = full), direction = "forward", trace = 0)
model15.s <- summary(model15); print(model15.s)

plot_res(residuals(model15), "Model15")

all.poss.regs(pct~., data = all.data.colr)
model16   <- lm(pct~lead.o+pa+peny+ppg+w.h+w.r, data = all.data)
model16.s <- summary(model16); print(model16.s)

plot_res(residuals(model16), "Model16")

coll.track <- rbind(coll.track, track_cln(all.data.colc))
coll.track <- rbind(coll.track, track_cln(all.data.colc))

# model17 and model18

check_cln(all.data.colc, print = TRUE)

all.data.colr <- within(all.data.colr, rm("ppg"))
all.data.colc <- within(all.data.colc, rm("ppg"))

base     <- lm(pct~1, data = all.data.colr)
full     <- lm(pct~., data = all.data.colr)

model17   <- step(base, scope = list(lower = base, upper = full), direction = "forward", trace = 0)
model17.s <- summary(model17); print(model17.s)

plot_res(residuals(model17), "Model17")

all.poss.regs(pct~., data = all.data.colr)
model18   <- lm(pct~lead.o+pa+peny+td+w.h+w.r+ypp, data = all.data)
model18.s <- summary(model18); print(model18.s)

plot_res(residuals(model18), "Model18")

coll.track <- rbind(coll.track, track_cln(all.data.colc))
coll.track <- rbind(coll.track, track_cln(all.data.colc))

# model19 and model20
check_cln(all.data.colc, print = TRUE)

all.data.colr <- within(all.data.colr, rm("td"))
all.data.colc <- within(all.data.colc, rm("td"))

base     <- lm(pct~1, data = all.data.colr)
full     <- lm(pct~., data = all.data.colr)

model19   <- step(base, scope = list(lower = base, upper = full), direction = "forward", trace = 0)
model19.s <- summary(model19); print(model19.s)

plot_res(residuals(model19), "Model19")

all.poss.regs(pct~., data = all.data.colr)
model20   <- lm(pct~lead.o+pa+peny+w.h+w.r+ypp, data = all.data)
model20.s <- summary(model20); print(model20.s)

plot_res(residuals(model20), "Model20")

check_cln(all.data.colc, print = TRUE) # GOOD NOW

coll.track <- rbind(coll.track, track_cln(all.data.colc))
coll.track <- rbind(coll.track, track_cln(all.data.colc))

# Put all the models together
if(TRUE)
{
  m.n <- c("Model1","Model2","Model3","Model4","Model5","Model6","Model7","Model8","Model9","Model10","Model11","Model12","Model13","Model14","Model15","Model16","Model17","Model18","Model19","Model20")
  ms.table <- data.frame()
  temp <- glance(model1.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model1)))
  temp <- glance(model2.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model2)))
  temp <- glance(model3.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model3))) 
  temp <- glance(model4.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model4))) 
  temp <- glance(model5.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model5))) 
  temp <- glance(model6.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model6))) 
  temp <- glance(model7.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model7))) 
  temp <- glance(model8.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model8))) 
  temp <- glance(model9.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model9))) 
  temp <- glance(model10.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model10))) 
  temp <- glance(model11.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model11)))
  temp <- glance(model12.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model12)))
  temp <- glance(model13.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model13)))
  temp <- glance(model14.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model14)))
  temp <- glance(model15.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model15)))
  temp <- glance(model16.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model16)))
  temp <- glance(model17.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model17)))
  temp <- glance(model18.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model18)))
  temp <- glance(model19.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model19)))
  temp <- glance(model20.s)
  ms.table <- rbind(ms.table, c(temp$r.squared, temp$sigma, AIC(model20)))
  
  ms.table <- cbind(ms.table, coll.track[,1], coll.track[,2],coll.track[,3])
  rownames(ms.table) <- m.n; colnames(ms.table) <- c("r.sqr","sigma","AIC","Coll.Threshold","(%) Above Median","Predcitors above Threshold")
  
}

print(ms.table)

# Pick a model:
model.compare <- data.frame() # Will store data to compare models

summary(model17)

summary(model20)

model <- lm(pct~lead.o+pa+peny+w.h+w.r+ypp, data = all.data) # Model 20
model.s <- summary(model); print(model.s)

m.s <- glance(model.s); model.compare <- rbind(model.compare, c(m.s$r.squared, m.s$sigma, AIC(model)))

# Plot predictor data to identify where/if we need transforms
transform <- function(data)
{
  par(mfrow = c(1,2))
  hist(data       , main = "reg" ) ; qqnorm(data)        ; print(summary(data))
  hist(log(data)  , main = "log" ) ; qqnorm(log(data))   ; print(summary(log(data)))
  hist(2*log(data), main = "2log") ; qqnorm(2*log(data)) ; print(summary(2*log(data)))   
  hist(sqrt(data) , main = "sqrt") ; qqnorm(sqrt(data))  ; print(summary(sqrt(data))) 
  hist((data)^2   , main = "pow2") ; qqnorm((data)^2)    ; print(summary((data)^2)) 
  hist((data)^3   , main = "pow3") ; qqnorm((data)^3)    ; print(summary((data)^3)) 
  hist((data)^4   , main = "pow4") ; qqnorm((data)^4)    ; print(summary((data)^4)) 
}

#transform(all.data$lead.o)
transform(all.data$pa)
transform(all.data$peny)
#transform(all.data$w.h)
#transform(all.data$w.r)
transform(all.data$ypp)


model.t1   <- lm(pct~lead.o+pa+log(peny)+w.h+w.r+ypp, data = all.data)
model.t1.s <- summary(model.t1); print(model.t1.s); plot_res(residuals(model.t1), "model.t1")

m.s <- glance(model.t1.s); model.compare <- rbind(model.compare, c(m.s$r.squared, m.s$sigma, AIC(model.t1)))
colnames(model.compare) <- c("r-sq","sigma","AIC"); print(model.compare)

model.t2   <- lm(pct~lead.o+log(pa)+log(peny)+w.h+w.r+ypp, data = all.data)
model.t2.s <- summary(model.t2); print(model.t2.s); plot_res(residuals(model.t2), "model.t2")

m.s <- glance(model.t2.s); model.compare <- rbind(model.compare, c(m.s$r.squared, m.s$sigma, AIC(model.t2)))
print(model.compare)

model.t3   <- lm(pct~lead.o+log(pa)+log(peny)+w.h+w.r+log(ypp), data = all.data)
model.t3.s <- summary(model.t3); print(model.t3.s); plot_res(residuals(model.t3), "model.t3")

m.s <- glance(model.t3.s); model.compare <- rbind(model.compare, c(m.s$r.squared, m.s$sigma, AIC(model.t3)))
print(model.compare)

model.t4   <- lm(pct~lead.o+log(pa)+log(peny)+w.h+w.r+sqrt(ypp), data = all.data)
model.t4.s <- summary(model.t4); print(model.t4.s); plot_res(residuals(model.t4), "model.t4")

m.s <- glance(model.t4.s); model.compare <- rbind(model.compare, c(m.s$r.squared, m.s$sigma, AIC(model.t4)))
print(model.compare)

model.t5   <- lm(pct~lead.o+log(pa)+log(peny)+w.h+w.r+log(ypp), data = all.data)
model.t5.s <- summary(model.t5); print(model.t5.s); plot_res(residuals(model.t5), "model.t5")

m.s <- glance(model.t5.s); model.compare <- rbind(model.compare, c(m.s$r.squared, m.s$sigma, AIC(model.t5)))
print(model.compare)

model.t6   <- lm(pct~lead.o+log(pa)+log(peny)+(w.h^2)+(w.r^2)+log(ypp), data = all.data)
model.t6.s <- summary(model.t6); print(model.t6.s); plot_res(residuals(model.t6), "model.t6")

m.s <- glance(model.t6.s); model.compare <- rbind(model.compare, c(m.s$r.squared, m.s$sigma, AIC(model.t6)))
print(model.compare)

model.t7   <- lm(pct~lead.o+log(pa)+log(peny)+w.h+w.r+log(ypp), data = all.data)
model.t7.s <- summary(model.t7); print(model.t7.s); plot_res(residuals(model.t7), "model.t7")

m.s <- glance(model.t7.s); model.compare <- rbind(model.compare, c(m.s$r.squared, m.s$sigma, AIC(model.t7)))
print(model.compare)


# Isolate final models for selection:

model.all     <- model1   ; model.all.s   <- summary(model.all)   ; model.all.stats   <- glance(model.all.s)
model.nt      <- model    ; model.nt.s    <- summary(model.nt)    ; model.nt.stats    <- glance(model.nt.s)
model.final   <- model.t7 ; model.final.s <- summary(model.final) ; model.final.stats <- glance(model.final.s)
model.factors <- lm(pct~factor(lead.o)+log(pa)+log(peny)+factor(w.h)+factor(w.r)+log(ypp), data = all.data)
mf.s <- summary(model.factors); mf.stats <- glance(mf.s)

final.table <- data.frame()
final.table <- rbind(final.table, c(model.all.stats$r.squared, model.all.stats$sigma, AIC(model.all)))
final.table <- rbind(final.table, c(model.nt.stats$r.squared, model.nt.stats$sigma, AIC(model.nt)))
final.table <- rbind(final.table, c(mf.stats$r.squared, mf.stats$sigma, AIC(model.factors)))
final.table <- rbind(final.table, c(mf.stats$r.squared, mf.stats$sigma, AIC(model.factors)))

colnames(final.table) <- c("R-sqr","sigma","AIC"); rownames(final.table) <- c("Full Model","Optimized","Optimized + Transformed","Opt/Trans with factors")
print(final.table)
plot_res(residuals(model.all), "Full Model"); plot_res(residuals(model.nt), "Optimized")
plot_res(residuals(model.final), "optimized + Transformed"); plot_res(residuals(model.factors), "Opt/trans + factors")

summary(model.factors)
summary(model.final)

# End of this source code

