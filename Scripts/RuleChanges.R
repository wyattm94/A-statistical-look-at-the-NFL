# Statistics Final Project (FE 541)
# December 7th 2017
# Question (Part 4) - Analyze the effects of major rule changes in the NFL
# Wyatt Marciniak
# Pia Om
# Laramie Regalado

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
  #options(scipen = "100", digits = 4)
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
    a = length(data[,1]) - (2016-from); b = length(data[,1]) - (2016-to) 
    if(a <= 0 || a > length(data[,1])) { print("Something is wrong with your function or data, a is out of data range")}
    if(b <= 0 || b > length(data[,1])) { print("Something is wrong with your function or data, b is out of data range")}
    temp <- c()
    for(i in 1: length(data))
    {
      temp <- c(temp, c(data[a:b,i]))
    }
    temp
  }
  rule_effect   <- function(datax, datay, lowb, highb, lowa, higha) # Check regression models before and after rule change
  {
    bx    <- agg_data(datax, lowb, highb)
    by    <- agg_data(datay, lowb, highb)
    ax    <- agg_data(datax, lowa, higha)
    ay    <- agg_data(datay, lowa, higha)
    b.m   <- lm(by~bx)
    b.ms  <- summary(b.m)
    a.m   <- lm(ay~ax)
    a.ms  <- summary(a.m)
    
    b.ms_stats <- glance(b.ms)
    a.ms_stats <- glance(a.ms)
    
    print(b.ms_stats); print(a.ms_stats)
    
    compare <- data.frame()
    compare <- rbind(compare, c(b.ms_stats$r.squared, a.ms_stats$r.squared))
    compare <- rbind(compare, c(b.ms_stats$sigma, a.ms_stats$sigma))
    compare <- rbind(compare, c(AIC(b.m),AIC(a.m)))
    compare <- rbind(compare, c(mean(bx), mean(ax)))
    
    rsq.delta <- (compare[1,2] - compare[1,1])/compare[1,1]
    sig.delta <- (compare[2,2] - compare[2,1])/compare[2,1]
    aic.delta <- (abs(compare[3,2]) - abs(compare[3,1]))/compare[3,1]
    avg.delta <- (compare[4,2] - compare[4,1])/compare[4,1]
    
    compare <- cbind(compare, c(rsq.delta*100, sig.delta*100, aic.delta*100, avg.delta*100))
    print(compare)
    
    colnames(compare) <- c(paste(lowb,"-",highb), paste(lowa,"-",higha),"Change (%)")
    rownames(compare) <- c("r.squared","sigma (res)","AIC","Average/year")
    
    compare
  }
  tt_rules      <- function(data, lowb, highb, lowa, higha) # Generate t-test table, < ci.l, ci.h, pval, (%) chgs in mean, median and std (full data)
  {
    data.b    <- agg_data(data, lowb, highb)
    data.a    <- agg_data(data, lowa, higha)
    test      <- t.test(as.numeric(data.b), as.numeric(data.a), alternative = "two.sided")
    test.ci   <- data.frame(test$conf.int)
    test.p    <- data.frame(test$p.value)
    
    test.mean <- data.frame(test$estimate)
    test.mb   <- test.mean[1,1]
    test.ma   <- test.mean[2,1]
    change.mu <- ((test.ma - test.mb)/test.mb)*100
    
    test.medb <- median(data.b)
    test.meda <- median(data.a)
    change.med<- ((test.meda - test.medb)/test.medb)*100
    
    test.sdb  <- sd(data.b)
    test.sda  <- sd(data.a)
    change.std<- ((test.sda - test.sdb)/test.sdb)*100
    
    
    tt.row  <- data.frame(test.ci[1,1], test.ci[2,1], test.p[1,1], change.mu, change.med, change.std)
    tt.row
  }
  tt_rules.t    <- function(data, lowb, highb, lowa, higha) # Generates row of p-values from t-tests (17 variables/ team --> 17cols per row, 1 row)
  {
    temp <- c()
    for(i in 1:17) # covers all 17 parameters per team
    {
      data.b <- c(); data.a <- c()
      for(j1 in 1:(highb - lowb)+1)
      {
        data.b    <- c(data.b, data[j1,i]) # get data for before rule
      }
      for(j2 in j1+1:length(data[,i]))
      {
        data.a    <- c(data.a, data[j2,i]) # get data for after rule
      }
      
      test      <- t.test(as.numeric(data.b), as.numeric(data.a), alternative = "two.sided")
      test.p    <- data.frame(test$p.value)
      p <- test.p[1,1]
      
      temp      <- c(temp, p) # adds p value here
    }
    temp  # returns 1 row (list) of p value (17 variables for the given team)
    print(temp)
  }
  rule.reg      <- function(datax, datay, lowb, highb, lowa, higha)
  {
    b.0 = length(datax)-(highb-lowb)+1; b.1 = length(datax) - (2016 - highb)
    a.0 = length(datay)-(higha-lowa)+1; a.1 = length(datay) - (2016 - higha)
    
    
    bx    <- datax[b.0:b.1]
    by    <- datay[b.0:b.1]
    ax    <- datax[a.0:a.1]
    ay    <- datay[a.0:a.1]
    b.m   <- lm(by~bx)
    b.ms  <- summary(b.m)
    a.m   <- lm(ay~ax)
    a.ms  <- summary(a.m)
    
    b.ms.s <- glance(b.ms)
    a.ms.s <- glance(a.ms)
    
    delta.rsq = ((a.ms.s$r.squared - b.ms.s$r.squared)/b.ms.s$r.squared)*100
    delta.sig = ((a.ms.s$sigma - b.ms.s$sigma)/b.ms.s$sigma)*100
    delta.aic = ((AIC(a.m) - AIC(b.m))/AIC(b.m))*100
    
    
    temp <- c(b.ms.s$r.squared, a.ms.s$r.squared, delta.rsq, 
              b.ms.s$sigma, a.ms.s$sigma, delta.sig,
              AIC(b.m), AIC(a.m), delta.aic)
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
  ttest.waves   <- function(vars, times, mark, type)
  {
    rule.time      <- times[1]
    waves.time     <- times[2:length(times)]
    test.analysis  <- data.frame(matrix(ncol=length(vars))); waves.names <- c()
    test.deltamean <- data.frame(matrix(ncol=length(vars))); delta.names <- c()
    
    for(i in 1:length(waves.time))
    {
      temp.p <- c(); temp.mc <- c()
      for(j in 1:length(vars))
      {
        b0 = rule.time - waves.time[i]; b1 = rule.time - 1
        a0 = rule.time; a1 = rule.time + waves.time[i] - 1
        
        data.b    <- agg_data(get(vars[j]), b0, b1);
        data.a    <- agg_data(get(vars[j]), a0, a1);
        test      <- t.test(as.numeric(data.b), as.numeric(data.a), alternative = type)
        test.p    <- data.frame(test$p.value)
        
        print(paste(b0,"-",b1,":",a0,"-",a1))
        print(paste("Size.b = ",length(data.b)," , size.a = ",length(data.a)))
        print(paste("CI = ", test$conf))
        
        test.mean <- data.frame(test$estimate)
        test.mb   <- test.mean[1,1]
        test.ma   <- test.mean[2,1]
        test.mc   <- ((test.ma - test.mb)/test.mb)*100
        
        temp.p      <- c(temp.p, as.numeric(test.p))
        temp.mc     <- c(temp.mc, as.numeric(test.mc))
      }
      waves.names    <- c(waves.names, paste("[",b0,":",b1," , ",a0,":",a1,"]"))
      delta.names    <- c(delta.names, paste("(%) Change in Mean for ",waves.time[i]," years"))
      test.analysis  <- rbind(test.analysis, temp.p)
      test.deltamean <- rbind(test.deltamean, temp.mc)
    }
    test.analysis            <- test.analysis[-1,]; test.deltamean <- test.deltamean[-1,]
    colnames(test.analysis)  <- vars; rownames(test.analysis)      <- waves.names
    colnames(test.deltamean) <- vars; rownames(test.deltamean)     <- delta.names 
    
    # Plotting:
    par(mfrow = c(1,1))
    ts.plot(test.analysis, main = paste(mark," P-values for T-tests"),gpars= list(col=rainbow(7)))
    legend("topright", legend = vars, col = 1:7, lty = 1)
    abline(0.05, 0, col = "black", style = "dashed")
    boxplot(test.analysis, main = paste(mark," Summaries for T-test p.value per variable"))
    abline(0.05,0, col = "red")
    boxplot(test.deltamean, main = paste(mark," Summaries for T-test mean (%) changes per variable"))
    
    # Output
    output <- c()
    output[[1]] <- test.analysis; output[[2]] <- test.deltamean;
    output
  }
  ttest.step    <- function(vars, times, mark, type)
  {
    rule.time      <- times[1]
    steps          <- times[2]
    test.analysis  <- data.frame(matrix(ncol=length(vars))); steps.names <- c()
    test.deltamean <- data.frame(matrix(ncol=length(vars))); delta.names <- c()
    
    # Steps before rule change
    for(i in steps:1)
    {
      temp.p <- c(); temp.mc <- c()
      s0 = rule.time - i; s1 = s0 + 1
      
      for(j in 1:length(vars))
      {
        data.s0   <- agg_data(get(vars[j]), s0, s0);
        data.s1   <- agg_data(get(vars[j]), s1, s1);
        test      <- t.test(as.numeric(data.s0), as.numeric(data.s1), alternative = type)
        test.p    <- data.frame(test$p.value)
        
        test.mean <- data.frame(test$estimate)
        test.mb   <- test.mean[1,1]
        test.ma   <- test.mean[2,1]
        test.mc   <- ((test.ma - test.mb)/test.mb)*100
        
        temp.p      <- c(temp.p, as.numeric(test.p))
        temp.mc     <- c(temp.mc, as.numeric(test.mc))
      }
      steps.names    <- c(steps.names, paste("[",s0,":",s1,"]"))
      delta.names    <- c(delta.names, paste("(%) Change in Mean in [",s0,":",s1,"]"))
      test.analysis  <- rbind(test.analysis, temp.p)
      test.deltamean <- rbind(test.deltamean, temp.mc)
    }
    
    # Steps after rule change
    for(i in 1:steps)
    {
      temp.p <- c(); temp.mc <- c()
      s0 = rule.time + i - 1; s1 = s0 + 1
      
      for(j in 1:length(vars))
      {
        data.s0   <- agg_data(get(vars[j]), s0, s0);
        data.s1   <- agg_data(get(vars[j]), s1, s1);
        test      <- t.test(as.numeric(data.s0), as.numeric(data.s1), alternative = "two.sided")
        test.p    <- data.frame(test$p.value)
        
        test.mean <- data.frame(test$estimate)
        test.mb   <- test.mean[1,1]
        test.ma   <- test.mean[2,1]
        test.mc   <- ((test.ma - test.mb)/test.mb)*100
        
        temp.p      <- c(temp.p, as.numeric(test.p))
        temp.mc     <- c(temp.mc, as.numeric(test.mc))
      }
      steps.names    <- c(steps.names, paste("[",s0,":",s1,"]"))
      delta.names    <- c(delta.names, paste("(%) Change in Mean in [",s0,":",s1,"]"))
      test.analysis  <- rbind(test.analysis, temp.p)
      test.deltamean <- rbind(test.deltamean, temp.mc)
    }
    
    test.analysis            <- test.analysis[-1,]; test.deltamean <- test.deltamean[-1,]
    colnames(test.analysis)  <- vars; rownames(test.analysis)      <- steps.names
    colnames(test.deltamean) <- vars; rownames(test.deltamean)     <- delta.names 
    
    # Plotting:
    par(mfrow = c(1,1))
    ts.plot(test.analysis, main = paste(mark," P-values for 1 year step T-tests"),gpars= list(col=rainbow(7)))
    legend("topright", legend = vars, col = 1:7, lty = 1)
    abline(v = c(0,10), h = c(0.05,5), col = "black") 
    abline(0,10, col = "red")
    boxplot(test.analysis, main = paste(mark," Summaries for T-test p.value per variable (steps)"))
    abline(0.05,0, col = "red")
    boxplot(test.deltamean, main = paste(mark," Summaries for T-test mean (%) changes per variable (steps)"))
    
    # Output
    output <- c()
    output[[1]] <- test.analysis; output[[2]] <- test.deltamean;
    output
  }
}

if(TRUE) # Calculate full team data sets and correlation tables
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

# PART 1: EFFECT OF MAJOR RULE CHANGES --------------------------------------------------------------------------------------------------

# Rule 1 - 1995 (Encroachment rule for defense created, 1 penalty)

t0.b = 1976
t1.b = 1993
t0.a = 1994
t1.a = 2016

if(TRUE) # Generate table of ttests on all 17 paramters (before/after change of rule)
{
  rule1.test.agg <- data.frame()
  cnames <- c("CI.low","CI.high","P.value","(%) Change of data mean","(%) Change of the data Median","(%) Change of the data std.dev")
  rnames <- c("l","l.h","l.r","lead.o","lead.d","netpt","pa","pct","peny","pf","ppg","td","w","w.h","w.r","ypg","ypp")
  
  rule1.test.agg <- rbind(rule1.test.agg, tt_rules(l, t0.b, t1.b, t0.a, t1.a))
  rule1.test.agg <- rbind(rule1.test.agg, tt_rules(l.h, t0.b, t1.b, t0.a, t1.a))
  rule1.test.agg <- rbind(rule1.test.agg, tt_rules(l.r, t0.b, t1.b, t0.a, t1.a))
  rule1.test.agg <- rbind(rule1.test.agg, tt_rules(lead.o, t0.b, t1.b, t0.a, t1.a))
  rule1.test.agg <- rbind(rule1.test.agg, tt_rules(lead.d, t0.b, t1.b, t0.a, t1.a))
  rule1.test.agg <- rbind(rule1.test.agg, tt_rules(netpt, t0.b, t1.b, t0.a, t1.a))
  rule1.test.agg <- rbind(rule1.test.agg, tt_rules(pa, t0.b, t1.b, t0.a, t1.a))
  rule1.test.agg <- rbind(rule1.test.agg, tt_rules(pct, t0.b, t1.b, t0.a, t1.a))
  rule1.test.agg <- rbind(rule1.test.agg, tt_rules(peny, t0.b, t1.b, t0.a, t1.a))
  rule1.test.agg <- rbind(rule1.test.agg, tt_rules(pf, t0.b, t1.b, t0.a, t1.a))
  rule1.test.agg <- rbind(rule1.test.agg, tt_rules(ppg, t0.b, t1.b, t0.a, t1.a))
  rule1.test.agg <- rbind(rule1.test.agg, tt_rules(td, t0.b, t1.b, t0.a, t1.a))
  rule1.test.agg <- rbind(rule1.test.agg, tt_rules(w, t0.b, t1.b, t0.a, t1.a))
  rule1.test.agg <- rbind(rule1.test.agg, tt_rules(w.h, t0.b, t1.b, t0.a, t1.a))
  rule1.test.agg <- rbind(rule1.test.agg, tt_rules(w.r, t0.b, t1.b, t0.a, t1.a))
  rule1.test.agg <- rbind(rule1.test.agg, tt_rules(ypg, t0.b, t1.b, t0.a, t1.a))
  rule1.test.agg <- rbind(rule1.test.agg, tt_rules(ypp, t0.b, t1.b, t0.a, t1.a))
  
  colnames(rule1.test.agg) <- cnames
  rownames(rule1.test.agg) <- rnames
}

show(rule1.test.agg)

if(TRUE) # Investigate netpt data to see if normal (based on -22% change in mean)
{
  t.total <- agg_data(netpt, 1976, 2016)
  summary(t.total)
  par(mfrow = c(3,1))
  plot(t.total, main = "Total Data"); abline(0,0, col = "red") # Add by factor here
  hist(as.numeric(t.total))
  boxplot(t.total)
  
  t.br    <- agg_data(netpt, 1976, 1993)
  summary(t.br)
  par(mfrow = c(3,1))
  plot(t.br, main = "Before Change"); abline(0,0, col = "red") # Add by factor here
  hist(as.numeric(t.br))
  boxplot(t.br)

  t.ar    <- agg_data(netpt, 1994, 2016)
  summary(t.ar)
  par(mfrow = c(3,1))
  plot(t.ar, main = "After Change"); abline(0,0, col = "red") # Add by factor here
  hist(as.numeric(t.ar))
  boxplot(t.ar)

  
  ggqqplot(as.numeric(t.total), main = "Total Data")
  ggqqplot(as.numeric(t.br),    main = "Before Change")
  ggqqplot(as.numeric(t.ar),    main = "After Change")
  
}

if(TRUE) # Generate table of ttests per team per variable (p-value table)
{
  rule1.test.team <- data.frame(matrix(ncol= 17))
  for(i in 1:length(tnames.l))
  {
    temp <- tt_rules.t(get(tnames.l[i]), t0.b, t1.b, t0.a, t1.a)
    rule1.test.team <- rbind(rule1.test.team, as.list(temp))
  }
  
  rule1.test.team <- rule1.test.team[-1,]
  
  countlist = c()
  for(i in 1:length(rule1.test.team))
  {
    count = 0
    for(j in 1:length(rule1.test.team[,i]))
    {
      if(rule1.test.team[j,i] < 0.05) { count = count + 1}
    }
    countlist <- c(countlist, count)
  }
  
  percentlist <- (countlist/24)*100
  
  rule1.test.team <- rbind(rule1.test.team, countlist)
  rule1.test.team <- rbind(rule1.test.team, percentlist)
  
  colnames(rule1.test.team) <- rnames # used for columns here, same things
  rownames(rule1.test.team) <- c(tnames,"Number of significant changes", "(%) of teams")
}

print(rule1.test.team)

# Focused Variable Tests
focus <- c("pf","pa","ppg","ypg","ypp","peny","td")

if(TRUE) # Create a full data set of focus group
{
  focus.data <- data.frame(matrix(ncol = 1)); reg.names <- c()
  for(i in 1:length(focus))
  {
    focus.data <- cbind(focus.data, agg_data(get(focus[i]), 1976, 2016))
  }
  focus.data <- focus.data[,-1]; colnames(focus.data) <- focus
}

times <- c(1994,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)
tt.wave1 <- ttest.waves(focus, times, 1995, "two.sided")
tt.wave1.p <- data.frame(tt.wave1[1]); tt.wave1.m <- data.frame(tt.wave1[2])

times <- c(1994, 10)
tt.step1 <- ttest.step(focus, times, 1995, "two.sided")
tt.step1.p <- data.frame(tt.step1[[1]]); tt.step1.m <- data.frame(tt.step1[2])


# Rule 2 - 2005 (More penalties added - 3)

t0.b = 1993
t1.b = 2004
t0.a = 2005
t1.a = 2016

if(TRUE) # Generate table of ttests on all 17 paramters (before/after change of rule)
{
  rule2.test.agg <- data.frame()
  cnames <- c("CI.low","CI.high","P.value","(%) Change of data mean","(%) Change of the data Median","(%) Change of the data std.dev")
  rnames <- c("l","l.h","l.r","lead.o","lead.d","netpt","pa","pct","peny","pf","ppg","td","w","w.h","w.r","ypg","ypp")
  
  rule2.test.agg <- rbind(rule2.test.agg, tt_rules(l, t0.b, t1.b, t0.a, t1.a))
  rule2.test.agg <- rbind(rule2.test.agg, tt_rules(l.h, t0.b, t1.b, t0.a, t1.a))
  rule2.test.agg <- rbind(rule2.test.agg, tt_rules(l.r, t0.b, t1.b, t0.a, t1.a))
  rule2.test.agg <- rbind(rule2.test.agg, tt_rules(lead.o, t0.b, t1.b, t0.a, t1.a))
  rule2.test.agg <- rbind(rule2.test.agg, tt_rules(lead.d, t0.b, t1.b, t0.a, t1.a))
  rule2.test.agg <- rbind(rule2.test.agg, tt_rules(netpt, t0.b, t1.b, t0.a, t1.a))
  rule2.test.agg <- rbind(rule2.test.agg, tt_rules(pa, t0.b, t1.b, t0.a, t1.a))
  rule2.test.agg <- rbind(rule2.test.agg, tt_rules(pct, t0.b, t1.b, t0.a, t1.a))
  rule2.test.agg <- rbind(rule2.test.agg, tt_rules(peny, t0.b, t1.b, t0.a, t1.a))
  rule2.test.agg <- rbind(rule2.test.agg, tt_rules(pf, t0.b, t1.b, t0.a, t1.a))
  rule2.test.agg <- rbind(rule2.test.agg, tt_rules(ppg, t0.b, t1.b, t0.a, t1.a))
  rule2.test.agg <- rbind(rule2.test.agg, tt_rules(td, t0.b, t1.b, t0.a, t1.a))
  rule2.test.agg <- rbind(rule2.test.agg, tt_rules(w, t0.b, t1.b, t0.a, t1.a))
  rule2.test.agg <- rbind(rule2.test.agg, tt_rules(w.h, t0.b, t1.b, t0.a, t1.a))
  rule2.test.agg <- rbind(rule2.test.agg, tt_rules(w.r, t0.b, t1.b, t0.a, t1.a))
  rule2.test.agg <- rbind(rule2.test.agg, tt_rules(ypg, t0.b, t1.b, t0.a, t1.a))
  rule2.test.agg <- rbind(rule2.test.agg, tt_rules(ypp, t0.b, t1.b, t0.a, t1.a))
  
  colnames(rule2.test.agg) <- cnames
  rownames(rule2.test.agg) <- rnames
}

show(rule2.test.agg)


if(TRUE) # Generate table of ttests per team per variable (p-value table)
{
  rule2.test.team <- data.frame(matrix(ncol= 17))
  for(i in 1:length(tnames.l))
  {
    temp <- tt_rules.t(get(tnames.l[i]), t0.b, t1.b, t0.a, t1.a)
    rule2.test.team <- rbind(rule2.test.team, as.list(temp))
  }
  
  rule2.test.team <- rule2.test.team[-1,]
  
  countlist = c()
  for(i in 1:length(rule2.test.team))
  {
    count = 0
    for(j in 1:length(rule2.test.team[,i]))
    {
      if(rule2.test.team[j,i] < 0.05) { count = count + 1}
    }
    countlist <- c(countlist, count)
  }
  
  percentlist <- (countlist/24)*100
  
  rule2.test.team <- rbind(rule2.test.team, countlist)
  rule2.test.team <- rbind(rule2.test.team, percentlist)
  
  colnames(rule2.test.team) <- rnames # used for columns here, same things
  rownames(rule2.test.team) <- c(tnames,"Number of significant changes", "(%) of teams")
}

print(rule2.test.team)


# Focused Variable Tests
focus <- c("pf","pa","ppg","ypg","ypp","peny","td")

if(TRUE) # Create a full data set of focus group
{
  focus.data <- data.frame(matrix(ncol = 1)); reg.names <- c()
  for(i in 1:length(focus))
  {
    focus.data <- cbind(focus.data, agg_data(get(focus[i]), 1976, 2016))
  }
  focus.data <- focus.data[,-1]; colnames(focus.data) <- focus
}


times <- c(2005,1,2,3,4,5,6,7,8,9,10,11)
tt.wave2 <- ttest.waves(focus, times, 2005, "two.sided")
tt.wave2.p <- data.frame(tt.wave2[1]); tt.wave2.m <- data.frame(tt.wave2[2])



times <- c(2005, 10)
tt.step2 <- ttest.step(focus, times, 2005, "two.sided")
tt.step2.p <- data.frame(tt.step2[[1]]); tt.step2.m <- data.frame(tt.step2[2])







