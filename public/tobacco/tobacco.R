library(Hmisc)
library(RColorBrewer)
library(car)

# Set wd
setwd("C:/Users/BrewJR/Documents/tobacco")

# Read in data
comb <- read.csv("comb1.csv")

# Read in names of questions
comb_names <- read.csv("comb1.csv", header = FALSE)
comb_names <- comb_names[1,]

###################
# SIMPASYM FUNCTION FOR CALCULATIN C.I. OF PROPORTIONS
###################
simpasym <- function(n, p, z=1.96, cc=TRUE){
  out <- list()
  if(cc){
    out$lb <- p - z*sqrt((p*(1-p))/n) - 0.5/n
    out$ub <- p + z*sqrt((p*(1-p))/n) + 0.5/n
  } else {
    out$lb <- p - z*sqrt((p*(1-p))/n)
    out$ub <- p + z*sqrt((p*(1-p))/n)
  }
  out
}

###################
# WRITE FUNCTION FOR PLOTTING BARS
###################
BarFun <- function(var, recode_var = NULL){
  
  # Ensure it's treated as a character
  var <- as.character(var)
  
  # Replace 77,88,99 with "Unknown"
  var[which(var %in% c(77,88,99))] <- "Unknown"
  
  # If recoding vector is supplied, use it
  if(is.null(recode_var)){
    var <- Recode(var,
                  "'1' = 'Yes';
                '2' = 'No'")
  } else {
    var <- Recode(var,
                  recode_var)    
  }
  
  # Make var a factor and relevel so "unknown" comes first
  var <- factor(var)
  var <- relevel(var, ref = "Unknown")
    
  # Make a table
  var_table <- table(var)
  var_names <- names(var_table)
  var_vals <- as.numeric(var_table)
  var_prop <- prop.table(var_table) * 100
  
  # Assing positions based on relative value compared to others
  var_pos <- ifelse(var_prop < 0.5 * max(var_prop),
                    3, 1)
  
  # Calculate confidence intervals
  ci <- simpasym(n=sum(var_table), 
                 p=var_prop / 100, 
                 z=1.96, cc=TRUE) 
  lb <- ci$lb * sum(var_table)
  ub <- ci$ub * sum(var_table)
  
  # Create color vector (first is always red if there are unknowns)
  if( var_table["Unknown"] > 0 ){
    my_colors <- c("Red", colorRampPalette(c("darkblue", "darkgreen"))(length(levels(var)) -1))
  } else {
    my_colors <- colorRampPalette(c("darkblue", "darkgreen"))(length(levels(var)))
  }
  my_colors <- adjustcolor(my_colors, alpha.f = 0.6)
  
  # Make barplot
  bp <- barplot(var_table,
                ylim = c(0, max(ub)*1.2),
                col = my_colors,
                border = "darkgrey")
  
  # Add point estimate text
#   text(x = bp[,1],
#        y = var_vals,
#        labels = paste0(round(var_prop, digits = 1), "%"),
#        pos = var_pos)
  
# Add error bars
  errbar(x=bp[,1],
         y=var_table,
         yplus=ub,
         yminus=lb,
         add=TRUE,
         type="n",
         errbar.col=adjustcolor("darkred", alpha.f=0.6),
         lwd=2)
  
  # Add text of point estimate with c.i.'s
  text(x=bp[,1], 
       y= ifelse(var_pos == 1, 
                 var_table - (.15*max(var_table)),
                 var_table + (.15*max(var_table))),
       labels=paste0(" ", round(var_prop, digits=1),
                     "%",
                     "\n(",
                     round(ci$lb*100, digits=1),
                     "%",
                     "-",
                     round(ci$ub*100, digits=1),
                     "%",
                     ")"
       ),
       cex=2/length(var_table),
       col=adjustcolor("black", alpha.f=0.7))
  
}

par(mfrow=c(1,1))
BarFun(comb[,7])
title(main = comb_names[,7])

par(mfrow=c(1,2))
BarFun(comb[which(comb[,5] == 1),8])
title(main = "< 50 employees", cex.main = 0.8)
BarFun(comb[which(comb[,5] == 2),8])
title(main = "50 + employees", cex.main = 0.8)
title(main = "Does your employeer provide health insurance?",
      outer = TRUE,
      line = -1)


# Question names
q_names <- c("Number of employees at work place",
             "Size Category",
             
             )
# of tobacco users at work place	Does employer hire tobacco users?             --yes or no	Does employer provide health insurance?  yes or no?  If no, proceed to column Y.	Please select type of insurance coverage:  self-insured or fully-insured	Insurance Carrier	Please select one employer sector: government, healthcare, or business (including private or non-profit)	Any tobacco cessation coverage?	Patch NRT OTC Covered?                     -- Yes or No	Gum NRT OTC Covered?                      -- Yes or No	Lozenge NRT OTC Covered?                    -- yes or no	Covered OTC only?	Inhaler NRT RX Covered?                    -- yes or no	Nose Spray RX Covered?                    -- yes or no	Bupropion RX  Covered?                   -- yes or no	Varenicline RX Covered?                    -- yes or no	Covered RX only?	Covered both OTC and RX?	Individual Counseling Covered?                   -- yes or no	Phone Counseling Covered?                    -- yes or no	Group Counseling Covered?                      -- yes or no	Web Based Counseling Covered?    -- yes or no	Covered some types of counseling?	Covered all types of counseling?	Covers at least 2 Quit Attempts provided per year                  -- yes or no	Are a combination of both cessation medication and counseling covered?	Any barriers to coverage, such as counseling before medications or    co-pays?             --yes or no	Extent of TFG Policy Coverage -- 100% SFG, 100% TFG, SFIW, or SFIWO     	E-cigs included in policy as restricted yes or no	Silver FTCA Medication Criteria Met	Gold FTCA Medication Criteria Met	Baseline SOC      BENEFIT	Baseline SOC           TFG Policy	Baseline SOC            Promote Encourage Provide	BaselineWas a referral to a local AHEC  made for this employer this quarter?           Yes or No	Q-1        FY(13-14) SOC      BENEFIT	Q-1        FY(13-14) SOC      TFG Policy	Q-1        FY(13-14) SOC         Promote Encourage Provide	Q-1        Was a referral to a local AHEC  made for this employer this quarter?           Yes or No	Q-2        FY(13-14) SOC      BENEFIT	Q-2        FY(13-14) SOC      TFG Policy	Q-2        FY(13-14) SOC         Promote Encourage Provide	Q-2      Was a referral to a local AHEC  made for this employer this quarter?           Yes or No	Q-3        FY(13-14) SOC      BENEFIT	Q-3        FY(13-14) SOC      TFG Policy	Q-3        FY(13-14) SOC         Promote Encourage Provide	Q-3      Was a referral to a local AHEC  made for this employer this quarter?           Yes or No	Q-4        FY(13-14) SOC      BENEFIT	Q-4        FY(13-14) SOC      TFG Policy	Q-4        FY(13-14) SOC         Promote Encourage Provide	Q-4      Was a referral to a local AHEC  made for this employer this quarter?           Yes or No	Briefly describe any progress made with this employer from baseline interview.                  (For example, has an employer strengthened a campus policy, instituted a no-hire policy,   increased benefit coverage, or made a decision to do so?  Have you sent this employer emails or information newsletters to maintain regular communication?  Have you spoken by phone or face-fo-face?)	Briefly describe any plans, opportunities or potential strategies to engage this employer in the coming quarter to move them from one stage of change to another.

