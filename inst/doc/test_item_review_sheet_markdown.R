## ----knitr setup, include=FALSE-----------------------------------------------
# global options for all chunks
knitr::opts_chunk$set(echo = FALSE)


## ----prep---------------------------------------------------------------------
# Required packages
library("conquestr")
library("kableExtra")


## ----sysfile------------------------------------------------------------------

# Read default system file
mySys <- ConQuestSys()

# Read itanal
mySys_itanal <- getCqItanal(mySys)

# Summary stats are alwasy the last element of the list returned by getCqItanal
SumStat <- mySys_itanal[[length(mySys_itanal)]]

# internal formatting of itanal
mySys_itanalFmt <- fmtCqItanal(mySys_itanal)

# iteration history
iter_hist <- getCqHist(mySys)


## ----rout---------------------------------------------------------------------
#generate a ICC/MCC plot from an 'ACER ConQuest' Rout file - traditional ICC/MCC plot
my_Rout <- ConQuestRout()


## ----plots--------------------------------------------------------------------
# iteration history plot of item parameters 
iter_hist_plot <- plotCqHist(iter_hist, centre = FALSE, legend = TRUE, params = "Xsi")
# ICC plot
my_Rout_plot <- plotRout(my_Rout)


## ----summary stats------------------------------------------------------------
SumStat %>%
  kbl(digits = 2) %>% # could set digits as a global option
  kable_styling()

## ----convergence plot, message=FALSE, warning=FALSE, fig.cap = "Iteration history of item parameters."----

plot(iter_hist_plot)


## ----item_1_stats-------------------------------------------------------------
#Item 1 name only
mySys_itanal[[1]]$name %>%
  kbl() %>%
  kable_styling()

#Item 1 table only
mySys_itanal[[1]]$table %>%
  kbl(digits = 2) %>%
  kable_styling()

#Item 1 item corrs only
mySys_itanal[[1]]$item_rest_total %>%
  kbl(digits = 2) %>%
  kable_styling()


## ---- plot_icc----------------------------------------------------------------
plot(my_Rout_plot)

