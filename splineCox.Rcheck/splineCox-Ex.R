pkgname <- "splineCox"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "splineCox-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('splineCox')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("splineCox.reg1")
### * splineCox.reg1

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: splineCox.reg1
### Title: Fitting the five-parameter spline Cox model giving a specified
###   shape
### Aliases: splineCox.reg1

### ** Examples

# Example data
library(joint.Cox)
data(dataOvarian)
t.event = dataOvarian$t.event
event = dataOvarian$event
Z = dataOvarian$CXCL12

reg1 <- splineCox.reg1(t.event, event, Z, model = "constant")
print(reg1)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("splineCox.reg1", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("splineCox.reg2")
### * splineCox.reg2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: splineCox.reg2
### Title: Fitting the five-parameter spline Cox model with a specified
###   shape, selecting the best fit
### Aliases: splineCox.reg2

### ** Examples

# Example data
library(joint.Cox)
data(dataOvarian)
t.event = dataOvarian$t.event
event = dataOvarian$event
Z = dataOvarian$CXCL12

M = c("constant", "increase", "decrease")
reg2 <- splineCox.reg2(t.event, event, Z, model = M)
print(reg2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("splineCox.reg2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
