# CEE05 VAR Replication 
# Mardoqueo Arteaga, Fordham University
CEE = read.csv("CEE05data.csv", header = TRUE)
library(vars)
library(aTSA)
CEEDat = CEE[,-1]
head(CEEDat)
CEE = ts(CEEDat, start = c(1965, 2), frequency = 4) # turns into ts data

plot(CEE,  main = "Macro Time Series CEE05")
# All variables except FFR and M2SL are logged. FFR is reported as is, M2 is percent change (diff of logs)
# This type of transformation is all the paper says they do 

# ADF for most of these fails, though. I(1) obtained by doing differences, something not said they do in the paper
adf.test(CEE[,"GDPC1"])
adf.test(diff(CEE[,"GDPC1"])) # I(1)

adf.test(CEE[,"PCECC96"])
adf.test(diff(CEE[,"PCECC96"])) # I(1) 

adf.test(CEE[,"GDPDEF"])
adf.test(diff(diff(CEE[,"GDPDEF"]))) # I(1)

adf.test(CEE[,"GPDIC1"])
adf.test(diff(CEE[,"GPDIC1"]))# I(1)

adf.test(CEE[,"COMPNFB"])
adf.test(diff(diff(CEE[,"COMPNFB"]))) # I(1)

adf.test(CEE[,"LP"])
adf.test(diff(CEE[,"LP"]))# I(1)

adf.test(CEE[,"FFR"])
adf.test(diff(CEE[,"FFR"])) # I(1)

adf.test(CEE[,"CP"])
adf.test(diff(CEE[,"CP"])) # I(1)

adf.test(CEE[,"M2SL_PCH"])
adf.test(diff(CEE[,"M2SL_PCH"])) # I(1)

VARselect(CEE, lag.max=4, type="none", season = 4)
Acf(CEE, lag.max = NULL, plot = TRUE, calc.ci = TRUE, level = 0.95)
# The following is attempting the VAR with the ts as it was via CEE05, without I(1) variables via ADF
CEE05VAR = VAR(CEE, p = 2, type = "none", ic="AIC") # type = "none" due to CEE saying there are no intercepts
CEE05VARcon = VAR(CEE, p = 4, type = "const", ic="AIC") # second attempt with constants included, not huge differences
CEE05VAR
CEE05VARcon
causality(CEE05VAR, cause = "FFR", vcov.=NULL, boot=FALSE, boot.runs=100)


# Cannot compare the coefs of the VAR since they are not included in paper, so I try the IRFs next

gdpirf = irf(CEE05VAR, impulse = "FFR", response = "GDPC1",
            n.ahead = 20, ortho = TRUE, runs = 100)# ortho = TRUE via paper identification
cirf = irf(CEE05VAR, impulse = "FFR", response = "PCECC96",
           n.ahead = 20, ortho = FALSE, runs = 100)
defirf = irf(CEE05VAR, impulse = "FFR", response = "GDPDEF",
             n.ahead = 20, ortho = TRUE, runs = 100)
invirf = irf(CEE05VAR, impulse = "FFR", response = "GPDIC1",
             n.ahead = 20, ortho = TRUE, runs = 100)
wirf = irf(CEE05VAR, impulse = "FFR", response = "COMPNFB",
           n.ahead = 20, ortho = TRUE, runs = 100)
lpirf = irf(CEE05VAR, impulse = "FFR", response = "LP",
            n.ahead = 20, ortho = TRUE, runs = 100)
ffrirf = irf(CEE05VAR, impulse = "FFR", response = "FFR",
             n.ahead = 20, ortho = TRUE, runs = 100)
cpirf = irf(CEE05VAR, impulse = "FFR", response = "CP",
            n.ahead = 20, ortho = TRUE, runs = 100)
m2irf = irf(CEE05VAR, impulse = "FFR", response = "M2SL_PCH",
            n.ahead = 20, ortho = TRUE, runs = 100)

# I attempt to plot them here. 
# First page of IRFs
plot(defirf)
plot(ffrirf)
plot(invirf)
plot(lpirf)
plot(m2irf)
# Second page of IRFs
plot(wirf)
plot(gdpirf)
plot(cirf) 
plot(cpirf) 

t(chol(cov(resid(CEE05VAR)))) # CEE say diagonal of C is unity; this is not the case.
fevd(CEE05VAR, n.ahead = 20)# Here is also the forecast error variance decomp, but the numbers do not quite match Table 1, page 8


## Extension?
# So here I try to make a new ts using the stationary versions of the variables

CEEs = cbind(diff(CEE[,"GDPC1"]), diff(CEE[,"PCECC96"]), diff(diff(CEE[,"GDPDEF"])),diff(CEE[,"GPDIC1"]), 
             diff(diff(CEE[,"COMPNFB"])), diff(CEE[,"LP"]), diff(CEE[,"FFR"]),
             diff(CEE[,"CP"]), diff(CEE[,"M2SL_PCH"]))
CEEs = CEEs[complete.cases(CEEs),]
colnames(CEEs) = colnames(CEE)
CEEs = ts(CEEs, start = c(1965, 2), frequency = 4)
plot(CEEs, main = "Stationary Macro Time Series CEE05")
Acf(CEEs, lag.max = NULL, plot = TRUE, calc.ci = TRUE, level = 0.95)
lapply(CEEs, VARselect)
VARselect(CEEs, lag.max=8, type="none", season = 4)

CEEs05VAR = VAR(CEEs, p=1, type ="none", ic= "AIC")

gdpirf_s = irf(CEEs05VAR, impulse = "FFR", response = "GDPC1",
              n.ahead = 20, ortho = TRUE, runs = 100)
cirf_s = irf(CEEs05VAR, impulse = "FFR", response = "PCECC96",
           n.ahead = 20, ortho = FALSE, runs = 100)
defirf_s = irf(CEEs05VAR, impulse = "FFR", response = "GDPDEF",
             n.ahead = 20, ortho = TRUE, runs = 100)
invirf_s = irf(CEEs05VAR, impulse = "FFR", response = "GPDIC1",
             n.ahead = 20, ortho = TRUE, runs = 100)
wirf_s = irf(CEEs05VAR, impulse = "FFR", response = "COMPNFB",
           n.ahead = 20, ortho = TRUE, runs = 100)
lpirf_s = irf(CEEs05VAR, impulse = "FFR", response = "LP",
            n.ahead = 20, ortho = TRUE, runs = 100)
ffrirf_s = irf(CEEs05VAR, impulse = "FFR", response = "FFR",
             n.ahead = 20, ortho = TRUE, runs = 100)
cpirf_s = irf(CEEs05VAR, impulse = "FFR", response = "CP",
            n.ahead = 20, ortho = TRUE, runs = 100)
m2irf_s = irf(CEEs05VAR, impulse = "FFR", response = "M2SL_PCH",
            n.ahead = 20, ortho = TRUE, runs = 100)

# First page of IRFs
plot(defirf_s)
plot(ffrirf_s)
plot(invirf_s)
plot(lpirf_s)
plot(m2irf_s)

# Second page of IRFs
plot(wirf_s)
plot(gdpirf_s)
plot(cirf_s) 
plot(cpirf_s) 

# Not much improvement under this from the ts that did not pass the ADF test 


t(chol(cov(resid(CEEs05VAR))))  # CEE say diagonal of C is unity; this is still not the case.
fevd(CEEs05VAR) # Here is also the forecast error variance decomp, but the numbers do not quite match Table 1, page 8


