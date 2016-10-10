###The Bag of Little Bootstraps##############
library(data.table)
library(parallel)
library(Hmisc)

Ni = as.numeric(system("wc -l trip_fare* | cut -d ' ' -f4", intern = TRUE)[-13])-1
N = sum(Ni) #total population--173179759
n = round(N^0.65) #sample size--226476
s = 12 #The number of subsets
r = 13 #the number of bootstrap replicates per subset 

f = list.files("/home/data/NYCTaxis", pattern = "trip_fare.*\\.csv$", full.names = TRUE)
g = list.files("/home/data/NYCTaxis", pattern = "trip_data.*\\.csv$", full.names = TRUE)

cl = makeCluster(12, "FORK")

cmd1 = paste("cut -f 10,11 -d ,",f) #10--tolls amount, 11--total amount
els1 = clusterSplit(cl,cmd1)

cmd2 = paste("cut -f 9 -d ,",g) #9--trip time in secs
els2 = clusterSplit(cl, cmd2)

Probs = seq(0.1,0.9,0.1)

clusterExport(cl, c("els1","els2"))
Amtbysubset = parLapply(cl, 1:12, function(x) {fread(els1[[x]])[, " total_amount", with = F] - fread(els1[[x]])[, " tolls_amount", with = F]})
Amt = unlist(Amtbysubset) 

Timbysubset = parLapply(cl, 1:12, function(x) {fread(els2[[x]])[, 1, with = F]})
Tim = unlist(Timbysubset)

sm = lapply(1:12, function(x) {sample(1:N, n, replace = FALSE)})

clusterExport(cl, c("Amt","Tim"))

SubsetVal_Amt = parLapply(cl, sm, function(x)Amt[x])
SubsetVal_Tim = parLapply(cl, sm, function(x)Tim[x])

clusterCall(cl, library, "Hmisc", character.only = TRUE)

##calculate se of deciles
clusterExport(cl,"SubsetVal_Amt")
Bootstr_Amt = parLapply(cl, SubsetVal_Amt, function(x){sapply(1:13, function(y){tbl = table(sample(x, N, replace = TRUE));wtd.quantile(as.numeric(names(tbl)), weights = tbl, probs = Probs)})})

se_dec = parLapply(cl, Bootstr_Amt, function(x) {apply(x, 1, function(y) sd(y))})

mtx_se.dec = matrix(unlist(se_dec), byrow = TRUE, nrow = 12)

se_d = apply(mtx_se.dec, 2, function(x) mean(x))

##calculate se of betas
Bootstr_AnT = parLapply(cl, 1:12, function(x){sapply(1:r, function(y){
                                                          ind = sample(sm[[x]], N, replace = TRUE);
                                                          beta1 = cov(Amt[[ind]],Tim[[ind]])/var(Tim[[ind]]);
                                                          beta0 = mean(Amt[[ind]]) - beta1*mean(Tim[[ind]]);
                                                          c(beta1, beta0)
                                                          })})
                                                      
se_beta = perLapply(cl, Bootstr_AnT, function(x) {apply(x, 1, function(y) sd(y))})

mtx_se.beta = matrix(unlist(se_beta), byrow = TRUE, nrow = 12)

se_b = apply(mtx_se.beta, 2, function(x) mean(x))

