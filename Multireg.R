###Multilinear Regression
install.packages("data.table", lib = "~/Rpackages")
library(data.table)
library(parallel)
library(Hmisc)

Ni = as.numeric(system("wc -l trip_fare* | cut -d ' ' -f4", intern = TRUE)[-13])-1 #extract counts of cases in each trip_fare file #space as "delimiter"
N = sum(Ni) #total population--173179759
n = round(N^0.65) #sample size--226476
ni = round(n*(Ni/N)) #sample size in each subset is based on its population proportion

#set.clusters
cl = makeCluster(12, "FORK")

Index = parLapply(cl, 1:12, function(i) (sample(1:Ni[i], ni[i], replace = FALSE)))

f = list.files("/home/data/NYCTaxis", pattern = "trip_fare.*\\.csv$", full.names = TRUE)
g = list.files("/home/data/NYCTaxis", pattern = "trip_data.*\\.csv$", full.names = TRUE)

cmd1 = paste("cut -f 7,10,11 -d ,",f) #7--surcharge, 10--tolls amount, 11--tot amount
els1 = clusterSplit(cl,cmd1)

cmd2 = paste("cut -f 9 -d ,",g)
els2 = clusterSplit(cl, cmd2)

ones = sapply(1:12, function(x) rep(1, ni[x]))

clusterExport(cl,c("els1","els2","ones","Index"))
mtx.xy = parLapply(cl, 1:12, function(x){mtx = data.matrix(fread(els2[[x]])[Index[[x]],1,with =F]);
                                         mtx.xs = data.matrix(fread(els1[[x]])[Index[[x]], " surcharge", with =F])
                                         mtx.x = cbind(ones[[x]], mtx, mtx.xs);
                                         
                                         mtx.y = data.matrix(fread(els1[[x]])[Index[[x]]," total_amount", with = F] - fread(els1[[x]])[Index[[x]], " tolls_amount", with = F]);
                                         c(t(mtx.x)%*%mtx.x, t(mtx.x)%*%mtx.y)
})
mtx.xy = Reduce("+", mtx.xy)
mtx.1 = matrix(mtx.xy[1:9], nrow = 3) #XtX matrix
mtx.2 = matrix(mtx.xy[10:12], nrow = 3) #xtY matrix

beta = solve(mtx.1)%*%(mtx.2)
