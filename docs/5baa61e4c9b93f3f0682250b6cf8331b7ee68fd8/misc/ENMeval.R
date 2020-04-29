library(ENMeval)
library(spocc)
library(dismo)
#setwd('/home/pgalante/Projects/PracticeData')
#locs<-read.csv('locs.csv')
#colnames(locs) <- c("x","y")

Locs <- spocc::occ(query = "Lota lota", from = "gbif", limit = 300)
Locs$gbif
occs <- occ2df(Locs)
as.data.frame(occs)


library(spThin)
tLocs <- thin(loc.data = occs, lat.col = 'latitude', long.col = 'longitude', spec.col = 'name', thin.par = 10, reps = 10,
              write.files = F, max.files = 1, locs.thinned.list.return = T, write.log.file = F)


env1<-stack(list.files(path = "C:/Users/pgalante/layers/wc10", pattern = '\\.tif$', full.names = T))

library(rgeos)
#mcp
mcp <- function (xy) {
  xy <- as.data.frame(coordinates(xy))
  coords.t <- chull(xy[, 1], xy[, 2])
  xy.bord <- xy[coords.t, ]
  xy.bord <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
  return(SpatialPolygons(list(Polygons(list(Polygon(as.matrix(xy.bord))), 1))))
}
buff.dist <- 5 # This value will change depending on expert knowledge
MCP.locs<-mcp(tLocs[[1]])
shp <- gBuffer(MCP.locs, width = buff.dist)
env <- mask(env, shp)
backg <- randomPoints(env, 10000)
colnames(backg) <- colnames(tLocs[[1]])

ev<-ENMevaluate(occ = tLocs[[1]], env = env, bg.coords = backg, RMvalues = seq(1,3,1), fc = c("L","LQ","H","LQH"),
                method = "block", n.bg = 10000, rasterPreds = T, parallel = T, numCores = 8)

ev@results

# optimize
optimize <- function(res) {
  ###Remove any candidate model which has an AUC less than 0.51= models with no discrimination
  opt.auc <- res[res[,4] >= 0.5,]
  ###Remove any candidates which have no parameters
  no.param <- opt.auc[opt.auc[,13] > 1,]
  ###Remove any candidates where the AIC score was NA (too many parameters)
  noAICNA<- no.param[which(!is.na(no.param$AICc)),]
  ###Remove any models which have an OR of zero
  noOR0 <- noAICNA[noAICNA[,9] != 0,]
  ###Order the remaining list by lowest OR then highest AUC, sequentially
  ordered<-noOR0[with(noOR0, order(avg.test.or10pct, -avg.test.AUC)), ]
  ###Grab the settings of that first model (the optimal model)
  ordered[1,]
}
best<-optimize(ev@results)

#####Modified from Adam B Smith @ Missouri Botanical Garden (earthskysea.org)
###Create that optimal model using all localities (no withheld data)
###x = rasterstack, p = locs object as data.frame, a = background coords, factors = categoricals,
### make true only the arguments wanted
mod <- maxent(
  x=env, # bio stack
  p=tLocs[[1]], # locality csv
  a= NULL, # background coords
  #path= "C:/Users/pgalante/Documents/Projects/Madagascar/endemism_diversity/tests", # path to save to
  args=c(
    'betamultiplier=2',
    'linear=true',
    'quadratic=true',
    'product=false',
    'threshold=false',
    'hinge=false',
    'threads=2',
    'responsecurves=true',
    'jackknife=true',
    'askoverwrite=false'
  )
)


##Predict that optimal model: select output format wanted, output file type wantes, and pathway to save it
Pred.Mod1<- predict(
  object = mod,
  x = env1,
  #filename = "C:/Users/pgalante/Documents/Projects/Madagascar/endemism_diversity/tests", #where do you want the prediction saved?
  na.rm = T,
  format = 'GTiff',#or GTiff
  overwrite = F,
  args = "cloglog"
)

plot(Pred.Mod)
