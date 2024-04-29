##script to add updates to database and merge changes
library(data.table)
library(pool)
library(RPostgres)
require(dplyr)

sppDb <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "spp_feas",
  host = "138.197.168.220",
  port = 5432,
  user = "postgres",
  password = "PowerOfBEC"
)

# updates <- fread("Vancouver_Tree_Suitability_2021_Coast_7Oct2021.csv")
# updates <- updates[,.(Region,SS_NoSpace,Spp,`Suitability(refguide)`,`COAST 5Feb+25Feb`,`COAST SEPT 2021`)]
# setnames(updates,c("region","ss_nospace","spp","OrigFeas","Update1","Update2"))


dat <- dbGetQuery(sppDb, "select distinct feasorig.bgc, feasorig.spp, feasorig.newfeas 
                  from forhealth full join feasorig on 
                  forhealth.treecode = feasorig.spp and 
                  forhealth.bgc = feasorig.bgc where forhealth.bgc IS NULL;")
dat <- as.data.table(dat)
dat2 <- dat[,.(FeasMax = min(newfeas)), by = .(bgc, spp)]
dat2 <- dat2[FeasMax <= 3,]
unique(dat2$bgc)
bgcs <- fread("../CCISS_ShinyApp/data-raw/data_tables/WNA_BGCs_Info_v12_15.csv")
bc_bgcs <- bgcs[DataSet == "BC",BGC]
dat3 <- dat2[bgc %in% bc_bgcs,]
dat3 <- dat3[spp != "",]
spps <- unique(dat3$spp)

fh <- dbGetQuery(sppDb, "select * from forhealth")
fh <- as.data.table(fh)
fwrite(fh,"fh_save_April23.csv")
fh <- fread("fh_save_April23.csv")

spps <- c("Fd", "Sw","At","Pw","Sx","Ss","Yc","Act","Bl","Cw","Hw","Lw","Pl","Py")

for(curr_spp in spps){
  cat(curr_spp," ")
  pests <- unique(fh[treecode == curr_spp,.(pest,pest_name)])
  new_bgcs <- dat3[spp == curr_spp, bgc]
  if(length(new_bgcs) > 0 & nrow(pests) > 0){
    new_dat <- data.table(bgc = rep(new_bgcs, each = nrow(pests)), 
                          treecode = curr_spp,
                          pest = rep(pests$pest, length(new_bgcs)),
                          pest_name = rep(pests$pest_name, length(new_bgcs)),
                          hazard = "UN",hazard_update = "UN",
                          mod = NA,
                          region = "BC")
    dbWriteTable(sppDb, "forhealth", new_dat, row.names = F, append = TRUE)
  }
}


temp <- fh[,.(Num = .N), by = .(bgc,treecode)]

fh <- dbGetQuery(sppDb, "select * from forhealth")
fwrite(fh, "ForestHealthDownload.csv")
##check that it's safe to combine the update columns
# updates[!is.na(Update1) & !is.na(Update2),]
# updates[!is.na(Update2),Update1 := Update2]
# updates[,Update2 := NULL]
# updates <- updates[!is.na(Update1),] ##only care about rows that actually have an update
# updates[,OrigFeas := NULL]

##change to base codes to make sure it merges properly
# updates[spp %in% c("Fdi","Fdc"),spp := "Fd"]
# updates[spp %in% c("Pli","Plc"),spp := "Pl"]
# updates[spp %in% c("Sw","Se","Sxw"),spp := "Sx"]
# updates[spp %in% c("Ss", "Sxl","Sxs"),spp := "Ss"]
# updates[spp %in% c("Pyi","Pyc"),spp := "Py"]
# updates[spp %in% c("Acb","Act"),spp := "Ac"]

###now pull from database
dbFeas <- setDT(dbGetQuery(sppDb,"select * from feasorig"))
fwrite(dbFeas,"feasorig_save4.csv") ##just in case something goes wrong
 #updaterate <- fread("./inputs/Feasibility_v12_11.csv")
# dbFeasNew <- left_join(dbFeas, updaterate)
# fwrite(dbFeasNew,"feasorig_save2.csv")##just incase something goes wrong
# feasNew <- merge(dbFeas,updates, by = c("ss_nospace","spp"), all = T)
##____________LOAD Externally updated spread shett to merge back into database

feasNew <- fread("C:/Users/kirid/Downloads/FeasibilityUpdates_ByBEC.csv")###read back in an updated csv file
feasNew[,feasible := NULL]
setnames(feasNew, old = c("newfeas","mod"), new = c("nf_update","mod_update"))
setnames(feasNew, old = "spp",new = "sppsplit")

check_updates <- merge(dbFeas, feasNew, by = c("bgc","ss_nospace","sppsplit"), all = T)
temp <- check_updates[newfeas != nf_update,]
temp <- temp[mod != "",]
fixed_conflicts <- fread("Conflicting_feas.csv")
fixed_conflicts <- fixed_conflicts[,.(ss_nospace,sppsplit,use,mod_use)]
check_updates[newfeas != nf_update,`:=`(newfeas = nf_update, mod = mod_update)]
check_updates[fixed_conflicts, `:=`(fix_feas = i.use, fix_mod = i.mod_use),
              on = c("ss_nospace","sppsplit")]
check_updates[!is.na(fix_feas),`:=`(newfeas = fix_feas,mod = fix_mod)]
newDat <- check_updates[,.(bgc, ss_nospace, sppsplit, feasible, spp, newfeas, 
                             mod)]
newDat[mod == "", mod := NA]
##check that none of the updates conflict with previous updates
#feasNew[!is.na(mod) & !is.na(Update1),]
##ok all good
#reviewers <- "SAS-HAK"
#feasNew[!is.na(Update1),`:=`(newfeas = Update1,mod = reviewers)]
# feasNew[is.na(bgc),bgc := gsub("/.*","",ss_nospace)]
# feasNew[is.na(sppsplit),temp := spp]
# feasNew[temp == "Pl",temp := "Plc"]
# feasNew[temp == "Fd",temp := "Fdc"]
# feasNew[temp == "Ss",temp := "Sxs"]
# feasNew[temp == "Sx",temp := "Sxw"]
# feasNew[temp == "Ac",temp := "Act"]
# feasNew[!is.na(temp), sppsplit := temp]
# feasNew[,temp := NULL]

feasNew <- feasNew[,.(bgc,ss_nospace,sppsplit,feasible,spp,newfeas,mod)] ##make sure it's only got the rows required
unique(feasNew$mod)
##warning! make sure you've saved a copy before dropping table###
dbExecute(sppDb,"drop table feasorig")
dbWriteTable(sppDb,"feasorig",newDat,row.names = F)
dbGetQuery(sppDb,"select count(*) from feasorig")
dbExecute(sppDb,"create index on feasorig (bgc,sppsplit)")
dbExecute(sppDb,"create index on feasorig (spp)")
##update species names


ycUpdate <- fread("~/Downloads/USAplotswithYc.csv")
ycUpdate[,`:=`(sppsplit = "Yc",spp = "Yc",region = "US")]
library(sf)
yc <- st_as_sf(ycUpdate,coords = c("Longitude","Latitude"), crs = 4326)
colnames(yc)[1] <- "plotnum"
st_write(yc,sppDb,"plotdata",append = T, row.names = F)
