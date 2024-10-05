###############################################
# Code for statistical analyses, creating     #
# tables, and creating figures                #
#                                             #
# Matt Yourek                                 #  
# matthew.yourek@wsu.edu                      #
# October 3, 2024                             # 
###############################################


library(ggplot2)
library(sf)
library(gridExtra)
library(grid)

setwd("~/water_right_forfeiture")

#### Notes #####
## pairwise.t.test uses chi-square test to determine p values. The confidence interval for difference in proportions seems to use a critical t value and 
### CI  = p1-p2 +/- t*sqrt(sqrt(p1*(1-p1)/n1) ^ 2 + sqrt(p2*(1-p2)/n2) ^ 2)

water_rights <- read.table("input_data_files/water_rights_all_updated_adj2.csv", sep=",", encoding="ANSI", header=T)
water_rights$Comment <- iconv(water_rights$Comment, from="ISO-8859-1", to="UTF-8")
water_rights$EventComment <- iconv(water_rights$EventComment, from="ISO-8859-1", to="UTF-8")
water_rights$Status[water_rights$Stage %in% c("Rejected", "Withdrawn")] <- "Inactive"
water_rights$QaTotal <- sapply(1:nrow(water_rights), function(x) replace_zeroes(x))
ChangeIntentFinal <- read.csv("cleaned_data/WRChanges.csv")
ChangeIntentFinal$ChangeDate <- as.Date(ChangeIntentFinal$ChangeDate)
ChangeIntentFinal$Pre1967 <- ifelse(ChangeIntentFinal$ChangeDate < as.Date("1967-07-01"), TRUE, FALSE)
ChangeIntentFinal$ParentPhase <- water_rights$Phase[match(ChangeIntentFinal$Parent, water_rights$WRDocID)]

reg_lookup <- data.frame(Region=c(rep("SWRO", 12), rep("NWRO", 7), rep("CRO", 7), rep("ERO", 13)),
  County=c("Clallam", "Clark", "Cowlitz", "Grays Harbor", "Jefferson", "Mason", "Lewis", "Pacific", "Pierce", "Skamania", "Thurston", "Wahkiakum", "Island", "King", "Kitsap", "San Juan", "Skagit", "Snohomish", "Whatcom", "Benton", "Chelan", "Douglas", "Kittitas", "Klickitat", "Okanogan", "Yakima", "Adams", "Asotin", "Columbia", "Ferry",
  "Franklin", "Garfield", "Grant", "Lincoln", "Pend Oreille", "Spokane", "Stevens", "Walla Walla", "Whitman"))
ChangeIntentFinal$County <- water_rights$CountyNM[match(ChangeIntentFinal$Parent, water_rights$WRDocID)]
ChangeIntentFinal$WRIA <- water_rights$WRIA_NM[match(ChangeIntentFinal$Parent, water_rights$WRDocID)]
ChangeIntentFinal$Region <- reg_lookup$Region[match(ChangeIntentFinal$County, reg_lookup$County)]
oldest_WRIA <- unique(ChangeIntentFinal[,c("Oldest", "WRIA")])
dup_oldest <- oldest_WRIA$Oldest[which(duplicated(oldest_WRIA$Oldest))]
for (o in dup_oldest) {
  ChangeIntentFinal$WRIA[ChangeIntentFinal$Oldest == o] <- water_rights$WRIA_NM[water_rights$WRDocID == o]
}

ChangeIntentFinal_original <- ChangeIntentFinal
## select changes from 1967-07-01 (passage of relinquishment statutes) through 2019
ChangeIntentFinal <- subset(ChangeIntentFinal, ChangeDate > as.Date("1967-07-01") & ChangeDate < as.Date("2020-01-01")) 

##################### Summary of WRTS database ####################################################

records <- read.csv("input_data_files/Phase.csv")
records$Cat[records$Cat == "DroughtChange"] <- "Temporary"
records$Cat[records$Cat %in% c("ShortFormClaim", "LongFormClaim")] <- "Claim"
records.plot <- aggregate(records[,c("Active", "Inactive")], list(records$Cat), sum)
names(records.plot)[1] <- c("Phase")
records.plot$Phase <- c("Adjudicated Certificate", "Application", "Certificate", "Certificate of Change", "Change ROE", 
                        "Claim", "Permit", "Superseding Certificate", "Superseding Permit", "Temporary")
records.plot$Total <- records.plot$Active + records.plot$Inactive

jpeg("Figures/WRTS_records.jpg", width=3800, height=2800, units="px", res=600)
dotchart(log10(records.plot$Total), labels=records.plot$Phase, pch=16, xlab="Count", xaxt="n", xlim=c(2.6, 5.3))
axis(side=1, at=c(3, 4, 5), labels= c("10^3", "10^4", "10^5"))
text(labels=c("7,400", "22,985", "45,629", "2,414", "6,918", "169,285", "15,481", "1,705", "785", "1,610"), x=log10(records.plot$Total), y=1:10, adj=c(1.1, 0))
dev.off()

#### Summary of changes  ##########

n_change <- length(unique(ChangeIntentFinal$WaRecId))
n_diminished <- length(unique(subset(ChangeIntentFinal, DiminishingChange == TRUE)$WaRecId))
n_parent <- length(unique(ChangeIntentFinal$Parent))
n_dim_parent <- length(unique(subset(ChangeIntentFinal, DiminishingChange == TRUE)$Parent))

changes <- unique(ChangeIntentFinal[,c("WaRecId", "Parent")])
sum(table(changes$Parent)>1)
max(table(changes$Parent))

##### Relinquishment by year #######################################################

ChangeIntentFinal$Pre2000 <- ifelse(ChangeIntentFinal$ChangeDate < as.Date("2000-01-01") & ChangeIntentFinal$ChangeDate > as.Date("1967-07-01"), TRUE, FALSE)
ChangeIntentFinal$Post2000 <- ifelse(ChangeIntentFinal$ChangeDate > as.Date("2000-01-01"), TRUE, FALSE)
compare_years <- unique(ChangeIntentFinal[ChangeIntentFinal$Pre1967 == FALSE, c("Parent", "DiminishingChange", "Pre2000")])
## If a water right has some changes pre-2000 and some post-2000, we group that water right in the post-2000 category
dupParent_years <- compare_years$Parent[which(duplicated(compare_years$Parent))]
for (p in dupParent_years) {
  df <- subset(compare_years, Parent == p)
  if (any(df$DiminishingChange == TRUE)) {
    compare_years$Pre2000[compare_years$Parent == p] <- ifelse(any(df$Pre2000 == FALSE & df$DiminishingChange == TRUE), FALSE, TRUE)
  } else {
    compare_years$Pre2000[compare_years$Parent == p] <- ifelse(any(df$Pre2000 == FALSE), FALSE, TRUE)
  }
}
t1 <- data.frame(table(unique(compare_years[compare_years$DiminishingChange == TRUE,c("Parent", "Pre2000")])$Pre2000)) # forfeiture counts 
t2 <- data.frame(table(unique(compare_years[,c("Parent", "Pre2000")])$Pre2000)) # Changed parent counts
compare_years.t <- merge(t1, t2, "Var1", all=T)
names(compare_years.t) <- c("Pre2000", "Relinquished", "AllParents")
compare_years.t$Freq <- round(compare_years.t$Relinquished / compare_years.t$AllParents, 3)
relinqd_counts <- compare_years.t$Relinquished
names(relinqd_counts) <- c("Post-2000", "Pre-2000")
all_counts <- compare_years.t$AllParents
names(all_counts) <- c("Post-2000", "Pre-2000")

post2000_parents <- unique(compare_years$Parent[compare_years$Pre2000 == FALSE]) # parents with any changes after 2000
allQuant_Parent_all <- get_oldest_Qa_all() # forfeiture by original parent and parents with changes
allQuant_Parent <- get_oldest_Qa() # forfeiture by original parent and parents with changes after 2000

ChangeWRbyYear <- subset(ChangeIntentFinal, Pre1967 == FALSE)
ChangeWRbyYear$ChangeYear <- cut(as.numeric(strftime(ChangeWRbyYear$ChangeDate, "%Y")), breaks=seq(from=1966,2020,by=1))
ChangeWR_dim <- unique(subset(ChangeWRbyYear, DiminishingChange==TRUE)[c("Parent", "ChangeYear", "nonconsumptive", "QiOnly")])
## If a parent has more than one change, we assign the date of the most recent change to the parent
dupParent <- ChangeWR_dim$Parent[which(duplicated(ChangeWR_dim$Parent))]
for (p in dupParent) {
  y <- subset(ChangeWR_dim, Parent == p)$ChangeYear[which(as.numeric(subset(ChangeWR_dim, Parent == p)$ChangeYear) == max(as.numeric(subset(ChangeWR_dim, Parent == p)$ChangeYear)))]
  ChangeWR_dim$ChangeYear[ChangeWR_dim$Parent == p] <- y
}
ChangeWR_dim <- unique(ChangeWR_dim)

ChangeWR_nodim <- unique(subset(ChangeWRbyYear, DiminishingChange==FALSE)[c("Parent", "ChangeYear", "nonconsumptive", "QiOnly")])
dupParent <- ChangeWR_nodim$Parent[which(duplicated(ChangeWR_nodim$Parent))]
for (p in dupParent) {
  y <- subset(ChangeWR_nodim, Parent == p)$ChangeYear[which(as.numeric(subset(ChangeWR_nodim, Parent == p)$ChangeYear) == max(as.numeric(subset(ChangeWR_nodim, Parent == p)$ChangeYear)))]
  ChangeWR_nodim$ChangeYear[ChangeWR_nodim$Parent == p] <- y
}
ChangeWR_nodim <- unique(ChangeWR_nodim)
nodimParents <- ChangeWR_nodim$Parent[!(ChangeWR_nodim$Parent %in% ChangeWR_dim$Parent)] ## isolate parents with no diminishing changes
## Create dataframe with one row per parent. DiminishingChange == TRUE means the parent has one or more diminishing changes. DiminishingChange == FALSE means the parent has zero diminishing changes.
all_ChangeWR <- rbind(cbind(DiminishingChange=TRUE, ChangeWR_dim), cbind(DiminishingChange=FALSE, ChangeWR_nodim[ChangeWR_nodim$Parent %in% nodimParents,]))
all_ChangeWR$Relinq_Qa <- allQuant_Parent_all$Relinq_Qa[match(all_ChangeWR$Parent, allQuant_Parent_all$Parent)] ## the total diminishment for each parent (acre-ft or cfs)
all_ChangeWR$Parent_Qa <- ChangeIntentFinal$Parent_Qa[match(all_ChangeWR$Parent, ChangeIntentFinal$Parent)] ## the starting full quantity of the parent (acre-ft or cfs)
all_ChangeWR$Parent_Qa2 <- ChangeIntentFinal$Parent_Qa2[match(all_ChangeWR$Parent, ChangeIntentFinal$Parent)]
all_ChangeWR$RelRate <- ifelse(all_ChangeWR$DiminishingChange == TRUE, all_ChangeWR$Relinq_Qa / all_ChangeWR$Parent_Qa, NA) ## The per-incident reduction rate
all_ChangeWR$Pre2000 <- ifelse(as.numeric(all_ChangeWR$ChangeYear) > 33, FALSE, TRUE)
all_ChangeWR$Oldest <- allQuant_Parent_all$Oldest[match(all_ChangeWR$Parent, allQuant_Parent_all$Parent)]
all_ChangeWR$Oldest_Qa <- allQuant_Parent_all$Oldest_Qa[match(all_ChangeWR$Parent, allQuant_Parent_all$Parent)]
means_test <- subset(all_ChangeWR, DiminishingChange == TRUE)
means_test$Pre2000 <- factor(means_test$Pre2000)
means_test_2 <- subset(means_test, !(ChangeYear %in% c("(1981,1982]", "(1972,1973]")))
TukeyHSD(aov(RelRate ~ Pre2000, data=means_test)) ## compare group means of per-incident reduction rate using Tukey's Honest Significant Difference

all_ChangeWR$Relinq_Qa[all_ChangeWR$QiOnly == TRUE] <- 0 ## reset relinquishment in terms of annual quantity for the rights with diminishment in instantaneous quantity only
YearQuant <- all_ChangeWR
YearQuant$Oldest_Qa2 <- YearQuant$Oldest_Qa
## The following prevents double-counting the parent quantities due to multiple generations of changes 
dup_oldest <- unique(YearQuant$Oldest[which(duplicated(YearQuant$Oldest))]) # find duplicates of original
for (o in dup_oldest) {
  df <- subset(YearQuant, Oldest == o)
  df$col_num <- 0
  df_gen <- genealogy.ls[[as.character(o)]]$gen
  for (p in df$Parent) {
    df$col_num[df$Parent == p] <- ceiling(which(as.matrix(df_gen) == p)[1] / nrow(df_gen)) # to which generation does the parent belong in the genealogy?
  }
  for (k in unique(df$Pre2000)) { # loop over categorical variable (in this case whether the change is pre- or post-2000)
    sub_df <- subset(df, Pre2000 == k)
    sub_df <- sub_df[order(sub_df$col_num),]
    row_num.ls <- vector(length=nrow(sub_df), mode="list")
    for (p in 1:length(sub_df$Parent)) {
      row_num.ls[[p]] <- which(as.matrix(df_gen) == sub_df$Parent[p]) %% nrow(df_gen) # row numbers in the genealogy where the parent can be found
    }
    for (p in 1:length(sub_df$Parent)) {
      if (p == 1) {
        sum_Parent <- sub_df$Parent_Qa2[1]
      } else {
        # Do not add the parent quantity if it is part of a later generation and it is in the same row as another parent in the genealogy
        sum_Parent <- sum_Parent + ifelse(any(row_num.ls[[p]] %in% unlist(row_num.ls[-p])) & sub_df$col_num[p] > min(sub_df$col_num), 0, sub_df$Parent_Qa2[p])
      }
    }
    sum_Parent <- min(sum_Parent, ChangeIntentFinal$Oldest_Qa[ChangeIntentFinal$Oldest==o][1]) # The parent sum for a genealogy cannot exceed the quantity of the original parent
    YearQuant$Oldest_Qa2[YearQuant$Oldest == o & YearQuant$Pre2000 == k] <- sum_Parent
  }
}
YearQuant <- subset(YearQuant, nonconsumptive == FALSE) # We omit non-consumptive rights and rights with diminishment in Qi only when calculating the aggregate volume of diminishment and rate of diminishment
YearQuant.df <- cbind(aggregate(YearQuant$Relinq_Qa, list(YearQuant$Pre2000, YearQuant$Oldest), sum),
                         aggregate(YearQuant$Oldest_Qa2, list(YearQuant$Pre2000, YearQuant$Oldest), mean)[,3]) ## 6465112
names(YearQuant.df) <- c("Period", "Oldest", "Relinq_Qa", "Oldest_Qa")
YearQuant.df <- aggregate(YearQuant.df[,c("Relinq_Qa", "Oldest_Qa")], list(YearQuant.df$Period), sum)
names(YearQuant.df) <- c("Period", "Relinq_Qa", "Oldest_Qa2")
RelQuant <- aggregate(all_ChangeWR$Relinq_Qa[all_ChangeWR$nonconsumptive == FALSE], list(all_ChangeWR$ChangeYear[all_ChangeWR$nonconsumptive == FALSE]), sum) ## total diminishment per year
allParentQuant <- aggregate(all_ChangeWR$Parent_Qa2[all_ChangeWR$nonconsumptive == FALSE], list(all_ChangeWR$ChangeYear[all_ChangeWR$nonconsumptive == FALSE]), sum) ## sum of changed parent quantities per year
RelQuantFrac <- aggregate(all_ChangeWR$RelRate, list(all_ChangeWR$ChangeYear), function(x) mean(x, na.rm=T)) ## average per-incident reduction rate per year

change.df <- data.frame(ChangeYear=names(table(all_ChangeWR$ChangeYear)), Total=as.numeric(table(all_ChangeWR$ChangeYear)), Relinquished=as.numeric(table(all_ChangeWR[all_ChangeWR$DiminishingChange == TRUE,]$ChangeYear)))
change.df$FracDim <- round(change.df[,3] / change.df$Total, 3) # forfeiture rate

change.stack <- data.frame(ChangeYear=change.df$ChangeYear, stack(change.df)[-c(1:nrow(change.df)),])
change.stack$values <- as.numeric(change.stack$values)

change.df <- change.df[-which(change.df$ChangeYear == "(2019,2020]"),]
change.df$Mag <- RelQuant[match(change.df$ChangeYear, RelQuant[,1]),2]
change.df$Mag[is.na(change.df$Mag)] <- 0
change.df$RelRate <- RelQuantFrac[match(change.df$ChangeYear, RelQuantFrac[,1]),2]
change.df$RelRate[is.na(change.df$RelRate)] <- 0
change.df$ParentQa <- allParentQuant[match(change.df$ChangeYear, allParentQuant[,1]),2]
change.df$ParentQa[is.na(change.df$ParentQa)] <- 0

change_Pre2000 <- change.df[1:33,]
change_Post2000 <- change.df[34:53,]

### Table 3. Comparison of forfeiture between pre-2000 and post-2000 change authorizations

change_year_table <- data.frame(rbind(c(sum(change_Pre2000$Total), sum(change_Pre2000$Relinquished), sum(change_Pre2000$Relinquished) / sum(change_Pre2000$Total),
                                        sum(change_Pre2000$Mag), mean(all_ChangeWR$RelRate[all_ChangeWR$Pre2000 == TRUE], na.rm=T), YearQuant.df$Relinq_Qa[YearQuant.df$Period == TRUE] / YearQuant.df$Oldest_Qa2[YearQuant.df$Period == TRUE]),
                                      c(sum(change_Post2000$Total), sum(change_Post2000$Relinquished), sum(change_Post2000$Relinquished) / sum(change_Post2000$Total),
                                        sum(change_Post2000$Mag), mean(all_ChangeWR$RelRate[all_ChangeWR$Pre2000 == FALSE], na.rm=T), YearQuant.df$Relinq_Qa[YearQuant.df$Period == FALSE] / YearQuant.df$Oldest_Qa2[YearQuant.df$Period == FALSE])))
names(change_year_table) <- c("AllParents", "Relinquished", "Freq", "Mag", "RelRate", "AggRate")
row.names(change_year_table) <- c("Pre2000", "Post2000")

### Fig. 4. Plot of forfeiture over time ######################################

change.df$Mag[change.df$ChangeYear == "(2009,2010]"] <- 125000  ## Trim this large relinquishment volume for visibility on the graph

xlab <- rep(seq(from=1967, to=2019, by=2), each=2)
xlab[seq(from=2, to=length(xlab), by=2)] <- " "
text_size <- 12
plot_theme <- theme(axis.text.x=element_text(angle=90, vjust=0.5, color="black", size=text_size), 
                    axis.text.y=element_text(color="black", size=text_size), axis.title=element_text(color="black", size=text_size+2),
                    title=element_text(color="black", size=text_size+2))

pDim <- ggplot(change.df[,], aes(x=ChangeYear, y=Relinquished)) + geom_col(fill="black") +
  plot_theme + xlab("") + ylab("Frequency (count)") + ggtitle("(a)") +
  scale_x_discrete(label=xlab) + scale_y_continuous(expand=expansion(mult=c(0,0.05)))
pFracDim <- ggplot(change.df[,], aes(x=ChangeYear, y=FracDim)) + geom_col(fill="black") +
  plot_theme + xlab("") + ylab("Frequency (rate)") + ggtitle("(b)") +
  scale_x_discrete(label=xlab) + scale_y_continuous(expand=expansion(mult=c(0,0.05)))
pMag <- ggplot(change.df[,], aes(x=ChangeYear, y=Mag/1000)) + geom_col(fill="black") + 
  plot_theme + xlab("") + ylab("Magnitude (thousand acre-ft)") + ggtitle("(c)") +
  scale_x_discrete(label=xlab) + scale_y_continuous(expand=expansion(mult=c(0,0))) + annotation_custom(grobTree(textGrob("*", x=0.84, y=0.95, gp=gpar(col="red", fontsize=18))))
pMagFrac <- ggplot(change.df[,], aes(x=ChangeYear, y=RelRate)) + geom_col(fill="black") +
  plot_theme + xlab("") + ylab("Magnitude (reduction rate)") + ggtitle("(d)") +
  scale_x_discrete(label=xlab) + scale_y_continuous(expand=expansion(mult=c(0,0.05)))

p1 <- grid.arrange(pDim, pFracDim, pMag, pMagFrac, ncol=2) 

jpeg("Figures/timeline_plot.jpg", width=6000, height=4500, units="px", res=600)
plot(p1)
dev.off()

## Relinquishment quantity by WRIA (Figure 5) ##

diminishment_id <- "DiminishingChange"
WRIA_Qa.df <- WRIA_quants()
WRIA.poly <- st_read(dsn="input_maps/WR_WAT_WRIA.gdb", layer="Water_Resource_Inventory_Areas")
Region.poly <- st_read(dsn="input_maps/ECY_LOC_EcologyRegions.gdb")
Region.poly$ECY_REGION_CD = factor(Region.poly$ECY_REGION_CD, levels=c("SWRO", "NWRO", "CRO", "ERO"))
WRIA_reg <- read.csv("input_data_files/WRIA_region_table.csv")
WRIA.poly$Relinqd <- WRIA_Qa.df$Relinquished[match(WRIA.poly$WRIA_NR, WRIA_Qa.df$WRIA_ID)]
WRIA.poly$Quant <- WRIA_Qa.df$Relinq_Qa[match(WRIA.poly$WRIA_NR, WRIA_Qa.df$WRIA_ID)]
WRIA.poly$FractionWR <- WRIA_Qa.df$Freq[match(WRIA.poly$WRIA_NR, WRIA_Qa.df$WRIA_ID)]
WRIA.poly$FractionQa <- WRIA_Qa.df$RelRate[match(WRIA.poly$WRIA_NR, WRIA_Qa.df$WRIA_ID)]
WRIA.poly$logQuant <- ifelse(WRIA.poly$Quant == 0, 1, WRIA.poly$Quant)
WRIA.poly$logRelinqd <- ifelse(WRIA.poly$Relinqd ==0, 1, WRIA.poly$Relinqd)
WRIA.poly$Reg <- WRIA_reg$Reg[match(WRIA.poly$WRIA_NM, WRIA_reg$WRIA_NM)]
WRIA.poly$Reg <- factor(WRIA.poly$Reg, levels=c("SWRO", "NWRO", "CRO", "ERO"))

plot_colors <- c("lightblue", "orange", "darkred")
background <- theme_void()
text_size <- 6
legend_height <- 0.1
legend_height2 <- 0.17
legend_width <- 0.1
ln_wd <- 1
pWR <- ggplot() +
  geom_sf(data=WRIA.poly, aes(fill=logRelinqd)) +
  scale_fill_gradientn(colors=plot_colors, trans="log", breaks=c(2, 15, 30, 60, 120), name="") + 
  geom_sf(data=Region.poly, fill=NA, lwd=ln_wd, aes(colour=ECY_REGION_CD), inherit.aes=F) +
  scale_color_manual(values=c("purple","darkblue","black","forestgreen"), name="Region") +
  background + xlab("") + ylab("") + 
  theme(text=element_text(size=text_size), legend.text=element_text(size=text_size)) + ggtitle("(a)") +
  theme(legend.key.height=unit(legend_height,"inch"), legend.key.width=unit(legend_width,"inch"))
pWRFrac <- ggplot() +
  geom_sf(data=WRIA.poly, aes(fill=FractionWR)) +
  scale_fill_gradientn(colors=plot_colors, name="") + 
  geom_sf(data=Region.poly, fill=NA, lwd=ln_wd, aes(colour=ECY_REGION_CD), inherit.aes=F, show.legend=F) +
  scale_color_manual(values=c("purple","darkblue","black","forestgreen"), name="Region") +
  background + xlab("") + ylab("") + 
  theme(text=element_text(size=text_size), legend.text=element_text(size=text_size)) + ggtitle("(b)") +
  theme(legend.key.height=unit(legend_height2,"inch"), legend.key.width=unit(legend_width,"inch"))
pQuant <- ggplot() +
  geom_sf(data=WRIA.poly, aes(fill=logQuant)) +
  scale_fill_gradientn(colors=plot_colors, trans="log", breaks=c(10, 100, 1000, 10000, 10000,300000), name="Acre-feet") + 
  geom_sf(data=Region.poly, fill=NA, lwd=ln_wd, aes(colour=ECY_REGION_CD), inherit.aes=F, show.legend=F) +
  scale_color_manual(values=c("purple","darkblue","black","forestgreen"), name="Region") +
  background + xlab("") + ylab("") + 
  theme(text=element_text(size=text_size), legend.text=element_text(size=text_size)) + ggtitle("(c)") +
  theme(legend.key.height=unit(legend_height2,"inch"), legend.key.width=unit(legend_width,"inch"))
pQuantFrac <- ggplot() +
  geom_sf(data=WRIA.poly, aes(fill=FractionQa)) +
  scale_fill_gradientn(colors=plot_colors, name="") +
  geom_sf(data=Region.poly, fill=NA, lwd=ln_wd, aes(colour=ECY_REGION_CD), inherit.aes=F, show.legend=F) +
  scale_color_manual(values=c("purple","darkblue","black","forestgreen"), name="Region") +
  background + xlab("") + ylab("") + 
  theme(text=element_text(size=text_size), legend.text=element_text(size=text_size)) + ggtitle("(d)") +
  theme(legend.key.height=unit(legend_height2,"inch"), legend.key.width=unit(legend_width,"inch"))

jpeg("Figures/diminishment_by_WRIA.jpeg", res=400, width=2000, height=1000, units="px") ## Fig. 5
grid.arrange(pWR, pWRFrac, pQuant, pQuantFrac, ncol=2)
dev.off()

##### Relinquishment by change intent type ##########################

cat.ls <- list("AddIrrigatedArea", "WRAOther", "AddPurpose", "ChangePurpose", "ChangePlaceOfUse", c("ChangeSource", "AddSource"), c("ChangeOther", "Consolidate"))
cat_names <- c("AddIrrigatedArea", "WaterRightAcquisition", "AddPurpose", "ChangePurpose", "ChangePlaceOfUse", "AddOrChangeSource", "ChangeOther")
ByIntent <- ChangeIntentTable(cat.ls, cat_names)
IntentTypeTable <- ByIntent[[1]] ## Table 8
IntentQuant <- ByIntent[[2]]

intent_rel <- IntentTypeTable$Relinquished
names(intent_rel) <- IntentTypeTable$IntentType
intent_all <- IntentTypeTable$AllParents
names(intent_all) <- IntentTypeTable$IntentType
## pairwise comparison test of proportions with correction for multiple comparisons
intent_test <- pairwise.prop.test(x=intent_rel, n=intent_all)$p.value
intent_test <- ifelse(is.na(intent_test), "-", ifelse(intent_test<0.001, "<0.001", ifelse(round(intent_test, 4) == 1, ">0.999", round(intent_test, 3))))

means_test <- subset(IntentQuant, DiminishingChange == TRUE)
TukeyHSD(aov(RelRate ~ IntentType, data=means_test))

#### Relinquishment by ownership ###########################

ByOwner <- PersonTypeTable()
ChangePersonType <- ByOwner[[1]] ## Table 5
PersonType <- ByOwner[[2]]

means_test <- subset(PersonType, DiminishingChange == TRUE)
TukeyHSD(aov(RelRate ~ Parent_PersonClass, data=means_test))

person_rel <- ChangePersonType$Relinquished
names(person_rel) <- ChangePersonType$PersonType
person_all <- ChangePersonType$AllParents
names(person_all) <- ChangePersonType$PersonType
person_test <- pairwise.prop.test(x=person_rel, n=person_all)$p.value
person_test <- ifelse(is.na(person_test), "-", ifelse(person_test<0.001, "<0.001", ifelse(round(person_test, 4) == 1, ">0.999", round(person_test, 3))))

#### Relinquishment by purpose of use type ###########################

purpose_types = list("IR", "DG|DM|DS", "MU", "CI", "FS", "PO")
purpose_names <- c("Irrigation", "Domestic", "Municipal", "Commercial", "Fish", "Power")

ByPurpose <- PurposeTypeTable(purpose_types, purpose_names)
PurposeTable <- ByPurpose[[1]] ## Table 6
PurposeQuant <- ByPurpose[[2]]

means_test <- subset(PurposeQuant, DiminishingChange == TRUE)
TukeyHSD(aov(RelRate ~ Purpose, data=means_test))

purpose_rel <- PurposeTable$Relinquished
names(purpose_rel) <- PurposeTable$Purpose
purpose_all <- PurposeTable$All
names(purpose_all) <- PurposeTable$Purpose
purpose_test <- pairwise.prop.test(x=purpose_rel, n=purpose_all)$p.value
purpose_test <- ifelse(is.na(purpose_test), "-", ifelse(purpose_test<0.001, "<0.001", round(purpose_test, 3)))

## Relinquishment by source

BySource <- SourceTypeTable()
SourceTable <- BySource[[1]] ## Table 7
SourceQuant <- BySource[[2]]

source_rel <- SourceTable$Relinquished
names(source_rel) <- SourceTable$SourceType
source_all <- SourceTable$AllParents
names(source_all) <- SourceTable$SourceType
source_test <- pairwise.prop.test(x=source_rel, n=source_all)$p.value
source_test <- ifelse(is.na(source_test), "-", ifelse(source_test<0.001, "<0.001", round(source_test, 3)))

means_test <- subset(SourceQuant, DiminishingChange == TRUE)
TukeyHSD(aov(RelRate ~ ParentSource, data=means_test))

## Relinquishment by phase

ChangePhase <- unique(ChangeIntentFinal[ChangeIntentFinal$Parent %in% post2000_parents,c("DiminishingChange", "Parent", "ParentPhase", "WRIA", "nonconsumptive", "QiOnly")])
get_phase <- function(x) {
  phases <- strsplit(ChangePhase$ParentPhase[x], "\\|")[[1]]
  phases <- gsub("SupersedingQuincyBasinPermit", "SupersedingPermit", phases)
  phases <- gsub("LongForm", "Claim", phases)
  phases <- gsub("QuincyBasinPermit", "Permit", phases)
  p <- ifelse("AdjudicatedCertificate" %in% phases, "AdjudicatedCertificate", ifelse("Certificate" %in% phases, "Certificate", ifelse("Permit" %in% phases, "Permit", 
    ifelse("CertificateOfChange" %in% phases, "CertificateOfChange", ifelse("SupersedingPermit" %in% phases,
    "SupersedingPermit", ifelse("SupersedingCertificate" %in% phases, "SupersedingCertificate",
    ifelse("Claim" %in% phases, "Claim", ifelse("ChangeROE" %in% phases, "ChangeROE", "Other"))))))))
  return(p)
}
ChangePhase$ParentPhase <- sapply(1:nrow(ChangePhase), function(x) get_phase(x))
ChangePhase$source <- water_rights$RCWClass[match(ChangePhase$Parent, water_rights$WRDocID)]

doc_id <- c(2285521, 2032464, 2032815, 2032816, 2032842, 2033001, 2084815, 2086741, 2087175, 2087179, 2087222,
            2087326, 2087327, 2087328, 2088725, 2088970, 2088985, 2088998, 2089134, 2089831, 2129456,
            2129457, 2129458, 2129459, 2129460, 2129555, 2129633, 2129719, 2129745, 2129746, 2139425,
            2143328, 2143413, 2143414, 2143416, 2143461, 2143462, 2144978, 2206420, 2221053, 2223965,
            2285521, 4145385, 4145398, 4145428, 4145442, 4149821, 4149841, 4169831, 4181953, 4185014,
            4187905, 4212257, 4214613, 4224896, 4228121, 4257926, 4258362, 4262601, 4276116, 4285470,
            4320949, 4330572, 4330602, 4330624, 4342856, 4406587, 4410347, 4468203, 4468215, 4485788,
            4485839, 4499109, 4520026, 4545674, 4551681, 4551696, 4551710, 4557974, 4565533, 4581333,
            4658705, 4663973, 4667602, 4673895, 4681816, 4686848, 4697707, 4701466, 4702287, 4723280,
            4723377, 4723388, 4845661, 4999394, 5039162, 5039255, 5039308, 5080117, 5080279, 5118025,
            5118046, 5318011, 5355287, 5443374, 5523805, 5524229, 5565158, 5566151, 5566239, 5566485,
            5601720, 5646531, 5724062, 5802446, 5843236, 5885522, 5885532, 6129459, 6260823, 6260889,
            6260913, 6705950, 6727585, 6753618, 6799702, 6799849, 6799850, 6800841, 6801438, 6802302,
            2032386, 2032389, 2283051, 4175388, 4273606, 4323697, 4424669, 4512899, 4615661, 4676713,
            2076416, 2082089, 2089019, 2129581, 2129633, 2129714, 2135928, 2135963, 2137322, 2137896,
            2138311, 2139049, 2139574, 2140432, 2140492, 2140544, 2140613, 2140730, 2140912, 2141069,
            2141069, 2141123, 2141333, 2141415, 2141451, 2141625, 2141773, 2141901, 2142739, 2142741,
            2142756, 2142771, 2142781, 2142994, 2143044, 2143175, 2143178, 2143195, 2143322, 2143324,
            2143386, 2143387, 2143481, 2143585, 2144552, 2144597, 2144660, 2144682, 2144763, 2144763,
            2144781, 2144785, 2144787, 2144788, 2144795, 2144838, 2145499, 2221106, 2278372, 2279517,
            4162842, 4199701, 4588490, 5000985,
            subset(ChangePhase, ParentPhase == "ChangeROE" & WRIA %in% c("Lower Yakima", "Upper Yakima", "Naches") & source == "surfaceWater")$Parent)
            
phase <- c("Permit", "Claim", "Certificate", "Claim", "Certificate", "Certificate", "Claim", "Certificate", "Claim", "Claim", "Certificate",
           "Claim", "Claim", "Claim", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate",
           "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "SupersedingCertificate", "Certificate", "Certificate", "Certificate", "SupersedingCertificate",
           "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "SupersedingCertificate", "Certificate", "Permit", "Certificate",
           "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate",
           "Certificate", "Certificate", "Claim", "SupersedingCertificate", "CertificateOfChange", "Certificate", "Certificate", "SupersedingCertificate", "Certificate", "AdjudicatedCertificate",
           "Certificate", "Certificate", "Certificate", "Certificate", "Permit", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate",
           "AdjudicatedCertificate", "Claim", "Claim", "Certificate", "Certificate", "Certificate", "SupersedingCertificate", "CertificateofChange", "Certificate", "Certificate",
           "Certificate", "Certificate", "SupersedingCertificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate",
           "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Permit", "Certificate", "CertificateofChange", "Certificate",
           "SupersedingCertificate", "Certificate", "Certificate", "SupersedingCertificate", "SupersedingCertificate", "Certificate", "Certificate", "AdjudicatedCertificate", "AdjudicatedCertificate", "CertificateOfChange",
           "Certificate", "Certificate", "Certificate", "SupersedingCertificate", "Certificate", "SupersedingCertificate", "SupersedingCertificate", "Certificate", "Certificate", "SupersedingCertificate",
           "Certificate", "Certificate", "Certificate", "Certificate", "AdjudicatedCertificate", "Certificate", "Certificate", "Certificate", "SupersedingCertificate", "Certificate",
           "Certificate", "Certificate", "SupersedingPermit", "Certificate", "Claim", "Certificate", "Certificate", "Permit", "Certificate", "Claim",
           "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate",
           "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate",
           "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate",
           "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate",
           "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate",
           "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate",
           "Certificate", "Certificate", "Certificate", "Certificate",
           rep("CourtClaim", nrow(subset(ChangePhase, ParentPhase == "ChangeROE" & WRIA %in% c("Lower Yakima", "Upper Yakima", "Naches") & source == "surfaceWater")))) 
doc_id <- subset(ChangePhase, ParentPhase == "ChangeROE" & WRIA %in% c("Lower Yakima", "Upper Yakima", "Naches") & source == "surfaceWater")$Parent
phase <- rep("CourtClaim", nrow(subset(ChangePhase, ParentPhase == "ChangeROE" & WRIA %in% c("Lower Yakima", "Upper Yakima", "Naches") & source == "surfaceWater")))
replace_cc <- c(2079718, 2079707, 2079735, 2079668, 2078275, 2078215, 2078907, 4749470, 4749720, 4749913, 
                2079699, 4766773, 2079706, 2079707, 2079710, 2079699, 2079672, 2079671, 2079669, 2079668,
                2079720, 2079716, 2079729, 2079730, 2079735, 2079742, 2079753, 2079754, 2079769, 4760180,
                4743052, 4742795, 4749804, 4749720, 4749346, 4749292, 4742074, 4745086, 4749470, 4749913,
                4767186, 4749265, 4749253, 4742586, 4746191, 2079700, 4764410, 4759363, 4759363, 6589159,
                6801350, 6801349, 6801348, 6801333, 6801332, 4748622, 4748709, 4766237, 4766969, 5545698,
                4766969, 4755779, 4758627, 4758655, 4758870, 6589159, 4750516, 4766215, 2085239, 2085249,
                2085256, 2087135, 2087168, 2087183, 4147092, 4147258, 4147269, 4206123, 4236779, 4237360,
                4270542, 4272202, 4442887, 4538034, 4275467, 6800808, 6800804, 6800718, 6799756, 6045285,
                5683000, 5002361, 4883444, 4717239, 4716807, 2087659, 2084770, 4228166, 4423480, 4644814,
                4644821, 2086749, 2086753, 2087126, 4145294, 4180345, 4180695, 4180725, 4185859, 4199804,
                4199855, 4210910, 4210953, 4231963, 4247882, 4304929, 4442716, 4463697, 4652940, 4810323,
                5160277, 5843148, 4541298, 2079675, 2079703, 2079736) #Yak adj. certs. that parents but should be children of court claims
docid <- c(2079489, 2140432, 2144838, 2141333, 4214774, 4214788, 2145865, 2145231, 2139636, 2144597,
           2137283, 2138311, 2144787, 2143322, 2143514, 2143823, 2137635, 2143178, 2143175, 2141415,
           2140912, 2139574, 2144795, 4588490, 4199701, 2141773, 2144788, 2137322, 2140730, 2089019,
           2089781, 2143211, 2143195, 2075155, 2144393, 2282938, 4305326, 4448917, 4654759, 6271529,
           2137449, 2135963, 2141625, 2143044, 2144785, 2144762, 2141451, 2142709, 2143481, 2137896,
           2283763, 4886561, 2143537, 4162842, 2145925, 2142756, 2221106, 2135928, 2139049, 2140613,
           2142739, 2142741, 2142771, 2142781, 2142994, 2144552, 2144587, 2144660, 2144674, 2144675,
           2144715, 2144829, 2144966, 2145089, 2147167, 4157687, 4398749, 2143387, 2143386, 2143570,
           2283079, 4714441, 2218226, 2271815, 2144682, 4188909, 4188983, 2221558, 2279517, 4666772,
           2088699, 2143386, 2143387, 2143585, 2144607, 2144608, 2144623, 2144781, 2215326, 2220478,
           2222643, 2223421, 4303986, 4147694, 2086891, 6048594, 4237120, 6799909, 6800900, 4147694,
           4188983, 2271815, 4189021, 2281227, 4188909, 2279517, 2278372, 2223421, 2221106, 4162842,
           2221558, 5160931, 4664538, 2222643, 2088699, 2143570, 4200712, 2142771, 2142781, 2144587,
           2139049, 2135928, 5121478, 5724721, 4727747, 2091775, 2144781, 2144787, 2130902, 2145499,
           2144552, 2141069, 2139574, 4588490, 2089019, 2141901, 2141451, 2129581, 2142994, 2143386,
           2137322, 2135963, 2143324, 2143044, 2143322, 2143481, 2143211, 4174101, 4204764, 4204730,
           4204713, 2141415, 2141069, 5728134, 5728224, 4541455, 4541437, 4303986, 4697623, 5728224,
           5886633, 5886598, 2129600, 2144660, 2140613, 2141123, 2143175, 2143178, 2144788, 2140730,
           2129714, 2144467, 2144466, 2095324, 2144387, 4235961, 4236000, 4220755, 6799365, 4169452,
           5767482, 4350751, 2283763, 4630317, 2283620, 2081626, 2081772, 2082998, 2224203, 2278495,
           2085410, 2141123, 2140746, 2285159, 2271882, 2279052, 2078958, 2278495, 2279874, 2278316,
           2279157, 2280478)
newPhase <- c("SupersedingCertificate", "Certificate", "Certificate", "Certificate", "SupersedingCertificate", "SupersedingCertificate", "ChangeROE", "Certificate", "Certificate", "Certificate",
              "Certificate", "Certificate", "Certificate", "Certificate", "Permit", "Permit", "Certificate", "Certificate", "Certificate", "Certificate",
              "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate",
              "Permit", "Permit", "Certificate", "SupersedingPermit", "Permit", "SupersedingPermit", "SupersedingPermit", "SupersedingPermit", "SupersedingPermit", "SupersedingPermit",
              "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate",
              "Permit", "Permit", "Permit", "Permit", "Permit", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate",
              "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Permit", "Certificate", "Permit", "Permit",
              "Permit", "Permit", "Permit", "Permit", "Permit", "Permit", "Permit", "Certificate", "Certificate", "Permit",
              "Permit", "Certificate", "Certificate", "Certificate", "Certificate", "Permit", "Permit", "Permit", "Certificate", "Permit",
              "Permit", "Certificate", "Certificate", "Certificate", "Permit", "Permit", "Permit", "Certificate", "Permit", "Certificate",
              "Permit", "Permit", "Certificate", "Claim", "CourtClaim", "CourtClaim", "CourtClaim", "CertificateOfChange", "SupersedingAdjudicatedCertificate", "Claim",
              "Permit", "Certificate", "Permit", "SupersedingPermit", "Permit", "Certificate", "Certificate", "Permit", "Certificate", "Permit",
              "Permit", "Certificate", "Permit", "Permit", "Permit", "Permit", "Certificate", "Certificate", "Certificate", "Permit",
              "Certificate", "Certificate", "Certificate", "Permit", "Certificate", "Permit", "Certificate", "Certificate", "SupersedingCertificate", "Certificate",
              "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate",
              "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Permit", "Certificate", "Certificate", "SupersedingCertificate",
              "Certificate", "Certificate", "Certificate", "Certificate", "SupersedingCertificate", "SupersedingCertificate", "Certificate", "Certificate", "SupersedingCertificate", "SupersedingCertificate",
              "SupersedingCertificate", "Certificate", "Permit", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate",
              "Certificate", "Permit", "Permit", "Certificate", "Permit", "Certificate", "Certificate", "Certificate", "Permit", "Permit", 
              "Permit", "Permit", "Permit", "Permit", "Permit", "SupersedingCertificate", "SupersedingCertificate", "SupersedingCertificate", "Permit", "Certificate",
              "Permit", "Certificate", "Certificate", "Permit", "Permit", "Certificate", "Certificate", "Certificate", "Certificate", "Certificate",
              "Certificate", "Certificate")  # parents with both the certificate and superseding certificate, when the change was on the certificate
to_permit <- c(2085330, 2084675, 2084698, 2084862, 2086871, 2086873, 2089304, 2095308, 2095317, 2129482, 
               2143509, 2143737, 2144255, 2144331, 2144541, 2146083, 2222371, 2282734, 2283072, 
               2283349, 2283549, 2283631, 2283751, 2283899, 2283900, 4168805, 2221735, 2221561, 
               2221469, 2144110, 2144921, 2215606, 2221596, 2283615, 4156380, 2207743, 4188933, 
               6799551, 2147167, 2222714, 2221734, 4189506, 2129279, 
               2144482, 2144677, 2084679, 2283050, 2283156, 2032415, 2283262, 
               2282938, 2283715, 2143568, 2143940) # changes happened while certificate was still in the permitting phase

phase_replace2 <- data.frame(docid=docid, phase=newPhase)
phase_replace <- data.frame(docid=doc_id, phase=phase)
row_num <- which(ChangePhase$Parent %in% phase_replace$docid)
ChangePhase$ParentPhase[row_num] <- phase_replace$phase[match(ChangePhase$Parent[row_num], phase_replace$docid)]
ChangePhase$ParentPhase[which(ChangePhase$Parent %in% replace_cc)] <- "CourtClaim"
row_num <- which(ChangePhase$Parent %in% phase_replace2$docid)
ChangePhase$ParentPhase[row_num] <- phase_replace2$phase[match(ChangePhase$Parent[row_num], phase_replace2$docid)]
ChangePhase$ParentPhase[which(ChangePhase$Parent %in% to_permit)] <- "Permit"
ChangePhase$ParentPhase[ChangePhase$ParentPhase %in% c("SupersedingPermit", "SupersedingCertificate", "SupersedingAdjudicatedCertificate", "CertificateOfChange", "ChangeROE")] <- "SupersedingDoc"

ChangePhase$Relinq_Qa <- allQuant_Parent$Relinq_Qa[match(ChangePhase$Parent, allQuant_Parent$Parent)]
ChangePhase$Parent_Qa <- allQuant_Parent$Parent_Qa[match(ChangePhase$Parent, allQuant_Parent$Parent)]
diminished <- unique(ChangePhase$Parent[ChangePhase$DiminishingChange == TRUE])
nodiminished <- unique(ChangePhase$Parent[ChangePhase$DiminishingChange == FALSE])
rm_parents <- nodiminished[which(nodiminished %in% diminished)] 
ChangePhase <- ChangePhase[-which(ChangePhase$Parent %in% rm_parents & ChangePhase$DiminishingChange == FALSE),]
ChangePhase$RelRate <- ifelse(ChangePhase$DiminishingChange == TRUE, ChangePhase$Relinq_Qa / ChangePhase$Parent_Qa, NA)
PhaseTable <- cbind(aggregate(ChangePhase$DiminishingChange, list(ChangePhase$ParentPhase), length),
  aggregate(ChangePhase$DiminishingChange, list(ChangePhase$ParentPhase), sum)[,2],
  aggregate(ChangePhase$RelRate, list(ChangePhase$ParentPhase), function(x) mean(x, na.rm=T))[,2])
names(PhaseTable) <- c("PhaseType", "AllParents", "Relinquished", "RelRate")
PhaseTable$Freq <- PhaseTable$Relinquished / PhaseTable$AllParents
PhaseTable <- PhaseTable[,c(1,2,3,5,4)]

PhaseQuant <- ChangePhase
PhaseQuant$Oldest <- allQuant_Parent$Oldest[match(PhaseQuant$Parent, allQuant_Parent$Parent)]
PhaseQuant$Oldest_Qa <- ChangeIntentFinal$Oldest_Qa[match(PhaseQuant$Parent, ChangeIntentFinal$Parent)]
dup_oldest <- PhaseQuant$Oldest[which(duplicated(PhaseQuant$Oldest))]
PhaseQuant$Oldest_Qa2 <- PhaseQuant$Oldest_Qa
PhaseQuant$Parent_Qa2 <- allQuant_Parent$Parent_Qa2[match(PhaseQuant$Parent, allQuant_Parent$Parent)]
for (o in dup_oldest) {
  df <- subset(PhaseQuant, Oldest == o)
  df$col_num <- 0
  df_gen <- genealogy.ls[[as.character(o)]]$gen
  for (p in df$Parent) {
    df$col_num[df$Parent == p] <- ceiling(which(as.matrix(df_gen) == p)[1] / nrow(df_gen)) # to which generation does the parent belong in the genealogy?
  }
  for (k in unique(df$ParentPhase)) {
    sub_df <- subset(df, ParentPhase == k)
    sub_df <- sub_df[order(sub_df$col_num),]
    row_num.ls <- vector(length=nrow(sub_df), mode="list")
    for (p in 1:length(sub_df$Parent)) {
      row_num.ls[[p]] <- which(as.matrix(df_gen) == sub_df$Parent[p]) %% nrow(df_gen) # row numbers in the genealogy where the parent can be found
    }
    for (p in 1:length(sub_df$Parent)) {
      if (p == 1) {
        sum_Parent <- sub_df$Parent_Qa2[1]
      } else {
        # Do not add the parent quantity if it is part of a later generation and it is in the same row as another parent in the genealogy
        sum_Parent <- sum_Parent + ifelse(any(row_num.ls[[p]] %in% unlist(row_num.ls[-p])) & sub_df$col_num[p] > min(sub_df$col_num), 0, sub_df$Parent_Qa2[p])
      }
    }
    sum_Parent <- min(sum_Parent, ChangeIntentFinal$Oldest_Qa[ChangeIntentFinal$Oldest==o][1]) # The parent sum for a genealogy cannot exceed the quantity of the original parent
    PhaseQuant$Oldest_Qa2[PhaseQuant$Oldest == o & PhaseQuant$ParentPhase == k] <- sum_Parent
  }
}
rownum <- which(PhaseQuant$nonconsumptive == FALSE)
PhaseQuant$Relinq_Qa[PhaseQuant$QiOnly == TRUE] <- 0
PhaseQuant.df <- cbind(aggregate(PhaseQuant$Relinq_Qa[rownum], list(PhaseQuant$ParentPhase[rownum], PhaseQuant$Oldest[rownum]), sum),
  aggregate(PhaseQuant$Oldest_Qa2[rownum], list(PhaseQuant$ParentPhase[rownum], PhaseQuant$Oldest[rownum]), mean)[,3]) 
names(PhaseQuant.df) <- c("PhaseType", "Oldest", "Relinq_Qa", "Oldest_Qa2")
PhaseQuant.df <- aggregate(PhaseQuant.df[,c("Relinq_Qa", "Oldest_Qa2")], list(PhaseQuant.df$PhaseType), function(x) sum(x, na.rm=T))
names(PhaseQuant.df) <- c("PhaseType", "Relinq_Qa", "Oldest_Qa2")
PhaseQuant.df$AggRate <- PhaseQuant.df$Relinq_Qa / PhaseQuant.df$Oldest_Qa2
PhaseTable$AggRate <- PhaseQuant.df$AggRate[match(PhaseTable$PhaseType, PhaseQuant.df$PhaseType)]
PhaseTable$Relinq_Qa <- round(PhaseQuant.df$Relinq_Qa[match(PhaseTable$PhaseType, PhaseQuant.df$PhaseType)], 0)
phase_order <- c("Claim", "Permit", "Certificate", "AdjudicatedCertificate", "SupersedingDoc", "CourtClaim")
PhaseTable <- PhaseTable[match(phase_order, PhaseTable$PhaseType),c(1:4,7,5,6)]  ## Table 4
PhaseTable[,-c(1:3,5)] <- apply(PhaseTable[,-c(1:3,5)], 2, function(x) round(x, 3))

means_test <- subset(PhaseQuant, DiminishingChange == TRUE)
TukeyHSD(aov(RelRate ~ ParentPhase, data=means_test))

rel_phase <- PhaseTable$Relinquished
names(rel_phase) <- PhaseTable$PhaseType
all_phase <- PhaseTable$AllParents
names(all_phase) <- PhaseTable$PhaseType
test <- pairwise.prop.test(x=rel_phase, n=all_phase)
test.table <- test$p.value
test.table <- ifelse(is.na(test.table), "-",ifelse(test.table<0.001, "<0.001", ifelse(round(test.table, 8) == 1, ">0.999", round(test.table, 3))))

### Show cause ##########

show_cause <- read.csv("cleaned_data/show_cause_filled.csv")
changes <- all_ChangeWR
changes <- subset(changes, ChangeYear != "(2019,2020]")

changes$Oldest_Qa <- allQuant_Parent_all$Oldest_Qa[match(changes$Parent, allQuant_Parent_all$Parent)]
changes$Oldest <- allQuant_Parent_all$Oldest[match(changes$Parent, allQuant_Parent_all$Parent)]
change_parent_sum <- sum(aggregate(changes$Oldest_Qa[changes$DiminishingChange == TRUE & changes$nonconsumptive == FALSE], list(changes$Oldest[changes$DiminishingChange == TRUE & changes$nonconsumptive == FALSE]), mean)[,2])
show_cause$Relinq_Qa <- show_cause$Qa_start_filled - show_cause$Qa_end_filled
show_cause$RelRate <- show_cause$Relinq_Qa / show_cause$Qa_start_filled
show_cause$RelRate <- show_cause$Relinq_Qa / show_cause$Qa_start_filled


## Table 9 ##
rownum <- which(show_cause$nc == "no")
show_cause_table <- rbind(c(length(show_cause$Relinq_Qa), sum(show_cause$Qa_start_filled[rownum]), sum(show_cause$Relinq_Qa[rownum]), mean(show_cause$RelRate)),
      c(length(changes$Relinq_Qa[changes$DiminishingChange==TRUE]), change_parent_sum, sum(changes$Relinq_Qa[changes$nonconsumptive == FALSE]), mean(changes$RelRate, na.rm=T)))
show_cause_table <- show_cause_table[,-2]

purpose_types = list("IR", "DG|DM|DS", "MU", "CI", "FS", "PO")
purpose_names <- c("Irrigation", "Domestic", "Municipal", "Commercial", "Fish", "Power")
show_cause_by_purpose <- PurposeTypeTable_sc(purpose_types, purpose_names) ## Table A.5

person <- read.csv("input_data_files/PersonOrOrganization.csv")
show_cause$Owner <- person$PersonOrOrganization[match(show_cause$docid, person$WaRecId)]
show_cause$Owner_class <- person_class$Class[match(show_cause$Owner, person_class$Person)]
show_cause$Owner_class <- factor(show_cause$Owner_class, levels=c("Individual", "Other Company", "Irrigation District", "Water Company", "Department/Agency", "Other District", "Club/Association", "Municipality", "Irrigation Company"))
show_cause$Owner_class[show_cause$Owner_class == "Irrigation Company"] <- "Irrigation District"

## Table A.6
owner_table <- cbind(aggregate(show_cause$Relinq_Qa, list(show_cause$Owner_class), length),
                     round(aggregate(show_cause$Relinq_Qa[show_cause$nc == "no"], list(show_cause$Owner_class[show_cause$nc == "no"]), sum)[,2], 0),
                     round(aggregate(show_cause$RelRate, list(show_cause$Owner_class), mean)[,2], 3))
names(owner_table) <- c("OwnerType", "Count", "Relinq_Qa", "RelRate")

#### Yakima Adjudication (Table 10)

yak <- read.csv("cleaned_data/Yakima_adj.csv")

yak_table <- cbind(aggregate(yak$Qa_filled, list(yak$Class), length), 
      aggregate(yak[,c("Qa_filled", "Qi_filled")], list(yak$Class), function(x) sum(x, na.rm=T))[,2:3])

(yak_table[2,2:4] - yak_table[1,2:4]) / yak_table[2,2:4]





