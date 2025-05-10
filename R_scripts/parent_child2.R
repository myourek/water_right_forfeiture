###############################################
# Code for assembling water right genealogies #
# and detecting water right forfeiture        #
# Matt Yourek                                 #  
# matthew.yourek@wsu.edu                      #
# May 9, 2025                                 # 
###############################################

#### Read in the data ##################################
setwd("~/water_right_forfeiture")
parent_child <- read.table("input_data_files/WaRecParent_adj.txt", header=T, fill=T) ## parent and child document ids
names(parent_child) <- c("Parent", "Child", "IsMitigation")
parent_child <- subset(parent_child, IsMitigation != "True") ## remove children that are mitigated by the parent (these are not changes)
oldest <- with(parent_child, which(Parent == Child | !Parent %in% Child)) ## search for 1st-gen parents.
original_parents <- unique(parent_child$Parent[oldest]) ## list of 1st-gen parents
O <- length(original_parents)

source("R_scripts/load_parent_child_functions.R") ## load functions
water_rights <- read.table("input_data_files/water_rights_all_updated_adj2.csv", sep=",", encoding="ANSI", header=T) ## water right attributes for all water right records in WRTS
water_rights$Comment <- iconv(water_rights$Comment, from="ISO-8859-1", to="UTF-8") ## rectify some formatting issues
water_rights$EventComment <- iconv(water_rights$EventComment, from="ISO-8859-1", to="UTF-8")
water_rights$Status[water_rights$Stage %in% c("Rejected", "Withdrawn")] <- "Inactive"
water_rights$QaTotal <- sapply(1:nrow(water_rights), function(x) replace_zeroes(x)) ## replace zeroes with NA
relinquishment <- read.csv("input_data_files/Relinquishment.csv") ## an older forfeiture spreadsheet, used for some initial calcs. These have been manually verified.
water_rights_cleaned <- read.csv("input_data_files/water_rights_cleaned_adj.csv") ## annual water quantities for select water rights. These have been manually verified.

############################################
#### Create genealogy with attributes ######
############################################

parent_genealogy <- get_genealogy_parent() ## create genealogy (one generation per column). All genealogies in a single dataframe.
## Collect some data about the genealogies so  we can find the most recent (youngest) document. We assemble a fuller set of attributes later.
var_prelim <- c("phase", "stage", "assignment")
match_name_prelim <- c("Phase", "Stage", "AssignmentGroup")
gen_data_prelim <- vector(mode="list", length=length(var_prelim))
names(gen_data_prelim) <- var_prelim
for (var in var_prelim) {
  gen_data_prelim[[var]] <- parent_genealogy 
}
for (i in 1:length(var_prelim)) {
  for (n in names(parent_genealogy)) {
    name_col <- match_name_prelim[i]
    gen_data_prelim[[i]][,n] <- water_rights[,name_col][match(parent_genealogy[,n], water_rights$WRDocID)]
  }
}
parent_genealogy$youngest <- sapply(1:nrow(parent_genealogy), function(x) find_youngest(x)) # Find the most recent water right(s) in a genealogy.

# Attach attributes from WRTS to the genealogies
var_list <- c("event", "event_comment", "event_date", "Qa_raw", "Qa", "purpose",
              "status", "use", "priority", "phase", "stage", "docNM", "comment", "assignment", "Qi", "IA")  # attribute list
match_name <- c("EventType", "EventComment", "EventDoneDate", "QaTotal", "Parent_Qa", "PurposeOfUse", 
                "Status", "UseType", "PriorityDate", "Phase", "Stage", "PrimaryNumber", "Comment", "AssignmentGroup", "QiTotal", "IrrAreaTotal") # name of column that corresponds to each attribute
gen_data <- vector(mode="list", length=length(var_list))
names(gen_data) <- var_list
for (var in var_list) {
  gen_data[[var]] <- parent_genealogy # Create a copy of the genealogy dataframe for each attribute.Then we fill with data from input spreadsheets.
}

for (i in 1:length(var_list)) {                      # loop over water right attributes
  for (n in names(parent_genealogy)) {               # loop through generations 
    if (var_list[i] == "Qa") {                       # lookup from a spreadsheet with manually verified quantities
      lookup.df <- water_rights_cleaned
      if (n == "Parent") { 
        doc_col <- "Parent"
        name_col <- "Parent_Qa"
      } else { 
        doc_col <- "WR_Doc_ID" 
        name_col <- "AnnualVolumeQuantity_filled"
      }
    } else {                                         # lookup from master water rights spreadsheet. Raw values, verified only in some instances.
      lookup.df <- water_rights
      doc_col <- "WRDocID"
      name_col <- match_name[i]
    }
    gen_data[[i]][,n] <- lookup.df[,name_col][match(parent_genealogy[,n], lookup.df[,doc_col])] # For each document id, copy the water right attributes from input spreadsheets.
  }
}
gen_data[["Qa"]][is.na(gen_data[["Qa"]])] <- gen_data[["Qa_raw"]][is.na(gen_data[["Qa"]])] # If a manually verified quantity is not available, we use the raw one from the master water rights spreadsheet.

## Reorganize the data, so that each 1st-gen parent is an entry in a list. 
genealogy.ls <- vector(length=O, mode="list")
names(genealogy.ls) <- original_parents
status <- gen_data[["status"]]
for (i in 1:O) {
  print(i)
  row_num <- which(parent_genealogy$Parent == original_parents[i]) 
  list1 <- vector(mode="list", length=length(var_list) + 2)
  names(list1) <- c("gen", var_list, "diminishment")
  list1[[1]] <- parent_genealogy[row_num,]
  for (var in 1:length(var_list)) {
    list1[[var+1]] <- gen_data[[var]][row_num,]
  }
  list1[[length(list1)]] <- get_diminishment(row_num)
  genealogy.ls[[i]] <- list1
}


##################################################

#####################################################
#### Create Dataframe of Change Requests by Child ###
####    Child Document and Change Intent Type     ###
#####################################################

ChangeIntent <- read.csv("input_data_files/WaRecChangeIntent.csv") ## Change intent type for each child
Process <- read.csv("input_data_files/WaterRightsTablesWaRecProcess.txt", sep="\t") ## provides crosswalk from ProcessID to WaRecID
ChangeIntent <- merge(ChangeIntent, Process[,c("WaRecProcessId", "WaRecId")], "WaRecProcessId", all=T) # join the WaRecId based on WaRecProcessId. We need WaRecId to join other fields.
ChangeIntent <- ChangeIntent[,c("WaRecId", "WaRecChangeIntentTypeCode")]
ChangeIntent <- ChangeIntent[!is.na(ChangeIntent$WaRecId),] # remove any entries where the change intent type is empty
ChangeIntent$PrimaryNumber <- water_rights$PrimaryNumber[match(ChangeIntent$WaRecId, water_rights$WRDocID)] ## alpha-numeric document name 
ChangeIntent$Qa <- water_rights_cleaned$AnnualVolumeQuantity_filled[match(ChangeIntent$WaRecId, water_rights_cleaned$WR_Doc_ID)] # annual quantity from the cleaned dataset, if available
ChangeIntent$Qa_raw <- water_rights$QaTotal[match(ChangeIntent$WaRecId, water_rights$WRDocID)] # annual quantity from the raw WRTS data
ChangeIntent$Qa[is.na(ChangeIntent$Qa)] <- ChangeIntent$Qa_raw[is.na(ChangeIntent$Qa)] # If the cleaned quantity is not available, we use the raw.
ChangeIntent$Parent <- parent_child$Parent[match(ChangeIntent$WaRecId, parent_child$Child)] # doc id of the parent
a <- lapply(genealogy.ls, function(x) unique(cbind(stack(x$gen), stack(x$gen)[1,1])))
b <- do.call(rbind.data.frame, a) # convert the genealogies to a dataframe
ChangeIntent$Oldest <- b[match(ChangeIntent$WaRecId, b[,1]),3] # find the oldest ancestor of each child
nullrows <- which(is.na(ChangeIntent$Oldest))
ChangeIntent <- ChangeIntent[-nullrows,] ## Remove water rights that are not change authorizations and do not have a genealogy
## Some children are "orphans", i.e., they don't have a parent in the system. This makes child == parent == oldest so there are no NAs.
ChangeIntent$Parent[is.na(ChangeIntent$Parent)] <- ChangeIntent$Oldest[is.na(ChangeIntent$Parent)] 
ChangeIntent$Phase <- water_rights$Phase[match(ChangeIntent$WaRecId, water_rights$WRDocID)] # append phase of child
ChangeIntent$Parent_phase <- water_rights$Phase[match(ChangeIntent$Parent, water_rights$WRDocID)] # append parent phase
ChangeIntent$Oldest_phase <- water_rights$Phase[match(ChangeIntent$Oldest, water_rights$WRDocID)] # append phase of 1st-gen parent
## get the child annual quantity
ChangeIntent$Qa <- sapply(1:nrow(ChangeIntent), function(x) get_Qa(x, dfp=ChangeIntent$Phase, dfq=ChangeIntent$Qa)) |> unlist() |> as.numeric()
## get the child instantaneous quantity
ChangeIntent$Qi <- water_rights$QiTotal[match(ChangeIntent$WaRecId, water_rights$WRDocID)]
ChangeIntent$Qi <- ChangeIntent$Qi |> strsplit(split="\\|") |> lapply(function(x)
  ifelse(all(is.na(gsub("NA",NA,x))), NA, min(as.numeric(gsub("NA",NA,x)), na.rm=T))) |> unlist()
## get difference between phases of child
ChangeIntent$compare_Qa <- sapply(1:nrow(ChangeIntent), function(x) get_diff0(ChangeIntent$Qa_raw[x], ChangeIntent$Phase[x]))
## get parent annual quantity
ChangeIntent$Parent_Qa <- water_rights_cleaned$Parent_Qa[match(ChangeIntent$Parent, water_rights_cleaned$Parent)] ## from cleaned quantities, if available
ChangeIntent$Parent_Qa_raw <- water_rights$QaTotal[match(ChangeIntent$Parent, water_rights$WRDocID)] # from raw WRTS data
ChangeIntent$Parent_Qa[is.na(ChangeIntent$Parent_Qa)] <- ChangeIntent$Parent_Qa_raw[is.na(ChangeIntent$Parent_Qa)]
## get parent annual quantity from raw data file (water_rights), parse and choose quantity from the right phase
ChangeIntent$Parent_Qa <- sapply(1:nrow(ChangeIntent), function(x) get_Qa(x, dfp=ChangeIntent$Parent_phase, dfq=ChangeIntent$Parent_Qa)) |> unlist() |> as.numeric()
## get annual quantity of 1st-gen parent
ChangeIntent$Oldest_Qa <- water_rights_cleaned$QaTotal[match(ChangeIntent$Oldest, water_rights$WRDocID)] 
ChangeIntent$Oldest_Qa_raw <- water_rights$QaTotal[match(ChangeIntent$Oldest, water_rights$WRDocID)]
ChangeIntent$Oldest_Qa[is.na(ChangeIntent$Oldest_Qa)] <- ChangeIntent$Oldest_Qa_raw[is.na(ChangeIntent$Oldest_Qa)]
## get 1st-gen parent annual quantity from raw data file (water_rights), parse and choose quantity from the right phase
ChangeIntent$Oldest_Qa <- sapply(1:nrow(ChangeIntent), function(x) get_Qa(x, dfp=ChangeIntent$Oldest_phase, dfq=ChangeIntent$Oldest_Qa)) |> unlist() |> as.numeric()
## get parent instantaneous quantity
ChangeIntent$Parent_Qi <- water_rights$QiTotal[match(ChangeIntent$Parent, water_rights$WRDocID)]
ChangeIntent$Parent_Qi <- ChangeIntent$Parent_Qi |> strsplit(split="\\|") |> lapply(function(x)
  ifelse(all(is.na(gsub("NA",NA,x))), NA, min(as.numeric(gsub("NA",NA,x)), na.rm=T))) |> unlist()   ## choose lowest Qi among phases
ChangeIntent$IsDiminished <- unlist(sapply(ChangeIntent$Oldest, function(x) genealogy.ls[[as.character(x)]]$diminishment$IsDiminished)) ## lookup diminishment status from genealogy
ChangeIntent$LastInactive <- unlist(sapply(ChangeIntent$WaRecId, function(x) last_inactive(x))) ## Is the stage "withdrawn", "rejected", "pending", "cancelled", or "denied"?
ChangeIntent$IsApp <- sapply(water_rights$Phase[match(ChangeIntent$WaRecId, water_rights$WRDocID)], function(x) length(grep("App", strsplit(x, "\\|")[[1]], invert=TRUE)) == 0) ## Is the doc in the application phase?
ChangeIntent$TrustDonation <- sapply(water_rights$Phase[match(ChangeIntent$WaRecId, water_rights$WRDocID)], function(x) length(grep("Donation", strsplit(x, "\\|")[[1]], invert=TRUE)) == 0) ## Is the doc trust water donation?
ChangeIntent$Relinquish <- unlist(sapply(ChangeIntent$Oldest, function(x) genealogy.ls[[as.character(x)]]$diminishment$Relinquished)) ## Is there a certificate of relinquishment?
ChangeIntent$lack_of_diligence <- unlist(sapply(ChangeIntent$Oldest, function(x) genealogy.ls[[as.character(x)]]$diminishment$lack_of_diligence)) ## has there been lack of diligence in permitting stage?
ChangeIntent$DiminishingChange <- unlist(sapply(1:nrow(ChangeIntent), function(x) diminishing_change0(x))) ## Did the change result in diminishment (preliminary determination)
ChangeIntent <- ChangeIntent[order(ChangeIntent$Parent),]

########### Add more attributes to the change request dataframe. Remove non-change documents. #######

ChangeIntent_backup <- ChangeIntent
ChangeIntent <- ChangeIntent[!is.na(ChangeIntent$WaRecChangeIntentTypeCode),] # remove documents that are not changes
ChangeIntent$Event <- water_rights$EventType[match(ChangeIntent$WaRecId, water_rights$WRDocID)] # application accepted, permit issued, certificate issued, etc.
ChangeIntent$ParentEvent <- water_rights$EventType[match(ChangeIntent$Parent, water_rights$WRDocID)]
ChangeIntent$EventDate <- water_rights$EventDoneDate[match(ChangeIntent$WaRecId, water_rights$WRDocID)] # date of event in the life of a child water right
ChangeIntent$AppDate <- paste(ChangeIntent$Event, ChangeIntent$EventDate, sep="|") |> strsplit(split="\\|") |> # look for "AppAccepted" event and retrieve the date when the application was accepted
  lapply(function(x) ifelse(length(grep("AppAccepted", x)) == 0, NA, x[length(x)/2 + grep("AppAccepted", x)])) |> 
  unlist()
ChangeIntent$Assignment <- water_rights$AssignmentGroup[match(ChangeIntent$WaRecId, water_rights$WRDocID)] # append the assignment group field
ChangeIntent$Stage <- water_rights$Stage[match(ChangeIntent$WaRecId, water_rights$WRDocID)] # append the stage of the child water right
d_changes <- read.csv("input_data_files/denial_changes.csv")
seasonal_changes <- read.csv("input_data_files/seasonal_changes.csv")
misc_changes <- read.csv("input_data_files/final_check.csv")
keep_temporary <- subset(ChangeIntent, WaRecId %in% c(d_changes$Child, seasonal_changes$Child, misc_changes$Child)) # this is a compilation of all the temporary changes and denials/cancelations we want to keep
## Remove trust water donations and temporary changes. All the temporary changes that we need to keep are in the keep_temporary, and we will add those back.
ChangeIntent <- ChangeIntent[grep("Seasonal Changes|Drought|TW Acquisition - Temporary Don|TW Acquisition - Permanent Don|Banking", ChangeIntent$Assignment, invert=T),]
## Remove change requests if the child is an application or the child is in the stage "withdrawn", "rejected", "pending", "cancelled", or "denied" and there is no change ROE
ChangeIntent <- ChangeIntent[-which(ChangeIntent$LastInactive == TRUE | ChangeIntent$IsApp == TRUE),]
ChangeIntent <- rbind(ChangeIntent, keep_temporary) # Add back the temporary, denied, cancelled, and withdrawn changes that passed the application phase 

########################## Now we will check for duplication in the change application count by looking for administrative splits ###############

ChangeIntent$IsSplitP <- unlist(sapply(ChangeIntent$ParentEvent, function(x) length(grep("Split", x))>0)) ## Did the parent go through an administrative split?
ChangeIntent$IsSplitC <- unlist(sapply(ChangeIntent$Event, function(x) length(grep("Split", x))>0)) ## was the child split?
ChangeIntent$IsSplitC[ChangeIntent$Stage == "Split"] <- TRUE
ChangeIntent$IsSplit <- ifelse(ChangeIntent$IsSplitC == TRUE | ChangeIntent$IsSplitP == TRUE, TRUE, FALSE)
## Remove superseding documents (these are a form of duplicate, in that they are the child in its mature phase and not a separate change)
ChangeIntent <- ChangeIntent[-which(ChangeIntent$Phase %in% c("SupersedingPermit", "SupersedingCertificate", "AdjudicatedCertificate", "SupersedingQuincyBasinPermit", "Certificate", "Permit", "SupersedingCertificateOfChange")),]
ChangeIntent <- ChangeIntent[-grep("SupersedingCertificate", ChangeIntent$Phase),]

## Duplicates arise anytime multiple changes share the same change application. There is no way to know other than checking manually,
## but the code below applies some basic business rules to predict when a duplicate occurs. 

dup_ind <- which(grepl("\\([A-Z]\\)$", ChangeIntent$PrimaryNumber) | ChangeIntent$IsSplitC == TRUE)
ChangeIntent$DupInd <- grepl("\\([A-Z][0-9]*\\)$", ChangeIntent$PrimaryNumber) | ChangeIntent$IsSplitC == TRUE ## based on the primary number and whether the child was split
ChangeIntent$DupInd2 <- grepl("\\([A-Z][0-9]*\\)$|@[0-9][a-z]", ChangeIntent$PrimaryNumber) ## based on the primary number

dup.df <- NULL
for (p in 1:length(unique(ChangeIntent$Parent))) {
  print(p)
  parent <- unique(ChangeIntent$Parent)[p]
  df <- subset(ChangeIntent, Parent == parent | WaRecId == parent)[c("WaRecId", "PrimaryNumber", "Parent", "Oldest", "WaRecChangeIntentTypeCode", "AppDate", "DupInd", "DupInd2", "IsSplitC", "IsSplitP")]
  if (all(df$DupInd[df$Parent == parent] == TRUE)) {
    if (length(df$WaRecId[df$WaRecId == parent]) == 0) {
      if ((all(df$DupInd2 == TRUE))) {
        if (all(is.na(df$AppDate))) {
          counts <- table(df$WaRecId)
          df$IsDuplicated <- ifelse(df$WaRecId == names(which(counts == max(counts)))[1], FALSE, TRUE)
        } else {
          df$IsDuplicated <- ifelse(is.na(df$AppDate) & (df$DupInd2 == TRUE | df$IsSplitC == TRUE | df$IsSplitP == TRUE), TRUE, FALSE) ## Typically, if there is an application date, the change app is unique
        }
      } else {
        df$IsDuplicated <- ifelse(is.na(df$AppDate) & (df$DupInd2 == TRUE | df$IsSplitC == TRUE | df$IsSplitP == TRUE), TRUE, FALSE)
      }
    } else {
      if (length(unique(df$WaRecId[df$Parent == parent])) == 1) {
        if (all(is.na(df$AppDate[df$Parent == parent])) & all(!is.na(df$AppDate[df$WaRecId == parent]))) {
          df$IsDuplicated <- ifelse(df$WaRecId == parent, FALSE, TRUE)
        } else {
          df$IsDuplicated <- ifelse(df$DupInd2 == FALSE | !is.na(df$AppDate), FALSE, TRUE)
        }
      } else {
        if (all(!is.na(df$AppDate))) {
          df$IsDuplicated <- FALSE
        } else {
          df$IsDuplicated <- ifelse(df$WaRecId == parent & (df$IsSplitP == FALSE | !is.na(df$AppDate)) | df$DupInd2 == FALSE & !is.na(df$AppDate), FALSE, TRUE)
        }
        if (df$Parent[df$WaRecId == parent][1] %in% ChangeIntent$WaRecId & df$DupInd2[df$WaRecId == parent][1] == TRUE & df$IsSplitP[df$WaRecId == parent][1] == TRUE) {
          df$IsDuplicated[df$WaRecId == parent] <- TRUE
        }
      }
    }
  } else {
    if (grepl("CL|J", df$PrimaryNumber[df$WaRecId == parent][1]) & grepl("CV", df$PrimaryNumber[df$Parent == parent][1])) {
      df$IsDuplicated <- ifelse(is.na(df$AppDate), TRUE, FALSE)
    } else {
      df$IsDuplicated <- ifelse(is.na(df$AppDate) & (df$DupInd2 == TRUE | df$IsSplitC == TRUE | df$IsSplitP == TRUE), TRUE, FALSE)
    }
  }
  dup.df <- rbind(dup.df, df) 
}

dup.df <- dup.df[order(dup.df$Oldest),]
## After checking, the coded decision rules sometime incorrectly identified duplicates. Whether a change is a duplicate is adjusted as follows:
to_duplicate_F <- c(4702227, 4702331, 5080441, 4480755, 4252979, 4241278, 4251822, 4702149, 4673004, 4673019, 4681582, 4681608, 6798894, 4210332,
                    4672985, 6564673, 5846645, 6280198, 6801697, 4554825, 6801581, 4540129, 4234013, 6801829, 4223239, 4696974, 4684564, 6800618,
                    4925302, 4663973, 4295637, 4465213, 4485166, 4491508, 4492871, 4505054, 4680847, 4719470, 4847982, 5803316, 4485290, 6418315,
                    6274552, 6274540, 4190754, 4175590, 4185179, 4156809, 2145614, 4199524, 4228109, 6263958, 6463203, 6463217, 6571493,
                    6802108, 6802709, 6802702, 6801396, 2084804, 4845234, 4701004, 6168085, 4306759, 4306734)
to_duplicate_T <- c(6325376, 6802301, 4252971, 4683851, 5080117, 4320949, 6007887, 5644838, 6007864, 5042938, 5681815, 6281045, 2214814, 6129072,
                    5282086, 4706458, 5281967, 5281989, 5281950, 4273094, 4332894, 4540217, 6464282, 4696974, 5080441, 4480697, 6359659, 6799907,
                    6359709, 6359684, 6359635, 6007906, 4228094, 5601720, 4475278, 2145086, 4234625, 4626950, 2154006, 2206111, 4273863, 2223129,
                    2147431, 2220550, 2223128, 2221006, 4175279, 2221372, 4442700, unique(ChangeIntent$WaRecId[ChangeIntent$Oldest == 2231784 & ChangeIntent$Phase == "CertificateOfChange"]),
                    6129080, 2271815, 4650164, 6270988, 5280223, 2271968, 4156380, 4686610, 4733457, 4480093, 4480122, 6260533, 6325398, 6325422, 6325446, 
                    6325479, 2206351, 4264802, 6800904, 4224550, 2145226, 4200689, 4175526, 4167432, 2129302, 2147246, 6801747,	4674108,
                    4162216, 6464293, 2144973, 6007743, 4496497, 4496441, 6007743, 2145390, 4638723, 6674893, 6281020, 4590916, 4925302, 5767407, 4203844, 4203959, 6047840, 
                    2087231, 4173404, 4173365, 4166072, 4234366, 4681929, 4194431, 4192193, 2085111, 2280129, 4189643, 4199892, 4192363,
                    4264216, 4316289, 4341401, 4405804, 4441064, 4441561, 4480036, 4480065, 4480080, 4480135, 4480832, 4484055, 4484113, 4532798,
                    4554733, 4599487, 4644878, 5078919, 5441741, 5563978, 6717931, 6780868, 6780917, 6799431, 6799432, 6799890, 5405144, 5003182,
                    6801422, 4582123, 6801494, 2144949, 4266787, 4469608, 6129654, 6799588, 6801494, 6801642, 5403775, 6801823, 6801824, 2223072,
                    2086993, 2139425, 4539077, 5404384, 5404397, 5443374, 5885522, 5885532, 6801293, 6801383, 5644470, 5767245, 6801659, 4316273,
                    4169540, 4170904, 2075338, 4157245, 4228801, 4295796, 2147571, 2285565, 2134677, 4479555, 4479617, 4479645, 4737419, 2145900,
                    4479704, 4479779, 4479823, 4479848, 4480052, 4480109, 4502867, 5885501, 5156362, 4210346, 2222928, 4170002, 4710003, 2139425,
                    5443374, 6800798, 6801395, 6802768, 5405178, 5405110, 4392279, 4488259, 6801874, 6799498, 5405110, 4556527, 4680094)

dup.df$IsDuplicated[dup.df$WaRecId %in% to_duplicate_F] <- FALSE
dup.df$IsDuplicated[dup.df$WaRecId %in% to_duplicate_T] <- TRUE
dup.df <- unique(dup.df)
ChangeIntent$IsDuplicate <- dup.df$IsDuplicated[match(ChangeIntent$WaRecId, dup.df$WaRecId)]

## Find the date of change authorization (date change ROE was issued, if available)
ChangeIntent$ChangeDate <- sapply(1:nrow(ChangeIntent), function(x) ChangeDate(input_df=ChangeIntent, event_colname="Event", date_colname="EventDate", x))
ChangeIntent$ChangeDate <- as.Date(ChangeIntent$ChangeDate)
ChangeIntent$WaRecChangeIntentTypeCode <- ifelse(ChangeIntent$IsDuplicate == TRUE, NA, ChangeIntent$WaRecChangeIntentTypeCode)

select_cols <- c("WaRecId", "WaRecChangeIntentTypeCode", "PrimaryNumber", "Phase", "Qa", "Qa_raw", "Qi", "Parent", "Oldest", "compare_Qa", 
                 "Parent_Qa", "Parent_Qa_raw", "Parent_Qi", "Oldest_Qa", "Oldest_Qa_raw", "IsDiminished", "DiminishingChange", "Event", "IsDuplicate")
ChangeIntentFinal <- ChangeIntent[which(ChangeIntent$IsDuplicate == FALSE),c(select_cols, "EventDate", "ChangeDate")] ## start a new data frame based on ChangeIntent that has all duplicates removed
ChangeIntentFinal$WRIA <- water_rights$WRIA_NM[match(ChangeIntentFinal$WaRecId, water_rights$WRDocID)]
change_docid <- c(2134788, 2141313) ## Change to no diminishment
ChangeIntentFinal$DiminishingChange[which(ChangeIntentFinal$WaRecId %in% change_docid)] <- FALSE
### Some sequential changes (@) need to be converted to no diminishment because the diminishment occurred in a previous change
## Whenever a water right was manually checked and was determined that no diminishment had taken place, it was added here.
to_diminishing_F <- c(4634837, 5200512, 6801090, 6801271, 6799219, 6511030, 6800497, 6801191, 6796136, 2084513,
                      4669128, 5042891, 4413444, 4157414, 4719790, 4621183, 5115658, 5923541, 5355765, 6256183,
                      6800541, 6627062, 6800167, 6802229, 6007847, 4550494, 4645567, 4691197, 6800788, 5566239,
                      2084857, 5682834, 6801676, 2129541, 5321241, 4306783, 6697117, 6264169, 2085229, 2085235,
                      4229671, 4594378, 6799984, 6801189, 4412861, 4961496, 6802423, 6802927, 6323850, 6801056,
                      6802585, 6718575, 6802581, 4480122, 6801064, 4215646, 4215667, 4727047, 4481847, 5566485,
                      4615661, 4270791, 4484714, 4726815, 6801079, 6801753, 6802314, 6800526, 2143146, 2089189, 
                      2089088, 4249642, 5767959, 4149841, 6009544, 4214507, 4252971, 2089073, 5843236, 4314143,
                      4923536, 4288099, 5646028, 5525694, 6570099, 6800027, 6801619, 2087185, 4251181, 2145445,
                      6801270, 6800259, 6801797, 6800259, 6800166, 4143257, 4847911, 4845061, 4675294, 4192917,
                      5801870, 4149821, 4646871, 5565471, 4660697, 4549831, 5964016, 2084802, 2145294, 2145446,
                      5405110, 4557376, 6500899, 6500741, 4214865, 4658360, 4658360, 5480889, 4274748, 4550753, 
                      4245957, 4153091, 5523805, 2088916, 2129316, 6801829, 6802483, 2089798, 4237638, 4308377,
                      4486605, 2087702, 4597711, 4665612, 4307979, 4548416, 2087335, 4158093, 4309634, 2032388, 
                      2086864, 2088972, 2088974, 2089005, 2129644, 2129645, 2146109, 2146111, 2146112, 2146138, 
                      2144957, 4551696, 4175388, 4183969, 4185014, 4548321, 6718606, 4212947, 4267537, 2087337,
                      2032320, 6799922, 6801581, unique(ChangeIntentFinal$WaRecId[ChangeIntentFinal$Parent %in%  c(2231784, 2080039, 2074433)]),
                      ChangeIntentFinal$WaRecId[which(ChangeIntentFinal$Oldest == 2231784 & ChangeIntentFinal$compare_Qa == 0)],
                      2222631, 4549255, 6799217, 6616148, 4193789, 4193779, 2087688, 2087691, 4224192, 2087455,
                      4192986, 4192443, 4193244, 4193372, 4597311, 2147244, 4226999, 4653432, 5199206, 2088730,
                      2087736, 2084508, 2085044, 4181547, 4237857, 5603648, 2086985, 4228021, 6008305, 4223636,
                      4190754, 4175009, 4158023, 4158036, 4158043, 4143245, 2144978, 4162007, 2134789, 2222930,
                      4249487, 4712040, 4249517, 4185227, 2087231, 2087279, 4273745, 4241278, 4478053,
                      4204916, 6085225, 4192421, 4192816, 4653272, 4169540, 4170441, 2145864, 2088979, 2084539,
                      2089029, 2145795, 2129304, 2134790, 2145753, 2145129, 2145900, 4204807, 2089034, 2085110,
                      6008305, 6129533, 6799984, 6802581, 6802585, 4653520, 5079981, 5404639, 6670559, 2078131,
                      6753618, 6801544, 2084867, 2085230, 2087328, 2282945, 4240703, 6676777, 6413778, 5239129,
                      6720605, 6799794, 6799899, 6801541, 2087497, 6802235, 2032760, 2075134, 2075338, 2089037,
                      2085003, 2085278, 2087066, 2087512, 4170452, 4170464, 4170474, 4170647, 4170662,
                      4170669, 4170688, 4170694, 4170700, 4170710, 4170723, 4170770, 4170776, 4170782, 4170806,
                      4170870, 4170877, 4170883, 4170890, 4170896, 4170904, 4170910, 4170919, 4170937, 4170957,
                      4170964, 4171009, 4171037, 4171087, 4171095, 4171101, 4171108, 4192703, 4192711,
                      4192742, 4192750, 4192803, 4192809, 2032401, 4196608, 4275467, 4683759, 5039534, 2089039,
                      4170763, 4170789, 2032464, 2032539, 2032544, 2032765, 2032995, 2075476, 2075483,
                      2084509, 2084510, 2084511, 2084655, 2084793, 2084807, 2084813, 2084852, 2084858,
                      2084876, 2084899, 2085038, 2085082, 2085222, 2085259, 2085274, 2086878, 2087129, 2087191,
                      2087193, 2087237, 2087381, 2087491, 2087656, 2087689, 2087742, 4187892, 4187905, 2089799,
                      4219984, 4243774, 4557974, 4669043, 4700946, 4723089, 4735419, 5525124, 6090171,
                      6801178, 2087367, 2087368, 2087369, 2087373, 2087374, 2087375, 2087376, 2087684, 2084773,
                      2087685, 2087686, 4192354, 4194972, 4194980, 4470812, 4487028, 4510750, 4712040,
                      4712498, 4717208, 4734137, 5079063, 6274540, 6274552, 6557390, 2084804, 2085280, 6800950, 
                      2087339, 2087338, 2087336, 4222641, 2085147, 4181448, 4212257, 4602677, 4664939, 2087124,
                      2129408, 2129414, 2129537, 2129538, 2129765, 2134180, 2134616, 2134788, 2134789, 2134790,
                      2134792, 2134793, 2134794, 2143194, 2143196, 2144980, 2145276, 2086980,
                      4169784, 4247300, 4257926, 4261785, 4261803, 4261818, 4396041, 4504815, 4514532, 4676621, 
                      5685569, 6129197, 6802480, 2129737, 2143381, 4199515, 4252979, 4554825, 2084832, 2089213,
                      5845292, 6008305, 6218810, 6799296, 6799367, 6799419, 6800231, 2095330, 2086988,
                      6801758, 6802727, 6802729, 2145715, 2145899, 4258390, 4489374, 4639220, 4883746,
                      6800408, 2086943, 5079981, 4558041, 4602764, 5441652, 6264116, 2144912, 4226175, 4226179,
                      6801736, 4252098, 4219440, 6282626, 2222971, 4600129, 4600116, 4204860, 2154038, 4564115,
                      4180639, 4203514, 4540204, 4621441, 5201927, 2147519, 2147520, 2223170,
                      4143101, 4143109, 4150592, 4194421, 4225207, 4271098, 4553304, 4553324, 6799681, 4192132,
                      2223042, 4205066, 4205174, 4232270, 6801542, 4203855, 4673684, 2089212, 2143275,
                      2271967, 4321901, 5442116, 5600909, 5566424, 6798864, 2271828, 4167443, 4238639, 4270554,
                      4274533, 4274540, 4480310, 4231530, 4273730, 4503266, 4625498, 2145537, 2088957, 2145792,
                      2088958, 5600709, 6800619, 2089132, 4643052, 6502120, 6718575, 4143156, 4261267, 4150030,
                      2145816, 4695107, 4320890, 2143473, 2095230, 2129583, 2129710, 2129741, 2129753, 2143472,
                      2089712, 2145987, 2222925, 2222643, 2129774, 2129775, 2129622, 2129624, 2129582, 2089211,
                      2145974, 4250795, 4250821, 4251461, 2129625, 2145172, 2143271, 2145682, 2145497, 4656505,
                      2285582, 2223973, 4224175, 4224192, 4251475, 5042443, 2145596, 4143140, 2145851, 4143198,
                      4143176, 4143167, 4143146, 4152876, 2145475, 4215675, 2089190, 4600151, 4197544, 2084662,
                      2143327, 2143329, 2143390, 2143468, 2143471, 2143552, 2144982, 2145447, 2145448, 2145451,
                      2145473, 2145474, 2145476, 4468215, 2145768, 2145909, 2146140, 2147138, 2206401, 2223025,
                      2223165, 2223403, 2223932, 2282933, 2283051, 2285831, 2285879, 4143092, 4143101, 2145295,
                      4162776, 4175626, 4182507, 4194484, 4205448, 4212693, 4216397, 4224896, 4227189, 4259207,
                      4268874, 4276309, 4330727, 4422641, 4509616, 4582068, 4667602, 5239734, 5242473, 5355765,
                      5600808, 5724062, 5843236, 5845201, 6269702, 6564673, 6664492, 6800676, 6800677, 6799500,
                      2089083, 2145178, 2145209, 2145297, 2145324, 2145389, 2145391, 2145463, 2145510, 2145511,
                      2145551, 2145567, 2145594, 2145668, 2145721, 2145961, 2145962, 2145964, 2146118, 4180478,
                      4180489, 4203754, 4206170, 4211381, 4219457, 4224412, 4246006, 4247291, 4249045, 4250688,
                      4250714, 4250748, 4250876, 4250887, 4251052, 4251294, 4251338, 4251672, 4252348, 4253104,
                      2087371, 5523424, 2085276, 2129750, 2145681, 2154021, 4553338, 4565533, 6800189, 6799553,
                      2032996, 6802524, 2129420, 4204948, 4196632, 2085056, 2032962, 2075129, 2084530, 4193797,
                      2087107, 2087111, 2087117, 2087239, 2087240, 2087247, 2087255, 2087258, 4727012, 4330624,
                      2088850, 2088851, 2088852, 2088853, 2088854, 2088855, 2088856, 2088857, 2089085, 2089086,
                      2145022, 2145059, 2145186, 2145187, 2145199, 2145453, 2145491, 2145492, 2145523, 2145527,
                      2145528, 2145553, 2145584, 2145585, 2145703, 2145704, 2285876, 4147269, 4156653, 4156798,
                      4156898, 4157156, 4158051, 4160807, 4165050, 4165061, 4165067, 4165259, 4168554, 4171054,
                      4174582, 4174592, 4174602, 4184081, 4185133, 4189630, 4190517, 4190566, 4190572, 4190671,
                      4191049, 4192059, 4202669, 4205193, 4206664, 4207105, 4207912, 4207967, 4208008, 4208019,
                      4208027, 4208037, 4208048, 4208118, 4235987, 4241422, 4243701, 4249631, 4262852, 4242079,
                      4316273, 4478922, 4478958, 4478972, 4478986, 4538419, 4650083, 4717239, 4845219, 2129533,
                      5564663, 5600109, 6260411, 4171048, 4203822, 4207352, 4652879, 4196626, 2222640, 6626043,
                      5405144, 4634882, 4234736, 4726910, 4726775, 4726796, 4726815, 4150706, 4150717, 5160277,
                      4199804, 4199855, 5843148, 4145294, 4180725, 6269182, 4265740, 5404676, 4463697, 2075339,
                      4240828, 4193161, 4463697, 5441956, 5442230, 5442325, 5442396, 5442586, 6801151, 6799768,
                      6799769, 6799771, 6799772, 6799773, 2087126, 5523398, 4726863, 4652940, 4240828, 4193161,
                      4688015, 4193171, 4193185, 4482817, 4210910, 4231963, 4247882, 4520026, 5442473, 4810323,
                      6800434, 2086749, 4180345, 4210953, 2088902, 2089084, 4156968, 4207879, 4208128, 2144746,
                      4845661, 6219313, 2086978, 2144997, 2145007, 2145019, 2145109, 2145132, 2145145, 6500607,
                      6624974, 6801156, 6802474, 4541298, 2145066, 4170192, 4182552, 4190325, 4199524, 4540129,
                      4156809, 2088864, 2087259, 6802393, 4584763, 6800967, 4226216, 4227036, 4227054, 4247910, 
                      4157340, 4157383, 4230461, 4249386, 4249419, 4150717, 4150706, 4175317, 4237601, 4221148,
                      4240828, 4180458, 4180345, 4231963, 4247882, 4520026, 2032992, 6801998, 4221159, 4515105,
                      4515351, 4653446, 2084545, 2084546, 2084540, 4160659, 2084804, 4847452, 6797069, 6203537,
                      6203471, 4677084, 4529874, 5964055, 6799427, 6801437, 2147405, 6801057, 4509616, 4334999, 
                      4925019, 4464078, 4510392, 4654003, 4680158, 5563882, 6801643, 4156816, 5967099, 4688005,
                      6203604, 4306759, 4306734, 4846740, 6463240, 2222754, 5320028, 4276259,
                      6705238, 4231387, 4713349, 2143372, 4631401, 4622671, 4809924)
## Whenever a water right was manually checked and was determined diminishment had taken place, it was added here
to_diminishing_T <- c(6801760, 6799490, 6801574, 4245023, 4169820, 2075172, 2086955, 2087278, 6798812, 2087372,
                      4426749, 6801395, 4161947, 4203786, 4148842, 5600787, 6579322, 5278930, 5600885, 6045056, 
                      6506845, 4643178, 2087326, 6799941, 6579340, 4722227, 4169820, 6774930, 2088924, 2285735,
                      2089038, 6623793, 2085117, 6623750, 4589980, 4145768, 2084647, 5357090, 2087326, 4252159, 
                      4671112, 5682725, 4148842, 4162367, 4162354, 2032764, 2084811, 2086858, 2084501, 4231312,
                      2087348, 4237819, 4663897, 6359863, 2032387, 4669955, 4201157, 2086918, 4209354,
                      4601310, 2145408, 4469398, 4149543, 4310074, 5921637, 2087698, 2087699, 2087711, 4500273, 
                      4309702, 2145747, 4672985, 4189636, 2032593, 2223274, 2223273, 2087602, 4256849, 2089070, 
                      4235944, 2032955, 2033016, 2214834, 6464293, 4636443, 5002234, 2145861, 2084792, 2088923,
                      2032375, 4265753, 4663973, 2084869, 4467495, 2087669, 5118025, 4645314, 4216857, 2087675,
                      6047807, 6577532, 6801169, 4715759, 5965303, 4480611, 6734811, 2088889, 2223965, 2084808,
                      6800959, 6799094, 4145983, 4146045, 4524157, 4289234, 4174206, 6579288, 4207821, 2145804,
                      2084880, 4653349, 2089133, 4259696, 2145084, 2145085, 2144888, 2145655, 2032454, 4259487,
                      2145649, 2145651, 2145654, 2145653, 2145652, 2145650, 2145789, 4700991, 2089214, 2084499,
                      4184262, 5600097, 6798863, 6798859, 5241083, 2144871, 4209133, 4259518, 2032997, 2087503,
                      6044951, 4500364, 5278946, 2145471, 5484992, 4244536, 2145642, 2144878, 2144986, 2144992,
                      4313150, 5600451, 4649235, 2032998, 4314005, 6798859, 4179951, 2145359, 2145360, 2285671,
                      4221132, 2084680, 6553789, 2075340, 2084750, 2147273, 2229374, 4215034, 4621254, 6622727,
                      2271858, 6717809, 4232277, 2143046, 6126831, 4265912, 4217225, 2145912, 4164728, 5682755,
                      6167626, 4669982, 2145611, 4185073, 2144990, 4169869, 4162199, 4199235, 6799853, 6800516,
                      6800513, 6800514, 6800517, 6800518, 4192853, 6009240, 2271881, 4153027, 4167139, 4170062,
                      4167411, 2224129, 4624838, 2272172, 5278978, 4257191, 2129548, 4264818, 4234013, 4234512,
                      2145573, 4711118, 6800933, 6800931, 6800926, 2129721, 4244689, 2089946, 2144879, 4221181,
                      4259822, 2085053, 2085054, 2087327, 6044002, 5565158, 4239694, 4256490, 4259320, 4261503,
                      2032324, 2084871, 2032406, 2032398, 2032389, 2032386, 4292342, 4350891, 4495909, 4519411,
                      4162237, 4162310, 4310120, 4310202, 4502477, 4615728, 2075110, 2084536, 2084791, 2084809,
                      2084815, 2085033, 2085216, 2086876, 2087696, 2087732, 4169257, 4186992, 4468968, 4695161,
                      6800698, 2086769, 4195046, 4195055, 6045272, 6556913, 6577646, 6618649, 6800762, 4296728,
                      2087513, 2087516, 2087517, 2087560, 4181020, 4244557, 4244575, 4883496, 4883534, 4644764,
                      2089778, 2129415, 2129584, 4200603, 4259347, 4582123, 5682242, 6798796, 2088969, 2089795,
                      2129427, 2143223, 2145913, 2089194, 2089195, 2095231, 2129298, 2129299, 2129300, 2145450,
                      2145598, 2145599, 2145850, 2145852, 4186851, 4330678, 2086942, 4215245, 4652081, 6282678,
                      4259734, 5884421, 2147116, 2206417, 2223013, 2223116, 4194004, 4220371, 4414964, 5198881,
                      2222819, 2223229, 2223399, 2222812, 2223135, 2222761, 2221174, 4175482, 4678054, 2206122,
                      2222871, 4169572, 4273675, 6267306, 6506683, 6579247, 6800477, 6800478, 6801896, 6801898,
                      6798856, 6622626, 6556096, 6555875, 5601242, 6802570, 2285521, 4273687, 2223946, 6801119,
                      2285676, 2285796, 4165947, 4223246, 4223256, 4712144, 2145679, 2129466, 4677568, 6802227,
                      2032402, 2129308, 4197535, 4324435, 6683666, 4471683, 2086875, 2085146, 4695881, 6800816,
                      6799017, 4565755, 2145002, 2145667, 4538434, 2089248, 2129590, 4653364, 6800932, 2087128,
                      6800934, 6801025, 2129305, 2146087, 2146115, 4146618, 2088965, 2088966, 2129751, 2143339,
                      4146638, 4173940, 4275449, 4652466, 4653768, 2032513, 2224282, 4257191, 2086982, 2075393,
                      2086983, 2086987, 2087183, 2087263, 2087268, 2087274, 2087276, 2087341, 2087456, 2147276,
                      2285842, 4146121, 4157116, 4157373, 4157949, 4160722, 4165247, 4184066, 4185065, 2145051,
                      4196619, 4244904, 4244960, 4262601, 4299654, 4148190, 4442887, 4543536, 4652176, 2033004,
                      4659381, 4961839, 5078855, 5079600, 5600403, 5600480, 5603677, 5724640, 6009122, 2222814,
                      6043913, 6045285, 6800808, 6800967, 6800870, 2075376, 2086989, 2087272, 2145426, 2285843,
                      4735430, 2087709, 4701004, 5683853, 6007954, 6803035, 6801556, 2032391, 2223205, 4846771,
                      2087315, 4709390, 4304929, 4180695, 4228121, 4442716, 4237612, 2085234, 2085050, 6045033,
                      2089197, 4157967, 4164305, 4168352, 4168359, 4169628, 5603723, 6360146, 6125357, 6571493,
                      2271863, 4202658, 4191940, 4180695, 4243114, 2147301, 4297235, 4223642, 4223649, 5965254,
                      2145608, 4267521, 6803035, 4286915, 4286894, 4286876, 4286847, 4585232,
                      4237612, 2086753, 5645789, 6800347, 4847426, 5724994, 2144987,
                      2085279, 2084843, 4187950, 4462731, 2086872, 4202493, 4210364, 2144944, 2090004, 6717231, 	
                      6460199, 4173810, 2223104, 2223105, 6044929, 4192551, 6088577, 5404625, 4712089, 6800710,
                      6801287, 5078908)
## Adjust diminishment status accordingly
ChangeIntentFinal$DiminishingChange[which(ChangeIntentFinal$WaRecId %in% to_diminishing_F)] <- FALSE
ChangeIntentFinal$DiminishingChange[which(ChangeIntentFinal$WaRecId %in% to_diminishing_T)] <- TRUE
## Calculate the diminishment quantity
ChangeIntentFinal$ChildQ <- sapply(1:nrow(ChangeIntentFinal), function(x) child_quantQ(x, ChangeIntentFinal))
print("Done ChildQ")


## Make adjustments to diminishment quanities. These were verified manually by reading the change ROEs.
docs=c(4686645, 4686732, 4230341, 4598053, 2084647, 4496991, 6045056, 4148751, 2145221,           #1
       4292342, 2223119, 4631401, 4622671, 2032454, 2087694, 4159603, 2129441, 4485257, 2089070,  #2
       2145672, 4647660, 2145803, 4200329, 2087153, 2145801, 4174126, 2075171,                    #3
       4150662, 4150697, 2084811, 2285804, 4459985, 2145731, 4216209, 2087703, 2087737, 2145804,  #4
       4170062, 2223031, 6045124, 4647739, 2145669, 2145670, 2229082, 2088838, 4170029, 4410835,  #5
       5845529, 6798797, 4197426, 2145671, 5357090, 4186869, 4200632, 2088985, 2084911, 2087326,  #6
       2147274, 4162354, 4148842, 4709412, 2032376, 2145051, 2032391, 4162940, 4159475, 4163656,  #7
       4163667, 4163673, 2222814, 6800634, 4494349, 4494832, 4548296, 4276243, 4314334, 2223119,  #8
       2085262, 4148796, 2087733, 2086858, 2143326, 2129764, 4485788, 5003088, 2145790,           #9
       2145740, 4147539, 4147650, 4147660, 4147670, 4147684, 2224152, 2129546, 2087501, 6799094,  #10
       2145543, 4256490, 2129446, 2129619, 4237819, 4663897, 2032330, 2032364, 2032423, 2032424,  #11
       2032436, 2032442, 2032466, 2032467, 2032468, 2032473, 2032486, 2032516, 2032578, 2032582,  #12
       2032584, 2032585, 2032586, 2032587, 2032588, 2032590, 2032598, 2032599, 2032624, 2032680,  #13
       2032781, 2032786, 2032791, 2032803, 2032804, 2032805, 2032811, 2032812, 2032821, 2032828,  #14
       2032830, 2032831, 2032833, 2032837, 2032861, 2032868, 2032871, 2032874, 2032875, 2032955,  #15
       2032972, 2032973, 2032974, 2032976, 2032977, 2032978, 2032979, 2032980, 2032981, 2033014,  #16
       2033018, 2033020, 2033022, 2033024, 2033026, 2033039, 2084656, 2084788, 2085022, 2085057,  #17
       2085070, 2085072, 2085073, 2085075, 2085080, 2085142, 2085143, 2085144, 2085206, 2085272,  #18
       2086881, 2087166, 2087226, 2087518, 2087519, 2087520, 2087522, 2087523, 2087588, 2087673,  #19 
       2087687, 2087704, 2087713, 4462610, 5921254, 2145749, 2145887, 4169852, 4596569, 4500418,  #20
       2085079, 2075131, 6359863, 4216871, 4220393, 2084814, 2086918, 4148741, 4194123,           #21
       4323627, 4469414, 2085195, 2085237, 4183207, 4255102, 4530065, 4530208, 4530717,           #22
       4530771, 4530797, 4531454, 4308010, 4201157, 4149543, 2089070, 2145672, 6799853,           #23
       2032455, 4264786, 4469398, 5320009, 6364560, 2086765, 2147785, 4235944, 2033016,           #24
       2087698, 2087711, 2087699, 4697524, 2206349, 2075478, 2032593, 2223274, 5319900, 2087602,  #25
       2084789, 2087215, 4259224, 2086752, 4465178, 2224234, 2223050, 2088889,                    #26
       4237107, 5278930, 5278957, 4456033, 6203117, 6802205, 4688040, 6802086, 4142853, 4194861,  #27 
       4687994, 4148464, 4214841, 4500273, 4152153, 4696500, 4164703, 4145230, 4162199, 2087735,  #28
       2085051, 2087348, 2085262, 4148796, 4189636, 2085225, 2214834, 4636443, 5002234, 2144871,  #29 
       2084792, 2032375, 4265753, 2084869, 4467495, 2087669, 2086994, 2086981, 4702287, 4216857,  #30
       2086977, 4482751, 5118025, 4480611, 6579288, 6734811, 6800959, 6799094, 4145983,           #31
       4146045, 4174206, 4289234, 4524157, 4207821, 2084880, 4653349, 2089133, 4164728,           #32
       4259696, 2145084, 2145085, 2144888, 2145655, 2145649, 2145651, 2145654, 2145653,           #33 
       2145652, 4691882, 2145650, 2145789, 4581333, 4700991, 2089214, 4169547, 2084499, 4184262,  #34
       5600097, 5600403, 6798863, 6798859, 5241083,                                               #35
       6044951, 4500364, 5278946, 2145471, 5484992, 4244536,                                      #36
       4313150, 5600451, 4649235, 2032998, 4314005, 6798859, 4179951, 5802872,                    #37
       4148765, 4145778, 2129564, 2144990, 2129566, 4221132, 2084680, 6553789, 2075340, 2084750,  #38
       2147273, 2223050, 2229374, 4215034, 4265912, 4621254, 2145861, 2271858, 6717809,           #39
       4232277, 2144749, 2143046, 6126831, 2088924, 2088923, 4199235, 4217225, 2145912,           #40
       4653285, 4653298, 4653380, 4653393, 4669890, 4669982, 2145611, 4185073,                    #41
       4169869, 6800516, 6800515, 6800513, 6800514, 6800517, 6800518, 4192853, 6009240,           #42
       2271881, 4167411, 2224129, 4624838, 2272172, 6622727, 5278978, 2129548,                    #43
       4264818, 2285735, 2145665, 4711118, 6800933, 6800931, 6800926, 2129721, 4244689, 2089946,  #44 
       2144879, 4221181, 4259822, 4429327, 5565158, 4663973, 5682755,                             #45
       2085053, 2085054, 2087327, 6044002, 2032324,                                               #46  
       2084871, 2032406, 2032398, 2032389, 2032386, 4162237, 4162310, 4309702, 4310074,           #47
       4310120, 4310202, 4502477, 4615728, 4697707, 2084536, 2084791, 2087131, 2087696, 2087732,  #48
       4466628, 4468968, 4695161, 6045095, 6045111, 6045139, 6800698, 2086769, 4195046, 4195055,  #49
       6556913, 6799546, 4296728, 2087514, 2087513, 2087516, 2087517, 2087560, 4181020, 4244557,  #50
       4244575, 4288333, 4498960, 4883496, 4924499, 2089057, 2089778, 2129415, 2129532,           #51
       2129584, 2144986, 2144992, 2145679, 2145696, 4163041, 4169820, 4200603, 4228225, 4234013,  #52
       4259347, 4551681, 4582123, 4590078, 5682242, 6798796, 6799062, 2088969, 2089795, 2129427,  #53
       2143223, 2145163, 2145164, 2145913, 6009762, 2089194, 2089195, 2095231, 2129298, 2129299,  #54
       2129300, 2145450, 2145598, 2145599, 2145850, 2145852, 4186851, 4323341,                    #55
       4330678, 2086942, 4158686, 4215245, 4259734, 4681816, 6751439, 6799624, 2145674,           #56
       4558057, 5884421, 5884447, 2147116, 2206371, 2206417, 2223013, 2223116, 4194004, 4220371,  #57
       4414964, 5198881, 2222819, 2222863, 2223197, 2223205, 2223229, 2223399, 4197426, 4267646,  #58
       2222812, 2223135, 2222761, 2221174, 4175482, 4678054, 2206122, 2222871, 4169572, 2223276,  #59
       4145768, 4273675, 5681685, 6267306, 6420271, 6506683, 6506845, 6579247, 6800477, 6800478,  #60
       6801896, 6801898, 6798856, 6622626, 6556096, 6555875, 5601242, 6802570, 4273687, 2223946,  #61
       6801119, 2285676, 2285796, 4165947, 4211159, 4223246, 4223256, 4223294, 4712144,           #62
       2129466, 4677568, 2032402, 2129308, 4197535, 4324435, 6683666, 4471683, 2086875, 2085146,  #63
       6800816, 6799017, 4565755, 2145002, 2145667, 4538434, 2089248, 2129590, 4653364, 6800932,  #64
       6800934, 6801025, 2129305, 2146087, 2143339, 2146115, 4146618, 2088965, 2088966, 2129751,  #65
       4146638, 2032513, 2224282, 4257191, 2084808, 2086983, 2086987, 2087183, 2087263,           #66
       2087268, 2087274, 2087276, 2087341, 2087456, 2285842, 4146121, 4157116, 4157373,           #67
       4160722, 4165247, 4184066, 4185065, 4196619, 4244904, 4244960, 4148190, 4442887, 4543536,  #68
       4652081, 4846771, 4961839, 5078855, 5079600, 5356464, 5356486, 5356505, 5356517, 5356528,  #69
       5356540, 5356555, 5600403, 5600480, 5724640, 6009122, 6043913, 6045285, 6800808, 2075376,  #70
       2086989, 2087272, 2145426, 4735430, 2087709, 4701004, 4267521,                             #71 
       5683853, 6007954, 6803035, 6801556, 2087315, 4709427, 4709439, 4709390, 4304929, 4329800,  #72
       4240783, 4180695, 4185859, 4228121, 4442716, 4329809, 4237612, 2085234, 2089197, 2145859,  #73
       4157967, 4158059, 4164305, 4168352, 4168359, 4169628, 4191940, 4202658, 4658705, 6360146,  #74
       6125357, 6571493, 2271863, 2143372, 2145748, 6800008, 4243114, 2147301, 4223642, 4223649,  #75
       6799907, 5965254, 2145608, 4231387, 4713349, 4180606, 4286915, 4286894, 4286876, 4286847,  #76
       4585232, 4237612, 2086753, 5645789, 6800347, 4847426, 4231312, 4231332, 4231370, 4231397,  #77
       5281798, 5724994, 6800841, 4209133, 4209354, 2145642, 2032997, 2075172, 2075393, 2086955,  #78
       2087503, 2144878, 2144987, 2145359, 2145360, 2285671, 4153027, 4167139, 4234512, 4239694,  #79
       4259320, 4261503, 4350891, 4495909, 4519411, 4644764, 6282678, 6802227, 2033004, 2085050,  #80
       6045033, 2087278, 2085279, 2084843, 4187950, 4462731, 2086872, 4202493, 4210364, 2144944,  #81
       6460199, 2090004, 6798812, 6717231, 4173810, 2223104, 2223105, 6044929, 4192551, 6088577,  #82
       5404625, 6800710, 6801287, 5078908, 4263493, 4502553, 2085012, 4157624, 4163642, 2095245,  #83
       4161947, 4299654, 2206121)                                                                 #84
Qs=c(-107, -102, -52.5, -4.34, -106.82, -1.63, -53.4, -3.2, -18.49,                               #1
     -8.5, -208, -19, -19, -72, -32, -5.5, -5.5, -6.7, -104.5,                                    #2 
     -131.5, -6.57, -271.2, -18.56, -29, -12.2, -4.5, -42.4,                                      #3 
     -0.4, -2, -98, -45.6, -6.3, -7.2, -1.5, -9.1, -15.07, -218,                                  #4
     -28.6, -25.33, -94, -144.1, -41.6, -6.6, -32.9, -12, -21, -46,                               #5
     -83.85, -10, -63, -17, -3.6, -464, -46, -137, -2, -33.56,                                    #6 
     -21.4, -14, -4, -342, -8.5, -73, -97, -84, -76.5, -12.5,                                     #7
     -5.2, -13.8, -115, -50, -14.8, -7.5, -10.5, -51.52, -210.9, -208,                            #8 
     -14, -72, -16.7, -71, -20, -130.8, -72.7, -152.7, -3,                                        #9 
     -71, -18.3, -9.1, -13.4, -10.9, -8.9, -1042, -432, -248, -84,                                #10
     -525, -185, -100, -135, -119, -38.7, -0.46, -0.13, -0.7, -1.35,                              #11
     -0.48, -0.31, -4.9, -0.47, -0.36, -0.23, -0.178, -1.17, -0.65, -1.08,                        #12
     -1.115, -0.94, -0.7, -1.35, -0.376, -0.294, -0.02, -0.99, -0.47, -0.616,                     #13 
     -1.17, -0.53, -0.82, -0.93, -0.26, -0.176, -0.21, -0.28, -0.47, -0.47,                       #14
     -0.56, -0.75, -0.49, -0.33, -0.82, -0.34, -0.76, -1.11, -1.14, -111,                         #15
     -0.69, -0.41, -0.022, -0.07, -0.446, -0.53, -0.29, -0.29, -0.35, -0.87,                      #16
     -0.66, -0.74, -0.153, -0.29, -1.03, -0.47, -1.13, -2.76, -3.87, -0.41,                       #17 
     -2.73, -1.53, -1, -0.96, -1.76, -1.35, -3.83, -1.53, -1.17, -0.176,                          #18
     -0.09, -1, -1.2, -0.293, -0.47, -0.7, -0.53, -0.47, -10.86, -0.29,                           #19
     -0.19, -0.88, -0.094, -1.02, -132.9, -78, -312, -20, -243.55, -1371.5,                       #20
    -109.3, -257.2, -17.6, -500, -44.7, -4.2, -6.43, -224, -343,                                  #21
     -197, -147, -176, -72, -364, -337, -128, -45, -1.16,                                         #22       
     -26.8, -165.8, -12.4, -948, -447.5, -118.8, -104.5, -131.5, -42.9,                           #23
     -1347, -449, -28, -637.7, -3355, -1180, -1649.5, -966.4, -86437,                             #24
     -269, -70, -225, -132, -1382, -2204.2, -474, -4950, -7553, -9551,                            #25 
     -6930, -8268, -1457, -36, -110, -17, -0.96, -149,                                            #26 
     -71, -8387, -7081, -2, -69, -58.3, -3, -66.6, -496, -36.5,                                   #27
     -28.5, -31.5, -3.5, -1693.2, -126, -127, -190, -240, -66.58, -76,                            #28
     -36, -110.5, -14.2, -72, -27, -124.8, -454.9, -59, -85.52, -8.26,                            #29
     -3141.4, -181.92, -1.67, -168.28, -16800, -733.5, -78, -24.4, -41.3, -160,                   #30
     -38, -115, -187.7, -682, -10, -1102.9, -206, -84, -10.75,                                   #31
     -10.6, -33, -32, -14.8, -0.75, -73, -0.5, -120, -4,                                          #32 
     -15, -11, -77, -8, -2.3, -0.45, -7.7, -21.8, -8,                                             #33
     -1.9, -4.125, -7.4, -421.4, -3.8, -18.9, -12.4, -17.25, -122, -880,                          #34 
     -9.4, -1.84, -5.8, -6.8, -3.8,                                                               #35
     -44, -61, -1057.5, -235, -69.82, -76.6,                                                      #36
     -80, -4, -29.5, -47, -24, -6.8, -3.2, -108,                                                  #37
     -52.9, -4.2, -276, -296, -10.09, -1, -2205.5, -449, -458, -352,                              #38 
     -20, -0.96, -16.67, -17.6, -0.085, -13.5, -246, -12.5, -74,                                  #39
     -8.1, -2, -254.8, -10, -17.2, -51.6, -148.5, -2031, -423.6,                                  #40 
     -135, -102.72, -2.5, -1.25, -172.8, -9.5, -252.8, -285.31,                                   #41
     -20, -387.6, -463.1, -69.7, -260.4, -1947.8, -2006.9, -1123.6, -1460.5,                      #42
    -9.7, -7, -59, -156, -12.6, -0.4, -1714, -139,                                                #43
     -470.5, -5, -16.6, -265.3, -42.6, -1162, -6657.7, -399, -41.53, -4,                          #44
     -2, -227, -19.25, -53.8, -6.3, -56.4, -86,                                                   #45
     -65.33, -32.67, -706.24, -24, -34,                                                           #46 
     -15.3, -129.7, -140.2, -439, -85, -146, -360.3, -2097.9, -1057.7,                            #47
     -1025.5, -1270.3, -241.5, -129.7, -2718, -63, -4889, -7.2, -132.75, -115.6,                  #48
     -13.8, -132.8, -346, -43.9, -44.9, -168.1, -5.27, -211, -195, -57,                           #49
     -3.5, -24.89, -19.6, -131.7, -283.3, -49.9, -357.6, -166.2, -36, -2,                         #50
    -2, -20.5, -203.2, -5.6, -12.136, -4, -2705.5, -30, -9,                                       #51
    -12.5, -33.1, -17, -4.3, -11.44, -238, -1.5, -0.61, -2.3, -100,                               #52  
    -0.48, -15.8, -117.6, -243, -97.7, -6, -25.39, -80, -100, -2,                                 #53 
    -49.1, -3.76, -207.92, -322, -17.2, -1, -134, -4, -2, -2,                                     #54
    -1, -2, -640, -200, -1133.2, -1108.3, -971, -2160,                                            #55 
    -437, -118, -355, -96.75, -88, -362.9, -7.97, -55.4, -27.4,                                   #56 
    -303, -2.4, -33.1, -805, -106.3, -1.1, -22.5, -43, -112, -0.3,                                #57
    -56.75, -24.12, -794, -0.56, -17.7, -39, -18.1, -4.5, -37, -24.8,                             #58
    -2, -8, -4.5, -20, -16, -100, -9, -11, -62, -32,                                              #59  
    -53, -93.5, -17, -52, -87.2, -11.9, -18.8, -36.5, -1.8, -2.4,                                 #60
    -0.5, -5, -1.5, -9.3, -4.5, -1.8, -2.57, -26.5, -150, -960,                                   #61
    -132.7, -1430, -1.4, -210.25, -513, -182, -756, -315, -122.8,                                 #62
    -969.5, -10, -50, -323.6, -395.2, -774, -19, -8, -53.6, -994,                                 #63
    -386, -146, -1, -53, -44.7, -51, -780, -7.8, -8.69, -296.8,                                   #64
    -94.4, -36.1, -387.2, -1699, -31, -97.5, -58.4, -588, -102, -16.5,                            #65
    -2.2, -56.5, -20, -18, -14.4, -36, -2, -221, -16,                                             #66
    -343, -144, -112.5, -12, -200, -22.4, -273.5, -3, -300,                                       #67
    -293.2, -12.5, -157.1, -40.8, -40, -3747.3, -11.28, -25, -36.42, -52,                         #68
    -53.9, -68.06, -149, -140, -5352, -37.5, -37.5, -22.5, -29.85, -5,                            #69    
    -13.125, -13.125, -1.84, -2.15, -245, -28, -3, -17.34, -23.53, -203,                          #70
    -48, -21.8, -120, -88.32, -97.5, -19, -16.7,                                                  #71
    -80, -10, -103, -45.9, -32.3, -433, -137, -15, -810.67, -39,                                  #72
    -999, -100, -479.4, -74.6, -10.57, -66, -5.59, -60, -1, -187.75,                              #73
    -248, -80, -19.3, -100, -200, -11.6, -142, -96, -117.58, -1,                                  #74 
    -2, -1.5, -4.8, -157.1, -261.5, -44.8, -70, -146, -45, -35.5,                                 #75
    -1460.5, -70.8, -169, -192.8, -22, -30, -50.12, -9.73, -23.06, -1.75,                         #76
    -178, -5.59, -5042, -242.08, -85, -200, -114.16, -50.1, -47.9, -55.4,                           #77
    -388, -24, -10.16, -19, -19, -6.3, -8, -5.7, -11.8, -62.9,                                    #78
    -39.4, -15.6, -6.71, -23, -16, -960, -1.6, -20, -47.6, -206.9,                                #79
    -48.1, -75.28, -0.75, -50, -2.95, -12.5, -11, -40, -64.75, -36,                               #80 
    -191.7, -2, -2, -18, -2, -11.6, -59.5, -4.5, -39, -1464.6,                                    #81
    -307.1, -38, -265, -20, -1.75, -40, -13.8, -1.5, -176, -39.7,                                  #82
    -0.35, -282, -40.25, -12.84, -90, -478, -19, -42, -2.53, -800,                                #83
    -3.5, -204.1, -1.75)                                                                          #84                                                                                                                                          
    
## For older certificates, the annual quantity is based on the calculated beneficial use of the right in the change ROE.
## For adjudicated certificates, the annual quantity is based on the water duty from the change ROE 
## (sometimes based on beneficial use analysis, sometimes based on the water duty awarded in the adjudication), if there is one. Otherwise, 
## it is filled based on the ratio of Irrigated acreage between parent and child, if there is any, otherwise it is based on the ratio of instantaneous quantity between parent and child

docs2 <- c(4157949, 2282531, 2275535, 2214795, 2212264, 2211043, 2211036, 2145244, 2285581, 2214243,   #1
           2136366, 2135882, 2133449, 2133435, 2109874, 2105379, 2079868, 2079867, 2079844, 2213245,   #2  
           2079700, 2079100, 2078125, 2077793, 2077535, 2067890, 2044810, 6799849, 6727585, 6677365,   #3
           6285062, 5646531, 4199066, 4190754, 4188122, 4185179, 4175590, 4158212, 4158128, 4156833,   #4
           2277999, 2277967, 2277903, 2277446, 2277406, 2277210, 2277013, 2276866, 2276759, 2214814,   #5
           2276727, 2276477, 2276326, 2276295, 2276271, 2276201, 2276051, 2137837, 2145089, 2138225,   #6
           2222531, 2222426, 2217808, 2214833, 2214784, 2214498, 2214445, 2276183, 2271815, 2281227,   #7
           2214185, 2213719, 2213687, 2213623, 2213587, 2213566, 2213390, 2213210, 2213204, 2213095,   #8
           2212928, 2212530, 2212467, 2212061, 2211305, 2211023, 2211022, 2210585, 2209917, 2270635,   #9
           2209788, 2182383, 2182382, 2169175, 2145573, 2145517, 2144949, 2143364, 2136932, 2136811,   #10
           2136759, 2136696, 2136585, 2136520, 2136471, 2136015, 2135213, 2134986, 2277373, 2282603,   #11  
           2134515, 2134083, 2133991, 2105268, 2087094, 2087053, 2081235, 2080040, 2080064, 2276761,   #12  
           2079953, 2079946, 2079943, 2079918, 2079898, 2079895, 2079887, 2087013, 2079877, 2079735,   #13
           2079718, 2079707, 2079675, 2079668, 2079656, 2079638, 2079489, 2079369, 2079162, 2079123,   #14
           2079118, 2079092, 2079079, 2078770, 2078737, 2078718, 2078472, 2078275, 2078215,            #15
           2078138, 2078116, 2078115, 2077770, 2077763, 2077749, 2077632, 2077580, 2077553, 2068710,   #16
           2068673, 2061854, 6799849, 2219732, 2080020, 4200712, 2218226, 2278372, 2144781,            #17
           2079797, 2079826, 2079769, 2084750, 2079820, 2283079, 2139246, 2130902, 2139635,            #18
           2135216, 2079320, 2078987, 2078516, 2142232, 2142413, 2214430,                              #19
           2078907, 2078891, 2077533, 4670139, 4151606, 2278045, 2277854, 2214589, 2079879, 4189987,   #20
           2144605, 2274286, 2038639, 2039500, 4167129, 4286596, 2137064, 2135963,                     #21
           4184262, 4173365, 4145473, 4715759, 2087014, 2140432, 2133252, 2143196, 4766773,            #22  
           2084675, 2084679, 2085330, 2089304, 2283156, 2282938, 4168805, 4305326, 4654759, 6271529,   #23
           2129714, 2142709, 4162842, 2133550, 2079703, 2079711, 2143940, 2079669, 2079671, 2079660,   #24
           2079694, 4170675, 2079694, 2079680, 2079670, 2079736, 2079672, 2079677, 2079682, 2079686,   #25
           2079705, 2079706, 2079710, 2079715, 2079716, 2079720, 2079721, 2079729, 2079730, 2079742,   #26
           2079749, 2079753, 2079754, 2079768, 2078317, 2078442, 2078192, 2078315, 2078283, 2078421,   #27
           2078325, 2078397, 2078423, 2079181, 2079187, 4194737, 2058495, 2032415, 4764410, 4759363,   #28
           2283262, 2078794, 4188122, 2080045, 4193115, 2078478, 2079444, 2079460, 2079431, 2079417,   #29
           2079407, 4750516, 4470812, 6589159, 4758870, 4717506, 4716827, 4755779, 2079905, 2079010,   #30
           4588490, 2089019, 2135888, 2143175, 2144795, 2139183, 2145499, 4186886, 2143737, 2133308,   #31
           2095507, 2276313, 2275080, 2285521, 2276854, 2137953, 2145783, 2143809, 2283416, 2221735,   #32
           2207743, 2283843, 2141705, 5601720, 2134731, 2135002, 2143853, 2143339, 2146114, 2078891,   #33
           2285581, 2134831, 2134770, 2144448, 2211384, 2077783, 2138163, 2133813, 2142506, 2141901,   #34
           2134562, 2136932, 2077745, 4189786, 2077964, 2079133, 2084773, 2087034, 2077755, 4175042,   #35
           2078131, 2074932, 2087023, 2079014, 2078113, 2078114, 4174731, 2087120, 2079016, 2079018,   #36
           2079597, 2079596, 2079547, 2064060, 2045647, 2135217, 2135209, 2135211, 2135226, 2142407,   #37
           2142001, 2136567, 2138590, 2136931, 2136841, 2136440, 2142815, 2229373, 2144104, 2138864,   #38
           2077927, 2079845, 2140132, 2084023, 2078052, 2133005, 2083415, 2082951, 2082952, 2077762,   #39
           2079719, 2079723, 4174558, 2143371, 2133852, 2079628, 2074103, 2077645, 4188225, 4190566,   #40
           2077708, 2077947, 2077948, 2079260, 2083595, 2078323, 2078447, 2087017, 2211040, 2214221,   #41
           2212651, 2211744, 2212219, 2213718, 2211037, 2213543, 2214116, 2212409, 2214563, 2214149,   #42
           2212091, 2213250, 2278039, 2136502, 2136688, 2135505, 2135504, 2135497, 2136854, 4249631,   #43
           2276586, 2277997, 2277966, 2136637, 2142619, 2133901, 2223989, 2087098, 2079287, 2079285,   #44
           2136720, 2049858, 2079888, 4632452, 2276847, 2074860, 2077791, 2077910, 2079296,            #45
           2211022, 2077641, 2079098, 4199855, 6800634, 2087096, 2079319, 2133721, 2082362, 2083682,   #46
           2084332, 2084331, 2078117, 2224203, 2278495, 2214173, 2214150, 5564481, 2169457, 4752434,   #47
           2136347, 2136596, 2077949, 2056167, 4175232, 2079284, 2083179, 2084584, 2084242, 4165259,   #48
           2133134, 2079623, 2219263, 2141809, 2082922, 2211384, 2283620, 2211027, 4180606, 4541455,   #49
           2133251, 4188959, 2136904, 5039162, 4165114, 4226216, 4227036, 4227054, 4249386, 2095305,   #50
           2090014, 2135777, 2032416, 2239998, 2080039, 2073891, 4753005, 4756880, 2079954, 2276525,   #51
           2277787, 4753264, 4753277, 2075604, 2084804, 4921379, 2076975, 2079706, 2088168, 2138841,   #52
           2143922, 2060746, 2079311, 2084298, 4766969, 2133432, 2133455, 4697707, 2114243)            #53      
noQuantParent <- c(120, 279, 231, 2295.6, 140, 10427, 9492, 356, 67, 125.2,            #1
                   1083, 93.25, 770, 85.5, 252, 17.6, 378.5, 357, 480, 80,             #2
                   362, 94.83, 76, 115.6, 135, 106.3, 437, 112, 89.1, 20.69,           #3 
                   13.93, 351.77, 7.2, 4088, 737.25, 4220, 4220, 216, 289.24, 39.2,    #4
                   483.9, 1810, 1086.0, 29.8, 86.3, 867, 44.8, 67, 36.63, 48,          #5
                   18.8, 107.6, 56.8, 18.3, 70.1, 49.09, 28.8, 291.3, 450, 450,        #6
                   36, 242, 1440, 1706, 5781.60, 75.6, 3618, 26547.5, 24.5, 50,        #7
                   83.5, 73.7, 62, 120, 31, 48, 64.8, 81, 160, 6,                      #8
                   200, 12, 30, 90, 4.7, 2164, 2970.5, 2501, 8688, 37,                 #9
                   21717.50, 80.6, 80.6, 27.17, 85, 203.6, 64.5, 600, 20606, 1302,     #10
                   160, 275, 125.5, 1421, 50.2, 239, 572, 19, 43, 4,                   #11
                   3.05, 85, 1.68, 280, 190.5, 119.5, 224, 244, 450, 83,               #12
                   106, 131.2, 314, 86, 225, 28.8, 39, 12.8, 71.7, 60,                 #13
                   83.83, 640, 160, 61.6, 5557, 65, 27482, 132, 105, 1,                #14
                   69.81, 242.24, 108.2, 240, 1, 55, 4.4, 24, 4.27,                    #15
                   192, 160, 80, 8.1, 20, 100, 5.68, 90, 486, 3.9,                     #16
                   80.7, 123, 112, 7.2, 4088, 23, 16.2, 112, 375,                      #17
                   4220, 4220, 182, 440, 36.6, 314, 647.6, 78, 3600,                   #18
                   4381, 184, 10, 120, 160, 320, 150,                                  #19
                   23.65, 39.99, 20.4, 0, 217, 2, 5, 73, 119.5, 920,                   #20
                   658, 1120, 400, 160, 2172, 440, 1424, 808,                          #21 
                   1903, 455, 237, 6670, 9.54, 209, 270, 186, 589,                     #22
                   2144, 2602, 1374, 74.95, 141, 203, 140, 182, 2550, 305,             #23
                   23, 720, 19, 360, 464.8, 270, 4240, 14, 8, 180,                     #24
                   125, 50, 5547.5, 200, 400, 184.5, 165.25, 385.1, 141.35, 290,       #25
                   200, 309.5, 302, 200, 24, 116.8, 297, 68, 191, 470.1,               #26
                   494, 27.2, 676, 598.5, 350, 385, 200, 165, 132.5, 407.5,            #27
                   444.35, 310, 200, 242.5, 50, 4500, 4, 868, 66, 2535,                #28
                   810.7, 37959, 737.25, 214, 675, 1150, 8, 185, 715, 375,             #29
                   80, 7321, 127, 187.75, 136.93, 55.82, 372.17, 690, 1128.4, 303,     #30
                   186, 186, 28, 220, 360, 558, 1288, 1553, 371, 14.2,                 #31
                   226, 37.1, 55, 134.7, 7.4, 1660, 681, 2340, 225, 3226,              #32
                   30, 887, 101, 420, 270, 335, 335, 63, 707, 40,                      #33
                   67, 5, 82.5, 63, 30, 216, 7455, 84, 200, 140,                       #34
                   30, 20606, 46.4, 3190.72, 340, 756, 2700, 78.4, 27, 1036,           #35
                   1088, 4, 8000, 140, 2464, 142, 25, 17, 30, 30,                      #36
                   200, 800, 700, 40, 260, 1765.5, 3200, 3600, 500, 95,                #37
                   450, 144, 280, 0.5, 178.4, 120, 326, 978, 382, 926,                 #38
                   147, 400, 67, 600, 103, 430, 2944, 5840, 300, 48,                   #39
                   90, 70, 68.75, 225.6, 60, 20.4, 12853, 295, 103.2, 103.2,           #40
                   35, 45, 35, 13.6, 7200, 225, 300, 2444, 14028, 40,                  #41
                   35.6, 0.4, 90, 10, 20, 1, 2, 120, 64, 58,                           #42
                   80, 2, 30, 10, 80, 55, 10, 40, 1311, 92,                            #43  
                   183, 16300, 1000, 16.7, 68, 114, 168000, 140, 153.4, 137,           #44
                   504, 160, 12.18, 630, 0.59, 60, 307, 12000, 234.5,                  #45  
                   6160, 23.52, 113, 260, 380, 228.7, 228.7, 80, 8, 177.6,             #46
                   300, 1000, 275, 41, 304, 0.8, 120, 931281, 32, 261000,              #47
                   80, 31.73, 5, 17, 62.5, 140, 58, 50, 116, 146,                      #48
                   97.75, 7.6, 40, 240, 7, 30, 158, 4658, 43.75, 288,                  #49
                   352, 150, 141, 296.1, 32, 50, 120, 225, 14.8, 779,                  #50
                   779, 65, 13416, 320, 5465.62, 800, 290.92, 128, 237, 56,            #51
                   10857, 10.36, 10.15, 220.02, 12, 425, 280, 82, 905.6, 1106.25,      #52
                   9253, 9660, 153.3, 146, 540, 86, 92.91, 7696, 132)                  #53

adj_childQ <- data.frame(docid=docs, newQ=Qs)
adj_parent <- data.frame(docid=docs2, newQ=noQuantParent)

docid <- c(2129533, 2143194, 4923142, 2271858, 4212257, 2075333, 4221159, 4221148, 6801396)
newParent <- c(2139562, 2139562, 2132751, 2271857, 2078494, 2084040, 2095305, 2090014, 2117218) # sometimes the parent docs in WRTS are incorrect and need to be adjusted
assign_new_parent <- data.frame(docid=docid, newParent=newParent)
row_num <- which(ChangeIntentFinal$WaRecId %in% assign_new_parent$docid)
ChangeIntentFinal$Parent[row_num] <- assign_new_parent$newParent[match(ChangeIntentFinal$WaRecId[row_num], assign_new_parent$docid)]
ChangeIntentFinal$Parent_Qa[row_num] <- ChangeIntentFinal$Oldest_Qa[row_num]

ChangeIntentFinal$Comment <- water_rights$Comment[match(ChangeIntentFinal$WaRecId, water_rights$WRDocID)]
ChangeIntentFinal$IsYakima <- grepl("Yakima River Basin Adjudication", ChangeIntentFinal$Comment)
ChangeIntentFinal$Assignment <- water_rights$AssignmentGroup[match(ChangeIntentFinal$WaRecId, water_rights$WRDocID)]
## Water rights in the "Temporary Other" Assignment group. We want to keep some and exclude others, based on whether a formal 
## change of water right took place
donations <- c(6800797, 6801719, 6801722, 4192211, 2223152, 6800541, 4306656, 4306669, 6800526, 6778437, 
               4484385, 4255126, 4238445, 4237325, 4237310, 4265527, 4594419, 4230962, 4413352, 4327567,
               4594452, 4237338, 6801270, 5080269, 5484611, 2145458, 5080291, 5565179, 6802228, 4237349,
               6801436, 4503134, 6802423, 6219015, 5683781, 4625498, 4649925, 6800852, 6800289, 6685217,
               4537096, 6801594, 6801607, 6801606, 4163743, 4488937, 5564481, 6579454, 6579441, 	
               5080279, 6800366, 6800367)
lease_exclude <- c( 4232182, 4234736, 4159580, 4684692, 4684681, 4922163, 6129721, 6279280, 6279292, 6279203, 
                    6279141, 4922104, 4681103, 4684728, 4686286, 4684774, 4631190, 4640342, 4684599, 4684861, 
                    5523817, 6129702, 6129685, 4520862, 4520900)
conservation_exclude <- c(4221482, 4221490, 4221443, 4221465, 4221454, 6800794, 4247882, 4231963)  

## Use adj_childQ and adj_parent data frames created above to modify the diminishment quantities and parent quantities, respectively.
row_num <- which(ChangeIntentFinal$WaRecId %in% adj_childQ$docid)
ChangeIntentFinal$ChildQ[row_num] <- adj_childQ$newQ[match(ChangeIntentFinal$WaRecId[row_num], adj_childQ$docid)]
row_num <- which(ChangeIntentFinal$Parent %in% adj_parent$docid)
ChangeIntentFinal$Parent_Qa[row_num] <- adj_parent$newQ[match(ChangeIntentFinal$Parent[row_num], adj_parent$docid)]
row_num <- which(ChangeIntentFinal$Oldest %in% adj_parent$docid)
ChangeIntentFinal$Oldest_Qa[row_num] <- adj_parent$newQ[match(ChangeIntentFinal$Oldest[row_num], adj_parent$docid)]

ChangeIntentFinal$Purpose <- water_rights$PurposeOfUse[match(ChangeIntentFinal$WaRecId, water_rights$WRDocID)]
ChangeIntentFinal$WRIA_ID <- water_rights$WRIA_ID[match(ChangeIntentFinal$WaRecId, water_rights$WRDocID)]
ChangeIntentFinal$Parent_IA <- water_rights$IrrAreaTotal[match(ChangeIntentFinal$Parent, water_rights$WRDocID)]
ChangeIntentFinal <- subset(ChangeIntentFinal, !WaRecId %in% c(lease_exclude, conservation_exclude, donations)) # remove short-term leases, donations, and certain IEGP conservation rights

### Adjust TW-Acq Temporary-Other category based on manual checks

to_diminishing_F <- c(4232323, 4232113, 4159548, 4166088, 4180181, 4185859)
to_diminishing_T <- c(2087712, 2087313, 6800779, 4157578, 4180606)

docs <- c(2087284, 2087313, 6800779)  
childQ <- c(-95.32, -44.88, -2958)
ChangeIntentFinal$DiminishingChange[which(ChangeIntentFinal$WaRecId %in% to_diminishing_F)] <- FALSE
ChangeIntentFinal$DiminishingChange[which(ChangeIntentFinal$WaRecId %in% to_diminishing_T)] <- TRUE
adj_ChildQ <- data.frame(docid=docs, dimQ=childQ)
ChangeIntentFinal$ChildQ[ChangeIntentFinal$WaRecId %in% docs] <- adj_ChildQ$dimQ[match(ChangeIntentFinal$WaRecId[ChangeIntentFinal$WaRecId %in% docs], adj_ChildQ$docid)]
ChangeIntentFinal$ChildQ[ChangeIntentFinal$DiminishingChange == FALSE] <- 0

################### Fill annual quantities of the 1st-gen parents ###################################

docs3 <- c(2084016, 2212424, 2134368, 2039500, 2084253, 2038639, 2135988, 4170276, 2078794, 2069330,
           2055499, 2078112, 2078119, 2078718, 2079087, 2079306, 2079797, 2079826, 2079849,
           2079877, 2079879, 2079927, 2079950, 2080020, 2136969, 2140298, 4167129, 4233186, 2144605,
           2274286, 2086871, 2077783, 2088699, 2143853, 2136670, 2138163, 4715759, 2079877,
           2080082, 2141773, 2133780, 2081549, 4274053, 2085192, 2079284, 5768493, 2078122, 4172845,
           2084254, 2082115, 2083725, 2079492, 2080068, 2078764, 2079163, 2078872, 2078873, 2078874,
           2079855, 2079854, 2064761, 2135959, 2144061, 2136617, 2134536, 2134835, 2212281, 2210664,
           2162067, 2223053, 2214275, 2066857, 2079622, 2079624, 2086968, 2115325, 2222528, 2079494,
           2136086, 2216975, 2137072, 2136242, 2136867, 2136075, 2041083, 2135225, 2071882, 2182712,
           2086857, 4166396, 2083578, 2078482, 2080044, 4556143, 2276525, 6413554, 4556143)

Qa3 <- c(200, 100, 260, 160, 285, 400, 356, 280, 37959, 16,
         62965, 1250, 8000, 55, 926, 45.22, 4220, 4220, 45.08,
         71.7, 119.5, 309, 417, 4088, 240, 1220, 2172, 780, 658,
         1120, 59.5, 216, 1088, 335, 800, 7455, 6670, 127,
         181250, 274, 1020, 212, 285, 2650, 140, 756, 1036, 1420,
         205, 75, 360, 202, 160, 576, 2, 1, 1.2, 1.2,
         600, 4800, 1000, 105, 110, 280, 60, 10, 50, 0.4,
         2, 15, 1000, 350, 60, 146.4, 36, 2, 140, 25,
         90, 880, 280, 300, 15, 320, 280, 412, 18, 160,
         87, 350, 32, 675, 166, 145776, 56, 755.3, 145776)

fill_oldest <- data.frame(docid=docs3, Qa=Qa3)
row_num <- which(ChangeIntentFinal$Oldest %in% fill_oldest$docid)
ChangeIntentFinal$Oldest_Qa[row_num] <- fill_oldest$Qa[match(ChangeIntentFinal$Oldest[row_num], fill_oldest$docid)]

############## Fill parent court claim quantities (Acquavella adjudication) #########################################

fill_Yakima <- read.csv("input_data_files/fill_Yakima.csv") # fill the Parent court claim quantities
row_num <- which(ChangeIntentFinal$WaRecId %in% fill_Yakima$docid)
ChangeIntentFinal$Parent_Qa[row_num] <- fill_Yakima$Qa[match(ChangeIntentFinal$Parent[row_num], fill_Yakima$docid)]
ChangeIntentFinal$Oldest_Qa[row_num] <- fill_Yakima$Qa[match(ChangeIntentFinal$Oldest[row_num], fill_Yakima$docid)]
ChangeIntentFinal$Oldest_Qa <- ifelse(is.na(ChangeIntentFinal$Oldest_Qa) & ChangeIntentFinal$Oldest == ChangeIntentFinal$Parent, ChangeIntentFinal$Parent_Qa, ChangeIntentFinal$Oldest_Qa)

ChangeIntentFinal$ParentPhase <- water_rights$Phase[match(ChangeIntentFinal$Parent, water_rights$WRDocID)] ## phase of the parent
ChangeIntentFinal$ParentPurpose <- water_rights$PurposeOfUse[match(ChangeIntentFinal$Parent, water_rights$WRDocID)] ## purose of use of the parent

############## Fill missing parent water quantities based on average water duties ########################################

fill_parent <- read.csv("input_data_files/fill_parent_Qa.csv") 
row_num <- which(ChangeIntentFinal$Parent %in% fill_parent$Parent)
ChangeIntentFinal$Parent_Qa[row_num] <- fill_parent$Parent_Qa[match(ChangeIntentFinal$Parent[row_num], fill_parent$Parent)]
row_num <- which(ChangeIntentFinal$Oldest %in% fill_parent$Parent)
ChangeIntentFinal$Oldest_Qa[row_num] <- fill_parent$Parent_Qa[match(ChangeIntentFinal$Oldest[row_num], fill_parent$Parent)]

############# Add the water right owner category ###############################

person <- read.csv("input_data_files/PersonOrOrganization.csv")
ChangeIntentFinal$Person <- person$PersonOrOrganization[match(ChangeIntentFinal$WaRecId, person$WaRecId)] # name of child water right holder
ChangeIntentFinal$Parent_Person <- person$PersonOrOrganization[match(ChangeIntentFinal$Parent, person$WaRecId)] # name of parent water right holder
person_class <- read.csv("input_data_files/public_private3.csv") ## type of water right holder
ChangeIntentFinal$PersonClass <- person_class$Class[match(ChangeIntentFinal$Person, person_class$Person)]
ChangeIntentFinal$Parent_PersonClass <- person_class$Class[match(ChangeIntentFinal$Parent_Person, person_class$Person)]

####### a few corrections to the water right holder classifications ######################################

new_owner_class <- data.frame(docid=c(4145294, 4180725, 6799768, 6799769, 6799771, 6799772, 6799773, 6801151, 6801156), 
                              class=c("Irrigation Company", "Irrigation Company", "Irrigation District", "Irrigation District", "Irrigation District", "Irrigation District", "Irrigation District", "Irrigation District", "Department/Agency"))
row_num <- which(ChangeIntentFinal$WaRecId %in% unique(new_owner_class$docid))
ChangeIntentFinal$Parent_PersonClass[row_num] <- new_owner_class$class[match(ChangeIntentFinal$WaRecId[row_num], new_owner_class$docid)]

ChangeIntentFinal$Source <- water_rights$RCWClass[match(ChangeIntentFinal$WaRecId, water_rights$WRDocID)] ## groundwater or surface water?
ChangeIntentFinal$ParentSource <- water_rights$RCWClass[match(ChangeIntentFinal$Parent, water_rights$WRDocID)]

############## Adjustments to the purpose of use designation #######################################################

doc_id <- c(2141873, 2142048, 2143253, 2216913, 2218866, 2218956, 2219631, 2219652, 2219982, 2220052, 
            2220376, 2221081, 2281850, 4189096, 4673895, 5357353, 6799551, 2078033, 2078959, 2087367,
            2087368, 2087369, 2132786, 2136932, 2142591, 2142867, 2142869, 2143278, 2144234, 2206308,
            2206339, 2215466, 2215467, 2215645, 2216458, 2216552, 2218665, 2219207, 2219422, 2219544,
            2219574, 2219640, 2219827, 2219899, 2220831, 2221080, 2221734, 2276896, 2282944, 2283699,
            2285019, 4157587, 4157657, 4157694, 4189555, 4224896, 4275449, 4487028, 4620835, 2139193,
            2141489, 2283416, 2081558, 2082557, 2139575, 2139843, 2140297, 2141279, 2141407, 2141796,
            2142051, 2142149, 2142234, 2142634, 2144772, 2144776, 2216003, 2221739, 2222516, 2275296,
            2276757, 2277999, 2278442, 2279696, 2280336, 2280948, 2281762, 4189074, 4214483, 4214613,
            5318011, 2084501, 2085249, 2084815, 2084773, 2085239, 2211037, 2085207, 2085208, 2085210,
            2075489, 2084538, 2084864, 2085074, 2087281, 2087282, 2087283, 2087288, 2087289, 2087301,
            2087302, 2087303, 2087308, 2087309, 2087311, 2087318, 2087324, 2087325, 2087350, 2087363,
            2087393, 2087398, 2087402, 2087411, 2087412, 2087413, 2087419, 2087420, 2087421, 2087423,
            2087424, 2087425, 2087426, 2087427, 2087431, 2087432, 2087435, 2087436, 2087452, 2087469,
            2087474, 2087475, 2087476, 2087477, 2087478, 2087479, 2087480, 2087481, 2087482, 2087537,
            2087538, 2087577, 2087578, 2087579, 2087580, 2087581, 2087583, 2087473)
purpose <- c("DM", "MU", "DM", "IR", "DM", "IR", "DM", "IR", "IR|DG", "IR", 
             "DM", "DM", "IR", "DM", "IR", "MU|IR", "IR", "DM", "OT", "IR|ST",
             "IR", "IR|ST", "DM", "MU|IR", "DM|IR|MU", "IR", "IR", "IR|DM", "DM", "DM",
             "DM", "DM", "DM", "DM", "DM", "DM", "DM", "DM", "DM", "DM",
             "DM", "DM", "DM", "DM", "DM", "DM", "DM", "DG", "DM|IR", "DM|IR",
             "DM|FR", "DM|IR", "DM|IR", "DM|IR", "DM|IR", "IR|DG", "IR|ST", "IR|ST", "DM", "IR|DM",
             "DM", "IR", "DM", "DM", "DM", "DM", "DM", "DM", "DM|IR", "DM",
             "DM", "DM", "DM", "DM", "DM", "DM", "DM", "DM", "DM", "IR",
             "IR", "DM", "DM", "DM", "DM", "DM", "DM", "DM", "DM", "MU|IR",
             "MU|IR", "DG", "MU", "IR|ST", "SR", "IR|ST", "IR", "IR", "IR", "IR",
             "IR", "IR|DM|PO", "MU", "IR", "IR", "IR", "IR", "IR", "IR", "IR",
             "IR", "IR", "IR", "IR", "IR", "IR", "IR", "IR", "IR", "IR",
             "IR", "IR", "IR", "IR", "IR", "IR", "IR", "IR", "IR", "IR",
             "IR", "IR", "IR", "IR", "IR", "IR", "IR", "IR", "IR|DM", "IR",
             "IR", "IR", "IR", "IR", "IR", "IR", "IR", "IR", "IR", "IR|DM",
             "DM", "IR", "IR", "IR", "IR", "IR", "IR", "IR")

person_purpose.df <- data.frame(docid=doc_id, purpose=purpose)
row_nums <- which(ChangeIntentFinal$Parent %in% person_purpose.df$docid)
ChangeIntentFinal$ParentPurpose[row_nums] <- person_purpose.df$purpose[match(ChangeIntentFinal$Parent[row_nums], person_purpose.df$docid)]

######################### Adjustments to the change intent type #################################

new_ChangeIntent <- read.csv("input_data_files/ChangeIntentType2.csv")
ChangeIntentFinal <- unique(merge(new_ChangeIntent[,1:2], ChangeIntentFinal[,-2], all=T, by="WaRecId"))
del_rows <- which(is.na(ChangeIntentFinal$PrimaryNumber))
if (length(del_rows) > 0) {
  ChangeIntentFinal <- ChangeIntentFinal[-del_rows,]
}

######################## Cases where only the child is present in the genealogy ####################

docs <- c(2087370, 2075167, 2087683, 4145343, 4162674, 4162682, 4171022, 4216748, 4237149,
          4259518, 4298593, 4298628, 4302478, 4442716, 4638842, 4641543, 4643846, 4645379, 4645388,
          4652446, 4652457, 4700176, 6801760, 4721251, 4723861, 5965244, 5965329, 6556983, 6719691,
          6799675, 6799677, 6799678, 6800490, 6800491, 6800562, 6800566, 6800568, 6800716, 6800717,
          6801167, 6801464, 4259487, 4645314, 4652466, 6047807, 6577532, 6801169,
          4715759, 5965303, 4145473, 2087128, 2087675, 2087372, 4173940, 4275449, 4652466, 6045272,
          6577646, 6618649, 6800762, 2285521, 2086753, 6802157, 4712089) # child = parent = oldest & DiminishingChange == TRUE
childQ <- c(-6.58, -106.4, -10.5, -183, -124.5, -100, -1.7, -56.6, -15.53,
            -40, -12, -15.6, -93.38, -10.57, -972.73, -283.76, -75.395, -7.42, -6.27,
            -1.14, -38.9, -4.36, -588.5, -2.64, -53.9, -1, -15.75, -3.5, -0.67,
            -136.55, -19.55, -33.361, -1, -80.25, -1.05, -0.88, -2.46, -84.12, -444.5,
            -69.86, -332.6, -66, -54.1, -10, -13, -33.6, -524.6,
            -1407, -1, -108, -123.5, -13.15, -8, -5, -20.7, -9.56, -21.6,
            -21.26, -4.85, -26.22, -33.6, -5042, -362, -62.64)
adj_ChildQ <- data.frame(docid=docs, dimQ=childQ)

ChangeIntentFinal$ChildQ[ChangeIntentFinal$WaRecId %in% docs] <- adj_ChildQ$dimQ[match(ChangeIntentFinal$WaRecId[ChangeIntentFinal$WaRecId %in% docs], adj_ChildQ$docid)]

##### Add relinquishment quantities from spreadsheets (seasonal changes, denials, and misc. changes that were added later) #########
select_rows <- which(ChangeIntentFinal$WaRecId %in% d_changes$Child)
ChangeIntentFinal$ChildQ[select_rows] <- -1 * d_changes$relinqQa[match(ChangeIntentFinal$WaRecId[select_rows], d_changes$Child)]
ChangeIntentFinal$Parent_Qa[select_rows] <- d_changes$Parent_Qa[match(ChangeIntentFinal$WaRecId[select_rows], d_changes$Child)]
ChangeIntentFinal$DiminishingChange[select_rows] <- ifelse(ChangeIntentFinal$ChildQ[select_rows] == 0, FALSE, TRUE)

select_rows <- which(ChangeIntentFinal$WaRecId %in% seasonal_changes$Child)
ChangeIntentFinal$ChildQ[select_rows] <- -1 * seasonal_changes$ChildQ[match(ChangeIntentFinal$WaRecId[select_rows], seasonal_changes$Child)]
ChangeIntentFinal$Parent_Qa[select_rows] <- seasonal_changes$Parent_Qa[match(ChangeIntentFinal$WaRecId[select_rows], seasonal_changes$Child)]
ChangeIntentFinal$DiminishingChange[select_rows] <- ifelse(ChangeIntentFinal$ChildQ[select_rows] == 0, FALSE, TRUE)

select_rows <- which(ChangeIntentFinal$WaRecId %in% misc_changes$Child)
ChangeIntentFinal$ChildQ[select_rows] <- -1 * misc_changes$relinq_Qa[match(ChangeIntentFinal$WaRecId[select_rows], misc_changes$Child)]
ChangeIntentFinal$Parent_Qa[select_rows] <- misc_changes$Parent_Qa[match(ChangeIntentFinal$WaRecId[select_rows], misc_changes$Child)]
ChangeIntentFinal$DiminishingChange[select_rows] <- ifelse(ChangeIntentFinal$ChildQ[select_rows] == 0, FALSE, TRUE)

########################## non-consumptive water rights ##############################################

nonconsumptive_rights <- read.csv("input_data_files/nc_rights.csv") ## water rights for non-consumptive uses
nc_parents <- unique(subset(ChangeIntentFinal, WaRecId %in% nonconsumptive_rights$WaRecId)$Parent)
row_num <- which(ChangeIntentFinal$Parent %in% nc_parents)
ChangeIntentFinal$ChildQ[row_num] <- nonconsumptive_rights$ChildQi[match(ChangeIntentFinal$WaRecId[row_num], nonconsumptive_rights$WaRecId)]
ChangeIntentFinal$Parent_Qa[row_num] <- nonconsumptive_rights$Parent_Qi[match(ChangeIntentFinal$Parent[row_num], nonconsumptive_rights$Parent)]
ChangeIntentFinal$nonconsumptive <- FALSE
ChangeIntentFinal$nonconsumptive[row_num] <- TRUE

not_enough_info <- c(2143474, 2145422, 2032556, 4194725, 4485290, 2144742, 2144743, 4459406, 4317022, 4400748, 
                     2143455, 4214601, 2144913, 6726271, 2129711, 4549502, 4152111, 6418883, 2144910, 2143498, 
                     4719846, 6781320, 5043679, 6781326, 6125289, 2143467, 4240820, 4316933, 4316966, 4316998, 
                     6279203, 6279141, 6168104, 2075339, 4656298, 6571764, 6571724, 2075338, 2144910,
                     4184645, 4240650, 5565067, 5080580, 2144743, 2144938, 4594064, 4673140,
                     2075136, 2084886, 2087618, 6725978, 2087390, 2087422, 2075387, 4594064, 4673163,
                     6168085, 4200672, 2085256, 2143352, 4549831, 2145269) ## Records with not enough information to determine forfeiture

delete_rows <- which(ChangeIntentFinal$WaRecId %in% not_enough_info)
ChangeIntentFinal <- ChangeIntentFinal[-delete_rows,]

## Forfeiture is of the instantaneous quantity only (no forfeiture of annual quantity)
Qi_not_Qa <- c(2272126, 2271919, 2224462, 4394899, 4394930, 2147773, 2154083, 4175336, 2088839,
               2088840, 2088847, 4153104, 4669809, 4506418, 5603592, 5603620, 6799664, 6166220,
               2086766, 2223408, 2224436, 2272120, 6506656, 2285751, 4273501, 2145533, 4668956, 
               2085028, 2085115, 4163905, 4695881, 2087490, 2129450, 2086982, 4157949, 5603677,
               5603723)

### Corrections to instantaneous quantities
docid <- c(2283549, 2087490)
Qi <- c(11.14, 300)
replace_Qi <- data.frame(docid=docid, Qi=Qi)
row_num <- which(ChangeIntentFinal$Parent %in% docid)
ChangeIntentFinal$Parent_Qi[row_num] <- replace_Qi$Qi[match(ChangeIntentFinal$Parent[row_num], replace_Qi$docid)]

## water rights with forfeiture in the superseding document phase
dd1 <- c(4186851, 4186869, 4163642, 2095239, 2147116, 2222819, 2223197, 2223237, 2222761, 
         2221174, 4426749, 2285664, 2223946, 4677568, 2129466, 4197535, 2085115, 2085146, 2223408,
         4197535, 6799017, 2145002, 2089248, 2129590, 6801025, 2129305, 2129450, 4259734, 2145599, 
         2145598, 2145602, 2145850, 2145852, 2087490, 2147276, 2285842, 4148190, 2145426, 5965254) 
ChangeIntentFinal$DiminishingChange[which(ChangeIntentFinal$WaRecId %in% c(Qi_not_Qa, dd1))] <- TRUE

diminish_Qi <- read.csv("input_data_files/Qi_not_Qa.csv") # list of parents with diminished Qi but not Qa
dimQi_parents <- unique(subset(ChangeIntentFinal, WaRecId %in% diminish_Qi$WaRecId)$Parent)
row_num <- which(ChangeIntentFinal$Parent %in% dimQi_parents)
ChangeIntentFinal$Parent_Qa2 <- ChangeIntentFinal$Parent_Qa
ChangeIntentFinal$ChildQ[row_num] <- diminish_Qi$ChildQi[match(ChangeIntentFinal$WaRecId[row_num], diminish_Qi$WaRecId)]
## If diminishment is of Qi only, we substitute Parent_Qi for Parent_Qa
ChangeIntentFinal$Parent_Qa[row_num] <- diminish_Qi$Parent_Qi[match(ChangeIntentFinal$Parent[row_num], diminish_Qi$Parent)]  
ChangeIntentFinal$QiOnly <- FALSE
ChangeIntentFinal$QiOnly[row_num] <- TRUE

## Additional changes we need to delete because they are pre-1967 or donations
delete <- c(4193937, 4192455, 4174304, 4174558, 2234525, 2139378, 2141313, 2084776, 2075394, 2087661, 
            4211752, 4211766, 2084532, 2087210, 4228060, 2087067, 4184081, 4549440, 4961507, 4732035, 
            4923549, 5601904, 4686042, 6164928, 6573530, 4686077, 4686009, 4961519, 6800785, 5115675, 
            4961546, 5321276, 4715312, 4923640, 4923652, 5321289, 4588959, 6124895, 5200550, 6803047,
            4697127, 4732069, 6202408, 6362389, 6799254, 6005686, 6276881, 6511071, 6799229, 6272941, 
            4686023, 2145249, 2145252, 4520862, 6800462, 6800864, 6802983, 2032435, 2145902, 2087741,
            4245937, 4193244, 2089074, 4243434, 6800401, 4286830, 6558987, 6801177, 6802153, 6803044,
            6799718, 2222643, 6802590, 6802591, 4190105, 2086971)
ChangeIntentFinal <- ChangeIntentFinal[-which(ChangeIntentFinal$WaRecId %in% delete),]
ChangeIntentFinal$ChildQ[is.na(ChangeIntentFinal$ChildQ) & ChangeIntentFinal$DiminishingChange == FALSE] <- 0

## Make sure that when a parent is the 1st-gen parent, parent_Qa == oldest_Qa
row_num <- which(ChangeIntentFinal$Parent == ChangeIntentFinal$Oldest & ChangeIntentFinal$QiOnly == FALSE & ChangeIntentFinal$nonconsumptive == FALSE & ChangeIntentFinal$Parent_Qa != ChangeIntentFinal$Oldest_Qa)
ChangeIntentFinal$Oldest_Qa[row_num] <- ChangeIntentFinal$Parent_Qa[row_num]
row_num <- which(ChangeIntentFinal$Parent == ChangeIntentFinal$Oldest & is.na(ChangeIntentFinal$Oldest_Qa))
ChangeIntentFinal$Oldest_Qa[row_num] <- ChangeIntentFinal$Parent_Qa[row_num]

# Write the final change authorization dataframe to file.

write.csv(ChangeIntentFinal, "cleaned_data/WRChanges.csv", row.names=F)








