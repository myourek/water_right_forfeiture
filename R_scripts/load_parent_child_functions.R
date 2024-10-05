#####################################
#                                   #
# Functions used by parent_child2.R #
#                                   #
#####################################


## Construct genealogies from the parent_child data file ("WaRecParent_adj.txt")
get_genealogy_parent <- function() {
  genealogy.df <- data.frame(matrix(nrow=0, ncol=13))
  for (i in 1:length(original_parents)) {
    parent <- original_parents[i]
    cyclic <- cyclic1 <- cyclic2 <- cyclic3 <- cyclic4 <- cyclic5 <- FALSE
    genealogy_ii <- genealogy_iii <- genealogy_iv <- genealogy_v <- genealogy_vi <- genealogy_vii <- NULL
    print(i)
    child <- parent_child$Child[parent_child$Parent == parent]
    if (child[1] == parent) {
      genealogy.df <- rbind(genealogy.df, c(parent, rep(NA, ncol(genealogy.df) - 1)))
      next
    }
    for (ch in child) {
      j <- 1
      genealogy_i <- c(parent, ch)
      ind1 <- which(parent_child$Parent == ch)
      if (length(ind1) == 0) {
        genealogy.df <- rbind(genealogy.df, c(genealogy_i, rep(NA, ncol(genealogy.df) - length(genealogy_i))))
        next
      } 
      while (length(ind1) > 0) {
        if (length(ind1) == 1) {
          j <- j + 1
          child <- parent_child$Child[ind1]
          genealogy_i <- c(genealogy_i, child)
          ind1 <- which(parent_child$Parent == child)
        } else {
          j1 <- j
          genealogy_ii <- genealogy_i
          for (k in 1:length(ind1)) {
            ind2 <- ind1[k]
            j <- j1
            while (length(ind2) > 0 & cyclic == FALSE) {
              if (length(ind2) == 1) {
                j <- j + 1
                child <- parent_child$Child[ind2]
                genealogy_i <- c(genealogy_i, child)
                ind2 <- which(parent_child$Parent == child)
                cyclic <- identical(ind2,ind1) & length(ind2) > 0
              } else {
                j2 <- j
                genealogy_iii <- genealogy_i
                for (k1 in 1:length(ind2)) {
                  ind3 <- ind2[k1]
                  j <- j2
                  while (length(ind3) > 0 & cyclic1 == FALSE) {
                    if (length(ind3) == 1) {
                      j <- j + 1
                      child <- parent_child$Child[ind3]
                      genealogy_i <- c(genealogy_i, child)
                      ind3 <- which(parent_child$Parent == child)
                      cyclic1 <- identical(ind3,ind2) | identical(ind3,ind1) & length(ind3) > 0 
                    } else {
                      j3 <- j
                      genealogy_iv <- genealogy_i
                      for (k2 in 1:length(ind3)) {
                        ind4 <- ind3[k2]
                        j <- j3
                        while (length(ind4) > 0 & cyclic2 == FALSE) {
                          if (length(ind4) == 1) {
                            j <- j + 1
                            child <- parent_child$Child[ind4]
                            genealogy_i <- c(genealogy_i, child)
                            ind4 <- which(parent_child$Parent == child)
                            cyclic2 <- identical(ind4,ind3) | identical(ind4,ind2) | identical(ind4,ind1) & length(ind4) > 0
                          } else {
                            j4 <- j
                            genealogy_v <- genealogy_i
                            for (k3 in 1:length(ind4)) {
                              ind5 <- ind4[k3]
                              j <- j4
                              while (length(ind5) > 0 & cyclic3 == FALSE) {
                                if (length(ind5) == 1) {
                                  j <- j + 1
                                  child <- parent_child$Child[ind5]
                                  genealogy_i <- c(genealogy_i, child)
                                  ind5 <- which(parent_child$Parent == child)
                                  cyclic3 <- identical(ind5,ind4) | identical(ind5,ind3) | identical(ind5,ind2) | identical(ind5,ind1) & length(ind5) > 0
                                } else {
                                  j5 <- j
                                  genealogy_vi <- genealogy_i
                                  for (k4 in 1:length(ind5)) {
                                    ind6 <- ind5[k4]
                                    j <- j5
                                    while (length(ind6) > 0 & cyclic4 == FALSE) {
                                      if (length(ind6) == 1) {
                                        j <- j + 1
                                        child <- parent_child$Child[ind6]
                                        genealogy_i <- c(genealogy_i, child)
                                        ind6 <- which(parent_child$Parent == child)
                                        cyclic4 <- identical(ind6,ind5) | identical(ind6,ind4) | identical(ind6,ind3) | identical(ind6,ind2) | identical(ind6,ind1) & length(ind6) > 0
                                      } else {
                                        j6 <- j
                                        genealogy_vii <- genealogy_i
                                        for (k5 in 1:length(ind6)) {
                                          ind7 <- ind6[k5]
                                          j <- j6
                                          while (length(ind7) > 0 & cyclic5 == FALSE) {
                                            j <- j + 1
                                            child <- parent_child$Child[ind7]
                                            genealogy_i <- c(genealogy_i, child)
                                            ind7 <- which(parent_child$Parent == child)
                                            cyclic5 <- identical(ind7,ind6) | identical(ind7,ind5) | identical(ind7,ind4) | identical(ind7,ind3) | identical(ind7,ind2) | identical(ind7, ind1) & length(ind7) > 0
                                          }
                                          genealogy.df <- rbind(genealogy.df, c(genealogy_i, rep(NA, ncol(genealogy.df) - length(genealogy_i))))
                                          genealogy_i <- genealogy_vii
                                        }
                                        ind6 <- character(0)
                                      }
                                    }
                                    if (!identical(genealogy_i, genealogy_vii)) {
                                      genealogy.df <- rbind(genealogy.df, c(genealogy_i, rep(NA, ncol(genealogy.df) - length(genealogy_i))))
                                    }
                                    genealogy_i <- genealogy_vi
                                  }
                                  ind5 <- character(0)
                                }
                              }
                              if (!identical(genealogy_i, genealogy_vi)) {
                                genealogy.df <- rbind(genealogy.df, c(genealogy_i, rep(NA, ncol(genealogy.df) - length(genealogy_i))))
                              }
                              genealogy_i <- genealogy_v
                            }
                            ind4 <- character(0)
                          }
                        }
                        if (!identical(genealogy_i, genealogy_v)) {
                          genealogy.df <- rbind(genealogy.df, c(genealogy_i, rep(NA, ncol(genealogy.df) - length(genealogy_i))))
                        }
                        genealogy_i <- genealogy_iv
                      }
                      ind3 <- character(0)
                    }
                  }
                  if (!identical(genealogy_i, genealogy_iv)) {
                    genealogy.df <- rbind(genealogy.df, c(genealogy_i, rep(NA, ncol(genealogy.df) - length(genealogy_i))))
                  }
                  genealogy_i <- genealogy_iii
                }
                ind2 <- character(0)
              }
            }
            if (!identical(genealogy_i, genealogy_iii)) {
              genealogy.df <- rbind(genealogy.df, c(genealogy_i, rep(NA, ncol(genealogy.df) - length(genealogy_i))))
            }
            genealogy_i <- genealogy_ii
          }
          ind1 <- character(0)
        }
      }
      if (!identical(genealogy_i, genealogy_ii)) {
        genealogy.df <- rbind(genealogy.df, c(genealogy_i, rep(NA, ncol(genealogy.df) - length(genealogy_i))))
      }
    }
  }
  names(genealogy.df) <- c("Parent", paste0("Child", 1:(ncol(genealogy.df) - 1)))
  return(genealogy.df)
}

## Find the youngest generation of water right
find_youngest <- function(x) {
  cond1 <- phase[x,] |> as.character() |> strsplit(split="\\|") |> 
    lapply(function(y) length(grep("App", y, invert=TRUE)) > 0) |> unlist() & 
    !is.na(parent_genealogy[x,])
  exclude_stage <- "Pending|Denied|Withdrawn|Rejected|Cancelled"
  cond2 <- union(grep(exclude_stage, stage[x,], invert=TRUE), which(is.na(stage[x,])))
  cond3 <- intersect(which(cond1), cond2)
  cond4 <- intersect(grep("Partnership Water Banking|Drought|Seasonal Changes|Temporary Use", assignment[x,], invert=T), cond3)
  if (length(cond4) == 0) {
    out <- NA
  } else {
    col_num <- max(cond4)
    out <- parent_genealogy[x,col_num]
  }
  return(out)
}
## Determine whether forfeiture occurred anywhere in the genealogy
get_diminishment <- function(row_num) {
  sum_child1 <- which(parent_genealogy[row_num,"youngest"] != parent_genealogy[row_num, "Parent"]) |>
    intersect(y=grep("Seasonal Changes", assignment[row_num, "youngest"], invert=T)) |>
    intersect(y=which(unlist(lapply(strsplit(AnnualVol[row_num,"youngest"], split="\\|"), function(x) sum(as.numeric(gsub("NA", NA, x)), na.rm=T))) > 0))
  sum_child1_Qi <- which(parent_genealogy[row_num,"youngest"] != parent_genealogy[row_num, "Parent"]) |>
    intersect(y=grep("Seasonal Changes", assignment[row_num, "youngest"], invert=T)) |>
    intersect(y=which(unlist(lapply(strsplit(Qi[row_num,"youngest"], split="\\|"), function(x) sum(as.numeric(gsub("NA", NA, x)), na.rm=T))) > 0))
  sum_child1_IA <- which(parent_genealogy[row_num,"youngest"] != parent_genealogy[row_num, "Parent"]) |>
    intersect(y=grep("Seasonal Changes", assignment[row_num, "youngest"], invert=T)) |>
    intersect(y=which(unlist(lapply(strsplit(IA[row_num,"youngest"], split="\\|"), function(x) sum(as.numeric(gsub("NA", NA, x)), na.rm=T))) > 0))
  sum_child2 <- which(parent_genealogy[row_num,"youngest"] != parent_genealogy[row_num, "Parent"]) |>
    intersect(y=grep("Seasonal Changes", assignment[row_num, "youngest"], invert=T))
  Relinquished <- ifelse(length(grep("Relinq|Relinquishment", c(events[row_num,]), ignore.case=T)) > 0, TRUE, FALSE)
  LackOfDiligence <- ifelse(sum(LackDiligence(row_num)$Qa) < 0, TRUE, FALSE)
  Parent_phase <- strsplit(phase[row_num[1], "Parent"], split="\\|")[[1]]
  Parent_Qa <- strsplit(AnnualVol[row_num[1],"Parent"], split="\\|")[[1]] |> 
    gsub(pattern="NA", replace=NA) |> as.numeric()
  Parent_Qa <- if (length(Parent_Qa) < 2) { Parent_Qa } else { Parent_Qa[grep("App", Parent_phase, invert=TRUE)] }
  Parent_Qa <- na.omit(Parent_Qa)
  Parent_Qi <- strsplit(Qi[row_num[1],"Parent"], split="\\|")[[1]] |> 
    gsub(pattern="NA", replace=NA) |> as.numeric()
  Parent_Qi <- if (length(Parent_Qi) < 2) { Parent_Qi } else { Parent_Qi[grep("App", Parent_phase, invert=TRUE)] }
  Parent_Qi <- na.omit(Parent_Qi)
  Parent_IA <- strsplit(IA[row_num[1],"Parent"], split="\\|")[[1]] |> 
    gsub(pattern="NA", replace=NA) |> as.numeric()
  Parent_IA <- if (length(Parent_IA) < 2) { Parent_IA } else { Parent_IA[grep("App", Parent_phase, invert=TRUE)] }
  Parent_IA <- na.omit(Parent_IA)
  Parent_Qa <- if(length(which(Parent_Qa == 0)) > 0) { Parent_Qa[-which(Parent_Qa == 0)] } else { Parent_Qa }
  Parent_Qi <- if(length(which(Parent_Qi == 0)) > 0) { Parent_Qi[-which(Parent_Qi == 0)] } else { Parent_Qi }
  Parent_IA <- if(length(which(Parent_IA == 0)) > 0) { Parent_IA[-which(Parent_IA == 0)] } else { Parent_IA }
  Parent_Qa <- ifelse(length(Parent_Qa) == 0, NA, min(Parent_Qa, na.rm=T))
  Parent_Qi <- ifelse(length(Parent_Qi) == 0, NA, min(Parent_Qi, na.rm=T))
  Parent_IA <- ifelse(length(Parent_IA) == 0, NA, min(Parent_IA, na.rm=T))
  Child_Qa <- get_child_quant(row_num, Parent_Qa, sum_child1, "AnnualQuant")
  Child_Qi <- get_child_quant(row_num, Parent_Qi, sum_child1_Qi, "InstantQ")
  Child_IA <- get_child_quant(row_num, Parent_IA, sum_child1_IA, "IrrArea")
  compare_Qa <- get_diff(row_num)
  if (length(grep("Yakima Adjud", assignment[row_num,1:(ncol(assignment)-1)])) == 0) {
    active_Yakima <- FALSE
    active_Yakima2 <- FALSE
  } else {
    active_Yakima <- !all(status[row_num, grep("Yakima Adjud", assignment[row_num,1:(ncol(assignment)-1)])] == "Inactive")
    active_Yakima2 <- !all(apply(status[row_num,-ncol(status)], 1, function(x) length(which(x=="Active"))) < 2)
  }
  AllInactive <- ifelse(length(which(status[row_num,] != "Inactive")) == 0, TRUE, FALSE)
  TrustWater <- ifelse(length(grep("TW Acquisition|Partnership Water Banking|Trust Wtr Right", assignment[row_num,])) > 0, TRUE, FALSE)
  active_parent <- any(unlist(apply(status[row_num,-ncol(status)], 1, function(x) x[tail(which(x != "Inactive"), 1) - 1])) != "Inactive")
  ChangeOnPortion <- ifelse(length(grep("ChangePartOfWR", change_intent[row_num,])) > 0 & compare_Qa >= 0 & phase$Parent[row_num][1] != "Claim" & !is.na(Child_Qa) & !is.na(Parent_Qa) & active_parent == TRUE, TRUE, FALSE)
  ChangeOnPortion <- ifelse(TrustWater == TRUE & ChangeOnPortion == FALSE & 
    (((status$Parent[row_num][1] != "Inactive" | active_parent == TRUE) & phase$Parent[row_num][1] != "Claim") | (length(grep("Yakima Adjud", assignment[row_num,])) > 0 & active_Yakima2 == TRUE)) & 
    length(grep("Temp donation, full|Temp Donation \\(Full\\)", event_comment[row_num,], ignore.case=T)) == 0 &
    length(grep("TW Full Temporary", assignment[row_num,])) == 0 &
    (compare_Qa >= 0 | length(grep("partial", c(comments[row_num,], event_comment[row_num,]), ignore.case=T)) > 0 | 
    length(unlist(apply(event_comment[row_num,], 2, function(x) grep("partial|Donation.*portion|TW.*portion|remaining portion", x, ignore.case=T)))) > 0) & 
    length(grep("ROE|Super", strsplit(phase[row_num[1],1], "\\|")[[1]], ignore.case=T, invert=T)) > 0, TRUE, ChangeOnPortion)
  ChangeOnPortion <- ifelse(ChangeOnPortion == TRUE & compare_Qa >= 0 & !is.na(Parent_Qa == Child_Qa) & Parent_Qa == Child_Qa, FALSE, ChangeOnPortion)
  ChangeOnPortion <- ifelse(is.na(ChangeOnPortion), FALSE, ChangeOnPortion)
  ChangeOnPortion <- ifelse(length(grep("partial relinquishment", relinquishment$comments[which(relinquishment$Parent == parent_genealogy$Parent[row_num][1])])) > 0 & ChangeOnPortion == "TRUE", "FALSE", ChangeOnPortion)
  ChangeOnPortion <- ifelse(length(grep("no relinquishment", relinquishment$comments[which(relinquishment$Parent == parent_genealogy$Parent[row_num][1])])) > 0, TRUE, ChangeOnPortion)
  if (is.na(Child_Qa) | is.na(Parent_Qa) | (phase$Parent[row_num][1] == "NewApp" & status$Parent[row_num][1] != "Inactive")) {
    if (is.na(Child_Qi) | is.na(Parent_Qi)) {
      if (is.na(Child_IA) | is.na(Parent_IA)) {
        IsDiminished <- ifelse(Relinquished == TRUE | LackOfDiligence == TRUE, TRUE, FALSE) 
      } else {
        IsDiminished <- ifelse(Relinquished == TRUE | LackOfDiligence == TRUE | (round(Parent_IA, 0) > round(Child_IA, 0) & ChangeOnPortion == FALSE), TRUE, FALSE)
      }
    } else {
      IsDiminished <- ifelse(Relinquished == TRUE | LackOfDiligence == TRUE | (round(Parent_Qi, 2) > round(Child_Qi, 2) & ChangeOnPortion == FALSE), TRUE, FALSE)
    }
  } else {
    IsDiminished <- ifelse(LackOfDiligence == TRUE | Relinquished == TRUE |
      length(grep("partial relinquishment", relinquishment$comments[which(relinquishment$Parent == parent_genealogy$Parent[row_num][1])])) > 0 |
       ChangeOnPortion == FALSE & Child_Qa > 0 & round(Parent_Qa, 0) > round(Child_Qa), TRUE, FALSE)
  }
  IsDiminished <- ifelse(length(grep("no relinquishment", relinquishment$comments[which(relinquishment$Parent == parent_genealogy$Parent[row_num][1])])) > 0, FALSE, IsDiminished)
  IsDiminished <- ifelse(length(grep("partial relinquishment", relinquishment$comments[which(relinquishment$Parent == parent_genealogy$Parent[row_num][1])])) > 0, TRUE, IsDiminished)
  IsDiminished <- ifelse(phase$Parent[row_num[1]] == "NewApp" & status$Parent[row_num][1] != "Inactive", FALSE, IsDiminished)
  diminishment <- data.frame(Parent=Parent_Qa, Child=Child_Qa, diff=compare_Qa,
  ChangeOnPortion=ChangeOnPortion, Relinquished=Relinquished, lack_of_diligence=LackOfDiligence, 
  IsDiminished=IsDiminished, AllInactive=AllInactive, TrustWater=TrustWater, row.names=NULL)
  return(diminishment)
}

## Was there lack of due diligence between the permit and certificate phases?
LackDiligence <- function(row_num) {
  ldf.df <- NULL
  Qa_ld <- na.omit(stack(AnnualVol_raw[row_num,]))
  Qa_ld$key <- row.names(Qa_ld)
  p_ld <- na.omit(stack(phase[row_num,]))
  p_ld$key <- row.names(p_ld)
  nm_ld <- na.omit(stack(parent_genealogy[row_num,]))
  nm_ld$key <- row.names(nm_ld)
  df <- merge(Qa_ld[,c("key", "values")], p_ld[,c("key", "values")], by="key", all=T) |> merge(y=nm_ld[,c("key", "values", "ind")], by="key", all=T)
  df <- unique(na.omit(df[,-1]))
  names(df) <- c("Qa", "Phase", "WRDocID", "gen")
  check_ind <- grep("(Permit|Certificate).*(Permit|Certificate)", df$Phase)
  for (n in check_ind) {
    docid <- df$WRDocID[n]
    Q.df <- subset(df, WRDocID==docid)$Qa |> sapply(function(x) strsplit(x, split="\\|")) |> lapply(function(x) gsub("NA", NA, x)) |> unlist() |> as.numeric()
    p.df <- subset(df, WRDocID==docid)$Phase |> sapply(function(x) strsplit(x, split="\\|")) |> unlist()
    permit <- Q.df[p.df %in% c("Permit", "SupersedingPermit")][1]
    cert <- Q.df[p.df == "Certificate"][1]
    ldf.df <- rbind(ldf.df, data.frame(Qa=min(0, cert - permit), WRDocID=docid, gen=df$gen[n]))
  }
  ldf.df <- na.omit(ldf.df)
  ldf.df <- ldf.df[!duplicated(ldf.df[,-3]),]
  if (is.null(ldf.df)) {
    ldf.df <- data.frame(Qa=0, WRDocID=NA, gen=NA)
  }
  return(ldf.df)
}

## Calculate sume of child quantities
get_child_quant <- function(row_num, parent, child_ind, type) {
  parent_quant <- parent
  if (type == "AnnualQuant") {
    child_quant <- AnnualVol
  } else if (type == "InstantQ") {
    child_quant <- Qi
  } else if (type == "IrrArea") {
    child_quant <- IA
  }
  if (length(child_ind) == 0 | is.na(parent_quant)) {
    Child_o <- NA
  } else {
    sum_child <- child_ind[!(duplicated(parent_genealogy[row_num,"youngest"][child_ind]))]
    youngest_id <- parent_genealogy[row_num[sum_child], "youngest"]
    youngest_NR <- doc_NR[row_num[sum_child], "youngest"] |>
      gsub(pattern="([0-9])(C|CL)", replacement="\\1") |>
      gsub(pattern="(C*)([A-Z])", replacement="\\2") |>
      gsub(pattern="\\)\\([[:alnum:]]*-[[:alnum:]]*\\)", replacement="\\)") |>
      gsub(pattern="\\)\\([[:alnum:]]*\\)", replacement="\\)") |>
      gsub(pattern="(\\()([A-Z])(\\))", replacement="\\2") |>
      gsub(pattern="(P*)([A-Z])", replacement="\\2") |>
      gsub(pattern="([A-Z])(P*)", replacement="\\1") |>
      gsub(pattern="@[0-9]*", replacement="") |>
      gsub(pattern=" PEND", replacement="") |> 
      gsub(pattern="([A-Z][0-9]-)([A-Z]*)", replacement="\\1") |>
      gsub(pattern= "(-)(0)", replacement="\\1")
    lookup = NULL
    for (n in 1:length(youngest_NR)) {
      ind1 <- row_num[sum_child][n]
      youngest_quant <- child_quant[ind1, "youngest"] |> strsplit(split="\\|") |> 
        lapply(function(x) as.numeric(gsub("NA",NA,x))) |> unlist()
      youngest_phase <- phase[ind1, "youngest"] |> strsplit(split="\\|") |> 
        lapply(function(x) x) |> unlist()
      if (length(youngest_quant) > 1) {
        if (all(is.na(youngest_quant[grep("App", youngest_phase, ignore.case=T, invert=T)]))) {
          youngest_quant <- youngest_quant[grep("App", youngest_phase, ignore.case=T, invert=F)]
        } else {
          youngest_quant <- youngest_quant[grep("App", youngest_phase, ignore.case=T, invert=T)] 
        }
      }
      if (length(grep("DonationAccept", stage[row_num, "youngest"])) > 0) {
        if (sum(!is.na(youngest_quant)) == 0) {
          CQ <- NA
        } else {
          CQ <- min(youngest_quant, na.rm=T)
        }
      } else {
        if (sum(!is.na(youngest_quant)) == 0) {
          CQ <- NA
        } else {
          CQ <- max(youngest_quant, na.rm=T)
        }
      }
      lookup <- rbind(lookup, data.frame(NM=youngest_NR[n], Q=CQ, id=youngest_id[n]))
    }
    lookup <- na.omit(lookup)
    if (length(grep("P[0-9]", youngest_NR)) > 0 & length(grep("P[0-9]", youngest_NR, invert=T)) > 0) {
        lookup <- lookup[-grep("P[0-9]", youngest_NR),]
    }
    if (is.na(parent_quant)) {
      Child_o <- sum(lookup$Q)
    } else {
      if (nrow(lookup) > 1 & round(sum(lookup$Q), 0) > round(parent_quant, 0) & length(which(lookup$Q >= parent_quant)) > 0 & parent_quant > 0 & (length(grep("R", youngest_NR)) > 0 | length(grep("TW Acquisition", assignment[row_num[sum_child], "youngest"])) > 0)) {
        if (length(grep("R", youngest_NR)) > 0 | length(grep("Yakima Adjud", assignment[row_num,])) > 0) {
          lookup <- lookup[which(lookup$Q >= parent_quant),]
        } else {
          lookup_active <- parent_genealogy[row_num[sum_child], "youngest"][which(status[row_num[sum_child], "youngest"] != "Inactive")]
          lookup_active <- lookup[lookup$id %in% lookup_active,]
          if (round(sum(lookup$Q), 2) > round(parent_quant, 2)) {
            lookup <- lookup_active
            if (round(sum(lookup$Q), 2) > round(parent_quant, 2)) {
              if (length(which(lookup$Q >= parent_quant)) < nrow(lookup)) {
                lookup <- lookup[-which(lookup$Q >= parent_quant),]
              }
            }
          }
        }
        if (nrow(lookup) == 0) {
          Child_o <- NA
        } else {
          Child_o <- sum(lookup$Q)
        }
      } else if (round(sum(lookup$Q), 2) > round(parent_quant, 2)) {
        lookup_active <- parent_genealogy[row_num[sum_child], "youngest"][which(status[row_num[sum_child], "youngest"] != "Inactive")]
        lookup_active <- lookup[lookup$id %in% lookup_active,]
        if (nrow(lookup_active) == 0) {
          lookup <- lookup[!duplicated(lookup$Q),]
        }
        if (round(sum(lookup$Q), 2) > round(parent_quant, 2)) {
          lookup <- lookup_active
        }
        if (round(sum(lookup$Q), 2) <= round(parent_quant, 2)) {
          Child_o <- sum(lookup$Q)
        } else {
          if (get_diff(row_num) >= 0) {
            Child_o <- sum(aggregate(Q ~ NM, lookup, max)$Q)
          } else {
            Child_o <- sum(aggregate(Q ~ NM, lookup, min)$Q)
          }
        }
      } else {
        Child_o <- sum(lookup$Q)
      }
    }
  }
  return(Child_o)
}

## Compute difference between quantity in application and certificate phases
get_diff <- function(row_num) {
  df1 <- na.omit(data.frame(stack(AnnualVol_raw[row_num,-14])$values, stack(phase[row_num,-14])$values, 
    stack(parent_genealogy[row_num,-14])$values))
  if (nrow(df1) == 0) {
    out <- 0
  } else {
    Qa.diff <- df1[,1]
    phase.diff <- df1[,2]
    names(Qa.diff) <- names(phase.diff) <- df1[,3]
    df <- data.frame(unlist(strsplit(Qa.diff, "\\|")), unlist(strsplit(phase.diff, "\\|")))
    df$WRDocID <- substr(names(unlist(strsplit(Qa.diff, "\\|"))), 1, 7)
    names(df) <- c("Qa", "Phase", "WRDocID")
    df <- unique(df)
    df$Qa[df$Qa==0] <- NA
    df <- na.omit(df)
    rm_ind <- which(df$WRDocID %in% names(which(table(df$WRDocID)==1)))
    if (length(rm_ind) > 0) {
      df <- df[-rm_ind,]
    }
    cond <- length(grep("App", df$Phase)) == 0 | length(grep("App", df$Phase, invert=T)) == 0 |
      nrow(na.omit(df)) == 0
    if (cond) {
      out <- 0
    } else {
      first.df <- aggregate(Qa ~ WRDocID, data=df[grep("App", df$Phase, invert=T),], function(x) min(x, na.rm=T))
      second.df <- df[grep("App", df$Phase),]
      df2 <- merge(first.df, second.df[,-2], by="WRDocID", all=T)
      names(df2) <- c("WRDocID", "Qa1", "Qa2")
      df2[,-1] <- apply(df2[,-1], 2, function(x) as.numeric(gsub("NA", NA, x)))
      out <- sum(apply(df2[,-1], 1, function(x) min(0, x[1] - x[2])), na.rm=T)
    }
  }
  return(out)
}
## Determine whether the change resulted in diminishment of the parent water right
## child_quant0 == TRUE -> Diminishment == FALSE
child_quant0 <- function(p, df) {
  child.df <- subset(ChangeIntent, Parent==df$Parent[p])
  child.df$IsDrought <- ifelse(length(grep("Drought", child.df$Phase)) > 0, TRUE, FALSE)
  parent_compare_Qa <- get_diff0(child.df$Parent_Qa_raw[1], child.df$Parent_phase[1])
  remove_child <- unique(child.df$WaRecId[which(child.df$IsDrought == TRUE | 
    grepl("VOID", child.df$PrimaryNumber) | 
    child.df$WaRecId %in% c(NA, child.df$Parent[1]) | 
    child.df$LastInactive == TRUE | child.df$IsApp == TRUE)])
  if (length(remove_child) > 0) {
    if (df$WaRecId[p] %in% remove_child) {
      return(FALSE)
    } else {
      child.df <- child.df[-which(child.df$WaRecId %in% remove_child),]
    }
  }
  if(nrow(na.omit(child.df[,c("Qa", "Parent_Qa")])) == 0) {
    if (nrow(na.omit(child.df[,c("Qi", "Parent_Qi")])) == 0) {
      child_quant <- NA
      return(child_quant)
    } else {
      col <- c("Qi", "Parent_Qi", "WaRecId")
    }
  } else {
    col <- c("Qa", "Parent_Qa", "WaRecId")
  }
  child.df <- unique(child.df[,col])
  if (sum(child.df[,1], na.rm=T) > child.df[1,2]) {
    child.df <- child.df[!duplicated(child.df[,1]),]
  }
  if (sum(child.df[,1], na.rm=T) > child.df[1,2]) {
    if (length(which(child.df[,1] >= child.df[1,2])) > 0) {
      child.df <- child.df[-which(child.df[,1] >= child.df[1,2]),]
      if (nrow(child.df) == 0) {
        return(TRUE)
      }
    } else {
      child.df <- child.df[which(child.df[,1] == max(child.df[,1], na.rm=T)),]
    }
  }
  round_to <- ifelse(col[1] == "Qa", 1, 2)
  child_quant <- (round(sum(child.df[,1], na.rm=T), round_to) >= round(child.df[1,2], round_to) & (parent_compare_Qa == 0 | sum(child.df$compare_Qa) == 0)) | 
    df$Phase[p] %in% c("SupersedingCertificate", "SupersedingPermit")
  return(child_quant)
}

## Calculate the quantity of diminishment
child_quantQ <- function(p, df) {
  #print(p)
  child.df <- subset(ChangeIntent_backup, Parent==df$Parent[p])
  child.df$IsDrought <- ifelse(grepl("Drought", child.df$Phase), TRUE, FALSE)
  parent_compare_Qa <- get_diff0(child.df$Parent_Qa_raw[1], child.df$Parent_phase[1])
  remove_child <- unique(child.df$WaRecId[which(child.df$IsDrought == TRUE | 
    grepl("VOID", child.df$PrimaryNumber) | 
    child.df$WaRecId %in% c(NA, child.df$Parent[1]) | 
    child.df$LastInactive == TRUE | child.df$IsApp == TRUE)])
  if (length(remove_child) > 0) {
    if (df$WaRecId[p] %in% remove_child) {
      return(FALSE)
    } else {
      child.df <- child.df[-which(child.df$WaRecId %in% remove_child),]
    }
  }
  col <- c("Qa", "Parent_Qa", "WaRecId")
  child.df <- unique(child.df[,col])
  child.df$status <- water_rights$Status[match(child.df$WaRecId, water_rights$WRDocID)]
  if (sum(child.df[,1], na.rm=T) == 0 | is.na(child.df[1,2])) {
    child_quantQ <- NA
  } else {
    if (sum(child.df[,1], na.rm=T) > child.df[1,2]) {
      if (sum(unique(child.df[,1]), na.rm=T) > child.df[1,2]) {
        child.df_active <- child.df[which(child.df$status != "Inactive"),]
        if (nrow(child.df_active) > 0) {
          child.df <- child.df_active
        } else {
          child.df <- child.df
        }
      } else {
        child.df <- child.df[!duplicated(child.df[,1]),]
      }
    }
    if (sum(child.df[,1], na.rm=T) > child.df[1,2]) {
      child.df <- child.df[!duplicated(child.df[,1]),]
    }
    if (sum(child.df[,1], na.rm=T) > child.df[1,2]) {
      if (length(which(child.df[,1] >= child.df[1,2])) > 0 & length(which(child.df[,1] < child.df[1,2])) > 0) {
        child.df1 <- child.df[-which(child.df[,1] >= child.df[1,2]),]
        child.df2 <- child.df[-which(child.df[,1] < child.df[1,2]),]
        if (sum(child.df1$Qa, na.rm=T) > sum(child.df2$Qa, na.rm=T)) {
          child.df <- child.df2
        } else {
          child.df <- child.df1
        }
      } else if (length(which(child.df[,1] >= child.df[1,2])) > 0) {
        child.df <- child.df
      } else {
        child.df <- child.df[which(child.df[,1] == max(child.df[,1], na.rm=T)),]
      }
    }
    round_to <- ifelse(col[1] == "Qa", 2, 2)
    child_quantQ <- round(sum(child.df[,1], na.rm=T), round_to) - round(child.df[1,2], round_to)
  }
  return(child_quantQ)
}
## Is the water right the last in the geneology and inactivated because of a cancellation or denial?
last_inactive <- function(docid) {
  df <- genealogy.ls[[as.character(ChangeIntent$Oldest[which(ChangeIntent$WaRecId==docid)][1])]]
  if (length(df) > 0) {
    gen <- df$gen[,-ncol(df$gen)]
    loc <- which(gen == docid)[1]
    row_ind <- ifelse(loc %% nrow(gen) == 0, nrow(gen), loc %% nrow(gen))
    i <- ceiling(loc / nrow(gen))
    if (i == 1) {
      out <- FALSE
    } else {
      sg <- df$stage[row_ind,i]
      out <- ifelse(length(grep("Pending|Denied|Withdrawn|Rejected|Cancelled", ignore.case=T, sg)) > 0, TRUE, FALSE)
    }
  } else {
    out <- NA
  }
  return(out)
}
## Retrieve the annual quantity 
get_Qa <- function(x, dfp, dfq) {
  Q_out <- strsplit(as.character(dfq[x]), split="\\|")[[1]]
  if (length(Q_out) == 1) {
    return(as.numeric(gsub("NA", NA, Q_out)))
  } else {
    phase_o <- strsplit(dfp[x], split="\\|")[[1]]
    Q_out.df <- data.frame(Q_out, phase_o)
    rm_ind <- grep("App", Q_out.df$phase_o)
    if (length(rm_ind) > 0) {
      Q_out.df <- Q_out.df[-rm_ind,]
    }
    Q_out.df$Q_out[Q_out.df$Q_out %in% c(0, "NA")] <- NA
    Q_out.df <- na.omit(Q_out.df)
    if (length(Q_out.df$Q_out) == 0) {
      Q_out <- NA
    } else {
      if (length(grep("Certificate", Q_out.df$phase_o)) > 0) {
        Q_out <- na.omit(Q_out.df$Q_out[grep("Certificate", Q_out.df$phase_o)])[1]
      } else {
        Q_out <- min(Q_out.df$Q_out)
      }
    }
    return(Q_out)
  }
}
## Calculate difference between change ROE and Application phase quantities
get_diff0 <- function(Qa, phase0) {
  Q <- as.character(Qa) |> strsplit(split="\\|") |> unlist()
  App <- phase0 |> strsplit(split="\\|") |> unlist() |> grep(pattern="App")
  ROE <- phase0 |> strsplit(split="\\|") |> unlist() |> grep(pattern="App", invert=T)
  if (length(App) > 0 & length(ROE) > 0) {
    compare_o <- min(0, as.numeric(gsub("NA", NA, Q[ROE])) - as.numeric(gsub("NA", NA, Q[App])))
  } else {
    compare_o <- 0
  }
  compare_o <- ifelse(is.na(compare_o), 0, compare_o)
  return(compare_o)
}
diminishing_change0 <- function(x) {
  df <- ChangeIntent[x,]
  out <- ifelse(length(grep("Drought", df$Phase)) == 0 & df$IsDiminished == TRUE & df$TrustDonation == FALSE &
    ((df$Relinquish == FALSE & df$lack_of_diligence == FALSE) | (df$compare_Qa < 0 | any(is.na(c(df$Qa, df$Parent_Qa))) | df$Qa < df$Parent_Qa)) &
    ifelse(any(is.na(c(df$Qa, df$Parent_Qa))), df$Qi < df$Parent_Qi & !is.na(df$Qi), 
    df$compare_Qa < 0 | df$Qa < df$Parent_Qa), TRUE, FALSE)
  return(out)
}
replace_zeroes <- function(x) {
  quants <- water_rights$QaTotal[x][[1]] |> strsplit(split="\\|") |> unlist()
  quants[quants=="0"] <- NA
  quants <- paste(quants, collapse="|")
  return(quants)
}
## Calculate the date of issuance for water right documents, priority is certificate.
DocDate2 <- function(input_df, event_colname, date_colname, x) {
  df <- input_df
  out.df <- data.frame(matrix(nrow=1, ncol=0))
  out.df$DocDate <- ifelse(length(grep("CertIssued", df[x, event_colname])) == 0, NA,
    gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("CertIssued", strsplit(df[x,event_colname], "\\|")[[1]])]))  
  out.df$DocDate2 <- ifelse(length(grep("PermitIssued", df[x, event_colname])) == 0, NA,
    gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("PermitIssued", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$DocDate3 <- ifelse(length(grep("SuAmDocIssued", df[x, event_colname])) == 0, NA,
    gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("SuAmDocIssued", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$DocDate4 <- ifelse(length(grep("Seasonal|TW-AcqdTempOthe|BCFiled|WWWBAgrIssued|Split|TW-AcqdTempDon|TempPmtIssued|TW-AcqdPermDon|SDIssued|Assignment|CCFiled", df[x, event_colname])) == 0, NA,
    gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("Seasonal|TW-AcqdTempOthe|BCFiled|WWWBAgrIssued|Split|TW-AcqdTempDon|TempPmtIssued|TW-AcqdPermDon|SDIssued|Assignment|CCFiled", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$DocDate5 <- ifelse(length(grep("ROEIssued", df[x, event_colname])) == 0, NA,
    gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("ROEIssued", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$DocDate6 <- ifelse(length(grep("RODIssued", df[x, event_colname])) == 0, NA,
    gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("RODIssued", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$DocDate7 <- ifelse(length(grep("AppAccepted", df[x, event_colname])) == 0, NA,
    gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("AppAccepted", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$DocDate8 <- ifelse(length(grep("Received", df[x, event_colname])) == 0, NA,
    gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("Received", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$DocDate9 <- ifelse(length(grep("DocRecorded", df[x, event_colname])) == 0, NA,
    gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("DocRecorded", strsplit(df[x,event_colname], "\\|")[[1]])])) 
  out.df$DocDateFinal <- ifelse(is.na(out.df$DocDate), 
                                   ifelse(is.na(out.df$DocDate2), 
                                          ifelse(is.na(out.df$DocDate3), 
                                                 ifelse(is.na(out.df$DocDate4), 
                                                        ifelse(is.na(out.df$DocDate5), 
                                                               ifelse(is.na(out.df$DocDate6),
                                                                      ifelse(is.na(out.df$DocDate7), 
                                                                             ifelse(is.na(out.df$DocDate8), out.df$DocDate9, out.df$DocDate8),  
                                                                             out.df$DocDate7), out.df$DocDate6), out.df$DocDate5), out.df$DocDate4), 
                                                                             out.df$DocDate3), out.df$DocDate2), out.df$DocDate)
  out.df$DocDateFinal[is.na(out.df$DocDateFinal)] <- "2019-01-01"
  return(out.df$DocDateFinal)
}

## Calculate the date of issuance for water right documents, priority is change ROE.
DocDate <- function(input_df, event_colname, date_colname, x) {
  df <- input_df
  out.df <- data.frame(matrix(nrow=1, ncol=0))
  out.df$ChangeDate2 <- ifelse(length(grep("ROEIssued", df[x,event_colname])) == 0, NA, 
           gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("ROEIssued", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$ChangeDate3 <- ifelse(length(grep("RODIssued", df[x,event_colname])) == 0, NA, 
          gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("RODIssued", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$ChangeDate <- ifelse(length(grep("Relinqd", df[x,event_colname])) == 0, NA, 
           gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("Relinqd", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$ChangeDate4 <- ifelse(length(grep("CertIssued", df[x,event_colname])) == 0, NA, 
           gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("CertIssued", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$ChangeDate5 <- ifelse(length(grep("PermitIssued", df[x,event_colname])) == 0, NA, 
           gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("PermitIssued", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$ChangeDate6 <- ifelse(length(grep("Seasonal|TW-AcqdTempOthe|BCFiled|WWWBAgrIssued|Split|TW-AcqdTempDon|SuAmDocIssued|TempPmtIssued|TW-AcqdPermDon|SDIssued|Assignment|CCFiled", df[x,event_colname])) == 0, NA, 
            gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("Seasonal|TW-AcqdTempOthe|BCFiled|WWWBAgrIssued|Split|TW-AcqdTempDon|SuAmDocIssued|TempPmtIssued|TW-AcqdPermDon|SDIssued|Assignment|CCFiled", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$ChangeDate7 <- ifelse(length(grep("AppAccepted", df[x,event_colname])) == 0, NA, 
           gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("AppAccepted", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$ChangeDate8 <- ifelse(length(grep("Received", df[x,event_colname])) == 0, NA, 
           gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("Received", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$ChangeDate9 <- ifelse(length(grep("DocRecorded", df[x,event_colname])) == 0, NA, 
           gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("DocRecorded", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$ChangeDateFinal <- ifelse(is.na(out.df$ChangeDate), 
    ifelse(is.na(out.df$ChangeDate2), 
    ifelse(is.na(out.df$ChangeDate3), 
    ifelse(is.na(out.df$ChangeDate4), 
    ifelse(is.na(out.df$ChangeDate5), 
    ifelse(is.na(out.df$ChangeDate6),
    ifelse(is.na(out.df$ChangeDate7), 
    ifelse(is.na(out.df$ChangeDate8), out.df$ChangeDate9, out.df$ChangeDate8),  
    out.df$ChangeDate7), out.df$ChangeDate6), out.df$ChangeDate5), out.df$ChangeDate4), 
    out.df$ChangeDate3), out.df$ChangeDate2), out.df$ChangeDate)
  out.df$ChangeDateFinal[is.na(out.df$ChangeDateFinal)] <- "2019-01-01"
  return(out.df$ChangeDateFinal)
}
## Determine when the parent water right was changed
ChangeDate <- function(input_df, event_colname, date_colname, x) {
  df <- input_df
  out.df <- data.frame(matrix(nrow=1, ncol=0))
  out.df$ChangeDate <- ifelse(length(grep("ROEIssued", df[x,event_colname])) == 0, NA, 
                               gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("ROEIssued", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$ChangeDate2 <- ifelse(length(grep("RODIssued", df[x,event_colname])) == 0, NA, 
                               gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("RODIssued", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$ChangeDate3 <- ifelse(length(grep("CertIssued", df[x,event_colname])) == 0, NA, 
                               gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("CertIssued", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$ChangeDate4 <- ifelse(length(grep("PermitIssued", df[x,event_colname])) == 0, NA, 
                               gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("PermitIssued", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$ChangeDate5 <- ifelse(length(grep("Seasonal|TW-AcqdTempOthe|BCFiled|WWWBAgrIssued|Split|TW-AcqdTempDon|SuAmDocIssued|TempPmtIssued|TW-AcqdPermDon|SDIssued|Assignment|CCFiled", df[x,event_colname])) == 0, NA, 
                               gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("Seasonal|TW-AcqdTempOthe|BCFiled|WWWBAgrIssued|Split|TW-AcqdTempDon|SuAmDocIssued|TempPmtIssued|TW-AcqdPermDon|SDIssued|Assignment|CCFiled", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$ChangeDate6 <- ifelse(length(grep("DocRecorded", df[x,event_colname])) == 0, NA, 
                               gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("DocRecorded", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$ChangeDate7 <- ifelse(length(grep("Received", df[x,event_colname])) == 0, NA, 
                               gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("Received", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$ChangeDate8 <- ifelse(length(grep("AppAccepted", df[x,event_colname])) == 0, NA, 
                               gsub("NA|1901-01-01", NA, strsplit(df[x, date_colname], "\\|")[[1]][grep("AppAccepted", strsplit(df[x,event_colname], "\\|")[[1]])]))
  out.df$ChangeDateFinal <- ifelse(is.na(out.df$ChangeDate), 
                                   ifelse(is.na(out.df$ChangeDate2), 
                                          ifelse(is.na(out.df$ChangeDate3), 
                                                 ifelse(is.na(out.df$ChangeDate4), 
                                                        ifelse(is.na(out.df$ChangeDate5), 
                                                               ifelse(is.na(out.df$ChangeDate6),
                                                                  ifelse(is.na(out.df$ChangeDate7), out.df$ChangeDate8, out.df$ChangeDate7),  
                                                                             out.df$ChangeDate6), out.df$ChangeDate5), out.df$ChangeDate4), out.df$ChangeDate3), 
                                                 out.df$ChangeDate2), out.df$ChangeDate)
  out.df$ChangeDateFinal[is.na(out.df$ChangeDateFinal)] <- "2019-01-01"
  return(out.df$ChangeDateFinal)
}

################ Functions for summary tables ######################

ChangeIntentTable <- function(cat.ls, cat_names) {
  ChangeIntentType <- ChangeIntentFinal[ChangeIntentFinal$Parent %in% post2000_parents,c("WaRecId", "Parent", "WaRecChangeIntentTypeCode", "DiminishingChange", "ChildQ", "Parent_Qa", "Parent_Qa2", "nonconsumptive", "QiOnly")]
  #allQuant_Parent <- get_oldest_Qa()
  ChangeIntentType$Relinq_Qa <- allQuant_Parent$Relinq_Qa[match(ChangeIntentType$Parent, allQuant_Parent$Parent)]
  change_c.ls <- vector(length=length(cat.ls), mode="list")
  change.ls <- vector(length=length(cat.ls), mode="list")
  adjust.ls <- vector(length=length(cat.ls), mode="list")
  change_c.ls[[1]] <- unique(subset(ChangeIntentType, WaRecChangeIntentTypeCode %in% cat.ls[[1]])$Parent)
  compare_vect <- change_c.ls[[1]]
  for (i in 2:length(cat.ls)) {
    change_c.ls[[i]] <- unique(subset(ChangeIntentType, WaRecChangeIntentTypeCode %in% cat.ls[[i]])$Parent)
    change_c.ls[[i]] <- change_c.ls[[i]][!(change_c.ls[[i]] %in% compare_vect)]
    compare_vect <- c(compare_vect, change_c.ls[[i]])
  }
  for (i in 1:length(cat.ls)) {
    change.ls[[i]] <- unique(subset(ChangeIntentType, Parent %in% change_c.ls[[i]])[-c(1,3,5)])
    check_parent <- unique(change.ls[[i]]$Parent[change.ls[[i]]$DiminishingChange == TRUE])
    adjust.ls[[i]] <- check_parent[!(check_parent %in% unique(subset(ChangeIntentType, Parent %in% check_parent & WaRecChangeIntentTypeCode %in% cat.ls[[i]] & DiminishingChange == TRUE)$Parent))]
    if (length(adjust.ls[[i]]) > 0) {
      change.ls[[i]] <- change.ls[[i]][-which(change.ls[[i]]$Parent %in% adjust.ls[[i]]),]
    }
    change.ls[[i]]$RelRate <- ifelse(change.ls[[i]]$DiminishingChange == TRUE, change.ls[[i]]$Relinq_Qa / change.ls[[i]]$Parent_Qa, NA)
    if (i < length(cat.ls)) {
      change_c.ls[[i+1]] <- c(change_c.ls[[i+1]], adjust.ls[[i]])
    }
  }
  for (i in 1:length(cat.ls)) {
    diminished <- unique(change.ls[[i]]$Parent[change.ls[[i]]$DiminishingChange == TRUE])
    no_diminished <- unique(change.ls[[i]]$Parent[change.ls[[i]]$DiminishingChange == FALSE])
    rm_rows <- no_diminished[no_diminished %in% diminished]
    if (length(rm_rows) > 0) {
      change.ls[[i]] <- change.ls[[i]][-which(change.ls[[i]]$Parent %in% rm_rows & change.ls[[i]]$DiminishingChange == FALSE),]
    }
  }
  IntentQuant <- NULL
  for (i in 1:length(cat.ls)) {
    IntentQuant <- rbind(IntentQuant, cbind(IntentType=cat_names[i], change.ls[[i]]))
  }
  IntentQuant$Oldest <- allQuant_Parent$Oldest[match(IntentQuant$Parent, allQuant_Parent$Parent)]
  IntentQuant$Oldest_Qa <- ChangeIntentFinal$Oldest_Qa[match(IntentQuant$Parent, ChangeIntentFinal$Parent)]
  IntentQuant$Oldest_Qa2 <- IntentQuant$Oldest_Qa
  dup_oldest <- IntentQuant$Oldest[which(duplicated(IntentQuant$Oldest))]
  for (o in dup_oldest) {
    df <- subset(IntentQuant, Oldest == o)
    df$col_num <- 0
    df_gen <- genealogy.ls[[as.character(o)]]$gen
    for (p in df$Parent) {
      df$col_num[df$Parent == p] <- ceiling(which(as.matrix(df_gen) == p)[1] / nrow(df_gen)) # to which generation does the parent belong in the genealogy?
    }
    for (k in unique(df$IntentType)) { # loop over categorical variable (in this case whether the change is pre- or post-2000)
      sub_df <- subset(df, IntentType == k)
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
      IntentQuant$Oldest_Qa2[IntentQuant$Oldest == o & IntentQuant$IntentType == k] <- sum_Parent
    }
  }
  rownum <- which(IntentQuant$nonconsumptive == FALSE)
  IntentQuant$Relinq_Qa[IntentQuant$QiOnly == TRUE] <- 0
  IntentQuant.df <- cbind(aggregate(IntentQuant$Relinq_Qa[rownum], list(IntentQuant$IntentType[rownum], IntentQuant$Oldest[rownum]), sum),
                           aggregate(IntentQuant$Oldest_Qa2[rownum], list(IntentQuant$IntentType[rownum], IntentQuant$Oldest[rownum]), mean)[,3]) 
  names(IntentQuant.df) <- c("IntentType", "Oldest", "Relinq_Qa", "Oldest_Qa2")
  IntentQuant.df <- aggregate(IntentQuant.df[,c("Relinq_Qa", "Oldest_Qa2")], list(IntentQuant.df$IntentType), sum)
  names(IntentQuant.df) <- c("IntentType", "Relinq_Qa", "Oldest_Qa2")
  IntentQuant.df$AggRate <- IntentQuant.df$Relinq_Qa / IntentQuant.df$Oldest_Qa2
  
  IntentTable <- data.frame(cbind(aggregate(IntentQuant$DiminishingChange, list(IntentQuant$IntentType), length),
    aggregate(IntentQuant$DiminishingChange, list(IntentQuant$IntentType), sum)[,2],
    aggregate(IntentQuant$RelRate, list(IntentQuant$IntentType), function(x) mean(x, na.rm=T))[,2]))
  names(IntentTable) <- c("IntentType", "AllParents", "Relinquished", "RelRate")
  IntentTable$Freq <- IntentTable$Relinquished / IntentTable$AllParents
  IntentTable <- IntentTable[,c(1,2,3,5,4)]  
  IntentTable$AggRate <- IntentQuant.df$AggRate[match(IntentTable$IntentType, IntentQuant.df$IntentType)]
  IntentTable$Relinq_Qa <- round(IntentQuant.df$Relinq_Qa[match(IntentTable$IntentType, IntentQuant.df$IntentType)], 0)
  IntentTable[,-c(1:3)] <- apply(IntentTable[,-c(1:3)], 2, function(x) round(x, 3))
  IntentTable <- IntentTable[match(cat_names, IntentTable$IntentType),c(1:4,7,5,6)]
  return(list(IntentTable, IntentQuant))
}

## Calculates total forfeiture (volumetric) by original parent for all changed water rights
get_oldest_Qa_all <- function() {
  dimQuant <- subset(ChangeIntentFinal, ChangeIntentFinal$DiminishingChange==TRUE)[,c("WaRecId", "PrimaryNumber", "Parent", "Oldest", "WaRecChangeIntentTypeCode", "ChildQ", "Qa_raw", "Qa", "Parent_Qa_raw", "Parent_Qa", "Parent_Qa2", "Oldest_Qa", "compare_Qa", "WRIA", "County", "Region")]
  nodimQuant <- subset(ChangeIntentFinal, ChangeIntentFinal$DiminishingChange==FALSE)[,c("WaRecId", "PrimaryNumber", "Parent", "Oldest", "WaRecChangeIntentTypeCode", "ChildQ", "Qa_raw", "Qa", "Parent_Qa_raw", "Parent_Qa", "Parent_Qa2", "Oldest_Qa", "compare_Qa", "WRIA", "County", "Region")]
  #nodimQuant$ChildQ <- 0  ## If DiminishingChange == FALSE, the relinquishment should be zero
  allQuant <- rbind(cbind(DiminishingChange=TRUE, dimQuant), cbind(DiminishingChange=FALSE, nodimQuant))
  allQuant <- unique(allQuant[,-6])
  all_Quant_table <- unique(allQuant[,c("WaRecId", "Oldest", "Parent", "compare_Qa", "ChildQ", "WRIA", "Oldest_Qa", "Parent_Qa")]) 
  all_Quant_table$ChildQ <- -1 * all_Quant_table$ChildQ
  all_Quant.df <- aggregate(all_Quant_table[,c("ChildQ")], list(all_Quant_table$Oldest, all_Quant_table$Parent), sum) ## total diminishment of each parent doc
  names(all_Quant.df) <- c("Oldest", "Parent", "Relinq_Qa")
  all_Quant.df$Parent_Qa <- all_Quant_table$Parent_Qa[match(all_Quant.df$Parent, all_Quant_table$Parent)]
  all_Quant.df$Oldest_Qa <- all_Quant_table$Oldest_Qa[match(all_Quant.df$Oldest, all_Quant_table$Oldest)]
  dup_oldest <- unique(all_Quant.df$Oldest[duplicated(all_Quant.df$Oldest)])
  for (o in dup_oldest) {
    df <- subset(all_Quant.df, Oldest == o)
    all_Quant.df$Oldest_Qa[all_Quant.df$Oldest == o] <- min(sum(df$Parent_Qa2), mean(df$Oldest_Qa)) ## For aggregate reduction rate, the denominator is the portion of the original parent quantity affected by change authorization (excluding administrative splits)
  }
  all_Quant.df <- all_Quant.df[order(all_Quant.df$Oldest, all_Quant.df$Parent),]
  return(all_Quant.df)
}
## Calculates total forfeiture by original parent and parent for changed water rights post 2000
get_oldest_Qa <- function() {
  dimQuant <- subset(ChangeIntentFinal, ChangeIntentFinal$DiminishingChange==TRUE)[,c("WaRecId", "PrimaryNumber", "Parent", "Oldest", "WaRecChangeIntentTypeCode", "ChildQ", "Qa_raw", "Qa", "Parent_Qa_raw", "Parent_Qa", "Parent_Qa2", "Oldest_Qa", "compare_Qa", "WRIA", "County", "Region")]
  nodimQuant <- subset(ChangeIntentFinal, ChangeIntentFinal$DiminishingChange==FALSE)[,c("WaRecId", "PrimaryNumber", "Parent", "Oldest", "WaRecChangeIntentTypeCode", "ChildQ", "Qa_raw", "Qa", "Parent_Qa_raw", "Parent_Qa", "Parent_Qa2", "Oldest_Qa", "compare_Qa", "WRIA", "County", "Region")]
  allQuant <- rbind(cbind(DiminishingChange=TRUE, dimQuant), cbind(DiminishingChange=FALSE, nodimQuant))
  allQuant <- unique(allQuant[,-6])
  all_Quant_table <- unique(allQuant[,c("WaRecId", "Oldest", "Parent", "compare_Qa", "ChildQ", "WRIA", "Oldest_Qa", "Parent_Qa", "Parent_Qa2")]) 
  all_Quant_table$ChildQ <- -1 * all_Quant_table$ChildQ
  all_Quant.df <- aggregate(all_Quant_table[,c("ChildQ")], list(all_Quant_table$Oldest, all_Quant_table$Parent), sum)
  names(all_Quant.df) <- c("Oldest", "Parent", "Relinq_Qa")
  all_Quant.df$Parent_Qa <- all_Quant_table$Parent_Qa[match(all_Quant.df$Parent, all_Quant_table$Parent)]
  all_Quant.df$Parent_Qa2 <- all_Quant_table$Parent_Qa2[match(all_Quant.df$Parent, all_Quant_table$Parent)]
  all_Quant.df$Oldest_Qa <- all_Quant_table$Oldest_Qa[match(all_Quant.df$Oldest, all_Quant_table$Oldest)]
  all_Quant.df <- subset(all_Quant.df, Parent %in% post2000_parents)
  dup_oldest <- unique(all_Quant.df$Oldest[duplicated(all_Quant.df$Oldest)])
  for (o in dup_oldest) {
    df <- subset(all_Quant.df, Oldest == o)
    all_Quant.df$Oldest_Qa[all_Quant.df$Oldest == o] <- min(sum(df$Parent_Qa2), mean(df$Oldest_Qa)) ## For aggregate reduction rate, the denominator is the portion of the original parent quantity affected by change authorization (excluding administrative splits)
  }
  all_Quant.df <- all_Quant.df[order(all_Quant.df$Oldest, all_Quant.df$Parent),]
  return(all_Quant.df)
}

## Function to calculate the forfeiture rates by WRIA
WRIA_quants <- function() {
  dimQuant <- subset(ChangeIntentFinal, ChangeIntentFinal$DiminishingChange==TRUE)[,c("WaRecId", "PrimaryNumber", "Parent", "Oldest", "WaRecChangeIntentTypeCode", "ChildQ", "Qa_raw", "Qa", "Parent_Qa_raw", "Parent_Qa", "Parent_Qa2", "Oldest_Qa", "compare_Qa", "WRIA", "County", "Region", "nonconsumptive", "QiOnly")]
  nodimQuant <- subset(ChangeIntentFinal, ChangeIntentFinal$DiminishingChange==FALSE)[,c("WaRecId", "PrimaryNumber", "Parent", "Oldest", "WaRecChangeIntentTypeCode", "ChildQ", "Qa_raw", "Qa", "Parent_Qa_raw", "Parent_Qa", "Parent_Qa2", "Oldest_Qa", "compare_Qa", "WRIA", "County", "Region", "nonconsumptive", "QiOnly")]
  allQuant <- rbind(cbind(DiminishingChange=TRUE, dimQuant), cbind(DiminishingChange=FALSE, nodimQuant))
  allQuant <- unique(allQuant[,-6])
  
  ## Calculate the per-incident reduction rate
  Quant_table <- unique(dimQuant[,c("WaRecId", "Oldest", "Parent", "compare_Qa", "ChildQ", "WRIA", "Oldest_Qa", "Parent_Qa", "QiOnly")])
  Quant_table$ChildQ <- -1 * Quant_table$ChildQ
  dimQuant_by_WRIA <- aggregate(Quant_table[,c("ChildQ")], list(Quant_table$Oldest, Quant_table$Parent, Quant_table$WRIA), sum)
  names(dimQuant_by_WRIA) <- c("Oldest", "Parent", "WRIA", "Relinq_Qa")
  dimQuant_by_WRIA$Parent_Qa <- Quant_table$Parent_Qa[match(dimQuant_by_WRIA$Parent, Quant_table$Parent)]
  #dimQuant_by_WRIA$Oldest_Qa <- Quant_table$Oldest_Qa[match(dimQuant_by_WRIA$Oldest, Quant_table$Oldest)]
  dimQuant_by_WRIA$RelRate <- dimQuant_by_WRIA$Relinq_Qa / dimQuant_by_WRIA$Parent_Qa # per-incident reduction rate (acre-ft/acre-ft or cfs/cfs)
  dimQuant_by_WRIA$nonconsumptive <- dimQuant$nonconsumptive[match(dimQuant_by_WRIA$Parent, dimQuant$Parent)]
  dimQuant_by_WRIA$QiOnly <- dimQuant$QiOnly[match(dimQuant_by_WRIA$Parent, dimQuant$Parent)]
  dimQuant_by_WRIA <- subset(dimQuant_by_WRIA, Parent %in% post2000_parents) ## only interested in post-Jan12000 changes
  dimQuant_by_WRIA$Relinq_Qa[dimQuant_by_WRIA$QiOnly == TRUE] <- 0
  dimQuant_by_WRIA_1 <- aggregate(dimQuant_by_WRIA$Relinq_Qa[dimQuant_by_WRIA$nonconsumptive == FALSE], list(dimQuant_by_WRIA$WRIA[dimQuant_by_WRIA$nonconsumptive == FALSE]), sum) ## only relevant for rights with an annual quantity
  dimQuant_by_WRIA_2 <- aggregate(dimQuant_by_WRIA$RelRate, list(dimQuant_by_WRIA$WRIA), mean) #  
  dimQuant_by_WRIA <- merge(dimQuant_by_WRIA_1, dimQuant_by_WRIA_2, by="Group.1", all=TRUE)
  #dimQuant_by_WRIA[is.na(dimQuant_by_WRIA)] <- 0
  names(dimQuant_by_WRIA) <- c("WRIA", "Relinq_Qa", "RelRate")
  dimQuant_by_WRIA$County <- water_rights$CountyNM[match(dimQuant_by_WRIA$WRIA, water_rights$WRIA_NM)]
  dimQuant_by_WRIA$Region <- reg_lookup$Region[match(dimQuant_by_WRIA$County, reg_lookup$County)]
  dimQuant_by_WRIA$Region <- factor(dimQuant_by_WRIA$Region, levels=rev(c("ERO", "CRO", "SWRO", "NWRO")))
  dimQuant_by_WRIA <- dimQuant_by_WRIA[,c(5,1,2,3)]
  dimQuant_by_WRIA <- dimQuant_by_WRIA[order(dimQuant_by_WRIA$Region, dimQuant_by_WRIA$Relinq_Qa, decreasing=T),]

  ## Calculate the total volume of changed parents, total volume of relinquishment, and aggregate reduction rate (only for non-consumptive ==FALSE)
  all_Quant_table <- unique(allQuant[,c("WaRecId", "Oldest", "Parent", "compare_Qa", "ChildQ", "WRIA", "Oldest_Qa", "Parent_Qa", "Parent_Qa2", "QiOnly")]) 
  all_Quant_table$ChildQ <- -1 * all_Quant_table$ChildQ
  allQuant_by_WRIA <- aggregate(all_Quant_table[,c("ChildQ")], list(all_Quant_table$Oldest, all_Quant_table$Parent, all_Quant_table$WRIA), sum)
  names(allQuant_by_WRIA) <- c("Oldest", "Parent", "WRIA", "Relinq_Qa")
  allQuant_by_WRIA$Parent_Qa2 <- all_Quant_table$Parent_Qa2[match(allQuant_by_WRIA$Parent, all_Quant_table$Parent)]
  allQuant_by_WRIA$Oldest_Qa <- all_Quant_table$Oldest_Qa[match(allQuant_by_WRIA$Oldest, all_Quant_table$Oldest)]
  allQuant_by_WRIA$nonconsumptive <- allQuant$nonconsumptive[match(allQuant_by_WRIA$Parent, allQuant$Parent)]
  allQuant_by_WRIA$QiOnly <- allQuant$QiOnly[match(allQuant_by_WRIA$Parent, allQuant$Parent)]
  allQuant_by_WRIA <- subset(allQuant_by_WRIA, Parent %in% post2000_parents)
  dup_oldest <- unique(allQuant_by_WRIA$Oldest[duplicated(allQuant_by_WRIA$Oldest)])
  allQuant_by_WRIA$Oldest_Qa2 <- allQuant_by_WRIA$Oldest_Qa
  for (o in dup_oldest) {
    df <- subset(allQuant_by_WRIA, Oldest == o)
    allQuant_by_WRIA$Oldest_Qa2[allQuant_by_WRIA$Oldest == o] <- min(sum(df$Parent_Qa2), mean(df$Oldest_Qa)) # For a given genealogy, the smaller of the sum of changed parents and the quantity of the oldest parent
  }
  allQuant_by_WRIA <- subset(allQuant_by_WRIA, nonconsumptive == FALSE)
  allQuant_by_WRIA <- aggregate(allQuant_by_WRIA$Oldest_Qa2, list(allQuant_by_WRIA$Oldest, allQuant_by_WRIA$WRIA), mean)
  allQuant_by_WRIA <- aggregate(allQuant_by_WRIA[,3], list(allQuant_by_WRIA[,2]), sum)
  names(allQuant_by_WRIA) <- c("WRIA", "Oldest_Qa2")
  allQuant_by_WRIA$County <- water_rights$CountyNM[match(allQuant_by_WRIA$WRIA, water_rights$WRIA_NM)]
  allQuant_by_WRIA$Region <- reg_lookup$Region[match(allQuant_by_WRIA$County, reg_lookup$County)]
  allQuant_by_WRIA$Region <- factor(allQuant_by_WRIA$Region, levels=rev(c("ERO", "CRO", "SWRO", "NWRO")))
  allQuant_by_WRIA <- allQuant_by_WRIA[,c(4,1,2)]
  allQuant_by_WRIA$Relinq_Qa <- dimQuant_by_WRIA$Relinq_Qa[match(allQuant_by_WRIA$WRIA, dimQuant_by_WRIA$WRIA)]
  allQuant_by_WRIA$Relinq_Qa[is.na(allQuant_by_WRIA$Relinq_Qa)] <- 0
  allQuant_by_WRIA$RelinqFract <-  round(allQuant_by_WRIA$Relinq_Qa / allQuant_by_WRIA$Oldest_Qa2, 3)
  
  Quants_merged <- merge(dimQuant_by_WRIA, allQuant_by_WRIA, by="WRIA", all=T)[,c(5,1,7,6,4,8)]
  names(Quants_merged) <- c("Region", "WRIA", "Relinq_Qa", "Oldest_Qa", "RelRate", "AggRate") ## Relinq_Qa, Oldest_Qa, and AggRate all for non-consumptive == FALSE
  Quants_merged[,3:4] <- apply(Quants_merged[,3:4], 2, function(x) round(ifelse(is.na(x), 0, x), 0))
  Quants_merged[,5] <- ifelse(is.na(Quants_merged[,5]), 0, Quants_merged[,5])
  Quants_merged <- Quants_merged[order(Quants_merged$Region, Quants_merged$Relinq_Qa, decreasing = T),]
  Quants_merged$WRIA_ID <- water_rights$WRIA_ID[match(Quants_merged$WRIA, water_rights$WRIA_NM)]
  
  ## Calculate the forfeiture frequency
  ChangeWRIA <- unique(ChangeIntentFinal[,c("Parent", "DiminishingChange", "Region", "WRIA")])
  ChangeWRIA <- subset(ChangeWRIA, Parent %in% post2000_parents)
  t1 <- data.frame(table(ChangeWRIA$WRIA[ChangeWRIA[,diminishment_id]==TRUE]))
  t2 <- data.frame(table(unique(ChangeWRIA[,c("Parent", "Region", "WRIA")])$WRIA))
  ChangeWRIA <- merge(t1, t2, "Var1", all=T)
  names(ChangeWRIA) <- c("Subbasin", "Relinquished", "AllChanges")
  ChangeWRIA[,2][is.na(ChangeWRIA[,2])] <- 0
  ChangeWRIA$Freq <- round(ChangeWRIA[,2] / ChangeWRIA$AllChanges, 2)
  ChangeWRIA$County <- water_rights$CountyNM[match(ChangeWRIA$Subbasin, water_rights$WRIA_NM)] 
  ChangeWRIA$Region <- reg_lookup$Region[match(ChangeWRIA$County, reg_lookup$County)]
  ChangeWRIA <- ChangeWRIA[,c(6,1,2,3,4)]
  ChangeWRIA$Region <- factor(ChangeWRIA$Region, levels=rev(c("ERO", "CRO", "SWRO", "NWRO")))
  ChangeWRIA <- ChangeWRIA[order(ChangeWRIA$Region, ChangeWRIA[,2], decreasing = T), ]
  ChangeWRIA$WRIA_ID <- water_rights$WRIA_ID[match(ChangeWRIA$Subbasin, water_rights$WRIA_NM)]
  Quants_merged <- merge(Quants_merged, ChangeWRIA[,c("Relinquished", "AllChanges", "Freq", "WRIA_ID")], by="WRIA_ID")
  Quants_merged <- Quants_merged[order(Quants_merged$Region),]
  return(Quants_merged)
}

PurposeTypeTable_sc <- function(purpose_types, purpose_names) {
  PurposeType <- show_cause
  purpose.ls <- list(length=length(purpose_types), mode="list")
  for (i in 1:length(purpose_types)) {
    neg_ind <- unlist(purpose_types[which(!((1:length(purpose_types)) %in% i))]) # If the other purpose is in neg_ind, exclude the multipurpose right
    if (purpose_types[[i]] == "IR") {
      neg_ind <- paste(neg_ind[grep("DG|DM|DS|CI", neg_ind, invert=TRUE)], collapse="|")
    } else if (purpose_types[[i]] %in% c("PO", "FS")) {
      neg_ind <- paste(neg_ind[grep("DG|DM|DS|IR|MU|CI", neg_ind, invert=TRUE)], collapse="|")
    } else if (purpose_types[[i]] == "MU") {
      neg_ind <- paste(neg_ind[grep("DG|DM|DS|IR", neg_ind, invert=TRUE)], collapse="|")
    } else if (purpose_types[[i]] == "CI") {
      neg_ind <- paste(neg_ind[grep("DG|DM|DS", neg_ind, invert=TRUE)], collapse="|")
    } else {
      neg_ind <- paste(neg_ind, collapse="|")
    }
    select_rows <- intersect(grep(purpose_types[[i]], PurposeType$Purpose), grep(neg_ind, PurposeType$Purpose, invert=TRUE))
    purpose.ls[[i]] <- cbind(Purpose2=purpose_names[i], PurposeType[select_rows,c("docid", "WRIA_ID", "Purpose", "Qa_start_filled", "Relinq_Qa", "RelRate", "nc")])
  }
  purpose.df <- NULL
  for (i in 1:length(purpose_types)) {
    purpose.df <- rbind(purpose.df, purpose.ls[[i]])
  }
  purpose.df$Purpose2 <- factor(purpose.df$Purpose2, levels=purpose_names)
  table.df <- cbind(aggregate(purpose.df$Relinq_Qa, list(purpose.df$Purpose2), length),
                    round(aggregate(purpose.df$Relinq_Qa[purpose.df$nc == "no"], list(purpose.df$Purpose2[purpose.df$nc == "no"]), sum)[,2], 0),
                    round(aggregate(purpose.df$RelRate, list(purpose.df$Purpose2), mean)[,2], 3))
  names(table.df) <- c("Purpose", "Count", "Relinq_Qa", "RelRate")
  return(table.df)
}


PurposeTypeTable <- function(purpose_types, purpose_names) {
  PurposeType <- unique(ChangeIntentFinal[ChangeIntentFinal$Parent %in% post2000_parents, c("Parent", "DiminishingChange", "ParentPurpose", "nonconsumptive", "QiOnly")])
  purpose.ls <- list(length=length(purpose_types), mode="list")
  purpose_quant.ls <- list(length=length(purpose_types), mode="list")
  for (i in 1:length(purpose_types)) {
    neg_ind <- unlist(purpose_types[which(!((1:length(purpose_types)) %in% i))]) # If the other purpose is in neg_ind, exclude the multipurpose right
    if (purpose_types[[i]] == "IR") {
      neg_ind <- paste(neg_ind[grep("DG|DM|DS|CI", neg_ind, invert=TRUE)], collapse="|")
    } else if (purpose_types[[i]] %in% c("PO", "FS")) {
      neg_ind <- paste(neg_ind[grep("DG|DM|DS|IR|MU|CI", neg_ind, invert=TRUE)], collapse="|")
    } else if (purpose_types[[i]] == "MU") {
      neg_ind <- paste(neg_ind[grep("DG|DM|DS|IR", neg_ind, invert=TRUE)], collapse="|")
    } else if (purpose_types[[i]] == "CI") {
      neg_ind <- paste(neg_ind[grep("DG|DM|DS", neg_ind, invert=TRUE)], collapse="|")
    } else {
      neg_ind <- paste(neg_ind, collapse="|")
    }
    select_rows <- intersect(grep(purpose_types[[i]], PurposeType$ParentPurpose), grep(neg_ind, PurposeType$ParentPurpose, invert=TRUE))
    purpose.ls[[i]] <- PurposeType[select_rows,]
    diminished <- unique(purpose.ls[[i]]$Parent[purpose.ls[[i]]$DiminishingChange == TRUE])
    nodiminished <- unique(purpose.ls[[i]]$Parent[purpose.ls[[i]]$DiminishingChange == FALSE])
    rm_parents <- nodiminished[which(nodiminished %in% diminished)]
    if (length(rm_parents) > 0) {
      purpose.ls[[i]] <- purpose.ls[[i]][-which(purpose.ls[[i]]$Parent %in% rm_parents & purpose.ls[[i]]$DiminishingChange == FALSE),]
    }
    purpose.ls[[i]]$Relinq_Qa <- allQuant_Parent$Relinq_Qa[match(purpose.ls[[i]]$Parent, allQuant_Parent$Parent)]
    purpose.ls[[i]]$Parent_Qa <- allQuant_Parent$Parent_Qa[match(purpose.ls[[i]]$Parent, allQuant_Parent$Parent)]
    purpose.ls[[i]]$Parent_Qa2 <- allQuant_Parent$Parent_Qa2[match(purpose.ls[[i]]$Parent, allQuant_Parent$Parent)]
    purpose.ls[[i]]$RelRate <- ifelse(purpose.ls[[i]]$DiminishingChange == TRUE, purpose.ls[[i]]$Relinq_Qa / purpose.ls[[i]]$Parent_Qa, NA)
  }
  PurposeQuant <- NULL
  for (i in 1:length(purpose_types)) {
    PurposeQuant <- rbind(PurposeQuant, cbind(Purpose=purpose_names[i], purpose.ls[[i]]))
  }
  PurposeQuant$Oldest <- allQuant_Parent$Oldest[match(PurposeQuant$Parent, allQuant_Parent$Parent)]
  PurposeQuant$Oldest_Qa <- ChangeIntentFinal$Oldest_Qa[match(PurposeQuant$Parent, ChangeIntentFinal$Parent)]
  PurposeQuant$Oldest_Qa2 <- PurposeQuant$Oldest_Qa
  dup_oldest <- PurposeQuant$Oldest[which(duplicated(PurposeQuant$Oldest))] # same original parent with same purpose of use
  
  for (o in dup_oldest) {
    df <- subset(PurposeQuant, Oldest == o)
    df$col_num <- 0
    df_gen <- genealogy.ls[[as.character(o)]]$gen
    for (p in df$Parent) {
      df$col_num[df$Parent == p] <- ceiling(which(as.matrix(df_gen) == p)[1] / nrow(df_gen)) # to which generation does the parent belong in the genealogy?
    }
    for (k in unique(df$ParentPurpose)) { # loop over categorical variable (in this case whether the change is pre- or post-2000)
      sub_df <- subset(df, ParentPurpose == k)
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
      PurposeQuant$Oldest_Qa2[PurposeQuant$Oldest == o & PurposeQuant$ParentPurpose == k] <- sum_Parent
    }
  }
  row_num <- which(PurposeQuant$nonconsumptive == FALSE)
  PurposeQuant$Relinq_Qa[PurposeQuant$QiOnly == TRUE] <- 0
  PurposeQuant.df <- cbind(aggregate(PurposeQuant$Relinq_Qa[row_num], list(PurposeQuant$Purpose[row_num], PurposeQuant$Oldest[row_num]), sum),
    aggregate(PurposeQuant$Oldest_Qa2[row_num], list(PurposeQuant$Purpose[row_num], PurposeQuant$Oldest[row_num]), mean)[,3]) 
  names(PurposeQuant.df) <- c("PurposeType", "Oldest", "Relinq_Qa", "Oldest_Qa2")
  PurposeQuant.df <- aggregate(PurposeQuant.df[,c("Relinq_Qa", "Oldest_Qa2")], list(PurposeQuant.df$PurposeType), sum)
  names(PurposeQuant.df) <- c("PurposeType", "Relinq_Qa", "Oldest_Qa2")
  PurposeQuant.df$AggRate <- PurposeQuant.df$Relinq_Qa / PurposeQuant.df$Oldest_Qa2
  
  PurposeTable <- data.frame(cbind(aggregate(PurposeQuant$DiminishingChange, list(PurposeQuant$Purpose), length),
    aggregate(PurposeQuant$DiminishingChange, list(PurposeQuant$Purpose), sum)[,2],
    aggregate(PurposeQuant$RelRate, list(PurposeQuant$Purpose), function(x) mean(x, na.rm=T))[,2]))
  names(PurposeTable) <- c("PurposeType", "AllParents", "Relinquished", "RelRate")
  PurposeTable$Freq <- PurposeTable$Relinquished / PurposeTable$AllParents
  PurposeTable <- PurposeTable[,c(1,2,3,5,4)]  
  PurposeTable$AggRate <- PurposeQuant.df$AggRate[match(PurposeTable$PurposeType, PurposeQuant.df$PurposeType)]
  PurposeTable$Relinq_Qa <- round(PurposeQuant.df$Relinq_Qa[match(PurposeTable$PurposeType, PurposeQuant.df$PurposeType)], 0)
  PurposeTable <- PurposeTable[match(purpose_names, PurposeTable$PurposeType),c(1:4,7,5,6)]
  PurposeTable[,-c(1:3,7)] <- apply(PurposeTable[,-c(1:3,7)], 2, function(x) round(x, 3))
  return(list(PurposeTable, PurposeQuant))
}

PersonTypeTable <- function() {
  PersonType <- unique(ChangeIntentFinal[ChangeIntentFinal$Parent %in% post2000_parents, c("Parent", "DiminishingChange", "Parent_PersonClass", "nonconsumptive", "QiOnly")])
  PersonType$Parent_PersonClass[PersonType$Parent_PersonClass == "Irrigation Company"] <- "Irrigation District"
  PersonType$Relinq_Qa <- allQuant_Parent$Relinq_Qa[match(PersonType$Parent, allQuant_Parent$Parent)]
  PersonType$Parent_Qa <- allQuant_Parent$Parent_Qa[match(PersonType$Parent, allQuant_Parent$Parent)]
  PersonType$Parent_Qa2 <- allQuant_Parent$Parent_Qa2[match(PersonType$Parent, allQuant_Parent$Parent)]
  diminished <- unique(PersonType$Parent[PersonType$DiminishingChange == TRUE])
  nodiminished <- unique(PersonType$Parent[PersonType$DiminishingChange == FALSE])
  rm_parents <- nodiminished[which(nodiminished %in% diminished)] 
  PersonType <- PersonType[-which(PersonType$Parent %in% rm_parents & PersonType$DiminishingChange == FALSE),]
  PersonType$RelRate <- ifelse(PersonType$DiminishingChange == TRUE, PersonType$Relinq_Qa / PersonType$Parent_Qa, NA)
  PersonTable <- cbind(aggregate(PersonType$DiminishingChange, list(PersonType$Parent_PersonClass), length),
    aggregate(PersonType$DiminishingChange, list(PersonType$Parent_PersonClass), sum)[,2],
    aggregate(PersonType$RelRate, list(PersonType$Parent_PersonClass), function(x) mean(x, na.rm=T))[,2])
  names(PersonTable) <- c("PersonType", "AllParents", "Relinquished", "RelRate")
  PersonTable$Freq <- PersonTable$Relinquished / PersonTable$AllParents
  PersonTable <- PersonTable[,c(1,2,3,5,4)]
  PersonQuant <- PersonType
  PersonQuant$Oldest <- allQuant_Parent$Oldest[match(PersonQuant$Parent, allQuant_Parent$Parent)]
  PersonQuant$Oldest_Qa <- ChangeIntentFinal$Oldest_Qa[match(PersonQuant$Parent, ChangeIntentFinal$Parent)]
  PersonQuant$Oldest_Qa2 <- PersonQuant$Oldest_Qa

  dup_oldest <- PersonQuant$Oldest[which(duplicated(PersonQuant$Oldest))]
  for (o in dup_oldest) {
    df <- subset(PersonQuant, Oldest == o)
    df$col_num <- 0
    df_gen <- genealogy.ls[[as.character(o)]]$gen
    for (p in df$Parent) {
      df$col_num[df$Parent == p] <- ceiling(which(as.matrix(df_gen) == p)[1] / nrow(df_gen)) # to which generation does the parent belong in the genealogy?
    }
    for (k in unique(df$Parent_PersonClass)) { # loop over categorical variable (in this case whether the change is pre- or post-2000)
      sub_df <- subset(df, Parent_PersonClass == k)
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
      PersonQuant$Oldest_Qa2[PersonQuant$Oldest == o & PersonQuant$Parent_PersonClass == k] <- sum_Parent
    }
  }
  rownum <- which(PersonQuant$nonconsumptive == FALSE)
  PersonQuant$Relinq_Qa[PersonQuant$QiOnly == TRUE] <- 0
  PersonQuant.df <- cbind(aggregate(PersonQuant$Relinq_Qa[rownum], list(PersonQuant$Parent_PersonClass[rownum], PersonQuant$Oldest[rownum]), sum),
    aggregate(PersonQuant$Oldest_Qa2[rownum], list(PersonQuant$Parent_PersonClass[rownum], PersonQuant$Oldest[rownum]), mean)[,3]) 
  names(PersonQuant.df) <- c("OwnerType", "Oldest", "Relinq_Qa", "Oldest_Qa2")
  PersonQuant.df <- aggregate(PersonQuant.df[,c("Relinq_Qa", "Oldest_Qa2")], list(PersonQuant.df$OwnerType), sum)
  names(PersonQuant.df) <- c("OwnerType", "Relinq_Qa", "Oldest_Qa2")
  PersonQuant.df$AggRate <- PersonQuant.df$Relinq_Qa / PersonQuant.df$Oldest_Qa2
  PersonTable$AggRate <- PersonQuant.df$AggRate[match(PersonTable$PersonType, PersonQuant.df$OwnerType)]
  PersonTable$Relinq_Qa <- round(PersonQuant.df$Relinq_Qa[match(PersonTable$PersonType, PersonQuant.df$OwnerType)],0)
  person_order <- c("Individual", "Other Company", "Irrigation District", "Water Company", "Department/Agency", "Other District", "Club/Association", "Municipality")
  PersonTable <- PersonTable[match(person_order, PersonTable$PersonType),c(1:4,7,5,6)]
  PersonTable[,-c(1:3,7)] <- apply(PersonTable[,-c(1:3,7)], 2, function(x) round(x, 3))
  return(list(PersonTable, PersonType))
}

SourceTypeTable <- function() {
  SourceType <- unique(ChangeIntentFinal[ChangeIntentFinal$Parent %in% post2000_parents, c("Parent", "DiminishingChange", "ParentSource", "nonconsumptive", "QiOnly")])
  SourceType$Relinq_Qa <- allQuant_Parent$Relinq_Qa[match(SourceType$Parent, allQuant_Parent$Parent)]
  SourceType$Parent_Qa <- allQuant_Parent$Parent_Qa[match(SourceType$Parent, allQuant_Parent$Parent)]
  SourceType$Parent_Qa2 <- allQuant_Parent$Parent_Qa2[match(SourceType$Parent, allQuant_Parent$Parent)]
  SourceType <- SourceType[-which(SourceType$ParentSource == "reservoir"),]
  diminished <- unique(SourceType$Parent[SourceType$DiminishingChange == TRUE])
  nodiminished <- unique(SourceType$Parent[SourceType$DiminishingChange == FALSE])
  rm_parents <- nodiminished[which(nodiminished %in% diminished)] 
  SourceType <- SourceType[-which(SourceType$Parent %in% rm_parents & SourceType$DiminishingChange == FALSE),]
  SourceType$RelRate <- ifelse(SourceType$DiminishingChange == TRUE, SourceType$Relinq_Qa / SourceType$Parent_Qa, NA)
  SourceTable <- cbind(aggregate(SourceType$DiminishingChange, list(SourceType$ParentSource), length),
                       aggregate(SourceType$DiminishingChange, list(SourceType$ParentSource), sum)[,2],
                       aggregate(SourceType$RelRate, list(SourceType$ParentSource), function(x) mean(x, na.rm=T))[,2])
  names(SourceTable) <- c("SourceType", "AllParents", "Relinquished", "RelRate")
  SourceTable$Freq <- SourceTable$Relinquished / SourceTable$AllParents
  SourceTable <- SourceTable[,c(1,2,3,5,4)]
  
  SourceQuant <- SourceType
  SourceQuant$Oldest <- allQuant_Parent$Oldest[match(SourceQuant$Parent, allQuant_Parent$Parent)]
  SourceQuant$Oldest_Qa <- ChangeIntentFinal$Oldest_Qa[match(SourceQuant$Parent, ChangeIntentFinal$Parent)]
  SourceQuant$Oldest_Qa2 <- SourceQuant$Oldest_Qa
  dup_oldest <- SourceQuant$Oldest[which(duplicated(SourceQuant$Oldest))]
  for (o in dup_oldest) {
    df <- subset(SourceQuant, Oldest == o)
    df$col_num <- 0
    df_gen <- genealogy.ls[[as.character(o)]]$gen
    for (p in df$Parent) {
      df$col_num[df$Parent == p] <- ceiling(which(as.matrix(df_gen) == p)[1] / nrow(df_gen)) # to which generation does the parent belong in the genealogy?
    }
    for (k in unique(df$ParentSource)) { # loop over categorical variable (in this case whether the change is pre- or post-2000)
      sub_df <- subset(df, ParentSource == k)
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
      SourceQuant$Oldest_Qa2[SourceQuant$Oldest == o & SourceQuant$ParentSource == k] <- sum_Parent
    }
  }
  rownum <- which(SourceQuant$nonconsumptive == FALSE)
  SourceQuant$Relinq_Qa[SourceQuant$QiOnly == TRUE] <- 0
  SourceQuant.df <- cbind(aggregate(SourceQuant$Relinq_Qa[rownum], list(SourceQuant$ParentSource[rownum], SourceQuant$Oldest[rownum]), sum),
                          aggregate(SourceQuant$Oldest_Qa2[rownum], list(SourceQuant$ParentSource[rownum], SourceQuant$Oldest[rownum]), mean)[,3]) 
  names(SourceQuant.df) <- c("SourceType", "Oldest", "Relinq_Qa", "Oldest_Qa2")
  SourceQuant.df <- aggregate(SourceQuant.df[,c("Relinq_Qa", "Oldest_Qa2")], list(SourceQuant.df$SourceType), function(x) sum(x, na.rm=T))
  names(SourceQuant.df) <- c("SourceType", "Relinq_Qa", "Oldest_Qa2")
  SourceQuant.df$AggRate <- SourceQuant.df$Relinq_Qa / SourceQuant.df$Oldest_Qa2
  SourceTable$AggRate <- SourceQuant.df$AggRate[match(SourceTable$SourceType, SourceQuant.df$SourceType)]
  SourceTable$Relinq_Qa <- round(SourceQuant.df$Relinq_Qa[match(SourceTable$SourceType, SourceQuant.df$SourceType)], 0)
  source_order <- c("surfaceWater", "groundwater")
  SourceTable <- SourceTable[match(source_order, SourceTable$SourceType),c(1:4,7,5,6)]
  SourceTable[,-c(1:3,7)] <- apply(SourceTable[,-c(1:3,7)], 2, function(x) round(x, 3))
  return(list(SourceTable, SourceType))
}







