require(dplyr)
require(tidyr)

allocate_resources <- function(money, pot) {
  # Sort individuals by their total amount of money, then by their initial amount of money
  allocation_order <- order(money)
  
  # Calculate amount of money to next highest value
  money_diff <- money[allocation_order[2:length(allocation_order)]] - money[allocation_order[1:(length(allocation_order) - 1)]]
  
  # Remove excess money, starting with individuals who have the largest total amount of money
  remaining_pot <- pot
  allocated_money <- rep(0,length(money))
  
  for (i in 1:length(money)) {
    if (remaining_pot > 0) {
      # Calculate the maximum amount of money that can be allocated to the individual without violating the constraint
      max_additional_allocation <- ifelse(i < length(money),
                                          min(remaining_pot, i*money_diff[i]),
                                          remaining_pot)
      if (max_additional_allocation > 0) {
        allocated_money[allocation_order[1:i]] <- allocated_money[allocation_order[1:i]] + max_additional_allocation / i
        remaining_pot <- remaining_pot - max_additional_allocation
      }
    }
  }
  
  return(allocated_money)
}

processFundingWithTracts <- function(tractsOnly, tol = 10^-3){
  # clean up funding amounts so there's no overlap between buffer, low income, and low income household funding
  # Much of the funding is double counted (same number appears in multiple "Amount" columns). 
  # We delineate the funding based on the following assumptions:
  # 
  # Projects assigned to a specific census tract:
  # (1) Funding can come from sources based on their designation (DAC, Buffer, Low-income, Low-income households). Designations are in "ab1550censustracts_ces3_2021.xlsx"
  #     Not all categories are mutually exclusive, as Low-income can have funding from the buffer funding if it's within 1/2 mile from a DAC.
  #     Only monies from allowed funding sources will be counted. All money not from allowed funding sources is assumed to be from GGRF
  # (2) Many projects will double count funding (same amount appears in multiple columns). 
  #     If the sum of values across DAC, Buffer, Low-income, and Low-income households is less than the total funds, we will assume the values are not double counted
  #     If the sum of values across the 4 named funding sources is 2x the total, split across funding sources evenly (very likely double counted)
  #     If the non-zero values across columns are not equal, we disperse the smallest sum first, then give the remainder to the other column
  
  #only select tracts that began between 2017-2021
  # TODO: filter by CES 3.0
  
  print("PROCESSING PROJETS WITH ASSIGNED CENSUS TRACT")
  
  
  print(paste0("Filtering out ", tractsOnly %>% filter(`Total Program GGRFFunding` < tol) %>% nrow(), " projects that have negative funding"))
  tractsOnly <- tractsOnly %>% filter(`Total Program GGRFFunding` > 0)
  
  print(paste0("Projects assigned to a Census Tract: ", nrow(tractsOnly)))
  
  tractsOnly <- tractsOnly %>%
    mutate(funding.dac = ifelse(designation == "dac", DAC3Amount, 0),
           funding.buffer = ifelse(buffer == 1, `Buffer Amount`, 0),
           funding.li = ifelse(designation == "low income", `Low Income Amount`, 0),
           funding.lih =ifelse(designation == "low income household", `Low Income Houshold Amount`, 0),
           funding.priority = funding.dac + funding.buffer + funding.li + funding.lih) %>%
    mutate_at(.vars = vars(starts_with("funding", ignore.case = F)), ~replace_na(.,0))
  
  print(paste0("Projects assigned to a Census Tract with greater priority funding than total: ", nrow(tractsOnly %>% filter(funding.priority > `Total Program GGRFFunding`))))
  print("If total funding is less than priority funding")
  print("    If any funding column is greater than total, cap to total (assume input error)")
  print("    Split between funding sources equally if any non-zerovalues in the columns are equal")
  print("    If non-zero values aren't equal, disperse smaller denominations to funding sources, and disperse remainer to other funding sources")
  
  tractsOnly <- tractsOnly %>%
    mutate(notes = case_when(funding.dac > `Total Program GGRFFunding` ~ "capped dac",
                             funding.buffer > `Total Program GGRFFunding` ~ "capped buffer",
                             funding.li > `Total Program GGRFFunding` ~ "capped li",
                             funding.lih > `Total Program GGRFFunding` ~ "capped lih",
                             TRUE ~ ""),
           funding.dac = pmin(funding.dac, `Total Program GGRFFunding`),
           funding.buffer = pmin(funding.buffer, `Total Program GGRFFunding`),
           funding.li = pmin(funding.li, `Total Program GGRFFunding`),
           funding.lih = pmin(funding.lih, `Total Program GGRFFunding`),
           funding.priority = funding.dac + funding.buffer + funding.li + funding.lih)
  
  print(table(tractsOnly$notes))
  
  tractsOnly <- tractsOnly %>%
    mutate(funding.ratio = funding.priority / `Total Program GGRFFunding`,
           funding.buffer = case_when(abs(funding.ratio - 2) < tol ~ funding.buffer/2, # factor of 2
                                      abs(funding.ratio - 3) < tol ~ funding.buffer/3, # factor of 3
                                      funding.ratio > 1 & funding.li > 0 & funding.li < funding.buffer ~ pmin(`Total Program GGRFFunding` - funding.li, funding.buffer), # prioritize low income dispersal first
                                      funding.ratio > 1 & funding.buffer > 0 & funding.li > funding.buffer ~ funding.buffer, #prioritize buffer first
                                      TRUE ~ funding.buffer
           ),
           funding.li = case_when(abs(funding.ratio - 2) < tol ~ funding.li/2, # factor of 2
                                  abs(funding.ratio - 3) < tol ~ funding.li/3, # factor of 3
                                  funding.ratio > 1 & funding.li > 0 & funding.li < funding.buffer ~ funding.li, # prioritize low income dispersal first
                                  funding.ratio > 1 & funding.buffer > 0 & funding.li > funding.buffer ~ pmin(`Total Program GGRFFunding` - funding.buffer, funding.li), #prioritize buffer first
                                  TRUE ~ funding.li
           ),
           # only need to split evenly for low income housing
           funding.lih = case_when(abs(funding.ratio - 2) < tol ~ funding.lih/2, # factor of 2
                                   abs(funding.ratio - 3) < tol ~ funding.lih/3, # factor of 3
                                   TRUE ~ funding.lih
           ),
           funding.priority = funding.dac + funding.buffer + funding.li + funding.lih,
           funding.total = `Total Program GGRFFunding`,
           funding.other = funding.total - funding.priority
    )
  print(paste0("Projects assigned to a Census Tract with greater priority funding than total AFTER splitting funding: ", nrow(tractsOnly %>% filter(funding.priority > `Total Program GGRFFunding`))))
  
  print("FUNDING SUMMARY: projects with assigned census tracts:")
  ct.counts <- tractsOnly %>% summarise_at(.vars = vars(starts_with("funding", ignore.case = F)), .funs = ~sum(.))
  ct.counts <- ct.counts[names(ct.counts) != "funding.ratio"]
  # print(ct.counts)
  print(ct.counts / ct.counts$funding.total)
  
  return(tractsOnly %>% select(-c(funding.total, funding.ratio), -starts_with("n."), -ends_with("Amount"), -matches("District")))
}

processFundingWithoutTracts <- function(noTracts, cesAD, tol = 10^-3, parallelize = FALSE){
  print(paste0("Projects without assigned Census Tracts: ", nrow(noTracts)))
  
  print(paste0("Filtering out ", noTracts %>% filter(`Total Program GGRFFunding` < tol) %>% nrow(), " projects that have negative funding"))
  noTracts <- noTracts %>% filter(`Total Program GGRFFunding` > 0)
  
  print("    If any funding column is greater than total, cap to total (assume input error)")
  
  noTracts <- noTracts %>%
    rename(funding.dac.total = DAC3Amount,
           funding.buffer.total = `Buffer Amount`,
           funding.li.total = `Low Income Amount`,
           funding.lih.total = `Low Income Houshold Amount`) %>%
    mutate_at(.vars = vars(starts_with("funding", ignore.case = F)), ~replace_na(.,0)) %>%
    mutate(notes = case_when(funding.dac.total > `Total Program GGRFFunding` ~ "capped dac",
                             funding.buffer.total > `Total Program GGRFFunding` ~ "capped buffer",
                             funding.li.total > `Total Program GGRFFunding` ~ "capped li",
                             funding.lih.total > `Total Program GGRFFunding` ~ "capped lih",
                             TRUE ~ ""),
           funding.dac.total = pmin(funding.dac.total, `Total Program GGRFFunding`),
           funding.buffer.total = pmin(funding.buffer.total, `Total Program GGRFFunding`),
           funding.li.total = pmin(funding.li.total, `Total Program GGRFFunding`),
           funding.lih.total = pmin(funding.lih.total, `Total Program GGRFFunding`),
           funding.priority.total = funding.dac.total + funding.buffer.total + funding.li.total + funding.lih.total)
  
  print(table(noTracts$notes))
  
  noTracts <- noTracts %>%
    mutate(funding.ratio = funding.priority.total / `Total Program GGRFFunding`) %>%
    filter(!(funding.ratio - 1 >= tol)) %>%
    mutate(funding.total = `Total Program GGRFFunding`,
           funding.other.total = funding.total - funding.priority.total
    ) 
  
  print(paste0("Only ", noTracts %>% filter(funding.ratio - 1 >= tol) %>% nrow(), "/", nrow(noTracts), " of district-level projects that double count (ratio > 1)."))
  print("    We choose to remove these tracts because the way the funding is distributed is unclear (e.g., how much should each designation group get)")
  # We include a tolerance because funding is rounded to nearest dollar so this accounts for small dollar differences between the sum of funding sources and the total
  
  print("FUNDING SUMMARY: projects without assigned census tracts:")
  ct.counts <- noTracts %>% summarise_at(.vars = vars(starts_with("funding", ignore.case = F)), .funs = ~sum(.))
  ct.counts <- ct.counts[names(ct.counts) != "funding.ratio"]
  print(ct.counts / ct.counts$funding.total)
  
  # Split by Assembly District then find census tracts in AD
  noTracts <- noTracts %>% 
    mutate(rowNumber = row_number())
  
  print("Splitting into assembly districts then into census tracts")
  
  print("For projects with named DAC tracts and contain DAC funding, assign funds to named DAC tracts")
  print("For other projects, split among Assembly District")
  
  specifiesDAC <- noTracts %>% 
    filter(!is.na(`Disadvantaged Community Census Tracts`), funding.dac.total > 0) %>%
    mutate(`Census Tract` = str_extract_all(`Disadvantaged Community Census Tracts`,"\\d+")) %>%
    unnest(`Census Tract`) %>%
    mutate(`Census Tract` = as.character(`Census Tract`)) %>%
    select(-`Assembly Districts(s)`) %>%
    left_join(cesAD)
  
  # if funding is entirely DAC, only give to named DAC tracts
  specifiesDAC.all <- specifiesDAC %>%
    filter(abs(`Total Program GGRFFunding` - funding.dac.total) < tol, designation == "dac") %>%
    mutate(fund.dac = 1)
  
  # if funding not entirely DAC, only give to named DAC tracts, but split evenly across all other tracts
  specifiesDAC.some <- specifiesDAC %>%
    filter(abs(`Total Program GGRFFunding` - funding.dac.total) > tol, designation == "dac") %>%
    mutate(fund.dac = 1)
  
  noTractsAD <- noTracts %>%
    anti_join(specifiesDAC.all %>% select(rowNumber) %>% distinct()) %>%
    # filter(!(!is.na(`Disadvantaged Community Census Tracts`) & funding.dac.total > 0)) #%>%
    select(-`Census Tract`) %>%
    mutate(`Assembly Districts(s)` = 
             strsplit(as.character(`Assembly Districts(s)`),", ")) %>%
    unnest(`Assembly Districts(s)`) %>% 
    mutate(`Assembly Districts(s)` = as.numeric(`Assembly Districts(s)`)) %>%
    full_join(cesAD, by=c("Assembly Districts(s)"="district")) %>%
    left_join(specifiesDAC.some %>% select(rowNumber, `Census Tract`, fund.dac)) %>%
    group_by(rowNumber) %>%
    replace_na(list(fund.dac = 0)) %>%
    mutate(fund.dac.any = max(fund.dac)) %>%
    mutate(fund.dac = case_when(fund.dac.any < 0.5 & designation == "dac" ~ 1, 
                                fund.dac.any < 0.5 & designation != "dac" ~ 0, 
                                designation == "dac" & fund.dac > 0.5 ~ 1,
                                TRUE ~ 0)) %>%
    select(-fund.dac.any)
  
  noTractsAD <- bind_rows(noTractsAD, specifiesDAC.all)
  
  print(paste0("Number of projects: ", nrow(noTracts)))
  print(paste0("Number of project-census tracts: ", nrow(noTractsAD)))
  
  noDistricts <- noTractsAD %>%
    filter(is.na(`Assembly Districts(s)`), is.na(`Senate District(s)`))
  if(nrow(noDistricts) > 0){
    print("For projects without assigned assembly districts, disperse evenly across all census tracts")
    
    cols.to.remove <- intersect(colnames(cesAD), colnames(noTractsAD))
    
    noDistricts <- noDistricts %>%
      select(-!!cols.to.remove)
    
    x <- expand.grid(1:nrow(noDistricts),1:nrow(cesAD))
    
    noDistricts <- dplyr::bind_cols(noDistricts[x[,1],], cesAD[x[,2],])
    # noDistricts <- as.data.frame(noDistricts)
    noTractsAD <- dplyr::bind_rows(noTractsAD %>% filter(!(is.na(`Assembly Districts(s)` & is.na(`Senate District(s)`)))),
                                   noDistricts) 
  }
  
  print("Filtering out Census Tract (6037930401) which was erroneously assigned in the 2010 Census")
  print(paste0("Number of project-tracts removed: ", noTractsAD %>% filter(`Census Tract` == 6037930401) %>% nrow()))
  
  noTractsAD <- noTractsAD %>%
    filter(`Census Tract` != 6037930401)
  
  print("If there are no tracts within the assembly district that meet earmarked funding criteria, disperse funding evenly across tracts")
  noTractsAD <- noTractsAD %>%
    group_by(rowNumber, `Total Program GGRFFunding`, funding.priority.total) %>%
    mutate(n.dac = sum(designation == "dac" & fund.dac > 0.5),
           n.li = sum(designation == "low income"),
           n.lih = sum(designation == "low income household"),
           n.buffer = sum(buffer),
           n.none = sum(designation == "none") + sum(designation == "dac" & fund.dac < 0.5),
           n.total = n(),
           funding.left = `Total Program GGRFFunding` - funding.priority.total
           ) %>%
    ungroup() %>%
    mutate(funding.dac = case_when(designation == "dac" & fund.dac > 0.5 ~ funding.dac.total / n.dac, 
                                   abs(n.dac) < tol & funding.dac.total > tol ~ funding.dac.total / n.total,
                                   TRUE ~ 0),
           funding.li = case_when(designation == "low income" ~ funding.li.total / n.li, 
                                  abs(n.li) < tol & funding.li.total > tol ~ funding.li.total / n.total,
                                  TRUE ~ 0),
           funding.lih = case_when(designation == "low income household" ~ funding.lih.total / n.lih, 
                                   abs(n.lih) < tol & funding.lih.total > tol ~ funding.lih.total / n.total,
                                   TRUE ~ 0),
           funding.buffer = case_when(buffer == 1 ~ funding.buffer.total / n.buffer, 
                                      abs(n.buffer) < tol & funding.buffer.total > tol ~ funding.buffer.total / n.total,
                                      TRUE ~ 0),
           funding.priority = funding.dac + funding.li + funding.lih + funding.buffer)
  
  noTractsAD <- noTractsAD %>% arrange(rowNumber) %>% mutate(rowNumber = factor(rowNumber))
  
  print("Allocating other funding as evenly as possible")
  print("Specifically, we try to minimize difference between the range of funding for census tracts of a particular project")
  print("This function takes a while")
  tract.df <- noTractsAD %>%
    select(rowNumber, funding.left, funding.priority)
  calc.allocation <- function(x) {
    df = subset(tract.df, rowNumber == x)
    funding.left = df[["funding.left"]][1]
    funding.priority = df[["funding.priority"]]
    funding.other = allocate_resources(funding.priority, funding.left)
    return(funding.other)
  }
  require(pbapply)
  
  if(parallelize == F) {
    print("parallelize is set to FALSE. To speed up, specify the number of clusters to make.")
    
    funding.other <- pblapply(unique(noTractsAD$rowNumber), 
                              FUN=calc.allocation)
  } else{
    require(parallel)
    cl <- parallel::makeCluster(as.integer(parallelize), setup_strategy = "sequential")
    clusterExport(cl, c("allocate_resources", "tract.df"), envir = environment())
    
    environment(calc.allocation) <- .GlobalEnv
    
    funding.other <- pblapply(unique(noTractsAD$rowNumber), 
                              cl = cl, 
                              FUN=calc.allocation)
    
    stopCluster(cl)
  }
  
  funding.other <- bind_rows(lapply(funding.other, as.data.frame), .id = "rowNumber") %>%
    rename(funding.other = `X[[i]]`)
  
  noTractsAD$funding.other <- funding.other$funding.other
  
  return(noTractsAD %>% select(-ends_with(".total"), -starts_with("n."), -matches("District"), -funding.ratio, -funding.left, -fund.dac, -`Disadvantaged Community Census Tracts`))
}

createFundingDF <- function(cciDF,cesAD, writeName, parallelize = FALSE) {
  tractsOnly <- cciDF %>% 
    filter(!is.na(`Census Tract`)) %>% 
    mutate(`Census Tract` = as.character(`Census Tract`)) %>%
    left_join(cesAD)
  tractsOnly <- processFundingWithTracts(tractsOnly)
  
  noTracts <- processFundingWithoutTracts(cciDF %>% filter(is.na(`Census Tract`)), cesAD, parallelize = parallelize)
  
  print("Combining Low-income and Low-income household funding")
  
  allTracts <- bind_rows(tractsOnly, noTracts) %>%
    group_by(`Census Tract`) %>%
    summarize(funding.dac = sum(funding.dac, na.rm = T),
              funding.buffer = sum(funding.buffer, na.rm = T),
              funding.li = sum(funding.li, na.rm = T) + sum(funding.lih, na.rm = T),
              funding.other = sum(funding.other, na.rm = T)) %>%
    left_join(cesAD)
  
  tractsOnlySum <- tractsOnly %>%
    group_by(`Census Tract`) %>%
    summarize(funding.dac = sum(funding.dac, na.rm = T),
              funding.buffer = sum(funding.buffer, na.rm = T),
              funding.li = sum(funding.li, na.rm = T) + sum(funding.lih, na.rm = T),
              funding.other = sum(funding.other, na.rm = T)) %>%
    left_join(cesAD)
  
  write_csv(allTracts,here::here("data","processed",paste0(writeName,".csv")))
  write_csv(tractsOnlySum,here::here("data","processed",paste0(writeName,"_noAD.csv")))
}

