#Function to expand patterns dataset by blockgroup.
#param patterns_data: Safegraph patterns dataset.
#pre It should have a column visitor_home_cbgs with a JSON dictionary of blockgroup GEOIDs and number of visitors.
#returns patterns_data with breakdown of visitors by blockgroup or unrecorded.
expandOrigins <- function(patterns_data){
  
  expanded_patterns <-
    1:nrow(patterns_data) %>% 
    map_dfr(function(row){
      
      # if(row%%100 == 0) print(row)
      
      if(patterns_data$visitor_home_cbgs[row] == "{}") {
        
        data_row <- 
          data.frame(
            safegraph_place_id = patterns_data$safegraph_place_id[row],
            origin_census_block_group = NA, #origin block group is not available.
            origin_raw_visitor_counts_high = patterns_data$raw_visitor_counts[row],
            origin_raw_visitor_counts_low = patterns_data$raw_visitor_counts[row]
          )
        
      } 
      # Else, need to unpack the JSON dictionary.
      else {
        #Unpack, unlist, rename.
        json <- 
          patterns_data$visitor_home_cbgs[row] %>% 
          fromJSON() %>% 
          unlist() %>% 
          as.data.frame() %>% 
          rownames_to_column() %>% 
          rename(
            origin_census_block_group = "rowname",
            origin_raw_visitor_counts_high = "."
          ) %>% 
          mutate( # Dealing with the 2-4 visitor issue
            origin_raw_visitor_counts_low =
              ifelse(
                origin_raw_visitor_counts_high == 4,
                2,
                origin_raw_visitor_counts_high
              )
          )
        
        #Compute the unrecorded raw visitors by subtracting blockgroup-mapped origin_raw_visitor_counts from raw_visitor_counts. Special cases need to be considered because of the 2-4 visitor issue
        if(patterns_data$raw_visitor_counts[row] > sum(json$origin_raw_visitor_counts_high)){
          
          unrecorded_raw_visitor_counts_high <- 
            patterns_data$raw_visitor_counts[row] - sum(json$origin_raw_visitor_counts_high)
          
          unrecorded_raw_visitor_counts_low <-
            patterns_data$raw_visitor_counts[row] - sum(json$origin_raw_visitor_counts_low)
          
        } else {
          
          #In this rare case, there were enough origins overcounted as having 4 raw visitors that the total count for recorded origins exceeded that actual raw_visitor_count. We downscale all recorded origin visitor counts so that the sum of recorded origin visitor counts equals actual raw_visitor_count exactly.
          json$origin_raw_visitor_counts_high <-
            json$origin_raw_visitor_counts_high/sum(json$origin_raw_visitor_counts_high)*patterns_data$raw_visitor_counts[row]
          
          unrecorded_raw_visitor_counts_high <- 0
          
          unrecorded_raw_visitor_counts_low <-
            max((patterns_data$raw_visitor_counts[row] - sum(json$origin_raw_visitor_counts_low)),0)
          
        }
        
        #Add one more row which contains unrecorded raw_visitor_counts.
        data_row <- 
          json %>% 
          rbind(
            data.frame(
              origin_census_block_group = NA,
              origin_raw_visitor_counts_high = unrecorded_raw_visitor_counts_high,
              origin_raw_visitor_counts_low = unrecorded_raw_visitor_counts_low
            )
          ) %>% 
          mutate(
            safegraph_place_id = patterns_data$safegraph_place_id[row]
          )
        
      }
      
    })
  
  #Join the rest of the patterns data using safegraph place id.
  expanded_patterns <-
    expanded_patterns %>% left_join(patterns_data, by = 'safegraph_place_id')
  
  return(expanded_patterns)
}


#Function to do block-groupwise normalization.
#param patterns: Safegraph patterns dataset.
#pre patterns must have the columns: 'raw_visits_counts'.
#param home_summary: Safegraph home_panel_summary dataset.
#pre home_summary must have columns 'census_block_group' and 'number_devices_residing'.
#param bay_blockgroups: list of blockgroup GEOIDs in the bay area for filtering of the Safegraph datasets.
#param ca_pop_blockgroup: dataframe. Censis population count for each blockgroup in CA.
#returns patterns dataset with column visit_counts that multiples raw visits based on ratio of Bay area population to safegraph population.
normBG <- function(patterns,home_summary,bay_blockgroups = norm_bay_blockgroups,ca_pop_blockgroup = norm_ca_pop_blockgroup)
{
  
  #Expand and categorize visitors by origin_census_block_group.
  #Also join population and home summary data.
  patterns <- 
    expandOrigins(patterns) %>% 
    left_join(
      home_summary %>% 
        dplyr::select(
          origin_census_block_group = census_block_group,
          number_devices_residing
        ), 
      by = 'origin_census_block_group'
    ) %>%
    left_join(ca_pop_blockgroup, by = 'origin_census_block_group')
  
  #Potential cleaning of lack of population data if not present within California.
  # lack_of_pop <- which(is.na(patterns$pop) & !is.na(patterns$origin_census_block_group))
  # for (i in lack_of_pop) {
  #   geoid = patterns[i,'origin_census_block_group']
  #   state <- substring(geoid,1,2)
  #   county <- substring(geoid,3,5)
  #   tract <- substring(geoid,6,11)
  #   bg <- substring(geoid,12,12)
  #   #Get population data for non-California blockgroups.
  #   patterns[i,'pop'] = getCensus(
  #     name = "acs/acs5",
  #     vintage = acs_year,
  #     region = paste0("block group:", bg), 
  #     regionin = paste0("state:",state,"+county:",county,"+tract:",tract),
  #     vars = "B01003_001E"
  #   ) %>% pull('B01003_001E')
  # }
  
  #To estimate number of visitors from unrecorded block groups, use the visitors-to-devices ratio generated by the aggregate recorded block groups.
  recorded_pop <-
    ca_pop_blockgroup %>% 
    filter(
      origin_census_block_group %in% patterns$origin_census_block_group
    ) %>% 
    pull(pop) %>% 
    sum()
  
  recorded_sg_pop <-
    home_summary %>% 
    filter(census_block_group %in% patterns$origin_census_block_group) %>% 
    pull(number_devices_residing) %>% 
    sum()
  
  #Compute ratio
  recorded_ratio <- recorded_pop/recorded_sg_pop
  
  patterns <-
    patterns %>% 
    mutate(
      origin_visitor_counts_high =
        ifelse(
          !is.na(origin_census_block_group),
          origin_raw_visitor_counts_high*pop/number_devices_residing,
          origin_raw_visitor_counts_high*recorded_ratio
        ),
      origin_visitor_counts_low =
        ifelse(
          !is.na(origin_census_block_group),
          origin_raw_visitor_counts_low*pop/number_devices_residing,
          origin_raw_visitor_counts_low*recorded_ratio
        ),
      origin_visitor_counts = (origin_visitor_counts_high + origin_visitor_counts_low)/2
    )
  
  #Finally get visit_counts by multiplying by ratio of raw visit:visitor ratio.
  patterns <- 
    patterns %>% 
    mutate(
      visit_counts_high = origin_visitor_counts_high*raw_visit_counts/raw_visitor_counts,
      visit_counts_low = origin_visitor_counts_low*raw_visit_counts/raw_visitor_counts,
      visit_counts = (visit_counts_high + visit_counts_low)/2,
      ratio = visit_counts/raw_visit_counts
    )
  
  return(patterns)
}
