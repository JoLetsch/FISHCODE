;;; author: Jonas Letschert
;;; email: Jonas_letschert@web.de
;;; version 1.0


;;; loading packages
extensions [time csv gis ]

;;; sourcing initialisation and functions
__includes[

  "init.nls"                       ; global and state variables, breeds, setup function
  "percieve_and_SocNet.nls"        ; functions related to agent perceptions and social network
  "consumat.nls"                   ; functions related to the consumat approach including satisfactions and uncertainties
  "behavioral_options.nls"         ; function related to perceiving, predicting, and choosing behavioral options
  "fishing.nls"                    ; functions related to modelling spatial movement, landings, and profits
  "export.nls"                     ; function to export model results
  "generic.nls"                    ; generic functions used throughout the whole code

]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;     GO PROCEDURE     ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  ;;; progress ticks and time

  tick
  set day time:plus day 1 "days"
  set day_ch lput time:show day "yyyy-MM-dd" day_ch
  set day_ongoing time:plus day_ongoing 1 "days"
  if( (time:show day_ongoing "MM-dd") = "29-02" )[ set day_ongoing time:plus day_ongoing 1 "days" ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;  processes at the beginning of a YEAR  ;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;  update yearly data sets
  ;; note: of several data sets get current and next year, because fishing trips may take place in both years
  if( time:get "dayofyear" day = 1  )[

    ; update year variable
    set year substring last day_ch 0 4

    ; make a list of all weeks
    let first_week read-from-string time:show (word year "-01-01") "ww"
    let last_week read-from-string time:show (word year "-12-31") "ww"
    if( last_week = 0 or last_week = 1 )[ set last_week 52 ]
    set year_weeks n-values last_week [ i -> i ]
    set year_weeks lput last_week year_weeks
    if( first_week != 0 )[ set year_weeks but-first year_weeks ]
    set year_weeks map [ a -> ifelse-value( length (word a) = 1 )[ (word year "-0" a) ][ (word year "-" a) ] ] year_weeks

    ; setup yearly overall quotas
    set temp first filter [ i -> (word first i) = year ] quotas
    set quota_sol round item 1 temp
    set quota_ple round item 2 temp
    set quota_tur_bll round item 3 temp
    set quota_nep round item 4 temp
    set quota_cod round (item 5 temp * .2)   ; just use 20% of national quota, because about 80% is used by high-seas fleet.

    ; setup global catches (initially 0)
    set global_sol 0
    set global_ple 0
    set global_tur_bll 0
    set global_nep 0
    set global_cod 0
    set global_csh 0
    set global_other 0

    ; update per vessel: quotas & common used FishGros
    ask vessels [

      ; yearly plaice quota
      set quota_year_ple ifelse-value( member? "PLE" fish_licence )
      [ (quota_ple * 0.9) / (count vessels with [member? "PLE" fish_licence]) ]
      [ 1 ]

      ; quartely sole quota
      set quota_quarter_sol ifelse-value( member? "SOL" fish_licence )
      [ (quota_sol * 0.9) / (count vessels with [member? "SOL" fish_licence]) / 4 ]
      [ 1 ]

      ; yearly nephrops quota
      set quota_year_nep ifelse-value( member? "NEP" fish_licence )
      [ (quota_nep * 0.9) / (count vessels with [member? "NEP" fish_licence]) ]
      [ 1 ]

      ; yearly cod quota
      set quota_year_cod ifelse-value( member? "COD" fish_licence )
      [ (quota_cod * 0.9) / (count vessels with [member? "COD" fish_licence]) ]
      [ 1 ]

    ]

    ; spatially averaged (per fishing ground) oceanographic parameters
    let w_pos position "date" all_swh_colnames
    set swh_year_OTB-NEP&PLE filter [ a -> substring (item w_pos a) 0 4 = year ] swh_OTB-NEP&PLE
    set swh_year_OTB-PLE filter [ a -> substring (item w_pos a) 0 4 = year ] swh_OTB-PLE
    set swh_year_PUL-CSH filter [ a -> substring (item w_pos a) 0 4 = year ] swh_PUL-CSH
    set swh_year_TBB-CSH filter [ a -> substring (item w_pos a) 0 4 = year ] swh_TBB-CSH
    set swh_year_PUL-PLE&SOL filter [ a -> substring (item w_pos a) 0 4 = year ] swh_PUL-PLE&SOL
    set swh_year_TBB-PLE&SOL filter [ a -> substring (item w_pos a) 0 4 = year ] swh_TBB-PLE&SOL
    set swh_year_PUL-SOL&PLE filter [ a -> substring (item w_pos a) 0 4 = year ] swh_PUL-SOL&PLE
    set swh_year_TBB-SOL&PLE filter [ a -> substring (item w_pos a) 0 4 = year ] swh_TBB-SOL&PLE

    ;;; update spatially resolved (per patch) oceanographic parameters

    ;; bottom temperature
    let par_list []
    foreach year_weeks [ sel_w ->
      file-open (word path_input_folder "env_dat/env_NWS_bottomT_" sel_w ".asc")
      repeat 6 [let header file-read-line] ; skip header
      set temp []
      while [file-at-end? = false][ set temp lput (precision file-read 3) temp ]
      file-close
      set par_list fput temp par_list
    ]
    set par_list map [ a -> map [ b -> item a item b par_list ]  n-values length par_list [i -> i]   ]  n-values length first par_list [i -> i]
    (foreach sort patches par_list [ [a b] -> ask a [ set NWS_bottomT_allWeeks b ] ] )

    ;; bottom salinity
    set par_list []
    foreach year_weeks [ sel_w ->
      file-open (word path_input_folder "env_dat/env_NWS_SAL_" sel_w ".asc")
      repeat 6 [let header file-read-line] ; skip header
      set temp []
      while [file-at-end? = false][ set temp lput (precision file-read 3) temp ]
      file-close
      set par_list fput temp par_list
    ]
    set par_list map [ a -> map [ b -> item a item b par_list ]  n-values length par_list [i -> i]   ]  n-values length first par_list [i -> i]
    (foreach sort patches par_list [ [a b] -> ask a [ set NWS_SAL_allWeeks b ] ] )

    ;; mixed layer depth
    set par_list []
    foreach year_weeks [ sel_w ->
      file-open (word path_input_folder "env_dat/env_NWS_MLD_" sel_w ".asc")
      repeat 6 [let header file-read-line] ; skip header
      set temp []
      while [file-at-end? = false][ set temp lput (precision file-read 3) temp ]
      file-close
      set par_list fput temp par_list
    ]
    set par_list map [ a -> map [ b -> item a item b par_list ]  n-values length par_list [i -> i]   ]  n-values length first par_list [i -> i]
    (foreach sort patches par_list [ [a b] -> ask a [ set NWS_MLD_allWeeks b ] ] )

    ;; international fishing vessels
    set par_list []
    foreach year_weeks [ sel_w ->
      file-open (word path_input_folder "weekly_int_ves_distr/ves_distr_" (substring sel_w 0 2) ".asc")
      repeat 6 [let header file-read-line] ; skip header
      set temp []
      while [file-at-end? = false][ set temp lput file-read temp ]
      file-close
      set par_list fput temp par_list
    ]
    set par_list map [ a -> map [ b -> item a item b par_list ]  n-values length par_list [i -> i]   ]  n-values length first par_list [i -> i]
    (foreach sort patches par_list [ [a b] -> ask a [ set int_ves_num_allWeeks b ] ] )

    ; annual fuel
    set fuel_year filter [ a -> substring (item 1 a) 0 4 = year ] price_fuel

    ; annual resource prices
    set w_pos (position "month" res_prices_colnames)
    set res_prices_year filter [ a -> substring (item w_pos a) 0 4 = year ] res_prices
    set w_pos (position "spec" res_prices_colnames)
    set price_ple filter [ a -> (item w_pos a) = "PLE" ] res_prices_year
    set price_sol filter [ a -> (item w_pos a) = "SOL" ] res_prices_year
    set price_bll filter [ a -> (item w_pos a) = "BLL" ] res_prices_year
    set price_tur filter [ a -> (item w_pos a) = "TUR" ] res_prices_year
    set price_cod filter [ a -> (item w_pos a) = "COD" ] res_prices_year
    set price_nep filter [ a -> (item w_pos a) = "NEP" ] res_prices_year
    set price_csh filter [ a -> (item w_pos a) = "CSH" ] res_prices_year

  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;  processes at the beginning of every yearly QUARTER ;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  let month_day substring last day_ch 5 10
  if( month_day = "01-01" or month_day = "04-01" or month_day = "07-01" or month_day = "10-01" )[

    ; reset quarterly sole quotas
    ask vessels[
      set quota_quarter_sol ifelse-value( member? "SOL" fish_licence )
      [ (quota_sol * 0.9) / (count vessels with [member? "SOL" fish_licence]) / 4 ]
      [ 1 ]
    ]
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;  processes at the beginning of every MONTH  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  if( time:get "dayofmonth" day = 1 or ticks = 1 ) [

    ; update month and season variable
    set month substring last day_ch 0 7
    set season get.season (substring month 5 7)

    ; get current fuel prices
    set curr_price_fuel first first filter [ a -> item 1 a = month ] fuel_year

    ; get current species prices
    let w_pos (position "month" res_prices_colnames)
    let w_pos2 (position "price" res_prices_colnames)
    set curr_price_all filter [ a -> item w_pos a = month ] res_prices_year
    set curr_price_ple item w_pos2 first filter [ a -> item w_pos a = month ] price_ple
    set curr_price_sol item w_pos2 first filter [ a -> item w_pos a = month ] price_sol
    set curr_price_nep item w_pos2 first filter [ a -> item w_pos a = month ] price_nep
    set curr_price_bll item w_pos2 first filter [ a -> item w_pos a = month ] price_bll
    set curr_price_tur item w_pos2 first filter [ a -> item w_pos a = month ] price_tur
    set curr_price_cod item w_pos2 first filter [ a -> item w_pos a = month ] price_cod
    set curr_price_csh item w_pos2 first filter [ a -> item w_pos a = month ] price_csh

    ; Commonly used gears, primary species (those that were relatively used more than 20%).
    ; Only use if this is not the first month. Otherwise values set during setup
    if( ticks != 1 )[
      ask vessels[
        let uni_g remove-duplicates list_gears
        let freq_g map[ a -> (frequency a list_gears) / length list_gears ] uni_g
        set freq_g map[ a -> a > 0.2 ] freq_g
        set freq_g all.position freq_g true
        set freq_g map[ a -> item a uni_g ] freq_g
        set common_gears remove-duplicates (sentence common_gears freq_g)

        let uni_sp remove-duplicates list_primary_spec
        let freq_sp map[ a -> (frequency a list_primary_spec) / length list_primary_spec ] uni_sp
        set freq_sp map[ a -> a > 0.2 ] freq_sp
        set freq_sp all.position freq_sp true
        set freq_sp map[ a -> item a uni_sp ] freq_sp
        set common_prime_spec remove-duplicates (sentence common_prime_spec freq_sp)
      ]
    ]

    ; update spatial restricitons
    if( Scenario? = "none" )[

      ; avoid apply-raster bug
      file-open (word path_input_folder "fish_restr_months/Fish_restr_" month ".asc")
      repeat 6 [let header file-read-line] ; skip header
      set temp []
      while [file-at-end? = false][ set temp lput file-read temp ]
      file-close
      (foreach sort patches temp [ [a b] -> ask a [ set spat_restr b ] ] )

      ; translate spatial restriction information from numbers to characters
      let spat_restr_trans but-first csv:from-file (word path_input_folder "fish_restr_months/spat_restr_trans.csv")
      foreach spat_restr_trans [ a -> ask patches with [ spat_restr = item 1 a ][ set spat_restr item 0 a ] ]
      ask patches with [ member? "OWF" spat_restr or member? "NTZ" spat_restr  ][
        set fishable? false
        set pcolor orange
      ]
    ]

    ; forget fishing trips from one year ago
    let one_year_ago time:show time:plus day_ongoing (-1 * mem_backwards) "months" "YYYY-MM-dd HH:MM:SS"
    ask vessels [

      ; get position of first entry in memory that should be kept
      set w_pos map [ a -> abs( time:difference-between one_year_ago a "day" ) ] list_tripDays_delMem
      set w_pos position min w_pos w_pos

      ; remove all entries in memory list that are updated per trip and that are older than one year
      set list_catch sublist list_catch w_pos length list_catch
      set list_eur sublist list_eur w_pos length list_eur
      set list_fuel sublist list_fuel w_pos length list_fuel
      set list_profit sublist list_profit w_pos length list_profit
      set list_profit_day sublist list_profit_day w_pos length list_profit_day
      set list_depreciations sublist list_depreciations w_pos length list_depreciations
      set list_pred_profit_day sublist list_pred_profit_day w_pos length list_pred_profit_day
      set list_FishGro sublist list_FishGro w_pos length list_FishGro
      set list_trip_days sublist list_trip_days w_pos length list_trip_days
      set list_first_day sublist list_first_day w_pos length list_first_day
      set list_tripDays_delMem sublist list_tripDays_delMem w_pos length list_tripDays_delMem
      set list_last_day sublist list_last_day w_pos length list_last_day
      set list_perc_bT_trip sublist list_perc_bT_trip w_pos length list_perc_bT_trip
      set list_matched_ID sublist list_matched_ID w_pos length list_matched_ID
      set list_uncertainty sublist list_uncertainty w_pos length list_uncertainty
      set list_primary_spec sublist list_primary_spec w_pos length list_primary_spec

      set list_center_ID sublist list_center_ID w_pos length list_center_ID
      set list_center_agent sublist list_center_agent w_pos length list_center_agent
      set list_landing_port sublist list_landing_port w_pos length list_landing_port
      set list_num_searches sublist list_num_searches w_pos length list_num_searches
      set list_num_patches sublist list_num_patches w_pos length list_num_patches
      set list_steam_time sublist list_steam_time w_pos length list_steam_time
      set list_fish_time sublist list_fish_time w_pos length list_fish_time
      set list_trip_patch_ids sublist list_trip_patch_ids w_pos length list_trip_patch_ids
      set list_mean_depl_coeff sublist list_mean_depl_coeff w_pos length list_mean_depl_coeff

      ; remove incidences from consumat helper lists
      set list_meanProfit_peers sublist list_meanProfit_peers w_pos length list_meanProfit_peers
      set list_gears sublist list_gears w_pos length list_gears
      set list_gears_peers sublist list_gears_peers w_pos length list_gears_peers
      set list_primeSpec_peers sublist list_primeSpec_peers w_pos length list_primeSpec_peers
    ]

  ] ; beginning of month end

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;  processes at the beginning of every WEEK  ;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  if( time:get "dayofweek" day = 1 or ticks = 1 ) [

    ; get week of the year number
    let w_week time:show day "ww"

    ; update social network
    update.soc.net 0.25 0.5

    ;; determine whether this will be an active week for the fisher and
    let month_number read-from-string substring month 5 7
    ask vessels [
      set active_week ifelse-value( random-float 100 <= item (month_number - 1) chance_weekly )[ true ][ false ]
      if( status = "in-port" and active_week = false )[ set target_FishGro "STOP: inactive week" ]
    ]

    ; determine whether vessels would go out on a weekend
    ask vessels [ set weekend_trip ifelse-value (random-float 100 <= chance_weekend)[ true ][ false ] ]

    ;;; update environmental variables per patch (bottom temperature, salinity, and mixed layer depth)
    ; and weekly international fishing vessel distribution

    let w_pos position (word year "-" w_week) year_weeks
    if( w_pos = false and w_week = "53" )[ set w_pos 0 ]
    ask patches [
      set NWS_bottomT item w_pos NWS_bottomT_allWeeks
      set NWS_MLD item w_pos NWS_MLD_allWeeks
      set NWS_SAL item w_pos NWS_SAL_allWeeks
      set int_ves_num_weekly item w_pos int_ves_num_allWeeks
    ]

    ;;; distribution of international vessels

    ; weekly number of mean and sd values of grid vessels
    set w_week read-from-string time:show day "w"
    set temp first filter [ i -> first i = w_week ] weekly_int_tot_GridVes
    let tot_GridVes random-normal item 1 temp item 2 temp

    ; insert numbers in patch variables
    ; for patches with full fishing restrictions (OWF or NTZ), save relative effort values
    let displaced_int_effort sum [ int_ves_num_weekly ] of patches with [ member? "NTZ" spat_restr or member? "OWF" spat_restr or member? "coastFFH" spat_restr ]
    let patches_with_effort count patches with [ not member? "NTZ" spat_restr and not member? "OWF" spat_restr and not member? "coastFFH" spat_restr and int_ves_num_weekly > 0 ]

    ; Equally redistribute displaced international effort to other patches
    ; and convert relative effort to total effort
    let displaced_effort_patch displaced_int_effort / patches_with_effort
    ask patches with [ int_ves_num_weekly > 0 ] [
      set int_ves_num_weekly round ((int_ves_num_weekly + displaced_effort_patch) * tot_GridVes)
    ]

    ; set international fishing effort in spatial fishing restrictions to 0
    ask patches with [ member? "NTZ" spat_restr or member? "OWF" spat_restr or member? "coastFFH" spat_restr and int_ves_num_weekly > 0 ][ set int_ves_num_weekly 0 ]

    ; grid cells with NA values for environmental parameters will be unfishable.
    ; These are just a few areas very close to the coast and offshore areas beyond the German fishing grounds
    ask patches with [ NWS_bottomT = -9999 or NWS_SAL = -9999 or NWS_MLD = -9999 or elevation = -9999 ][ set fishable? false ]

  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;  DAILY processes ;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; subtracted daily private expenses (salary) of fishers from their savings (same for all fishers)
  ; and subtract daily fixed costs (depends on vessel length)
  ask vessels [ set savings savings - (monthly_expanses / 30) - daily_fixed_costs ]

  ; update current bottom temp
  set curr_bottomT last item (position last day_ch map [ a -> first a ] bottomT_daily) bottomT_daily

  ; update international vessel distribution
  let ves_num_mult random-normal daily_int_ves_distr_mean 0.25
  if ( ves_num_mult < 0 )[ set ves_num_mult 0 ]
  if ( ves_num_mult > 1 )[ set ves_num_mult 1 ]
  ask patches [ set int_ves_num_daily round ( int_ves_num_weekly * ves_num_mult ) ]    ; calculate the number of vessels influencing vessel density thresholds

  ; update the local depletion factor
  if( fish_depletion != 1 )[
    ask patches [
      ; all international vessels fish and deplete resources
      set depletion_coeff depletion_coeff * (fish_depletion ^ int_ves_num_daily)
      if( depletion_coeff > 1 )[ set depletion_coeff 1 ]
  ]]

  ;; update visuals

  ; show the distribution of international vessels as color gradient, if it was switched on
  if ( show_ves_distr = true )[ ask patches [ set pcolor scale-color white (int_ves_num_daily + ger_ves_num) 10 0 ] ]

  ; show the distribution depletion coefficients
  if ( show_depletion = true )[ ask patches [ set pcolor scale-color blue depletion_coeff 1 0 ] ]

  ; set colors after international distribution was updated
  if( show_ves_distr = true or show_depletion = true )[ ask patches with [ member? "OWF" spat_restr or member? "NTZ" spat_restr ][ set pcolor orange ] ]

  ;; fisher variables

  ask vessels [

    ; perceive environmental and economic variables
    fishers.perceive

    ; update satisfactions and uncertainties
    set temp (consumat.var.update true ["exist_sat" "pers_sat" "soc_sat" "soc_unc" "exist_unc"] "own" "own" "own" "own" "own" "own" "own" "own" "own" "own" "own")
  ]

  ;;; FISHING

  ; check if fishers that are currently transfering to new landing port are ready to go fishing again
  ask vessels with [ member? "transfer" status ][

    ; if the new starting date after the transfer is reached, the vessel has reached the new port and is ready to go fishing again.
    ; Set status to in-port, so that the vessel can try to go fishing again
    if( time:difference-between last status day "day" >= 0 )[ set status "in-port" ]

  ]; end vessels during transfer

  ; fishers in port
  ask vessels with [ status = "in-port" and active_week = true ][

    ; target_FishGro is the main control variable when an agent tries to go fishing.
    ; in case there are some restrictions or the agent decides to stay in the port,
    ; target_FishGro will be filled with a character starting with "STOP:"
    ; In the beginning of the precedure, target_FishGro is set to go for agents in ports,
    ; so they assess whether they can/want to go fishing
    ; In case the agent goes fishing, target_FishGro will be filled with the metier option.
    set target_FishGro "go"

    ; check whether vessels need repair days
    ; there is a 20% chance after every fishing trip that a vessel needs repairs (see functuion go.fishng), which always last two days
    if( need_repair != false )[
      set need_repair ifelse-value( need_repair >= 1 )[ false ][ need_repair + 1 ]
      set target_FishGro "STOP: need repair"
    ]

    ; if there is no repair maintenance, get the maximum trip length and see whether agent would engage in a weekend trip
    if( target_FishGro = "go" )[

      ; determine maximum trip length
      let chance random-float 100
      let w_pos 0
      while[ w_pos < (length trip_days_steps) and chance <= item w_pos chance_trip_length ][ set w_pos w_pos + 1 ]
      set curr_trip_length item w_pos trip_days_steps
      set curr_trip_length min (list curr_trip_length max_trip_days)  ; the trip length is limited by the fixed maximum vessel trip days

      ; if the vessel does not engage in a weekend trip, restrict trip length by the weekend
      if( weekend_trip = false )[ restrict.trip.length.weekend ]
    ]

    ; this while loop makes sure that the whole process is repeated in case the consumat_strategy was 'repeat' or 'imitate' and
    ; a fishing trip was not possible, because of technical reasons (e.g. no quota, no spatial path, or wave height)
    ; in this case the consumat_strategy will be changed from 'repeat' to 'deliberate' and from 'imitate' to 'inquire'
    while[ target_FishGro = "go" ][

      ; perceive behavioral options (depends on chosen consumat_strategy)
      let perceived_options perceive.options

      ; restrict options to available fishing gears, species licenses, and species quota
      set perceived_options (check.gear.license.quota.seasons perceived_options)

      ; estimate trip length (including weather, weekend, and multi-day limitations)
      if( not member? "STOP:" target_FishGro )[ set perceived_options (restrict.trip.wave.height perceived_options) ]

      ; predict profits, target species, center patch of trip, and change in consumat variables
      if( not member? "STOP:" target_FishGro )[ set perceived_options (predict.options perceived_options) ]

      ; calculate overall change in gain of satisfaction and loss of uncertainty
      ; rank (order) options based on satisfaction and uncertainty improvement
      ; sort out options that lead to worse satisfaction and uncertainty
      if( not member? "STOP:" target_FishGro )[ set perceived_options (rank.options perceived_options) ]

      ; sort out options with bycatches of quota-regulated species surpassing thresholds (10% of catch / 5% for Nephrops), while not owning the necessary license
      ; Also sort out options that target species for which global or individual quotas are exhausted
      if( not member? "STOP:" target_FishGro )[ set perceived_options (check.bycatch.quota perceived_options) ]

      ; while loop for remaining options, starting with the one promising the largest improvement for cosumat variables (gain in satisfaction and loss of uncertainty)
      let found_option? ifelse-value( member? "do nothing" target_FishGro )[ true ][ false ]; setting this to true will prevent the vessel from looking for alternative fishing options
      if( not member? "STOP:" target_FishGro )[
        while[ found_option? = false and length perceived_options > 0 ][

          ; set FishGro to "go", so the loop can run
          set target_FishGro "go"

          ; look for shortest path to fishing ground check whether there is a path available and fishing time is long enough
          let option (get.fished.patches first perceived_options)

          ; match current environmental parameters and engine power information with the trip data base and search closest trip
          if( not member? "STOP:" target_FishGro )[ set option (option.match.trip option) ]

          ; check if vessel transfer to another port is needed
          let transfer_time "dummy"
          if( not member? "STOP:" target_FishGro )[ set transfer_time transfer.vessel option ]
          if( member? "transfer" target_FishGro )[ set found_option? true ] ; setting this to true will prevent the vessel from looking for alternative fishing options

          ; look for shortest path to fishing ground check whether there is a path available and fishing time is long enough
          if( not member? "STOP:" target_FishGro )[ set option (option.calc.path option transfer_time) ]

          ; If the option is a viable fishing option, execute that option and leave while loop.
          if( not member? "STOP:" target_FishGro )[
            set found_option? true
            go.fishing option
          ]

          ; If the stop reason is transfering to a new port, then indicate that an option was found,
          ; so that the the consumat strategy doesnt switch to deliberate or inquire to look for more options.
          if( found_option? = false )[
            ifelse( member? "transfer" target_FishGro )[
              set found_option? true
            ][; If the stop reason is something else, delete the first option of perceived_option, and try next option
              set perceived_options but-first perceived_options
          ]]

        ] ; end while loop
      ] ; end check for stop

      ; if the consumat_strategy was repeat and the fishing option is non-suitable, then change strategy to deliberate and repeat
      if( last consumat_strategy = "repetition" and found_option? = false )[
        set consumat_strategy replace-item (length consumat_strategy - 1) consumat_strategy  "deliberation"
        set target_FishGro "go"
      ]

      ; if the consumat_strategy was imitation and the fishing option is non-suitable, then change strategy to inquiring and repeat
      if( last consumat_strategy = "imitation" and found_option? = false )[
        set consumat_strategy replace-item (length consumat_strategy - 1) consumat_strategy  "inquiring"
        set target_FishGro "go"
      ]

    ] ; end while loop for perceiving fishing options and going fishing
  ] ; end vessels in port

  ; vessels currently fishing add one day to their trip_days variable
  ; or return to the port if they reached their maximum trip length
  ask vessels with [status = "fishing"] [

    ; trip days proceeds 1
    set trip_days trip_days + 1

    ; all vessels currently fishing go home if the trip length is equal to
    ; the determined maximum trip days for the current trip (curr_trip_length)
    if( trip_days >= curr_trip_length )[ vessel.to.port  ]

  ]

  ; save information about fishers' decisions (target_FishGro), i.e. did they leave for a trip? If not, why? Are they currently on a trip?
  save.decision

  ; if local depletion is activated, fish resources grow
  ; The minimum for the depletion_coeff is 0.05
  if( fish_depletion != 0 )[
    ask patches[
      if( depletion_coeff < 1 )[
        if( depletion_coeff < 0.05 )[ set depletion_coeff 0.05 ]
        set depletion_coeff depletion_coeff  * fish_recovery
        if( depletion_coeff > 1 )[ set depletion_coeff 1 ]
      ]
    ]
  ]


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;  processes at the END OF THE YEAR ;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; prepare sensitivtiy variables
  ; These variables are exported for the sensitivity analysis using morris screening.
  ; Otherwise they have no effect.
  if( ticks = 364 )[

    set sensitivity_TripDays []
    set sensitivity_NumTrips []
    set sensitivity_lon []
    set sensitivity_lat []

    ask vessels[
      set sensitivity_TripDays sentence list_trip_days sensitivity_TripDays
      set sensitivity_NumTrips sentence (length list_first_day) sensitivity_NumTrips
      let center_patches patch-set list_center_agent
      set sensitivity_lon sentence ([pxcor] of center_patches) sensitivity_lon
      set sensitivity_lat sentence ([pycor] of center_patches) sensitivity_lat
    ]

    ; form means of lists
    set sensitivity_TripDays ifelse-value( length sensitivity_TripDays > 0 )[ mean sensitivity_TripDays ][ 0 ]
    set sensitivity_NumTrips ifelse-value( length sensitivity_NumTrips > 0 )[ mean sensitivity_NumTrips ][ 0 ]
    set sensitivity_lon ifelse-value( length sensitivity_lon > 0 )[ mean sensitivity_lon ][ 8.6 ]
    set sensitivity_lat ifelse-value( length sensitivity_lat > 0 )[ mean sensitivity_lat ][ 53.5 ]

] ; end prepare sensitivity files

  ; export results at the end of one year
  if( substring (last day_ch) 5 10 = "12-31" )[
    export
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;; end of model run ;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  if ( last day_ch = model_end_date ) [

    ; send all vessels to port and collect last catches
    ask vessels [ vessel.to.port ]

    ; stop model
    stop
  ]

end
@#$#@#$#@
GRAPHICS-WINDOW
1319
34
1873
475
-1
-1
3.25
1
10
1
1
1
0
0
0
1
0
167
0
132
0
0
1
ticks
30.0

BUTTON
19
10
88
43
setup
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
97
11
174
44
one day
go
NIL
1
T
OBSERVER
NIL
F
NIL
NIL
1

MONITOR
1327
42
1489
87
Date
word last day_ch \" \" time:show day \"EEEE\"
17
1
11

BUTTON
184
12
247
45
go
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
1

PLOT
22
49
404
240
Catches
Days
catch [t]
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Plaice" 1.0 0 -16777216 true "" "plot global_ple"
"Sole" 1.0 0 -955883 true "" "plot global_sol"
"Nephrops" 1.0 0 -13791810 true "" "plot global_nep"
"Brown shrimp" 1.0 0 -987046 true "" "plot global_csh"
"Other" 1.0 0 -7500403 true "" "plot global_other"
"Turbot & Brill" 1.0 0 -10263788 true "" "plot global_tur_bll"

PLOT
410
49
786
240
Revenue
Days
Revenue [10³ €]
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Plaice" 1.0 0 -16777216 true "" "plot (sum map [ a -> sum map [ b -> item (position \"PLE\" species_order) b ] a ] [ list_eur ] of vessels) / 1000"
"Sole" 1.0 0 -955883 true "" "plot (sum map [ a -> sum map [ b -> item (position \"SOL\" species_order) b ] a ] [ list_eur ] of vessels) / 1000"
"Nephrops" 1.0 0 -13791810 true "" "plot (sum map [ a -> sum map [ b -> item (position \"NEP\" species_order) b ] a ] [ list_eur ] of vessels) / 1000"
"Brown shrimp" 1.0 0 -987046 true "" "plot (sum map [ a -> sum map [ b -> item (position \"CSH\" species_order) b ] a ] [ list_eur ] of vessels) / 1000"
"Other" 1.0 0 -7500403 true "" "plot (sum map [ a -> sum map [ b -> item (position \"Other\" species_order) b ] a ] [ list_eur ] of vessels) / 1000"
"Turbot & Brill" 1.0 0 -10263788 true "" "plot ((sum map [ a -> sum map [ b -> item (position \"TUR\" species_order) b ] a ] [ list_eur ] of vessels) + (sum map [ a -> sum map [ b -> item (position \"BLL\" species_order) b ] a ] [ list_eur ] of vessels)) / 1000"

BUTTON
254
12
323
45
export
export
NIL
1
T
OBSERVER
NIL
E
NIL
NIL
1

MONITOR
799
87
931
132
plaice
word round global_ple \" / \" quota_ple
17
1
11

TEXTBOX
806
56
902
84
summed catch / quota [tons]
11
0.0
0

MONITOR
800
143
931
188
sole
word round global_sol \" / \" quota_sol
17
1
11

MONITOR
801
197
930
242
nephrops
word round global_nep \" / \" quota_nep
17
1
11

MONITOR
802
251
930
296
turbot & brill
word round global_tur_bll \" / \" quota_tur_bll
17
1
11

TEXTBOX
941
57
1047
99
Market prices [€ / kg] or [€ / l]
11
0.0
1

MONITOR
937
87
994
132
Plaice
precision curr_price_ple 2
17
1
11

MONITOR
937
138
994
183
Sole
precision curr_price_sol 2
17
1
11

MONITOR
937
192
1003
237
Nephrops
precision curr_price_nep 2
17
1
11

MONITOR
936
300
993
345
Fuel
precision curr_price_fuel 2
17
1
11

SWITCH
1156
164
1298
197
show_ves_distr
show_ves_distr
1
1
-1000

PLOT
22
465
405
689
CSH fleet - Metier Composition
Days
%
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"TBB - CSH" 1.0 0 -2674135 true "" "plot (count vessels with [ fleet = \"CSH\" and target_FishGro = \"TBB - CSH\" ] / count vessels with [ fleet = \"CSH\" ] ) * 100 "
"PUL - CSH" 1.0 0 -14070903 true "" "plot (count vessels with [ fleet = \"CSH\" and target_FishGro = \"PUL - CSH\" ] / count vessels with [ fleet = \"CSH\" ] ) * 100 "

SWITCH
797
434
929
467
Stochasticity?
Stochasticity?
1
1
-1000

MONITOR
803
302
929
347
brown shrimp
word round global_csh
17
1
11

MONITOR
936
246
1013
291
Brown shrimp
precision curr_price_csh 2
17
1
11

INPUTBOX
413
568
664
648
path_output_folder
path to output data folder
1
0
String

INPUTBOX
413
477
663
563
path_input_folder
path to input data folder
1
0
String

SWITCH
797
398
900
431
PUL?
PUL?
0
1
-1000

CHOOSER
797
351
997
396
Scenario?
Scenario?
"none" "test" "base_year" "NTZ"
3

SLIDER
708
592
883
625
fish_depletion
fish_depletion
.8
1
0.97
0.01
1
NIL
HORIZONTAL

SLIDER
709
556
880
589
fish_recovery
fish_recovery
1
1.5
1.2
0.01
1
NIL
HORIZONTAL

SWITCH
1156
272
1298
305
show_depletion
show_depletion
1
1
-1000

SLIDER
1073
590
1218
623
w_exist_sat_OTB
w_exist_sat_OTB
0
1
0.333333333
0.01
1
NIL
HORIZONTAL

SLIDER
1073
625
1220
658
w_exist_sat_PLESOL
w_exist_sat_PLESOL
0
1
0.2
0.01
1
NIL
HORIZONTAL

SLIDER
1073
661
1222
694
w_exist_sat_CSH
w_exist_sat_CSH
0
1
0.8
0.01
1
NIL
HORIZONTAL

SLIDER
1234
590
1382
623
w_pers_sat_OTB
w_pers_sat_OTB
0
1
0.333333333
0.01
1
NIL
HORIZONTAL

SLIDER
1235
625
1382
658
w_pers_sat_PLESOL
w_pers_sat_PLESOL
0
1
0.2
0.01
1
NIL
HORIZONTAL

SLIDER
1235
660
1382
693
w_pers_sat_CSH
w_pers_sat_CSH
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
1393
589
1540
622
w_soc_sat_OTB
w_soc_sat_OTB
0
1
0.333333333
0.01
1
NIL
HORIZONTAL

SLIDER
1393
624
1540
657
w_soc_sat_PLESOL
w_soc_sat_PLESOL
0
1
0.6
0.01
1
NIL
HORIZONTAL

SLIDER
1393
660
1542
693
w_soc_sat_CSH
w_soc_sat_CSH
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
1548
588
1701
621
w_exist_unc_OTB
w_exist_unc_OTB
0
1
0.9
0.01
1
NIL
HORIZONTAL

SLIDER
1548
624
1701
657
w_exist_unc_PLESOL
w_exist_unc_PLESOL
0
1
0.3
0.01
1
NIL
HORIZONTAL

SLIDER
1548
659
1702
692
w_exist_unc_CSH
w_exist_unc_CSH
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
1709
587
1863
620
w_soc_unc_OTB
w_soc_unc_OTB
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
1709
623
1865
656
w_soc_unc_PLESOL
w_soc_unc_PLESOL
0
1
0.7
0.01
1
NIL
HORIZONTAL

SLIDER
1709
660
1866
693
w_soc_unc_CSH
w_soc_unc_CSH
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
898
591
1063
624
target_savings_OTB
target_savings_OTB
0
100000
-999.0
1000
1
NIL
HORIZONTAL

SLIDER
897
626
1063
659
target_savings_PLESOL
target_savings_PLESOL
0
100000
-999.0
1000
1
NIL
HORIZONTAL

SLIDER
896
661
1063
694
target_savings_CSH
target_savings_CSH
0
100000
-999.0
1000
1
NIL
HORIZONTAL

SLIDER
898
557
1063
590
target_savings_gl
target_savings_gl
0
500000
440786.0
1000
1
NIL
HORIZONTAL

SLIDER
1073
556
1220
589
w_exist_sat_gl
w_exist_sat_gl
0
1
-999.0
.1
1
NIL
HORIZONTAL

PLOT
22
247
403
463
OTB - Metier composition
Days
%
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"OTB - NEP" 1.0 0 -10649926 true "" "plot (count vessels with [ fleet = \"OTB\" and target_FishGro = \"OTB - NEP&PLE\" ] / count vessels with [ fleet = \"OTB\" ] ) * 100"
"OTB - PLE" 1.0 0 -10603201 true "" "plot (count vessels with [ fleet = \"OTB\" and target_FishGro = \"OTB - PLE\" ] / count vessels with [ fleet = \"OTB\" ] ) * 100"

PLOT
409
248
788
466
PLE&SOL fleet - Metier Composition
Days
%
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"TBB - PLE" 1.0 0 -1664597 true "" "plot (count vessels with [ fleet = \"PLE&SOL\" and target_FishGro = \"TBB - PLE&SOL\" ] / count vessels with [ fleet = \"PLE&SOL\" ] ) * 100 "
"PUL - PLE" 1.0 0 -1184463 true "" "plot (count vessels with [ fleet = \"PLE&SOL\" and target_FishGro = \"PUL - PLE&SOL\" ] / count vessels with [ fleet = \"PLE&SOL\" ] ) * 100 "
"TBB - SOL" 1.0 0 -10141563 true "" "plot (count vessels with [ fleet = \"PLE&SOL\" and target_FishGro = \"TBB - SOL&PLE\" ] / count vessels with [ fleet = \"PLE&SOL\" ] ) * 100 "
"PUL - SOL" 1.0 0 -13840069 true "" "plot (count vessels with [ fleet = \"PLE&SOL\" and target_FishGro = \"PUL - SOL&PLE\" ] / count vessels with [ fleet = \"PLE&SOL\" ] ) * 100 "

SLIDER
1233
555
1383
588
w_pers_sat_gl
w_pers_sat_gl
0
1
-999.0
.1
1
NIL
HORIZONTAL

SLIDER
1392
554
1540
587
w_soc_sat_gl
w_soc_sat_gl
0
1
-999.0
.1
1
NIL
HORIZONTAL

SLIDER
1548
553
1700
586
w_exist_unc_gl
w_exist_unc_gl
0
1
-999.0
.1
1
NIL
HORIZONTAL

SLIDER
1710
552
1862
585
w_soc_unc_gl
w_soc_unc_gl
0
1
-999.0
.1
1
NIL
HORIZONTAL

SWITCH
1027
485
1245
518
read_calibrated_parameters?
read_calibrated_parameters?
0
1
-1000

SLIDER
707
628
879
661
perceiving_error
perceiving_error
0
1
0.05
.01
1
NIL
HORIZONTAL

SLIDER
706
663
881
696
CPUE_uncertainty_multiplier
CPUE_uncertainty_multiplier
0
2
0.5
.1
1
NIL
HORIZONTAL

TEXTBOX
1130
54
1312
166
Green - land\nYellow - vessels in port\nBlue - fished cells\nBlack - landing ports\nOrange - complete fishing restrictions\nGrey - Conditional fishing restrictions\n
11
0.0
1

TEXTBOX
1162
310
1312
338
Gradient from non-depleted (black) to depleted (white)
11
0.0
1

TEXTBOX
1159
199
1309
241
International vessel distribution from low (white) to high (black)
11
0.0
1

TEXTBOX
1028
471
1223
489
Should calibrated parameters be used?
11
0.0
1

TEXTBOX
712
537
862
555
General parameters\n
11
0.0
1

TEXTBOX
899
535
1762
564
Target savings and weightings of  satisfactions and uncertainties. Global sliders are used instead of fleet sliders, unless they are set to -999
11
0.0
1

TEXTBOX
1000
368
1150
386
NTZ = no-take zone
11
0.0
1

INPUTBOX
1158
343
1277
403
model_end_date
3000-12-31
1
0
String

TEXTBOX
1161
405
1311
447
Model is built for one-year runs. If scenario is chosen, the year will be 3000
11
0.0
1

TEXTBOX
904
402
1054
430
Are electric pulse gears allowed?
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

This Agent-based model (ABM) simulates the spatio-temporal dynamics of German fishers in the southern North Sea by applying high temporal (daily) and spatial (0.045 lon x 0.045 lat) resolution and a complex human decision-making methodology called the Consumat approach that goes beyond pure profit maximization.
The aim of the ABM is to test how different scenarios affect the spatio-temporal behavior and adaptive capacity of the fishers. Scenarios will encompass changes in resource availability (e.g. species migrations due to climate change), closed fishing area (e.g. more OWFs or MPAs), and market prices. These scenarios provide insights into potential future fishing effort displacement and shifting target species and fishing gears, which is useful information for effective fisheries management.  
Moreover, the ABM assesss the applicability of the Consumat approach for fishers’ behavior beyond rational decision-making. By building on existing behavioral theories for fishers, we use the ABM to anaylze the behavioral motivations of three German fleets, namely the near-coastal beam trawl fleet catching shrimps, the beam trawl fleet catching the flatfishes sole & plaice, and the otter board fleet catching plaice & Nephrops


## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

Every agent (fishing vessel) decides daily whether to go fishing (unless its already on a fishing trip), and in what fishing metiér to engage. Metiérs are a combination of gear and target species and depending on the technical characteristics and quota availabilities, every vessel has a different pool of potential metiérs to choose from. External factors, such as wave height, fish prices and fuel prices influence the agents’ decisions. One sea, agents prefer areas with less fishing vessels.
We included a complex human decision-making framework, namely the Consumat approach, in which agents’ behavioural strategy is determined by their levels of satisfaction and uncertainty. Depending on these levels, they will engage in one out of four strategies that vary in complexity and social engagement. In general, satisfied agents engage in simpler strategies (e.g. repeating their last metiér choice), whereas unsatisfied agents choose more complex strategies (e.g. evaluating several metiér options). Uncertain agents engage in more social strategies (e.g. evaluating options of their colleagues) and certain agents in more individual strategies (e.g. repeating their last metiér option). In our model, each agent has three satisfactions (personal, social, and existence)  and two uncertainties (social and existence) that each stand for different behavioural elements.

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

setup - Initialization. Setting up the model according to files in the model_input folder.

path_input_folder - file path to the folder with model input files

path_output_folder - file path to where exported results should be saved

one day - One time step/tick (day)

go - Executes ticks until the chosen end date of the model is reached (usually one run = one year)

Scenario? - Choose which scenario should be run

show_ves_distr - visual representation of international fishing vessels 

show_ves_path - visual representation of German fishing vessels movements

show_depletion - visual representation of local depletion


## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

 - The first day takes long, because yearly environmental variables are set up
 - Depending on the spatial scenario, the fishing effort will be restricted by spatial       fishing restrictions (red), i.e. offshore wind farms and marine protected areas
 - In areas with high fishing effort (e.g. close to the coast), local depletion is higher
 - Fishing trip data (used to simulated landings) and offshore wind farm data are not publicly available. Therefore, we removed offshore wind farm scenarios, and replaced real fishing trip data with simulated proxy data. 

All results including the analyis of scenarios are done in sepperated R scripts. Scenarios are compared to a baseline scenario to assess their effect.

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

- Choose scenarios and run the model
- Play with the model parameters
- Implement your own shapefiles for fishing restrictions (code for preparation can be provided)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="check_weather" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="0"/>
      <value value="3"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search_radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_searhes">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability_need_repair">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="check_SiteSearch" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="0"/>
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="check_obstacles" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
      <value value="500"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="check_fish_restr" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
      <value value="500"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="check_PathAlgo" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="check_altQuota" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="1"/>
      <value value="10"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;PLE&quot;"/>
      <value value="&quot;NEP&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="check_altPrices" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="1"/>
      <value value="10"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;PLE&quot;"/>
      <value value="&quot;NEP&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="check_altTemp" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="1"/>
      <value value="10"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="check_AmbTol" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.2"/>
      <value value="0.5"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.2"/>
      <value value="0.5"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability_need_repair">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search_radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_searches">
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="REPETITION" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="stochasticity" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="altBOxy" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="60"/>
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="altBTemp" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="60"/>
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="altBChl" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export
r:stop</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restr_weekend">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_trip_length">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="60"/>
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="altBSal" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export
r:stop</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restr_weekend">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_trip_length">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="60"/>
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="altExistSat" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
      <value value="-1000"/>
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="altSocSat" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
      <value value="-1000"/>
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="altPersSat" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
      <value value="-1000"/>
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="altExistUnc" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
      <value value="-1000"/>
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="altSocUnc" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
      <value value="-1000"/>
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="check_AmbTol_0unc" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export
r:stop</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restr_weekend">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
      <value value="0.7"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_trip_length">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="check_ExistPers_sat" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export
r:stop</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restr_weekend">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_trip_length">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
      <value value="-1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-1000"/>
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="3"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Weights" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export
r:stop</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restr_weekend">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_trip_length">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.4"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.2"/>
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="backwards" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="check_Sats" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
      <value value="-1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-1000"/>
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-1000"/>
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="check_Uncs" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_steam_dist">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
      <value value="-1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
      <value value="-1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="rel_inactive" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="rel_inactive_time">
      <value value="0"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="check_FuelP" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rel_inactive_time">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_searches">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial_restrictions?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
      <value value="1000"/>
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search_radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target_savings">
      <value value="50000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability_need_repair">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="BestFit" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export</final>
    <enumeratedValueSet variable="show_ves_distr">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_tolerance_lvl">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel_dens_thresh">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish_restr">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rel_inactive_time">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_unc">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altExist_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_exist_sat">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="G_ambition_lvl">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_searches">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="repetition">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFishP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_ves_path">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial_restrictions?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A*_OR_Dijkstra">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altFuelP">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSpecies">
      <value value="&quot;ALL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stochasticity?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="POM?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_unc">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swh_thresh">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_soc_sat">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem_backwards">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altWave">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altTemp">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="path_output_folder">
      <value value="&quot;M://ABM_results/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="check">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBOxy">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altPers_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily_int_ves_distr_mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBChl">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search_radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Result_variable">
      <value value="&quot;repetition&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_unc">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altSoc_sat">
      <value value="-999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altBSal">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target_savings">
      <value value="30000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Result_ID">
      <value value="&quot;BestFit_&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="path_input_folder">
      <value value="&quot;M://data_products/ABM_data/model_input/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEBUG">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altQuota">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability_need_repair">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="W_pers_sat">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
