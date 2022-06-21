# data prep 
path         <- "S:/RhoFED/NIAID/DAIT/Allergy_Asthma/COVID_19_004/Biostatistics/Data/Derive"
path_site_act <- "S:/RhoFED/NIAID/DAIT/Allergy_Asthma/COVID_19_004/ClinOps/CTL"

# Site activation files - choose the most recent
file_site_act <- list.files(path_site_act, pattern = "*.xlsx")[startsWith(list.files(path_site_act, pattern = "*.xlsx"), "COVID-19-004_Site status ")] %>% sort(decreasing = TRUE) %>% pluck(1)

site_activations <- read_xlsx(file.path(path_site_act, file_site_act)) %>% 
   setNames(., tolower(names(.))) %>% 
   select(siteid = `cris id`, institution  , site_activation_date = activated) %>% 
   # mutate(site_activation_date = openxlsx::convertToDate(site_activation_date),
   mutate(site_activation_date = lubridate::as_date(site_activation_date),
          siteid = as.character(siteid)) %>% 
   filter(!is.na(site_activation_date))

# adsl
dd00 <- read_sas(file.path(path, "adsl.sas7bdat")) %>% 
   setNames(., tolower(names(.)))  %>% 
   mutate(across(where(is.character), ~ zap_empty(.x)))

dd01 <- dd00 %>% 
   right_join(site_activations) %>% 
   select(subjid, site, institution, site_activation_date, siteid, eosdt, trtsdt, comptrfl, randdt, enrlfl, acrfl, protocol, cohort, mcdfl, haurelfl, sex, agegr1, mhfalg, mhlax, mhmed, mhfill, mhmast, mhmast1, mhmast2, mhae, mhinsec) %>%
   separate(site, c("site_short","site_dept"), sep = ": ", fill = "right", extra = "merge") %>% 
   mutate(site_short = ifelse(is.na(site_short), institution, site_short)) %>%  
   mutate(site = str_glue("{siteid}: {site_short}"),
          started_trt = !is.na(trtsdt),
          cohort = fct_rev(cohort))

# adsl - subset to randomized population
dd02 <- dd01 %>% filter(acrfl=="Y")
dd04 <- dd02 %>% filter(protocol=="5.0")

# adsl - subset to randomized population
dd03 <- dd01 %>% filter(protocol=="5.0")

# cumulative randomizations
#   - ALL
#   - by site
d_site_act <- dd02 %>% 
   select(site, randdt = site_activation_date) %>%  
   unique %>%
   mutate(n = 0) %>% 
   bind_rows(tibble(site = "00000",
                    randdt = min(.$randdt),
                    n = 0))
d_site <- dd02 %>% 
   bind_rows(dd02 %>% mutate(site = "00000")) %>%    
   group_by(site, randdt) %>% 
   tally %>% 
   ungroup %>% 
   bind_rows(d_site_act) %>% 
   group_by(site, randdt) %>% 
   summarise(n = sum(n))

d_site_cumul <- d_site  %>% 
   full_join(crossing(randdt = today(),
                      site = unique(d_site$site),
                      n = 0)) %>% 
   group_by(site)  %>% 
   arrange(site, randdt) %>% 
   mutate(n_tot = cumsum(n)) %>% 
   ungroup

d_site2 <- dd04 %>% 
   bind_rows(dd04 %>% mutate(site = "00000")) %>%    
   group_by(site, randdt) %>% 
   tally %>% 
   ungroup %>% 
   bind_rows(d_site_act) %>% 
   group_by(site, randdt) %>% 
   summarise(n = sum(n))

d_site_cumu <- d_site2  %>% 
   full_join(crossing(randdt = today(),
                      site = unique(d_site$site),
                      n = 0)) %>% 
   group_by(site)  %>% 
   arrange(site, randdt) %>% 
   mutate(n_tot = cumsum(n)) %>% 
   ungroup

# Cumulative randomizations
#   - by cohort
d_cohort <- dd02 %>%    
   group_by(cohort, randdt) %>% 
   tally %>% 
   ungroup  %>% 
   bind_rows(crossing(cohort = unique(.$cohort),
                      randdt = min(dd02$site_activation_date, na.rm=TRUE),
                      n = 0))


d_cohort_cumul <- d_cohort  %>%
   full_join( crossing(randdt = c(min(d_cohort$randdt), today()),
                       cohort = unique(d_cohort$cohort),
                       n=0))%>%
   group_by(cohort)  %>%
   arrange(cohort, randdt) %>%
   mutate(n_tot = cumsum(n)) %>%
   ungroup()

# EXPORT
dc <- dd01 %>% 
   mutate(min_site_activation_date = min(site_activation_date),
          max_randdt = max(randdt, na.rm=TRUE)) %>% 
   nest_by(site, site_activation_date, min_site_activation_date, max_randdt) %>% 
   mutate(data = list(data %>% filter(acrfl=="Y")),
          data_tab = list(data %>% 
                             select(acrfl, started_trt) %>%  
                             summarise(n_rand = sum(acrfl=="Y", na.rm = TRUE),
                                       n_started_trt = sum(started_trt, na.rm = TRUE)) %>%
                             select(n_rand, n_started_trt) %>% 
                             mutate(pct_started_trt = case_when(
                                n_rand==0 ~ NA_real_,
                                TRUE ~ round(100*n_started_trt/n_rand,1) 
                             ))),
          data_nrow = nrow(data)) %>% 
   mutate(
      data_sparkline = list(data %>% 
                               select(randdt) %>% 
                               mutate(n=1) %>% 
                               bind_rows(crossing(randdt = seq(min_site_activation_date, 
                                                               max_randdt, by = "days"),
                                                  n=0)) %>% 
                               group_by(randdt) %>% 
                               summarise(n = sum(n)) %>% 
                               ungroup %>% 
                               mutate(n_cumul = cumsum(n)))) %>% 
   ungroup %>% 
   select(site, site_activation_date, data_tab, data_sparkline) %>% 
   unnest(data_tab) %>% 
   mutate(rand_over_time = NA) %>% 
   arrange(site_activation_date, desc(n_rand), site)%>% 
   select(site, site_activation_date, n_rand, rand_over_time, data_sparkline, n_started_trt, pct_started_trt) %>% 
   unnest(cols = c(data_sparkline))

rio::export(dc %>% 
               mutate(site = str_extract(site, "[0-9]{5}") ), 
            'C:/R/UseR/2022_dashboards/dashboard/SARS/dat/d_c.rds')

rio::export(dd02 %>% 
               mutate(site = str_extract(site, "[0-9]{5}") ) %>% 
               select(site,
                      acrfl,
                      started_trt, 
                      comptrfl,
                      cohort,
                      mcdfl,
                      haurelfl,
                      sex,
                      agegr1,
                      haurelfl,
                      mhfalg,
                      mhlax,
                      mhmed,
                      mhfill,
                      mhmast,
                      mhmast1,
                      mhmast2,
                      mhae,
                      mhinsec), 
            'C:/R/UseR/2022_dashboards/dashboard/SARS/dat/d_2.rds')

rio::export(d_site_cumul %>% 
               mutate(site = str_extract(site, "[0-9]{5}") ), 
            'C:/R/UseR/2022_dashboards/dashboard/SARS/dat/d_site_cumul.rds')

rio::export(d_cohort_cumul, 
            'C:/R/UseR/2022_dashboards/dashboard/SARS/dat/d_cohort_cumul.rds')
