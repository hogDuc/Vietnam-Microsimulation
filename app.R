
library(dplyr)
library(tidyr)
library(haven)
library(readxl)
library(jsonlite)
library(readr)
library(sf)
library(ggplot2)

source("read_data.R")

# Calculate total market income from salary and business as well as expenditures ----

get_market_income <- function(
    tc_that_nghiep = 0, tc_thoi_viec = 0, luong_bt = 0, 
    luong_som = 0, tc_mat_suc = 0,
    hoc_phi_dt = 0, hoc_phi_tt = 0, dong_phuc = 0,
    sgk = 0, dung_cu_hoctap = 0, hoc_them = 0, gd_khac = 0,
    tc_thuong_binh = 0, tc_bao_tro_xh = 0, tc_thien_tai = 0,
    db_trong_trot = 0, db_chan_nuoi = 0, db_dvnn = 0,
    db_trong_rung = 0, db_nuoi_thuy_san = 0,
    thue_chan_nuoi = 0, thue_dvnn = 0, thue_lam_nghiep = 0, 
    thue_kd_lam_nghiep = 0, thue_doc_than = 0,
    inc_tax_1 = 0.05, inc_tax_2 = 0.1, inc_tax_3 = 0.15, inc_tax_4 = 0.2, 
    inc_tax_5 = 0.25, inc_tax_6 = 0.3, inc_tax_7 = 0.35,
    inc_threshold_1 = 5000, inc_threshold_2 = 10000, inc_threshold_3 = 18000, inc_threshold_4 = 32000,
    inc_threshold_5 = 52000, inc_threshold_6 = 80000
  ) {
  personal_income <- Ho_ThanhVien.dta %>% # tính thu nhập từ lương của mỗi cá nhân
    mutate( 
      M4A_C18A = M4A_C18A * (1 + tc_that_nghiep), 
      M4A_C18B = M4A_C18B * (1 + tc_thoi_viec), 
      M4A_C18C = M4A_C18C * (1 + luong_bt), 
      M4A_C18D = M4A_C18D * (1 + luong_som), 
      M4A_C18E = M4A_C18E * (1 + tc_mat_suc),
      
      # Primary education fee
      M2_C8A_2 = M2_C8A * (1 + hoc_phi_dt), 
      M2_C8B_2 = M2_C8B * (1 + hoc_phi_tt), 
      # M2_C8C = M2_C8C * (1 + ),
      # M2_C8D,
      M2_C8E_2 = M2_C8E * (1 + dong_phuc), 
      M2_C8F_2 = M2_C8F * (1 + sgk), 
      M2_C8G_2 = M2_C8G * (1 + dung_cu_hoctap), 
      M2_C8H_2 = M2_C8H * (1 + hoc_them), 
      M2_C8I_2 = M2_C8I * (1 + gd_khac),
      
      tro_cap_personal = M4A_C18A + M4A_C18B + M4A_C18E,
      monthly_salary = (M4A_C5 + M4A_C11 + M4A_C15)/12,
      salary_tax = case_when(
         monthly_salary <= inc_threshold_1 ~ inc_tax_1 * monthly_salary,
         monthly_salary <= inc_threshold_2 ~ inc_tax_2 * monthly_salary,
         monthly_salary <= inc_threshold_3 ~ inc_tax_3 * monthly_salary,
         monthly_salary <= inc_threshold_4 ~ inc_tax_4 * monthly_salary,
         monthly_salary <= inc_threshold_5 ~ inc_tax_5 * monthly_salary,
         monthly_salary <= inc_threshold_6 ~ inc_tax_6 * monthly_salary,
         TRUE ~ inc_tax_7 * monthly_salary
      )
    ) %>% 
    group_by(IDHO) %>% 
    summarise(
      salary_tax = sum(salary_tax, na.rm = TRUE),
      salary_income = sum(
        sum(M4A_C5, na.rm = TRUE), sum(M4A_C6A, na.rm = TRUE), 
        sum(M4A_C6B, na.rm = TRUE), sum(M4A_C11, na.rm = TRUE), 
        sum(M4A_C12A, na.rm = TRUE), sum(M4A_C12B, na.rm = TRUE), 
        sum(M4A_C15, na.rm = TRUE), sum(M4A_C18A, na.rm = TRUE), 
        sum(M4A_C18B, na.rm = TRUE), sum(M4A_C18C, na.rm = TRUE), 
        sum(M4A_C18D, na.rm = TRUE), sum(M4A_C18E, na.rm = TRUE),
        na.rm = TRUE
      ),
      tro_cap_personal = sum(tro_cap_personal, na.rm = TRUE),
      chiphi_giaoduc_base = sum(
        M2_C8A = sum(M2_C8A, na.rm = T), M2_C8B = sum(M2_C8B, na.rm = T),
        M2_C8C = sum(M2_C8C, na.rm = T), M2_C8D = sum(M2_C8D, na.rm = T),
        M2_C8E = sum(M2_C8E, na.rm = T), M2_C8F = sum(M2_C8F, na.rm = T),
        M2_C8G = sum(M2_C8G, na.rm = T), M2_C8H = sum(M2_C8H, na.rm = T),
        M2_C8I = sum(M2_C8I, na.rm = T), M2_C11 = sum(M2_C11, na.rm = T),
        na.rm = T
      ),
      chiphi_giaoduc_reformed = sum(
        M2_C8A_2 = sum(M2_C8A_2, na.rm = T), M2_C8B_2 = sum(M2_C8B_2, na.rm = T),
        M2_C8C = sum(M2_C8C, na.rm = T), M2_C8D = sum(M2_C8D, na.rm = T),
        M2_C8E_2 = sum(M2_C8E_2, na.rm = T), M2_C8F_2 = sum(M2_C8F_2, na.rm = T),
        M2_C8G_2 = sum(M2_C8G_2, na.rm = T), M2_C8H_2 = sum(M2_C8H_2, na.rm = T),
        M2_C8I_2 = sum(M2_C8I_2, na.rm = T), M2_C11 = sum(M2_C11, na.rm = T),
        na.rm = T
      )
    )  
    # mutate_all(~replace(., is.na(.), 0))
  
  household_income <- Ho_ThongTinHo_2 %>% # Tính thu nhập từ hộ kinh doanh
    mutate(
      M4B1T2 = M4B1T2 * (1 + db_trong_trot), # custom den bu
      M4B2T2 = M4B2T2 * (1 + db_chan_nuoi),
      M4B3T2 = M4B3T2 * (1 + db_dvnn),
      M4B4T2 = M4B4T2 * (1 + db_trong_rung),
      M4B5T2 = M4B5T2 * (1 + db_nuoi_thuy_san),
      M4D_04 = M4D_04 * (1 + tc_thuong_binh),
      M4D_05 = M4D_05 * (1 + tc_bao_tro_xh),
      M4D_06 = M4D_06 * (1 + tc_thien_tai),
      M4B22_C17_2 = M4B22_C17 * (1 + thue_chan_nuoi), # Thue
      M4B32_C15_2 = M4B32_C15 * (1 + thue_dvnn),
      M4B42_C12_2 = M4B42_C12 * (1 + thue_lam_nghiep),
      M4B52_C17_2 = M4B52_C17 * (1 + thue_kd_lam_nghiep),
    ) 
    # mutate_all(~replace(., is.na(.), 0))
  household_income$business_income <- rowSums(
    household_income[
      c(
        'M3TN', 'M2TN',
        'M4B0TN', "M4B1T2",
        'M4B11T', 'M4B12T', 'M4B13T', 'M4B14T', 'M4B15T',  
        'M4B21T', # Thu chăn nuôi
        'M4B22T', # Thu săn bắt 
        'M4B3T', 'M4B4T', 'M4B5T1', 'M4CT', 'M4D_01', 'M4D_02',
        'M4D_03', 'M4D_04', 'M4D_05', 'M4D_06', 'M4D_07', 'M4D_08', 'M4D_09', 
        'M4D_10', 'M4D_11', 'M4D_12',
        'M7_C12'
      )
    ],
    na.rm = TRUE
  )

  thongtinho_modified <- left_join(
    household_income,
    personal_income,
    by = "IDHO"
  ) 
  thongtinho_modified <- thongtinho_modified %>% 
    mutate(
      chitieu_sinhhoat_base = rowSums(
        thongtinho_modified %>% 
          select(
            chiphi_giaoduc_base, 
            # M3CT,
            M3A_C10, M3A_C11,  M3CT1, M3CT2, M3CT3,
            # M4B1C, M4B21C, M4B3C, M4B4C,
            M5A1C4, M5A1C5, M5A2C6, M5A2C7, M5A2C8, M5B1C6, M5B1C7, M5B1C8, 
            M5B2C4, M5B2C5, M5B3CT,
            # M5A1CT, M5A2CT, M5B1CT, M5B2CT, M5B3CT, 
            M6A_C7,
            M7_C6, M7_C9, M7_C14, M7_C17, M7_C19
            
            # M7_C20
          ),
        na.rm = T
      ),
      chitieu_sinhhoat_reformed = rowSums(
        thongtinho_modified %>% 
          select(
            chiphi_giaoduc_reformed, 
            # M3CT,
            M3A_C10, M3A_C11,  M3CT1, M3CT2, M3CT3,
            # M4B1C, M4B21C, M4B3C, M4B4C,
            M5A1C4, M5A1C5, M5A2C6, M5A2C7, M5A2C8, M5B1C6, M5B1C7, M5B1C8, 
            M5B2C4, M5B2C5, M5B3CT,
            # M5A1CT, M5A2CT, M5B1CT, M5B2CT, M5B3CT, 
            M6A_C7,
            M7_C6, M7_C9, M7_C14, M7_C17, M7_C19
            
            # M7_C20
          ),
        na.rm = T
      ),
      bachelor_tax = thue_doc_than * number_of_singles,
      total_household_income = business_income + salary_income,
      total_household_fee = TONGCHI - (M4B22_C17 + M4B32_C15 + M4B42_C12 + M4B52_C17) + # minus pre-tax reform values
        + (M4B22_C17_2 + M4B32_C15_2 + M4B42_C12_2 + M4B52_C17_2) + salary_tax + bachelor_tax # apply post-tax reform values
    ) %>% 
    left_join(
      cities,
      by = c("MATINH" , "MAHUYEN", "MAXA", "MADIABAN")
    ) %>% 
    mutate(
      wt_household = wt45 * SONHANKHAU,
      total_household_income_weight = (business_income + salary_income) * wt_household,
      total_household_fee_weight = total_household_fee * wt_household,
      total_chitieuBQ = TONGCHITIEUBQ - (chitieu_sinhhoat_base - chitieu_sinhhoat_reformed)/12,
      custom_thubq = ((total_household_income - total_household_fee) / SONHANKHAU)/12 + (chitieu_sinhhoat_base - chitieu_sinhhoat_reformed)/12,
      custom_thubq_weight = (((total_household_income - total_household_fee)*wt_household) / SONHANKHAU)/12,
      
    ) 
  # vẫn có chút chênh lệch k đáng kể
  return(thongtinho_modified)
}

# Calculate poverty rate by World Bank standard ----

get_poverty_rate <- function(df) {
  return(
    df %>% mutate(
      poverty_status = case_when(
        (custom_thubq) / 30 < (2.15  *  8.721)  ~ "Extreme poverty",
        (custom_thubq) / 30 < (3.65  *  8.721) ~ "Poor",
        # (THUBQ) / 30 < 6.85  *  8.621 ~ "upper-middle",
        TRUE ~ "Normal"
      )
    ) %>% 
      select(poverty_status, wt45, SONHANKHAU) %>% 
      group_by(poverty_status) %>% 
      summarise(n_poverty = sum(wt45 * SONHANKHAU))%>% 
      mutate(pc = n_poverty / sum(.$n_poverty))
  )
}

poverty_theo_vung <- function(df) {
  list_vung <- list()
  lanhtho <- unique(df$lanhtho)
  for (i in 1:length(lanhtho)) {
    vung <- lanhtho[i]
    if (!is.na(vung)) {
      data <-  df[df$lanhtho == vung,]
      list_vung[[vung]] <- get_poverty_rate(data)
    }
    else {
      next
    }
  }
  return(
    list_vung
  )
}

get_area_poverty <- function(df) {
  table <- data.frame()
  for (i in 1:length(df)) {
    table <- rbind(
      table,
      df[[i]] %>% 
        select(
          poverty_status, pc
        ) %>% 
        pivot_wider(
          names_from = poverty_status,
          values_from = pc
        ) %>% 
        mutate(
          vung = names(df[i]),
          Normal = round(Normal, digits = 4),
          Poor = round(Poor, digits = 4),
          `extreme poverty` = round(`extreme poverty`, digits = 4)
        ) %>% 
        select(
          vung, Normal, Poor, `extreme poverty`
        )
    )
  }
  return(table)
}

get_poverty_rate_tinh <- function(df) {
  return(
    df %>% mutate(
      poverty_status = case_when(
        (custom_thubq) / 30 < 2.15  *  8.721  ~ "extreme poverty",
        (custom_thubq) / 30 < 3.65  *  8.721 ~ "Poor",
        # (THUBQ) / 30 < 6.85  *  8.621 ~ "upper-middle",
        TRUE ~ "Normal"
      )
    ) %>% 
      select(tentinh, poverty_status, wt45, SONHANKHAU) %>% 
      group_by(tentinh, poverty_status) %>% 
      summarise(n_poverty = sum(wt45 * SONHANKHAU)) %>% 
      mutate(pc = n_poverty / sum(.$n_poverty))
  )
}


# Calculate GINI ----

gini_by_region <- function (df, region="") {
  if (region != "") {
    df2 <- df[df$mien == region, ]
  } else {
    df2 <- df
  }
  gini <- reldist::gini(df2$custom_thubq, df2$wt_household) 
  return(
    data.frame(
      "GINI" = gini
    )
  )
}

gini_by_province <- function (df, province="") {
  if (province != "") {
    df2 <- df[df$tentinh == province, ]
  } else {
    df2 <- df
  }
  gini <- reldist::gini(df2$custom_thubq, df2$wt_household) 
  return(gini)
}
