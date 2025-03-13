# Đọc và tùy chỉnh dữ liệu 
library(dplyr)
library(tidyr)
library(haven)
library(readxl)
library(jsonlite)
library(readr)
library(sf)


data_path <- 'VHLSS2022'
file_names <- list.files(data_path) 
for (file_name in file_names) {
  if (grepl(".dta", file_name)) {
    tryCatch(
      {
        eval(parse(text = paste0(
          file_name, '<- read_dta("', data_path, '/', file_name, '")'
        )))
        # print(paste("Imported file", file_name))
      },
      error = function(e) {
        warning(e)
      }
    )
  }
}

URL <- 'https://data.opendevelopmentmekong.net/dataset/55bdad36-c476-4be9-a52d-aa839534200a/resource/b8f60493-7564-4707-aa72-a0172ba795d8/download/vn_iso_province.geojson'

tryCatch({
  vietnam <- st_read(URL)
  vietnam <- vietnam %>% 
    mutate(
      Name_VI = case_when(
        Name_VI == "Bình  Dương" ~ "Bình Dương",
        Name_VI == "Thanh Hóa" ~ "Thanh Hóa",
        Name_VI == "Thừa Thiên-Huế" ~ "Thừa Thiên Huế",
        Name_VI == "TP. Hồ Chí Minh" ~ "Hồ Chí Minh",
        Name_VI == "Khánh  Hòa" ~ "Khánh Hòa",
        Name_VI == "Kiên  Giang" ~ "Kiên Giang",
        Name_VI == "Ninh  Thuận" ~ "Ninh Thuận",
        Name_VI == "Hòa Bình" ~ "Hoà Bình",
        Name_VI == "Quảng  Nam" ~ "Quảng Nam",
        Name_VI == "Quảng  Ngãi" ~ "Quảng Ngãi",
        Name_VI == "Thái  Nguyên" ~ "Thái Nguyên",
        TRUE ~ Name_VI
      )
    )
  saveRDS(vietnam, file = "vietnam_poly.RData")
}, error = function(e){
  vietnam <- readRDS('vietnam_poly.RData')
})

vung_mien <- read_csv("vung_mien_vn.csv", show_col_types = FALSE) %>% 
  fill(Miền) %>% 
  fill(`Lãnh thổ`) %>% 
  rename(
    c(
      "mien" = "Miền",
      "lanhtho" = "Lãnh thổ",
      "tentinh" = "Thành phố"
    )
  ) %>% 
  mutate(
    tentinh = case_when(
      tentinh  == "Hòa Bình" ~ "Hoà Bình",
      tentinh == "Thừa Thiên - Huế" ~ "Thừa Thiên Huế",
      TRUE ~ tentinh
    )
  )

Ho_ThongTinHo.dta <- Ho_ThongTinHo.dta %>% 
  left_join(
    Ho_Muc4B22.dta %>% 
      select(IDHO, M4B22_C17) %>% 
      group_by(IDHO) %>% 
      summarize(
        M4B22_C17 = sum(M4B22_C17, na.rm = T)
      ),
    by = 'IDHO'
  ) %>% 
  left_join(
    Ho_Muc4B32.dta %>% 
      select(IDHO, M4B32_C15) %>% 
      group_by(IDHO) %>% 
      summarize(
        M4B32_C15 = sum(M4B32_C15, na.rm = T)
      ),
    by = 'IDHO'
  ) %>% 
  left_join(
    Ho_Muc4B42.dta %>% 
      select(IDHO, M4B42_C12) %>% 
      group_by(IDHO) %>% 
      summarize(
        M4B42_C12 = sum(M4B42_C12, na.rm = T)
      ),
    by = 'IDHO'
  ) %>% 
  left_join(
    Ho_Muc4B52.dta %>% 
      select(IDHO, M4B52_C17) %>% 
      group_by(IDHO) %>% 
      summarize(
        M4B52_C17 = sum(M4B52_C17, na.rm = T)
      ),
    by = 'IDHO'
  )


# Determine area type for Income threshold----

area_type <- read_xlsx("ma_vung.xlsx") %>% 
  select(
    `Tỉnh/thành phố trực thuộc TW`, `Quận/ /thị xã/thành phố thuộc tỉnh`, `Vùng`
  ) %>% 
  fill(`Tỉnh/thành phố trực thuộc TW`, .direction = "down") %>% 
  drop_na() %>% 
  dplyr::rename(
    tentinh = `Tỉnh/thành phố trực thuộc TW`,
    tenhuyen = `Quận/ /thị xã/thành phố thuộc tỉnh`,
    vung = `Vùng`
  ) %>% 
  merge(
    data.frame(
      vung = c('I', 'II', 'III', 'IV'),
      min_salary = c(4960, 4410, 3860, 3450)
    )
  )


# cities <- read.csv("new_weight.csv") %>% # trong so va ten khu vuc
cities <- weight2022.dta %>% # trong so va ten khu vuc
  select(
    tinh, huyen, xa, diaban, tentinh, tenhuyen, tenxa, tendb, wt45
  ) %>% 
  dplyr::rename(
    MATINH = tinh ,
    MAHUYEN = huyen,
    MAXA = xa,
    MADIABAN = diaban 
  ) %>%  # Tính weight mỗi hộ trong các địa bàn
  mutate(
    MATINH = as.character(MATINH),
    MAHUYEN = as.character(MAHUYEN),
    MAXA = as.character(MAXA),
    MADIABAN = as.character(MADIABAN)
  )

cities$tentinh <- gsub("Tỉnh ", "", cities$tentinh)
cities$tentinh <- gsub("Thành phố ", "", cities$tentinh)

cities <- left_join(
  cities,
  vung_mien,
  by = join_by(tentinh)
) 


# Modification for bachelor tax ----

Ho_ThongTinHo_2 <- left_join(    # lấy số người độc thân trong mỗi hộ
  Ho_ThongTinHo.dta,
  select(
    Ho_ThanhVien.dta,
    IDHO, M1A_C7, M1A_C5, M1A_C2
  ) %>% # Lấy thông tin về tình trạng hôn nhân
    mutate(
      marriage_status = case_when(
        ((M1A_C2 = 1 & M1A_C5 >= 20)|(M1A_C2 = 2 & M1A_C5 >= 18)) &
          (M1A_C7 != 2 & M1A_C7 != 5) ~ 1,
        TRUE ~ 0
      )
    ) %>% # Gán 1 cho các trường hợp độc thân
    group_by(IDHO) %>%
    summarise(
      number_of_singles = sum(marriage_status, na.rm = TRUE)
    ),
  by = "IDHO"
) %>% 
  mutate_all(~replace(., is.na(.), 0))

