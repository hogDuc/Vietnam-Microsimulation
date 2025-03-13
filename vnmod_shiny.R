

library(DT)
library(shiny)
library(bslib)
library(shinyjs)
library(shinydashboard)
library(formattable)

# functions file
source("app.R")

# Đọc dữ liệu

ui <- page_sidebar(
  tags$script(HTML(
    "document.addEventListener('wheel', function(event){
        if(document.activeElement.type === 'number'){
            document.activeElement.blur();
        }
     });"
  )),
  title = "",
  sidebar = sidebar(
    useShinyjs(),
    style = "font-size: 15px",
    div(
        style = "display: flex; gap: 10px",
        actionButton(
          inputId = "submit",
          label = "Submit"
        ),
        actionButton(
          inputId = "reset",
          label = "Reset"
        )
    ),
    navset_card_tab(
      nav_panel(
        title = "Trợ cấp, đền bù",
        numericInput(
          "tc_thatnghiep",
          label = "Tỷ lệ thay đổi các khoản trợ cấp thất nghiệp",
          value = 0
        ),
        numericInput(
          "tc_thoiviec",
          label = "Tỷ lệ thay đổi các khoản trợ cấp thôi việc 1 lần",
          value = 0
        ),
        numericInput(
          "tc_matsuc",
          label = "Tỷ lệ thay đổi các khoản trợ cấp mất sức lao động",
          value = 0
        ),
        numericInput(
          "tc_baotro",
          label = "Tỷ lệ thay đổi các khoản trợ cấp các đối tượng bảo trợ xã hội",
          value = 0
        ),
        numericInput(
          "tc_thuongbinh",
          label = "Tỷ lệ thay đổi các khoản trợ cấp thương binh, liệt sỹ",
          value = 0
        ),
        numericInput(
          "tc_thientai",
          label = "Tỷ lệ thay đổi các khoản trợ cấp khắc phục thiên tai, dịch bệnh",
          value = 0
        ),
        numericInput(
          "db_trongtrot",
          label = "Tỷ lệ thay đổi các khoản đền bù trồng trọt",
          value = 0
        ),
        numericInput(
          "db_channuoi",
          label = "Tỷ lệ thay đổi các khoản đền bù về chăn nuôi",
          value = 0
        ),
        numericInput(
          "db_dvnn",
          label = "Tỷ lệ thay đổi các khoản đền bù dịch vụ nông nghiệp",
          value = 0
        ),
        numericInput(
          "db_trongrung",
          label = "Tỷ lệ thay đổi các khoản đền bù trồng rừng, lâm nghiệp",
          value = 0
        ),
        numericInput(
          "db_thuysan",
          label = "Tỷ lệ thay đổi các khoản đền bù về nuôi trồng thủy sản",
          value = 0
        )
      ),
      nav_panel(
        title = "Lương",
        numericInput(
          "luong_bt",
          label = "Tỷ lệ thay đổi lương nghỉ hưu thông thường",
          value = 0
        ),
        numericInput(
          "luong_som",
          label = "Tỷ lệ thay đổi lương nghỉ hưu sớm",
          value = 0
        )
      ),
      nav_panel(
        title = "Thuế",
        numericInput(
          "t_channuoi",
          label = "Tỷ lệ thay đổi về thuế chăn nuôi",
          value = 0
        ),
        numericInput(
          "t_dvnn",
          label = "Tỷ lệ thay đổi về thuế kinh doanh dịch vụ nông nghiệp",
          value = 0
        ),
        numericInput(
          "t_lamnghiep",
          label = "Tỷ lệ thay đổi về thuế lâm nghiệp",
          value = 0
        ),
        numericInput(
          "t_kdlamnghiep",
          label = "Tỷ lệ thay đổi về thuế kinh doanh lâm nghiệp",
          value = 0
        ),
        numericInput(
          "t_channuoi",
          label = "Tỷ lệ thay đổi về thuế chăn nuôi",
          value = 0
        ),
        numericInput(
          "income_tax_1",
          label = "% thuế TNCN mức 1",
          value = 0.05
        ),
        numericInput(
          "income_tax_2",
          label = "% thuế TNCN mức 2",
          value = 0.1
        ),
        numericInput(
          "income_tax_3",
          label = "% thuế TNCN mức 3",
          value = 0.15
        ),
        numericInput(
          "income_tax_4",
          label = "% thuế TNCN mức 4",
          value = 0.2
        ),
        numericInput(
          "income_tax_5",
          label = "% thuế TNCN mức 5",
          value = 0.25
        ),
        numericInput(
          "income_tax_6",
          label = "% thuế TNCN mức 6",
          value = 0.3
        ),
        numericInput(
          "income_tax_7",
          label = "% thuế TNCN mức 7",
          value = 0.35
        ),
        numericInput(
          "threshold_1",
          label = "Mức thu nhập chịu thuế 1",
          value = 5000
        ),
        numericInput(
          "threshold_2",
          label = "Mức thu nhập chịu thuế 2",
          value = 10000
        ),
        numericInput(
          "threshold_3",
          label = "Mức thu nhập chịu thuế 3",
          value = 18000
        ),
        numericInput(
          "threshold_4",
          label = "Mức thu nhập chịu thuế 4",
          value = 32000
        ),
        numericInput(
          "threshold_5",
          label = "Mức thu nhập chịu thuế 5",
          value = 52000
        ),
        numericInput(
          "threshold_6",
          label = "Mức thu nhập chịu thuế 6",
          value = 80000
        )
      ),
      nav_panel(
        title = "Chính sách thử nghiệm",
        numericInput(
          "t_docthan",
          label = "Thuế độc thân (nghìn đồng / năm)",
          value = 0
        ),
        numericInput(
          inputId = 'hp_dungtuyen',
          value = 0,
          label = '% Thay đổi mức học phí đúng tuyến'
        ),
        numericInput(
          inputId = 'hp_traituyen',
          value = 0,
          label = '% Thay đổi mức học phí trái tuyến'
        ),
        numericInput(
          inputId = 'dong_phuc',
          value = 0,
          label = '% Thay đổi phí đồng phục'
        ),
        numericInput(
          inputId = 'phi_sgk',
          value = 0,
          label = '% Thay đổi giá sách giáo khoa'
        ),
        numericInput(
          inputId = 'dungcu_ht',
          value = 0,
          label = '% Thay đổi giá dụng cụ học tập'
        ),
        numericInput(
          inputId = 'hoc_them',
          value = 0,
          label = '% Thay đổi phí học thêm'
        )
      )
    ),
    width = "30%"
  ),
    navset_card_tab(
      title = "Kết quả",
      nav_panel(
        "Thu nhập, chi tiêu",
        fluidRow(
          h3("Tổng thu toàn thị trường"),
          formattableOutput(
            "market_income"
          )
        ),
        fluidRow(
          h3("Tổng thu chính phủ"),
          formattableOutput(
            "government_income"
          )
        ),
        fluidRow(
          h3("Tổng chi chính phủ"),
          formattableOutput(
            "government_expenditure"
          )
        )
      ),
      nav_panel(
        "GINI",
        fluidRow(
          h3("Chỉ số GINI toàn quốc"),
          formattableOutput(
            "vietnam_gini"
          )
        ),
        fluidRow(
          h3("Chỉ số GINI phân chia theo 3 miền"),
          formattableOutput(
            "gini_mien"
          )
        ),
        fluidRow(
          h3("Chỉ số GINI phân theo các tỉnh, thành phố"),
          formattableOutput(
            "gini_tinh"
          )
        )
      ),
      nav_panel(
        "Tỷ lệ nghèo",
        fluidRow(
          card(
            card_header(
              "Tỷ lệ nghèo toàn quốc"
            ),
            card_body(
              plotOutput(
                "compare_overall_poverty"
              )
            )
          )
        ),
        fluidRow(
          card(
            card_header(
              "Dân số nghèo toàn quốc"
            ),
            card_body(
              formattableOutput("compare_poverty_population")
            )
          )
        # ),
        # layout_columns(
        #   card(
        #     card_header(
        #       "Bản đồ tỷ lệ nghèo Việt Nam (baseline)"
        #     ),
        #     card_body(
        #       plotOutput(
        #         "vietnam_map_baseline"
        #       )
        #     )
        #   ),
        #   card(
        #     card_header(
        #       "Bản đồ tỷ lệ nghèo Việt Nam (reformed)"
        #     ),
        #     card_body(
        #       plotOutput("vietnam_map_reformed")
        #     )
        #   )
        )
      )
    )
  )

server <- function(input, output) {
  baseline <- get_market_income() 
  baseline_poverty <- get_poverty_rate(baseline) 
  baseline_poverty_2 <- baseline_poverty %>% 
    select(poverty_status, pc) %>% 
    filter(
      poverty_status != "Normal"
    )
  baseline_vung_poverty <- poverty_theo_vung(baseline)
  baseline_poverty_2$condition <- "baseline"
  
  map_baseline <- get_poverty_rate_tinh(baseline)
  
  region_1 <- unique(baseline$mien)
  gini <- sapply(
    region_1,
    function (region_1) gini_by_region(baseline, region = region_1)
  ) %>% t
  
  province_1 <- unique(baseline$tentinh)
  gini_tinh_baseline <- sapply(
    province_1, 
    function (province_1) gini_by_province(baseline, province = province_1)
  ) 
  
  observeEvent(input$submit, {
    showModal(modalDialog(
      "Đang thực hiện mô phỏng...",
      footer = NULL
    ))
    reformed <- get_market_income(
      tc_that_nghiep = input$tc_thatnghiep, tc_thoi_viec = input$tc_thoiviec, 
      luong_bt = input$luong_bt, luong_som = input$luong_som, 
      hoc_phi_dt = input$hp_dungtuyen, hoc_phi_tt = input$hp_traituyen, dong_phuc = input$dong_phuc,
      sgk = input$phi_sgk, dung_cu_hoctap = input$dungcu_ht, hoc_them = input$hoc_them,
      tc_mat_suc = input$tc_matsuc,
      tc_thuong_binh = input$tc_thuongbinh, tc_bao_tro_xh = input$tc_baotro, 
      tc_thien_tai = input$tc_thientai,
      db_trong_trot = input$db_trongtrot, db_chan_nuoi = input$db_channuoi, 
      db_dvnn = input$db_dvnn, db_trong_rung = input$db_trongrung, 
      db_nuoi_thuy_san = input$db_thuysan,
      thue_chan_nuoi = input$t_channuoi, thue_dvnn = input$t_dvnn, 
      thue_lam_nghiep = input$t_lamnghiep, thue_kd_lam_nghiep = input$t_kdlamnghiep, 
      thue_doc_than = input$t_docthan,
      inc_tax_1 = input$income_tax_1, inc_tax_2 = input$income_tax_2, 
      inc_tax_3 = input$income_tax_3, inc_tax_4 = input$income_tax_4, 
      inc_tax_5 = input$income_tax_5, inc_tax_6 = input$income_tax_6, 
      inc_tax_7 = input$income_tax_7,
      inc_threshold_1 = input$threshold_1, inc_threshold_2 = input$threshold_2, 
      inc_threshold_3 = input$threshold_3, inc_threshold_4 = input$threshold_4,
      inc_threshold_5 = input$threshold_5, inc_threshold_6 = input$threshold_6
    )
    reformed_poverty <- get_poverty_rate(reformed) 
    reformed_poverty_2 <- reformed_poverty %>% 
      select(poverty_status, pc) %>% 
      filter(
        poverty_status != "Normal"
      )
    reformed_poverty_2$condition <- "reformed"
    
    compare_poverty <- rbind(
      baseline_poverty_2, reformed_poverty_2
    )
    compare_poverty_plot <- ggplot(
      compare_poverty,
      aes(x = pc, y = condition, fill = poverty_status)
    ) + 
      geom_bar(
        position = "dodge", stat = "identity"
      ) + 
      theme_bw() +
      labs(x = "Percent", y = "Poverty condition") + 
      theme(legend.position = "bottom")
    
    reformed_vung_poverty <- poverty_theo_vung(reformed)
    
    hi_lo_formatter <- formatter(
      'span', 
      style = x ~ style(
        font.weight = "bold",
        color = ifelse(x > 0, "green", ifelse(x < 0, 'red', 'yellow'))
      ),
      x ~ icontext(ifelse(x > 0, 'chevron-up', ifelse(x < 0, 'chevron-down', "")), x)
    )
    
    market_income <- formattable(
      data.frame(
        "category" = c(
          "Tổng thu nhập từ lương, kinh doanh toàn thị trường",
          "Chi phí kinh doanh toàn thị trường",
          "Lợi nhuận (Tổng thu - chi phí) toàn thị trường",
          "Chi tiêu sinh hoạt bình quân hằng tháng"
        ),
        "baseline_million" = c(
          sum(baseline$total_household_income_weight) / 1000,
          sum(baseline$total_household_fee_weight) / 1000,
          sum(baseline$total_household_income_weight - baseline$total_household_fee_weight) / 1000,
          weighted.mean(
            baseline %>% 
              filter(TONGCHITIEUBQ>0) %>% 
              .$total_chitieuBQ, 
            w = baseline %>% 
              filter(TONGCHITIEUBQ>0) %>%
              .$wt_household
          ) / 1000
        ),
        "reformed_million" = c(
          sum(reformed$total_household_income_weight) / 1000,
          sum(
            reformed$total_household_fee_weight) / 1000,
          sum(
            reformed$total_household_income_weight - reformed$total_household_fee_weight
          ) / 1000,
          weighted.mean(
            reformed %>% 
              filter(TONGCHITIEUBQ>0) %>%
              .$total_chitieuBQ,
            w = reformed %>%
              filter(TONGCHITIEUBQ>0) %>%
              .$wt_household
          ) / 1000
        )
      ) %>% mutate(
        diff = reformed_million - baseline_million
      ) %>% 
        rename(
          'Mục' = 'category',
          'Baseline (Triệu VND)' = 'baseline_million',
          'Reformed (Triệu VND)' = 'reformed_million',
        'Thay đổi' = 'diff'
        ) %>%
        mutate(
          across(where(is.numeric), ~prettyNum(., big.mark =',', digits = 4, scientific = FALSE))
        ),
      list(
        `Thay đổi` = hi_lo_formatter
      )
    )
    
    government_income <- formattable(data.frame(
        "category" = c(
          "Tổng thu nhập chính phủ từ thuế", 
          "Thuế TNCN từ lương", 
          "Thuế kinh doanh chăn nuôi",
          "Thuế dịch vụ nông nghiệp", 
          "Thuế lâm nghiệp", "Thuế kinh doanh lâm nghiệp",
          "Thuế độc thân"
        ),
        "baseline_million" = c(
          sum(
            sum(baseline$salary_tax * baseline$wt_household),
            sum(baseline$M4B22_C17_2 * baseline$wt_household),
            sum(baseline$M4B32_C15_2 * baseline$wt_household),
            sum(baseline$M4B42_C12_2 * baseline$wt_household),
            sum(baseline$M4B52_C17_2 * baseline$wt_household),
            sum(baseline$bachelor_tax * baseline$wt_household)
          )/ 1000,
          sum(baseline$salary_tax * baseline$wt_household)/ 1000,
          sum(baseline$M4B22_C17_2 * baseline$wt_household)/ 1000,
          sum(baseline$M4B32_C15_2 * baseline$wt_household)/ 1000,
          sum(baseline$M4B42_C12_2 * baseline$wt_household)/ 1000,
          sum(baseline$M4B52_C17_2 * baseline$wt_household)/ 1000,
          sum(baseline$bachelor_tax * baseline$wt_household)/ 1000
        ),
        "reformed_million" = c(
          sum(
            sum(reformed$salary_tax * reformed$wt_household),
            sum(reformed$M4B22_C17_2 * reformed$wt_household),
            sum(reformed$M4B32_C15_2 * reformed$wt_household),
            sum(reformed$M4B42_C12_2 * reformed$wt_household),
            sum(reformed$M4B52_C17_2 * reformed$wt_household),
            sum(reformed$bachelor_tax * reformed$wt_household)
          )/ 1000,
          sum(reformed$salary_tax * reformed$wt_household)/ 1000,
          sum(reformed$M4B22_C17_2 * reformed$wt_household)/ 1000,
          sum(reformed$M4B32_C15_2 * reformed$wt_household)/ 1000,
          sum(reformed$M4B42_C12_2 * reformed$wt_household)/ 1000,
          sum(reformed$M4B52_C17_2 * reformed$wt_household)/ 1000,
          sum(reformed$bachelor_tax * reformed$wt_household)/ 1000
        )
      ) %>% 
      mutate(
        diff = reformed_million - baseline_million
      ) %>% 
      rename(
        'Mục' = 'category',
        'Baseline (Triệu VND)' = 'baseline_million',
        'Reformed (Triệu VND)' = 'reformed_million',
        'Thay đổi' = 'diff'
      ) %>% 
      mutate(
        across(where(is.numeric), ~prettyNum(., big.mark =',', digits = 4, scientific = FALSE))
      ),
      list(
        `Thay đổi` = hi_lo_formatter
      ))

    government_expenditure <- formattable(data.frame(
      "category" = c(
        "Tổng chi trợ cấp, đền bù chính phủ",
        "Trợ cấp thất nghiệp, thôi việc 1 lần và trợ cấp mất sức lao động",
        "Đền bù trồng trọt",
        "Đền bù hoạt động chăn nuôi",
        "Đền bù dịch vụ nông nghiệp",
        "Đền bù hoạt động trồng rừng",
        "Đền bù hoạt động nuôi, trồng thủy sản",
        "Trợ cấp thương binh, liệt sỹ",
        "Trợ cấp các đối tượng bảo trợ xã hội",
        "Trợ cấp khắc phục thiên tai, dịch bệnh"
      ),
      "baseline_million" = c(
        sum(
          sum(baseline$tro_cap_personal * baseline$wt_household, na.rm = TRUE),
          sum(baseline$M4B1T2 * baseline$wt_household, na.rm = TRUE),
          sum(baseline$M4B2T2 * baseline$wt_household, na.rm = TRUE),
          sum(baseline$M4B3T2 * baseline$wt_household, na.rm = TRUE),
          sum(baseline$M4B4T2 * baseline$wt_household, na.rm = TRUE),
          sum(baseline$M4B5T2 * baseline$wt_household, na.rm = TRUE),
          sum(baseline$M4D_04 * baseline$wt_household, na.rm = TRUE),
          sum(baseline$M4D_05 * baseline$wt_household, na.rm = TRUE),
          sum(baseline$M4D_06 * baseline$wt_household, na.rm = TRUE)
        )/ 1000,
        sum(baseline$tro_cap_personal * baseline$wt_household, na.rm = TRUE)/ 1000,
        sum(baseline$M4B1T2 * baseline$wt_household, na.rm = TRUE)/ 1000,
        sum(baseline$M4B2T2 * baseline$wt_household, na.rm = TRUE)/ 1000,
        sum(baseline$M4B3T2 * baseline$wt_household, na.rm = TRUE)/ 1000,
        sum(baseline$M4B4T2 * baseline$wt_household, na.rm = TRUE)/ 1000,
        sum(baseline$M4B5T2 * baseline$wt_household, na.rm = TRUE)/ 1000,
        sum(baseline$M4D_04 * baseline$wt_household, na.rm = TRUE)/ 1000,
        sum(baseline$M4D_05 * baseline$wt_household, na.rm = TRUE)/ 1000,
        sum(baseline$M4D_06 * baseline$wt_household, na.rm = TRUE)/ 1000
      ),
      "reformed_million" = c(
        sum(
          sum(reformed$tro_cap_personal * reformed$wt_household, na.rm = TRUE),
          sum(reformed$M4B1T2 * reformed$wt_household, na.rm = TRUE),
          sum(reformed$M4B2T2 * reformed$wt_household, na.rm = TRUE),
          sum(reformed$M4B3T2 * reformed$wt_household, na.rm = TRUE),
          sum(reformed$M4B4T2 * reformed$wt_household, na.rm = TRUE),
          sum(reformed$M4B5T2 * reformed$wt_household, na.rm = TRUE),
          sum(reformed$M4D_04 * reformed$wt_household, na.rm = TRUE),
          sum(reformed$M4D_05 * reformed$wt_household, na.rm = TRUE),
          sum(reformed$M4D_06 * reformed$wt_household, na.rm = TRUE)
        )/ 1000,
        sum(reformed$tro_cap_personal * reformed$wt_household, na.rm = TRUE)/ 1000,
        sum(reformed$M4B1T2 * reformed$wt_household, na.rm = TRUE)/ 1000,
        sum(reformed$M4B2T2 * reformed$wt_household, na.rm = TRUE)/ 1000,
        sum(reformed$M4B3T2 * reformed$wt_household, na.rm = TRUE)/ 1000,
        sum(reformed$M4B4T2 * reformed$wt_household, na.rm = TRUE)/ 1000,
        sum(reformed$M4B5T2 * reformed$wt_household, na.rm = TRUE)/ 1000,
        sum(reformed$M4D_04 * reformed$wt_household, na.rm = TRUE)/ 1000,
        sum(reformed$M4D_05 * reformed$wt_household, na.rm = TRUE)/ 1000,
        sum(reformed$M4D_06 * reformed$wt_household, na.rm = TRUE)/ 1000
      ) 
    ) %>% 
      mutate(
        diff = reformed_million - baseline_million
      ) %>% 
      rename(
        'Mục' = 'category',
        'Baseline (Triệu VND)' = 'baseline_million',
        'Reformed (Triệu VND)' = 'reformed_million',
        'Thay đổi' = 'diff'
      ) %>% 
      mutate(
        across(where(is.numeric), ~prettyNum(., big.mark =',', digits = 4, scientific = FALSE))
      ),
    list(
      `Thay đổi` = hi_lo_formatter
    ))
    
    poverty_population <- formattable(data.frame(
      cbind(
        select(baseline_poverty, poverty_status, n_poverty), 
        select(reformed_poverty, poverty_status, n_poverty)
      )
    ) %>% 
      select(1,2, 4) %>% 
      mutate(
        diff = n_poverty.1 - n_poverty
      ) %>% 
      arrange(
        match(poverty_status, c("Normal", "Poor", "Extreme poverty"))
      ) %>% 
      rename(
        c(
          "Tình trạng nghèo" = "poverty_status" ,
          "Baseline (người)" = "n_poverty" ,
          "Reformed (người)" = "n_poverty.1" ,
          'Thay đổi' = 'diff'
        )
      ) %>% 
      mutate(
        across(where(is.numeric), ~prettyNum(., big.mark =',', digits = 4, scientific = FALSE))
      ),
    list(
      `Thay đổi` = hi_lo_formatter
    ))
    
    # Ve map Vietnam
    map_reformed <- get_poverty_rate_tinh(reformed) %>% select(
      -c("n_poverty")
    ) %>% filter(
      poverty_status != "Normal"
    )
    map_baseline <- map_baseline %>% select(
      -c("n_poverty")
    ) %>% filter(
      poverty_status != "Normal"
    )
    
    data_map_baseline <- left_join(
      vietnam,
      map_baseline,
      by = c("Name_VI" = "tentinh"),
      relationship = "many-to-many"
    )
    data_map_reformed <- left_join(
      vietnam,
      map_reformed,
      by = c("Name_VI" = "tentinh"),
      relationship = "many-to-many"
    )
    
    vietnam_map_base <- ggplot(data_map_baseline) + 
      geom_sf(aes(fill = pc)) +
      scale_fill_gradient(
        low = "lightblue", 
        high = "darkblue",
        na.value = "grey",
        # breaks = c(0.025, 0.1)
      ) +
      theme_void() +
      theme(
        legend.position = c(0.78, 0.82),
        legend.title = element_text(size=9),
        plot.background = element_rect(fill='white', colour='white')
      )
    
    vietnam_map_ref <- ggplot(data_map_reformed) + 
      geom_sf(aes(fill = pc)) +
      scale_fill_gradient(
        low = "lightblue", 
        high = "darkblue",
        na.value = "grey",
        # breaks = c(0.025, 0.1)
      ) +
      theme_void() +
      theme(
        legend.position = c(0.78, 0.82),
        legend.title = element_text(size=9),
        plot.background = element_rect(fill='white', colour='white')
      )
    
    # Bang tinh GINI
    gini_vietnam <- formattable(cbind(
      gini_by_region(baseline) %>% 
        rename(
          c("Baseline" = "GINI")
        ),
      gini_by_region(reformed) %>% 
        rename(
          c("Reformed" = "GINI")
        )
    ) %>% 
      mutate(
        `Thay đổi` = Reformed - Baseline,
        across(where(is.numeric), ~prettyNum(., big.mark =',', digits = 4, scientific = FALSE))
      ),
    list(
      `Thay đổi` = hi_lo_formatter
    )
    )
    
    region_2 <- unique(reformed$mien)
    gini_2 <- sapply(
      region_2,
      function (region_2) gini_by_region(reformed, region = region_2)
    ) %>% t
    
    mien_gini <- data.frame(rbind(gini, gini_2)) %>% 
        rename(
          'GINI miền Bắc' = 'Bắc.GINI',
          'GINI miền Trung' = 'Trung.GINI',
          'GINI miền Nam' = 'Nam.GINI'	
        ) %>% 
        t() %>% as.data.frame() %>% 
        rename(
          'Baseline' = 'V1',
          'Reformed' = 'V2'
        ) %>% 
        mutate(
          Baseline = as.numeric(Baseline),
          Reformed = as.numeric(Reformed),
          `Thay đổi` = Reformed - Baseline,
          across(where(is.numeric), ~prettyNum(., big.mark =',', digits = 4, scientific = FALSE))
        )
    mien_gini <- cbind(rownames(mien_gini), mien_gini) %>% 
      rename(
        "Miền" = 1
      )
    rownames(mien_gini) <- NULL
    
    mien_gini <- formattable(
      mien_gini,
      list(
        `Thay đổi` = hi_lo_formatter
        )
      )
    
    province_2 <- unique(reformed$tentinh)
    gini_tinh_reformed <- sapply(
      province_2, 
      function (province_2) gini_by_province(reformed, province = province_2)
    )
    
    table_gini_tinh <- cbind(
      data.frame(gini_tinh_baseline),
      data.frame(gini_tinh_reformed)
    ) %>% 
    tibble::rownames_to_column(var = "tinh") %>% 
      mutate(
        diff = gini_tinh_reformed - gini_tinh_baseline,
        across(where(is.numeric), ~prettyNum(., big.mark =',', digits = 4, scientific = FALSE))
      )
    table_gini_tinh <- formattable(table_gini_tinh %>%
      rename(
        'Tỉnh' = 'tinh',
        'Baseline' = 'gini_tinh_baseline',
        'Reformed' = 'gini_tinh_reformed',
        'Thay đổi' = 'diff'
      ),
      list(
        `Thay đổi` = hi_lo_formatter
      ))
    
    output$market_income <- renderFormattable(market_income)
    output$government_income <- renderFormattable(government_income)
    output$government_expenditure <- renderFormattable(government_expenditure)
    output$compare_overall_poverty <- renderPlot(compare_poverty_plot)
    output$compare_poverty_population <- renderFormattable(poverty_population)
    # output$vietnam_map_baseline <- renderPlot(vietnam_map_base)
    # output$vietnam_map_reformed <- renderPlot(vietnam_map_ref)
    output$vietnam_gini <- renderFormattable(gini_vietnam)
    output$gini_mien <- renderFormattable(mien_gini)
    output$gini_tinh <- renderFormattable(table_gini_tinh)
    
    removeModal()
  })
  
  observeEvent(input$reset, {
    reset("tc_thatnghiep")
    reset("tc_thoiviec")
    reset("luong_bt")
    reset("luong_som") 
    reset("tc_matsuc")
    reset("tc_thuongbinh")
    reset("tc_baotro") 
    reset("tc_thientai")
    reset("db_trongtrot")
    reset("db_channuoi") 
    reset("db_dvnn")
    reset("db_trongrung") 
    reset("db_thuysan")
    reset("t_channuoi")
    reset("t_dvnn") 
    reset("t_lamnghiep")
    reset("t_kdlamnghiep") 
    reset("t_docthan")
    reset("income_tax_1")
    reset("income_tax_2") 
    reset("income_tax_3")
    reset("income_tax_4") 
    reset("income_tax_5")
    reset("income_tax_6") 
    reset("income_tax_7")
    reset("threshold_1")
    reset("threshold_2") 
    reset("threshold_3")
    reset("threshold_4")
    reset("threshold_5")
    reset("threshold_6")
  })
}

shinyApp(ui, server)

