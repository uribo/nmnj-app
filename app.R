library(shiny)
library(bslib)
library(shinyalert, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
df_merge_animalia <- 
    readr::read_csv("data/small_animalia_spname.csv",
                    col_types = "dcc")
df_merge_plantae <- 
    readr::read_csv("data/small_plantae_spname.csv",
                    col_types = "dcc")

opts <- 
    df_merge_animalia %>% 
    dplyr::bind_rows(df_merge_plantae) %>% 
    dplyr::filter(locale == "ja", 
                  stringr::str_detect(name, "^[ァ-ヴー]{1,}$")) %>% 
    dplyr::pull(name) %>% 
    unique()

my_theme <- bs_theme(bootswatch = "lumen",
                     base_font = "sans-serif",
                     heading_font = list(font_google("Sriracha", local = FALSE),
                                      "sans-serif"))

switch_sp_source <- function(kingdom) {
    rlang::arg_match(kingdom,
                     c("Animalia", "Plantae"))
    switch (kingdom,
            "Animalia" = df_merge_animalia,
            "Plantae" = df_merge_plantae)
}

make_locale_df <- function(kingdom, locale = "ja") {
    switch_sp_source(kingdom = kingdom) %>% 
        dplyr::filter(locale == {{ locale }}) %>% 
        dplyr::filter(stringr::str_detect(name, "^[ァ-ヴー]{1,}$")) %>% 
        dplyr::select(id, name)
}

cmn2sci_name <- function(kingdom, name, locale = "ja") {
    df_merge <-
        make_locale_df({{ kingdom }}, locale = {{ locale }})
    id <- 
        df_merge %>% 
        dplyr::filter(name == {{ name }}) %>% 
        dplyr::pull("id")
    if (identical(id, numeric(0))) {
        rlang::inform("条件に一致する和名が見つかりませんでした")
        NA_character_
    } else {
        d <- 
            switch_sp_source(kingdom = {{ kingdom }}) %>% 
            dplyr::filter(id == {{ id }} & locale == "sci")
        if (nrow(d) == 1L) {
            d %>%
                dplyr::pull(name)
        }    
    }
}

cmn2sci_name_animalia <- function(name, locale = "ja") {
    cmn2sci_name(kingdom = "Animalia",
                 name = {{ name }},
                 locale = {{ locale }})
}
cmn2sci_name_plantae <- function(name, locale = "ja") {
    cmn2sci_name(kingdom = "Plantae",
                 name = {{ name }},
                 locale = {{ locale }})
}

spname <- 
    dplyr::bind_rows(
        switch_sp_source(kingdom = "Animalia"),
        switch_sp_source(kingdom = "Plantae"))

ui <- fluidPage(
    useShinyalert(),
    theme = my_theme,
    fluidRow(
        column(width = 12,
               tags$div(class = "titleBox",
                        tags$img(src = "nmnjkun.png", height = 26, width = 24),
                        titlePanel("NANJYA MONJYA")),
               h4("生物種名・分類群に関する情報を整理したWeb API・サービス(仮)"),
               align = "center")
    ),
    tags$br(),
    fluidRow(
        column(width = 12,
            dqshiny::autocomplete_input("name",
                                        label = "学名を調べたい生物種の和名を入力してください",
                                        value = "ニホンアマガエル",
                                        options = opts,
                                        width = 600,
                                        create = TRUE,
                                        max_options = 6),
            radioButtons(inputId = "kingdom",
                         label = "対象の分類群",
                         selected = "動物界",
                         choices = c("動物界", "植物界"),
                         inline = TRUE),
            align = "center"
        )
    ),
    tags$br(),
    fluidRow(
        column(width = 12,
               textOutput("res"),
               align = "center")
    ),
    tags$hr(),
    tags$footer(
        tags$div(class = "navbar-start",
                 tags$nav(
                     actionLink(inputId = "about", "本サービスについて"),
                     tags$a(heref = "", "API"))
                 ),
        tags$div(class = "navbar-end",
                 tags$a(href = "https://twitter.com/u_ribo", icon("twitter")),
                 tags$a(href = "https://github.com/uribo", icon("github")),
                 "Shinya Uryu")
    ),
    includeCSS("www/css/style.css")
)

server <- function(input, output, session) {
    name_res <- reactive({
        rlang::inform(input$name)
        switch (input$kingdom,
                "動物界" = cmn2sci_name_animalia(input$name, locale = "ja"),
                "植物界"= cmn2sci_name_plantae(input$name, locale = "ja"))
    })
    
    output$res <- renderText(name_res())
    observeEvent(input$about, {
        shinyalert(
            html = TRUE,
            text = tagList(
                "生物名の和名を入力することで、対応する学名を出力します。"
            )
        )
    })
}

shinyApp(ui = ui, server = server)
