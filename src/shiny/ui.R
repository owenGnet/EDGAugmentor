ui <- fluidPage(
 shinyjs::useShinyjs(),
  extendShinyjs(script = "script.js", functions = c("alerta", "updateHighlight", "breaker")),
  dashboardPage(
    dashboardHeader(title = tags$button(id = "edgaug_btn", class = "btn action-button",
      tags$img(id = "edgaug_logo", src = "EdgaugBtn2.png"))),
    dashboardSidebar(
      sidebarMenu(
        tags$div(class = "tenk_parent",
            DTOutput("tenk_table") #, height="25vh" ) # can set height in css as long as !important
        ),
        tags$hr(),
        uiOutput("itemLinks"),
        tags$div(class = "tenk_items",
          DTOutput("item_table")
        )
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "displacy.css"),
        tags$link(rel = "shortcut icon", href = "http://www.oweng.net/EDGAugmentor/favicon.ico")
      ),
      fluidRow(
        column(12, h4(span("NOTE: augmented text available for:"), 
          span("id" = "item1_aug_note", class = "item_note", "Item 1"), span(" + "),
          span("id" = "item7_aug_note", class = "item_note", "Item 7"), span(", ChatGPT summary text in different "),
          span("id" = "item_flavors_note", class = "item_note", "flavors"), span(" for Item 1"))
      ),
        bsTooltip(id = "item1_aug_note", title = "ESG keyword highlighting", placement = "top", trigger = "hover",
                  options = NULL),
        bsTooltip(id = "item7_aug_note", title = "named entity highlighting", placement = "top", trigger = "hover",
                  options = NULL),
        bsTooltip(id = "item_flavors_note", title = "choice of default or snarky", placement = "top", trigger = "hover",
                  options = NULL)
        # debug: column(9, verbatimTextOutput("selected_tenk")),
        # column(3, actionButton("breaker", "add breaker") )
      ),

    fluidRow(
         htmlOutput("tenk_summary")
    ),
    fluidRow(
        shinyjs::hidden(
            tags$div(id = "item_text_type_group",
             radioButtons("item_display_choice", "Section text",
                          c("original" = "orig",
                            "marked terms" = "marked",
                            "summary" = "summary",
                            "snarky summary" = "snarky"),
                            inline=TRUE),
           ),
            tags$div(id = "ner_element_group",
                bsCollapse(id = "entity_type_summary",
                    bsCollapsePanel(">> Possible entity types", DTOutput("entity_type_defs_table"),
                    style = NULL
                )),
                tags$div(id = "btns_select_ent_types",
                    actionButton("select_all_ent_types", "Select All", class = "btn-control-checkboxes"),
                    actionButton("select_none_ent_types", "Select None", class = "btn-control-checkboxes")
                ),
                checkboxGroupInput("checkGroupEntities",
                    label = NULL, #"HIDDEN, via id=checkGroupEntities-label",
                    choices = entity_type_choices,
                    inline = TRUE,
                    selected = unname(unlist(entity_type_choices))
                )
            ),
            tags$div(id = "esg_element_group",
                bsCollapse(id = "esg_type_summary",
                    bsCollapsePanel(">> ESG keyword list", DTOutput("esg_type_defs_table"),
                    style = NULL
                )),
                tags$div(id = "btns_select_esg_types",
                    actionButton("select_all_esg_types", "Select All", class = "btn-control-checkboxes"),
                    actionButton("select_none_esg_types", "Select None", class = "btn-control-checkboxes")
                ),
                checkboxGroupInput("checkGroupEsg",
                    label = NULL,
                    choices = esg_type_choices,
                    inline = TRUE,
                    selected = unname(unlist(esg_type_choices))
                )
            )
        )
    ),
    fluidRow(
        htmlOutput("showfile", inline = TRUE)
    )
   )
  )
)
