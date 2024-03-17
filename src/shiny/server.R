shinyServer(function(input, output, session) {
    rv_tenk_items <- reactiveValues(
        data = df_item_defs
    )
    rv_checked_entity_types <- reactiveVal(unname(unlist(entity_type_choices)))
    rv_checked_esg_types <- reactiveVal(unname(unlist(esg_type_choices)))
    rv_iframe_src <- reactiveVal()

    tenk_idx_to_hide <- dt_idx_to_hide(df_tenks, keep = c("display_name", "company_name"))
    tenk_idx_to_hide <- dt_idx_to_hide(df_tenks, keep = c("company_name", "filing_period"))
    output$tenk_table <- renderDT(
        df_tenks,  rownames = FALSE,
        selection = list(mode = "single", selected = c(1)),
        options = list(
            columnDefs = list(
                list(
                    visible = FALSE, targets = tenk_idx_to_hide
                ),
                list(
                    className = "filing_period",
                    targets = dt_idx_col_names(names(df_tenks), "filing_period")
                )
            ),
            autoWidth = FALSE,
            paging = FALSE,
            pageLength = 4,
            info = FALSE, # hide the "showing N of X footer"
            ordering = FALSE
        )
    )

    item_idx_to_hide  <- dt_idx_to_hide(df_item_defs, keep = c("display_label"))
    output$item_table <- {
        renderDT(
            DT::datatable(rv_tenk_items$data,  rownames = FALSE,
            selection = list(mode = "single", selected = c(1)),
            options = list(
              #  search = list(search="der"),
                columnDefs = list(list(
                            visible = FALSE, targets = item_idx_to_hide
                        ), list(
                                className = "display_label",
                                targets = dt_idx_col_names(names(df_item_defs), "display_label")
                        )
            ),
                paging = FALSE,
                info = FALSE, # hide the "showing N of X footer"
                ordering = FALSE,
                dummy_to_avoid_trailing_comma = TRUE
            )
        ) %>%
        formatStyle(
            "display_label",
            valueColumns = "is_available", # disable clickability if is_available col = False
            backgroundColor = styleEqual(FALSE, "gray"),
            color = styleEqual(FALSE, "#5a5959"),
            pointerEvents = styleEqual(FALSE, "none"),
            cursor = styleEqual(FALSE, "no-drop"), #pointer-event seems to override this, oh well
            )
        )
    }

    tenks_curr_idx <- reactive({
        as.integer(input$tenk_table_rows_selected)
    })

    items_curr_idx <- reactive({
        as.integer(input$item_table_rows_selected)
    })

    # output$selected_tenk = renderPrint({
    #     req(tenks_curr_idx())
    #     selected_file <- file.path("tenk", tenks[tenks_curr_idx()])
    #     selected_item <- rv_tenk_items$data$item_id[[items_curr_idx()]]
    #     cat(glue("{selected_file}#{selected_item}\n\n"))
    # })

    observeEvent(input$edgaug_btn, {
      showModal(modalDialog(
        title = "Singularity of EDGAUGMENTOR",
        size = "l",
        easyClose = TRUE,
        htmltools::includeMarkdown("./markdown/README.md")
      ))
    })

    observeEvent(input$breaker, {
        cat(rep("-", 55))
        print("")
    })

    output$tenk_summary <- renderUI({
        req(tenks_curr_idx())
        tenk_row <- df_tenks[tenks_curr_idx(), ]
        # TODO: shift this whole bit to the df_tenks itself, i.e. for every row
        parts <- stringr::str_split(tenk_row[["display_name"]], "-")
        tickers_text <- str_replace_all(parts[[1]][1], "_", ", ")
        filing_date <- readr::parse_date(parts[[1]][2], format = "%Y%m%d")
        company_name <- tenk_row[["company_name"]]
        filing_period <- tenk_row[["filing_period"]]
        panel_header <- glue(">> {company_name},   filing period: {filing_period}")
        filing_url <- tags$a("sec.gov", href = tenk_row[["html_filing_summary_url"]],
                                target = "_blank", rel = "noopener noreferrer")
        missing_items <- tenk_row[["missing_items"]][[1]]
        missing_items <- if (rlang::is_empty(missing_items)) "∅" else missing_items
        address_table <- tags$table(class = "expando_address",
            tags$tr(tags$th("Mailing Address"), tags$th("Business Address")),
            tags$tr(
                tags$td(tags$pre(tenk_row[["mailing_address"]])),
                tags$td(tags$pre(tenk_row[["business_address"]])),
            )
        )

        panel_content <- tags$div(id = "tenk_summary_expando",
            tags$div(id = "tenk_summary_expando_list",
            tags$ul(
                tags$li(glue("ticker(s): {tickers_text}")),
                tags$li(tags$span("orig filing url @ "), tags$span(filing_url)),
                tags$li(glue("Items not detected: {glue_collapse(missing_items, sep=', ')}")),
                tags$li(glue("filing date: {filing_date}"))
            )), tags$div(address_table)
        )

        bsCollapse(
            bsCollapsePanel(panel_header, panel_content, style = "info")
        )
    })

    observeEvent(input$item_display_choice, {
        req(tenks_curr_idx())
        #shinyjs::toggle("ner_element_group")
        show_marked <- if (input$item_display_choice == "marked") TRUE else FALSE
        show_query <- if (input$item_display_choice == "query_llm") TRUE else FALSE
        handle_element_showage(rv_iframe_src(), show_marked = show_marked, show_query = show_query)
    })

    output$entity_type_defs_table <- renderDT(
        DT::datatable(df_entity_defs,
            rownames = FALSE,
            selection = "none",
            options = list(
                paging = FALSE,
                info = FALSE,
                searching = FALSE
            )
        )
    )

    output$esg_type_defs_table <- renderDT(
        DT::datatable(df_esg_defs,
            rownames = FALSE,
            selection = "none",
            options = list(
                paging = FALSE,
                info = FALSE,
                searching = FALSE
            )
        )
    )

    observeEvent(input$select_all_ent_types, {
        updateCheckboxGroupInput(session, "checkGroupEntities", NULL, choices = entity_type_choices, inline = TRUE,
                                 selected = unname(unlist(entity_type_choices)))
    })
    observeEvent(input$select_none_ent_types, {
        updateCheckboxGroupInput(session, "checkGroupEntities", NULL, choices = entity_type_choices, inline = TRUE,
                                 selected = NULL)
    })
    observeEvent(input$select_all_esg_types, {
        updateCheckboxGroupInput(session, "checkGroupEsg", NULL, choices = esg_type_choices, inline = TRUE,
                                 selected = unname(unlist(esg_type_choices)))
    })
    observeEvent(input$select_none_esg_types, {
        updateCheckboxGroupInput(session, "checkGroupEsg", NULL, choices = esg_type_choices, inline = TRUE,
                                 selected = NULL)
    })

    output$llm_response <- renderUI({
        response <- get_llm_query_response()
        return(response)
    }) %>% bindEvent(input$send_query)

    update_highlight <- Vectorize(function(data_value, data_type, num_changes = 0) {
        js$updateHighlight(data_value = data_value,
                            data_type = data_type,
                            num_changes = num_changes,
                            run_count = 1)
    }, vectorize.args = "data_value")

    observeEvent(c(input$item_table_rows_selected,
                    input$checkGroupEntities,
                    input$checkGroupEsg,
                    input$item_display_choice), {
        if (input$item_display_choice == "marked" && str_detect(rv_iframe_src(), "_marked")) {

            if (str_detect(rv_iframe_src(), "_item7_")) {
                available_choices <- entity_type_choices
                check_group <- input$checkGroupEntities
                rv_checked_types <- rv_checked_entity_types
                data_type <- "entity"
            } else if (str_detect(rv_iframe_src(), "_item1_")) {
                available_choices <- esg_type_choices
                check_group <- input$checkGroupEsg
                rv_checked_types <- rv_checked_esg_types
                data_type <- "esg"
            }
            unchecked_data_types <- setdiff(available_choices, check_group)
            # print(glue("unchecked_data_types: {glue_collapse(unchecked_data_types, sep=',')}"))
            newly_unchecked_data_types <- setdiff(rv_checked_types(), check_group)
            static_unchecked_data_types <- setdiff(unchecked_data_types, newly_unchecked_data_types)
            print(glue("static_unchecked_data_types: {glue_collapse(static_unchecked_data_types, sep=',')}"))
            print(glue("newly_UNchecked_data_types: {glue_collapse(newly_unchecked_data_types, sep=',')}"))
            newly_checked_data_types <- setdiff(check_group, rv_checked_types())
            print(glue("newly_checked_data_types: {glue_collapse(newly_checked_data_types, sep=',')}"))
            # Joe Cheng comment re onFlushed @ https://github.com/rstudio/shiny/issues/3348
            session$onFlushed(function() {
                static_unchecked_data_types %>% update_highlight(data_type)
                newly_unchecked_data_types %>% update_highlight(data_type, length(newly_unchecked_data_types) * -1)
                newly_checked_data_types %>% update_highlight(data_type, length(newly_checked_data_types))
                js$breaker()
            })
            rv_checked_types(check_group)
         }
    })

    observe({
        showNotification(glue("{input$updateHighlightMsg}"), type = "message", duration = 3, id = "highlightMsg")
    })
    
    handle_element_showage <- function(iframe_src, show_marked, show_query) {
        if (show_marked) {
            if (str_detect(iframe_src, "_item1_")) {
                shinyjs::hide("query_llm_group")
                shinyjs::hide("ner_element_group")
                shinyjs::show("esg_element_group")
            } else {
                shinyjs::hide("query_llm_group")
                shinyjs::hide("esg_element_group")
                shinyjs::show("ner_element_group")
            }
        } else if (show_query) {
            shinyjs::hide("ner_element_group")
            shinyjs::hide("esg_element_group")
            shinyjs::show("query_llm_group")
        } else {
            shinyjs::hide("ner_element_group")
            shinyjs::hide("esg_element_group")
            shinyjs::hide("query_llm_group")
        }
    }

    get_llm_query_response <- function(){
        # assuming "orig" rv_iframe_src, e.g. 10K_ASFI-20191220_aug.html#edgaug_item7_id -> 10K_ASFI-20191220_7
        groups <- stringr::str_match(rv_iframe_src(), "^tenk\\/(10K_.*_)aug.html#edgaug_item(\\d)_id")
        pinecone_namespace <- glue("{groups[2]}{groups[3]}")

        dummy_response <- markdown(glue("
            ## query to send over to namespace = {pinecone_namespace}:
            > {input$query_text}
        "))
        return(dummy_response)
    }

    output$showfile <- renderUI({
        req(tenks_curr_idx())
        req(items_curr_idx()) # (uncommenting 2022.01.05) if live this breaks replaceData/showing the tenk
        selected_file <- file.path("tenk", tenks[tenks_curr_idx()])
        selected_item <- rv_tenk_items$data$item_id[[items_curr_idx()]]
        iframe_src <- glue("{selected_file}#edgaug_{selected_item}_id")
        print(glue("iframe_src: {iframe_src}"))

        # if there is a _marked item, provide opportunity to display that one
        item_file_name <- stringr::str_replace(tenks[tenks_curr_idx()],
                                               "(_pretty)?_aug\\.",
                                               glue("_{selected_item}_marked."))
        item_file_src <- file.path("item", item_file_name)
        item_full_path <- file.path(getwd(), "www", item_file_src)

        if (file.exists(item_full_path)) {
            shinyjs::disable(selector = ".radio-inline:has(input[value='summary'])" )
            default_summary_file_name = stringr::str_replace(item_file_name, "_marked.html", "_default.txt")
            snarky_summary_file_name = stringr::str_replace(item_file_name, "_marked.html", "_snarky.txt")
            item_summary_default_src  <- file.path("item_summary", default_summary_file_name)
            item_summary_snarky_src  <- file.path("item_summary", snarky_summary_file_name)

            item_summ_default_full_path <- file.path(getwd(), "www", item_summary_default_src)
            item_summ_snarky_full_path <- file.path(getwd(), "www", item_summary_snarky_src)
            must_change_seleced <- FALSE
            if (file.exists(item_summ_default_full_path)) {
                shinyjs::enable(selector = ".radio-inline:has(input[value='summary'])" )
            } else {
                shinyjs::disable(selector = ".radio-inline:has(input[value='summary'])" )
                if (input$item_display_choice == "summary") must_change_seleced <- TRUE
            }
            if (file.exists(item_summ_snarky_full_path)) {
                shinyjs::enable(selector = ".radio-inline:has(input[value='snarky'])" )
            } else {
                shinyjs::disable(selector = ".radio-inline:has(input[value='snarky'])" )
                if (input$item_display_choice == "snarky") must_change_seleced <- TRUE
            }
            if (must_change_seleced) {
                updateRadioButtons(session, "item_display_choice", selected="orig")
            }

          shinyjs::show("item_text_type_group")
          if (input$item_display_choice == "marked") {
                handle_element_showage(item_file_src, show_marked = TRUE, show_query = FALSE)
                iframe_src <- item_file_src
            } else if (input$item_display_choice == "query_llm") {
                handle_element_showage(item_file_src, show_marked = FALSE, show_query = TRUE)
            } else {
                handle_element_showage(item_file_src, show_marked = FALSE, show_query = FALSE)
                if(input$item_display_choice == "orig") {
                    # anything to do?
                } else if (input$item_display_choice == "summary") {
                    iframe_src <- item_summary_default_src

                } else if (input$item_display_choice == "snarky") {
                    iframe_src <- item_summary_snarky_src
                }
            }
        } else {
            shinyjs::hide("item_text_type_group")            
            handle_element_showage(item_file_src, show_marked = FALSE, show_query = FALSE)
        }

        out <- tags$iframe(
            id = "tenkFrame",
            class = "iffy",
            src = iframe_src
        #   src = "/item/AEY-20191217_item7_marked.html"
        )
        rv_iframe_src(iframe_src)
        return(out)
    })

    ## option 1 is proxy + replaceData = no flicker, no items selected on tenk change
    proxy <- dataTableProxy("item_table")
    observeEvent(input$tenk_table_rows_selected, {
        tenk_row <- df_tenks[tenks_curr_idx(), ]
        missing_items <- tenk_row[["missing_items"]][[1]]
        if (!rlang::is_empty(missing_items)) {
            missing_items <- glue("Item {toupper(missing_items)}")
            print(glue("missing items: {glue_collapse(missing_items, sep=', ')}"))
        }
        display_items <- rv_tenk_items$data %>%
            mutate(is_available = ifelse(item_name %in% missing_items, FALSE, TRUE))
        # clearSelection = "none" on replaceData() to keep curr selection
        replaceData(proxy, display_items, rownames = FALSE)
        # if there is a search filter, select min(displayed-item), else select Item 1
        item_selected_idx <- if (is.null(input$item_table_rows_current)) 1 else min(input$item_table_rows_current)
        selectRows(proxy, selected = item_selected_idx)
        ## option 2 are the two below, data reloads each w/flicker, whatever is default in DT def is selected
        ##   - later, prob want to show only those items that are present, so maybe load anew anyway?
        #rv_tenk_items$data <- NULL
        #rv_tenk_items$data <- display_items
    }, ignoreInit = TRUE
  )
})
