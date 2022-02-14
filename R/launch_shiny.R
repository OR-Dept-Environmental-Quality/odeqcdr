#' Launch Shiny app for further review
#'
#' A shiny application to review data
#'
#' The data must first be formatted as a named list and saved as an .RData file.
#'
#' The object must be named 'shiny_list' with the named listed elements as follows:
#' \itemize{
#'   \item 'Deployment'
#'   \item 'Audit_Stats'
#'   \item 'Results_Anom'
#'   }
#'
#' @examples
#'
#' # list to export to Shiny
#' shiny_list <-list(Deployment=df1.deployment,
#'                  Audit_Stats=df3.audits.dql,
#'                  Results_Anom=df5.results.anom)
#'
#' save(shiny_list, file=paste0(gsub(".xlsx","",xlsx_output),"_CDR.Rdata"))
#'
#' # Launch Shiny app for further review.
#' odeqcdr::launch_shiny()
#'
#' @export
#' @return Launches a Shiny app

launch_shiny <- function(){

  options(shiny.maxRequestSize=40*1024^2)

  app <- shiny::shinyApp(

    ui = shiny::shinyUI(

      shiny::navbarPage(title = "Continuous Data Check", fluid=TRUE,
                        shiny::tabPanel("Choose RData",
                                        shiny::fluidPage(shiny::fluidRow(shiny::column(width=10,
                                                                                       shiny::fileInput(inputId="rdata",
                                                                                                        label="Choose RData File", multiple = FALSE, width='100%',
                                                                                                        accept=c(".RData",".Rda"), buttonLabel="Select Data"))))),
                        shiny::tabPanel("Plot",
                                        shiny::fluidPage(shiny::fluidRow(shiny::column(width=8, shiny::uiOutput("selectDeployment")),
                                                                         shiny::column(width=2, align = "left",
                                                                                       shiny::actionButton(inputId="PREVIOUS", label="Previous", style = "margin-top: 25px;"),
                                                                                       shiny::actionButton(inputId="NEXT", label="Next", style = "margin-top: 25px;"))),
                                                         shiny::fluidRow(shiny::column(width=6, align = "left",
                                                                                       shiny::checkboxGroupInput("HLcolumns", "Highlight Results",
                                                                                                                 choices= c("dt_shift",
                                                                                                                            "daily_min_q10",
                                                                                                                            "daily_max_q90",
                                                                                                                            "daily_mean_q10",
                                                                                                                            "daily_mean_q90",
                                                                                                                            "Rejected"), inline = TRUE)),
                                                                         shiny::column(width=4, align = "left",
                                                                                       shiny::numericInput(inputId="dph", label = "Delta per hour >=", width= "50%", min = 0, max=1000, value=5, step=0.5))),
                                                         shiny::fluidRow(shiny::column(width=10, shiny::plotOutput('plot',
                                                                                                                   click = "plot_click",
                                                                                                                   dblclick = "plot_dblclick",
                                                                                                                   brush = shiny::brushOpts(id = "plot_brush",
                                                                                                                                            resetOnNew = TRUE)))),
                                                         shiny::fluidRow(shiny::column(width=1, align = "right", shiny::h6("Table Rows")),
                                                                         shiny::column(width=9, align = "left", shiny::verbatimTextOutput("SELECTprintout", placeholder=TRUE))),
                                                         shiny::fluidRow(shiny::column(width=10, DT::dataTableOutput("results_dt"))),
                                        )),
                        shiny::tabPanel("Audit Data", DT::dataTableOutput("audit_dt"))
      )),

    server = shiny::shinyServer(function(input, output, session) {

      output$selectDeployment <- shiny::renderUI({

        deploy_choices <- NULL

        # named shiny_list
        load(input$rdata$datapath)

        deploy_choices  <- shiny_list[["Deployment"]] %>%
          dplyr::mutate(Deployment=paste(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, sep = " - ")) %>%
          dplyr::pull(Deployment) %>%
          unique()

        shiny::selectInput("selectDeployment", label="Select Deployment",
                           choices = deploy_choices,
                           width='100%',
                           selectize=TRUE)

      })

      deploy_data_reactive <- shiny::reactive({

        if(!is.null(input$rdata$datapath)) {

          load(input$rdata$datapath)

          df.deploy <- shiny_list[["Deployment"]] %>%
            dplyr::mutate(Deployment=paste(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, sep = " - "))

          df.deploy

        } else {
          NULL
        }

      })

      deploy_select_reactive <- shiny::reactive({
        # named shiny_list

        df.deploy <- deploy_data_reactive() %>%
          dplyr::filter(Deployment==input$selectDeployment)

        df.deploy
      })

      audit_data_reactive <- shiny::reactive({

        # named shiny_list
        load(input$rdata$datapath)

        df <-  shiny_list[["Audit_Stats"]] %>%
          dplyr::mutate(Deployment=paste(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, sep = " - ")) %>%
          dplyr::filter(Deployment==input$selectDeployment) %>%
          dplyr::select(-Deployment)


        # These if statements update col names in older audit dataframes
        # developed with odeqcdr < v 0.10.

        if("DQL_prec" %in% names(df)) {

          df <- dplyr::rename(df, precDQL=DQL_prec)
        }

        if("datetime" %in% names(df)) {
          # rename result datetime

          df <- dplyr::rename(df, Logger.datetime=datetime)
        }

        if(!"rDQL" %in% names(df)) {
          # Add rDQL

          df <- df %>%
            dplyr::mutate(rDQL=precDQL)
        }

        if("Audit.Result.Status.ID" %in% names(df)){

          df <- df %>%
            dplyr::mutate(Result.Status.ID=Audit.Result.Status.ID) %>%
            dplyr::select(-Audit.Result.Status.ID)
        }

        # This if statement updates col names in audit data frames
        # developed with odeqcdr > version 0.14.

        if("Logger.Result.Value" %in% names(df) & "Result.Value" %in% names(df)){

          df <- dplyr::rename(df, Audit.Result.Value = Result.Value)
        }

        # These if statements updates col names in audit data frames
        # developed with odeqcdr version 0.10 - version 0.14.

        if("Result.datetime" %in% names(df)){

          df <- dplyr::rename(df, Logger.datetime = Result.datetime)
        }

        if("Result.Value" %in% names(df)){

          df <- dplyr::rename(df, Logger.Result.Value = Result.Value)
        }

        df

      })

      result_data_reactive <- shiny::reactive({

        # named shiny_list
        load(input$rdata$datapath)

        df.results.anom <- shiny_list[["Results_Anom"]] %>%
          dplyr::mutate(Deployment=paste(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, sep = " - ")) %>%
          dplyr::filter(Deployment==input$selectDeployment) %>%
          dplyr::select(-Deployment)

        df.results.anom

      })

      result_dt <- shiny::reactiveValues(dt=NULL)

      output$results_dt <- DT::renderDT({

        dt <- result_data_reactive() %>%
          dplyr::rename(Row=row.results) %>%
          dplyr::select(datetime, Result.Value, Anomaly, dt_shift, delta_per_hour,
                        daily_min_q10,
                        daily_max_q90,
                        daily_mean_q10,
                        daily_mean_q90,
                        rDQL,
                        Result.Status.ID,
                        Result.Comment,
                        Row) %>%
          dplyr::mutate(datetime=format(datetime, format="%Y-%m-%d %H:%M %Z"),
                        Anomaly=factor(as.character(Anomaly)),
                        dt_shift=factor(as.character(dt_shift)),
                        daily_min_q10=factor(as.character(daily_min_q10)),
                        daily_max_q90=factor(as.character(daily_max_q90)),
                        daily_mean_q10=factor(as.character(daily_mean_q10)),
                        daily_mean_q90=factor(as.character(daily_mean_q90)),
                        rDQL=factor(rDQL),
                        Result.Status.ID=factor(Result.Status.ID),
                        Result.Comment=factor(Result.Comment))

        DT::datatable(data=shiny::isolate(dt), selection = "multiple",
                      options = list(searching=FALSE),
                      filter = "top",
                      class = "nowrap cell-border hover stripe",
                      rownames = FALSE) %>%
          DT::formatStyle(columns=c(2:14), `text-align`="right")

      })

      output$audit_dt <- DT::renderDT({

        df <- audit_data_reactive()

        dt <- df %>%
          dplyr::select(Monitoring.Location.ID, audit.datetime.start, Audit.Result.Value, Logger.datetime,
                        Logger.Result.Value, diff.minutes, diff.Result, precDQL, rDQL, Result.Status.ID,
                        Row=row.audits) %>%
          dplyr::mutate(audit.datetime.start=format(audit.datetime.start, format="%Y-%m-%d %H:%M %Z"),
                        Logger.datetime=format(Logger.datetime, format="%Y-%m-%d %H:%M %Z"))

        DT::datatable(data=shiny::isolate(dt), selection = "none",
                      options = list(lengthChange=FALSE, searching=FALSE, searchable = FALSE),
                      class = "nowrap cell-border hover stripe",
                      rownames = FALSE)

      })

      shiny::observeEvent(input$selectDeployment, {
        # When a new deployment is selected, update the row printout


        rows_selected <- result_data_reactive() %>%
          dplyr::rename(Row=row.results) %>%
          dplyr::mutate(Row.dt=dplyr::row_number())

        result_dt$dt <- rows_selected %>%
          dplyr::select(Row, Row.dt)

        output$SELECTprintout <- shiny::renderPrint({

          if(nrow(rows_selected)>0){

            rows_selected2 <- rows_selected %>%
              dplyr::arrange(Row) %>%
              dplyr::pull(Row)

            rconseq <- split(rows_selected2, cumsum(c(0, diff(rows_selected2) > 1)))

            # find the range of each sequence
            rrange <- lapply(rconseq, FUN=range)

            # collapse the range into start:stop format and unlist to a single string
            cat(paste0("[",input$selectDeployment,"]: ", paste(unlist(lapply(rrange, FUN=function(x) {paste(unique(x), collapse = ":")})), collapse = ", ")))

          } else {cat("None")}
        })

      })

      shiny::observeEvent(input$plot_brush, {
        # If there's a brush on the plot set the
        # table to the rows in the brush area, and summarize the selected rows
        # in the printout

        pb <- input$plot_brush

        proxy1 <- DT::dataTableProxy("results_dt")

        result_data <- result_data_reactive() %>%
          dplyr::rename(Row=row.results) %>%
          dplyr::select(datetime, Result.Value, Anomaly, dt_shift, delta_per_hour,
                        daily_min_q10,
                        daily_max_q90,
                        daily_mean_q10,
                        daily_mean_q90,
                        rDQL,
                        Result.Status.ID,
                        Result.Comment,
                        Row)

        rows_selected <- shiny::brushedPoints(result_data, pb,
                                              xvar = "datetime", yvar = "Result.Value") %>%
          dplyr::mutate(datetime=format(datetime, format="%Y-%m-%d %H:%M %Z"),
                        Anomaly=as.character(Anomaly),
                        dt_shift=as.character(dt_shift),
                        daily_min_q10=as.character(daily_min_q10),
                        daily_max_q90=as.character(daily_max_q90),
                        daily_mean_q10=as.character(daily_mean_q10),
                        daily_mean_q90=as.character(daily_mean_q90))

        result_dt$dt <- rows_selected %>%
          dplyr::mutate(Row.dt=dplyr::row_number()) %>%
          dplyr::select(Row, Row.dt)

        DT::replaceData(proxy=proxy1, data=rows_selected, rownames = FALSE)

        output$SELECTprintout <- shiny::renderPrint({

          if(nrow(rows_selected)>0){

            rows_selected2 <- rows_selected %>%
              dplyr::arrange(Row) %>%
              dplyr::pull(Row)

            rconseq <- split(rows_selected2, cumsum(c(0, diff(rows_selected2) > 1)))

            # find the range of each sequence
            rrange <- lapply(rconseq, FUN=range)

            # collapse the range into start:stop format and unlist to a single string
            cat(paste0("[",input$selectDeployment,"]: ", paste(unlist(lapply(rrange, FUN=function(x) {paste(unique(x), collapse = ":")})), collapse = ", ")))

          } else {cat("None")}
        })


      })

      shiny::observeEvent(input$plot_dblclick, {
        # When a double-click happens, check if there's a brush on the plot.
        # If so, zoom to the brush bounds; if not, reset the zoom and table

        proxy1 <- DT::dataTableProxy("results_dt")

        shiny::isolate({

          brush <- input$plot_brush
          if (!is.null(brush)) {
            ranges$x <- c(as.POSIXct(brush$xmin, origin = "1970-01-01"),
                          as.POSIXct(brush$xmax, origin = "1970-01-01"))
            ranges$y <- c(brush$ymin, brush$ymax)
          } else {
            ranges$x <- NULL
            ranges$y <- NULL

            dt <- result_data_reactive() %>%
              dplyr::rename(Row=row.results) %>%
              dplyr::select(datetime, Result.Value, Anomaly, dt_shift, delta_per_hour,
                            daily_min_q10,
                            daily_max_q90,
                            daily_mean_q10,
                            daily_mean_q90,
                            rDQL,
                            Result.Status.ID,
                            Result.Comment,
                            Row) %>%
              dplyr::mutate(datetime=format(datetime, format="%Y-%m-%d %H:%M %Z"),
                            Anomaly=as.character(Anomaly),
                            dt_shift=as.character(dt_shift),
                            daily_min_q10=as.character(daily_min_q10),
                            daily_max_q90=as.character(daily_max_q90),
                            daily_mean_q10=as.character(daily_mean_q10),
                            daily_mean_q90=as.character(daily_mean_q90))

            result_dt$dt <- dt %>%
              dplyr::mutate(Row.dt=dplyr::row_number()) %>%
              dplyr::select(Row, Row.dt)

            DT::replaceData(proxy=proxy1, data=dt, rownames = FALSE)

            output$SELECTprintout <- shiny::renderPrint({

              if(nrow(dt)>0){

                rows_selected2 <- dt %>%
                  dplyr::arrange(Row) %>%
                  dplyr::pull(Row)

                rconseq <- split(rows_selected2, cumsum(c(0, diff(rows_selected2) > 1)))

                # find the range of each sequence
                rrange <- lapply(rconseq, FUN=range)

                # collapse the range into start:stop format and unlist to a single string
                cat(paste0("[",input$selectDeployment,"]: ", paste(unlist(lapply(rrange, FUN=function(x) {paste(unique(x), collapse = ":")})), collapse = ", ")))

              } else {cat("None")}
            })



          }
        })

      })

      shiny::observeEvent(input$plot_click, {
        # If there's a click on the plot set the
        # table to nearby rows and summarize the selected rows
        # in the printout

        proxy1 <- DT::dataTableProxy("results_dt")

        result_data <- result_data_reactive() %>%
          dplyr::rename(Row=row.results) %>%
          dplyr::select(datetime, Result.Value, Anomaly, dt_shift,
                        delta_per_hour,
                        daily_min_q10,
                        daily_max_q90,
                        daily_mean_q10,
                        daily_mean_q90,
                        rDQL,
                        Result.Status.ID,
                        Result.Comment,
                        Row)

        rows_clicked <- shiny::nearPoints(result_data, input$plot_click, allRows = FALSE) %>%
          dplyr::mutate(datetime=format(datetime, format="%Y-%m-%d %H:%M %Z"),
                        Anomaly=as.character(Anomaly),
                        dt_shift=as.character(dt_shift),
                        daily_min_q10=as.character(daily_min_q10),
                        daily_max_q90=as.character(daily_max_q90),
                        daily_mean_q10=as.character(daily_mean_q10),
                        daily_mean_q90=as.character(daily_mean_q90))

        result_dt$dt <- rows_clicked %>%
          dplyr::mutate(Row.dt=dplyr::row_number()) %>%
          dplyr::select(Row, Row.dt)

        output$SELECTprintout <- shiny::renderPrint({

          if(nrow(rows_clicked)>0){

            rows_clicked2 <- rows_clicked %>%
              dplyr::arrange(Row) %>%
              dplyr::pull(Row)

            rconseq <- split(rows_clicked2, cumsum(c(0, diff(rows_clicked2) > 1)))

            # find the range of each sequence
            rrange <- lapply(rconseq, FUN=range)

            # collapse the range into start:stop format and unlist to a single string
            cat(paste0("[",input$selectDeployment,"]: ", paste((lapply(rrange, FUN=function(x) {paste(unique(x), collapse = ":")})), collapse = ", ")))

          } else {
            cat("None")}

        })

        DT::replaceData(proxy=proxy1, data=rows_clicked, rownames = FALSE)

      })

      shiny::observeEvent(input$PREVIOUS, {
        # When the previous button is clicked, go to the previous deployment

        deploy <- deploy_data_reactive() %>%
          dplyr::mutate(row=dplyr::row_number())

        prev.row <- grep(input$selectDeployment, deploy$Deployment, fixed=TRUE) -1

        if(prev.row <=0) {prev.row <-1}

        prev.choice <- deploy %>%
          dplyr::filter(row==prev.row) %>%
          dplyr::pull(Deployment)

        shiny::updateSelectizeInput(
          session = session,
          inputId = "selectDeployment",
          selected = prev.choice
        )

      })

      shiny::observeEvent(input$NEXT, {
        # When the next button is clicked, advance to next deployment

        deploy <- deploy_data_reactive() %>%
          dplyr::mutate(row=dplyr::row_number())

        next.row <- grep(input$selectDeployment, deploy$Deployment, fixed=TRUE) + 1

        next.choice <- deploy %>%
          dplyr::filter(row==next.row) %>%
          dplyr::pull(Deployment)

        shiny::updateSelectizeInput(
          session = session,
          inputId = "selectDeployment",
          selected = next.choice
        )

      })

      ranges <- shiny::reactiveValues(x = NULL, y = NULL)

      output$plot <- shiny::renderPlot({

        result_data <- result_data_reactive()

        if(is.null(input$results_dt_rows_selected)) {

          tselect <- c(NA)

          } else {

          tselect <- result_dt$dt %>%
            dplyr::filter(Row.dt %in% input$results_dt_rows_selected) %>%
            dplyr::pull(Row)
        }

        if("delta_per_hour" %in% names(result_data)) {

        result_data <- result_data %>%
          mutate(dph=dplyr::if_else(delta_per_hour >= input$dph, TRUE, FALSE),
                 Rejected=dplyr::if_else(Result.Status.ID=="Rejected", TRUE, FALSE),
                 tselect=dplyr::if_else(row.results %in% tselect,TRUE, FALSE))

        } else {

          result_data <- result_data %>%
            mutate(delta_per_hour=NA,
                   dph=NA,
                   Rejected=dplyr::if_else(Result.Status.ID=="Rejected", TRUE, FALSE),
                   tselect=dplyr::if_else(row.results %in% tselect,TRUE, FALSE))

        }

        if(!is.null(input$HLcolumns)) {

          result_data$hl_row <-rowSums(result_data[, c(input$HLcolumns, "dph", "tselect")], na.rm = TRUE) > 0
        } else {
          result_data$hl_row <-rowSums(result_data[, c("dph", "tselect")], na.rm = TRUE) > 0
        }

        p <- ggplot2::ggplot(data=result_data) +
          ggplot2::geom_point(ggplot2::aes(x=datetime, y=Result.Value, color=rDQL, shape=Anomaly), size = 3) +
          ggplot2::scale_shape_manual(name ="Anomaly", values=c('TRUE' = 4, 'FALSE' = 16, 'NA'=16)) +
          ggplot2::scale_color_manual(name ="Result DQL", values = c("A"="black","B"="#FF9B4C","C"="#7277C1","E"="#998B6B","NA"="#C0C0C0","D"="#0528a8")) +
          ggplot2::coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
          ggplot2::theme_linedraw() +
          ggplot2::theme(legend.position="right",
                         #legend.direction = "horizontal", legend.box = "horizontal",
                         panel.grid.major = ggplot2::element_line(color = "gray"),
                         panel.grid.minor = ggplot2::element_line(color = "gray"))

        hl_rows <- dplyr::filter(result_data, hl_row)

        if(nrow(hl_rows) > 0) {

          p <- p +
            ggplot2::geom_point(data = hl_rows, ggplot2::aes(x=datetime, y=Result.Value),
                                color = 'yellow', size = 4)
        }

        audit_data <- audit_data_reactive()
        p <- p +
          ggplot2::geom_vline(xintercept = as.numeric(audit_data[,'audit.datetime.start']), color= 'green', size=1) +
          ggplot2::geom_point(data = audit_data, ggplot2::aes(x = audit.datetime.start, y = Audit.Result.Value),
                              color = 'green', size = 4)

        deploy_data <- deploy_select_reactive()
        p <- p +
          ggplot2::geom_vline(xintercept = as.numeric(deploy_data[,'Deployment.Start.Date']), color= 'black', size=1.5) +
          ggplot2::geom_vline(xintercept = as.numeric(deploy_data[,'Deployment.End.Date']), color= 'black', size=1.5)

        p
      })

      session$onSessionEnded(function() {
        shiny::stopApp()
      })

    })
  )
  shiny::runApp(app, launch.browser = TRUE)
}
