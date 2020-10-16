#' Launch Shiny app for further review
#'
#' A shiny application to review data
#'
#' The data must first be formatted as a named list and saved as an RData file.
#'
#' The object must be named 'shiny_list' with the named listed elements as follows:
#'   'Deployment'
#'   'Audit_Stats'
#'   'Results_Anom'
#'
#' Example code:
#'
#' shiny_list <-list(Deployment=df1.deployment,
#'                   Audit_Stats=df3.audits.dql,
#'                   Results_Anom=df5.results.anom)
#'
#' save(shiny_list, file=paste0(CDR_template_for_shiny_review.Rdata"))
#'
#' @export
#' @return Launches a Shiny app

launch_shiny <- function(){
  app <- shiny::shinyApp(

    ui = shiny::shinyUI(
      shiny::fluidPage(
        shiny::verticalLayout(
          shiny::titlePanel("Continuous Data Check"),
          shiny::wellPanel(shiny::fileInput(inputId="rdata", label="Choose RData File", multiple = FALSE,
                                            accept=c(".RData",".Rda"), buttonLabel="Select Data"),
                           shiny::uiOutput("selectDeployment"),
                           shiny::uiOutput("selectRange"),
                           shiny::uiOutput("selectRange2")),
          shiny::plotOutput('plot',
                            click = "plot_click",
                            dblclick = "plot1_dblclick",
                            brush = shiny::brushOpts(id = "plot1_brush",
                                                     resetOnNew = TRUE)),
          shiny::wellPanel(shiny::h5("Selected Results"),
                           shiny::verbatimTextOutput("results.print"),
                           shiny::h5("Audit Data"),
                           shiny::verbatimTextOutput("audit.print"))
        )
      )
    ),

    server = shiny::shinyServer(function(input, output, session) {

      output$selectDeployment <- shiny::renderUI({

        deploy_choices <- NULL

        # named shiny_list
        load(input$rdata$datapath)

        deploy_choices  <- shiny_list[["Deployment"]] %>%
          dplyr::mutate(choices=paste(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, sep = " - ")) %>%
          dplyr::pull(choices) %>%
          unique()

        shiny::selectInput("selectDeployment", label="Select Deployment",
                           choices = deploy_choices)

      })

      audit_data_reactive <- shiny::reactive({

        # named shiny_list
        load(input$rdata$datapath)

        df.audit.stats <-  shiny_list[["Audit_Stats"]] %>%
          dplyr::mutate(choices=paste(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, sep = " - ")) %>%
          dplyr::filter(choices==input$selectDeployment) %>%
          dplyr::select(-choices)

        df.audit.stats

      })

      result_data_reactive <- shiny::reactive({

        # named shiny_list
        load(input$rdata$datapath)

        df.results.anom <- shiny_list[["Results_Anom"]] %>%
          dplyr::mutate(choices=paste(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, sep = " - ")) %>%
          dplyr::filter(choices==input$selectDeployment) %>%
          dplyr::select(-choices)

        df.results.anom

      })

      output$results.print <- shiny::renderDataTable({
        result_data <- result_data_reactive() %>%
          dplyr::select(datetime, Result.Value, Anomaly, dt_shift,
                        daily_min_q10,
                        daily_max_q90,
                        daily_mean_q10,
                        daily_mean_q90,
                        rDQL,
                        Result.Status.ID,
                        Result.Comment,
                        Row=row.results)

        shiny::nearPoints(result_data, input$plot_click, allRows = FALSE)
      })

      output$audit.print <- shiny::renderPrint({
        audit_data_reactive() %>%
          dplyr::select(audit.datetime.start, Audit.Result.Value, Result.datetime=datetime,
                        Result.Value, diff.minutes, diff.Result, DQL_prec, Audit.Result.Status.ID,
                        Row=row.audits)
      })

      output$displayAudit <- shiny::renderUI({
        df <- audit_data_reactive()
        output$intermediate <- shiny::renderPrint(df,
                                               options = list(paging = FALSE,
                                                              searching = FALSE))
        shiny::dataTableOutput("intermediate")
      })

      ranges <- shiny::reactiveValues(x = NULL, y = NULL)

      shiny::observeEvent(input$plot1_dblclick, {
        brush <- input$plot1_brush
        if (!is.null(brush)) {
          ranges$x <- c(as.POSIXct(brush$xmin, origin = "1970-01-01"),
                        as.POSIXct(brush$xmax, origin = "1970-01-01"))
          ranges$y <- c(brush$ymin, brush$ymax)
        } else {
          ranges$x <- NULL
          ranges$y <- NULL
        }
      })

      output$plot <- shiny::renderPlot({
        result_data <- result_data_reactive()

        ltitle <- "Field Audit\nGrade and\nAnomaly Check"

        p <- ggplot2::ggplot(data=result_data) +
          ggplot2::geom_point(ggplot2::aes(x=datetime, y=Result.Value, color=rDQL, shape = Anomaly), size = 3) +
          ggplot2::scale_color_manual(name ="Result DQL", values = c("A"="black","B"="#FF9B4C","C"="#7277C1","E"="#998B6B","NA"="#C0C0C0","D"="#0528a8")) +
          ggplot2::scale_shape_manual(name ="Anamoly", values=c('TRUE' = 4, 'FALSE' = 16)) +
          ggplot2::coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
          ggplot2::theme_linedraw() +
          ggplot2::theme(legend.position="right",
                         #legend.direction = "horizontal", legend.box = "horizontal",
                         panel.grid.major = ggplot2::element_line(color = "gray"),
                         panel.grid.minor = ggplot2::element_line(color = "gray"))

        audit_data <- audit_data_reactive()
        p <- p +
          ggplot2::geom_vline(xintercept = as.numeric(audit_data[,'audit.datetime.start']), color= 'green', size=1) +
          ggplot2::geom_point(data = audit_data, ggplot2::aes(x = audit.datetime.start, y = Audit.Result.Value),
                              color = 'green', size = 4)

        p
      })

      session$onSessionEnded(function() {
        shiny::stopApp()
      })

    })
  )
  shiny::runApp(app, launch.browser = TRUE)
}
