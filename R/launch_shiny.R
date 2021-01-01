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
                        shiny::tabPanel("Plot",
                                        shiny::fluidPage(shiny::fluidRow(shiny::column(width=9,
                                                                                       shiny::fileInput(inputId="rdata",
                                                                                                        label="Choose RData File", multiple = FALSE, width='100%',
                                                                                                        accept=c(".RData",".Rda"), buttonLabel="Select Data"))),
                                                         shiny::fluidRow(shiny::column(width=9, shiny::uiOutput("selectDeployment")),
                                                                         shiny::column(width=1, shiny::actionButton(inputId="NEXT", label="Next", style = "margin-top: 25px;"), align = "left")),
                                                         shiny::uiOutput("selectRange"),
                                                         shiny::uiOutput("selectRange2"),
                                                         shiny::fluidRow(shiny::column(width=12, shiny::plotOutput('plot',
                                                                                                                   click = "plot_click",
                                                                                                                   dblclick = "plot1_dblclick",
                                                                                                                   brush = shiny::brushOpts(id = "plot1_brush",
                                                                                                                                            resetOnNew = TRUE)
                                                         )
                                                         )
                                                         )
                                        )
                        ),
                        shiny::tabPanel("Selected Results", shiny::dataTableOutput("results.print")),
                        shiny::tabPanel("Audit Data", shiny::dataTableOutput("audit.print"))
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
                           choices = deploy_choices,
                           width='100%',
                           selectize=TRUE)

      })


      deploy_data_reactive <- shiny::reactive({
        # named shiny_list
        load(input$rdata$datapath)

        df.deploy <- shiny_list[["Deployment"]] %>%
          dplyr::mutate(Deployment=paste(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, sep = " - ")) %>%
          dplyr::filter(Deployment==input$selectDeployment)

        df.deploy

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

      shiny::observeEvent(input$plot1_brush, {

        output$results.print <- shiny::renderDataTable({

          result_data <- result_data_reactive() %>%
            dplyr::rename(Row=row.results) %>%
            dplyr::select(Monitoring.Location.ID, Equipment.ID, Characteristic.Name,
                          datetime, Result.Value, Anomaly, dt_shift,
                          daily_min_q10,
                          daily_max_q90,
                          daily_mean_q10,
                          daily_mean_q90,
                          rDQL,
                          Result.Status.ID,
                          Result.Comment,
                          Row)

          shiny::brushedPoints(result_data, input$plot1_brush,
                               xvar = "datetime", yvar = "Result.Value")})

      })

      shiny::observeEvent(input$plot_click, {

        output$results.print <- shiny::renderDataTable({

          result_data <- result_data_reactive() %>%
            dplyr::rename(Row=row.results) %>%
            dplyr::select(Monitoring.Location.ID, Equipment.ID, Characteristic.Name,
                          datetime, Result.Value, Anomaly, dt_shift,
                          daily_min_q10,
                          daily_max_q90,
                          daily_mean_q10,
                          daily_mean_q90,
                          rDQL,
                          Result.Status.ID,
                          Result.Comment,
                          Row)

          shiny::nearPoints(result_data, input$plot_click, allRows = FALSE)})

      })

      output$audit.print <- shiny::renderDataTable({
        audit_data_reactive() %>%
          dplyr::select(Monitoring.Location.ID, audit.datetime.start, Audit.Result.Value, Result.datetime=datetime,
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

        deploy_data <- deploy_data_reactive()
        p <- p +
          ggplot2::geom_vline(xintercept = as.numeric(deploy_data[,'Deployment.Start.Date']), color= 'black', size=1.5) +
          ggplot2::geom_vline(xintercept = as.numeric(deploy_data[,'Deployment.End.Date']), color= 'black', size=1.5)

        p
      })

      # When the next button is clicked, advance to next deployment
      shiny::observeEvent(input$NEXT, {

        # named shiny_list
        load(input$rdata$datapath)

        deploy <- shiny_list[["Deployment"]] %>%
          dplyr::mutate(choices=paste(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, sep = " - "),
                        row=dplyr::row_number())

        next.row <- grep(input$selectDeployment, deploy$choices, fixed=TRUE) + 1

        next.choice <- deploy %>%
          dplyr::filter(row==next.row) %>%
          dplyr::pull(choices)

        shiny::updateSelectizeInput(
          session = session,
          inputId = "selectDeployment",
          selected = next.choice
        )


      })

      session$onSessionEnded(function() {
        shiny::stopApp()
      })

    })
  )
  shiny::runApp(app, launch.browser = TRUE)
}
