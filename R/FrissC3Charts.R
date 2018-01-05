#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
FrissC3Charts <- function(message, width = NULL, height = NULL) {

  # forward options using x
  x = list(
    message = message
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'FrissC3Charts',
    x,
    width = width,
    height = height,
    package = 'FrissC3Charts'
  )
}

#' Widget output function for use in Shiny
#'
#' @param outputId outputId of the component
#' @param chart charttype: either: FrissC3Gauge /
#' @param width width of the chart
#' @param height height of the chart
#'
#' @export
FrissC3ChartsOutput <- function(outputId, chart ,width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, chart, width, height, package = 'FrissC3Charts')
}

#' Widget render function for use in Shiny
#'
#' @export
renderFrissC3Charts <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, FrissC3ChartsOutput, env, quoted = TRUE)
}

#' Friss C3 LineChart
#'
#' Friss custom c3.js HTMLWidget output binding
#'
#' @import htmlwidgets
#'
#' @export
FrissC3LineChart <- function(timeData,lineData,color,showSubChart=FALSE,timeFormat='%Y-%m-%d',debounce=250,width='100%',height='400px',transition=250,yLabel='y-label',xLabel='x-label'){

  names(timeData) <- 'x'
  data <- data.frame(timeData,lineData,stringsAsFactors=FALSE)

  columns = list(data)

  nCol <- ncol(data)

  columns = list()
  types   = list()
  axes    = list()

  columns[[1]] <- c(names(data)[1], data[,1])
  types[[names(data)[1]]] <- 'timeseries'
  axes[[names(data)[1]]]  <- 'x'

  for(i in 2:nCol){
    columns[[i]] <- c(names(data)[i], data[,i])

    types[[names(data)[i]]] <- 'line'
    axes[[names(data)[i]]]  <- 'y'
  }

  axis=list(x=list(type='timeseries',label=xLabel,tick=list(format=timeFormat)))

  Data <- list(data=list(x='timeData',
                         columns=columns,
                         axes=axes),
               types=types,
               axis=axis,
               color=list(pattern=color),
               subchart=list(show=showSubChart),
               debounce=debounce,
               height=height,
               transition=transition)

  # create widget
  htmlwidgets::createWidget(
    name    = 'FrissC3LineChart',
    x       = Data,
    width   = width,
    height  = height,
    package = 'FrissC3Charts'
  )
}


#' C3 chart output function for use in Shiny
#'
#' @param outputId outputId of the component
#' @param chart charttype: either: FrissC3Gauge /
#' @param width width of the chart
#' @param height height of the chart
#'
#' @export
FrissC3LineChartOutput <- function(outputId, width = '100%', height = '400px'){
  FrissC3ChartsOutput(outputId, 'FrissC3LineChart' ,width = width, height = height)
}

#' C3 linebarchart render function for use in Shiny
#'
#' @export
renderFrissC3LineChart <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, FrissC3LineChartOutput, env, quoted = TRUE)
}


#' Friss C3 gauge
#'
#' @import htmlwidgets
#' @param value value to set the gauge on
#' @param min min value for the gauge
#' @param max max value for the gauge
#' @param text text to put under the gauge
#' @param gaugeWidth widh of the gauge within the div
#' @param color fill color of the gauge
#' @param showMinMax indicates wheater the min and max values are shown
#' @param width width of the div the gauge is put in
#' @param height height of the div the gauge is put in
#' @export
FrissC3Gauge <- function(value = 50, min = 0, max = 100, text = "value",
                         gaugeWidth = 20, color = "red", showMinMax = TRUE, width = NULL, height = NULL,transition=250) {

  # forward options using x
  x = list(
    value = value,
    min   = min,
    max   = max,
    text  = text,
    gaugeWidth = gaugeWidth,
    color = color,
    showMinMax = showMinMax,
    transition=transition
  )

  # create widget
  htmlwidgets::createWidget(
    name    = 'FrissC3Gauge',
    x       = x,
    width   = width,
    height  = height,
    package = 'FrissC3Charts'
  )
}

#' C3 gauge Widget output function for use in Shiny
#'
#' @param outputId outputId of the component
#' @param chart charttype: either: FrissC3Gauge /
#' @param width width of the chart
#' @param height height of the chart
#'
#' @export
FrissC3GaugeOutput <- function(outputId, width = '100%', height = '400px'){
  FrissC3ChartsOutput(outputId, 'FrissC3Gauge' ,width = '100%', height = '400px')
}

#' C3 gauge Widget render function for use in Shiny
#'
#' @export
renderFrissC3Gauge <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, FrissC3GaugeOutput, env, quoted = TRUE)
}

#' Friss Gauge
#'
#' Friss custom c3.js HTMLWidget output binding
#'
#' @import htmlwidgets
#'
#' @export
FrissC3LineBarChart <- function(timeData,counts,lineData,color,showSubChart=FALSE,timeFormat='%Y-%m-%d',debounce=250,width='100%',height='400px',transition=250,yLabel1='y-label',yLabel2='y-label2',xLabel='x-label'){

  names(timeData) <- 'x'
  data <- data.frame(timeData,counts,lineData,stringsAsFactors=FALSE)

  columns = list(data)

  nCol <- ncol(data)

  columns = list()
  types   = list()
  axes    = list()

  columns[[1]] <- c(names(data)[1], data[,1])
  types[[names(data)[1]]] <- 'timeseries'
  axes[[names(data)[1]]]  <- 'x'

  columns[[2]] <- c(names(data)[2], data[,2])
  types[[names(data)[2]]] <- 'bar'

  for(i in 3:nCol){
    columns[[i]] <- c(names(data)[i], data[,i])

    types[[names(data)[i]]] <- 'line'
    axes[[names(data)[i]]] <- 'y2'
  }

  Data <- list(data=list(x='timeData',
                           columns=columns,
                           axes=axes),
                 types=types,
                 axis=list(y=list(label=list(text=yLabel1,position='outer-middle')),
                           y2=list(show=TRUE,label=list(text=yLabel2,position='outer-middle')),
                           x=list(type='timeseries',label=xLabel,tick=list(format=timeFormat))),
                 color=list(pattern=color),
                 subchart=list(show=showSubChart),
                 debounce=debounce,
                 height=height,
                 transition=transition)

  # create widget
  htmlwidgets::createWidget(
    name    = 'FrissC3LineBarChart',
    x       = Data,
    width   = width,
    height  = height,
    package = 'FrissC3Charts'
  )
}


#' Zooms an existing C3Chart from shiny
#'
#' @param Shiny session id
#' @param outputId Id of the C3LineBarChart to update
#' @param minX minimum X value of the zoomed region
#' @param maxX maximum X value of the zoomed region
#'
#' @export
FrissC3ChartZoom <- function(session,outputId,minX, maxX){
  Data <- list(outputId = outputId,minX = minX, maxX = maxX)

  session$sendCustomMessage("ZoomC3Chart", Data)
}

#' C3 linebarchart output function for use in Shiny
#'
#' @param outputId outputId of the component
#' @param chart charttype: either: FrissC3Gauge /
#' @param width width of the chart
#' @param height height of the chart
#'
#' @export
FrissC3LineBarChartOutput <- function(outputId, width = '100%', height = '400px'){
  FrissC3ChartsOutput(outputId, 'FrissC3LineBarChart' ,width = width, height = height)
}

#' C3 linebarchart render function for use in Shiny
#'
#' @export
renderFrissC3LineBarChart <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, FrissC3LineBarChartOutput, env, quoted = TRUE)
}

#' Friss C3 pie or donut chart
#'
#' @import htmlwidgets
#' @param value dataframe with values for the pie, the dataframe should consists of one row with a column for each group.
#' @param type type of the chart, either donut or pie
#' @param width width of the div the gauge is put in
#' @param height height of the div the gauge is put in
#' @examples
#' \dontrun{
#' data <- data.frame(group1=20,group2=50,group3=70)
#' FrissC3PieChart(data)
#' }
#' @export
FrissC3PieChart <- function(value = data.frame(group1=20,group2=50,group3=70), type='pie', dataHidden=list(), width = NULL, height = 250,maxGroups=NA,maxStringSize=10,legendPosition='bottom',transition=250,displayValues=FALSE) {

  if(!tolower(legendPosition) %in% c('bottom','right')){
    warning(paste0('Invalied legend position supplied: ', legendPosition, ' using default possition bottom'))
    legendPosition = 'bottom'
  }

  nValues <-ncol(value)

  fullNames <- colnames(value)

  # Cut group names off to maximum size
  if(!is.na(maxStringSize)){
      colnames(value) <- lapply(colnames(value),function(x) {

        if(nchar(x)>10)
          return(paste0(substr(x,1,min(10,nchar(x))),".."))
        else
          return(x)
    })
  }

  # Replace column name with empty string with valid value.
  colnames(value)[which(colnames(value)=="")] <- "N/A"

  # Append a number to non-unique columnnames.
  #
  # If we have two column names 'long label part that gets cut off' and 'long label part that also gets cut off'
  # then after the previous step if maxStringSize=10 they become
  # 'long label' and 'long label'.
  # The c3 binding will then combine those groups because they share the same name which is not what we want.
  # To overcome this the strings are fixed such that they become 'long label' and 'long label2'
  lapply(unique(colnames(value)),function(x) {

    n <- sum(colnames(value)==x)

    if(n>1)
      colnames(value)[colnames(value)==x] <- paste0(colnames(value)[colnames(value)==x],c("",(2:(n))))
  })

  # forward options using x
  x = list(
    dataHidden     = dataHidden,
    value          = value,
    type           = type,
    height         = height,
    legendPosition = legendPosition,
    fullNames      = fullNames,
    transition     = transition,
    displayValues  = displayValues
  )

  # create widget
  htmlwidgets::createWidget(
    name    = 'FrissC3PieChart',
    x       = x,
    width   = width,
    height  = height,
    package = 'FrissC3Charts'
  )
}

#' C3 gauge Widget output function for use in Shiny
#'
#' @param outputId outputId of the component
#' @param chart charttype: either: FrissC3Gauge /
#' @param width width of the chart
#' @param height height of the chart
#'
#' @export
FrissC3PieChartOutput <- function(outputId, width = '100%', height = '400px'){
  FrissC3ChartsOutput(outputId, 'FrissC3PieChart' ,width = '100%', height = '400px')
}

#' C3 gauge Widget render function for use in Shiny
#'
#' @export
renderFrissC3PieChart <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, FrissC3PieChartOutput, env, quoted = TRUE)
}


#' Friss Gauge
#'
#' Friss custom c3.js HTMLWidget output binding
#'
#' @import htmlwidgets
#'
#' @export
FrissC3StackedAreaChart <- function(timeData,lineData,color,timeFormat='%Y-%m-%d',debounce=250,width='100%',height='400px',transition=250){

  names(timeData) <- 'x'
  data <- data.frame(timeData,lineData,stringsAsFactors=FALSE)

  columns = list(data)

  nCol <- ncol(data)

  columns = list()
  types   = list()
  axes    = list()

  columns[[1]] <- c(names(data)[1], data[,1])
  types[[names(data)[1]]] <- 'timeseries'
  axes[[names(data)[1]]]  <- 'x'

  for(i in 2:nCol){
    columns[[i]] <- c(names(data)[i], data[,i])

    types[[names(data)[i]]] <- 'area-spline'
  }

  Data <- list(data=list(x='timeData',
               columns=columns),
               types=types,
               axis=list(x=list(type='timeseries',tick=list(format=timeFormat))),
               color=list(pattern=color),
               groups=names(lineData),
               height=height,
               width=width,
               transition=transition)

  # create widget
  htmlwidgets::createWidget(
    name    = 'FrissC3StackedAreaChart',
    x       = Data,
    width   = width,
    height  = height,
    package = 'FrissC3Charts'
  )
}


#' C3 gauge Widget output function for use in Shiny
#'
#' @param outputId outputId of the component
#' @param chart charttype: either: FrissC3Gauge /
#' @param width width of the chart
#' @param height height of the chart
#'
#' @export
FrissC3StackedAreaChartOutput <- function(outputId, width = '100%', height = '400px'){
  FrissC3ChartsOutput(outputId, 'FrissC3StackedAreaChart' ,width = '100%', height = '400px')
}

#' C3 gauge Widget render function for use in Shiny
#'
#' @export
renderFrissC3StackedAreaChart <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, FrissC3StackedAreaChartOutput, env, quoted = TRUE)
}

#' Friss C3 BarChart
#'
#' Friss custom c3.js HTMLWidget output binding
#'
#' @import htmlwidgets
#' @examples
#' app <- shinyApp(
#'
#' ui = fluidPage(
#'   actionButton("Update","Update"),
#'   FrissC3BarChartOutput("Chart")
#' ),
#'
#' server = function(input, output) {
#'   output$Chart <- renderFrissC3BarChart({
#'
#'     cat("\nUpdate")
#'     input$Update
#'     Data <- data.frame(suspected = runif(3,8,11), investigated =
#'                          c(6, 9, 1), suspicious = c(7, 9, 12), proven = c(2, 3, 25), row.names =
#'                          c("actual", "planned", "average"))
#'
#'     # make number of columns data change
#'     if(input$Update %% 2 == 0) Columns <- 2:3
#'     else Columns <- 1:3
#'
#'     Data    <- Data[,Columns]
#'
#'     FrissC3BarChart(Data = Data, yUnit = "", width = "100%",
#'                     height = "500px", padding = list(top = 40, right = 100, bottom = 40, left
#'                                                      = 100), rotated = FALSE, legend_show = TRUE, legend_position = "bottom")
#'   })
#' }
#' )
#'
#' runApp(app)
#' @export
#'
#'
FrissC3BarChart <- function(Data=data.frame(suspected=c(10,8,11),
                                            investigated=c(6,9,1),
                                            suspicious=c(7,9,12),
                                            proven=c(2,3,25),row.names=c("actual","planned","average"))
                            ,yUnit=""
                            ,width = '100%', height = '500px'
                            ,padding = list(top    = 40,
                                            right  = 100,
                                            bottom = 40,
                                            left   = 100)
                            ,rotated = FALSE, legend_show = TRUE, legend_position = 'bottom'
                            ){

  # Data is translate into a set of 3 series (#rows)
  # Each series has (#col) values, each belonging to a different categorie
  # category names are equal to the column names
  # series names are equal to the rownames and will appear as the legend

  # category names
  categories        <- colnames(Data)

  # append rownames: first entry will be rowsnames
  # convert dataframe to matrix (note coversion to JSON will be different)
  Data <- as.matrix(data.frame(Name = rownames(Data),Data))

  Data <- list(columns         = Data,
               categories      = categories,
               yUnit           = yUnit,
               padding         = padding,
               rotated         = rotated,
               legend_show     = legend_show,
               legend_position = legend_position,
               width           = width,
               height          = height
               )

  # create widget
  htmlwidgets::createWidget(
    name    = 'FrissC3BarChart',
    x       = Data,
    width   = width,
    height  = height,
    package = 'FrissC3Charts'
  )
}


#' C3 Bar chart output function for use in Shiny
#'
#' @param outputId outputId of the component
#' @param chart charttype: either: FrissC3Gauge /
#' @param width width of the chart
#' @param height height of the chart
#'
#' @export
FrissC3BarChartOutput <- function(outputId, width = '100%', height = '500px'){
  FrissC3ChartsOutput(outputId, 'FrissC3BarChart' ,width = width, height = height)
}

#' C3 Bar chart render function for use in Shiny
#'
#' @export
renderFrissC3BarChart <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, FrissC3BarChartOutput, env, quoted = TRUE)
}

#' Friss C3 LineChart
#'
#' Friss custom c3.js HTMLWidget output binding
#'
#' @import htmlwidgets
#'
#' @export
FrissC3StackedBarChart <- function(barData,width = '100%', height = '400px'){

  columns <- list(barData)
  nCol <- ncol(barData)

  for(i in 1:nCol){
    columns[[i]] <- c(names(barData)[i], barData[,i])
  }

  Data <- list(columns=columns)

  # create widget
  htmlwidgets::createWidget(
    name    = 'FrissC3StackedBarChart',
    x       = Data,
    width   = width,
    height  = height,
    package = 'FrissC3Charts'
  )
}


#' C3 Bar chart output function for use in Shiny
#'
#' @param outputId outputId of the component
#' @param chart charttype: either: FrissC3Gauge /
#' @param width width of the chart
#' @param height height of the chart
#'
#' @export
FrissC3StackedBarChartOutput <- function(outputId, width = '100%', height = '400px'){
  FrissC3ChartsOutput(outputId, 'FrissC3StackedBarChart' ,width = width, height = height)
}

#' C3 Bar chart render function for use in Shiny
#'
#' @export
renderFrissC3StackedBarChart <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, FrissC3StackedBarChartOutput, env, quoted = TRUE)
}
