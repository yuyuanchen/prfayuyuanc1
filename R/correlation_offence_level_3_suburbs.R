## QUESTION ----

#' <Title>
#'
#' \code{correlation_offence_level_3_suburbs} This method is used to plot the correlaiton of offence event number
#' between two given suburb areas given description in offence level 3
#' @param crime_data A data.table object with the following columns:
#'     "date" (POSIXct), "suburb" (chr), "postcode" (chr), "offence_level_1" (chr),
#'     "offence_level_2" (chr), "offence_level_3" (chr), "offence_count" (num).
#' @param offence_description A character string of offence level 3
#' @param postcodes A two-element character vector. Each element is an SA postcode.
#' @export
#' @return  A ggplot object showing the correlation in offence count between the two input postcodes.
#' @examples
#' <one or two examples showing how to use the function>
  correlation_offence_level_3_suburbs<- function(crime_data, offence_description, postcodes) {
    if (length(suburbs)!=2) {
      stop("Please enter two suburbs")
    }


    expected_colnames <- c("date", "suburb", "postcode", "offence_level_1", "offence_level_2",
                           "offence_level_3", "offence_count")


    if (!all.equal(expected_colnames,names(crime_data))) {
      stop(paste("Input table columns need to match: ",
                 paste(expected_colnames, collapse = ", ")))
    }
    # Check that the input suburbs and offence description exist in crime_data
    if (any(!suburbs %in% crime_data$suburb) |
        !offence_description %in% crime_data$offence_level_3) {
      stop("inputs can not be found in dataset")
    }

    # Make a data table for plotting using data.table transformations
    # You will need to filter, summarise and group by
    # Expect cols: "date", "suburb", "total_offence_count"
    plot_data <- crime_data[(suburb==suburbs[1]|suburb==suburbs[2])&offence_level_3==offence_description,.(total_offence_count=sum(offence_count)),by = .(month(date),suburb)]


    # These lines will transform the plot_data structure to allow us to plot
    # correlations. Try them out
    plot_data[, suburb := plyr::mapvalues(suburb, suburbs, c("x", "y"))]

    plot_data <- dcast(plot_data, month ~ suburb, fun = sum,
                       fill = 0, value.var = "total_offence_count")

    # Generate the plot


    ggplot(plot_data, aes(month)) +
      geom_line(aes(y = plot_data$x, colour = suburbs[1])) +
      geom_line(aes(y = plot_data$y, colour = suburbs[2]))+
      labs(x="month",
           y=offence_description)+scale_fill_discrete(name = "Suburbs")+
      scale_x_continuous(breaks=seq(0,12,1))+
      scale_y_continuous(breaks=seq(0,50,5))}


