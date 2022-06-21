
# custom JS for bars:
create_js <- function(color, col_max){
  paste0("function(cellInfo, state) {
      //console.log(cellInfo.row);
      var value = cellInfo.value;
      var row = cellInfo.row;
      var level = cellInfo.level;
      
      if (level==2) {
        var pct = 100*value/row['denom_form'];
        var label = value.toString().padStart(", col_max,");
        var alpha = 0.5;
        var font = 'normal';
     
      } else if (level<2) {
        var pct = 100*value/(row['Completed']+row['Missing']+row['Expected']); 
        var label = Math.round(pct).toString().padStart(3) + '%';
        var alpha = 1;
        var font = 'bold';
      }
      return (
        '<div style=\"display: flex; align-items: center;\">' +
          '<div style = \"font-weight:' + font + '; white-space: pre\">' + label + '</div>' + 
          '<div style=\"flex-grow: 1; margin-left: 6px; height: 14px; background-color: #e1e1e1\">' +
            '<div style=\"height: 100%; width: ' + pct + '%; opacity: ' + alpha + '; background-color: ", color, "\"></div>' +
          '</div>' +
        '</div>'
      )
      
    }")
}

create_js_footer <- function(column, color){
  paste0("function(colInfo) {
         var column = '", column, "';
        var completed = 0
        var expected = 0
        var missing = 0
        console.log(colInfo.data)
         colInfo.data.forEach(function(row) { 
            completed += row[['Completed']]
            expected += row[['Expected']]
            missing += row[['Missing']]
        })
        if (column == 'Completed'){
            var pct = 100*completed/(completed + expected + missing)
        } else if (column=='Expected'){
            var pct = 100*expected/(completed + expected + missing)
        } else if (column=='Missing'){
            var pct = 100*missing/(completed + expected + missing)
        }
        var label = Math.round(pct).toString().padStart(3) + '%'
      return (
        '<div style=\"display: flex; align-items: center;\">' +
          '<div style = \"font-weight: bold; white-space: pre\">' + label + '</div>' + 
        '<div style=\"flex-grow: 1; margin-left: 6px; height: 14px; background-color: #e1e1e1\">' +
           '<div style=\"height: 100%; width: ' + pct + '%; background-color: ", color, "\"></div>' +
         '</div>' +
        '</div>'
       )
      }")
}


create_reactable <- function(shared_data, data, collection, total_footer){
  shared_data %>%  
    reactable( 
      groupBy  = c('SITENAME', 'VISIT'),
      theme = fivethirtyeight(),
      sortable = FALSE,
      highlight = TRUE, compact = TRUE,
      defaultPageSize = length(unique(data$SITENAME)),
      showPagination = FALSE, showPageInfo = FALSE, paginationType = "simple", 
      defaultExpanded = FALSE, 
      wrap = FALSE, 
      columns = list(
        SITENAME = colDef(name = "SITENAME (# Consented)",
                          filterable = FALSE,
                          footer = if(total_footer==TRUE){"All Sites"} else {NULL},
                          footerStyle = list(fontWeight = "bold"),
                          grouped = JS("function(cellInfo) {
                             return(cellInfo.value)
                                        }")),
        VISIT  = colDef(filterable = FALSE,
                        grouped = JS("function(cellInfo) {
                             return(cellInfo.value)
                                        }"),
                        aggregate = JS("function(values, rows){
                                         //  vals = new Set(values); 
                                         //  var str = vals.size.toString() + ' in-window';
                                         //   return ('<div style = \"font-style: italic\">' + str + '</div>')
                                       return('')
                                           }"),
                        html = TRUE),
        COLLECTION = colDef(filterable = FALSE,
                        name = collection,
                        aggregate = JS("function(values, rows){
                                          // var str = values.length.toString() + ' in-window';
                                          //  return ('<div style = \"font-style: italic\">' + str + '</div>')
                                       return('')
                                           }"),
                        html = TRUE
        ),
        Completed = colDef(
          align = 'left',
          aggregate = "sum",
          aggregated = JS(create_js(color = "#1B9E77", col_max = 4)),
          cell = JS(create_js(color = "#1B9E77", col_max = 4)),
          style = list(fontFamily = "monospace", whiteSpace = "pre"),
          footerStyle =  list(fontFamily = "monospace", whiteSpace = "pre"),
          html = TRUE   ,
          footer = if(total_footer==TRUE){
            JS(create_js_footer(column = "Completed", color = "#1B9E77"))
            } else {NULL},
            ),
        Missing = colDef(
          align = 'left',
          aggregate = "sum",
          aggregated = JS(create_js(color = "#D95F02", col_max = 4)),
          cell = JS(create_js(color = "#D95F02", col_max = 4)), 
          style = list(fontFamily = "monospace", whiteSpace = "pre"), 
          footerStyle =  list(fontFamily = "monospace", whiteSpace = "pre"),
          html = TRUE   ,
          footer = if(total_footer==TRUE){
            JS(create_js_footer(column = "Missing", color = "#D95F02")) 
          } else {NULL}
        ),
        Expected = colDef(
          align = 'left',
          aggregate = "sum",
          aggregated = JS(create_js(color = "#7570B3", col_max = 4)),
          cell = JS(create_js(color = "#7570B3", col_max = 4)), 
          style = list(fontFamily = "monospace", whiteSpace = "pre"),
          footerStyle =  list(fontFamily = "monospace", whiteSpace = "pre"),
          html = TRUE   ,
          footer = if(total_footer==TRUE){
            JS(create_js_footer(column = "Expected", color = "#7570B3"))
            } else {NULL}
        ), 
        denom_form = colDef(aggregate = "sum",
                            name = "Total",
                            show = FALSE),
        subj_dat = colDef(show = FALSE),
        pct_miss = colDef(show = FALSE)
      ),
      details = function(index){
        dat <- data$subj_dat[[index]]
        if (nrow(dat)>0){
          htmltools::div(style = "padding: 5px",
                         align = "right",
                         reactable(dat, 
                                   theme = fivethirtyeight(font_size = 13),
                                   groupBy = "name",
                                   defaultExpanded = TRUE,
                                   fullWidth=FALSE,
                                   showPagination = FALSE, 
                                   showPageInfo = FALSE,
                                   columns = list(
                                     USUBJID  = colDef(minWidth = 150),
                                     name  = colDef(minWidth = 150,
                                                    name = "STATUS")
                                     
                                   )
                         )
          )
        }
      }
    )
}