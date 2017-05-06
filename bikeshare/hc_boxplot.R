hc_boxplot <- function(hc, x, by = NULL, order_by = NULL, outliers = TRUE, ...) {
    if (is.null(by)) {
        by <- "value"
    } else {
        stopifnot(length(x) == length(by))
    }
    
    if (is.null(order_by)) {
        order_by <- by
    } else {
        stopifnot(length(by) == length(order_by))
    }
    
    df <- data_frame(value = x, by = by, order_by = order_by) %>% 
        group_by_("by", 'order_by') %>% 
        do(data = boxplot.stats(.$value)) %>% 
        ungroup %>% 
        arrange_("order_by") 
    bxps <- map(df$data, "stats")
    hc <- hc %>% hc_xAxis(categories = df$by) %>% hc_add_series(data = bxps, 
                                                                type = "boxplot",
                                                                ...)
    if (outliers) {
        outs <- map2_df(seq(nrow(df)), df$data, function(x, y) {
            if (length(y$out) > 0) 
                d <- data_frame(x = x - 1, y = y$out)
            else d <- data_frame()
            d
        })
        if (nrow(outs) > 0) {
            hc <- hc %>% hc_add_series(data = list_parse(outs), 
                                       name = str_trim(paste(list(...)$name, "outliers")), 
                                       type = "scatter", marker = list(...), tooltip = list(headerFormat = "<span>{point.key}</span><br/>", 
                                                                                            pointFormat = "<span style='color:{point.color}'></span> \n            Outlier: <b>{point.y}</b><br/>"), 
                                       ...)
        }
    }
    hc
}