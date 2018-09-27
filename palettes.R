# test
# n_breaks <- 20
# barplot(1:n_breaks, col = get_colours_codes(n_breaks, pkg_name = 'ghibli', pal_name = 'SpiritedMedium', is_pal_rev = TRUE))

get_colours_codes <- function(
                    n_breaks = 7,
                    fixed_cols = c('white', 'red'),
                    pkg_name = NA,
                    pal_name = NA,
                    is_pal_rev = FALSE
                ){

    # check package and palette name are valid
    if(!pkg_name %in% unique(palettes$package)){
        pkg_name <- NA
    } else if (!pal_name %in% palettes[package == pkg_name, name]){
        pkg_name <- NA
    }
    if(is.na(pkg_name)){
        # build gradient between two or three colours
        if(sum(fixed_cols %in% colors()) < 2) fixed_cols <- c('white', 'red')
        fixed_cols <- fixed_cols[fixed_cols %in% colors()][1:min(sum(fixed_cols %in% colors()), 5)]
        pal_cols <- colorRampPalette(fixed_cols)(n_breaks)
    } else {
        # extract colours from package
        pal_cols <- switch(pkg_name,
            'core'        = {
                get(pal_name)(n_breaks)
            },
            'brewer'      = {
                if(n_breaks > brewer.pal.info[pal_name, 'maxcolors']){
                    colorRampPalette(brewer.pal(brewer.pal.info[pal_name, 'maxcolors'], pal_name))(n_breaks)
                } else {
                    brewer.pal(n_breaks, pal_name)
                }
            },
            'carto'       = {
                carto_pal(n_breaks, pal_name)
            },
            'cartography' = {
                carto.pal(pal_name, n_breaks)
            },
            'colorspace'  = {

            },
            'dichromat'   = {

            },
            'ggsci'       = {

            },
            'ghibli'      = {
                ghibli_palette(pal_name, n_breaks, type = 'continuous')
            },
            'redmonder'   = {
                colorRampPalette(redmonder.pal(palettes[package == pkg_name & name == pal_name, max_cols], pal_name))(n_breaks)
            },
            'scico'       = {
                scico(n_breaks, palette = pal_name)
            },
            'tol'         = {
                if(palettes[package == pkg_name & name == pal_name, substr(type, 1, 1)] == 'Q'){
                    colorRampPalette(GetTolColors(palettes[package == pkg_name & name == pal_name, max_cols], scheme = pal_name))(n_breaks)
                } else {
                    GetTolColors(n_breaks, scheme = pal_name)
                }
            },
            'viridis'     = {
                get(pal_name)(n_breaks)
            },
            'wesanderson' = {
                wes_palette(pal_name, n_breaks, type = 'continuous')
            }
        )
        # reverse colours
        if(is_pal_rev) pal_cols <- rev(pal_cols)
    }

    return(pal_cols)
}

get_palette <- function(
                    y,
                    n_breaks = 7,
                    class_method = 'quintile',
                    fixed_cols = c('white', 'red'),
                    pkg_name = NA,
                    pal_name = NA,
                    is_pal_rev = FALSE
                ){

    # check classification method is valid
    if(!class_method %in% c('equal', 'quantile', 'pretty', 'hclust', 'kmeans')) class_method <- 'quantile'

    # check breaks are not more than 20
    n_breaks <- min(20, n_breaks)

    # return the lookup between values and colours
    findColours(
        classIntervals(y, n_breaks, class_method),
        get_colours_codes(n_breaks, fixed_cols, pkg_name, pal_name, is_pal_rev)
    )

}
