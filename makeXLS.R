library(openxlsx)

source('init.R')

makeXLS <- function(semestre, grupos = grupos, book = TRUE, path = tempdir())
{
    old <- setwd('csv')
    on.exit(setwd(old))
    
    dfs <- lapply(grupos, function(x)
    {
        f <- paste0(x, '_', semestre, '.csv')
        if (file.exists(f))
            read.csv2(f, stringsAsFactors = FALSE)
        else  data.frame()
    })
    names(dfs) <- grupos
    empty <- sapply(dfs, function(x)nrow(x) == 0)
    dfs <- dfs[!empty]

    if (!book) ## Todo junto en una misma hoja
    {
        dfs <- lapply(dfs, FUN = function(x)
            {
                if (!("Itinerario" %in% names(x)))
                    x$Itinerario <- " "
                x
            })
        dfs <- do.call(rbind, dfs)
    }
    
    if (book)
        xlsFile <- file.path(path, paste0('ETSIDI_S', semestre, '_grupos', '.xlsx'))
    else
        xlsFile <- file.path(path, paste0('ETSIDI_S', semestre, '.xlsx'))

    xls <- write.xlsx(dfs, file = xlsFile,
                      creator = 'ETSIDI',
                      asTable = TRUE)

    if (book) lapply(seq_along(dfs), function(i)
    {
        setColWidths(xls, sheet = i,
                     cols = seq_len(ncol(dfs[[i]])),
                     widths = "auto")
    })
    else
        setColWidths(xls, sheet = 1,
                     cols = seq_len(ncol(dfs)),
                     widths = "auto")
    
    saveWorkbook(xls, file = xlsFile, overwrite = TRUE)
}

makeXLS(1, grupos, book = FALSE)
makeXLS(1, grupos, book = TRUE)

makeXLS(2, grupos, book = FALSE)
makeXLS(2, grupos, book = TRUE)
