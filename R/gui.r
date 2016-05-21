
#' @title Launch the graphical user interface
#' @description A graphical user interface for the \pkg{gibbonsecr} package,
#'   built using \pkg{\link[tcltk]{tcltk}}.
#'
#' See the online manual for more details
#' \url{http://dkidney.github.io/gibbonsecr}.
#' @param prompt.save.on.exit if \code{TRUE}, a message box is shown when the
#'   GUI window is closed, asking if the user wishes to save the workspace.
# @param quit.r.on.exit if \code{TRUE}, the background R process is closed when
#   the GUI window is closed (keep this to \code{FALSE} unless \strong{R} is
#   launched as a background process).
#' @examples
#' \dontrun{
#'
#' library(gibbonsecr)
#' gui()
#' }
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @import tcltk
#' @importFrom tcltk2 tk2tip
#' @importFrom ggplot2 ggplot coord_fixed labs
#' @export

gui = function(prompt.save.on.exit = FALSE){

    # # only uncomment these lines if sourcing
    # # rm(list = ls())
    # prompt.save.on.exit = FALSE
    # # quit.r.on.exit = FALSE
    # library(secr)
    # library(ggplot2)
    # library(tcltk)
    # library(tcltk2)
    # library(gibbonsecr)

    options(width = 100)
    gui.env = environment()
    gibbonsecr_workspace = NULL # to satisfy cran checks

    # appearance settings ------------------------------------------------------

    # - this is just a convenience function to save space
    # - appearance settings are slightly different for mac and windows
    os = gui_appearance_settings()
    theme_set(os$theme)

    # logo icon
    tkimage.create("photo", "::img::tclLogo",
                   file = system.file("extdata/icon/gibbonsecr.gif",
                                      package = "gibbonsecr"))

    # dynamic R objects --------------------------------------------------------

    # - these depend on the data and the model
    robjects = list(
        capthist    = NULL,
        mask        = NULL,
        region      = NULL,
        habitat1    = NULL,
        habitat2    = NULL,
        fit         = NULL,
        wd          = path.expand(getwd()),
        array.names = ""
    )
    robj = list2env(robjects)

    # static R objects ---------------------------------------------------------

    # - these are mainly option labels for the various gui sections
    version = utils::packageVersion("gibbonsecr")
    csv.files = c("detections", "posts", "covariates")
    shp.files = c("region", "habitat1", "habitat2")
    submodels = c("D", "g0", "sigma", "bearings", "distances", "pcall")
    values = list(
        bearings.units  = c("degrees", "radians"),
        bearings.type   = c("continuous"),
        distances.units = c("km", "m"),
        distances.type  = c("continuous"),
        detfunc         = c("half normal", "hazard rate"),
        bearings.dist   = c("none", "von mises", "wrapped cauchy"),
        distances.dist  = c("none", "gamma", "log-normal"),
        ci.method       = c("none", "delta method", "bootstrap")
    )

    # tcl variable values  -----------------------------------------------------

    # - these are the values in plain text of the underlying variables that link
    #   to the gui widgets
    # - the values below are the defaults but they can all be changed by the user
    tvar = list2env(list(
        # data
        detections.path     = "",
        posts.path          = "",
        covariates.path     = "",
        bearings.units      = "degrees",
        bearings.type       = "continuous",
        distances.units     = "km",
        distances.type      = "continuous",
        # mask
        buffer              = "6000",
        spacing             = "250",
        region.path         = "",
        habitat1.path       = "",
        habitat2.path       = "",
        region.use          = "0",
        habitat1.use        = "0",
        habitat2.use        = "0",
        # model
        detfunc             = "half normal",
        bearings.dist       = "von mises",
        distances.dist      = "gamma",
        D.formula           = "",
        g0.formula          = "",
        sigma.formula       = "",
        bearings.formula    = "",
        distances.formula   = "",
        pcall.formula       = "",
        D.fixed             = "",
        g0.fixed            = "1",
        sigma.fixed         = "",
        bearings.fixed      = "",
        distances.fixed     = "",
        pcall.fixed         = "1",
        D.radio             = "formula",
        g0.radio            = "fixed",
        sigma.radio         = "formula",
        bearings.radio      = "formula",
        distances.radio     = "formula",
        pcall.radio         = "fixed",
        # plots
        detfunc.ci.method   = "none",
        bearings.ci.method  = "none",
        distances.ci.method = "none",
        detfunc.ci.level    = "95",
        bearings.ci.level   = "95",
        distances.ci.level  = "95",
        distances.truth     = "500",
        detsurf.contour     = "1",
        densurf.contour     = "0",
        detsurf.array       = "",
        densurf.array       = ""
    ))

    # tcl variables ------------------------------------------------------------

    # - these are the tcl variable values converted to tcl variable format
    # - this format is needed when constructing the widgets
    for(i in names(tvar)){
        tvar[[i]] = tclVar(tvar[[i]])
    }

    # tcl object containers ----------------------------------------------------

    # a heirarchical list to store the widgets
    # this allows loops to be written to make the code more concise
    tobj = list(
        data  = list(),
        mask  = list(),
        model = list(),
        plots = list()
    )

    # tk widgets ---------------------------------------------------------------

    # re-definintions of tcltk functions (using preferred defaults)
    # this is to save space when constructing the widgets

    tkbutton = function(parent, text, command = null_command,
                        width = os$button.width, ...){
        tcltk::ttkbutton(parent, text = text, command = command, width = width,
                         ...)
    }

    tkcheck = function(parent, variable, ...){
        tcltk::ttkcheckbutton(parent, variable = variable, ...)
    }

    tkcombo = function(parent, textvariable, values, width = os$combo.width,
                       state = "readonly", ...){
        tcltk::ttkcombobox(parent, textvariable = textvariable, values = values,
                           width = width, state = state, ...)
    }

    tkentry = function(parent, textvariable, width = os$entry.width, ...){
        tcltk::ttkentry(parent, textvariable = textvariable, width = width, ...)
    }

    tkframe = function(parent, width = os$lhs.width, padding = os$frame.padding,
                       relief = os$relief, ...){
        tcltk::ttkframe(parent, width = width, padding = padding,
                        relief = relief, ...)
    }

    tkgrid = function(..., sticky = "w", padx = os$grid.padx, pady = os$grid.pady){
        tcltk::tkgrid(..., sticky = sticky, padx = padx, pady = pady)
    }

    tklabel = function(parent, text, ...){
        tcltk::ttklabel(parent, text = text, ...)
    }

    tkpack = function(..., side = "top", fill = "none", expand = FALSE,
                      anchor = "center"){
        tcltk::tkpack(..., side = side, fill = fill, expand = expand,
                      anchor = anchor)
    }

    tkradio = function(parent, value, command = null_command, ...){
        tcltk::ttkradiobutton(parent, value = value, command = command, ...)
    }

    # tkimage.create("photo", "::img::gibbonsecr_logo", width = 100, height = 100,
    # file = system.file("extdata/icon/gibbonsecr.gif", package = "gibbonsecr"))


    # FUNCTIONS ----------------------------------------------------------------

    # bespoke functions use in the gui
    # these are mainly wrappers for exported gibbonsecr function

    # > appearance -------------------------------------------------------------

    # convenience function to add heading label to a frame widget
    add_heading = function(parent, text){
        tkpack(tklabel(parent, text, font = os$heading.font, foreground = "#0064FF"),
               anchor = "w")
    }

    # add logo icon (does not currently work on a mac)
    add_icon = function(window){
        if(.Platform$OS.type == "windows"){
            tcl('wm', 'iconbitmap', window,
                system.file('extdata/icon/gibbonsecr.ico', package = "gibbonsecr"))
        }else{
            # tcl('wm', 'iconimage', window,
            # system.file('extdata/icon/gibbonsecr.gif', package = "gibbonsecr"))
            # tcl('wm', 'iconphoto', window, tcl('image', 'create', 'photo', '-file',
            # system.file('extdata/icon/gibbonsecr.gif', package = "gibbonsecr")))
        }
        # tcl('wm', 'iconphoto', window, "::img::gibbonsecr_logo")
    }

    # convenience function to add horizontal separator to a frame widget
    add_separator = function(parent){
        tkpack(ttkseparator(parent, orient = "horizontal"), anchor = "center",
               fill = "x", padx = 0, pady = 10)
    }

    # > files ------------------------------------------------------------------

    # dynamically define a browse function for a specific file
    browse = function(file, ext = "csv"){
        text = gsub("FILE", file, gsub("EXT", ext, paste("
        function(){
            filepath = tclvalue(tkgetOpenFile(
                filetypes = '{{} {.EXT}}',
                initialdir = robj$wd))
            if(nchar(filepath) > 0){
                tclvalue(tvar$FILE.path) = filepath
                refresh()
            }
        }"))) # cat(text)
        eval(parse(text = text))
    }

    # base function for opening a csv file
    view_base = function(file){
        path = path.expand(tclvalue(tvar[[paste0(file, ".path")]]))
        if(file.exists(path)){
            system(paste0("open '", path, "'"))
        }else{
            print_error(paste0("cant find ", file, " file:\n'", path, "'"))
        }
    }

    # dynamically define a view function for a specific csv file
    view = function(file){
        eval(parse(text = paste0("function() view_base('", file, "')")))
    }

    # > import -----------------------------------------------------------------

    shp_import_base = function(file){
        cursor('wait') ; on.exit(cursor('normal'))
        print_to_console(paste("Importing", file, "shapefile..."))
        filepath = tclvalue(tvar[[paste0(file, ".path")]])
        if(!file.exists(filepath)){
            print_error(paste("file does not exist:\n", filepath))
        }else{
            result = try(utils::capture.output({
                poly = import_shp(filepath, fortify = FALSE)
            }))
            if(inherits(result, 'try-error')){
                print_error(result)
            }else{
                if(file != 'region'){
                    print_to_console('- checking covariate classes...')
                    out = check_covariate_classes(poly@data)
                    poly@data = poly@data[, out$use, drop = FALSE]
                    out = out[out$use,,drop = FALSE]
                    colnames(poly@data) = out$name
                    for(j in 1:ncol(poly@data)){ # j=2
                        var = out$name[j]
                        # class
                        FUN = if(out$class[j] == 'number') as.numeric else as.factor
                        poly@data[[var]] = FUN(poly@data[[var]])
                        # center
                        if(out$center[j])
                            poly@data[[paste0(var, "_centered")]] =
                            as.numeric(scale(poly@data[[var]], TRUE, FALSE))
                        # scale
                        if(out$scale[j])
                            poly@data[[paste0(var, "_scaled")]] =
                            as.numeric(scale(poly@data[[var]], TRUE, TRUE))
                        # log
                        if(out$log[j])
                            poly@data[[paste0(var, "_log")]] = log(poly@data[[var]])
                    }
                    if(all(!out$use)){
                        print_error('no covariates chosen')
                        stop(.call = FALSE)
                    }
                    poly@data = poly@data[, out$use, drop = FALSE]
                }
                print_to_console('- checking for errors...')
                poly = shp_check(poly, robj$capthist, region = file == 'region')
                robj[[file]] = import_shp(poly)
                print_success('Import successful')
                tclvalue(tvar[[paste0(file, ".use")]]) = '1'
                refresh()
            }
        }
    }

    shp_import = function(file){
        eval(parse(text = paste0("function() shp_import_base('", file, "')")))
    }

    shp_check = function(poly, capthist, region = TRUE){
        # traps are within poly bounding box
        if(!region){
            # spatial covariates are valid
            for(j in colnames(poly@data)){ #j=1
                if(length(unique(poly@data[[j]])) == 1){
                    print_warning(paste0("'", j, "' removed as values are all equal"))
                    poly@data[[j]] = NULL
                }
            }
            if(ncol(poly@data) == 0){
                print_warning("No attributes so using default Id names")
                poly@data[["id"]] = factor(rownames(poly@data))
            }
        }
        return(poly)
    }

    # > general ----------------------------------------------------------------

    # > general ----------------------------------------------------------------

    # > general ----------------------------------------------------------------

    # makes an 'About' message box
    about = function(){
        tkmessageBox(
            title = paste0("About gibbonsecr v", version), icon = "info", type = "ok",
            message = "This is a pre-release version of the software. If you notice any bugs or have any general queries, please email Darren Kidney at darrenkidney@googlemail.com")
        tkfocus(main.window)
    }

    # deletes everything in the gui console
    clear_console = function(){
        tkconfigure(console, state = "normal")
        tkdelete(console, "1.0", "end")
        tkconfigure(console, state = "disabled")
    }

    # pop menu for when user right clicks over gui console
    console_popup = function(){
        console.popup.menu = tkmenu(console, tearoff = FALSE)
        tkadd(console.popup.menu, "command", label = "Clear console",
              command = clear_console, state = "normal")
        x = tkwinfo("pointerx", main.window)
        y = tkwinfo("pointery", main.window)
        tkpopup(console.popup.menu, x, y)
    }

    # controls the appearance of the cursor
    cursor = function(x = c("normal", "wait")){
        x = match.arg(x)
        if(x == "normal"){
            tkconfigure(main.window, cursor = "left_ptr")
            tkconfigure(console,     cursor = "xterm")
            tkconfigure(menu$main,   cursor = "left_ptr")
        }else{
            cursor = if(.Platform$OS.type == "windows") "wait" else "watch"
            tkconfigure(main.window, cursor = cursor)
            tkconfigure(console,     cursor = cursor)
            tkconfigure(menu$main,   cursor = cursor)
        }
    }

    # bespoke wrapper for gibbonsecr::import_data
    data_import = function(){
        cursor("wait") ; on.exit(cursor("normal"))
        print_to_console("Importing data...")
        # check detection and posts exist
        # check covariates exists if file path isn't blank
        for(file in csv.files){ # file = "posts"
            path = tclvalue(tvar[[paste0(file, ".path")]])
            if(!(path == "" && file == "covariates")){
                if(!file.exists(path)){
                    print_error(paste0("cant find ", file, " file:\n", path))
                    stop(.call = FALSE)
                }
            }
        }
        result = try({
            utils::capture.output({
                robj$capthist = import_data(
                    detections = tclvalue(tvar$detections.path),
                    posts      = tclvalue(tvar$posts.path),
                    covariates = if(tclvalue(tvar$covariates.path) == ""){
                        NULL
                    }else{
                        tclvalue(tvar$covariates.path)
                    },
                    details = list(
                        bearings  = list(
                            units = tclvalue(tvar$bearings.units),
                            type  = tclvalue(tvar$bearings.type)
                        ),
                        distances = list(
                            units = tclvalue(tvar$distances.units),
                            type  = tclvalue(tvar$distances.type)
                        )
                    )
                )
            })
        }, TRUE)
        if(inherits(result, "try-error")){
            print_error(result)
        }else{
            print_success("Import successful")
            data_summary()
            refresh()
        }
    }

    # bespoke wrapper for summary.gcapthist
    data_summary = function(){
        if(is.null(robj$capthist)){
            print_error("no data available")
        }else{
            result = utils::capture.output(summary(robj$capthist))
            if(inherits(result, "try-error")){
                print_error(result)
            }else{
                print_to_console(result, "Data summary:", dashes = TRUE)
            }
        }
    }

    # opens a plotting device in a separate window
    device_popup = function(...){
        # FUN = eval(parse(text = switch(.Platform$OS.type,
        #                                windows = "windows", "x11")))
        # FUN(width = width, height = height, pointsize = pointsize, ...)
        FUN = switch(.Platform$OS.type, windows = "windows", "x11")
        dots = list(...)
        if(is.null(dots$width)) dots$width = 8
        if(is.null(dots$height)) dots$height = 6
        if(is.null(dots$pointsize)) dots$pointsize = if(FUN == "x11") 12 else 10
        if(is.null(dots$type)) if(FUN == "x11") dots$type = "cairo"
        do.call(FUN, dots)
    }

    # opens dialogue box when gui is closed to ask if user wants to save workspace
    exit_prompt = function(){
        if(prompt.save.on.exit){
            response = tkmessageBox(title = "", icon = "question",
                                    message = "Save workspace before quitting?",
                                    type = "yesnocancel", default = "no")
            switch(tclvalue(response),
                   "yes" = {
                       filename = workspace_save()
                       if(nchar(filename) > 0) tkdestroy(main.window)
                   },
                   "no" = tkdestroy(main.window)
            )
        }
        close_plots()
        tkdestroy(main.window)
        # if(quit.r.on.exit) q()
    }

    fixed_radio_command = function(submodel){
        text = gsub("SUBMODEL", submodel, paste0("
        function(){
            tkconfigure(tobj$model$SUBMODEL.formula.radio, state = 'disable')
            tkconfigure(tobj$model$SUBMODEL.fixed.radio, state = 'normal')
            refresh()
        }")) # cat(text)
        eval(parse(text = text))
    }

    formula_radio_command = function(submodel){
        text = gsub("SUBMODEL", submodel, paste0("
        function(){
            tkconfigure(tobj$model$SUBMODEL.formula.radio, state = 'normal')
            tkconfigure(tobj$model$SUBMODEL.fixed.radio,  state = 'disable')
            refresh()
        }")) # cat(text)
        eval(parse(text = text))
    }

    mask_check = function(){
        # used in mask_make()
        buffer  = tclvalue(tvar$buffer)
        spacing = tclvalue(tvar$spacing)
        small.spacing = spacing < 100
        large.spacing = spacing >= 500
        small.buffer  = buffer <= 3000
        large.buffer  = buffer > 6000
        if(large.spacing || small.spacing || small.buffer || large.buffer){
            if(small.spacing)
                print_warning("models using a smaller mask spacing should give more reliable results but will take longer to fit.")
            if(large.spacing)
                print_warning("models using a larger mask spacing will be quicker to fit but may give less reliable results.")
            if(small.buffer)
                print_warning("models using a smaller mask buffer will be quicker to fit but may give less reliable results.")
            if(large.buffer)
                print_warning("models using a larger mask buffer should give more reliable results but will take longer to fit.")
        }
        if(!is.null(covariates(robj$mask)[[1]])){
            missing = sapply(covariates(robj$mask), function(x){
                any(apply(x, 1, anyNA))
            })
            if(any(any(missing))){
                print_warning("some mask points have missing covariate values and \nwill be removed")
                # robj$mask = mask_na_rm(robj$mask)
                robj$mask = na.omit(robj$mask)
            }
        }
    }

    mask_make = function(){
        cursor("wait") ; on.exit(cursor("normal"))
        print_to_console("Making mask...")
        if(tclvalue(tvar$region.use) == "1"){
            print_to_console("- clipping mask to region polygon...")
        }
        robj$mask = make_mask(
            traps   = robj$capthist,
            buffer  = as.numeric(tclvalue(tvar$buffer)),
            spacing = as.numeric(tclvalue(tvar$spacing)),
            poly    = if(tclvalue(tvar$region.use) == "1") robj$region$sp else NULL
        )
        # habitat
        for(i in c("habitat1","habitat2")){ # i = "habitat1"
            if(tclvalue(tvar[[paste0(i, ".use")]]) == "1"){
                print_to_console(paste0("- adding covariates from ", i,
                                        " to mask..."))
                result = try({
                    utils::capture.output({
                        robj$mask = add_covariates(robj$mask, robj[[i]])
                    })
                }, TRUE)
                if(inherits(result, "try-error")){
                    print_error(result)
                }else{
                    print_to_console(paste0("- adding covariates from ", i,
                                            " to traps..."))
                    result = try({
                        utils::capture.output({
                            robj$capthist = add_covariates(robj$capthist,
                                                           robj[[i]])
                        })
                    }, TRUE)
                    if(inherits(result, "try-error")){
                        print_error(result)
                    }
                }
            }
        }
        print_to_console("- checking mask...")
        result = try(utils::capture.output(mask_check()), TRUE)
        if(inherits(result, "try-error")){
            print_error(result)
        }else{
            print_success("Mask ready to use")
            mask_summary()
            refresh()
        }
    }

    mask_summary = function(){
        result = try(utils::capture.output({
            summary(robj$mask, robj$capthist)
        }), TRUE)
        if(inherits(result, "try-error")){
            print_error(result)
        }else{
            print_to_console(result, "Mask summary:", dashes = TRUE)
        }
    }

    model_coef = function(){
        result = try(utils::capture.output(
            cbind(
                estimate = coef(robj$fit),
                confint(robj$fit),
                se = sqrt(diag(vcov(robj$fit)))
            )))
        if(inherits(result, "try-error")){
            print_error(result)
        }else{
            print_to_console(result, "Model coefficients:", dashes = TRUE)
        }
    }

    model_fit = function(){
        cursor("wait") ; on.exit(cursor("normal"))

        ##################################################
        ## check inputs

        # if the radio button is on fixed - check the value
        for(i in submodels){ # i = "D"
            if(tclvalue(tvar[[paste0(i, ".radio")]]) == "fixed"){
                if(tclvalue(tvar[[paste0(i, ".fixed")]]) == ""){
                    message = paste0("please enter a fixed value for '", i, "' or select the formula box")
                    print_error(message)
                    stop(.call = FALSE)
                }else{
                    if(is.na(as.numeric(tclvalue(tvar[[paste0(i, ".fixed")]])))){
                        message = paste0("fixed value for '", i, "' not recognised")
                        print_error(message)
                        stop(.call = FALSE)
                    }
                }
            }
        }

        # formulas and fixed ------------------------------------------------- #
        # convert formula entrys to actual formulas
        # if blank then use intercept-only model
        formulas = sapply(submodels, function(i){
            x = tclvalue(tvar[[paste0(i, ".formula")]])
            if(x == "") ~1 else as.formula(paste("~", x))
        }, simplify = FALSE)
        # convert fixed entrys to numeric
        fixed = sapply(submodels, function(i){
            x = tclvalue(tvar[[paste0(i, ".fixed")]])
            if(x == "") NULL else as.numeric(x)
        }, simplify = FALSE)
        # if radiobutton is on formula then set fixed to blank
        # otherwise set formula to blank
        for(i in submodels){
            if(tclvalue(tvar[[paste0(i, ".radio")]]) == "formula"){
                fixed[[i]] = NULL
            }
        }

        # g0 warning --------------------------------------------------------- #
        if(all(n_occasions(robj$capthist) == 1)){
            if(tclvalue(tvar$g0.radio) == "fixed" && tclvalue(tvar$g0.fixed) != "1"){
                print_warning("g0 normally fixed at 1 for single-occasion surveys")
            }
        }

        # fit model ---------------------------------------------------------- #
        result = try({
            utils::capture.output({
                robj$fit = gfit(
                    capthist = robj$capthist,
                    model = formulas,
                    fixed = fixed,
                    model.options = list(
                        detfunc = switch(
                            tclvalue(tvar$detfunc),
                            "half normal" = 0,
                            "hazard rate" = 1),
                        bearings = switch(
                            tclvalue(tvar$bearings.dist),
                            "none"           = 0,
                            "von mises"      = 1,
                            "wrapped cauchy" = 2),
                        distances = switch(
                            tclvalue(tvar$distances.dist),
                            "none"       = 0,
                            "gamma"      = 1,
                            "log-normal" = 2)
                    ),
                    mask = robj$mask,
                    fitting.options = list(
                        hessian = TRUE,
                        iterlim = 1000
                    ),
                    start = NULL,
                    trace = FALSE
                )
            })
        }, TRUE)

        # debugging
        if(0){
            capthist = robj$capthist
            model = formulas
            fixed = fixed
            model.options = list(
                detfunc = switch(
                    tclvalue(tvar$detfunc),
                    "half normal" = 0,
                    "hazard rate" = 1),
                bearings = switch(
                    tclvalue(tvar$bearings.dist),
                    "none"           = 0,
                    "von mises"      = 1,
                    "wrapped cauchy" = 2),
                distances = switch(
                    tclvalue(tvar$distances.dist),
                    "none"       = 0,
                    "gamma"      = 1,
                    "log-normal" = 2)
            )
            mask = robj$mask
            fitting.options = list(
                hessian = TRUE,
                iterlim = 1000
            )
            mask.options = list()
            start = NULL
            trace = FALSE
            debug = TRUE
        }

        # report results ----------------------------------------------------- #
        if(inherits(result, "try-error")){
            print_error(result)
        }else{
            model_summary()
        }
        refresh()
    }

    model_predict = function(){
        print_to_console("Predictions:\n", tag = "headingTag")
        print_to_console("\n")
        newdata = try({
            choose_newdata(robj$fit, padx = os$grid.padx)
        }, TRUE)
        if(inherits(newdata, "try-error")){
            print_error(newdata)
        }else{
            if(!is.null(newdata)){
                print_to_console("Prediction Data:")
                print_to_console(utils::capture.output(print(newdata)))
                print_to_console("\n")
            }
        }
        preds = try(predict(robj$fit, newdata = newdata), TRUE)
        if(inherits(preds, "try-error")){
            print_error(preds)
        }else{
            print_to_console("Point estimates and 95% confidence intervals\n\n")
            print_to_console(utils::capture.output(preds))
            # row.names = if(is.null(newdata)) FALSE else TRUE
            # print_to_console("Point estimates:")
            # print_to_console(utils::capture.output(
            #     print(as.data.frame(preds$est), row.names = row.names)))
            # if(!is.null(preds$lower)){
            #     print_to_console("\nLower 95% CL:")
            #     print_to_console(utils::capture.output(
            #         print(as.data.frame(preds$lower), row.names = row.names)))
            # }
            # if(!is.null(preds$upper)){
            #     print_to_console("\nUpper 95% CL:")
            #     print_to_console(utils::capture.output(
            #         print(as.data.frame(preds$upper), row.names = row.names)))
            # }
            print_dashes()
        }
    }

    model_summary = function(){
        result = try(utils::capture.output(robj$fit))
        if(inherits(result, "try-error")){
            print_error(result)
        }else{
            if(robj$fit$nlm$code >= 3){
                message = paste0("The fitting algorithm did not converge (nlm code = ",
                                 robj$fit$nlm$code, ")")
                print_error(message)
            }else if(robj$fit$nlm$iterations == 0){
                message = paste0("The fitting algorithm did not converge (n iterations = ",
                                 robj$fit$nlm$iterations, ")")
                print_error(message)
            }else{
                print_to_console(result, "Model fit summary:", dashes = TRUE)
            }
        }
    }

    null_command = function(){}

    open_manual_html = function(){
        browseURL("http://dkidney.github.io/gibbonsecr")
        # system(paste("open", system.file("doc/gui.html",
        # package = "gibbonsecr")))
    }

    download_manual_pdf = function(){
        browseURL("http://dkidney.github.io/gibbonsecr/docs/index.pdf")
        # system(paste("open", system.file("doc/gui.html",
        # package = "gibbonsecr")))
    }

    #     open_manual_pdf = function(){
    #         system(paste("open ", system.file("doc/gui.pdf",
    #                               package = "gibbonsecr")))
    #     }

    # par2 = function(i){
    #     par(oma = c(0,0,0,0), cex = 1, cex.main = 1, pty = "s")
    #     switch(i,
    #            par(mar = c(4.5,4.5,2.0,2.0)),
    #            par(mar = c(2.0,2.0,2.0,6.0)),
    #            par(mar = c(2.0,2.0,2.0,1.0))
    #     )
    # }

    # > plotting ---------------------------------------------------------------

    plot_shp_base = function(file){
        cursor('wait') ; on.exit(cursor('normal'))
        if(is.null(robj[[file]])){
            print_error('no data to plot')
        }else{
            title = basename(tclvalue(tvar[[paste0(file, ".path")]]))
            if(file == 'region'){
                device_popup()
                plot = ggplot() + coord_fixed() +
                    geom_shp(robj[[file]]) +
                    ggtitle(title) +
                    labs(x = "Longitude", y = "Latitude")
                print(plot)
            }else{
                for(j in colnames(robj[[file]]$sp@data)){
                    device_popup()
                    plot = ggplot() + coord_fixed() +
                        geom_shp(robj[[file]], covariate = j) +
                        ggtitle(title) +
                        labs(x = "Longitude", y = "Latitude")
                    if(!inherits(robj[[file]]$sp@data[[j]], c("character", "factor"))){
                        plot = plot +
                            scale_fill_distiller(palette = "Spectral")
                    }
                    print(plot)
                }
            }
        }
    }

    plot_shp_ = function(file){
        eval(parse(text = paste0("function() plot_shp_base('", file, "')")))
    }

    plot_mask_ = function(){
        cursor("wait") ; on.exit(cursor("normal"))
        base_plot = ggplot() + coord_fixed() +
            labs(x = "Longitude", y = "Latitude")
        if(is.null(secr::covariates(robj$mask[[1]]))){
            device_popup()
            plot = base_plot +
                geom_mask(robj$mask) +
                geom_capthist(robj$capthist, "arrays")
            if(!is.null(robj$region))
                plot = plot + geom_shp(robj$region)
            print(plot)
        }else{
            for(j in colnames(secr::covariates(robj$mask[[1]]))){
                device_popup()
                plot = base_plot +
                    geom_mask(robj$mask, covariate = j) +
                    geom_capthist(robj$capthist, "arrays")
                if(!is.null(robj$region))
                    plot = plot + geom_shp(robj$region)
                print(plot)
            }
        }
    }

    subtitle = function(method) paste0("(showing 95% ", if(method == "delta"){
        "delta method"
    }else{
        "parametric bootstrap"
    }, " confidence intervals", if(method == "delta") ")" else ", nboot = 999)")

    plot_detfunc = function(){
        newdata = try({
            choose_newdata(robj$fit, submodels = c("g0", "sigma"), all = FALSE,
                           padx = os$grid.padx)
        }, TRUE)
        device_popup()
        plot = ggplot() +
            geom_fit(robj$fit, "detfunc", newdata = newdata, ci = FALSE) +
            coord_cartesian(ylim = c(0, 1)) +
            scale_x_continuous(expand = c(0, 0)) +
            scale_y_continuous(expand = c(0, 0)) +
            labs(x = "Radial distance (m)", y = "Detection probability")
        title = "Detection function"
        if(tclvalue(tvar$detfunc.ci.method) != "none"){
            method = switch(tclvalue(tvar$detfunc.ci.method),
                            "bootstrap" = "boot", "delta")
            subtitle = subtitle(method)
            plot = plot +
                geom_fit(robj$fit, "detfunc", newdata = newdata, ci = TRUE,
                         method = method) +
                labs(title = bquote(atop(.(title), atop(italic(.(subtitle))))))
        }else{
            plot = plot + labs(title = title)
        }
        print(plot)
    }

    plot_bearings = function(){
        newdata = try({
            choose_newdata(robj$fit, submodels = "bearings", all = FALSE,
                           padx = os$grid.padx)
        }, TRUE)
        device_popup()
        plot = ggplot() +
            geom_fit(robj$fit, "bearings", newdata = newdata, ci = FALSE) +
            geom_vline(xintercept = 0, lty = 2) +
            scale_x_continuous(breaks = seq(-45, 45, 15)) +
            labs(x = "Bearing error (degrees)", y = "Probability density")
        title = "Bearing error distribution"
        if(tclvalue(tvar$bearings.ci.method) != "none"){
            method = switch(tclvalue(tvar$bearings.ci.method),
                            "bootstrap" = "boot", "delta")
            subtitle = subtitle(method)
            plot = plot +
                geom_fit(robj$fit, "bearings", newdata = newdata, ci = TRUE,
                         method = method) +
                labs(title = bquote(atop(.(title), atop(italic(.(subtitle))))))
        }else{
            plot = plot + labs(title = title)
        }
        print(plot)
    }

    plot_distances = function(){
        newdata = try({
            choose_newdata(robj$fit, submodels = "distances", all = FALSE,
                           padx = os$grid.padx)
        }, TRUE)
        true.dist = as.numeric(tclvalue(tvar$distances.truth))
        device_popup()
        plot = ggplot() +
            geom_fit(robj$fit, which = "distances", newdata = newdata,
                     ci = FALSE, true.distance = true.dist) +
            labs(x = "Estimated distance (metres)", y = "Probability density")
        title = "Estimated distances distribution"
        if(tclvalue(tvar$distances.ci.method) != "none"){
            method = switch(tclvalue(tvar$distances.ci.method),
                            "bootstrap" = "boot", "delta")
            subtitle = subtitle(method)
            plot = plot +
                geom_fit(robj$fit, which = "distances", newdata = newdata,
                         ci = TRUE, true.distance = true.dist, method = method) +
                labs(title = bquote(atop(.(title), atop(italic(.(subtitle))))))
        }else{
            plot = plot + labs(title = title)
        }
        plot = plot + geom_vline(xintercept = true.dist, lty = 2)
        print(plot)
    }

    plot_detsurf = function(){
        session = tclvalue(tvar$detsurf.array)
        # session = choose_array(robj$fit, padx = os$grid.padx)
        # if(!is.na(session)){
        device_popup()
        title = "Detection surface"
        subtitle = paste0("(array ", session, ")")
        plot = ggplot() + coord_fixed() +
            geom_fit(robj$fit, "detsurf", session = session) +
            scale_fill_distiller(palette = "Spectral", limits = c(0,1)) +
            geom_capthist(robj$fit$capthist, sessions = session, cex = 2) +
            labs(x = "Longitude", y = "Latitude",
                 title = bquote(atop(.(title), atop(italic(.(subtitle))))))
        if(tclvalue(tvar$detsurf.contour) == "1"){
            plot = plot +
                geom_fit(robj$fit, "detsurf", session = session, contour = TRUE)
        }
        print(plot)
        # }
    }

    plot_densurf = function(){
        session = tclvalue(tvar$densurf.array)
        device_popup()
        title = "Detection surface"
        subtitle = paste0("(using array-level covariates from array ", session, ")")
        plot = ggplot() + coord_fixed() +
            geom_fit(robj$fit, "densurf", session = session) +
            scale_fill_distiller(palette = "Spectral") +
            geom_capthist(robj$fit$capthist, "array") +
            labs(x = "Longitude", y = "Latitude",
                 title = bquote(atop(.(title), atop(italic(.(subtitle))))))
        if(tclvalue(tvar$densurf.contour) == "1")
            plot = plot +
                geom_fit(robj$fit, "densurf", session = session, contour = TRUE)
        if(!is.null(robj$region))
            plot = plot + geom_shp(robj$region)
        print(plot)
    }

    save_plots = function(){
        if(is.null(dev.list())){
            print_error("no plots to save")
        }else{
            device = "jpeg"
            units = "in"
            res = 300
            pointsize = 10
            for(i in dev.list()){
                dev.set(i)
                filename = file.path(robj$wd, paste0("plot_", i, ".", device))
                size = dev.size(units = units)
                dev.copy(eval(parse(text = device)), filename = filename,
                         width = size[1], height = size[2],
                         units = units, res = res, pointsize = pointsize)
                dev.off()
            }
            print_to_console(paste0("plots saved to working directory:\n",
                                    path.expand(robj$wd)), dashes = TRUE)
        }
    }

    close_plots = function(){
        for(i in dev.list()){
            dev.set(i)
            dev.off()
        }
    }

    # > console output ---------------------------------------------------------

    print_dashes = function(){
        print_to_console(paste(rep("-", 60), collapse = ""))
    }

    print_to_console = function(x, heading = NULL, tag = "normalTag", dashes = FALSE){
        x[length(x)] = paste0(gsub("\n$", "", x[length(x)]), "\n")
        tkconfigure(console, state = "normal")
        if(!is.null(heading)) tkinsert(console, "end", paste(heading, "\n\n"),
                                       "headingTag")
        tkinsert(console, "end", paste(x, collapse = "\n"), tag)
        tkconfigure(console, state = "disabled")
        tksee(console, "end")
        if(dashes) print_dashes()
    }

    print_error = function(message, dashes = TRUE){
        print_to_console(paste("Error:", gsub("Error : ", "", message)),
                         dashes = dashes, tag = "errorTag")
    }

    print_success = function(message, dashes = TRUE){
        print_to_console(message, dashes = dashes, tag = "successTag")
    }

    print_warning = function(message, dashes = FALSE){
        print_to_console(stringr::str_wrap(paste("Warning:", message), 60),
                         dashes = dashes, tag = "warningTag")
    }

    # > general ----------------------------------------------------------------

    refresh = function(){
        # this is run at the end of several functions
        # it updates the state of buttons and entry fields,
        # given the current data and results

        # start by disabling everything
        tkconfigure(tobj$data$summary, state = "disabled")
        for(i in names(tobj$mask))  tkconfigure(tobj$mask[[i]],  state = "disabled")
        for(i in names(tobj$model)) tkconfigure(tobj$model[[i]], state = "disabled")
        for(i in names(tobj$plots)) tkconfigure(tobj$plots[[i]], state = "disabled")

        ## if capthist exists
        if(!is.null(robj$capthist)){
            # enable data summary button
            # enable everything in the mask tabs except the summary and plot buttons
            tkconfigure(tobj$data$summary,      state = "normal")
            for(i in names(tobj$mask))  tkconfigure(tobj$mask[[i]],
                                                    state = "normal")
            tkconfigure(tobj$mask$summary,      state = "disabled")
            tkconfigure(tobj$mask$plot,         state = "disabled")
            # enable/disable region/habitat plot/check buttons
            for(i in c("region", "habitat1","habitat2")){
                state = if(is.null(robj[[i]])) "disabled" else "normal"
                tkconfigure(tobj$mask[[paste0(i, ".plot")]], state = state)
                tkconfigure(tobj$mask[[paste0(i, ".use")]], state = state)
            }
            # array names in detsurf plot
            sessions = session(robj$capthist)
            tkconfigure(tobj$plots$detsurf.array, values = sessions)
            if(!tclvalue(tvar$detsurf.array) %in% sessions)
                tclvalue(tvar$detsurf.array) = sessions[1]
            # array names in densurf plot
            tkconfigure(tobj$plots$densurf.array, values = sessions)
            if(!tclvalue(tvar$densurf.array) %in% sessions)
                tclvalue(tvar$densurf.array) = sessions[1]

            # if mask exists
            if(!is.null(robj$mask)){
                # enable mask summary and plot buttons
                # enable everything in the model tab
                tkconfigure(tobj$mask$summary,      state = "normal")
                tkconfigure(tobj$mask$plot,         state = "normal")
                for(i in names(tobj$model))
                    tkconfigure(tobj$model[[i]], state = "normal")
                tkconfigure(tobj$model$detfunc, state = "readonly")
                tkconfigure(tobj$model$bearings.dist, state = "readonly")
                tkconfigure(tobj$model$distances.dist, state = "readonly")
                # disable components of the model tab,
                # depending on the capthist and model options
                # - if no bearings/distances data, disable the model options combobox
                # - if no bearings/distances data or combobox set to "none",
                #   then disable formula and fixed
                for(i in c("bearings","distances")){ # i = "distances"
                    FUN = list(bearings = get_bearings, distances = get_distances)[[i]]
                    have.data = !is.null(FUN(robj$capthist))
                    if(!have.data)
                        tkconfigure(tobj$model[[paste0(i, ".dist")]],
                                    state = "disabled")
                    use.data = tclvalue(tvar[[paste0(i, ".dist")]]) != "none"
                    if(!have.data || !use.data){
                        for(j in c("formula", "fixed")){ # j = "formula"
                            tkconfigure(tobj$model[[paste0(i, ".", j)]],
                                        state = "disabled")
                            tkconfigure(tobj$model[[paste0(i, ".", j, ".radio")]],
                                        state = "disabled")
                        }
                    }
                }
                # for all submodels:
                # disable formula entry if radio is set to fixed
                # disable fixed entry if radio is set to formula
                for(i in submodels){ # i = "D"
                    if(tclvalue(tvar[[paste0(i, ".radio")]]) == "formula"){
                        tkconfigure(tobj$model[[paste0(i, ".fixed")]],
                                    state = "disabled")
                    }else{
                        tkconfigure(tobj$model[[paste0(i, ".formula")]],
                                    state = "disabled")
                    }
                }
                # disable g0 formula and fixed and set fixed to 1 if all n = 1
                if(all(n_occasions(robj$capthist) == 1)){
                    tkconfigure(tobj$model$g0.formula,       state = "disabled")
                    tkconfigure(tobj$model$g0.formula.radio, state = "disabled")
                    tkconfigure(tobj$model$g0.fixed,         state = "disabled")
                    tkconfigure(tobj$model$g0.fixed.radio,   state = "disabled")
                    tclvalue(tvar$g0.radio) = "fixed"
                    tclvalue(tvar$g0.fixed) = "1"
                }
                # disable pcall formula if all n = 1
                if(all(n_occasions(robj$capthist) == 1)){
                    tkconfigure(tobj$model$pcall.formula,       state = "disabled")
                    tkconfigure(tobj$model$pcall.formula.radio, state = "disabled")
                    tclvalue(tvar$pcall.radio) = "fixed"
                }
                # disable model summary and estimate buttons
                tkconfigure(tobj$model$summary, state = "disabled")
                tkconfigure(tobj$model$predict, state = "disabled")
                tkconfigure(tobj$model$coef,    state = "disabled")

                ## Fit
                if(!is.null(robj$fit)){
                    # fitted model exists
                    # enable model summary and estimate buttons
                    # enable everything in the plots tab
                    tkconfigure(tobj$model$summary, state = "normal")
                    tkconfigure(tobj$model$predict, state = "normal")
                    tkconfigure(tobj$model$coef,    state = "normal")
                    tkconfigure(tobj$model$predict, state = "normal")
                    for(i in names(tobj$plots))
                        tkconfigure(tobj$plots[[i]], state = "normal")
                    # disable components of the plots tab depending in the model
                    for(i in c("bearings", "distances")){ # i = "distances"
                        if(robj$fit$model.options[[i]] == 0){
                            tkconfigure(tobj$plots[[paste0(i, ".ci.method")]],
                                        state = "disabled")
                            tkconfigure(tobj$plots[[paste0(i, ".plot")]],
                                        state = "disabled")
                            if(i == "distances")
                                tkconfigure(tobj$plots[[paste0(i, ".truth")]],
                                            state = "disabled")
                        }
                    }
                }
            }
        }
    }






    tcl_args = function(x){
        y = sort(as.character(tkconfigure(console)))
        y = sapply(strsplit(y, " "), function(y) gsub("^-", "", y[1]))
        widget = deparse(substitute(x))
        do.call(rbind, lapply(y, function(i){
            text = paste0("tkcget(", widget, ", ", i, " = NULL)")
            value = try(tclvalue(eval(parse(text = text))), TRUE)
            if(inherits(value, "try-error")) value = NA
            c(i, value)
        }))
    }

    # > working directory ------------------------------------------------------

    wd_set = function(){
        wd = tclvalue(tkchooseDirectory())
        if(wd != ""){
            robj$wd = path.expand(wd)
            print_to_console(paste0("Working directory set to:\n",
                                    gsub("/","\\\\", robj$wd)), dashes = TRUE)
        }
        tkfocus(main.window)
    }

    wd_print = function(){
        print_to_console(paste0("Current working directory:\n",
                                gsub("/","\\\\", robj$wd)), dashes = TRUE)
    }

    # > workspace --------------------------------------------------------------

    workspace_clear = function(){
        response = tkmessageBox(
            title = "", icon = "warning", type = "okcancel", default = "cancel",
            message = "Clearing the workspace will delete all data and fitted models.\nClick OK to clear the workspace.")
        if(tclvalue(response) == "ok"){
            # tclvalue(tvar$detections.path) = ""
            # tclvalue(tvar$posts.path)      = ""
            # tclvalue(tvar$covariates.path) = ""
            # tclvalue(tvar$region.path)     = ""
            # tclvalue(tvar$habitat1.path)   = ""
            # tclvalue(tvar$habitat2.path)   = ""
            for(i in c(csv.files, shp.files))
                tclvalue(tvar[[paste0(i, ".path")]]) = ""
            for(i in submodels){
                tclvalue(tvar[[paste0(i, ".formula")]]) = ""
                value = if(i %in% c("g0", "pcall")) "1" else ""
                tclvalue(tvar[[paste0(i, ".fixed")]]) = value
            }
            robj$capthist = NULL
            robj$mask     = NULL
            robj$region   = NULL
            robj$habitat1 = NULL
            robj$habitat2 = NULL
            robj$fit      = NULL
            refresh()
        }
    }

    workspace_load_base = function(filename){
        print_to_console(paste0("Loading workspace:\n'", filename, "'"))
        if(file.exists(filename)){
            load(filename, envir = gui.env)
            for(i in names(gibbonsecr_workspace$robj)){
                if(i %in% names(robj))
                    robj[[i]] = gibbonsecr_workspace$robj[[i]]
            }
            for(i in names(gibbonsecr_workspace$tclvalues)){
                if(i %in% names(tvar))
                    tclvalue(tvar[[i]]) = gibbonsecr_workspace$tclvalues[[i]]
            }
            print_success("Load successful")
            refresh()
            tkfocus(main.window)
        }else{
            print_error("Load failed")
        }
    }

    workspace_load = function(){
        cursor("wait") ; on.exit(cursor("normal"))
        filename = tclvalue(tkgetOpenFile(
            initialdir = robj$wd,
            filetypes = "{{} {.rda}}"
        ))
        if(filename != ""){
            workspace_load_base(filename)
        }
    }

    workspace_save = function(){
        cursor("wait") ; on.exit(cursor("normal"))
        filename = tclvalue(tkgetSaveFile(
            initialdir = robj$wd,
            filetypes = "{{} {.rda}}"
        ))
        if(filename != ""){
            print_to_console("Saving workspace...")
            if(!tools::file_ext(filename) %in% "rda")
                filename = paste0(filename, ".rda")
            gibbonsecr_workspace = list(
                robj      = as.list(robj),
                tclvalues = sapply(tvar, tclvalue),
                version   = version
            )
            save(gibbonsecr_workspace, file = filename)
            print_to_console(paste("Workspace saved to:\n", filename),
                             dashes = TRUE)
        }
        tkfocus(main.window)
        return(filename)
    }

    # > examples ---------------------------------------------------------------

    load_N_annamensis_data = function(){
        folder = system.file("extdata/N.annamensis", package = "gibbonsecr")
        tclvalue(tvar$detections.path) = file.path(folder, "detections.csv")
        tclvalue(tvar$posts.path)      = file.path(folder, "posts.csv")
        tclvalue(tvar$covariates.path) = file.path(folder, "covariates.csv")
        tclvalue(tvar$bearings.units)  = "degrees"
        tclvalue(tvar$buffer)          = "6000"
        tclvalue(tvar$spacing)         = "250"
        tclvalue(tvar$region.path)     = file.path(folder, "region.shp")
        tclvalue(tvar$habitat1.path)   = file.path(folder, "habitat.shp")
        tclvalue(tvar$habitat2.path)   = ""
        tclvalue(tvar$region.use)      = ""
        tclvalue(tvar$habitat1.use)    = ""
        tclvalue(tvar$habitat2.use)    = ""
        robj$region   = NULL
        robj$habitat1 = NULL
        robj$habitat2 = NULL
        robj$mask     = NULL
        robj$fit      = NULL
        # data_import()
        refresh()
    }


    load_N_annamensis_workspace = function(){
        filename = "~/Dropbox/packages/gibbonsecr/gibbonsecr_1.0/gibbonsecr/testing/N.annamensis/N.annamensis.workspace.rda"
        workspace_load_base(filename)
    }

    # load_N_siki = function(){
    #     folder = "~/Dropbox/projects/gibbons/N.siki/data"
    #     tclvalue(tvar$detections.path) = file.path(folder, "detections.csv")
    #     tclvalue(tvar$posts.path)      = file.path(folder, "posts.csv")
    #     tclvalue(tvar$covariates.path) = file.path(folder, "covariates.csv")
    #     tclvalue(tvar$bearings.units)  = "degrees"
    #     tclvalue(tvar$distances.units) = "m"
    #     tclvalue(tvar$buffer)          = "5000"
    #     tclvalue(tvar$spacing)         = "250"
    #     tclvalue(tvar$region.path)     = ""
    #     tclvalue(tvar$habitat1.path)   = ""
    #     tclvalue(tvar$habitat2.path) = ""
    #     tclvalue(tvar$region.use)     = ""
    #     tclvalue(tvar$habitat1.use)    = ""
    #     tclvalue(tvar$habitat2.use)  = ""
    #     robj$region    = NULL
    #     robj$habitat1   = NULL
    #     robj$habitat2 = NULL
    #     robj$mask           = NULL
    #     robj$fit            = NULL
    #     data_import()
    #     refresh()
    # }

    # this should be disabled/removed/amended prior to release
    load_peafowl_data = function(){
        folder = "~/Dropbox/projects/greenpeafowl/data/dataframes"
        tclvalue(tvar$detections.path) = file.path(
            folder, "detections.averaged.calling.males.csv")
        tclvalue(tvar$posts.path)      = file.path(folder, "posts.csv")
        tclvalue(tvar$covariates.path) = file.path(folder, "covariates.csv")
        tclvalue(tvar$bearings.units)  = "degrees"
        tclvalue(tvar$distances.units) = "km"
        tclvalue(tvar$buffer)          = "1500"
        tclvalue(tvar$spacing)         = "100"
        tclvalue(tvar$region.path)     = ""
        folder = "~/Dropbox/projects/greenpeafowl/data/gis_layers/habitats_3000m"
        tclvalue(tvar$habitat1.path)   = file.path(
            folder, "all_habitats_3000m/all_habitats_3000m.shp")
        tclvalue(tvar$habitat2.path)   = file.path(
            folder, "distance_roads_river/dist_rds_vill.shp")
        tclvalue(tvar$region.use)   = ""
        tclvalue(tvar$habitat1.use) = ""
        tclvalue(tvar$habitat2.use) = ""
        robj$region   = NULL
        robj$habitat1 = NULL
        robj$habitat2 = NULL
        robj$mask     = NULL
        robj$fit      = NULL
        # data_import()
        refresh()
    }

    # this should be disabled/removed/amended prior to release
    load_peafowl_workspace = function(){
        filename = "~/Dropbox/packages/gibbonsecr/gibbonsecr_1.0/gibbonsecr/testing/peafowl/peafowl.workspace.rda"
        workspace_load_base(filename)
    }

    ## ###################################################################### ##
    ## ###################################################################### ##

    #                       ---- ** MAIN WINDOW ** ----

    # create main window
    main.window = tktoplevel(width = os$WIDTH, height = os$HEIGHT, bg = "white")
    add_icon(main.window)
    tkwm.title(main.window, paste0("gibbonsecr v", version))
    # put window in centre of computer screen
    screen.width = as.numeric(tclvalue(tkwinfo("screenwidth", main.window)))
    screen.height = as.numeric(tclvalue(tkwinfo("screenheight", main.window)))
    tkwm.geometry(main.window, paste0(os$WIDTH, "x", os$HEIGHT, "+",
                                      round((screen.width - os$WIDTH) / 2), "+",
                                      round((screen.height - os$HEIGHT) / 2)))
    # set minimum window size
    tkwm.minsize(main.window, os$min.width, os$min.height)
    # tkwm.minsize(main.window, os$lhs.width, os$min.height)
    # put notebook widget in left hand side for tabs
    lhs = ttknotebook(main.window, width = os$lhs.width, height = os$HEIGHT,
                      padding = c(0,0))
    # put frame on right hand side for output console
    rhs = tkframe(main.window, os$rhs.width, padding = c(10,10), relief = "flat")


    # DATA TAB =================================================================

    tab.data = tkframe(lhs)
    tkadd(lhs, tab.data, text = "Data", compound = "right")

    # > data csv files ---------------------------------------------------------

    add_heading(tab.data, "Data files")
    frame.data.csv = tkframe(tab.data)
    tkpack(frame.data.csv)
    tkgrid(tklabel(frame.data.csv, "Detections"), row = 1, column = 1)
    tkgrid(tklabel(frame.data.csv, "Posts"),      row = 2, column = 1)
    tkgrid(tklabel(frame.data.csv, "Covariates"), row = 3, column = 1)

    tobj$data$detections.path   = tkentry(frame.data.csv, tvar$detections.path,
                                          width = os$csv.entry.width)
    tobj$data$posts.path        = tkentry(frame.data.csv, tvar$posts.path,
                                          width = os$csv.entry.width)
    tobj$data$covariates.path   = tkentry(frame.data.csv, tvar$covariates.path,
                                          width = os$csv.entry.width)
    tobj$data$detections.browse = tkbutton(frame.data.csv, "...", browse("detections"),
                                           width = 2)
    tobj$data$posts.browse      = tkbutton(frame.data.csv, "...", browse("posts"),
                                           width = 2)
    tobj$data$covariates.browse = tkbutton(frame.data.csv, "...", browse("covariates"),
                                           width = 2)
    tobj$data$detections.view   = tkbutton(frame.data.csv, "View", view("detections"))
    tobj$data$posts.view        = tkbutton(frame.data.csv, "View", view("posts"))
    tobj$data$covariates.view   = tkbutton(frame.data.csv, "View", view("covariates"))
    tkgrid(tobj$data$detections.path,   row = 1, column = 2)
    tkgrid(tobj$data$posts.path,        row = 2, column = 2)
    tkgrid(tobj$data$covariates.path,   row = 3, column = 2)
    tkgrid(tobj$data$detections.browse, row = 1, column = 3)
    tkgrid(tobj$data$posts.browse,      row = 2, column = 3)
    tkgrid(tobj$data$covariates.browse, row = 3, column = 3)
    tkgrid(tobj$data$detections.view,   row = 1, column = 4)
    tkgrid(tobj$data$posts.view,        row = 2, column = 4)
    tkgrid(tobj$data$covariates.view,   row = 3, column = 4)

    # > data details  ----------------------------------------------------------

    add_separator(tab.data)
    add_heading(tab.data, "Data details")
    frame.data.details = tkframe(tab.data)
    tkpack(frame.data.details)
    tkgrid(tklabel(frame.data.details, "Bearings"),  row = 0, column = 1, sticky = "we")
    tkgrid(tklabel(frame.data.details, "Distances"), row = 0, column = 2, sticky = "we")
    tkgrid(tklabel(frame.data.details, "Units"),     row = 1, column = 0)
    tkgrid(tklabel(frame.data.details, "Type"),      row = 2, column = 0)
    tobj$data$bearings.units  = tkcombo(frame.data.details, tvar$bearings.units,
                                        values$bearings.units)
    tobj$data$bearings.type   = tkcombo(frame.data.details, tvar$bearings.type,
                                        values$bearings.type)
    tobj$data$distances.units = tkcombo(frame.data.details, tvar$distances.units,
                                        values$distances.units)
    tobj$data$distances.type  = tkcombo(frame.data.details, tvar$distances.type,
                                        values$distances.type)
    tkgrid(tobj$data$bearings.units,  row = 1, column = 1)
    tkgrid(tobj$data$bearings.type,   row = 2, column = 1)
    tkgrid(tobj$data$distances.units, row = 1, column = 2)
    tkgrid(tobj$data$distances.type,  row = 2, column = 2)

    # > data buttons -----------------------------------------------------------

    # import
    add_separator(tab.data)
    data.buttons.frame.1 = tkframe(tab.data)
    tkpack(data.buttons.frame.1)
    tobj$data$import  = tkbutton(data.buttons.frame.1, "Import",  data_import)
    tkgrid(tobj$data$import)
    # summary
    add_separator(tab.data)
    data.buttons.frame.2 = tkframe(tab.data)
    tkpack(data.buttons.frame.2)
    tobj$data$summary = tkbutton(data.buttons.frame.2, "Summary", data_summary)
    tkgrid(tobj$data$summary)

    # disable type
    tkconfigure(tobj$data$bearings.type,  state = "disabled")
    tkconfigure(tobj$data$distances.type, state = "disabled")


    # MASK TAB =================================================================

    tab.mask = tkframe(lhs)
    tkadd(lhs, tab.mask, text = "Mask", compound = "right")

    # > size and resolution  ---------------------------------------------------

    add_heading(tab.mask, "Area and resolution")
    frame.mask.details = tkframe(tab.mask)
    tkpack(frame.mask.details)
    label.buffer  = tklabel(frame.mask.details, "Buffer (m)")
    label.spacing = tklabel(frame.mask.details, "  Spacing (m)")
    tobj$mask$buffer  = tkentry(frame.mask.details, tvar$buffer,
                                width = os$fixed.entry.width)
    tobj$mask$spacing = tkentry(frame.mask.details, tvar$spacing,
                                width = os$fixed.entry.width)
    tkgrid(label.buffer,      row = 1, column = 1)
    tkgrid(tobj$mask$buffer,  row = 1, column = 2)
    tkgrid(label.spacing,     row = 1, column = 3)
    tkgrid(tobj$mask$spacing, row = 1, column = 4)

    # > shapefiles files -------------------------------------------------------

    add_separator(tab.mask)
    add_heading(tab.mask, "Shapefiles")
    frame.mask.shp = tkframe(tab.mask)
    tkpack(frame.mask.shp)
    for(i in shp.files){
        tobj$mask[[paste0(i, ".label")]]   = tklabel(
            parent       = frame.mask.shp,
            text         = tools::toTitleCase(i)
        )
        tobj$mask[[paste0(i, ".path")]]    = tkentry(
            parent       = frame.mask.shp,
            textvariable = tvar[[paste0(i, ".path")]]
        )
        tobj$mask[[paste0(i, ".browse")]]  = tkbutton(
            parent       = frame.mask.shp,
            text         = "...",
            command      = browse(i, "shp"),
            width        = 2
        )
        tobj$mask[[paste0(i, ".import")]]  = tkbutton(
            parent       = frame.mask.shp,
            text         = "Import",
            command      = shp_import(i)
        )
        tobj$mask[[paste0(i, ".plot")]]    = tkbutton(
            parent       = frame.mask.shp,
            text         = "Plot",
            command      = plot_shp_(i)
        )
        tobj$mask[[paste0(i, ".use")]]     = tkcheck(
            parent       = frame.mask.shp,
            variable     = tvar[[paste0(i, ".use")]]
        )
        row = which(i == shp.files)
        tkgrid(tobj$mask[[paste0(i, ".label")]],   row = row, column = 0)
        tkgrid(tobj$mask[[paste0(i, ".path")]],    row = row, column = 1)
        tkgrid(tobj$mask[[paste0(i, ".browse")]],  row = row, column = 2)
        tkgrid(tobj$mask[[paste0(i, ".import")]],  row = row, column = 3)
        tkgrid(tobj$mask[[paste0(i, ".plot")]],    row = row, column = 4)
        tkgrid(tobj$mask[[paste0(i, ".use")]],     row = row, column = 5)
    }

    # > mask buttons -----------------------------------------------------------

    # build
    add_separator(tab.mask)
    frame.mask.buttons.1 = tkframe(tab.mask)
    tkpack(frame.mask.buttons.1)
    tobj$mask$make    = tkbutton(frame.mask.buttons.1, "Build",   mask_make)
    tkgrid(tobj$mask$make)
    # summary and plot
    add_separator(tab.mask)
    frame.mask.buttons.2 = tkframe(tab.mask)
    tkpack(frame.mask.buttons.2)
    tobj$mask$summary = tkbutton(frame.mask.buttons.2, "Summary", mask_summary)
    tobj$mask$plot    = tkbutton(frame.mask.buttons.2, "Plot",    plot_mask_)
    tkgrid(tobj$mask$summary, tobj$mask$plot)


    # MODEL TAB ================================================================

    tab.model = tkframe(lhs)
    tkadd(lhs, tab.model, text = "Model", compound = "right")

    # > model options ----------------------------------------------------------

    add_heading(tab.model, "Model options")
    frame.model.options = tkframe(tab.model)
    tkpack(frame.model.options)
    label.detfunc        = tklabel(frame.model.options, "Detection function")
    label.bearings.dist  = tklabel(frame.model.options, "Bearings distribution")
    label.distances.dist = tklabel(frame.model.options, "Distances distribution ")
    tobj$model$detfunc        = tkcombo(frame.model.options, tvar$detfunc,
                                        values$detfunc)
    tobj$model$bearings.dist  = tkcombo(frame.model.options, tvar$bearings.dist,
                                        values$bearings.dist)
    tobj$model$distances.dist = tkcombo(frame.model.options, tvar$distances.dist,
                                        values$distances.dist)
    tkgrid(label.detfunc,        row = 1, column = 1)
    tkgrid(label.bearings.dist,  row = 2, column = 1)
    tkgrid(label.distances.dist, row = 3, column = 1)
    tkgrid(tobj$model$detfunc,        row = 1, column = 2)
    tkgrid(tobj$model$bearings.dist,  row = 2, column = 2)
    tkgrid(tobj$model$distances.dist, row = 3, column = 2)

    # > submodels --------------------------------------------------------------

    add_separator(tab.model)
    add_heading(tab.model, "Model parameters")
    frame.model.submodels = tkframe(tab.model)
    tkpack(frame.model.submodels)
    label.formula   = tklabel(frame.model.submodels, "Formula")
    label.fixed     = tklabel(frame.model.submodels, "Fixed")
    label.D         = tklabel(frame.model.submodels, "D")
    label.g0        = tklabel(frame.model.submodels, "g0")
    label.sigma     = tklabel(frame.model.submodels, "sigma")
    label.bearings  = tklabel(frame.model.submodels, "bearings")
    label.distances = tklabel(frame.model.submodels, "distances")
    label.pcall     = tklabel(frame.model.submodels, "pcall")
    tobj$model$D.formula         = tkentry(frame.model.submodels, tvar$D.formula)
    tobj$model$g0.formula        = tkentry(frame.model.submodels, tvar$g0.formula)
    tobj$model$sigma.formula     = tkentry(frame.model.submodels, tvar$sigma.formula)
    tobj$model$bearings.formula  = tkentry(frame.model.submodels, tvar$bearings.formula)
    tobj$model$distances.formula = tkentry(frame.model.submodels, tvar$distances.formula)
    tobj$model$pcall.formula     = tkentry(frame.model.submodels, tvar$pcall.formula)
    tobj$model$D.fixed           = tkentry(frame.model.submodels, tvar$D.fixed)
    tobj$model$g0.fixed          = tkentry(frame.model.submodels, tvar$g0.fixed)
    tobj$model$sigma.fixed       = tkentry(frame.model.submodels, tvar$sigma.fixed)
    tobj$model$bearings.fixed    = tkentry(frame.model.submodels, tvar$bearings.fixed)
    tobj$model$distances.fixed   = tkentry(frame.model.submodels, tvar$distances.fixed)
    tobj$model$pcall.fixed       = tkentry(frame.model.submodels, tvar$pcall.fixed)
    tobj$model$D.formula.radio         = tkradio(frame.model.submodels, "formula",
                                                 fixed_radio_command("D"))
    tobj$model$g0.formula.radio        = tkradio(frame.model.submodels, "formula",
                                                 fixed_radio_command("g0"))
    tobj$model$sigma.formula.radio     = tkradio(frame.model.submodels, "formula",
                                                 fixed_radio_command("sigma"))
    tobj$model$bearings.formula.radio  = tkradio(frame.model.submodels, "formula",
                                                 fixed_radio_command("bearings"))
    tobj$model$distances.formula.radio = tkradio(frame.model.submodels, "formula",
                                                 fixed_radio_command("distances"))
    tobj$model$pcall.formula.radio     = tkradio(frame.model.submodels, "formula",
                                                 fixed_radio_command("pcall"))
    tobj$model$D.fixed.radio           = tkradio(frame.model.submodels, "fixed",
                                                 fixed_radio_command("D"))
    tobj$model$g0.fixed.radio          = tkradio(frame.model.submodels, "fixed",
                                                 fixed_radio_command("g0"))
    tobj$model$sigma.fixed.radio       = tkradio(frame.model.submodels, "fixed",
                                                 fixed_radio_command("sigma"))
    tobj$model$bearings.fixed.radio    = tkradio(frame.model.submodels, "fixed",
                                                 fixed_radio_command("bearings"))
    tobj$model$distances.fixed.radio   = tkradio(frame.model.submodels, "fixed",
                                                 fixed_radio_command("distances"))
    tobj$model$pcall.fixed.radio       = tkradio(frame.model.submodels, "fixed",
                                                 fixed_radio_command("pcall"))
    tkgrid(label.formula,                      row = 0, column = 3, sticky = "we")
    tkgrid(label.fixed,                        row = 0, column = 5, sticky = "we")
    tkgrid(label.D,                            row = 1, column = 1)
    tkgrid(label.g0,                           row = 2, column = 1)
    tkgrid(label.sigma,                        row = 3, column = 1)
    tkgrid(label.bearings,                     row = 4, column = 1)
    tkgrid(label.distances,                    row = 5, column = 1)
    tkgrid(label.pcall,                        row = 6, column = 1)
    for(i in 1:6) tkgrid(tklabel(frame.model.submodels, "~"), row = i, column = 2)
    tkgrid(tobj$model$D.formula,               row = 1, column = 3)
    tkgrid(tobj$model$g0.formula,              row = 2, column = 3)
    tkgrid(tobj$model$sigma.formula,           row = 3, column = 3)
    tkgrid(tobj$model$bearings.formula,        row = 4, column = 3)
    tkgrid(tobj$model$distances.formula,       row = 5, column = 3)
    tkgrid(tobj$model$pcall.formula,           row = 6, column = 3)
    tkgrid(tobj$model$D.formula.radio,         row = 1, column = 4)
    tkgrid(tobj$model$g0.formula.radio,        row = 2, column = 4)
    tkgrid(tobj$model$sigma.formula.radio,     row = 3, column = 4)
    tkgrid(tobj$model$bearings.formula.radio,  row = 4, column = 4)
    tkgrid(tobj$model$distances.formula.radio, row = 5, column = 4)
    tkgrid(tobj$model$pcall.formula.radio,     row = 6, column = 4)
    tkgrid(tobj$model$D.fixed,                 row = 1, column = 5)
    tkgrid(tobj$model$g0.fixed,                row = 2, column = 5)
    tkgrid(tobj$model$sigma.fixed,             row = 3, column = 5)
    tkgrid(tobj$model$bearings.fixed,          row = 4, column = 5)
    tkgrid(tobj$model$distances.fixed,         row = 5, column = 5)
    tkgrid(tobj$model$pcall.fixed,             row = 6, column = 5)
    tkgrid(tobj$model$D.fixed.radio,           row = 1, column = 6)
    tkgrid(tobj$model$g0.fixed.radio,          row = 2, column = 6)
    tkgrid(tobj$model$sigma.fixed.radio,       row = 3, column = 6)
    tkgrid(tobj$model$bearings.fixed.radio,    row = 4, column = 6)
    tkgrid(tobj$model$distances.fixed.radio,   row = 5, column = 6)
    tkgrid(tobj$model$pcall.fixed.radio,       row = 6, column = 6)

    # configure entry width and radio button variables
    for(i in submodels){
        for(j in c("formula","fixed")){
            tkconfigure(tobj$model[[paste0(i, ".", j)]],
                        width = os[[paste0(j, ".entry.width")]])
            tkconfigure(tobj$model[[paste(i, j, "radio", sep = ".")]],
                        variable = tvar[[paste(i, "radio", sep = ".")]])
        }
    }

    # > model fit buttons ------------------------------------------------------

    # fit
    add_separator(tab.model)
    frame.model.buttons.1 = tkframe(tab.model)
    tkpack(frame.model.buttons.1)
    tobj$model$fit     = tkbutton(frame.model.buttons.1, "Fit",     model_fit)
    tkgrid(tobj$model$fit)
    # summary, predict, coef
    add_separator(tab.model)
    frame.model.buttons.2 = tkframe(tab.model)
    tkpack(frame.model.buttons.2)
    tobj$model$summary = tkbutton(frame.model.buttons.2, "Summary", model_summary)
    tobj$model$predict = tkbutton(frame.model.buttons.2, "Predict", model_predict)
    tobj$model$coef    = tkbutton(frame.model.buttons.2, "Coef",    model_coef)
    tkgrid(tobj$model$summary, tobj$model$coef, tobj$model$predict)


    # PLOTS TAB ================================================================

    tab.plots = tkframe(lhs)
    tkadd(lhs, tab.plots, text = "Plots", compound = "right")

    # > detection function -----------------------------------------------------

    add_heading(tab.plots, "Detection function")
    frame.plots.detfunc  = tkframe(tab.plots, padding = c(0,0))
    sframe.plots.detfunc = tkframe(frame.plots.detfunc, padding = c(0,0))
    tkpack(frame.plots.detfunc, fill = "both")
    tkpack(sframe.plots.detfunc, side = "left")
    label.detfunc.ci.method = tklabel(sframe.plots.detfunc, "95% CI method:")
    tobj$plots$detfunc.ci.method = tkcombo(sframe.plots.detfunc, tvar$detfunc.ci.method,
                                           values = values$ci.method)
    tobj$plots$detfunc.plot      = tkbutton(frame.plots.detfunc, "Plot", plot_detfunc)
    tkgrid(label.detfunc.ci.method,      row = 1, column = 1)
    tkgrid(tobj$plots$detfunc.ci.method, row = 1, column = 2)
    tkpack(tobj$plots$detfunc.plot, side = "right")

    # > bearings ---------------------------------------------------------------

    add_separator(tab.plots)
    add_heading(tab.plots, "Bearing error distribution")
    frame.plots.bearings  = tkframe(tab.plots, padding = c(0,0))
    sframe.plots.bearings = tkframe(frame.plots.bearings, padding = c(0,0))
    tkpack(frame.plots.bearings, fill = "both")
    tkpack(sframe.plots.bearings, side = "left")
    label.bearings.ci.method = tklabel(sframe.plots.bearings, "95% CI method:")
    tobj$plots$bearings.ci.method = tkcombo(sframe.plots.bearings,
                                            tvar$bearings.ci.method,
                                            values$ci.method)
    tobj$plots$bearings.plot      = tkbutton(frame.plots.bearings, "Plot", plot_bearings)
    tkgrid(label.bearings.ci.method,      row = 1, column = 1)
    tkgrid(tobj$plots$bearings.ci.method, row = 1, column = 2)
    tkpack(tobj$plots$bearings.plot, side = "right")

    # > distances --------------------------------------------------------------

    add_separator(tab.plots)
    add_heading(tab.plots, "Distance estimates distribution")
    frame.plots.distances  = tkframe(tab.plots, padding = c(0,0))
    sframe.plots.distances = tkframe(frame.plots.distances, padding = c(0,0))
    tkpack(frame.plots.distances, fill = "both")
    tkpack(sframe.plots.distances, side = "left")
    # ci method
    label.distances.ci.method = tklabel(sframe.plots.distances, "95% CI method:")
    tobj$plots$distances.ci.method = tkcombo(sframe.plots.distances,
                                             tvar$distances.ci.method,
                                             values$ci.method)
    tkgrid(label.distances.ci.method,      row = 1, column = 1)
    tkgrid(tobj$plots$distances.ci.method, row = 1, column = 2)
    # true distance
    label.distances.truth      = tklabel(sframe.plots.distances, "True dist. (m):")
    tobj$plots$distances.truth = tkentry(sframe.plots.distances,
                                         tvar$distances.truth,
                                         width = os$combo.width)
    tkgrid(label.distances.truth,      row = 2, column = 1)
    tkgrid(tobj$plots$distances.truth, row = 2, column = 2)
    # plot button
    tobj$plots$distances.plot      = tkbutton(frame.plots.distances, "Plot",
                                              plot_distances)
    tkpack(tobj$plots$distances.plot, side = "right")

    # > detection surface --------------------------------------------------------------

    add_separator(tab.plots)
    add_heading(tab.plots, "Detection surface")
    frame.plots.detsurf  = tkframe(tab.plots, padding = c(0,0))
    sframe.plots.detsurf = tkframe(frame.plots.detsurf, padding = c(0,0))
    tkpack(frame.plots.detsurf, fill = "both")
    tkpack(sframe.plots.detsurf, side = "left")
    # array
    label.detsurf.array        = tklabel(sframe.plots.detsurf, "Array:")
    tobj$plots$detsurf.array   = tkcombo(sframe.plots.detsurf,
                                         tvar$detsurf.array,
                                         robj$array.names)
    tkgrid(label.detsurf.array,      row = 1, column = 1)
    tkgrid(tobj$plots$detsurf.array, row = 1, column = 2)
    # contour
    label.detsurf.contour      = tklabel(sframe.plots.detsurf, "Contour lines:")
    tobj$plots$detsurf.contour = tkcheck(sframe.plots.detsurf, tvar$detsurf.contour)
    tkgrid(label.detsurf.contour,      row = 2, column = 1)
    tkgrid(tobj$plots$detsurf.contour, row = 2, column = 2)
    # plot button
    tobj$plots$detsurf.plot    = tkbutton(frame.plots.detsurf, "Plot", plot_detsurf)
    tkpack(tobj$plots$detsurf.plot, side = "right")

    # > density surface --------------------------------------------------------

    add_separator(tab.plots)
    add_heading(tab.plots, "Density surface")
    frame.plots.densurf  = tkframe(tab.plots, padding = c(0,0))
    sframe.plots.densurf = tkframe(frame.plots.densurf, padding = c(0,0))
    tkpack(frame.plots.densurf, fill = "both")
    tkpack(sframe.plots.densurf, side = "left")
    # array
    label.densurf.array        = tklabel(sframe.plots.densurf, "Array:")
    tobj$plots$densurf.array   = tkcombo(sframe.plots.densurf,
                                         tvar$densurf.array,
                                         robj$array.names)
    tkgrid(label.densurf.array,      row = 1, column = 1)
    tkgrid(tobj$plots$densurf.array, row = 1, column = 2)
    # contour
    label.densurf.contour      = tklabel(sframe.plots.densurf, "Contour lines:")
    tobj$plots$densurf.contour = tkcheck(sframe.plots.densurf, tvar$densurf.contour)
    tkgrid(label.densurf.contour,      row = 2, column = 1)
    tkgrid(tobj$plots$densurf.contour, row = 2, column = 2)
    # plot button
    tobj$plots$densurf.plot    = tkbutton(frame.plots.densurf, "Plot", plot_densurf)
    tkpack(tobj$plots$densurf.plot, side = "right")

    # > save plots -------------------------------------------------------------

    add_separator(tab.plots)
    frame.plots.buttons = tkframe(tab.plots)
    tkpack(frame.plots.buttons)
    tobj$plots$save = tkbutton(frame.plots.buttons, "Save", save_plots)
    tobj$plots$close = tkbutton(frame.plots.buttons, "Close", close_plots)
    tkgrid(tobj$plots$save, tobj$plots$close)


    # CONSOLE ==================================================================

    console = tktext(rhs, fg = os$console.fg, bg = os$console.bg, borderwidth = 1,
                     padx = 10, pady = 10, relief = "sunken")
    scrollbar = ttkscrollbar(rhs, command = function(...) tkyview(console, ...))
    tkconfigure(console, yscrollcommand = function(...) tkset(scrollbar, ...),
                state = "disabled")
    tkpack(scrollbar, side = "right", fill = "y")
    tkpack(console, fill = "both", expand = TRUE, padx = 0, pady = 0)
    tcl("tk_textCopy", console)

    # font tags
    tktag.configure(console, "normalTag",  foreground = os$console.fg,
                    font = os$console.normal.font)
    tktag.configure(console, "headingTag", foreground = os$console.fg,
                    font = os$console.heading.font)
    tktag.configure(console, "successTag", foreground = "springgreen3",
                    font = os$console.normal.font)
    tktag.configure(console, "warningTag", foreground = "orange",
                    font = os$console.normal.font)
    tktag.configure(console, "errorTag",   foreground = "firebrick1",
                    font = os$console.normal.font)

    # left click to open console popup menu
    tkbind(console, "<ButtonPress-3>", console_popup)


    # MENU BAR =================================================================

    menu = list()
    menu$main = tkmenu(main.window)
    tkconfigure(main.window, menu = menu$main)

    # > help menu --------------------------------------------------------------

    menu$help = tkmenu(menu$main, tearoff = FALSE)
    tkadd(menu$main, "cascade", label = "Help", menu = menu$help)
    # example data
    tkadd(menu$help, "command", label = "Example data - N.annamensis",
          command = load_N_annamensis_data)
    tkadd(menu$help, "command", label = "Example data - peafowl",
          command = load_peafowl_data)
    # manual
    tkadd(menu$help, "separator")
    tkadd(menu$help, "command", label = "Online manual (html)",
          command = open_manual_html)
    tkadd(menu$help, "command", label = "Online manual (pdf)",
          command = download_manual_pdf)
    # about
    tkadd(menu$help, "separator")
    tkadd(menu$help, "command", label = "About gibbonsecr", command = about)

    # > workspace menu ---------------------------------------------------------

    menu$workspace = tkmenu(menu$main, tearoff = FALSE)
    tkadd(menu$main, "cascade", label = "Workspace", menu = menu$workspace)
    tkadd(menu$workspace, "command", label = "Save workspace",
          command = workspace_save, accelerator = "CTRL+S")
    tkadd(menu$workspace, "command", label = "Load workspace",
          command = workspace_load, accelerator = "CTRL+L")
    tkadd(menu$workspace, "command", label = "Clear workspace",
          command = workspace_clear)
    # example workspaces
    tkadd(menu$workspace, "separator")
    tkadd(menu$workspace, "command", label = "Example workspace - N.annamensis",
          command = load_N_annamensis_workspace)
    tkadd(menu$workspace, "command", label = "Example workspace - peafowl",
          command = load_peafowl_workspace)
    # woking directory
    tkadd(menu$workspace, "separator")
    tkadd(menu$workspace, "command", label = "Set working directory",
          command = wd_set)
    tkadd(menu$workspace, "command", label = "Print working directory",
          command = wd_print)

    for(i in 1:length(menu)){
        tkconfigure(menu[[i]],
                    activebackground   = "#0064FF",
                    activeborderwidth  = 0,
                    activeforeground   = "white",
                    background         = "white",
                    borderwidth        = 0,
                    disabledforeground = "grey85",
                    foreground         = "black",
                    relief             = "flat"
        )
    }


    # TAGS AND BINDINGS  =======================================================

    tkwm.protocol(main.window, "WM_DELETE_WINDOW", exit_prompt)
    # tkbind(main.window, "<Control-q>", exit.prompt)
    tkbind(main.window, "<Control-s>", workspace_save)
    tkbind(main.window, "<Control-l>", workspace_load)
    # make sure that refresh is called if combo.model is changed by the user
    tkbind(tobj$model$bearings.dist,  "<<ComboboxSelected>>", refresh)
    tkbind(tobj$model$distances.dist, "<<ComboboxSelected>>", refresh)
    # tkbind(main.window, "d", function() tcl(lhs, "select", 0))
    # tkbind(main.window, "m", function() tcl(lhs, "select", 1))
    # tkbind(main.window, "p", function() tcl(lhs, "select", 2))


    # TOOLTIPS =================================================================

    # mask tab
    tk2tip(label.buffer, stringr::str_wrap("The radius of a buffer region around each array of listening posts.\n This defines an area beyond which the detection probability can be assumed to be zero."))
    tk2tip(label.spacing, stringr::str_wrap("The spacing between neighbouring grid points in the mask."))
    tk2tip(tobj$mask[["region.label"]], stringr::str_wrap("Polygon .shp file defining survey boundary"))
    tk2tip(tobj$mask[["habitat1.label"]], stringr::str_wrap("Polygon or point .shp file containing spatial covariates"))
    tk2tip(tobj$mask[["habitat2.label"]], stringr::str_wrap("Polygon or point .shp file containing spatial covariates"))

    # model tab
    tk2tip(label.detfunc, stringr::str_wrap("A curve that describes the relationship between detection probability (of a calling group) and distance from listening post."))
    tk2tip(label.bearings.dist,  stringr::str_wrap("The distribution for the estimated bearings."))
    tk2tip(label.distances.dist, stringr::str_wrap("The distribution for the estimated distances."))
    tk2tip(label.D, stringr::str_wrap("The number of groups per square kilometre."))
    tk2tip(label.g0, stringr::str_wrap("The detection function intercept parameter. This gives the probability of hearing a calling group whose activity centre is zero distance from the listening post."))
    tk2tip(label.sigma, stringr::str_wrap("The detection function scale parameter. This defines the width of the detection function (larger values = wider detection functions)."))
    tk2tip(label.bearings, stringr::str_wrap("The parameter of the distribution for the estimated bearings. This defines the spread of the distribution (larger values = narrower distributions = more accurate estimates)"))
    tk2tip(label.distances, stringr::str_wrap("The parameter of the distribution for the estimated distances. This defines the spread of the distribution (XXX values = XXX distributions = more accurate estimates) ."))
    tk2tip(label.pcall, stringr::str_wrap("The probability of a group calling on a given day. Alternatively, the proportion of groups which call on a given day."))


    # PACKING  =================================================================

    tkpack(lhs, fill = "both", side = "left")
    tkpack(rhs, fill = "both", expand = TRUE, side  = "left")

    # make sure window opens on top of other windows (but doesn't forcibly remain on top)
    tcl("wm", "attributes", main.window, topmost = TRUE)
    tcl("wm", "attributes", main.window, topmost = FALSE)
    # tcl("wm", "attributes", main.window, fullscreen = TRUE)

    tkwm.deiconify(main.window)
    tkgrab.set(main.window)
    tkfocus(main.window)
    tkgrab.release(main.window)

    if(0){
        # entry
        a = tcl_args(tobj$data$posts.path) ; a
        # combo
        b = tcl_args(tobj$data$bearings.units) ; b
        # button
        c = tcl_args(tobj$data$posts.view) ; c
        # radio
        d = tcl_args(tobj$model$pcall.fixed.radio) ; d
        # check
        e = tcl_args(tobj$mask$region.use) ; e
        # text
        f = tcl_args(console) ; f
        # scrollbar
        g = tcl_args(scrollbar) ; g

        tkconfigure(console, selectbackground = "#0064FF", selectforeground = "white")

        cat(paste0("\"", paste(sort(unique(c(a, b, c, d, e))), collapse = "\", \""), "\""))

        # tkconfigure(tobj$data$posts.path, validate = "focus", validatecommand = validate_entry("posts"))

    }

    refresh()
    print_dashes()
    print_to_console(welcome_message())
    print_dashes()

    invisible()

}
