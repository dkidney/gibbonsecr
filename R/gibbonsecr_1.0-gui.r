## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##
##                                                                            ##
##                               gibbonsecr GUI                               ##
##                                                                            ##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# mac users might need to install XQuartz from xquartz.macosforge.org
# download XQuartz-2.7.7.dmg

#' @title Launch the graphical user interface
#' @description Graphical user interface for fitting acoustic gibbon survey data using
#'   the gibbonsSECR package
#' @details Currently allows the inclusion of the following components:
#'   \itemize{
#'
#'   \item{half normal and hazard rate detection function (with or without g0)}
#'
#'   \item{von Mises and wrapped Cauchy distributions for estimated bearings}
#'
#'   \item{gamma and log-normal distributions for estimated distances}
#'
#'   \item{calling probability for multi-occasion surveys (i.e. surveys with at
#'   least two consecutive days of data at each set of listening posts, with
#'   recaptures made across days)}
#'
#'   \item{covariates for all model parameters except the hazard rate shape
#'   parameter (z)}
#'
#'   \item{saving and reloading of workspace}
#'
#'   }
#' @param prompt.save.on.exit logical scalar, determining whether a save workspace prompt message should be shown when the GUI window is closed (defaults to \code{FALSE})
#' @param quit.r.on.exit a logical scalar, determining whether the background R process should quit when the GUI window is closed (defaults to \code{FALSE})
#' @section Things still to be added:
#'
#'   \itemize{
#'
#'   \item{binned distance estimates}
#'
#'   \item{survey region and habitat polygons}
#'
#'   \item{spline-based models for the density surface}
#'
#'   \item{manual}
#'
#'   }
#'
# @details Built using the \code{\link{tcltk}} package.
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @export
#' @import tcltk
#' @importFrom tcltk2 tk2tip

gibbonsecr_gui = function(prompt.save.on.exit = FALSE, quit.r.on.exit = FALSE){

#     # only uncomment these lines if sourcing
#     # rm(list = ls())
#     prompt.save.on.exit = FALSE
#     quit.r.on.exit = FALSE
#     library(secr)
#     library(tcltk)
#     library(tcltk2)
#     library(gibbonsecr)

    ## ###################################################################### ##
    ## ###################################################################### ##

    #                       ---- Options and Appearance ----

    # gui = environment()
    # platform = .Platform$OS.type

    # plotting device
    # op = options()
    # options(device = switch(platform, windows = "windows", "x11"))
    # on.exit(options(op))

    # appearance
    # os is an environment containing appearance settings (dimensions, fonts, etc.)
    os = gui_appearance_settings()

    ## ###################################################################### ##
    ## ###################################################################### ##

    #                             ---- Functions ----

    about = function(){
        tkmessageBox(title = "About gibbonsecr", message = "This is a pre-release version of the software. If you notice any bugs or have any general queries, please email Darren Kidney at darrenkidney@googlemail.com", icon = "info", type = "ok")
        tkfocus(main.window)
    }

    add_heading = function(parent, text){
        tkpack(tklabel(parent, text = text, font = os$heading.font, anchor = "w"), side = "top", fill = "x")
    }

    add_icon = function(window){
        if(.Platform$OS.type == "windows"){
            tcl('wm', 'iconbitmap', window, system.file('doc/icon/gibbonsecr.ico', package = "gibbonsecr"))
        }else{
            tcl('wm', 'iconphoto', window, tcl('image', 'create', 'photo', '-file', system.file('doc/icon/gibbonsecr.png', package = "gibbonsecr")))
        }
    }

    add_separator = function(parent){
        tkpack(ttkseparator(parent, orient = "horizontal"), anchor = "center", fill = "x", pady = os$separator.pady)
    }

    clear_console = function(){
        tkconfigure(console, state = "normal")
        tkdelete(console, "1.0", "end")
        tkconfigure(console, state = "disabled")
    }

    console_popup = function(){
        console.popup.menu = tkmenu(console, tearoff = FALSE)
        tkadd(console.popup.menu, "command", label = "Clear console", command = clear_console, state = "normal")
        x = tkwinfo("pointerx", main.window)
        y = tkwinfo("pointery", main.window)
        tkpopup(console.popup.menu, x, y)
    }

    data_import = function(){
        result = try({
            utils::capture.output({
                env$capthist = import_data(
                    detections = tclvalue(env$csv.filepath.entry.value[["detections"]]),
                    posts      = tclvalue(env$csv.filepath.entry.value[["posts"]]),
                    covariates = tclvalue(env$csv.filepath.entry.value[["covariates"]]),
                    details = list(
                        bearings  = list(
                            units = tclvalue(env$data.details.value$bearings$units),
                            type  = tclvalue(env$data.details.value$bearings$type)
                        ),
                        distances = list(
                            units = tclvalue(env$data.details.value$distances$units),
                            type  = tclvalue(env$data.details.value$distances$type)
                        )
                    )
                )
            })
        }, TRUE)
        if(inherits(result, "try-error")){
            error_message(result)
        }else{
            data_summary()
            refresh()
        }
    }

    data_summary = function(){
        result = utils::capture.output(summary_capthist(env$capthist))
        if(inherits(result, "try-error")){
            error_message(result)
        }else{
            print_to_console(result, "Data summary:")
        }
    }

    device_popup = eval(parse(text = switch(.Platform$OS.type, windows = "windows", "x11")))

    error_message = function(message, dashes = FALSE){
        print_to_console(paste("Error :", gsub("Error : ", "", message)), dashes = dashes, tag = "errorTag")
    }

    exit_prompt = function(){
        # options(op)
        # if(prompt.save.on.exit){
#             response = tkmessageBox(title = "", message = "Save workspace before quitting?", icon = "question", type = "yesnocancel", default = "no")
#             switch(tclvalue(response),
#                    "yes" = {
#                        filename = workspace_save()
#                        if(nchar(filename) > 0) tkdestroy(main.window)
#                    },
#                    "no" = tkdestroy(main.window)
#             )
#         }
        tkdestroy(main.window)
        if(quit.r.on.exit) q()
    }

    make_command_browse = function(component, ext = "csv"){
        eval(parse(text = paste0("
        function(){
            filepath = tclvalue(tkgetOpenFile(filetypes = '{{Comma delimited} {.", ext, "}}'))
            if(nchar(filepath) > 0){
                env$", ext, ".filepath.entry.value[['",component,"']] = tclVar(filepath)
                tkdelete(", ext, ".filepath.entry[['",component,"']], '0', 'insert')
                tkinsert(", ext, ".filepath.entry[['",component,"']], '0', filepath)
            }
        }")))
    }

    make_command_csv_view = function(component){
        eval(parse(text = paste0("
        function(){
            system(paste('open ', tclvalue(env$csv.filepath.entry.value[['", component, "']])))
        }"
        )))
    }

    make_command_open_example_file = function(component){
        eval(parse(text = paste0("
        function(){
            system(paste('open ', system.file(paste0('extdata/N.annamensis/example_', component, '_file.csv'), package = 'gibbonsecr')))
        }"
        )))
    }

    make_command_model_formula_radiobutton = function(i){
        eval(parse(text = paste0("
        function(){
            tkconfigure(model.formula.entry[['",i,"']], state = 'normal')
            tkconfigure(model.fixed.entry[['",i,"']], state = 'disable')
            refresh()
        }")))
    }

    make_command_model_fixed_radiobutton = function(i){
        eval(parse(text = paste0("
        function(){
            tkconfigure(model.formula.entry[['",i,"']], state = 'disable')
            tkconfigure(model.fixed.entry[['",i,"']], state = 'normal')
            refresh()
        }")))
    }

    make_command_shp_check = function(shp){
        eval(parse(text = paste0("
        function(){
            result = try(utils::capture.output({
                env$shp[['", shp, "']] = import_shp(tclvalue(env$shp.filepath.entry.value[['", shp, "']]), type = '", shp, "')
            }))
            if(inherits(result, 'try-error')){
                error_message(result)
            }else{
                result = try(utils::capture.output({
                    check_shp(env$shp[['", shp, "']], env$capthist)
                }))
                if(inherits(result, 'try-error')){
                    error_message(result)
                }else{
                    print_to_console('", shp, " file ok', tag = 'successTag', dashes = FALSE)
                    device_popup()
                    par(mar = c(3,3,1,1))
                    plot_shp(env$shp[['", shp, "']], env$capthist)
                }
            }
        }"
        )))
    }

    mask_check = function(){
        # triggered by the mask_make button
        buffer  = mask_buffer(env$mask, traps(env$capthist))
        spacing = mask_spacing(env$mask)
        # M       = round(mean(mask_npoints(env$mask)))
        small.spacing = spacing < 100
        large.spacing = spacing >= 500
        small.buffer  = buffer <= 3000
        large.buffer  = buffer > 6000
        if(large.spacing || small.spacing || small.buffer || large.buffer){
            if(small.spacing)
                warning_message("models using a smaller mask spacing should give more reliable results but will take longer to fit.")
            if(large.spacing)
                warning_message("models using a larger mask spacing will be quicker to fit but may give unreliable results.")
            if(small.buffer)
                warning_message("models using a smaller mask buffer will be quicker to fit but may give unreliable results.")
            if(large.buffer)
                warning_message("Warning: models using a larger mask buffer should give more reliable results but will take longer to fit.")
        }
        if(!is.null(covariates(env$mask[[1]]))){
            missing = sapply(covariates(env$mask), function(x){
                any(apply(x, 1, function(x){
                    any(is.na(x))
                }))
            })
            if(any(any(missing))){
                warning_message("some mask points have missing covariate values and will be removed")
            }
        }
        print_dashes()
    }

    mask_make = function(){
        # region
        env$shp$region = if(tclvalue(env$shp.file.checkbox.value[["region"]]) == "1"){
            import_shp(tclvalue(env$shp.filepath.entry.value[["region"]]),
                       type = "region")
        }else NULL
        # mask
        env$mask = make.mask(
            traps   = traps(env$capthist),
            buffer  = as.numeric(tclvalue(env$mask.buffer.entry.value)),
            spacing = as.numeric(tclvalue(env$mask.spacing.entry.value)),
            type    = "trapbuffer",
            poly    = env$shp$region
        )
        # habitat
        env$shp$habitat = if(tclvalue(env$shp.file.checkbox.value[["habitat"]]) == "1"){
            import_shp(tclvalue(env$shp.filepath.entry.value[["habitat"]]),
                       type = "habitat")
        }else NULL
        if(!is.null(env$shp$habitat)){
            env$mask = addCovariates(env$mask, env$shp$habitat)
            # check for 'habtiat' in trap covariates
            trapcov = covlevels(env$capthist)$trapcov
            conflict = any(trapcov == "habitat") ; conflict
            if(conflict) warning_message("'habitat' already exists in trap covariates and will be overwritten")
            # add habitat to trap covariates
            # - save attributes (might be a bug in secr::addCovariates)
            attrs = attributes(env$capthist)
            traps(env$capthist) = addCovariates(traps(env$capthist), env$shp$habitat)
            attributes(env$capthist) = attrs
        }
        refresh()
        mask_summary()
        mask_check()
    }

    mask_plot = function(){
        device_popup()
        par(mfrow = c(1,1), mar = c(0,0,0,0), oma = c(0,0,1,0))
        col = covariate = NULL
        if(!is.null(env$shp$habitat)){
            covariate = "habitat"
            col = terrain.colors(length(env$shp$habitat@polygons))
            par(mar = par()$mar + c(0,0,0,7))
        }
        plot_mask(env$mask, traps = traps(env$capthist), region = env$shp$region, covariate = covariate, col = col, legend = FALSE)
        if(!is.null(env$shp$region)){
            plot_shp(env$shp$region, add = TRUE)
        }
        title("Mask", outer = TRUE)
        if(!is.null(env$shp$habitat)){
            legend(par()$usr[2], mean(par()$usr[3:4]), levels(env$shp$habitat@data$habitat), pch = 15, col = col, title = "habitat", xpd = TRUE, xjust = 0, yjust = 0.5)
        }
    }

    mask_summary = function(){
        result = utils::capture.output(summary_mask(env$mask, env$capthist))
        if(inherits(result, "try-error")){
            error_message(result)
        }else{
            print_to_console(result, "Mask summary:")
        }
    }

    model_estimates = function(){
        result = try(utils::capture.output(cbind(estimate = coef(env$fit), confint(env$fit))))
        if(inherits(result, "try-error")){
            error_message(result)
        }else{
            print_to_console(result, "Parameter estimates:")
        }
    }

    model_fit = function(){
        # make a wait cursor - just for fun
        tkconfigure(main.window, cursor = "watch")
        on.exit(tkconfigure(main.window, cursor = "arrow"))

        ##################################################
        ## check inputs

        # if the radio button is on fixed - check the value
        for(i in submodels){
            if(tclvalue(env$model.radiobutton.value[[i]]) == "fixed"){
                if(tclvalue(env$model.fixed.entry.value[[i]]) == ""){
                    message = paste("Please enter a fixed value for", i, "or select the formula box")
                    error_message(message)
                    stop(.call = FALSE)
                }else{
                    if(is.na(as.numeric(tclvalue(env$model.fixed.entry.value[[i]])))){
                        message = paste("Fixed value for", i, "not recognised")
                        error_message(message)
                        stop(.call = FALSE)
                    }
                }
            }
        }

        ##################################################
        ## formulas and fixed

        # convert formula entrys to actual formulas
        # if blank then use intercept-only model
        formulas = sapply(env$model.formula.entry.value, function(x){
            if(tclvalue(x) == ""){
                ~1
            }else{
                as.formula(paste("~", tclvalue(x)))
            }
        }, simplify = FALSE)
        # convert fixed entrys to numeric
        fixed = sapply(env$model.fixed.entry.value, function(x){
            if(tclvalue(x) == ""){
                NULL
            }else{
                as.numeric(tclvalue(x))
            }
        }, simplify = FALSE)
        # if radiobutton is on formula then set fixed to blank
        # otherwise set formula to blank
        for(i in names(formulas)){
            if(tclvalue(env$model.radiobutton.value[[i]]) == "formula"){
                fixed[[i]] = NULL
            }else{
                formulas[[i]] = NULL
            }
        }
        # remove null elements
        formulas = formulas[!sapply(formulas, is.null)]
        fixed = fixed[!sapply(fixed, is.null)]

        ##################################################
        ## fit model

        result <- try({
            utils::capture.output({
                env$fit = gibbonsecr::gibbonsecr_fit(
                    capthist = env$capthist,
                    model = formulas,
                    fixed = fixed,
                    model.options = list(
                        detectfn = switch(
                            tclvalue(tkget(model.options.combobox[["detfunc"]])),
                            "half normal" = 0,
                            "hazard rate" = 1),
                        bearings = switch(
                            tclvalue(tkget(model.options.combobox[["bearings"]])),
                            "none"           = 0,
                            "von mises"      = 1,
                            "wrapped cauchy" = 2),
                        distances = switch(
                            tclvalue(tkget(model.options.combobox[["distances"]])),
                            "none"       = 0,
                            "gamma"      = 1,
                            "log-normal" = 2)
                    ),
                    mask = env$mask,
                    fitting.options = list(
                        hessian = TRUE,
                        iterlim = 1000
                    ),
                    start = NULL,
                    trace = FALSE # consider making this changeable?
                )
            })
        })

        ##################################################
        ## report results

        if(inherits(result, "try-error")){
            error_message(result)
            # print_to_console(result, dashes = FALSE, tag = "errorTag")
        }else{
            if(env$fit$nlm$code < 3){
                result = utils::capture.output(env$fit)
                print_to_console(result, "Model fit summary:")
            }else{
                env$fit = NULL
                message = paste0("The fitting algorithm did not converge (nlm code = ", env$fit$nlm$code, ")")
                # tkmessageBox(title = "Error", message = paste0("The fitting algorithm did not converge\n\nDetails: nlm code ", env$fit$nlm$code), icon = "error", type = "ok")
                # print_to_console(message, dashes = FALSE, tag = "errorTag")
                error_message(message)
            }
        }
        refresh()
    }

    model_plot_bearings = function(){
        device_popup()
        plot(env$fit, which = "bearings", session = 1, CI = TRUE)
    }

    model_plot_density_surface = function(){
        device_popup()
        plot(env$fit, which = "density", session = 1, CI = FALSE)
    }

    model_plot_detection_function = function(){
        device_popup()
        plot(env$fit, which = "detectfn", session = 1, CI = TRUE)
    }

    model_plot_detection_surface = function(){
        device_popup()
        plot(env$fit, which = "pdot", session = 1, CI = FALSE)
    }

    model_plot_distances = function(){
        device_popup()
        plot(env$fit, which = "distances", session = 1, CI = TRUE)
    }

    model_summary = function(){
        result = try(utils::capture.output(env$fit))
        if(inherits(result, "try-error")){
            error_message(result)
        }else{
            print_to_console(result, "Model fit summary:")
        }
    }

    open_gibbons_manual = function(){
        system(paste("open ", system.file("doc/gibbonsecr_1.0-vignette.html", package = "gibbonsecr")))
    }

    print_dashes = function(){
        tkinsert(console, "end", paste0(paste(rep("-", 60), collapse = ""), "\n"))
    }

    print_to_console = function(x, heading = NULL, dashes = TRUE, tag = "normalTag"){
        tkconfigure(console, state = "normal")
        if(dashes) print_dashes()
        tkinsert(console, "end", "\n")
        if(!is.null(heading)) tkinsert(console, "end", paste(heading, "\n\n"), "headingTag")
        tkinsert(console, "end", paste(x, collapse = "\n"), tag)
        tkinsert(console, "end", "\n")
        tkconfigure(console, state = "disabled")
        tksee(console, "end")
    }

    refresh = function(){
        # this is run at the end of several functions
        # it updates the state of buttons and entry fields,
        # given the current data and results

        ##################################################
        ## entry boxes



        ##################################################
        ## if capthist exists

        if(!is.null(env$capthist)){
            tkconfigure(data.summary.button, state = "normal")
            tkconfigure(mask.buffer.entry,   state = "normal")
            tkconfigure(mask.spacing.entry,  state = "normal")
            for(i in c("bearings", "distances")){
                for(j in c("units","type")){
                    tkset(
                        data.details.combobox[[i]][[j]],
                        tclvalue(env$data.details.value[[i]][[j]])
                    )
                }
            }
            for(i in tolower(shp.file.types)){
                tkconfigure(shp.file.check.button[[i]], state = "normal")
            }
            tkconfigure(mask.make.button, state = "normal")
        }

        ##################################################
        ## if mask exists

        if(!is.null(env$mask)){
            tkconfigure(mask.summary.button, state = "normal")
            tkconfigure(mask.plot.button, state = "normal")
            tkconfigure(model.fit.button, state = "normal")
            tkconfigure(model.options.combobox[["detfunc"]], state = "normal")
            aux.data.present = list(
                bearings = !is.null(get_bearings(env$capthist)),
                distances = !is.null(get_distances(env$capthist))
            )
            for(i in submodels){
                # start by enabling all formula and fixed
                tkconfigure(model.formula.radiobutton[[i]], state = "normal")
                tkconfigure(model.fixed.radiobutton[[i]], state = "normal")
                tkconfigure(model.formula.entry[[i]], state = "normal")
                tkconfigure(model.fixed.entry[[i]], state = "normal")
                # if single occasion:
                # disable formula for pcall and g0
                # disable fixed for g0
                if(i %in% c("g0","pcall") && all(n_occasions(env$capthist) == 1)){
                    tclvalue(env$model.radiobutton.value[[i]]) = "fixed"
                    tkconfigure(model.formula.radiobutton[[i]], state = "disabled")
                    tkconfigure(model.formula.entry[[i]], state = "disabled")
                    if(i == "g0"){
                        tkconfigure(model.fixed.entry[[i]], state = "disabled")
                    }
                }else{
                    # otherwise:
                    # if radio button is on formula then disable fixed and enable formula
                    # if radio button is on fixed then disable formula and enable fixed
                    on.formula = tclvalue(env$model.radiobutton.value[[i]]) == "formula"
                    tkconfigure(model.formula.entry[[i]], state = if(on.formula) "normal" else "disabled")
                    tkconfigure(model.fixed.entry[[i]], state = if(on.formula) "disabled" else "normal")
                }
                # bearings and distances (i.e. auxiliary data)
                if(i %in% names(aux.data.present)){
                    # start by enabling the model options combobox
                    tkconfigure(model.options.combobox[[i]], state = "normal")
                    if(!aux.data.present[[i]]){
                        # if no auxiliary data then disable model options combobox
                        tkconfigure(model.options.combobox[[i]], state = "disabled")
                    }else{
                        # if auxiliary data exists but model options set to "none",
                        # then disable formula and fixed
                        if(tclvalue(tkget(model.options.combobox[[i]])) == "none"){
                            tkconfigure(model.formula.radiobutton[[i]], state = "disabled")
                            tkconfigure(model.fixed.radiobutton[[i]], state = "disabled")
                            tkconfigure(model.formula.entry[[i]], state = "disabled")
                            tkconfigure(model.fixed.entry[[i]], state = "disabled")
                        }
                    }
                }
            }
        }

        ##################################################
        ## if fitted model exists

        if(!is.null(env$fit)){
            if(env$fit$nlm$code < 3){
                tkconfigure(model.summary.button, state = "normal")
                tkconfigure(model.estimates.button, state = "normal")
                tkconfigure(model.plot.detection.function.button, state = "normal")
                tkconfigure(model.plot.detection.surface.button, state = "normal")
                tkconfigure(model.plot.density.surface.button, state = "normal")
                tkconfigure(model.plot.bearings.button,
                            state = if(env$fit$model.options$bearings == 0) "disabled" else "normal")
                tkconfigure(model.plot.distances.button,
                            state = if(env$fit$model.options$distances == 0) "disabled" else "normal")
            }
        }
    }

    warning_message = function(message, dashes = FALSE){
        print_to_console(paste("Warning :", message), dashes = dashes, tag = "warningTag")
    }

#     workspace_load = function(){
#         filename = tclvalue(tkgetOpenFile(
#             initialdir = env$wd,
#             filetypes = "{{} {.rda}}"
#         ))
#         if(filename != ""){
#             load(filename)
#             load(filename, envir = parent.env(environment()))
#             for(i in names(env.list)){
#                 env[[i]] = env.list[[i]]
#             }
#             refresh()
#         }
#         tkfocus(main.window)
#     }

#     workspace_save = function(){
#         filename = tclvalue(tkgetSaveFile(initialdir = env$wd, filetypes = "{{} {.rda}}"))
#         env.list = as.list(env)
#         if(filename != "") save(env, file = filename)
#         tkfocus(main.window)
#         return(filename)
#     }

#     set_working_directory = function(){
#         wd = tclvalue(tkchooseDirectory())
#         if(wd != ""){
#             setwd(wd)
#             env$wd = path.expand(getwd())
#         }
#         tkfocus(main.window)
#     }

    ##################################################
    ## redefinintions of tcltk functions using preferred defaults

    ttkbutton = function(parent, text, command, state = "disabled", width = os$button.width, padding = os$button.padding, ...){
        tcltk::ttkbutton(parent, text = text, command = command, width = width, state = state, padding = padding, ...)
    }

    tkcheckbutton = function(parent, variable, bg = os$tab.col, ...){
        tcltk::tkcheckbutton(parent, variable = variable, bg = bg, ...)
    }

    ttkcombobox = function(parent, values, textvariable, width = os$default.combobox.width, state = "disabled", ...){
        tcltk::ttkcombobox(parent, values = values, textvariable = textvariable, width = width, state = state, ...)
    }

    ttkentry = function(parent, textvariable, state = "disabled", width = os$default.entry.width, ...){
        tcltk::ttkentry(parent, textvariable = textvariable, width = width, state = state, ...)
    }

    tkframe = function(parent, relief = os$relief, bd = 2, width = os$tab.frame.width, height = 10, bg = os$tab.col, ...){
        tcltk::tkframe(parent, relief = relief, bd = bd, width = width, height = height, bg = bg, ...)
    }

    tkgrid = function(..., sticky = "w", padx = os$default.padx, pady = os$default.pady){
        tcltk::tkgrid(..., sticky = sticky, padx = padx, pady = pady)
    }

    tklabel = function(parent, text, bg = os$tab.col, ...){
        tcltk::tklabel(parent, text = text, bg = bg, ...)
    }

    tkmenu = function(parent, activebackground = os$heading.text.col, activeforeground = "white", foreground = os$default.text.col, bg = os$bg.col, ...){
        tcltk::tkmenu(parent, activebackground = activebackground, activeforeground = activeforeground, foreground = foreground, bg = bg, ...)
    }

    tkradiobutton = function(parent, variable, value, command, state = "disabled", bg = os$tab.col){
        tcltk::tkradiobutton(parent, variable = variable, value = value, command = command, state = state, bg = bg)
    }

    ## ###################################################################### ##
    ## ###################################################################### ##

    #                            ---- Variables ----

    ##################################################
    ## static variables

    csv.file.types = c("Detections", "Posts", "Covariates")
    shp.file.types = c("Region", "Habitat")
    submodels      = c("D", "g0", "sigma", "bearings", "distances", "pcall")

    # empty lists to store tcl objects and tcl object settings
    csv.filepath.entry = list()
    csv.file.browse.button = list()
    csv.file.view.button = list()
    data.details.combobox = list(bearings = list(), distances = list())
    data.details.options = list(
        bearings  = list(
            units = c("degrees", "radians"),
            type  = c("continuous")),
        distances = list(
            units = c("km", "m"),
            type  = c("continuous"))
    )
    shp.filepath.entry = list()
    shp.file.browse.button = list()
    shp.file.check.button = list()
    shp.file.checkbox = list()
    model.options.combobox = list()
    model.options.row.label = list()
    model.formula.entry = list()
    model.formula.radiobutton = list()
    model.fixed.entry = list()
    model.fixed.radiobutton = list()
    model.options.combobox.options = list(
        detfunc = c("half normal", "hazard rate"),
        bearings = c("none", "von mises", "wrapped cauchy"),
        distances = c("none", "gamma", "log-normal")
    )
    model.submodels.row.label = list()
    model.submodels.col.label = list()

    # empty lists to store functions linked to tcl objects
    csv.file.browse.function = list()
    csv.file.view.function = list()
    model.formula.radiobutton.command = list()
    model.fixed.radiobutton.command = list()

    ##################################################
    ## dynamic variables

    # save dynamic variables in new environment
    # this enables convenient referencing when changing values inside other functions
    # set to default values
    # env = list()
    env = new.env()
    # general
    env$capthist = NULL
    env$mask = NULL
    env$fit = NULL
    env$wd = path.expand(getwd())
    # data
    extdata = system.file("extdata/N.annamensis", package = "gibbonsecr")
    # CHANGE THIS
    env$csv.filepath.entry.value = list(
        detections = tclVar(file.path(extdata, "example_detections_file.csv")),
        posts      = tclVar(file.path(extdata, "example_posts_file.csv")),
        covariates = tclVar(file.path(extdata, "example_covariates_file.csv"))
    )
    # CHANGE THIS
    env$data.details.value = list(
        bearings = list(units = tclVar("degrees"), type = tclVar("continuous")),
        distances = list(units = tclVar("km"), type = tclVar("continuous"))
    )
    # mask
    # CHANGE THIS
    env$mask.buffer.entry.value = tclVar("6000")
    # CHANGE THIS
    env$mask.spacing.entry.value = tclVar("250")
    # CHANGE THIS
    env$shp.filepath.entry.value = list(
        region  = tclVar(file.path(extdata, "region.shp")),
        habitat = tclVar(file.path(extdata, "habitat.shp"))
    )
    # CHANGE THIS
    env$shp.file.checkbox.value = list(
        region  = tclVar("0"),
        habitat = tclVar("0")
    )
    env$shp = list()
    # model
    # CHANGE THIS
    env$model.options.combobox.value = list(
        detfunc = tclVar("half normal"),
        bearings = tclVar("von mises"),
        distances = tclVar("none")
    )
    env$model.formula.entry.value = sapply(submodels, function(i) tclVar(""), simplify = FALSE)
    env$model.fixed.entry.value = sapply(submodels, function(i){
        if(i %in% c("g0","pcall")) tclVar("1") else tclVar("")
    }, simplify = FALSE)
    env$model.radiobutton.value = sapply(submodels, function(i){
        if(i %in% c("g0","pcall")) tclVar("fixed") else tclVar("formula")
    }, simplify = FALSE)


    ## ###################################################################### ##
    ## ###################################################################### ##

    #                            ---- Main Window ----

    # create main window
    main.window = tktoplevel(width = os$main.window.width, height = os$main.window.height, bg = os$bg.col)
    add_icon(main.window)
    tkwm.title(main.window, paste0("gibbonsecr v", utils::packageVersion("gibbonsecr")))
    # put window in centre of computer screen
    screen.width = as.numeric(tclvalue(tkwinfo("screenwidth", main.window)))
    screen.height = as.numeric(tclvalue(tkwinfo("screenheight", main.window)))
    tkwm.geometry(main.window, paste0(os$main.window.width, "x", os$main.window.height, "+", round((screen.width - os$main.window.width) / 2), "+", round((screen.height - os$main.window.height) / 2)))
    # set minimum window size
    tkwm.minsize(main.window, os$main.window.width, os$main.window.height)
    # put notebook widget in left hand side for tabs
    lhs = ttknotebook(main.window, width = os$tab.frame.width, height = os$main.window.height, padding = c(0,0))
    # put frame on right hand side for output console
    rhs = tkframe(main.window, bg = os$bg.col, width = os$console.window.width)


    ## ###################################################################### ##
    ## ###################################################################### ##

    #                             ---- DATA TAB ----

    ##################################################
    ## data tab frame

    data.tab = tkframe(lhs, pady = os$tab.pady, padx = os$tab.padx)
    tkadd(lhs, data.tab, text = "Data", compound = "right", underline = 0)

    ##################################################
    ## csv files

    # tkpack(heading_label("TEMP"))


    add_heading(data.tab, "CSV files")
    csv.browser.frame = tkframe(data.tab)
    tkpack(csv.browser.frame)
    # for each csv file type make a text entry box, browse button and view button
    for(i in csv.file.types){ # i = row.names[1] ; i
        row = which(csv.file.types == i) - 1
        i = tolower(i)
        # row label
        tkgrid(tklabel(csv.browser.frame, csv.file.types[row + 1]), row = row, column = 0)
        # entry box
        csv.filepath.entry[[i]] = ttkentry(csv.browser.frame, env$csv.filepath.entry.value[[i]], "normal")
        tkgrid(csv.filepath.entry[[i]], row = row, column = 1)
        # browse button
        csv.file.browse.button[[i]] = ttkbutton(csv.browser.frame, "Browse", make_command_browse(i), "normal")
        tkgrid(csv.file.browse.button[[i]], row = row, column = 2)
        # view button
        csv.file.view.button[[i]] = ttkbutton(csv.browser.frame, "View", make_command_csv_view(i), "normal")
        tkgrid(csv.file.view.button[[i]], row = row, column = 3)
    }

    ##################################################
    ## data details

    # separator
    add_separator(data.tab)
    # heading
    add_heading(data.tab, "Data details")
    # frame
    data.details.frame = tkframe(data.tab)
    tkpack(data.details.frame)
    # column labels
    col.names = c("Bearings", "Distances")
    for(col in 1:2){
        tkgrid(tklabel(data.details.frame, col.names[col]), sticky = "we", row = 0, column = col)
    }
    # row labels
    row.names = c("Units","Type")
    for(row in 1:2){
        tkgrid(tklabel(data.details.frame, row.names[row]), row = row, column = 0)
    }
    # for each combination of bearings/distances units/type add combobox
    for(i in tolower(col.names)){
        for(j in tolower(row.names)){
            col = which(tolower(col.names) == i) ; col
            row = which(tolower(row.names) == j) ; row
            data.details.combobox[[i]][[j]] = ttkcombobox(data.details.frame, data.details.options[[i]][[j]], env$data.details.value[[i]][[j]])
            tkgrid(data.details.combobox[[i]][[j]], row = row, column = col)
        }
    }
    # enable units option (but not type, until interval methods are developed)
    tkconfigure(data.details.combobox[["bearings"]][["units"]], state = "normal")
    tkconfigure(data.details.combobox[["distances"]][["units"]], state = "normal")

    ##################################################
    ## data buttons

    add_separator(data.tab)
    data.buttons.frame = tkframe(data.tab)
    tkpack(data.buttons.frame)
    data.import.button = ttkbutton(data.buttons.frame, "Import", data_import, "normal")
    data.summary.button = ttkbutton(data.buttons.frame, "Summary", data_summary)
    tkgrid(data.import.button, data.summary.button)


    ## ###################################################################### ##
    ## ###################################################################### ##

    #                             ---- MASK TAB ----

    ##################################################
    ## mask tab frame

    mask.tab = tkframe(lhs, pady = os$tab.pady, padx = os$tab.padx)
    tkadd(lhs, mask.tab, text = "Mask", compound = "right", underline = 0)

    ##################################################
    ## mask size and resolution

    add_heading(mask.tab, "Size and resolution")
    mask.spec.frame = tkframe(mask.tab)
    tkpack(mask.spec.frame)
    # buffer entry box label
    mask.buffer.label = tklabel(mask.spec.frame, "Buffer (m)")
    tkgrid(mask.buffer.label, column = 0, row = 0, padx = 5)
    # buffer entry box
    mask.buffer.entry = ttkentry(mask.spec.frame, env$mask.buffer.entry.value, width = os$small.entry.width)
    tkgrid(mask.buffer.entry, column = 1, row = 0, padx = 5)
    # spacing entry box label
    mask.spacing.label = tklabel(mask.spec.frame, "Spacing (m)")
    tkgrid(mask.spacing.label, column = 2, row = 0, padx = 5)
    # spacing entry box
    mask.spacing.entry = ttkentry(mask.spec.frame, env$mask.spacing.entry.value, width = os$small.entry.width)
    tkgrid(mask.spacing.entry, column = 3, row = 0, padx = 5)

    ##################################################
    ## shp files

    add_separator(mask.tab)
    add_heading(mask.tab, "SHP files")
    gis.browser.frame = tkframe(mask.tab)
    tkpack(gis.browser.frame)
    # for each shp file type make a text entry box, browse button and view button
    for(i in shp.file.types){ # i = shp.file.types[1] ; i
        row = which(shp.file.types == i) - 1
        i = tolower(i)
        # row label
        tkgrid(tklabel(gis.browser.frame, shp.file.types[row + 1]), row = row, column = 0)
        # entry box
        shp.filepath.entry[[i]] = ttkentry(gis.browser.frame, env$shp.filepath.entry.value[[i]], "normal")
        tkgrid(shp.filepath.entry[[i]], row = row, column = 1)
        # browse button
        shp.file.browse.button[[i]] = ttkbutton(gis.browser.frame, "Browse", make_command_browse(i, "shp"), "normal")
        tkgrid(shp.file.browse.button[[i]], row = row, column = 2)
        # check button
        shp.file.check.button[[i]] = ttkbutton(gis.browser.frame, "Check", make_command_shp_check(i))
        tkgrid(shp.file.check.button[[i]], row = row, column = 3)
        # checkbox
        shp.file.checkbox[[i]] = tkcheckbutton(gis.browser.frame, env$shp.file.checkbox.value[[i]])
        tkgrid(shp.file.checkbox[[i]], row = row, column = 4)
    }

    ##################################################
    ## mask buttons

    add_separator(mask.tab)
    mask.buttons.frame = tkframe(mask.tab)
    tkpack(mask.buttons.frame)
    mask.make.button = ttkbutton(mask.buttons.frame, "Make", mask_make)
    mask.summary.button = ttkbutton(mask.buttons.frame, "Summary", mask_summary)
    mask.plot.button = ttkbutton(mask.buttons.frame, "Plot", mask_plot)
    tkgrid(mask.make.button, mask.summary.button, mask.plot.button)



    ## ###################################################################### ##
    ## ###################################################################### ##

    #                             ---- MODEL TAB ----

    ##################################################
    ## model tab frame

    model.tab = tkframe(lhs, pady = os$tab.pady, padx = os$tab.padx)
    tkadd(lhs, model.tab, text = "Model", compound = "right", underline = 0)

    ##################################################
    ## model options

    add_heading(model.tab, "Model options")
    model.options.frame = tkframe(model.tab)
    tkpack(model.options.frame)

    for(i in c("detfunc", "bearings", "distances")){
        # row in packing grid
        row = which(c("detfunc", "bearings", "distances") == i)
        # make labels - save them as objects so you can use tk2tip
        model.options.row.label[[i]] = tklabel(model.options.frame, c("Detection function","Bearings distribution","Distances distribution")[row])
        tkgrid(model.options.row.label[[i]], row = row - 1, column = 0)
        # make comboboxes
        model.options.combobox[[i]] = ttkcombobox(model.options.frame, model.options.combobox.options[[i]], env$model.options.combobox.value[[i]])
        tkgrid(model.options.combobox[[i]], column = 1, row = row - 1)
    }

    ##################################################
    ## submodels

    add_separator(model.tab)
    add_heading(model.tab, "Sub-models")
    model.submodels.frame = tkframe(model.tab)
    tkpack(model.submodels.frame)

    # column labels
    for(i in 1:2){
        model.submodels.col.label[[i]] = tklabel(model.submodels.frame, c("Formula","Fixed")[i])
        tkgrid(model.submodels.col.label[[i]], sticky = "we", row = 0, column = i * 2)
    }

    # loop over submodels and make label, formula entry, fixed entry, formula radiobutton and fixed radiobutton
    for(i in submodels){ # i = submodels[1] ; i
        row = which(submodels == i)
        # label
        model.submodels.row.label[[i]] = tklabel(model.submodels.frame, i)
        tkgrid(model.submodels.row.label[[i]], row = row, column = 0)
        # tilde
        tilde = tklabel(model.submodels.frame, "~")
        tkgrid(tilde, sticky = "e", row = row, column = 1, padx = 0)
        # formula entry
        model.formula.entry[[i]] = ttkentry(model.submodels.frame, env$model.formula.entry.value[[i]])
        tkgrid(model.formula.entry[[i]], row = row, column = 2)
        # fixed entry
        model.fixed.entry[[i]] = ttkentry(model.submodels.frame, env$model.fixed.entry.value[[i]], width = os$small.entry.width)
        tkgrid(model.fixed.entry[[i]], sticky = "e", row = row, column = 4)
        # formula radiobutton
        model.formula.radiobutton[[i]] = tkradiobutton(model.submodels.frame, env$model.radiobutton.value[[i]], "formula", make_command_model_formula_radiobutton(i))
        tkgrid(model.formula.radiobutton[[i]], row = row, column = 3, padx = 0)
        # fixed radiobutton
        model.fixed.radiobutton[[i]] = tkradiobutton(model.submodels.frame, env$model.radiobutton.value[[i]], "fixed", make_command_model_fixed_radiobutton(i))
        tkgrid(model.fixed.radiobutton[[i]], row = row, column = 5, padx = 0)
        # tkgrid.columnconfigure(model.submodels.frame, 3, minsize = 30)
    }


    ##################################################
    ## model fit buttons

    add_separator(model.tab)
    model.fit.buttons.frame = tkframe(model.tab)
    tkpack(model.fit.buttons.frame)
    model.fit.button = ttkbutton(model.fit.buttons.frame, "Fit", model_fit)
    model.summary.button = ttkbutton(model.fit.buttons.frame, "Summary", model_summary)
    model.estimates.button = ttkbutton(model.fit.buttons.frame, "Estimates", model_estimates)
    tkgrid(model.fit.button, model.summary.button, model.estimates.button)

    ##################################################
    ## model plot buttons

    add_separator(model.tab)
    add_heading(model.tab, "Plots")
    model.plot.buttons.frame = tkframe(model.tab)
    tkpack(model.plot.buttons.frame)
    model.plot.detection.function.button = ttkbutton(model.plot.buttons.frame, "Det. func.", model_plot_detection_function)
    model.plot.detection.surface.button = ttkbutton(model.plot.buttons.frame, "Det. surf.", model_plot_detection_surface)
    model.plot.density.surface.button = ttkbutton(model.plot.buttons.frame, "Den. surf.", model_plot_density_surface)
    model.plot.bearings.button = ttkbutton(model.plot.buttons.frame, "Bearings", model_plot_bearings)
    model.plot.distances.button = ttkbutton(model.plot.buttons.frame, "Distances", model_plot_distances)
    tkgrid(model.plot.detection.function.button, model.plot.detection.surface.button, model.plot.density.surface.button, row = 0)
    tkgrid(model.plot.bearings.button, model.plot.distances.button, row = 1)


    ## -------------------------------------------------------------------------- ##
    ## -------------------------------------------------------------------------- ##

    #                            ---- Output Console ----


    console = tktext(rhs, height = 5, fg = os$console.fg.col, bg = os$console.bg.col, relief = os$relief)
    scrollbar = tkscrollbar(rhs, command = function(...) tkyview(console, ...))
    tkconfigure(console, yscrollcommand = function(...) tkset(scrollbar, ...), state = "disabled")
    tkpack(scrollbar, side = "right", fill = "y")
    tkpack(console, fill = "both", expand = TRUE, padx = os$console.padx, pady = os$console.pady)

    # font tags
    tktag.configure(console, "normalTag", foreground = os$console.fg.col, font = os$console.normal.font)
    tktag.configure(console, "headingTag", foreground = os$console.fg.col, font = os$console.heading.font)
    tktag.configure(console, "successTag", foreground = "cornflowerblue", font = os$console.bold.font)
    tktag.configure(console, "warningTag", foreground = "orange", font = os$console.bold.font)
    tktag.configure(console, "errorTag", foreground = "red3", font = os$console.bold.font)

    # left click to open console popup menu
    tkbind(console, "<ButtonPress-3>", console_popup)


    ## -------------------------------------------------------------------------- ##
    ## -------------------------------------------------------------------------- ##

    #                              ---- Menu Bar ----


    menu.bar = tkmenu(main.window)
    tkconfigure(main.window, menu = menu.bar)

    ##################################################
    ## help menu

    help.menu = tkmenu(menu.bar, tearoff = FALSE)
    tkadd(menu.bar, "cascade", label = "Help", menu = help.menu)
    tkadd(help.menu, "command", label = "User manual", command = open_gibbons_manual)
    # examples
    help.menu.examples = tkmenu(help.menu, tearoff = FALSE)
    tkadd(help.menu, "cascade", label = "Examples", menu = help.menu.examples)
    # add option to open example data files
    for(i in csv.file.types)
        tkadd(help.menu.examples, "command", label = paste("Example",i,"file"), command = make_command_open_example_file(i))
    # about
    tkadd(help.menu, "command", label = "About gibbonsecr", command = about)

    ##################################################
    ## workspace menu

#     workspace.menu = tkmenu(menu.bar, tearoff = FALSE)
#     tkadd(menu.bar, "cascade", label = "Workspace", menu = workspace.menu)
#     tkadd(workspace.menu, "command", label = "Save workspace", accelerator = "CTRL+S", command = workspace_save, state = "normal")
#     tkadd(workspace.menu, "command", label = "Load workspace", accelerator = "CTRL+L", command = workspace_load, state = "normal")
#     tkadd(workspace.menu, "separator")
#     tkadd(workspace.menu, "command", label = "Set working directory", command = set_working_directory, state = "normal")

    ## -------------------------------------------------------------------------- ##
    ## -------------------------------------------------------------------------- ##

    #                           ---- Tags and Bindings ----


    tkwm.protocol(main.window, "WM_DELETE_WINDOW", exit_prompt)
    # tkbind(main.window, "<Control-q>", exit.prompt)
    # tkbind(main.window, "<Control-s>", workspace_save)
    # tkbind(main.window, "<Control-l>", workspace_load)
    # make sure that refresh is called if model.options.combobox is changed by the user
    for(i in c("bearings","distances")){
        tkbind(model.options.combobox[[i]], "<<ComboboxSelected>>", refresh)
    }
    # tkbind(distances.type.combobox, "<<ComboboxSelected>>", refresh)
    # # tkbind(main.window, "d", function() tcl(lhs, "select", 0))
    # # tkbind(main.window, "m", function() tcl(lhs, "select", 1))
    # # tkbind(main.window, "p", function() tcl(lhs, "select", 2))

    ## -------------------------------------------------------------------------- ##
    ## -------------------------------------------------------------------------- ##

    #                          ---- Style settings ----

    # default widget settings
    # tcl("ttk::style", "configure", "TNotebook", background = os$bg.col)
    # tcl("ttk::style", "configure", "TButton", background = os$tab.col, foreground = os$default.text.col)
    # tcl("ttk::style", "configure", "TEntry", selectbackground = os$heading.text.col)
    # tcl("ttk::style", "configure", "TLabel", foreground = os$heading.text.col)

    ## -------------------------------------------------------------------------- ##
    ## -------------------------------------------------------------------------- ##

    #                              ---- Tooltips ----

    # mask
    tk2tip(mask.buffer.label, "The radius of a buffer region around each array of listening posts.\n This defines an area beyond which the detection probability can be assumed to be zero.")
    tk2tip(mask.spacing.label, "The spacing between neighbouring grid points in the mask.")

    # model options
    tk2tip(model.options.row.label[["detfunc"]], "A curve that describes the relationship between detection probability and dsitance from observer.")
    tk2tip(model.options.row.label[["bearings"]], "The distribution for the estimated bearings.")
    tk2tip(model.options.row.label[["distances"]], "The distribution for the estimated distances.")

    # submodels
    tk2tip(model.submodels.row.label[["D"]], "The number of groups per square kilometre.")
    tk2tip(model.submodels.row.label[["g0"]], "The detection function intercept parameter.\nThis gives the probability of hearing a calling group whose activity centre is zero distance from the listening post.")
    tk2tip(model.submodels.row.label[["sigma"]], "The detection function scale parameter.\nThis defines the width of the detection function (larger values = wider detection functions).")
    tk2tip(model.submodels.row.label[["bearings"]], "The parameter of the distribution for the estimated bearings.\nThis defines the spread of the distribution (larger values = narrower distributions = more accurate estimates)")
    tk2tip(model.submodels.row.label[["distances"]], "The parameter of the distribution for the estimated distances.\nThis defines the spread of the distribution (XXX values = XXX distributions = more accurate estimates) .")
    tk2tip(model.submodels.row.label[["pcall"]], "The probability of a group calling on a given day.\nAlternatively, the proportion of groups which call on a given day.")


    ## -------------------------------------------------------------------------- ##
    ## -------------------------------------------------------------------------- ##

    #                              ---- Packing ----


    tkpack(lhs, fill = "both", side = "left")
    tkpack(rhs, fill = "both", expand = TRUE, side  = "left")

    # make sure window opens on top of other windows (but doesn't forcibly remain on top)
    tcl("wm", "attributes", main.window, topmost = TRUE)
    tcl("wm", "attributes", main.window, topmost = FALSE)

    tkwm.deiconify(main.window)
    tkgrab.set(main.window)
    tkfocus(main.window)
    tkgrab.release(main.window)



    # tcl("wm", "attributes", main.window, fullscreen = TRUE)

    invisible()

}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##
