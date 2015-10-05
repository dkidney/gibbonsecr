## -------------------------------------------------------------------------- ##
## gui functions which are independent of the gui environment
## -------------------------------------------------------------------------- ##

# center_window = function(tt){
#     winw = as.numeric(tclvalue(tkwinfo("width", tt)))
#     winh = as.numeric(tclvalue(tkwinfo("height", tt)))
#     scrw = as.numeric(tclvalue(tkwinfo("screenwidth", tt)))
#     scrh = as.numeric(tclvalue(tkwinfo("screenheight", tt)))
#     tkwm.geometry(tt, paste0(winw, "x", winh, "+", round((scrw - winw) / 2), "+", round((scrh - winh) / 2)))
# }

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# Popup menu to allow choice of covariate class
# Example
# x = data.frame(
#     colours = c("red", "green", "blue"),
#     fruits  = c("Apple", "Orange", "Banana", "Pear", "Cherry", "eggs"),
#     names   = c("Alice","Darren"),
#     numbers = 1:6
# )
# str(x)
# classes = check_covariate_classes(x)
# print(classes)

check_covariate_classes = function(x, padx = 1){

    tt = tktoplevel()
    tkwm.title(tt, "Check covariate classes")
    w = 100
    h = 100
    scrw = as.numeric(tclvalue(tkwinfo("screenwidth", tt)))
    scrh = as.numeric(tclvalue(tkwinfo("screenheight", tt)))
    tkwm.geometry(tt, paste0(w, "x", h, "+", round((scrw - w) / 2), "+", round((scrh - h) / 2)))
    tcl("wm", "attributes", tt, topmost = TRUE)
    tcl("wm", "attributes", tt, topmost = FALSE)
    tkwm.geometry(tt, "")
    tkfocus(tt)
    main = ttkframe(tt)

    ##################################################
    ## upper frame for entry and combo boxes

    upper = ttkframe(main, padding = c(5,5))
    tkgrid(ttklabel(upper, text = "Name"), row = 0, column = 1, sticky = "w")
    tkgrid(ttklabel(upper, text = "Type"), row = 0, column = 2, sticky = "w")
    tkgrid(ttklabel(upper, text = "Use"),  row = 0, column = 3, sticky = "w")
    combo.char = ifelse(sapply(x, is.numeric), "number", "category")
    entry.char = colnames(x)
    combo.tvar = list()
    entry.tvar = list()
    check.tvar = list()
    entry = list()
    combo = list()
    check = list()
    for(j in 1:ncol(x)){ # j=1
        entry.tvar[[j]] = tclVar(entry.char[j])
        entry[[j]] = ttkentry(upper, textvariable = entry.tvar[[j]], width = 30,
                             state = "normal")
        tkgrid(entry[[j]], row = j, column = 1)
        combo.tvar[[j]] = tclVar(combo.char[j])
        combo[[j]] = ttkcombobox(upper, textvariable = combo.tvar[[j]], width = 10,
                             state = "normal", values = c("number", "category"))
        tkgrid(combo[[j]], row = j, column = 2)
        check.tvar[[j]] = tclVar(1)
        check[[j]] = ttkcheckbutton(upper, variable = check.tvar[[j]],
                             state = "normal")
        tkgrid(check[[j]], row = j, column = 3)
    }

    ##################################################
    ## lower frame for buttons

    lower = ttkframe(main, padding = c(5,5))
    done = tclVar(0)
    ok = ttkbutton(
        lower, text = "OK", state = "normal", width = 10, command = function(){
            names = sapply(entry.tvar, tclvalue)
            duplicates = duplicated(names)
            if(any(duplicates)){
                tkmessageBox(title = "Error", icon = "error", type = "ok",
                             message = "Duplicate names not allowed")
                stop(.call = FALSE)
            }
            tclvalue(done) = 1
        }
    )
    cancel = ttkbutton(
        lower, text = "Cancel", state = "normal", width = 10, command = function(){
            tclvalue(done) = 2
        }
    )

    ##################################################
    # packing etc.

    tkgrid(ok, cancel, padx = padx)
    tkpack(upper)
    tkpack(lower)
    tkpack(main)
    tkwm.resizable(tt, 0, 0)
    tkwm.protocol(tt, "WM_DELETE_WINDOW", function(){
        tclvalue(done) = 2
    })
    tkwait.variable(done)

    ##################################################
    # return clasess

    result = if(tclvalue(done) == 1){
        list(
            name  = sapply(entry.tvar, tclvalue),
            class = sapply(combo.tvar, tclvalue),
            use   = sapply(check.tvar, tclvalue) == "1"
        )
    }else{
        list(
            name  = entry.char,
            class = combo.char,
            use   = rep(TRUE, ncol(x))
        )
    }

    tkdestroy(tt)
    return(result)

}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

choose_newdata = function(fit, submodels = NULL, all = TRUE, padx = 1){

    if(is.null(submodels))
        submodels = names(fit$parindx) ; submodels

    if(length(do.call(c, lapply(fit$model[submodels], all.vars))) == 0){
        return(NULL)
    }

    ##################################################
    ## toplevel window and main frame

    tt = tktoplevel()
    tkwm.title(tt, "Choose prediction data")
    w = 100
    h = 100
    scrw = as.numeric(tclvalue(tkwinfo("screenwidth", tt)))
    scrh = as.numeric(tclvalue(tkwinfo("screenheight", tt)))
    tkwm.geometry(tt, paste0(w, "x", h, "+", round((scrw - w) / 2), "+", round((scrh - h) / 2)))
    tcl("wm", "attributes", tt, topmost = TRUE)
    tcl("wm", "attributes", tt, topmost = FALSE)
    tkwm.geometry(tt, "")
    tkfocus(tt)
    main = ttkframe(tt)

    ##################################################
    ## upper frame for choosing covariate values

    upper = ttkframe(main, padding = c(5,5))
    tkgrid(ttklabel(upper, text = "Name"),     row = 0, column = 1, sticky = "w")
    tkgrid(ttklabel(upper, text = "Value(s)"), row = 0, column = 2, sticky = "w")

    ##################################################
    ## summarise covariates

    covnames = sapply(submodels, function(i){ # submodel = "sigma"
        bigmf = do.call(rbind, lapply(fit$model.frames, function(x) x[[i]]))
        sapply(bigmf, function(x){
            if(inherits(x, "factor")) levels(x) else (range(x))
        }, simplify = FALSE)
    }, simplify = FALSE)
    covnames = do.call(c, unname(covnames))
    covnames = covnames[!duplicated(names(covnames))]
    covnames = covnames[order(names(covnames))]

    ##################################################
    ## add a combo / entry for each covariate
    box = boxvar = list()
    for(i in names(covnames)){
        if(inherits(covnames[[i]], "character")){
            boxvar[[i]] = tclVar(covnames[[i]][1])
            values = if(all) c(covnames[[i]], "all") else covnames[[i]]
            box[[i]] = ttkcombobox(parent       = upper,
                                   textvariable = boxvar[[i]],
                                   values       = values,
                                   width        = 30)
        }else{
            values[[i]] = tclVar(mean(covnames[[i]]))
            box[[i]] = ttkentry(parent       = upper,
                                textvariable = values[[i]],
                                width        = 30)
        }
        row = which(names(covnames) == i)
        tkgrid(ttklabel(upper, text = i), row = row, column = 1, sticky = "w")
        tkgrid(box[[i]], row = row, column = 2, sticky = "w")

    }

    ##################################################
    ## lower frame for buttons

    lower = ttkframe(main, padding = c(5,5))
    done = tclVar(0)
    ok = ttkbutton(
        lower, text = "OK", state = "normal", width = 10,
        command = function(){
            # throw error if numeric variable outside observed range
            for(i in names(covnames)){
                if(!inherits(covnames[[i]], "character")){
                    x = as.numeric(tclvalue(boxvar[[i]]))
                    if(x < covnames[[i]][1] || x > covnames[[i]][2]){
                        tkmessageBox(title = "Error", icon = "error", type = "ok",
                                     message = paste("value for", i,
                                                     "is outside observed range"))
                        stop(.call = FALSE)
                    }
                }
            }
            tclvalue(done) = 1
        }
    )
    cancel = ttkbutton(
        lower, text = "Cancel", state = "normal", width = 10,
        command = function(){
            tclvalue(done) = 2
        }
    )

    ##################################################
    ## packing etc.

    tkgrid(ok, cancel, padx = padx)
    tkpack(upper)
    tkpack(lower)
    tkpack(main)
    tkwm.resizable(tt, 0, 0)
    tkwm.protocol(tt, "WM_DELETE_WINDOW", function(){
        tclvalue(done) = 2
    })
    tkwait.variable(done)

    ##################################################
    # return newdata

    newdata = if(tclvalue(done) == "1"){
        do.call(expand.grid, sapply(names(covnames), function(i){
            if(inherits(covnames[[i]], "character")){
                if(tclvalue(boxvar[[i]]) == "all"){
                    factor(covnames[[i]], levels = covnames[[i]])
                }else{
                    factor(tclvalue(boxvar[[i]]), levels = covnames[[i]])
                }
            }else{
                as.numeric(tclvalue(boxvar[[i]]))
            }
        }, simplify = FALSE))
    }else{
        NULL
    }

    tkdestroy(tt)
    return(newdata)

}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

choose_array = function(x, padx = 1){

    if(inherits(x, "gibbonsecr_fit"))
        x = x$capthist
    if(!inherits(x, "capthist"))
        stop("capthist object required")

    tt = tktoplevel()
    tkwm.title(tt)
    w = 100
    h = 100
    scrw = as.numeric(tclvalue(tkwinfo("screenwidth", tt)))
    scrh = as.numeric(tclvalue(tkwinfo("screenheight", tt)))
    tkwm.geometry(tt, paste0(w, "x", h, "+", round((scrw - w) / 2), "+", round((scrh - h) / 2)))
    tcl("wm", "attributes", tt, topmost = TRUE)
    tcl("wm", "attributes", tt, topmost = FALSE)
    tkwm.geometry(tt, "")
    tkfocus(tt)
    main = ttkframe(tt)

    ##################################################
    ## upper frame for entry and combo boxes

    upper = ttkframe(main, padding = c(5,5))
    tkgrid(ttklabel(upper, text = "Choose array: "), row = 1, column = 1, sticky = "w")
    sessions = session(x)
    combo.tvar = tclVar(sessions[1])
    combo = ttkcombobox(upper, textvariable = combo.tvar, width = 10,
                        state = "normal", values = sessions)
    tkgrid(combo, row = 1, column = 2)

    ##################################################
    ## lower frame for buttons

    lower = ttkframe(main, padding = c(5,5))
    done = tclVar(0)
    ok = ttkbutton(lower, text = "OK", state = "normal", width = 10,
                   command = function() tclvalue(done) = 1)
    cancel = ttkbutton(lower, text = "Cancel", state = "normal", width = 10,
                       command = function() tclvalue(done) = 2)

    ##################################################
    # packing etc.

    tkgrid(ok, cancel, padx = padx)
    tkpack(upper)
    tkpack(lower)
    tkpack(main)
    tkwm.resizable(tt, 0, 0)
    tkwm.protocol(tt, "WM_DELETE_WINDOW", function() tclvalue(done) = 2)
    tkwait.variable(done)

    ##################################################
    # return clasess

    result = if(tclvalue(done) == 1) tclvalue(combo.tvar) else NA

    tkdestroy(tt)
    return(result)

}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# platform-specific appearance setting for tcltk widgets
# .Tcl("ttk::style theme names")
# sort(as.character(tkfont.families()))

gui_appearance_settings = function(){

    .Tcl(paste("source", system.file("doc/tcl/gibbonsecr_theme.tcl",
                                     package = "gibbonsecr")))

    general = list(
        WIDTH  = 1150,
        HEIGHT = 650,
        frame.padding = c(5,5),
        console.fg    = "grey80",
        console.bg    = "grey15",
        relief        = "flat"
    )

    if(.Platform$OS.type == "windows"){

        specific = list(
            min.height = 430,
            min.width = 355,
            lhs.width = 350,
            rhs.width = general$WIDTH - 350,
            button.width = 9,
            combo.width  = 15,
            entry.width  = 16,
            csv.entry.width = 28,
            fixed.entry.width  = 6,
            formula.entry.width  = 25,
            grid.padx    = 1,
            grid.pady    = 1,
            normal.font          = tkfont.create(size = 10, family = "Trebuchet MS"),
            heading.font         = tkfont.create(size = 10, family = "Trebuchet MS",
                                                 slant = "roman", weight = "bold"),
            console.normal.font  = tkfont.create(size = 9,  family = "Lucida Console"),
            console.heading.font = tkfont.create(size = 10, family = "Lucida Console"),
            console.bold.font    = tkfont.create(size = 9,  family = "Lucida Console", slant = "roman")
            )
        .Tcl("ttk::style configure TButton       -padding {0 1}")
        .Tcl("ttk::style configure TCombobox     -padding {5 2}")
        .Tcl("ttk::style configure TEntry        -padding {5 2}")
        .Tcl("ttk::style configure TNotebook.Tab -padding {1 1 1 1}")
        .Tcl("ttk::style map       TNotebook.Tab -padding [list selected {2 3 2 3}]")

    }else{

        specific = list(
            min.height = 500,
            min.width = 415,
            lhs.width = 410,
            rhs.width = general$WIDTH - 410,
            button.width = 8,
            combo.width  = 12,
            entry.width  = 15,
            csv.entry.width = 25,
            fixed.entry.width  = 6,
            formula.entry.width  = 22,
            grid.padx    = 2,
            grid.pady    = 2,
            heading.font         = tkfont.create(size = 10, family = "Lucida Grande",
                                                 slant  = "roman", weight = "bold"),
            console.normal.font  = tkfont.create(size = 10, family = "Courier New",
                                                 weight = "bold"),
            console.heading.font = tkfont.create(size = 11, family = "Courier New",
                                                 weight = "bold"),
            console.bold.font    = tkfont.create(size = 10, family = "Courier New",                                               weight = "bold")
        )
        .Tcl("ttk::style configure TButton       -padding {0 3}")
        .Tcl("ttk::style configure TEntry        -padding {5 4}")
        .Tcl("ttk::style configure TCombobox     -padding {5 4}")
        .Tcl("ttk::style configure TNotebook.Tab -padding {2 2 2 2}")
        .Tcl("ttk::style map       TNotebook.Tab -padding [list selected {3 4 3 4}]")

    }

    return(c(general,specific))

}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# code adadpted from utils::sessionInfo

welcome_message = function(){
    if(.Platform$OS.type == "windows") {
        running = win.version()
    }else if(nzchar(Sys.which("uname"))){
        uname = system("uname -a", intern = TRUE)
        os = sub(" .*", "", uname)
        running = switch(os, Linux = if (file.exists("/etc/os-release")) {
            tmp = readLines("/etc/os-release")
            t2 = if (any(grepl("^PRETTY_NAME=", tmp))) sub("^PRETTY_NAME=",
                "", grep("^PRETTY_NAME=", tmp, value = TRUE)[1L]) else if (any(grepl("^NAME",
                tmp))) sub("^NAME=", "", grep("^NAME=", tmp,
                value = TRUE)[1L]) else "Linux (unknown distro)"
            sub("\"(.*)\"", "\\1", t2)
        } else if (file.exists("/etc/system-release")) {
            readLines("/etc/system-release")
        }, Darwin = {
            ver = readLines("/System/Library/CoreServices/SystemVersion.plist")
            ind = grep("ProductUserVisibleVersion", ver)
            ver = ver[ind + 1L]
            ver = sub(".*<string>", "", ver)
            ver = sub("</string>$", "", ver)
            ver1 = strsplit(ver, ".", fixed = TRUE)[[1L]][2L]
            sprintf("OS X %s (%s)", ver, switch(ver1, `4` = "Tiger",
                `5` = "Leopard", `6` = "Snow Leopard", `7` = "Lion",
                `8` = "Mountain Lion", `9` = "Mavericks", `10` = "Yosemite",
                `11` = "El Capitan", "unknown"))
        }, SunOS = {
            ver = system("uname -r", intern = TRUE)
            paste("Solaris", strsplit(ver, ".", fixed = TRUE)[[1L]][2L])
        }, uname)
    }
    paste0("\n",
          "Welcome to gibbonsecr version ", utils::packageVersion("gibbonsecr"), "\n",
          "\n",
          R.Version()$version, "\n",
          running, "\n",
          "\n",
          "This software was developed in partnership with the IUCN SSC \nPrimate Specialist Group Section on Small Apes and the Centre \nfor Research into Ecological and Environmental Modelling at \nthe Univerisity of St Andrews, UK\n\n",
          "For help on using the software go to Help > User manual\n\n",
          "This is a pre-release version of the software. If you notice \nany bugs or have any general queries, please email Darren \nKidney at darrenkidney@yahoo.co.uk\n",
          "\n"
    )
}

