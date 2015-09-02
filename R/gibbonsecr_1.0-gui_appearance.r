## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##
##                                                                            ##
##                         gibbonsecr GUI settings                            ##
##                                                                            ##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

gui_appearance_settings = function(){

    general = list(
        main.window.width = 1000,
        model.options.frame.height = 100,
        model.submodels.frame.height = 100,
        default.padx = 3,
        default.pady = 2,
        tab.padx = 5,
        tab.pady = 10,
        console.padx = 10,
        console.pady = 10,
        separator.pady = 10,
        button.padding = c(3,3),
        tab.col = "grey97",
        bg.col = "grey97",
        default.text.col = "black",
        heading.text.col = "cornflowerblue",
        console.fg.col = "grey80",
        console.bg.col = "grey15",
        relief = "flat"
    )

    ##################################################
    ## os-specific settings

    if(.Platform$OS.type == "windows"){
        # options(device = "windows")
        .Tcl("ttk::style theme use xpnative")
        specific = list(
            main.window.height = 610,
            tab.frame.width = 370,
            console.window.width = 610 - 370,
            default.entry.width = 20,
            small.entry.width = 6,
            default.combobox.width = 15,
            button.width = 9,
            heading.font         = tkfont.create(size = 10, weight = "bold", slant = "roman"),
            console.normal.font  = tkfont.create(family = "lucida console", size = 9),
            console.heading.font = tkfont.create(family = "lucida console", size = 10),
            console.bold.font    = tkfont.create(family = "lucida console", size = 9, slant = "roman")
        )
    }else{
        # options(device = "x11")
        .Tcl("ttk::style theme use clearlooks")
        specific = list(
            main.window.height = 550,
            tab.frame.width = 400,
            console.window.width = 550 - 400,
            default.entry.width = 15,
            small.entry.width = 6,
            default.combobox.width = 12,
            button.width = 8,
            heading.font         = tkfont.create(size = 11, weight = "bold", slant = "roman"),
            console.normal.font  = tkfont.create(family = "Courier", size = 10),
            console.heading.font = tkfont.create(family = "Courier", size = 11),
            console.bold.font    = tkfont.create(family = "Courier", size = 10, weight = "bold", slant = "roman")
        )
    }

    return(c(general,specific))
    # return(list2env(c(general,specific)))

}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##
