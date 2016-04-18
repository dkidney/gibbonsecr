
.onAttach = function(libname, pkgname){
    packageStartupMessage("\nThis is gibbonsecr version ",
                          utils::packageVersion("gibbonsecr"),
                          ". For help, type: ?gibbonsecr")
}

.onUnload = function(libpath){
    library.dynam.unload("gibbonsecr", libpath)
}
