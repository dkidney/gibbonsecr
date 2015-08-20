
.onAttach = function(libname, pkgname){
    
    packageStartupMessage("This is gibbonsecr version ", utils::packageVersion("gibbonsecr"))
    
}

.onUnload = function(libpath){

    library.dynam.unload("gibbonsecr", libpath)

}
