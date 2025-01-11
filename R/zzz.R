.datatable.aware = TRUE

.onLoad = function(libname, pkgname) {
  # Make sure data.table knows we are data.table aware
  setDTthreads()
}
