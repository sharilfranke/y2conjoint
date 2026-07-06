.onLoad <- function(libname, pkgname) {
  S7::methods_register()
  # methods_register() drops plain S3 methods on the print generic, so restore
  # print.conjoint_df afterwards.
  registerS3method(
    "print",
    "conjoint_df",
    print_conjoint_df,
    envir = asNamespace(pkgname)
  )
}
