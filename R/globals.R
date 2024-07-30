# templateK8 and templateK40 is are variables loaded from the data folder so we
# can't fix the note "no visible binding for global variable" by adding
# template <- NULL at the beginning of the function that uses template. Instead,
# we declare them here to fix the note.
utils::globalVariables(c("templateK8"))
utils::globalVariables(c("templateK40"))