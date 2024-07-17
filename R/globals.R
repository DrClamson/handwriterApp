# templateK40 is a variable loaded from the data folder so we can't silence the
# note "no visible binding for global variable" by adding template <- NULL at
# the beginning of the function that uses template. Instead, we declare it here
# to silence the note.
utils::globalVariables(c("templateK40"))