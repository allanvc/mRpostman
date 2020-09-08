#' Add sufix to a filename if it already exists in local folder
#' @param prefix A character string with a message content.
#' @noRd
serialize_filename = function(sufix, complete_path){
  # sol: https://stackoverflow.com/questions/25429557/how-to-create-a-new-output-file-in-r-if-a-file-with-that-name-already-exists
  complete_path_with_filename <- paste0(complete_path, "/", sufix)

  if(!file.exists(complete_path_with_filename)){return(complete_path_with_filename)}
  i=1
  repeat {
    # f = paste(prefix,i,sep="_")
    # f = paste0(sufix, "(", i, ")")
    # f = paste0("(", i, ")", sufix)
    complete_path_with_filename <- paste0(complete_path, "/", "(", i, ")", sufix)
    if(!file.exists(complete_path_with_filename)){return(complete_path_with_filename)}
    i=i+1
  }
}
