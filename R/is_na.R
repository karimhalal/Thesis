is_na<-function(vector){
return(vector=="NA(a)"| vector == "NA(b)" | vector == "NA(c)" | is.na(vector))
}