# solicits user input

readline(prompt="Say something: ")


# solicits a user integer input

readinteger <- function()
{ 
  n <- readline(prompt="Enter an integer: ")
  return(as.integer(n))
}

print(readinteger())