## fix the .tex file outputted by RStudio

# does 2 things:
#  1. fix the table captions to be at the end
#  2. make the authors go on separate lines

## 1. fix the table captions

rr <- readLines("paper.tex")

# where do the tables begin?
b_table <- which(grepl("\\begin{table}", rr, fixed=TRUE))
e_table <- which(grepl("\\end{table}", rr, fixed=TRUE))

# iterate over the tables
for(i in seq_along(b_table)){

  # get the start and end lines for this table
  bind <- b_table[i]
  eind <- e_table[i]

  # where is the caption?
  c_ind <- bind + which(grepl("\\caption{", rr[bind:eind], fixed=TRUE))-1
  # what is the caption?
  capt <- rr[c_ind]

  # reflow so we have:
  #  text up to caption
  #  rest of table after caption
  #  caption
  #  end of table, rest of text
  rr <- c(rr[1:(c_ind-1)],
          rr[(c_ind+1):(eind-1)],
          capt,
          rr[eind:length(rr)])

  # debugger for the above
  #aa <- c(rr[(c_ind+1):(eind-1)],
  #        capt,
  #        rr[eind])
  #print(paste0(aa, collapse=""))
}

# write out the file
writeLines(rr, "paper.tex")

##  2. make the authors go on separate lines

rr <- readLines("paper.tex")

authorline <- rr[which(grepl("\\author{", rr, fixed=TRUE))+1]

# I'm on a line on my own
authorline <- sub("\\\\And", "\\\\AND", authorline)
# 2 to a line after that
authorline <- sub("(.*?\\\\And.*?)\\\\And(.*)", "\\1\\\\AND\\2", authorline)

# insert that back into the document
rr[which(grepl("\\author{", rr, fixed=TRUE))+1] <- authorline

# write it out
writeLines(rr, "paper.tex")
