# fix the table captions in the tex file

rr <- readLines("paper.tex")

# where do the tables begin?
b_table <- which(grepl("\\begin{table}", rr, fixed=TRUE))
e_table <- which(grepl("\\end{table}", rr, fixed=TRUE))

for(i in seq_along(b_table)){

  bind <- b_table[i]
  eind <- e_table[i]

  c_ind <- bind+which(grepl("\\caption{", rr[bind:eind], fixed=TRUE))-1
  capt <- rr[c_ind]

  #rr <- c(rr[1:bind], rr[(bind+2):(eind-1)],
  #        capt, rr[eind:length(rr)])

  rr <- c(rr[1:(c_ind-1)],
          rr[(c_ind+1):(eind-1)],
          capt,
          rr[eind:length(rr)])

  #aa <- c(rr[(c_ind+1):(eind-1)],
  #        capt,
  #        rr[eind])
  #print(paste0(aa, collapse=""))

#  print(capt)
}


# write out the file
writeLines(rr, "paper.tex")

