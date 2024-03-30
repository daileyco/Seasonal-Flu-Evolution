

summarize_Tree <- function(tree){
  
  
  require(ape)
  require(castor)
  require(picante)
  require(treeCentrality)
  require(phyloTop)
  
  
  if(class(tree)=="try-error"){
    return(data.frame(ntips = NA, 
                      imbalance.colless = NA, 
                      imbalance.collessnorm = NA, 
                      imbalance.sackin = NA, 
                      imbalance.blum = NA, 
                      # cherries = NA, 
                      pd.PD = NA, 
                      pd.SR = NA,
                      mpd = NA, 
                      branchlength.sum = NA))
  }  
  
  treesummary <- data.frame(ntips = try(Ntip(tree)),
                            
                            imbalance.colless = try(tree_imbalance(tree, "Colless")), 
                            imbalance.collessnorm = try(tree_imbalance(tree, "Colless_normalized")), 
                            imbalance.sackin = try(tree_imbalance(tree, "Sackin")), 
                            imbalance.blum = try(tree_imbalance(tree, "Blum")), 
                            
                            # cherries = as.numeric(try(gsub("[A-Za-z[:space:][:punct:]]", "", capture.output(cherry(tree))[which(grepl("Number of cherries: ", capture.output(cherry(tree))))]))), 
                            
                            pd = try(pd(matrix(data = c(1), 
                                           nrow = 1, 
                                           ncol = length(tree$tip.label), 
                                           dimnames = list("all", tree$tip.label)), 
                                    tree)),  
                            mpd = try(mpd(matrix(data = c(1), 
                                             nrow = 1, 
                                             ncol = length(tree$tip.label), 
                                             dimnames = list("all", tree$tip.label)), 
                                      cophenetic(tree))),
                            
                            branchlength.sum = try(sum(tree$edge.length)), 
                            avgladder = try(phyloTop::avgLadder(tree)))

  
  treesummary <- cbind(treesummary,
                       data.frame(t(treeCentrality::computeNetworkStats(tree))), 
                       data.frame(t(treeCentrality::computeBasicStats(tree))))
  
  
  return(treesummary)
  
  
  
  
}











