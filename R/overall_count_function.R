### Function which returns the overall count of each search pattern
### within interesting regions of a reference genome given in input

##Reference_genome must be BSgenome objects
##Interesting_regions must be GRangesList objects
##search_patterns must be RNAstring or DNAstring objects
overall_count <- function(reference_genome, interesting_regions, search_patterns){
  ##getSeq function retrieves DNA sequences from the interesting regions
  #It returns a DNAStringSetList
  interesting_seq <- getSeq(reference_genome, interesting_regions)

  #@unlistData is a method to unlist the set of list of the interesting_seq
  sequences_unlisted <- interesting_seq@unlistData

  #vcountPattern function finds all the occurrences(aka exact matches) of sequences_unlisted in search_patterns
  occurrences <- vapply(search_patterns, vcountPattern, numeric(length(sequences_unlisted)), subject=sequences_unlisted)
  colSums(occurrences)
}



