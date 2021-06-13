#' @title COUNTING SEQUENCE OCCURRENCES IN ARBITRARY SUBSETS OF A GENOME
#' @description It provides a function which takes
#' 1) a reference genome(e.g., human hg38),
#' 2) a set of interesting genomic regions (e.g., 3'UTRs of coding genes),
#' 3) a list of short search sequences (e.g. miRNA binding seeds), to be identified within
#'     the interesting genomic regions (exact matching)
#' and returns an overall count for each search sequence within the interesting genomic regions.
#' @param reference_genome A BSgenome object
#' @param interesting_regions A GRangesList object
#' @param search_patterns A RNAstring or DNAstring object
#' @return Overall count of exact matches within interesting_regions for each element in search_patterns
#' @examples
#' #BiocManager::install("TxDb.Hsapiens.UCSC.hg38.knownGene")
#' #BiocManager::install("BSgenome")
#' library(TxDb.Hsapiens.UCSC.hg38.knownGene)
#' library(BSgenome.Hsapiens.UCSC.hg38)
#' ##Retrieving TxDb object of Homo sapiens data from UCSC build hg38 based on the knownGene Track
#' txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene
#'
#' ##extracting 3' UTR from TxDb object
#' threeUTR <- threeUTRsByTranscript(txdb)
#'
#' ####Retrieving 3 miRNA sequences (22nt) from miRBase database###
#'
#' ##>hsa-let-7a-5p MIMAT0000062 Homo sapiens let-7a-5p
#' ##UGAGGUAGUAGGUUGUAUAGUU
#' ##selecting just the sequence from nt 2 to nt 7, which is the miRNA binding seed
#' search_seq1 <- RNAString("GAGGUAG")
#'
#' ##>hsa-miR-448 MIMAT0001532 Homo sapiens miR-448
#' ##UUGCAUAUGUAGGAUGUCCCAU
#' search_seq2 <- RNAString("UGCAUAU")
#'
#' ##>hsa-miR-429 MIMAT0001536 Homo sapiens miR-429
#' ##UAAUACUGUCUGGUAAAACCGU
#' search_seq3 <- RNAString("AAUACUG")
#'
#'
#' ##OVERALL COUNT for each query sequence
#' reference_genome <- Hsapiens #from BSgenome.Hsapiens.UCSC.hg38 package
#' interesting_regions <- threeUTR
#' ##putting the 3 RNAstrings in a list
#' search_patterns <- list(search_seq1, search_seq2, search_seq3)
#'
#' overall_count(reference_genome, interesting_regions, search_patterns)
#' ##[1] 4279 8255 8453
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



