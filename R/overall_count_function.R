# library(GenomicRanges)
# library(GenomicFeatures)
# library(TxDb.Hsapiens.UCSC.hg38.knownGene)
# library(Biostrings)
# library(BSgenome.Hsapiens.UCSC.hg38)
# #
# #Retrieving TxDb object of Homo sapiens data from UCSC build
# #hg38 based on the knownGene Track
#
# txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene
# txdb
# threeUTR <- threeUTRsByTranscript(txdb) #extracting 3' UTR
# threeUTR
#
#
# #getSeq function from Biostrings package
# #retrieves DNA sequences of the 3' UTR regions
# UTR_seqs <- getSeq(Hsapiens, threeUTR)
# UTR_seqs
#
# ##Read the article "About miRNAs, miRNA seeds, target genes and target pathways",
# #they found miRNA sequences (22nt long) on database called miRBase,
# #downloaded Fasta format sequences of all mature miRNA sequences (mature.fa)
# ## Chose some miRNA sequences for Homo Sapiens
#
# ## From the article "Conserved seed pairing, often flanked by adenosines,
# #indicates that thousands of human genes are microRNA targets" I found out
# #that miRNA binding seed is from nt 2 to nt 7
#
# #>hsa-let-7a-5p MIMAT0000062 Homo sapiens let-7a-5p
# #UGAGGUAGUAGGUUGUAUAGUU
# let7a5ps <- RNAString("GAGGUAG") #selecting just the sequence from nt 2 to nt 7
# let7a5ps
# vcountPattern(let7a5ps, UTR_seqs@unlistData)
# sum(vcountPattern(let7a5ps, UTR_seqs@unlistData))
# #[1] 4251
#
# #>hsa-miR-448 MIMAT0001532 Homo sapiens miR-448
# #UUGCAUAUGUAGGAUGUCCCAU
# miR448s <- RNAString("UGCAUAU") #selecting just the sequence from nt 2 to nt 7
# miR448s
# vcountPattern(miR448s, UTR_seqs@unlistData)
# sum(vcountPattern(miR448s, UTR_seqs@unlistData))
# #[1] 7820
#
#
# #>hsa-miR-429 MIMAT0001536 Homo sapiens miR-429
# #UAAUACUGUCUGGUAAAACCGU
# miR429s <- RNAString("AAUACUG") #selecting just the sequence from nt 2 to nt 7
# miR429s
# vcountPattern(miR429s, UTR_seqs@unlistData)
# sum(vcountPattern(miR429s, UTR_seqs@unlistData))
# #[1] 8100
#
# #>hsa-miR-449a MIMAT0001541 Homo sapiens miR-449a
# #UGGCAGUGUAUUGUUAGCUGGU
# miR449as <- RNAString("GGCAGUG") #selecting just the sequence from nt 2 to nt 7
# miR449as
#
# ##putting the RNAstrings in a list which I'm gonna use in my for loop
# list1 <- list(let7a5ps, miR429s, miR448s, miR449as)
# names(list1) <- c("let7a5ps", "miR429s","miR448s", "miR449as") #assigning names to my strings
#
# ## OVERALL COUNT for each query sequence
# reference_genome <- Hsapiens
# interesting_regions <- threeUTR
# search_patterns <- list(miR429s, miR449as)

overall_count <- function(reference_genome, interesting_regions, search_patterns){

  interesting_seq <- getSeq(reference_genome, interesting_regions)

  sequences_unlisted <- interesting_seq@unlistData
  occurrences <- vapply(search_patterns, vcountPattern, numeric(length(sequences_unlisted)), subject=sequences_unlisted)
  colSums(occurrences)
}



