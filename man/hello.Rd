\name{overall_count}
\alias{overall_count}
\title{COUNTING SEQUENCE OCCURRENCES IN ARBITRARY SUBSETS OF THE GENOME}
\usage{
overall_count(reference_genome, interesting_regions, search_patterns)
}
\description{
It provides a function which takes
1) a reference genome( e.g human hg38),
2) a set of interesting genomic regions (e. g., 3'UTRs of coding genes),
3) a list of short search sequences (e.g. iRNA binding seeds), to be identified within
    the interesting genomic regions (exact matching)
and returns an overall count for each search sequence within the interesting genomic regions.
}
\examples{
overall_count(Hsapiens, ThreeUTR_regions, miRNA_binding_seeds)
}
