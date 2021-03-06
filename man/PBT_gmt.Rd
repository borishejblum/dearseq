% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PBT_gmt.R
\docType{data}
\name{PBT_gmt}
\alias{PBT_gmt}
\alias{PBT}
\title{PBT gene sets related to kidney transplant}
\format{
a \code{gmt} object containing 9 gene sets specific to kidney
transplant (see \code{\link[GSA]{GSA.read.gmt}}), which is simply a
\code{list} with the 3 following components:\itemize{
\item \code{genesets}: a \code{list} of \code{n} gene identifiers vectors 
composing eachgene set (each gene set is represented as the vector of the 
gene identifiers composing it)
\item \code{geneset.names}: a vector of length \code{n} containing the gene 
set names (i.e. gene sets identifiers)
\item{geneset.descriptions}: a vector of length \code{n} containing gene set 
descriptions (e.g. textual information on their biological function)
}
}
\source{
\url{http://atagc.med.ualberta.ca/Research/GeneLists}
}
\usage{
data(PBT_gmt)
}
\description{
9 Pathogenesis Based Transcripts (PBT) gene sets specifically related to
kidney transplant
}
\examples{
data('PBT_gmt')
PBT_gmt


}
\references{
Halloran PF, De Freitas DG, Einecke G, \emph{et al.}, The
molecular phenotype of kidney transplants: Personal viewpoint,
\emph{Am J Transplant}, 10: 2215-2222, 2010.
.

Sellares J, Reeve J, Loupy A, et al., Molecular diagnosis of
antibody-mediated rejection in human kidney transplants,
\emph{Am J Transplant}, 13:971-983, 2013.

Broin PO, Hayde N, Bao Y, et al.,  A pathogenesis-based transcript
signature in donor-specific antibody-positive kidney transplant patients with
normal biopsies, \emph{Genomics Data} 2: 357-60, 2014.
}
\keyword{datasets}
