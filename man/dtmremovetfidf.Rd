% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtmremovetfidf.R
\name{dtmremovetfidf}
\alias{dtmremovetfidf}
\title{Remove terms from a Document-Term-Matrix and documents with no terms based on the term frequency inverse document frequency}
\usage{
dtmremovetfidf(dtm, top, cutoff, prob, remove_emptydocs = TRUE)
}
\arguments{
\item{dtm}{an object class "dgCMatrix"}

\item{top}{integer with the number of terms which should be kept as defined by the highest mean tfidf}

\item{cutoff}{numeric cutoff value to keep only terms in \code{dtm} where the tfidf obtained by \code{dtmtfidf} is higher than this value}

\item{prob}{numeric quantile indicating to keep only terms in \code{dtm} where the tfidf obtained by \code{dtmtfidf} is higher than
the \code{prob} percent quantile}

\item{remove_emptydocs}{logical indicating to remove documents containing no more terms after the term removal is executed. Defaults to \code{TRUE}.}
}
\value{
a sparse Matrix as returned by \code{sparseMatrix}
where terms with high tfidf are kept and documents without any remaining terms are removed
}
\description{
Remove terms from a Document-Term-Matrix and documents with no terms based on the term frequency inverse document frequency.
Either giving in the maximum number of terms (argument \code{top}), the tfidf cutoff (argument \code{cutoff})
or a quantile (argument \code{prob})
}
