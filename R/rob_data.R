#' Example QUADAS-2 assessment dataset
#'
#' @description A data frame containing 12 example assessments performed using
#' the risk-of-bias portion of the QUADAS-2 tool for the assessment of
#' diagnostic accuracy studies.
#'
#' @format A data frame with the following 6 columns:
#' \describe{
#'   \item{Study}{Study identifier}
#'   \item{D1}{Domain 1 - Patient Selection}
#'   \item{D2}{Domain 2 - Index Test}
#'   \item{D3}{Domain 3 - Reference Standard}
#'   \item{D4}{Domain 4 -  Flow & Timing}
#'   \item{Overall}{Overall risk of bias}
#'          }
#' @source {Created for this package}
#' @keywords internal
"data_quadas"

#' Example QUIPS assessment dataset
#'
#' @description A data frame containing 12 example assessments performed using
#' the QUIPS (Quality In Prognosis Studies) tool.
#'
#' @format A data frame with the following 8 columns:
#' \describe{
#'   \item{Study}{Study identifier}
#'   \item{D1}{Domain 1 - Bias due to participation}
#'   \item{D2}{Domain 2 - Bias due to attrition}
#'   \item{D3}{Domain 3 - Bias due to prognostic factor measurement}
#'   \item{D4}{Domain 4 - Bias due to outcome measurement}
#'   \item{D5}{Domain 5 - Bias due to confounding}
#'   \item{D6}{Domain 6 - Bias in statistical analysis and reporting}
#'   \item{Overall}{Overall risk of bias}
#'          }
#' @source {Created for this package}
#' @keywords internal
"data_quips"

#' Example ROB1 assessment dataset
#'
#' @description A data frame containing 9 example assessments performed using
#'   the RoB1 assessment tool. Note that this dataset is distinct from other
#'   example datasets included in this package, in that the column names are the
#'   full domain names, rather than domain shortcodes (e.g. D1, D2, etc.). This
#'   is because the "Generic" (formerly "RoB1") template offered by
#'   \code{robvis} allows users to create custom risk-of-bias plots, and uses
#'   the column names of the provided dataset to define the domain names for
#'   these custom plots.
#'
#' @format A data frame with the following 9 columns:
#' \describe{
#'   \item{Study}{Study identifier}
#'   \item{Random.sequence.generation}{Domain 1}
#'   \item{Allocation.concealment}{Domain 2}
#'   \item{Blinding.of.participants.and.personnel}{Domain 3}
#'   \item{Blinding.of.outcome.assessment}{Domain 4}
#'   \item{Incomplete.outcome.data}{Domain 5}
#'   \item{Selective.reporting}{Domain 6}
#'   \item{Other.sources.of.bias}{Domain 7}
#'   \item{Overall}{Overall risk of bias}
#'          }
#' @source {Created for this package}
#' @keywords internal

"data_rob1"

#' Example ROB2.0 assessment dataset
#'
#' @description A data frame containing 9 example assessments performed using
#' the RoB 2.0 tool for randomised controlled trials.
#'
#' @format A data frame with the following 7 columns:
#' \describe{
#'   \item{Study}{Study identifier}
#'   \item{D1}{Domain 1 - Bias arising from the randomization process}
#'   \item{D2}{Domain 2 - Bias due to deviations from intended intervention}
#'   \item{D3}{Domain 3 - Bias due to missing outcome data}
#'   \item{D4}{Domain 4 - Bias in measurement of the outcome}
#'   \item{D5}{Domain 5 - Bias in selection of the reported result}
#'   \item{Overall}{Overall risk of bias}
#'          }
#' @source {Created for this package}
#' @keywords internal
"data_rob2"

#' Example ROB2.0 (cluster variant) assessment dataset
#'
#' @description A data frame containing 9 example assessments performed using
#' the cluster-randomised version of the RoB 2.0 tool.
#'
#' @format A data frame with the following 7 columns:
#'
#' \describe{
#'   \item{Study}{Study identifier}
#'   \item{D1}{Domain 1 - Bias arising from the randomization process}
#'   \item{D1b}{Domain 1b - Bias arising from the timing of identification and recruitment of Individual participants in relation to timing of randomization}
#'   \item{D2}{Domain 2 - Bias due to deviations from intended intervention}
#'   \item{D3}{Domain 3 - Bias due to missing outcome data}
#'   \item{D4}{Domain 4 - Bias in measurement of the outcome}
#'   \item{D5}{Domain 5 - Bias in selection of the reported result}
#'   \item{Overall}{Overall risk of bias}
#'          }
#' @source {Created for this package}
#' @keywords internal

"data_rob2_cluster"

#' Example ROBINS-I assessment dataset
#'
#' @description A data frame containing 10 example assessments performed using
#' the ROBINS-I (Risk Of Bias In Non-randomised Studies - of Interventions) tool.
#'
#' @format A data frame with the following 9 columns:
#'
#' \describe{
#'   \item{Study}{Study identifier}
#'   \item{D1}{Domain 1 - Bias due to confounding}
#'   \item{D2}{Domain 2 - Bias due to selection of participants}
#'   \item{D3}{Domain 3 - Bias in classification of interventions}
#'   \item{D4}{Domain 4 - Bias due to deviations from intended interventions}
#'   \item{D5}{Domain 5 - Bias due to missing data}
#'   \item{D6}{Domain 6 - Bias in measurement of outcomes}
#'   \item{D7}{Domain 7 - Bias in selection of the reported result}
#'   \item{Overall}{Overall risk of bias}
#'          }
#' @source {Created for this package}
#' @keywords internal

"data_robins_i"

#' Example ROBINS-E assessment
#'
#' @description A data frame containing 10 example assessments performed using
#'   the ROBINS-E (Risk Of Bias In Non-randomised Studies - of Exposures) tool.
#'
#' @format A data frame with the following 9 columns:
#'
#' \describe{
#'   \item{Study}{Study identifier}
#'   \item{D1}{Domain 1 - Bias due to confounding}
#'   \item{D2}{Domain 2 - Bias arising from measurement of the exposure}
#'   \item{D3}{Domain 3 - Bias in selection of participants into the study (or into the analysis)}
#'   \item{D4}{Domain 4 - Bias due to post-exposure interventions}
#'   \item{D5}{Domain 5 - Bias due to missing data}
#'   \item{D6}{Domain 6 - Bias arising from measurement of the outcome}
#'   \item{D7}{Domain 7 - Bias in selection of the reported result}
#'   \item{Overall}{Overall risk of bias}
#'          }
#' @source {Created for this package}
#' @keywords internal

"data_robins_e"

# TODO Update data documentation for this dataframe

#'@title Example directional risk-of-bias dataset
#'@description This dataset is include to support experimental aspects of the
#'  package.
#'@format A data frame with 20 rows and 27 variables:
#' \describe{
#'   \item{\code{result_id}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{study}}{character COLUMN_DESCRIPTION}
#'   \item{\code{type}}{character COLUMN_DESCRIPTION}
#'   \item{\code{yi}}{double COLUMN_DESCRIPTION}
#'   \item{\code{vi}}{double COLUMN_DESCRIPTION}
#'   \item{\code{d1j}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d1t}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d1d}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d2j}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d2t}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d2d}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d3j}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d3t}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d3d}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d4j}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d4t}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d4d}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d5j}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d5t}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d5d}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d6j}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d6t}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d6d}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d7j}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d7t}}{character COLUMN_DESCRIPTION}
#'   \item{\code{d7d}}{character COLUMN_DESCRIPTION}
#'   \item{\code{overall}}{character COLUMN_DESCRIPTION}
#'}
#'@source {Created for this package}
#'@keywords internal

"data_bias_direction"
