\name{renderer_position}
\alias{renderer_position}
\title{Adapt an Engine Kit RendererPosition envelope}
\usage{
renderer_position(x)
}
\arguments{
\item{x}{A parsed RendererPosition JSON object, JSON object string, or path
to a JSON file containing one accepted envelope.}
}
\value{
An object inheriting from \code{backgammon_position}, with
\code{backgammon_renderer_position} as its first class.
}
\description{
\code{renderer_position()} accepts the deterministic JSON envelope produced
by \code{backgammon-engine-kit} and maps its Universal Position facts into
the package's existing factual board model. Stable Engine Kit slots map as
\code{player_0} to White and \code{player_1} to Black internally; these names
are storage slots, not colour-based identities.
}
\details{
For the initial learner view, the accepted \code{bottom_player} is always the
learner, \code{top_player} is always the opponent, and
\code{point_labels_for} must equal the learner. The home-board side remains an
independent horizontal mirror choice. Changing the canonical on-roll player
moves the dice, on-roll arrow, and explicit status text without rotating or
mirroring the board.

The input may be a parsed named list, a JSON object string, or the path to a
UTF-8 JSON file. The adapter verifies supported schema versions and the
envelope structure, but deliberately does not calculate or replace Engine
Kit's semantic or view hashes.
}
