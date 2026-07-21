ggboard_fixture_table <- function() {
  utils::read.csv(
    testthat::test_path("fixtures", "xgid-factual-fixtures.csv"),
    stringsAsFactors = FALSE,
    na.strings = c("")
  )
}

ggboard_fixture <- function(fixture_id) {
  fixtures <- ggboard_fixture_table()
  row <- fixtures[fixtures$fixture_id == fixture_id, , drop = FALSE]
  if (nrow(row) != 1L) stop("Unknown fixture: ", fixture_id, call. = FALSE)
  row$xgid[[1L]]
}

plot_position <- function(plot) attr(plot, "backgammon_position")
plot_cube_display <- function(plot) attr(plot, "backgammon_cube_display")


test_that("ggboard renders a complete XGID as an ordinary ggplot", {
  plot <- ggboard(ggboard_fixture("opening_white_roll"))

  expect_s3_class(plot, "ggplot")
  expect_s3_class(plot_position(plot), "backgammon_position")
  expect_identical(attr(plot, "backgammon_decision"), "checker_play")
  expect_identical(attr(plot, "backgammon_perspective"), "white")
})


test_that("ggboard covers factual cube, match, dice, and player states", {
  cases <- c(
    "centered_cube", "white_owned_cube", "black_owned_cube",
    "ordinary_match", "crawford", "non_crawford_match",
    "no_dice", "ordinary_dice", "doubles", "cube_64",
    "white_bar", "black_bar", "borne_off"
  )

  for (fixture_id in cases) {
    plot <- ggboard(ggboard_fixture(fixture_id), decision = "none")
    expect_s3_class(plot, "ggplot")
  }

  expect_identical(plot_cube_display(ggboard(ggboard_fixture("centered_cube"), decision = "none"))$state, "centered")
  expect_identical(plot_cube_display(ggboard(ggboard_fixture("white_owned_cube"), decision = "none"))$owner, "white")
  expect_identical(plot_cube_display(ggboard(ggboard_fixture("black_owned_cube"), decision = "none"))$owner, "black")
  expect_identical(plot_cube_display(ggboard(ggboard_fixture("crawford"), decision = "none"))$state, "hidden")
  expect_identical(plot_position(ggboard(ggboard_fixture("cube_64"), decision = "none"))$cube_value, 64L)
  expect_identical(plot_position(ggboard(ggboard_fixture("ordinary_dice")))$on_roll, "black")
  expect_identical(plot_position(ggboard(ggboard_fixture("doubles")))$dice, c(4L, 4L))
})


test_that("ggboard rejects malformed and unsupported factual XGIDs", {
  invalid <- utils::read.csv(
    testthat::test_path("fixtures", "xgid-invalid-fixtures.csv"),
    stringsAsFactors = FALSE
  )

  malformed <- invalid$xgid[invalid$fixture_id == "invalid_character"]
  cube_128 <- invalid$xgid[invalid$fixture_id == "cube_128"]

  expect_error(ggboard(malformed), "xgid_invalid_position_character", fixed = TRUE)
  expect_error(ggboard(cube_128), "xgid_unsupported_cube_value", fixed = TRUE)
})


test_that("perspective changes layout and never factual state", {
  xgid <- ggboard_fixture("asymmetric_white_roll")
  factual <- backgammon_position(xgid)

  white_plot <- ggboard(factual, decision = "none", perspective = "white")
  black_plot <- ggboard(factual, decision = "none", perspective = "black")

  expect_identical(plot_position(white_plot), factual)
  expect_identical(plot_position(black_plot), factual)
  expect_identical(attr(white_plot, "backgammon_perspective"), "white")
  expect_identical(attr(black_plot, "backgammon_perspective"), "black")

  style <- board_style("bms")
  white_geometry <- board_geometry(style, perspective = "white")
  black_geometry <- board_geometry(style, perspective = "black")
  expect_false(identical(white_geometry$point_layout$point, black_geometry$point_layout$point))

  expect_identical(factual, backgammon_position(xgid))
})


test_that("D action marker derives a factual pending cube offer", {
  offer_to_black <- ggboard(ggboard_fixture("offer_to_black"), decision = "none", perspective = "white")
  offer_to_white <- ggboard(ggboard_fixture("offer_to_white"), decision = "none", perspective = "white")

  black_display <- plot_cube_display(offer_to_black)
  white_display <- plot_cube_display(offer_to_white)

  expect_identical(black_display$state, "offered")
  expect_identical(black_display$value, 4L)
  expect_identical(black_display$offerer, "white")
  expect_identical(black_display$receiver, "black")
  expect_identical(black_display$placement, "offered_to_black")

  expect_identical(white_display$state, "offered")
  expect_identical(white_display$value, 4L)
  expect_identical(white_display$offerer, "black")
  expect_identical(white_display$receiver, "white")
  expect_identical(white_display$placement, "offered_to_white")

  expect_identical(plot_position(offer_to_black)$cube_value, 2L)
  expect_identical(plot_position(offer_to_black)$cube_owner, "white")
  expect_identical(plot_position(offer_to_white)$cube_value, 2L)
  expect_identical(plot_position(offer_to_white)$cube_owner, "black")
})


test_that("offered cube placement is receiver-aware under one perspective", {
  style <- board_style("bms")
  geometry <- board_geometry(style, perspective = "white")

  black_position <- backgammon_position(ggboard_fixture("offer_to_black"))
  white_position <- backgammon_position(ggboard_fixture("offer_to_white"))

  black_cube <- cube_layout(
    black_position,
    geometry,
    style,
    cube_display = resolve_cube_display(black_position, pending_offer_from_position(black_position)),
    perspective = "white"
  )
  white_cube <- cube_layout(
    white_position,
    geometry,
    style,
    cube_display = resolve_cube_display(white_position, pending_offer_from_position(white_position)),
    perspective = "white"
  )

  left_center <- mean(c(geometry$left_field$xmin, geometry$left_field$xmax))
  right_center <- mean(c(geometry$right_field$xmin, geometry$right_field$xmax))

  expect_equal(white_cube$center$x[[1L]], left_center)
  expect_equal(black_cube$center$x[[1L]], right_center)
  expect_false(isTRUE(all.equal(white_cube$center$x[[1L]], black_cube$center$x[[1L]])))
})


test_that("offered cube placement follows the receiver under both perspectives", {
  style <- board_style("bms")
  offers <- list(
    white = backgammon_position(ggboard_fixture("offer_to_white")),
    black = backgammon_position(ggboard_fixture("offer_to_black"))
  )

  for (perspective in c("white", "black")) {
    geometry <- board_geometry(style, perspective = perspective)
    left_center <- mean(c(geometry$left_field$xmin, geometry$left_field$xmax))
    right_center <- mean(c(geometry$right_field$xmin, geometry$right_field$xmax))

    for (receiver in c("white", "black")) {
      position <- offers[[receiver]]
      cube <- cube_layout(
        position,
        geometry,
        style,
        cube_display = resolve_cube_display(
          position,
          pending_offer_from_position(position)
        ),
        perspective = perspective
      )

      expected_x <- if (identical(receiver, perspective)) {
        left_center
      } else {
        right_center
      }
      expect_equal(cube$center$x[[1L]], expected_x)
    }
  }
})


test_that("default D status is factual and explicit response context asks the receiver", {
  cases <- list(
    offer_to_white = c(
      factual = "Black on roll",
      question = "White to decide: Take or Pass?"
    ),
    offer_to_black = c(
      factual = "White on roll",
      question = "Black to decide: Take or Pass?"
    )
  )

  for (fixture_id in names(cases)) {
    xgid <- ggboard_fixture(fixture_id)
    expected <- cases[[fixture_id]]

    factual_plot <- ggboard(xgid, decision = "none", perspective = "white")
    factual_position <- plot_position(factual_plot)
    expect_identical(position_status_label(factual_position), expected[["factual"]])
    expect_identical(attr(factual_plot, "backgammon_context")$decision, "none")

    response_plot <- ggboard(xgid, decision = "take_pass")
    response_context <- attr(response_plot, "backgammon_context")
    expect_identical(
      position_status_label(plot_position(response_plot), response_context),
      expected[["question"]]
    )
    expect_identical(
      attr(response_plot, "backgammon_perspective"),
      response_context$offer$receiver
    )
  }
})


test_that("D marker respects Crawford, ownership, package, and match cube limits", {
  payload <- "-b----E-C---eE---c-e----B-"

  expect_error(
    ggboard(paste0("XGID=", payload, ":6:1:1:D:0:0:0:0:10"), decision = "none"),
    "package_cube_limit",
    fixed = TRUE
  )

  expect_error(
    ggboard(paste0("XGID=", payload, ":0:0:1:D:6:2:1:7:10"), decision = "none"),
    "crawford",
    fixed = TRUE
  )

  expect_error(
    ggboard(paste0("XGID=", payload, ":1:-1:1:D:0:0:0:0:10"), decision = "none"),
    "opponent_owns_cube",
    fixed = TRUE
  )

  factual_match_cube <- ggboard(
    paste0("XGID=", payload, ":3:1:1:00:2:2:0:7:10"),
    decision = "none"
  )
  expect_identical(plot_cube_display(factual_match_cube)$state, "owned")
  expect_identical(plot_cube_display(factual_match_cube)$value, 8L)

  expect_error(
    ggboard(
      paste0("XGID=", payload, ":3:1:1:D:2:2:0:7:10"),
      decision = "none"
    ),
    "match_cube_limit",
    fixed = TRUE
  )
})


test_that("D marker supports the 1 to 2 and 32 to 64 offers", {
  payload <- "-b----E-C---eE---c-e----B-"

  cube_2 <- ggboard(paste0("XGID=", payload, ":0:0:1:D:0:0:0:0:10"), decision = "none")
  cube_64 <- ggboard(paste0("XGID=", payload, ":5:1:1:D:0:0:0:0:10"), decision = "none")

  expect_identical(plot_cube_display(cube_2)$value, 2L)
  expect_identical(plot_cube_display(cube_64)$value, 64L)
})


test_that("a D marker rejects a second cube-offer decision", {
  expect_error(
    ggboard(ggboard_fixture("offer_to_black"), decision = "roll_double"),
    "offer_already_pending",
    fixed = TRUE
  )
})

test_that("automatic branding moves opposite an offered cube", {
  cases <- list(
    take_or_pass_black = list(
      xgid = paste0(
        "XGID=-b----E-C---eE---c-e----B-",
        ":0:0:1:00:0:0:0:0:10"
      ),
      decision = "take_pass",
      perspective = "decision_maker",
      expected_perspective = "black"
    ),
    take_or_pass_white = list(
      xgid = paste0(
        "XGID=-b----E-C---eE---c-e----B-",
        ":0:0:-1:00:0:0:0:0:10"
      ),
      decision = "take_pass",
      perspective = "decision_maker",
      expected_perspective = "white"
    ),
    offer_to_white_white_perspective = list(
      xgid = ggboard_fixture("offer_to_white"),
      decision = "none",
      perspective = "white",
      expected_perspective = "white"
    ),
    offer_to_black_black_perspective = list(
      xgid = ggboard_fixture("offer_to_black"),
      decision = "none",
      perspective = "black",
      expected_perspective = "black"
    )
  )

  for (case in cases) {
    plot <- ggboard(
      case$xgid,
      decision = case$decision,
      perspective = case$perspective,
      brand_text = "Backgammon\nMade Simple"
    )

    expect_identical(
      attr(plot, "backgammon_perspective"),
      case$expected_perspective
    )
    expect_identical(
      offered_cube_field_side(
        plot_cube_display(plot)$receiver,
        attr(plot, "backgammon_perspective")
      ),
      "left"
    )
    expect_identical(
      attr(plot, "backgammon_brand_side"),
      "right"
    )
  }
})


test_that("automatic branding remains opposite offers shown on the right", {
  offer_to_white_black_perspective <- ggboard(
    ggboard_fixture("offer_to_white"),
    decision = "none",
    perspective = "black",
    brand_text = "Backgammon\nMade Simple"
  )
  offer_to_black_white_perspective <- ggboard(
    ggboard_fixture("offer_to_black"),
    decision = "none",
    perspective = "white",
    brand_text = "Backgammon\nMade Simple"
  )

  expect_identical(
    attr(offer_to_white_black_perspective, "backgammon_brand_side"),
    "left"
  )
  expect_identical(
    attr(offer_to_black_white_perspective, "backgammon_brand_side"),
    "left"
  )
})


test_that("explicit brand side overrides automatic placement", {
  position <- backgammon_position(ggboard_fixture("offer_to_white"))
  offer <- pending_offer_from_position(position)

  plot <- render_board_preview(
    x = position,
    perspective = "white",
    brand_text = "Backgammon\nMade Simple",
    brand_side = "left",
    cube_offer = offer,
    show_information = FALSE
  )

  expect_identical(attr(plot, "backgammon_brand_side"), "left")
})
