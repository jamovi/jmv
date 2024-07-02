testthat::context('regutils')

testthat::test_that("Correct reference levels are not updated", {
    # GIVEN a dataframe with a factor column
    df <- data.frame(
        var = c('a', 'b', 'c', 'd', 'e'),
        stringsAsFactors = TRUE
    )
    # AND correctly defined reference levels
    refLevels <- list(list(var="var", ref="a"))

    # WHEN the reference levels are updated
    refLevelsUpdated <- jmv:::getReferenceLevels(df, "var", refLevels)

    # THEN the updated reference levels are the same as the original
    testthat::expect_equal(refLevelsUpdated$refLevels, refLevels)
    # AND the reference levels are not updated
    testthat::expect_false(length(refLevelsUpdated$changedVars) > 0)
})

testthat::test_that("Reference levels are set correctly with no original reference levels", {
    # GIVEN a dataframe with a factor column
    df <- data.frame(
        var = c('a', 'b', 'c', 'd', 'e'),
        stringsAsFactors = TRUE
    )
    # AND no original reference levels
    refLevels <- list()

    # WHEN the reference levels are updated
    refLevelsUpdated <- jmv:::getReferenceLevels(df, "var", refLevels)

    # THEN the updated reference levels are set to the first level
    testthat::expect_equal(refLevelsUpdated$refLevels, list(list(var="var", ref="a")))
    # AND the reference levels are updated
    testthat::expect_true(length(refLevelsUpdated$changedVars) > 0)
})

testthat::test_that("Reference levels are corrected with faulty orginal reference levels", {
    # GIVEN a dataframe with a factor column
    df <- data.frame(
        var = c('a', 'b', 'c', 'd', 'e'),
        stringsAsFactors = TRUE
    )
    # AND faulty original reference levels
    refLevels <- list(list(var="var", ref="X"))

    # WHEN the reference levels are updated
    refLevelsUpdated <- jmv:::getReferenceLevels(df, "var", refLevels)

    # THEN the updated reference levels are set to the first level
    testthat::expect_equal(refLevelsUpdated$refLevels, list(list(var="var", ref="a")))
    # AND the reference levels are updated
    testthat::expect_true(length(refLevelsUpdated$changedVars) > 0)
})

testthat::test_that("Reference levels are added when level is not contained in original", {
    # GIVEN a dataframe with two factors
    df <- data.frame(
        var1 = rep(letters[1:2], length.out=10),
        var2 = rep(LETTERS[3:4], length.out=10),
        stringsAsFactors = TRUE
    )
    # AND the original reference levels are only set for var1
    refLevels <- list(list(var="var1", ref="a"))

    # WHEN the reference levels are updated
    refLevelsUpdated <- jmv:::getReferenceLevels(df, c("var1", "var2"), refLevels)

    # THEN the updated reference levels are set to the first level
    testthat::expect_equal(
        refLevelsUpdated$refLevels,
        list(list(var="var1", ref="a"), list(var="var2", ref="C"))
    )
    # AND the reference levels are updated
    testthat::expect_true(length(refLevelsUpdated$changedVars) > 0)
})
