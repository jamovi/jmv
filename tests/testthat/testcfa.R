context('cfa')

test_that('cfa works', {

    data <- lavaan::HolzingerSwineford1939

    factors <- list(
        list(label='visual 1', vars=c("x1", "x2", "x3")),
        list(label='textual', vars=c("x4", "x5", "x6")),
        list(label='speed', vars=c("x7", "x8", "x9"))
    )

    resCov <- list(
            list(i1="x1",i2="x4")
    )

    cfa <- jmv::cfa(data=data, factors=factors, resCov=resCov)

    loadings <- cfa$factorLoadings$asDF

    # Test factor loadings table
    expect_equal(0.509, loadings$est[2], tolerance = 1e-3)
    expect_equal(0.063, loadings$se[5], tolerance = 1e-3)
    expect_equal(10.540, loadings$z[1], tolerance = 1e-3)

})
