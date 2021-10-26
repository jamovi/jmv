context('conttables')

test_that('conttables works without counts', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(100)

    x <- factor(sample(c("A","B"), 100, replace = TRUE), c("A","B"))
    y <- factor(sample(c("I","II"), 100, replace = TRUE), c("I","II"))
    z <- factor(sample(c("foo","bar"), 100, replace = TRUE), c("foo","bar"))
    w <- factor(sample(c("fred","steve"), 100, replace = TRUE), c("fred","steve"))

    data1 <- data.frame(x = x, y = y, z = z, w = w)

    table1<- jmv::contTables(data=data1, rows="x", cols="y")

    freqs1 <- as.data.frame(table1$freqs)

    expect_equal(28, freqs1[1, '1[count]'])
    expect_equal(22, freqs1[1, '2[count]'])
    expect_equal(22, freqs1[2, '1[count]'])
    expect_equal(28, freqs1[2, '2[count]'])

    table2 <- jmv::contTables(data=data1, rows="x", cols="y", layers=c("z","w"))

    freqs2 <- as.data.frame(table2$freqs)

    expect_equal(28, freqs2[25, '1[count]'])
    expect_equal(22, freqs2[25, '2[count]'])
    expect_equal(22, freqs2[26, '1[count]'])
    expect_equal(28, freqs2[26, '2[count]'])

    expect_equal(9, freqs2[10, '1[count]'])
    expect_equal(4, freqs2[10, '2[count]'])
    expect_equal(4, freqs2[11, '1[count]'])
    expect_equal(6, freqs2[11, '2[count]'])
})

test_that("conttables works with counts", {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(212)

    rows <- factor(c("A","B","C","A","B","C","A","B","C","A","B","C"), c("A","B","C"))
    cols <- factor(c("1","1","1","2","2","2","1","1","1","2","2","2"), c("1","2"))
    layer <- factor(c("I","I","I","I","I","I","II","II","II","II","II","II"), c("I","II"))
    counts <- sample(0:20, 12, replace = TRUE)

    data <- data.frame(rows = rows, cols = cols, layer = layer, counts = counts)

    table <- jmv::contTables(data=data, rows="rows", cols="cols", layers="layer", counts="counts")

    freqs <- as.data.frame(table$freqs)

    expect_equal(8, freqs[1, '1[count]'])
    expect_equal(3, freqs[1, '2[count]'])
    expect_equal(17, freqs[2, '1[count]'])
    expect_equal(0, freqs[2, '2[count]'])
    expect_equal(84, freqs[12, '1[count]'])
    expect_equal(32, freqs[12, '2[count]'])
})

test_that("bar plots work with spaces in variable name", {
    data <- ToothGrowth
    data$dose <- factor(data$dose)
    names(data) <- c("len", "su pp", "do se")

    table <- jmv::contTables(data=data, rows="su pp", cols="do se", barplot=TRUE)

    expect_true(table$barplot$.render())
})
