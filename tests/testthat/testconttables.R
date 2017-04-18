context('conttables')

test_that('conttables works', {

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

    set.seed(212)

    rows <- factor(c("A","B","C","A","B","C","A","B","C","A","B","C"), c("A","B","C"))
    cols <- factor(c("1","1","1","2","2","2","1","1","1","2","2","2"), c("1","2"))
    layer <- factor(c("I","I","I","I","I","I","II","II","II","II","II","II"), c("I","II"))
    counts <- sample(0:20, 12, replace = TRUE)

    data2 <- data.frame(rows = rows, cols = cols, layer = layer, counts = counts)

    table3 <- jmv::contTables(data=data2, rows="rows", cols="cols", layers="layer", counts="counts")

    freqs3 <- as.data.frame(table3$freqs)

    expect_equal(8, freqs3[1, '1[count]'])
    expect_equal(3, freqs3[1, '2[count]'])
    expect_equal(17, freqs3[2, '1[count]'])
    expect_equal(0, freqs3[2, '2[count]'])
    expect_equal(84, freqs3[12, '1[count]'])
    expect_equal(32, freqs3[12, '2[count]'])
})
