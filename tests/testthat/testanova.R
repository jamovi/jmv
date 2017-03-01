
context('anova')

data('ToothGrowth')

test_that('anova works', {

jmv::anova(ToothGrowth,
      dep='len',
      factors=c('dose', 'supp'))
})
