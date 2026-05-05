
describe("exprObj", {

    ex <- test_data$ex

    a <- as.array(ex[])
    m <- as.matrix(ex[])
    dgC <- ex[]
    ex_IDs <- spatIDs(ex)

    it("is created from array", {
        exprObj <- createExprObj(a)
        expect_no_error(validObject(exprObj))
        expect_s4_class(exprObj, "exprObj")
        expect_setequal(ex_IDs, spatIDs(exprObj))
    })

    it("is created from matrix", {
        exprObj <- createExprObj(m)
        expect_no_error(validObject(exprObj))
        expect_s4_class(exprObj, "exprObj")
        expect_setequal(ex_IDs, spatIDs(exprObj))
    })

    it("is created from dgCMatrix", {
        exprObj <- createExprObj(dgC)
        expect_no_error(validObject(exprObj))
        expect_s4_class(exprObj, "exprObj")
        expect_setequal(ex_IDs, spatIDs(exprObj))
    })

})
