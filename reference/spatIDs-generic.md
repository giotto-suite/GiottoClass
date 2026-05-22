# Spatial and feature IDs

Get the cell/spot IDs (termed spatial IDs to better reflect when not at
the single-cell level) and feature IDs of a giotto object or subobject.

\[**`giotto` object specific**\] When applied on a `giotto` object,
these functions pull from the `cell_ID` and `feat_ID` slots. The values
within these slots are updated whenever the object is data is changed
and, importantly, whenever the active spat_unit and feat_type is set
(see
[`activeSpatUnit()`](https://giotto-suite.github.io/GiottoClass/reference/activeSpatUnit-generic.md)
and
[`activeFeatType()`](https://giotto-suite.github.io/GiottoClass/reference/activeFeatType-generic.md)).
New values for these slots are specific to the active spat_unit and
feat_type and are detected from either the *subcellular* level
(`giottoPolygon` and `giottoPoints`) or the *aggregate* level
(expression matrix) data, with a preference for the latter if it exists.
Be aware that with this current behavior, values returned by`spatIDs()`
and `featIDs()` should be regarded as the minimal set of expected IDs
within all `giotto` slots, and not always the exact set or ordering.

## Usage

``` r
# S4 method for class 'giottoBinPoints'
featIDs(x, uniques = TRUE, ...)

# S4 method for class 'giotto'
spatIDs(x, spat_unit = NULL, subset, negate = FALSE, quote = TRUE, ...)

# S4 method for class 'exprObj'
spatIDs(x, ...)

# S4 method for class 'spatLocsObj'
spatIDs(x, ...)

# S4 method for class 'cellMetaObj'
spatIDs(x, ...)

# S4 method for class 'spatialNetworkObj'
spatIDs(x, ...)

# S4 method for class 'dimObj'
spatIDs(x, ...)

# S4 method for class 'giottoPolygon'
spatIDs(x, use_cache = TRUE, uniques = TRUE, ...)

# S4 method for class 'giottoPolygon'
spatIDs(x, old = NULL, ...) <- value

# S4 method for class 'spatEnrObj'
spatIDs(x, ...)

# S4 method for class 'nnNetObj'
spatIDs(x, ...)

# S4 method for class 'giotto'
featIDs(x, feat_type = NULL, subset, negate = FALSE, quote = TRUE, ...)

# S4 method for class 'exprObj'
featIDs(x, ...)

# S4 method for class 'featMetaObj'
featIDs(x, ...)

# S4 method for class 'giottoPoints'
featIDs(x, use_cache = TRUE, uniques = TRUE, ...)

# S4 method for class 'spatEnrObj'
featIDs(x, ...)
```

## Arguments

- x:

  an object

- uniques:

  return unique ID values only (currently gpoly and gpoints only)

- ...:

  additional params to pass when used with the `subset` param. For
  `spatID()`, these pass to
  [`spatValues()`](https://giotto-suite.github.io/GiottoClass/reference/spatValues.md).
  For `featID()`, these currently only pass to
  [`fDataDT()`](https://giotto-suite.github.io/GiottoClass/reference/fDataDT.md).

- spat_unit:

  (optional) specify which spatial unit

- subset:

  logical expression to find a subset of features.

- negate:

  logical. if `TRUE` all IDs that are **not** in the `subset` are
  selected

- quote:

  logical. If `TRUE`, the `subset` param will be quoted with
  [`substitute()`](https://rdrr.io/r/base/substitute.html). Set this to
  `FALSE` when calling from a function, although that may not be
  recommended since NSE output can be unexpected when not used
  interactively.

- use_cache:

  use cached IDs if available (gpoly and gpoints only)

- old:

  character. IDs to match against to replace

- value:

  character. IDs to replace with

- feat_type:

  (optional) specify which feature type

## Value

character vector of cell/spatial IDs or feature IDs

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10
spatIDs(g)
#>   [1] "AAAGGGATGTAGCAAG-1" "AAATGGCATGTCTTGT-1" "AAATGGTCAATGTGCC-1"
#>   [4] "AAATTAACGGGTAGCT-1" "AACAACTGGTAGTTGC-1" "AACAGGAAATCGAATA-1"
#>   [7] "AACAGGATGGGCCGCG-1" "AACCATGGGATCGCTA-1" "AACCCAGAGACGGAGA-1"
#>  [10] "AACCGAGCTTGGTCAT-1" "AACCGTTGTGTTTGCT-1" "AACGATAGAAGGGCCG-1"
#>  [13] "AACGATATGTCAACTG-1" "AACGCGGTCTCCAGCC-1" "AACGTCAGACTAGTGG-1"
#>  [16] "AACTCGATGGCGCAGT-1" "AACTGATATTAGGCCT-1" "AACTGGGTCCCGACGT-1"
#>  [19] "AACTTGCCCGTATGCA-1" "AAGAGATGAATCGGTA-1" "AAGCTCGTGCCAAGTC-1"
#>  [22] "AAGGAGCGGTTGGTGC-1" "AAGGCGCGTAAAGCTT-1" "AAGGCTGTGCTCATCG-1"
#>  [25] "AAGTAGAAGACCGGGT-1" "AAGTAGTGACGCGAGG-1" "AATCCAAGGGCCTGAG-1"
#>  [28] "AATGACTGTCAGCCGG-1" "AATGCAACCGGGTACC-1" "AATGGTTCTCACAAGC-1"
#>  [31] "AATTTGGTTCCAAAGA-1" "ACAACAGCATGAGCTA-1" "ACAACGGTCCCTGCGA-1"
#>  [34] "ACAAGGACAAGAGGTT-1" "ACACAAAGACGGGTGG-1" "ACACACTTTCTACACG-1"
#>  [37] "ACACCCAGCATGCAGC-1" "ACACCCGAGAAATCCG-1" "ACACCTTAAGTAGGGC-1"
#>  [40] "ACACGAGACTCCTTCT-1" "ACACGGGAACTTAGGG-1" "ACATAATAAGGCGGTG-1"
#>  [43] "ACCAAGAACGCGTGTC-1" "ACCACAAGTTTCTATC-1" "ACCCTTCATCTGCGAA-1"
#>  [46] "ACCGACTGAGTCCCAC-1" "ACCTAATCGACTTCCT-1" "ACGATCATACATAGAG-1"
#>  [49] "ACGCATTCGTGAGTAC-1" "ACGCCAGATGATTTCT-1" "ACGCGGGCCAAGGACA-1"
#>  [52] "ACGGAATTTAGCAAAT-1" "ACGTAGATTGCTGATG-1" "ACGTGCGCCTCGTGCA-1"
#>  [55] "ACGTTTAGTTGTGATC-1" "ACTACCAGCTCTCTGG-1" "ACTATTCGTCCGTGGT-1"
#>  [58] "ACTCAATAAAGGCACG-1" "ACTGCCGTCGTAACTC-1" "ACTGTAGCACTTTGGA-1"
#>  [61] "ACTGTCCAGGATTATA-1" "ACTGTCTTCTTTAGAA-1" "ACTGTGCTAGTAGATC-1"
#>  [64] "ACTTAGTACGACAAGA-1" "ACTTCGCCATACGCAC-1" "ACTTGGGACCCGGTGG-1"
#>  [67] "ACTTTCCTATAGCTTC-1" "AGAACGTGGTACATTC-1" "AGACCATGGGATACAA-1"
#>  [70] "AGACCCGCCCTCCTCG-1" "AGACTAGCCTTCCAGA-1" "AGAGCGGGCTAATCAT-1"
#>  [73] "AGAGCGTACAAGCTCG-1" "AGATAACTTCAGGGCC-1" "AGATACGACTTCATAT-1"
#>  [76] "AGATCTCAGGTGTGAT-1" "AGATGACTCGCCCACG-1" "AGCACCAGTACTCACG-1"
#>  [79] "AGCACTTAAGGACGCC-1" "AGCATCGTCGATAATT-1" "AGCCCTAAGCGAAGTT-1"
#>  [82] "AGCGACATCCCATTCA-1" "AGCGACCAACGATATT-1" "AGCGGGAAGGGTCCAT-1"
#>  [85] "AGCTATTTAATCCAAC-1" "AGGACATCGCACGTCG-1" "AGGCCACCCGTTATGA-1"
#>  [88] "AGGCCCATTGTACAGG-1" "AGGCTATGGTTAGCTT-1" "AGGGCGTGATCGGCTA-1"
#>  [91] "AGGGCTGCAGTTACAG-1" "AGGGTTTAGTTCGGGA-1" "AGGTAACCTCCTATTC-1"
#>  [94] "AGGTACGATATTGCCA-1" "AGGTAGGTACAAAGCT-1" "AGGTTGAGGCACGCTT-1"
#>  [97] "AGTAATGTCTTGCCGC-1" "AGTAGGTAACATACAT-1" "AGTATAATACTAGGCA-1"
#> [100] "AGTCAGCCACCGCCTG-1" "AGTCGGCCCAAACGAC-1" "AGTCTTCTCCTCAAAT-1"
#> [103] "AGTTCCTACAGAATTA-1" "AGTTTCGCAGGTCGGA-1" "ATAAAGGCTCGGTCGT-1"
#> [106] "ATAACGGAGTCCAACG-1" "ATAATAGTGTAGGGAC-1" "ATACCACGGGCAACTT-1"
#> [109] "ATACTAGCATGACCCT-1" "ATAGACGAAGAGAAAG-1" "ATAGAGTACTGGGACA-1"
#> [112] "ATAGGGATATCCTTGA-1" "ATAGGTTGGGCAGATG-1" "ATATCTTAGGGCCTTC-1"
#> [115] "ATATTCCCACAGGTCA-1" "ATATTGCTGTCAAAGT-1" "ATATTTAACCCTCAAG-1"
#> [118] "ATCAATCTGGGCTGCA-1" "ATCACTTCATCCTCGC-1" "ATCCAACGCAGTCATA-1"
#> [121] "ATCGACTCTTTCCGTT-1" "ATCGCACGCCGGGAGA-1" "ATCTCCCTGCAATCTA-1"
#> [124] "ATCTGGTTAAGACTGT-1" "ATCTGTAATTGTACCC-1" "ATCTTGACCTGCAACG-1"
#> [127] "ATGAAGCCAAGGAGCC-1" "ATGACGCCGGCTCTAA-1" "ATGAGGAGTGTTAATC-1"
#> [130] "ATGATCGGGAATAGAC-1" "ATGCACTACCGCATTG-1" "ATGCATGATCCAGGAT-1"
#> [133] "ATGCCGGTCTTGCATA-1" "ATGCGACAGTCCCATT-1" "ATGCTCAGTGTTGCAT-1"
#> [136] "ATGGATTGACCAAACG-1" "ATGTGCATCCGACGCA-1" "ATGTTACGAGCAATAC-1"
#> [139] "ATTAATACTACGCGGG-1" "ATTAATTCGGTCACTC-1" "ATTACTAGCCTCTTGC-1"
#> [142] "ATTAGATTGATAGCGG-1" "ATTATGCCATAGGGAG-1" "ATTCCTCCGCCAGTGC-1"
#> [145] "ATTCTCGTCTCTTTAG-1" "ATTGAAGATCTTAGTG-1" "ATTGCTGCTCCTCCAT-1"
#> [148] "ATTGGGAATATCTTGG-1" "ATTTAACTCGTATTAC-1" "ATTTACAGTTTACTGG-1"
#> [151] "ATTTGTTCCAGGGCTC-1" "CAAACGGTCGCACTTT-1" "CAAACTATTGAGCTTC-1"
#> [154] "CAAATTGTCAGCAAGC-1" "CAACGACCCGTTTACA-1" "CAACGGTTCTTGATAC-1"
#> [157] "CAACTCCTTGATCCCG-1" "CAAGCAACGTCGGAGT-1" "CAAGCACCAAATGCCT-1"
#> [160] "CAATAAACCTTGGCCC-1" "CAATTAAGGGTGATGA-1" "CACACAGGGATAGATT-1"
#> [163] "CACAGCACCCACGGCA-1" "CACAGCTAGGGAGTGA-1" "CACCATCGGAGGAGAC-1"
#> [166] "CACCCACACGTCACCC-1" "CACCGCCAGAAGGTTT-1" "CACCTAATCAGTTTAC-1"
#> [169] "CACCTCGATGGTGGAC-1" "CACGAAAGTTAGTCCC-1" "CACGCACAGCGCAGCT-1"
#> [172] "CACGCAGCGAGGCTTT-1" "CACTAAAGTTGCCTAT-1" "CACTCAGCTCTTGAGG-1"
#> [175] "CACTGTCCAAGTGAGA-1" "CACTTAATCAGACGGA-1" "CACTTCGCCACAGGCT-1"
#> [178] "CAGAGCATGAGCTTGC-1" "CAGATACTAACATAGT-1" "CAGCTCACTGAGACAT-1"
#> [181] "CAGCTCGTGCTTGTGT-1" "CAGCTTAGTAGGTAGC-1" "CAGTGTCCGCAGAATG-1"
#> [184] "CAGTTCAAATTGACAC-1" "CATACAAAGCCGAACC-1" "CATAGTAGCATAGTAG-1"
#> [187] "CATCATCTACCCGGAC-1" "CATCGCCCGCGGCCAA-1" "CATGAACCGACATTTG-1"
#> [190] "CATGATGCACAATTCT-1" "CATGCGTTAGACAGAA-1" "CATGCTGGCTCCAATT-1"
#> [193] "CATGGCAGGAAGATCG-1" "CATGGTCTAGATACCG-1" "CATTACGTCGGCCCGT-1"
#> [196] "CATTATGCTTGTTGTG-1" "CCAAACAGAACCCTCG-1" "CCAACGATGCACTGAT-1"
#> [199] "CCAAGACTTCTGCGAA-1" "CCAAGCGTAACTCGTA-1" "CCAAGGAACAGAGAGG-1"
#> [202] "CCAATAGTGCCGTCGA-1" "CCAATTGAATGTTAAT-1" "CCACGAGAAGAGAATC-1"
#> [205] "CCAGGGACGTGGCCTC-1" "CCATAAACAACCCGAC-1" "CCATAGGTTGGCGTGG-1"
#> [208] "CCATCCATACCAAGTC-1" "CCATGCCTGTTTAGTA-1" "CCATGCTCTGCAGGAA-1"
#> [211] "CCATTAGCGATAATCC-1" "CCATTCCCTGCCCACA-1" "CCCGACCATAGTCCGC-1"
#> [214] "CCCGCCATGCTCCCGT-1" "CCCGTCAGCGTCTGAC-1" "CCCTCATTCTGGAATT-1"
#> [217] "CCCTGAAATGAGTTGA-1" "CCGAAAGTGGTGAGCA-1" "CCGCCGGTCAACACAC-1"
#> [220] "CCGGAATGGTTTCAGT-1" "CCGGCGCATATTGGAT-1" "CCGGGACCCGCAGAGA-1"
#> [223] "CCGGGCGGTCTCGTCA-1" "CCGTACCCAAGCGCCA-1" "CCGTGCCCATGACGGC-1"
#> [226] "CCTACGGCTCAGTCGA-1" "CCTACTGCTTACACTT-1" "CCTAGTTAGTCGCATG-1"
#> [229] "CCTATGGGTTACCGTC-1" "CCTCACCAATCTTGAC-1" "CCTCCGACAATTCAAG-1"
#> [232] "CCTCGAAGTGGACGGG-1" "CCTCGCCAGCAAATTA-1" "CCTCTAATCTGCCAAG-1"
#> [235] "CCTCTCTCCCATCTAG-1" "CCTCTGGCCTAGACGG-1" "CCTGAATATTTACATA-1"
#> [238] "CCTGCTATTTGAGAAG-1" "CCTGGCTAGACCCGCC-1" "CCTGTCACCCGGGCTC-1"
#> [241] "CCTTCTTGATCCAGTG-1" "CGAACGCCCAGTGCCG-1" "CGAAGTTGCTCTGTGT-1"
#> [244] "CGAGAGATGTGAACCT-1" "CGAGCGTTGATCAGCC-1" "CGAGCTGGGCTTTAGG-1"
#> [247] "CGAGGCTAAATATGGC-1" "CGCAAACACGAGTTAC-1" "CGCAATTACTTTCGGT-1"
#> [250] "CGCAATTAGGGTAATA-1" "CGCAATTCTACAATAA-1" "CGCATTAGCTAATAGG-1"
#> [253] "CGCGCCCGACTTAATA-1" "CGCGTTCATGAAATAC-1" "CGCTATACCGCCCACT-1"
#> [256] "CGCTATTCAATGTATG-1" "CGCTCGACATAATGAT-1" "CGCTCTCCGTAGATTA-1"
#> [259] "CGCTGTGACGCCGCAC-1" "CGCTTCCACTGAAATC-1" "CGCTTTCATACCGGTG-1"
#> [262] "CGGAGTTTGAGAGACA-1" "CGGCACTCAAGAAAGT-1" "CGGCCACGCACAAAGT-1"
#> [265] "CGGGATCAATGTAAGA-1" "CGGGCAGCTAAACCGC-1" "CGGTGTACTTGATCCC-1"
#> [268] "CGTACCTGATAGGCCT-1" "CGTAGCGAATTGTCAG-1" "CGTATTAAGAGATCTA-1"
#> [271] "CGTCAGTGCGCACAAG-1" "CGTCTGGAAGGGCCCG-1" "CGTGTCCCATTCGCGA-1"
#> [274] "CGTTGTTTCAATTCCC-1" "CGTTTAAGCGGAGCAC-1" "CGTTTCGCTCATTACA-1"
#> [277] "CTAATTCGCACGCGCT-1" "CTAATTTCAACAACAC-1" "CTACGCACGGAGTACC-1"
#> [280] "CTACTGCCACCTGACC-1" "CTAGCATAGTATAATG-1" "CTAGTGAAGGACAGGA-1"
#> [283] "CTAGTTGGGCCCGGTA-1" "CTATCGGGTCTCAACA-1" "CTATGTCACTAGCCCA-1"
#> [286] "CTATGTCTATTGAAAC-1" "CTCAGGACTCACCTGT-1" "CTCATGGCTCACAATC-1"
#> [289] "CTCATTTGATGGGCGG-1" "CTCCGGCCTAATATGC-1" "CTCCTAAGTTATGTCT-1"
#> [292] "CTCGAGGTCGAACAGT-1" "CTCGCACCTATATAGT-1" "CTCGGTCCGTAGCCTG-1"
#> [295] "CTCTCACAATCGATGA-1" "CTCTGCGAAGCAAGCA-1" "CTCTGGACGCCTGGTG-1"
#> [298] "CTCTGTTTGAGGATTC-1" "CTGAAAGAGATCCGAC-1" "CTGCACCTGGAACCGC-1"
#> [301] "CTGGAAATGGATGCTT-1" "CTGTATGGTGTAGAAA-1" "CTGTTCACTGCCTGTG-1"
#> [304] "CTGTTCATCTCACGGG-1" "CTTAACTTCGAAGTAC-1" "CTTACACGGTATTCCA-1"
#> [307] "CTTACACTGGGAAATA-1" "CTTATGTTGACTACCA-1" "CTTCATAGCTCAAGAA-1"
#> [310] "CTTCTTACGTCGTATA-1" "CTTGTACTTGTTGACT-1" "CTTGTTTATGTAGCCA-1"
#> [313] "CTTTAACTTTCAAAGG-1" "CTTTGGCTTTAGTAAA-1" "GAAACAGCCATGCAGT-1"
#> [316] "GAAACTCTAATGAAGG-1" "GAAAGTGACTAACTGC-1" "GAAATCGCGCGCAACT-1"
#> [319] "GAAATGGCGGTGTTAG-1" "GAAATTCACATCGCTG-1" "GAACACACATCAACCA-1"
#> [322] "GAACGACCGAATGATA-1" "GAAGAACGGTGCAGGT-1" "GAAGCGTGAGGAATTT-1"
#> [325] "GAAGCTTGCTGACCGC-1" "GAAGTGCTGGATAGCT-1" "GAATCGACATGGTCAC-1"
#> [328] "GAATCGCCGGACACGG-1" "GAATGTTGGGTAATCT-1" "GAATTATAGTGAAAGG-1"
#> [331] "GAATTTCTCGCTGCAG-1" "GACAACGCAGCTTACG-1" "GACACAAGGGAAGAAA-1"
#> [334] "GACCAGAGCCCTGTAG-1" "GACGCCTGTTGCAGGG-1" "GACGTGTAGGGATTAT-1"
#> [337] "GACTAAGATCATGCAC-1" "GACTAAGTAGGCTCAC-1" "GAGACTGATGGGTAGA-1"
#> [340] "GAGATCTGCTTGGCAT-1" "GAGATCTGTCACTCCG-1" "GAGCATCATCCCTGGG-1"
#> [343] "GAGCGAGGGAGTACCG-1" "GAGCTCTCGGACCTAA-1" "GAGGAATGGAGAGGTT-1"
#> [346] "GAGGCTATCAAAGTCG-1" "GAGGGCATCGCGTATC-1" "GAGGTACATCCATCTT-1"
#> [349] "GAGTACGGGTATACAA-1" "GATAAATCGGTGGATG-1" "GATATCTCATGCAATA-1"
#> [352] "GATCATTCCAAACATT-1" "GATCCCTTTATACTGC-1" "GATCGACACTATCTGA-1"
#> [355] "GATCGGTGGCCATAAC-1" "GATGCTACAAGCGCCT-1" "GATGGCGCACACATTA-1"
#> [358] "GATGTTTGTGCGAGAT-1" "GATTCCCTTGTCGCAG-1" "GATTCCGCGTTTCCGT-1"
#> [361] "GCAAATATTACGCTTT-1" "GCAAGTGCACAGAGAA-1" "GCACTAGTCGCGCTAT-1"
#> [364] "GCAGGACTATAGAATA-1" "GCATCCCTAACTTTGA-1" "GCCCACCAAGGCTGTC-1"
#> [367] "GCCCAGTTGGTATGCC-1" "GCCCGCGCGTTTGACA-1" "GCCGAAATTCCTACGT-1"
#> [370] "GCCTAGCGATCTGACC-1" "GCCTATTCCGATATAG-1" "GCCTTCAGCCCTACCG-1"
#> [373] "GCGAAGCCATACCCGT-1" "GCGAGTTCTGCAAAGA-1" "GCGCTAATTGAATAGA-1"
#> [376] "GCGGGAACCAGGCCCT-1" "GCGGTCCCTAGACGCA-1" "GCGTCGAAATGTCGGT-1"
#> [379] "GCTAATACCGAATGCC-1" "GCTAGTTTCATTGAGG-1" "GCTCTAAACCCTGACG-1"
#> [382] "GCTCTATGTTACGTGC-1" "GCTCTCGGGTACCGAA-1" "GCTGCTCTCCGGACAC-1"
#> [385] "GCTGGCATATTCACCT-1" "GCTTGATGATAATCAG-1" "GGAGAAGTCATTGGCA-1"
#> [388] "GGAGTGCCGCCCTGGA-1" "GGATTCAGTACGGTGG-1" "GGCAATAGTCAATGAG-1"
#> [391] "GGCACTCCACTGGGCA-1" "GGCATACAGGTAGCGG-1" "GGCGGAGTAATATTAG-1"
#> [394] "GGCGGTAGGATCATTG-1" "GGCGTCCTATCCGCTG-1" "GGCTATTAAGTTGTAT-1"
#> [397] "GGCTCGTGCCACCAGC-1" "GGCTCTGCTCCAACGC-1" "GGCTGGCTAGCTTAAA-1"
#> [400] "GGGACTGCATAGATAG-1" "GGGAGAACTCACAGTA-1" "GGGAGTTAATGAGGCG-1"
#> [403] "GGGCGATATGTGTGAA-1" "GGGCGGTCCTATTGTC-1" "GGGCGTACATTTATAT-1"
#> [406] "GGGCTGCCTAGGGCGA-1" "GGGCTGGTTAGTCGCG-1" "GGGTATGTATGCACTT-1"
#> [409] "GGGTCACCGTGACGGT-1" "GGGTGTTTCAGCTATG-1" "GGTAACTATGTATCTG-1"
#> [412] "GGTAGTGCTCGCACCA-1" "GGTATTGCCGAGTTTA-1" "GGTCGGTAATTAGACA-1"
#> [415] "GGTGAGATGCAGATAA-1" "GGTGCGGATAAGTGGC-1" "GGTTAGGCTTGGAGAA-1"
#> [418] "GGTTTACAATCTCAAT-1" "GGTTTGAGTGCTGGAA-1" "GTAAGCGGGCAGTCAG-1"
#> [421] "GTAAGTAGGGTATACC-1" "GTACGAGATTGCGACA-1" "GTACTAAGATTTGGAG-1"
#> [424] "GTACTGAGGTCGTAAC-1" "GTACTGCATGAAGCGT-1" "GTACTTGGGCACTTCT-1"
#> [427] "GTAGACGTCGTTACAT-1" "GTATAGGACTCAGTAG-1" "GTATCAGCTTGGGTTC-1"
#> [430] "GTCAAAGTTTACATAG-1" "GTCACTCTCCAAATCT-1" "GTCAGAATAGTCTATG-1"
#> [433] "GTCATGCACCTCCGTT-1" "GTCATGGACATGACTA-1" "GTCCCAACGTAAAGTA-1"
#> [436] "GTCCGGCTGAATTGCG-1" "GTCGGATATCTCAGAC-1" "GTCGTCAATTATAAGG-1"
#> [439] "GTGAAACGTGCTCCAC-1" "GTGCAACAAATGTGGC-1" "GTGCACGAAAGTGACT-1"
#> [442] "GTGCGGGTCTCCAAAT-1" "GTGCTCAAGTACTGTC-1" "GTGGACGTGCTGAGAC-1"
#> [445] "GTGGGCTTAGACACAC-1" "GTTCACAGGAGTCTAG-1" "GTTCCAGTCTGACCAT-1"
#> [448] "GTTCTTCCCTCGATGT-1" "GTTGCACGGAGTTTCG-1" "GTTGTCGTGTTAGTTG-1"
#> [451] "GTTTCCTGGAGGGTGA-1" "GTTTGACCAAATCCTA-1" "GTTTGGCCGCTCAGCG-1"
#> [454] "TAAATGAATCCGTTTC-1" "TAACAAAGGGAGAAGC-1" "TAACGCTTTGAGAGCG-1"
#> [457] "TAACTATCGAAGGTCC-1" "TAACTCCATGGAGGCT-1" "TAAGGAACTTGTGGGA-1"
#> [460] "TAAGGCAACATAAGAT-1" "TAATACACAGTAGTAT-1" "TAATATTGAAATTCGC-1"
#> [463] "TACAAGTCTCGTGCAT-1" "TACCAGCTAGGTTTAA-1" "TACCTCACGCTTGTAC-1"
#> [466] "TACGAACACGACTTCA-1" "TACGAGAACTTCACGT-1" "TACGATGTTGATCATC-1"
#> [469] "TACGTGCACTATGCTG-1" "TAGAGCTACGAAGAAC-1" "TAGAGTCTAAGCGAAC-1"
#> [472] "TAGCTAAGTCCGGGAG-1" "TAGGGTGTTTCAAGAG-1" "TAGGTGAGCCCTACTC-1"
#> [475] "TAGGTTCGAGTTCGTC-1" "TAGTGCCCTCCAGAGT-1" "TAGTTTATTCTTGCTT-1"
#> [478] "TATAAATCCACAAGCT-1" "TATAAGTGAGGATAGC-1" "TATACACGCAAAGTAT-1"
#> [481] "TATCCATATCATGCGA-1" "TATCCTGCATGGGAAT-1" "TATCGATGATTAAACG-1"
#> [484] "TATGCTCCCTACTTAC-1" "TATGGATGTGCTACGC-1" "TATGGCCCGGCCTCGC-1"
#> [487] "TATGGGTACGTATCGT-1" "TATTCAATTCTAATCC-1" "TATTCGTGCCAGAATA-1"
#> [490] "TATTTAGTCTAGATCG-1" "TATTTGTTACCCTTTA-1" "TCAAACAACCGCGTCG-1"
#> [493] "TCAAACTTAGATTGTT-1" "TCAACACATTGGGTAA-1" "TCAACCATGTTCGGGC-1"
#> [496] "TCACAGGTTATTGGGC-1" "TCACGCATTGTAGATC-1" "TCACGGTCATCGCACA-1"
#> [499] "TCAGCAAATGCATCTC-1" "TCAGGGTGTAACGTAA-1" "TCAGGTTCTTTGAGAA-1"
#> [502] "TCAGTACTGACCCGCG-1" "TCATCACTCGAGCTCG-1" "TCATCCCAGAGGGTGG-1"
#> [505] "TCCACAATGGTTTACG-1" "TCCAGGCGAGTACGGT-1" "TCCCAAAGACGAAGGA-1"
#> [508] "TCCCAGCTTTAGTCTG-1" "TCCCAGGCTTAGCTAA-1" "TCCCGCGTACTCCTGG-1"
#> [511] "TCCCGGGTGTGCTGCT-1" "TCCCGTCAGTCCCGCA-1" "TCCCTGGCTCGCTGGA-1"
#> [514] "TCCGATAATTGCCATA-1" "TCCGATGACTGAGCTC-1" "TCCGATGGTGCGACAT-1"
#> [517] "TCCGATTACATTGCCG-1" "TCCTCTACGAGATGGC-1" "TCCTTCAGTGGTCGAA-1"
#> [520] "TCCTTTCTTACGCTTA-1" "TCGAAATTTAGGACCA-1" "TCGCATCCCTAAGTGT-1"
#> [523] "TCGCCGGTCGATCCGT-1" "TCGCGTCCAGAAGGTC-1" "TCGCTCGATATATTCC-1"
#> [526] "TCGCTGGGCGGATTGT-1" "TCGCTGTGCGTAAATC-1" "TCGGAATGCGCTCTGA-1"
#> [529] "TCGGAGTACATGAGTA-1" "TCGTATAGTGCAATTA-1" "TCGTCCGCTGGCGTCT-1"
#> [532] "TCTACCGTCCACAAGC-1" "TCTAGCATCTTCGATG-1" "TCTAGTTATCAGAAGA-1"
#> [535] "TCTATCATGCAGTTAC-1" "TCTATCGGTCGCAACA-1" "TCTATTACGCTGGCGA-1"
#> [538] "TCTCTAATAGCTGGTA-1" "TCTGAACCGGTCGGCT-1" "TCTTACAGAGGTACCG-1"
#> [541] "TCTTACGGCATCCGAC-1" "TCTTCGATACCAATAA-1" "TCTTGATGCGTAGCGA-1"
#> [544] "TCTTGGTAACACCAAA-1" "TGAAACTTATGCAAGC-1" "TGACCCACGTTAGACA-1"
#> [547] "TGAGAATGCTTTACCG-1" "TGATCGGTTTGACCCT-1" "TGATCTCCGGCGCCAG-1"
#> [550] "TGATTCGTCTATCACT-1" "TGATTTCCTCCTGACG-1" "TGCAAGAATGACGTAA-1"
#> [553] "TGCAGGATCGGCAAAG-1" "TGCATGGATCGGATCT-1" "TGCCACCTGGCGAAAC-1"
#> [556] "TGCCTGATCAAACGAT-1" "TGCGCAAAGCATTTGG-1" "TGCGCGATTAACGGAG-1"
#> [559] "TGCGGAGTAAAGGTGC-1" "TGCGGCATAGTTCAAC-1" "TGCGGTGAAATTTCAT-1"
#> [562] "TGCTGGTTGGACAATT-1" "TGGAAGACGAACACCA-1" "TGGAAGGATAAAGATG-1"
#> [565] "TGGAGTGATGCGATGA-1" "TGGCCAAACTGAAGTA-1" "TGGCCGTATATTGACC-1"
#> [568] "TGGCTTTGGGTAGACA-1" "TGGGAAATGCCTTTCC-1" "TGGGCACGTTCTATGG-1"
#> [571] "TGGGCCACAAGAGCGC-1" "TGGTTCGTAGCAAAGG-1" "TGTAGTGATCTATAAT-1"
#> [574] "TGTCCCGACATAGCAC-1" "TGTCCTAAGTCACCGC-1" "TGTCTACAGTTTCTGT-1"
#> [577] "TGTGACTACGCCAGTC-1" "TGTGTCGCGAGTTGCA-1" "TGTTCTTCCATTGACT-1"
#> [580] "TGTTTCGGTACTTCTC-1" "TTAACCAACCCTCCCT-1" "TTAAGCGCCTGACCCA-1"
#> [583] "TTAATCAGTACGTCAG-1" "TTAATGTAGACCAGGT-1" "TTAATTTCAGACGCGG-1"
#> [586] "TTACAACTACGCATCC-1" "TTACATCGTGGCCTGG-1" "TTACCATTGATTACCC-1"
#> [589] "TTAGCAACATGGATGT-1" "TTATATTTGGCAATCC-1" "TTATCCAATCGAACTC-1"
#> [592] "TTATCTGTATCATAAC-1" "TTATGACAAACTGGAT-1" "TTATTAGGGAAGCATC-1"
#> [595] "TTCAAAGTCTCTAGCC-1" "TTCAACGACCCGACCG-1" "TTCAAGCCGAGCTGAG-1"
#> [598] "TTCATGGCGCAACAGG-1" "TTCCTCGAGGGTGTCT-1" "TTCGACGGGAAGGGCG-1"
#> [601] "TTCGCACTCGCGTGCT-1" "TTCGTACTCCAGAACG-1" "TTCTACTTGCGAGGGC-1"
#> [604] "TTCTAGGCCAATTGTG-1" "TTCTTAGTGAACGGTG-1" "TTCTTAGTGGCTCAGA-1"
#> [607] "TTCTTGTAACCTAATG-1" "TTGAAGAATTCCCAGG-1" "TTGAATATGGACTTTC-1"
#> [610] "TTGATCTAACTTTGTC-1" "TTGATTATGCAGATGA-1" "TTGCACGGAGCAGCAC-1"
#> [613] "TTGCTCCCATACCGGA-1" "TTGCTGAAGGAACCAC-1" "TTGCTGATCATGTTCG-1"
#> [616] "TTGGATTGGGTACCAC-1" "TTGGCTCGCATGAGAC-1" "TTGGGACACTGCCCGC-1"
#> [619] "TTGGGCGGCGGTTGCC-1" "TTGTAATCCGTACTCG-1" "TTGTATCACACAGAAT-1"
#> [622] "TTGTCGTTCAGTTACC-1" "TTGTGGCCCTGACAGT-1" "TTGTTCAGTGTGCTAC-1"
spatIDs(g, subset = nr_feats <= 200)
#>  [1] "GTCGTCAATTATAAGG-1" "TCGTATAGTGCAATTA-1" "GAGTACGGGTATACAA-1"
#>  [4] "TCTTACAGAGGTACCG-1" "GTTGTCGTGTTAGTTG-1" "GTACTTGGGCACTTCT-1"
#>  [7] "CGCTATTCAATGTATG-1" "AATGGTTCTCACAAGC-1" "CTTCATAGCTCAAGAA-1"
#> [10] "CAACGGTTCTTGATAC-1" "ACACCCGAGAAATCCG-1" "CACTGTCCAAGTGAGA-1"
#> [13] "CATAGTAGCATAGTAG-1" "CAACTCCTTGATCCCG-1" "AAGTAGAAGACCGGGT-1"
#> [16] "TACGTGCACTATGCTG-1" "GTATCAGCTTGGGTTC-1" "TAACAAAGGGAGAAGC-1"
#> [19] "TTACATCGTGGCCTGG-1" "TCTGAACCGGTCGGCT-1" "TATAAGTGAGGATAGC-1"
#> [22] "TGCTGGTTGGACAATT-1" "TATAAATCCACAAGCT-1" "CGCTCGACATAATGAT-1"
#> [25] "TTCAACGACCCGACCG-1" "GTCACTCTCCAAATCT-1" "TTCTACTTGCGAGGGC-1"
#> [28] "CTGTTCATCTCACGGG-1" "TTCTTGTAACCTAATG-1" "GCTTGATGATAATCAG-1"
#> [31] "TTGGCTCGCATGAGAC-1" "GCCCAGTTGGTATGCC-1" "TCGTCCGCTGGCGTCT-1"
#> [34] "TTCTAGGCCAATTGTG-1" "ATGAAGCCAAGGAGCC-1" "CCCGTCAGCGTCTGAC-1"
#> [37] "GACTAAGATCATGCAC-1" "TGGTTCGTAGCAAAGG-1" "CTGTTCACTGCCTGTG-1"
#> [40] "CGTACCTGATAGGCCT-1" "GAATGTTGGGTAATCT-1" "CACACAGGGATAGATT-1"
#> [43] "TAACTATCGAAGGTCC-1" "GGGCGTACATTTATAT-1" "ATCGCACGCCGGGAGA-1"
#> [46] "TATGCTCCCTACTTAC-1" "TCCTCTACGAGATGGC-1" "TTGATTATGCAGATGA-1"
#> [49] "GTAAGTAGGGTATACC-1" "CGCAAACACGAGTTAC-1" "TGGCCGTATATTGACC-1"
#> [52] "ACTGTAGCACTTTGGA-1" "GCTCTCGGGTACCGAA-1" "CACCGCCAGAAGGTTT-1"
#> [55] "TCCCAAAGACGAAGGA-1" "TTATATTTGGCAATCC-1" "AGCACCAGTACTCACG-1"
#> [58] "CGTGTCCCATTCGCGA-1" "GATCCCTTTATACTGC-1" "ATAGGTTGGGCAGATG-1"
#> [61] "CAATTAAGGGTGATGA-1" "TCGCGTCCAGAAGGTC-1" "GGCGTCCTATCCGCTG-1"
#> [64] "TCAGCAAATGCATCTC-1" "GAGATCTGTCACTCCG-1" "CAGCTTAGTAGGTAGC-1"
#> [67] "CGAAGTTGCTCTGTGT-1" "CTCTGGACGCCTGGTG-1" "AGGGTTTAGTTCGGGA-1"
#> [70] "CTCTCACAATCGATGA-1" "TCGGAGTACATGAGTA-1" "TTACCATTGATTACCC-1"
#> [73] "CCTCTCTCCCATCTAG-1" "GCGCTAATTGAATAGA-1" "GGTAGTGCTCGCACCA-1"
#> [76] "AAGCTCGTGCCAAGTC-1"
spatIDs(g, subset = Dim.1 > 25, dim_reduction_to_use = "umap")
#>  [1] "AAATGGTCAATGTGCC-1" "AAATTAACGGGTAGCT-1" "AAGGCTGTGCTCATCG-1"
#>  [4] "ACCACAAGTTTCTATC-1" "ACCTAATCGACTTCCT-1" "ACTTGGGACCCGGTGG-1"
#>  [7] "AGTTCCTACAGAATTA-1" "ATATTTAACCCTCAAG-1" "ATTGAAGATCTTAGTG-1"
#> [10] "CAAACGGTCGCACTTT-1" "CAGCTCACTGAGACAT-1" "CCAACGATGCACTGAT-1"
#> [13] "CCATTCCCTGCCCACA-1" "CCTCCGACAATTCAAG-1" "CGAAGTTGCTCTGTGT-1"
#> [16] "CGCTCTCCGTAGATTA-1" "CGGGCAGCTAAACCGC-1" "CGTTTCGCTCATTACA-1"
#> [19] "CTTACACGGTATTCCA-1" "CTTTGGCTTTAGTAAA-1" "GAAATGGCGGTGTTAG-1"
#> [22] "GAAGCTTGCTGACCGC-1" "GCAGGACTATAGAATA-1" "GGCACTCCACTGGGCA-1"
#> [25] "GGGCTGGTTAGTCGCG-1" "GTTCACAGGAGTCTAG-1" "TACGAACACGACTTCA-1"
#> [28] "TACGAGAACTTCACGT-1" "TATGGGTACGTATCGT-1" "TCCAGGCGAGTACGGT-1"
#> [31] "TCCCAGCTTTAGTCTG-1" "TCGAAATTTAGGACCA-1" "TTGATCTAACTTTGTC-1"

featIDs(g)
#>   [1] "Gna12"         "Ccnd2"         "Btbd17"        "Sox9"         
#>   [5] "Sez6"          "Serpinf1"      "S100a6"        "Col1a1"       
#>   [9] "Vwf"           "Esam"          "Npas1"         "Tiam1"        
#>  [13] "Bcam"          "Hmgn2"         "Rab3b"         "Inmt"         
#>  [17] "Ddr1"          "Homer3"        "Cp"            "Calb2"        
#>  [21] "Man1a"         "Efnb3"         "Hlf"           "Gstm7"        
#>  [25] "Cavin1"        "Sst"           "Hapln2"        "Hspb1"        
#>  [29] "Ndrg1"         "Nid1"          "Kit"           "Pvalb"        
#>  [33] "Tmbim1"        "Crip1"         "Tek"           "Elovl1"       
#>  [37] "Cyba"          "Itih3"         "Cnp"           "Hap1"         
#>  [41] "Clic1"         "Stx1a"         "Arpp19"        "Dio2"         
#>  [45] "Id3"           "Crlf1"         "Ttc9b"         "Carhsp1"      
#>  [49] "Cabp7"         "Fam163b"       "Rarres2"       "Syn2"         
#>  [53] "Ick"           "Prox1"         "Adssl1"        "Vipr2"        
#>  [57] "Gltp"          "Amotl1"        "Bcas1"         "Aldh1a2"      
#>  [61] "Tppp3"         "Sirt2"         "Lpl"           "Cers2"        
#>  [65] "Qdpr"          "Stk32c"        "Lbp"           "Hsd11b1"      
#>  [69] "Vtn"           "Stac2"         "Plxdc1"        "Igfbp4"       
#>  [73] "Dbndd2"        "Pltp"          "Slc12a4"       "Cadps2"       
#>  [77] "Erbb3"         "Pmp22"         "Kcnab3"        "Myh11"        
#>  [81] "Plod1"         "Etnppl"        "Arhgef25"      "Rcn3"         
#>  [85] "Vip"           "Utrn"          "Grm1"          "Reep3"        
#>  [89] "Fabp7"         "Dcn"           "Slc17a8"       "Dusp6"        
#>  [93] "Kitl"          "Ccn2"          "Timp3"         "Ascl1"        
#>  [97] "Unc5b"         "Vsir"          "Gamt"          "Ptprb"        
#> [101] "Csrp2"         "Adarb1"        "Rnf130"        "Nefh"         
#> [105] "Aebp1"         "Sept4"         "Gria1"         "Sypl"         
#> [109] "Arsg"          "Cacng4"        "Rab37"         "Itgb4"        
#> [113] "Aspa"          "Cygb"          "Rflnb"         "Doc2b"        
#> [117] "Gfap"          "Npas3"         "Serpina3n"     "Clmn"         
#> [121] "Prkch"         "Fbln5"         "Akr1c18"       "Gng4"         
#> [125] "Sema4d"        "Cxcl14"        "Hexb"          "Iqgap2"       
#> [129] "Crhbp"         "Rab3c"         "Thbs4"         "Serinc5"      
#> [133] "Erbin"         "Prkcd"         "Pdlim2"        "Ednrb"        
#> [137] "Cpne6"         "Sema5a"        "Matn2"         "Nptxr"        
#> [141] "Enpp2"         "Efcab6"        "Ppp1r1a"       "Litaf"        
#> [145] "Il1rap"        "Apod"          "Cpox"          "Grik1"        
#> [149] "Clic6"         "Nr4a1"         "Igfbp6"        "Sncg"         
#> [153] "Gzma"          "Grm2"          "Prph"          "Rsph1"        
#> [157] "Sox8"          "Fam234a"       "Lims2"         "Aqp4"         
#> [161] "Slc12a2"       "Cd74"          "Pdgfrb"        "Csf1r"        
#> [165] "Cbln2"         "Frmd8"         "Gldc"          "Tcf7l2"       
#> [169] "Bhlhe22"       "Kcnip2"        "Alas2"         "Gdf11"        
#> [173] "Baiap2"        "Ifitm3"        "Phkg1"         "Nptx1"        
#> [177] "Pde8a"         "Itih5"         "Homer2"        "Fam20c"       
#> [181] "Cplx2"         "Myo5b"         "Snca"          "Col3a1"       
#> [185] "Ecrg4"         "Map4k4"        "2010300C02Rik" "Ogfrl1"       
#> [189] "Fn1"           "Efhd1"         "Ngef"          "Bok"          
#> [193] "Tmem163"       "Lct"           "Cfh"           "Ptpn4"        
#> [197] "Tnni1"         "Csrp1"         "Nfasc"         "Syt2"         
#> [201] "Atp2b4"        "Rgs16"         "Ncf2"          "Cnih3"        
#> [205] "Tmem63a"       "Cfap126"       "Meig1"         "Ccdc3"        
#> [209] "Rgs5"          "Enkur"         "Myoc"          "Vim"          
#> [213] "Prkcq"         "Gad2"          "Lcn2"          "Nr4a2"        
#> [217] "Ermn"          "Fibcd1"        "Gsn"           "Grb14"        
#> [221] "Notch1"        "Ube2l6"        "Gatm"          "Cd82"         
#> [225] "Mdk"           "Lamp5"         "Itpka"         "Chgb"         
#> [229] "Mal"           "Pdyn"          "Nkx2-2"        "Edn3"         
#> [233] "Car2"          "Col9a3"        "Chrna4"        "Gss"          
#> [237] "Slc7a11"       "Tm4sf1"        "Kcnab1"        "Shox2"        
#> [241] "Serpini1"      "Tspan2"        "S100a11"       "Vcam1"        
#> [245] "Lef1"          "Npy2r"         "F3"            "Calb1"        
#> [249] "Epha7"         "Slc44a1"       "1110017D15Rik" "Tpm2"         
#> [253] "Slc6a9"        "Laptm5"        "Sema3c"        "Hpca"         
#> [257] "Padi2"         "Slc4a2"        "Gabrd"         "Rgs12"        
#> [261] "Slc30a3"       "Emilin1"       "1700001C02Rik" "Gabra4"       
#> [265] "Pdgfra"        "Epha5"         "Tesc"          "Pitpnm2"      
#> [269] "Mmp17"         "Cit"           "Foxp2"         "Arpc1b"       
#> [273] "Col1a2"        "Eln"           "Pcolce"        "Tsc22d4"      
#> [277] "Cald1"         "Ccdc136"       "Npy"           "Slc13a4"      
#> [281] "Gkn3"          "Slc6a13"       "Mgp"           "Lmo3"         
#> [285] "Slco1c1"       "Cpne9"         "Itpr2"         "Slc6a11"      
#> [289] "Vamp1"         "Cd9"           "Pglyrp1"       "Slc17a6"      
#> [293] "Sipa1l3"       "Sh3gl3"        "Plekhb1"       "Nupr1"        
#> [297] "Dkk3"          "Cox6a2"        "Dkkl1"         "Fgfr2"        
#> [301] "Crym"          "Slc38a5"       "Flna"          "Bgn"          
#> [305] "Col4a1"        "Plat"          "Nr3c2"         "Cbln1"        
#> [309] "Gab1"          "Pllp"          "Necab2"        "Dbndd1"       
#> [313] "Agt"           "Cryab"         "Tagln"         "Mcam"         
#> [317] "Olfm2"         "Icam5"         "Anxa2"         "Rora"         
#> [321] "Calml4"        "Htr3a"         "Paqr5"         "Rasgrf1"      
#> [325] "Zic1"          "Mobp"          "Hhatl"         "Trf"          
#> [329] "Cpne4"         "Cdhr4"         "Cd59a"         "Gng11"        
#> [333] "Chrm1"         "Prr5l"         "Ugt8a"         "Rims3"        
#> [337] "Sgpp2"         "Camkv"         "Sox10"         "Micall1"      
#> [341] "Tie1"          "Fa2h"          "Ucp2"          "Vav3"         
#> [345] "Ccp110"        "Gria2"         "Rasl10a"       "Foxj1"        
#> [349] "Trim59"        "Dynlrb2"       "Gjc1"          "Neurod1"      
#> [353] "Ttyh2"         "Cpne7"         "Nrip3"         "Tbr1"         
#> [357] "Lcat"          "Tmem98"        "Myo1d"         "Prdm8"        
#> [361] "Tgfbi"         "Kcnc2"         "Thrsp"         "Isg15"        
#> [365] "Acta2"         "H2-Q4"         "Dock4"         "Myrf"         
#> [369] "Lmo1"          "Igfbp7"        "P2ry12"        "Pcdh8"        
#> [373] "Lgi4"          "Fxyd1"         "Fxyd7"         "Mag"          
#> [377] "Zcchc12"       "Anln"          "Wnt4"          "C1ql2"        
#> [381] "Unc93b1"       "Tspan15"       "Ppp1r14a"      "Clic4"        
#> [385] "Galnt6"        "Ccn3"          "Rims2"         "Zmat4"        
#> [389] "Cldn11"        "Ccdc33"        "Slc32a1"       "Tgm2"         
#> [393] "Vstm2l"        "Neurod6"       "Crygn"         "Enpp6"        
#> [397] "Neurod2"       "Satb2"         "Pcp4l1"        "Txnip"        
#> [401] "Egr1"          "Otud7b"        "Car14"         "Rgs4"         
#> [405] "Dock10"        "Lpar1"         "Trps1"         "Ptpn3"        
#> [409] "Lefty1"        "Zfhx3"         "St6galnac5"    "Ak5"          
#> [413] "Hrh3"          "Nexn"          "Adgrl4"        "Rlbp1"        
#> [417] "Igfbp2"        "Wfs1"          "Cplx3"         "Orai2"        
#> [421] "Olig2"         "Gpr37"         "Plcb4"         "Nxph4"        
#> [425] "Ndufa4l2"      "Cacng5"        "Plekhg1"       "Cmtm5"        
#> [429] "Gpr161"        "Rfx3"          "Igsf21"        "Ramp3"        
#> [433] "Arhgap12"      "Nkx6-2"        "Ninj2"         "Cldn5"        
#> [437] "Htr2c"         "Hspb8"         "Fmod"          "Prelp"        
#> [441] "Mpped1"        "Rapgef5"       "Vwa1"          "Stab1"        
#> [445] "Arl15"         "Reln"          "Mustn1"        "Pbxip1"       
#> [449] "Dnali1"        "Id1"           "Lrrtm3"        "Tmem212"      
#> [453] "Epop"          "Gjc2"          "1190005I06Rik" "Ctla2a"       
#> [457] "Cnr1"          "Gpr4"          "Sowaha"        "Tent5c"       
#> [461] "Serpinb1a"     "S1pr5"         "Penk"          "Cdc42ep2"     
#> [465] "Slitrk6"       "Hpcal4"        "Olig1"         "Rprml"        
#> [469] "Stxbp6"        "Hs3st2"        "Rbp1"          "Ppm1e"        
#> [473] "Sox18"         "Kctd4"         "Nxph3"         "Vat1l"        
#> [477] "Cd24a"         "Odf3b"         "Gjb1"          "Selplg"       
#> [481] "Bcl11b"        "Lhfp"          "Ranbp3l"       "Msx1"         
#> [485] "Zdhhc22"       "Phldb1"        "Ctxn1"         "Tprn"         
#> [489] "Vstm2a"        "Ndnf"          "Pdp1"          "Scn3b"        
#> [493] "Cdc42ep1"      "Gal3st1"       "Rasd1"         "Spink8"       
#> [497] "Opalin"        "Gja4"          "Synpo2"        "Pcdh20"       
#> [501] "Adra1b"        "Fam131a"       "Tmem125"       "Mb21d2"       
#> [505] "Lingo3"        "Ncald"         "Siglech"       "Rgs14"        
#> [509] "Gm14964"       "Gpr17"         "Cx3cr1"        "Ezr"          
#> [513] "Adarb2"        "Cpne8"         "Plekhg3"       "Lamb2"        
#> [517] "Creb5"         "Cntn2"         "Aldh1a1"       "Phgdh"        
#> [521] "Tnfaip6"       "Camk2d"        "Shisa6"        "Cthrc1"       
#> [525] "Tmem119"       "Hmgb2"         "Tmem158"       "Dsp"          
#> [529] "Lrtm2"         "Gabra5"        "Tns1"          "Nell1"        
#> [533] "Slain1"        "Prr18"         "S100a8"        "S100a9"       
#> [537] "Col8a2"        "Synpr"         "Sertm1"        "Adgrf5"       
#> [541] "Capg"          "Gjc3"          "Fcer1g"        "Pde1a"        
#> [545] "Tafa1"         "Ddn"           "Hopx"          "Ptk2b"        
#> [549] "Rgs3"          "Kcng2"         "Ntng1"         "H2-Q7"        
#> [553] "Ifitm2"        "Plaat3"        "Plekhh1"       "Myl4"         
#> [557] "H2-K1"         "Dpp6"          "Tmem91"        "Cyp2d22"      
#> [561] "Ttr"           "Suclg2"        "Patj"          "Btbd3"        
#> [565] "Unc13c"        "Syt9"          "Shisal1"       "Grm4"         
#> [569] "Sox11"         "Slc22a8"       "Slc24a3"       "Klk8"         
#> [573] "Ipcef1"        "Tnnt1"         "Hhip"          "Spag8"        
#> [577] "Fxyd6"         "Ppp1r3c"       "Cbln4"         "Nnat"         
#> [581] "Myl9"          "Vxn"           "Lgals1"        "Gpr88"        
#> [585] "Ctxn3"         "Lyz2"          "Tmem100"       "Wfdc17"       
#> [589] "Ahnak"         "Hba-a2"        "Hba-a1"        "Ccdc153"      
#> [593] "Ccnd1"         "Serpinh1"      "Gad1"          "Arhgef10"     
#> [597] "2410004P03Rik" "Nt5dc2"        "Cebpd"         "Sod3"         
#> [601] "H2-D1"         "C4b"           "Tmem88b"       "Hbb-bt"       
#> [605] "Rhog"          "Rprm"          "Ly6a"          "Mog"          
#> [609] "Trbc2"         "Igkc"          "Smim1"         "Hs3st4"       
#> [613] "Evi2a"         "Prkcg"         "Ly6c1"         "Gbp4"         
#> [617] "Wipf3"         "BC039966"      "Gm13889"       "Plcxd2"       
#> [621] "Mia"           "Pou3f1"        "Lrrc10b"       "Apol11b"      
#> [625] "Gpr62"         "Shisa8"        "Gm2115"        "Kctd12"       
#> [629] "1700047M11Rik" "Lhfpl3"        "C030029H02Rik" "Gm19935"      
#> [633] "9630013A20Rik" "2900040C04Rik"
featIDs(g, subset = nr_cells < 100)
#>  [1] "Col1a1"        "Inmt"          "Cp"            "Nid1"         
#>  [5] "Vipr2"         "Aldh1a2"       "Lbp"           "Myh11"        
#>  [9] "Dcn"           "Slc17a8"       "Ascl1"         "Aebp1"        
#> [13] "Fbln5"         "Akr1c18"       "Efcab6"        "Clic6"        
#> [17] "Sncg"          "Gzma"          "Prph"          "Rsph1"        
#> [21] "Cd74"          "Col3a1"        "Ncf2"          "Cfap126"      
#> [25] "Meig1"         "Enkur"         "Myoc"          "Lcn2"         
#> [29] "Edn3"          "1110017D15Rik" "Tpm2"          "Emilin1"      
#> [33] "1700001C02Rik" "Col1a2"        "Eln"           "Pcolce"       
#> [37] "Slc13a4"       "Gkn3"          "Slc6a13"       "Nupr1"        
#> [41] "Slc38a5"       "Tagln"         "Calml4"        "Htr3a"        
#> [45] "Paqr5"         "Cdhr4"         "Tie1"          "Foxj1"        
#> [49] "Dynlrb2"       "Gjc1"          "Tgfbi"         "H2-Q4"        
#> [53] "Ccdc33"        "Tgm2"          "Crygn"         "Nexn"         
#> [57] "Nxph4"         "Fmod"          "Stab1"         "Mustn1"       
#> [61] "Dnali1"        "Tmem212"       "Gpr4"          "Tent5c"       
#> [65] "Slitrk6"       "Odf3b"         "Ranbp3l"       "Msx1"         
#> [69] "Ndnf"          "Gja4"          "Gm14964"       "Dsp"          
#> [73] "S100a8"        "S100a9"        "Col8a2"        "Capg"         
#> [77] "H2-Q7"         "Hhip"          "Spag8"         "Ctxn3"        
#> [81] "Wfdc17"        "Ccdc153"       "2410004P03Rik" "Igkc"         
#> [85] "Gbp4"          "BC039966"      "Mia"           "Apol11b"      
#> [89] "Shisa8"        "C030029H02Rik" "Gm19935"       "9630013A20Rik"
#> [93] "2900040C04Rik"

gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
featIDs(gpoints)
#>   [1] "Mlc1"      "Gprc5b"    "Gfap"      "Ednrb"     "Sox9"      "Aqp4"     
#>   [7] "Gjc3"      "Sox8"      "Ntsr2"     "Adcyap1r1" "Smo"       "Gpr161"   
#>  [13] "C1qb"      "Olig1"     "C1qa"      "Lmtk2"     "Pth1r"     "Ephb6"    
#>  [19] "Gpr26"     "Gabbr1"    "Adra2a"    "Gabbr2"    "Timp4"     "Abcc9"    
#>  [25] "Csf1r"     "Ntrk2"     "Adgrg1"    "Ntrk3"     "Ddr1"      "Erbb4"    
#>  [31] "Selplg"    "Cspg5"     "Dlk1"      "Gpr37l1"   "Fzd1"      "Fgfr3"    
#>  [37] "Slc25a18"  "Igf1r"     "Adgrl1"    "S1pr1"     "Ryk"       "Grm5"     
#>  [43] "Eomes"     "Axl"       "Efemp1"    "Adgrb3"    "Sstr4"     "Fn1"      
#>  [49] "Adora1"    "Erbb2"     "Gper1"     "Gramd3"    "Mertk"     "Slc32a1"  
#>  [55] "Adgra1"    "Adgrl2"    "Ptgfr"     "Fzd2"      "Aldh1l1"   "Adgrg6"   
#>  [61] "Egfr"      "Gpr162"    "Lpar4"     "Kit"       "Ptgdr"     "Gad1"     
#>  [67] "Lgr4"      "Tyro3"     "Atp13a5"   "Ddr2"      "Rspo3"     "Arhgap29" 
#>  [73] "Ackr3"     "Gpr146"    "Fzd7"      "Slc47a1"   "Insr"      "Ptger3"   
#>  [79] "Bdkrb1"    "Traf4"     "Celsr2"    "Fzd4"      "Fgfr2"     "Cx3cr1"   
#>  [85] "Anxa11"    "Adgrb1"    "Lpar1"     "Gpr17"     "Gpr173"    "Ror1"     
#>  [91] "Cxcl12"    "Kiss1r"    "Epha4"     "Gpr182"    "Man1a"     "Gpr4"     
#>  [97] "P2ry14"    "Hcar1"     "Ephb1"     "Adrb1"     "Grm3"      "P2ry1"    
#> [103] "Nrp2"      "Ptk7"      "Slco1a4"   "Tmem108"   "Hrh3"      "Insrr"    
#> [109] "Cmklr1"    "Opn3"      "Adra1b"    "Gpr62"     "Npy1r"     "Nrp1"     
#> [115] "Fzd6"      "S1pr5"     "Pdgfrb"    "Flt1"      "Chrm3"     "Agtr1a"   
#> [121] "Adgrl4"    "Ephb3"     "Lpar6"     "Epha6"     "Fzd5"      "Emcn"     
#> [127] "Cldn5"     "Sstr1"     "Fzd3"      "P2ry12"    "Adgrl3"    "Sema4d"   
#> [133] "Adgrf5"    "Gpr160"    "Mrgpre"    "Amigo2"    "Adgra3"    "P2yr13"   
#> [139] "Epha3"     "Adgre1"    "P2ry6"     "Cx3cl1"    "Epha10"    "Tacr1"    
#> [145] "Peg10"     "Ednra"     "Plxnb3"    "Flt4"      "Slc17a7"   "Drd2"     
#> [151] "Tjap1"     "Fzd8"      "Cd300c2"   "Htr4"      "Gpr52"     "Tek"      
#> [157] "S1pr2"     "Kdr"       "Ror2"      "Kcnj8"     "Hrh1"      "Celsr1"   
#> [163] "Gpr153"    "S1pr3"     "Adgra2"    "Fgfrl1"    "Tie1"      "Slc15a3"  
#> [169] "Ccr10"     "Pdgfra"    "Lhcgr"     "Mrgprf"    "Ascl1"     "Baiap2"   
#> [175] "Gpr85"     "Gpr22"     "Gcgr"      "Grm1"      "Ptafr"     "Calcrl"   
#> [181] "Gpr34"     "Cenpe"     "Adora2b"   "Adrb2"     "Ccr2"      "Alk"      
#> [187] "Tacr3"     "Chrm5"     "Erbb3"     "Ccr9"      "Gpr21"     "Epha7"    
#> [193] "Adra2b"    "Pcdh15"    "Blank-104" "Adora2a"   "Drd1"      "Blank-57" 
#> [199] "Grm7"      "Htr1b"     "Fzd9"      "Grm4"      "Blank-152" "Aplnr"    
#> [205] "Gpr176"    "F2r"       "Vmn1r43"   "Gpr156"    "Gpr55"     "Blank-155"
#> [211] "Bdkrb2"    "C5ar2"     "Gpbar1"    "Adgrv1"    "Gprc5c"    "Lmod1"    
#> [217] "Gpr135"    "Rxfp3"     "Tbxa2r"    "Fzd10"     "Ret"       "Blank-80" 
#> [223] "Epha8"     "Gpr75"     "Ackr1"     "Adgrg4"    "Chrm1"     "Epha1"    
#> [229] "Vmn2r1"    "Syt4"      "Tas2r135"  "Ccr1"      "Lpar2"     "Myh11"    
#> [235] "Gipr"      "Musk"      "Gpr183"    "Cnr1"      "C3ar1"     "S1pr4"    
#> [241] "Grm2"      "C5ar1"     "Ephb2"     "Fpr1"      "Rho"       "Gpr27"    
#> [247] "Adra1d"    "Gpr157"    "Adgrg2"    "Blank-45"  "Gpr20"     "Casr"     
#> [253] "Avpr2"     "Mc5r"      "Sctr"      "Epha5"     "Gpr158"    "Cxcr4"    
#> [259] "Vipr1"     "Gpr6"      "Blank-148" "Celsr3"    "Uts2r"     "Hrh2"     
#> [265] "Htr6"      "Slc17a6"   "Gpr1"      "Glp2r"     "Ppp1r3g"   "V1ra8"    
#> [271] "Trhr"      "Sstr3"     "Gpr63"     "Htr1a"     "F2rl3"     "Pln"      
#> [277] "Grin2b"    "Gpr37"     "Chrm4"     "Lgr6"      "Gpr61"     "Taar7e"   
#> [283] "Mas1"      "Npy2r"     "Ptger2"    "P2ry2"     "Oprm1"     "Prokr2"   
#> [289] "Adra1a"    "Gpr45"     "Mc4r"      "Blank-128" "Sstr2"     "Flt3"     
#> [295] "Htr5a"     "Ghsr"      "Galr1"     "Blank-95"  "Gpr83"     "Crhr1"    
#> [301] "Brs3"      "Drd5"      "Gpr139"    "Taar6"     "Htr5b"     "Vmn1r40"  
#> [307] "Rrh"       "Blank-83"  "Crhr2"     "Blank-161" "Vmn1r50"   "Tas1r3"   
#> [313] "Taar2"     "P2ry4"     "Mchr1"     "Tas1r1"    "Blank-69"  "Htr7"     
#> [319] "Hcrtr2"    "Htr2a"     "Cysltr2"   "Vmn1r46"   "Grpr"      "Adgrf2"   
#> [325] "Blank-127" "Opn5"      "Taar3"     "Oxtr"      "Cckbr"     "Nmbr"     
#> [331] "Ptger4"    "Blank-145" "Gpr101"    "Slc17a8"   "Adgrf4"    "Epha2"    
#> [337] "Blank-139"

# ID replacements (currently only giottoPolygons)
polys <- g[["spatial_info"]][[1]]
slot(polys, "overlaps") <- NULL # make NULL to avoid a warning
head(spatIDs(polys))
#> [1] "AAAGGGATGTAGCAAG-1" "AAATGGCATGTCTTGT-1" "AAATGGTCAATGTGCC-1"
#> [4] "AAATTAACGGGTAGCT-1" "AACAACTGGTAGTTGC-1" "AACAGGAAATCGAATA-1"
spatIDs(polys) <- paste0("poly_", seq_len(nrow(polys)))
head(spatIDs(polys))
#> [1] "poly_1" "poly_2" "poly_3" "poly_4" "poly_5" "poly_6"
spatIDs(polys, old = c("poly_1", "poly_3")) <- c("test1", "test2")
head(spatIDs(polys))
#> [1] "test1"  "poly_2" "test2"  "poly_4" "poly_5" "poly_6"
```
