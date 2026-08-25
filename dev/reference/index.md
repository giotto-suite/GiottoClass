# Package index

## Giotto

Create a Giotto Object

- [`createGiottoObject()`](https://giotto-suite.github.io/GiottoClass/dev/reference/create_giotto.md)
  [`createGiottoObjectSubcellular()`](https://giotto-suite.github.io/GiottoClass/dev/reference/create_giotto.md)
  : Create a giotto object

## Updaters

Objects version compatibility v3.0 and up

- [`updateGiottoObject()`](https://giotto-suite.github.io/GiottoClass/dev/reference/updateGiottoObject.md)
  : Update giotto object
- [`updateGiottoPointsObject()`](https://giotto-suite.github.io/GiottoClass/dev/reference/updateGiottoPointsObject.md)
  : Update giotto points object
- [`updateGiottoPolygonObject()`](https://giotto-suite.github.io/GiottoClass/dev/reference/updateGiottoPolygonObject.md)
  : Update giotto polygon object

## Ingestion

### Read

Read data in and create Giotto-native objects

- [`readCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/readCellMetadata.md)
  : Read cell metadata
- [`readFeatMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/readFeatMetadata.md)
  : Read feature metadata
- [`readPolygonData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/readPolygonData.md)
  : Read list of polygons information
- [`readFeatData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/readFeatData.md)
  : Read feature information
- [`readExprData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/readExprData.md)
  : Read expression data
- [`readExprMatrix()`](https://giotto-suite.github.io/GiottoClass/dev/reference/readExprMatrix.md)
  : Read expression matrix
- [`readSpatLocsData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/readSpatLocsData.md)
  : Read spatial location data
- [`readSpatNetData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/readSpatNetData.md)
  : Read spatial networks
- [`readSpatEnrichData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/readSpatEnrichData.md)
  : Read spatial enrichment
- [`readDimReducData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/readDimReducData.md)
  : Read dimensional reduction data
- [`readNearestNetData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/readNearestNetData.md)
  : Read nearest neighbor network data

### evaluate

Evaluate and format data for usage with Giotto

- [`evaluate_input()`](https://giotto-suite.github.io/GiottoClass/dev/reference/evaluate_input.md)
  : Evaluate raw inputs to Giotto formatting

### Create

Create Giotto-native objects

- [`createCellMetaObj()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createCellMetaObj.md)
  : Create S4 cellMetaObj
- [`createDimObj()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createDimObj.md)
  : Create S4 dimObj
- [`createExprObj()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createExprObj.md)
  : Create S4 exprObj
- [`createFeatMetaObj()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createFeatMetaObj.md)
  : Create S4 featMetaObj
- [`createGiottoPoints(`*`<SpatVector>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPoints.md)
  [`createGiottoPoints(`*`<data.frame>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPoints.md)
  : Create giotto points object
- [`createGiottoPolygon(`*`<character>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  [`createGiottoPolygon(`*`<SpatVector>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  [`createGiottoPolygon(`*`<SpatRaster>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  [`createGiottoPolygon(`*`<data.frame>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  [`createGiottoPolygonsFromMask()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  [`createGiottoPolygonsFromDfr()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  [`createGiottoPolygonsFromGeoJSON()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  : Create giotto polygons object
- [`createNearestNetObj()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createNearestNetObj.md)
  : Create S4 nnNetObj
- [`createSpatEnrObj()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createSpatEnrObj.md)
  : Create S4 spatEnrObj
- [`createSpatLocsObj()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createSpatLocsObj.md)
  : Create S4 spatLocsObj
- [`createSpatNetObj()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createSpatNetObj.md)
  : Create S4 spatialNetworkObj
- [`createGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoImage.md)
  : createGiottoImage
- [`createGiottoLargeImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoLargeImage.md)
  : createGiottoLargeImage
- [`createGiottoLargeImageList()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoLargeImageList.md)
  : createGiottoLargeImageList

## Access

### Getters

Get data from a Giotto Object

- [`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getCellMetadata.md)
  : getCellMetadata
- [`getFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getFeatureMetadata.md)
  : getFeatureMetadata
- [`getPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getPolygonInfo.md)
  : Get polygon info
- [`getFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getFeatureInfo.md)
  : Get feature info
- [`getExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getExpression.md)
  : Get expression values
- [`getSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialLocations.md)
  : Get spatial locations
- [`getSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialNetwork.md)
  : Get spatial network
- [`getSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialEnrichment.md)
  : Get spatial enrichment
- [`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getDimReduction.md)
  : Get dimension reduction
- [`getMultiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getMultiomics.md)
  : Get multiomics integration results
- [`getNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getNearestNetwork.md)
  : Get nearest neighbor network
- [`getGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getGiottoImage.md)
  : Get giotto image object
- [`spatValues()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatValues.md)
  [`svkey()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatValues.md)
  : Giotto object spatial values

### Setters

Set (and replace) Giotto-native data objects in a Giotto Object

- [`setGiotto(`*`<giotto>`*`,`*`<giottoBinPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md)
  [`setGiotto(`*`<giotto>`*`,`*`<list>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md)
  [`setGiotto(`*`<giotto>`*`,`*`<cellMetaObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md)
  [`setGiotto(`*`<giotto>`*`,`*`<featMetaObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md)
  [`setGiotto(`*`<giotto>`*`,`*`<exprObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md)
  [`setGiotto(`*`<giotto>`*`,`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md)
  [`setGiotto(`*`<giotto>`*`,`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md)
  [`setGiotto(`*`<giotto>`*`,`*`<dimObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md)
  [`setGiotto(`*`<giotto>`*`,`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md)
  [`setGiotto(`*`<giotto>`*`,`*`<spatEnrObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md)
  [`setGiotto(`*`<giotto>`*`,`*`<nnNetObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md)
  [`setGiotto(`*`<giotto>`*`,`*`<spatialNetworkObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md)
  [`setGiotto(`*`<giotto>`*`,`*`<giottoLargeImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md)
  [`setGiotto(`*`<giotto>`*`,`*`<giottoImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md)
  : Set giotto subobjects into giotto object
- [`setCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setCellMetadata.md)
  : Set cell metadata
- [`setFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setFeatureMetadata.md)
  : Set feature metadata
- [`setPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setPolygonInfo.md)
  : Set polygon info
- [`setFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setFeatureInfo.md)
  : Set feature info
- [`setExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setExpression.md)
  : Set expression data
- [`setSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialLocations.md)
  : Set spatial locations
- [`setSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialNetwork.md)
  : Set spatial network
- [`setSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialEnrichment.md)
  : Set spatial enrichment
- [`setDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setDimReduction.md)
  : Set dimension reduction data
- [`setMultiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setMultiomics.md)
  : Set multiomics integration results
- [`setNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setNearestNetwork.md)
  : Set nearest neighbor network
- [`setGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiottoImage.md)
  : Set giotto image object

### Append

Add information to the Giotto Object

- [`addCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/addCellMetadata.md)
  : Add cell metadata
- [`addFeatMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/addFeatMetadata.md)
  : Add feature metadata
- [`addGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/addGiottoImage.md)
  : addGiottoImage
- [`addGiottoImageMG()`](https://giotto-suite.github.io/GiottoClass/dev/reference/addGiottoImageMG.md)
  : addGiottoImageMG
- [`addGiottoLargeImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/addGiottoLargeImage.md)
  : addGiottoLargeImage

## Gobject Contents

### Print

Print Giotto object slot contents

- [`showGiottoCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoCellMetadata.md)
  : showGiottoCellMetadata
- [`showGiottoFeatMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoFeatMetadata.md)
  : showGiottoFeatMetadata
- [`showGiottoSpatialInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatialInfo.md)
  : showGiottoSpatialInfo
- [`showGiottoFeatInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoFeatInfo.md)
  : showGiottoFeatInfo
- [`showGiottoExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoExpression.md)
  : showGiottoExpression
- [`showGiottoSpatLocs()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatLocs.md)
  : showGiottoSpatLocs
- [`showGiottoSpatNetworks()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatNetworks.md)
  : showGiottoSpatNetworks
- [`showGiottoSpatEnrichments()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatEnrichments.md)
  : showGiottoSpatEnrichments
- [`showGiottoDimRed()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoDimRed.md)
  : showGiottoDimRed
- [`showGiottoNearestNetworks()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoNearestNetworks.md)
  : showGiottoNearestNetworks
- [`showGiottoImageNames()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoImageNames.md)
  : showGiottoImageNames

### Return available data

Return slot contents as data.table with nested locations

- [`list_cell_metadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_cell_metadata.md)
  : list_cell_metadata
- [`list_dim_reductions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_dim_reductions.md)
  : list_dim_reductions
- [`list_expression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_expression.md)
  : list_expression
- [`list_feat_metadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_feat_metadata.md)
  : list_feat_metadata
- [`list_feature_info()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_feature_info.md)
  : list_feature_info
- [`list_images()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_images.md)
  : list_images
- [`list_nearest_networks()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_nearest_networks.md)
  : list_nearest_networks
- [`list_spatial_enrichments()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_spatial_enrichments.md)
  : list_spatial_enrichments
- [`list_spatial_grids()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_spatial_grids.md)
  : list_spatial_grids
- [`list_spatial_info()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_spatial_info.md)
  : list_spatial_info
- [`list_spatial_locations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_spatial_locations.md)
  : list_spatial_locations
- [`list_spatial_networks()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_spatial_networks.md)
  : list_spatial_networks

### Return names of available data

List slot contents as character vector. The spat_unit and/or feat_type
usually must be provided.

- [`list_cell_id_names()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_cell_id_names.md)
  : List cell ID names
- [`list_feat_id_names()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_feat_id_names.md)
  : List feat ID names
- [`list_dim_reductions_names()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_dim_reductions_names.md)
  : list_dim_reductions_names
- [`list_expression_names()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_expression_names.md)
  : list_expression_names
- [`list_feature_info_names()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_feature_info_names.md)
  : list_feature_info_names
- [`list_images_names()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_images_names.md)
  : list_images_names
- [`list_nearest_networks_names()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_nearest_networks_names.md)
  : list_nearest_networks_names
- [`list_spatial_enrichments_names()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_spatial_enrichments_names.md)
  : list_spatial_enrichments_names
- [`list_spatial_grids_names()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_spatial_grids_names.md)
  : list_spatial_grids_names
- [`list_spatial_info_names()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_spatial_info_names.md)
  : list_spatial_info_names
- [`list_spatial_locations_names()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_spatial_locations_names.md)
  : list_spatial_locations_names
- [`list_spatial_networks_names()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_spatial_networks_names.md)
  : list_spatial_networks_names

## Annotate

Giotto object metdata and annotation tools

- [`pDataDT()`](https://giotto-suite.github.io/GiottoClass/dev/reference/pDataDT.md)
  : pDataDT
- [`fDataDT()`](https://giotto-suite.github.io/GiottoClass/dev/reference/fDataDT.md)
  : fDataDT
- [`annotateGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/annotateGiotto.md)
  : Annotate Giotto object
- [`removeCellAnnotation()`](https://giotto-suite.github.io/GiottoClass/dev/reference/removeCellAnnotation.md)
  : Remove cell annotation
- [`removeFeatAnnotation()`](https://giotto-suite.github.io/GiottoClass/dev/reference/removeFeatAnnotation.md)
  : Remove feature annotation
- [`annotateSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/annotateSpatialNetwork.md)
  : annotateSpatialNetwork
- [`calculateMetaTable()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateMetaTable.md)
  : calculateMetaTable
- [`calculateMetaTableCells()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateMetaTableCells.md)
  : calculateMetaTableCells
- [`calculateLabelProportions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateLabelProportions.md)
  : Calculate Proportions of Labels Per Observation Group
- [`calculateSpatCellMetadataProportions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateSpatCellMetadataProportions.md)
  : calculateSpatCellMetadataProportions

## Instructions

Set instructions for Giotto Object behavior

- [`createGiottoInstructions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_instructions.md)
  [`instructions(`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_instructions.md)
  [`instructions(`*`<giotto>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_instructions.md)
  [`instructions(`*`<giotto>`*`,`*`<character>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_instructions.md)
  [`instructions(`*`<giottoInstructions>`*`,`*`<character>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_instructions.md)
  [`` `instructions<-`( ``*`<giotto>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_instructions.md)
  [`` `instructions<-`( ``*`<giotto>`*`,`*`<missing>`*`,`*`<logical>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_instructions.md)
  [`` `instructions<-`( ``*`<giotto>`*`,`*`<character>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_instructions.md)
  [`` `instructions<-`( ``*`<giotto>`*`,`*`<character>`*`,`*`<logical>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_instructions.md)
  [`` `instructions<-`( ``*`<giottoInstructions>`*`,`*`<character>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_instructions.md)
  : Giotto instructions

## Subset

Subsetting the Giotto Object

- [`subsetGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/subsetGiotto.md)
  : subsetGiotto
- [`subsetGiottoLocs()`](https://giotto-suite.github.io/GiottoClass/dev/reference/subsetGiottoLocs.md)
  : Subset by spatial locations
- [`subsetGiottoLocsMulti()`](https://giotto-suite.github.io/GiottoClass/dev/reference/subsetGiottoLocsMulti.md)
  : deprecated
- [`subsetGiottoLocsSubcellular()`](https://giotto-suite.github.io/GiottoClass/dev/reference/subsetGiottoLocsSubcellular.md)
  : Subset raw subcellular information by location
- [`head(`*`<giottoBinPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/headtail.md)
  [`tail(`*`<giottoBinPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/headtail.md)
  : Head and tail

## Join and Split

Joining and Splitting Giotto Objects

- [`joinGiottoObjects()`](https://giotto-suite.github.io/GiottoClass/dev/reference/joinGiottoObjects.md)
  : Join giotto objects

- [`splitGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/splitGiotto.md)
  : Split a Giotto Object

- [`sliceGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/sliceGiotto.md)
  :

  Slice `giotto` object by `spat_unit` and `feat_type`

## Data Processing

Data Processing Framework

- [`processData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/processData.md)
  : Data Processing
- [`processParam-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/processParam-class.md)
  [`processParam`](https://giotto-suite.github.io/GiottoClass/dev/reference/processParam-class.md)
  : Parameter Classes for Data Processing Operations
- [`clusterData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/clusterData.md)
  : Data Clustering

## Poly Generation

Creation of polygon shapes and arrays

- [`hexVertices()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hexVertices.md)
  : Generate regular hexagon vertices
- [`rectVertices()`](https://giotto-suite.github.io/GiottoClass/dev/reference/rectVertices.md)
  : Generate rectangular polygon vertices
- [`circleVertices()`](https://giotto-suite.github.io/GiottoClass/dev/reference/circleVertices.md)
  : Generate circle polygon vertices
- [`makePseudoVisium()`](https://giotto-suite.github.io/GiottoClass/dev/reference/makePseudoVisium.md)
  : makePseudoVisium
- [`polyStamp()`](https://giotto-suite.github.io/GiottoClass/dev/reference/polyStamp.md)
  : Spatial polygons stamp
- [`tessellate()`](https://giotto-suite.github.io/GiottoClass/dev/reference/tessellate.md)
  : Tessellated grid of polygons
- [`triGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/generate_grid.md)
  [`orthoGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/generate_grid.md)
  : Spatial grids

## Interoperability

### From

Convert from other frameworks to Giotto

- [`anndataToGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/anndataToGiotto.md)
  : Convert anndata to Giotto
- [`gefToGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/gefToGiotto.md)
  : Convert gef to Giotto
- [`seuratToGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/seuratToGiotto.md)
  : Deprecated
- [`seuratToGiottoV4()`](https://giotto-suite.github.io/GiottoClass/dev/reference/seuratToGiottoV4.md)
  : Convert a Seurat V4 object to a Giotto object
- [`seuratToGiottoV5()`](https://giotto-suite.github.io/GiottoClass/dev/reference/seuratToGiottoV5.md)
  : Convert a Seurat V5 object to a Giotto object
- [`spatialExperimentToGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatialExperimentToGiotto.md)
  : Utility function to convert a SpatialExperiment object to a Giotto
  object
- [`spatialdataToGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatialdataToGiotto.md)
  : Convert SpatialData to Giotto
- [`giottoMasterToSuite()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoMasterToSuite.md)
  : Convert a master Giotto object to suite

### To

Convert Giotto Object to other frameworks

- [`giottoToAnnData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoToAnnData.md)
  : Convert Giotto to anndata
- [`giottoToSeurat()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoToSeurat.md)
  : Deprecated
- [`giottoToSeuratV4()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoToSeuratV4.md)
  : Convert Giotto to Seurat V4
- [`giottoToSeuratV5()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoToSeuratV5.md)
  : Convert Giotto to Seurat V5
- [`giottoToSpatialData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoToSpatialData.md)
  : Convert Giotto to SpatialData
- [`giottoToSpatialExperiment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoToSpatialExperiment.md)
  : Utility function to convert a Giotto object to a SpatialExperiment
  object.
- [`createBentoAdata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createBentoAdata.md)
  : Create bento adata object from gobject

## Aggregate

### Polygon

Polygon and spatial unit aggregation and combination

- [`aggregateStacks()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacks.md)
  : aggregateStacks
- [`aggregateStacksExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacksExpression.md)
  : aggregateStacksExpression
- [`aggregateStacksLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacksLocations.md)
  : aggregateStacksLocations
- [`aggregateStacksPolygonOverlaps()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacksPolygonOverlaps.md)
  : aggregateStacksPolygonOverlaps
- [`aggregateStacksPolygons()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacksPolygons.md)
  : aggregateStacksPolygons
- [`aggregateFeatures()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateFeatures.md)
  : Aggregate Spatial Features Covered by Polygon Geometries
- [`combineToMultiPolygon()`](https://giotto-suite.github.io/GiottoClass/dev/reference/combineToMultiPolygon.md)
  : Combine giottoPolygon geometries

### Feature

Calculate spatial features overlapped by polygons

- [`calculateOverlap(`*`<giotto>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md)
  [`calculateOverlap(`*`<giottoPolygon>`*`,`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md)
  [`calculateOverlap(`*`<giottoPolygon>`*`,`*`<giottoLargeImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md)
  [`calculateOverlap(`*`<giottoPolygon>`*`,`*`<giottoAffineImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md)
  [`calculateOverlap(`*`<giottoPolygon>`*`,`*`<SpatRaster>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md)
  [`calculateOverlap(`*`<SpatVector>`*`,`*`<SpatRaster>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md)
  [`calculateOverlap(`*`<SpatVector>`*`,`*`<SpatVector>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md)
  : Calculate features overlapped by polygons
- [`calculateOverlapRaster()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlapRaster.md)
  : calculateOverlapRaster
- [`calculateOverlapPolygonImages()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlapPolygonImages.md)
  : calculateOverlapPolygonImages
- [`calculateOverlapSerial()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlapSerial.md)
  : calculateOverlapSerial
- [`calculateOverlapParallel()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlapParallel.md)
  : calculateOverlapParallel
- [`overlaps(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/overlaps-generic.md)
  : overlaps-generic

### Matrix

Convert overlapped features to raw expression matrix

- [`overlapImagesToMatrix()`](https://giotto-suite.github.io/GiottoClass/dev/reference/overlapImagesToMatrix.md)
  : overlapImagesToMatrix
- [`overlapToMatrix(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/overlapToMatrix.md)
  [`overlapToMatrix(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/overlapToMatrix.md)
  [`overlapToMatrix(`*`<SpatVector>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/overlapToMatrix.md)
  [`overlapToMatrix(`*`<data.table>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/overlapToMatrix.md)
  [`overlapToMatrix(`*`<overlapPointDT>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/overlapToMatrix.md)
  [`overlapToMatrix(`*`<overlapIntensityDT>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/overlapToMatrix.md)
  : overlapToMatrix
- [`overlapToMatrixMultiPoly()`](https://giotto-suite.github.io/GiottoClass/dev/reference/overlapToMatrixMultiPoly.md)
  : overlapToMatrixMultiPoly

### Centroids

Centroid calculation

- [`addSpatialCentroidLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/addSpatialCentroidLocations.md)
  : addSpatialCentroidLocations
- [`addSpatialCentroidLocationsLayer()`](https://giotto-suite.github.io/GiottoClass/dev/reference/addSpatialCentroidLocationsLayer.md)
  : addSpatialCentroidLocationsLayer
- [`centroids(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/centroids-generic.md)
  : centroids-generic

## Combine

Create combined table of metadata and other results and data

- [`combineCellData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/combineCellData.md)
  : combineCellData
- [`combineFeatureData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/combineFeatureData.md)
  : combineFeatureData
- [`combineFeatureOverlapData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/combineFeatureOverlapData.md)
  : combineFeatureOverlapData
- [`combineMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/combineMetadata.md)
  : combineMetadata
- [`combineSpatialCellMetadataInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/combineSpatialCellMetadataInfo.md)
  : combineSpatialCellMetadataInfo

## Metafeatures

Metafeature creation

- [`createMetafeats()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createMetafeats.md)
  : createMetafeats

## Classes

GiottoClass defined classes

- [`NNNetworkParam-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/NNNetworkParam-class.md)
  : NNNetworkParam — Nearest-Neighbour Network Param Classes
- [`affine2d-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine2d-class.md)
  [`affine2d`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine2d-class.md)
  : Affine Transform Object
- [`analyzeParam-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeParam-class.md)
  [`analyzeParam`](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeParam-class.md)
  : Parameter Classes for Data Analysis Operations
- [`cellMetaObj-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/cellMetaObj-class.md)
  [`cellMetaObj`](https://giotto-suite.github.io/GiottoClass/dev/reference/cellMetaObj-class.md)
  : S4 cellMetaObj
- [`delaunayNetworkParam()`](https://giotto-suite.github.io/GiottoClass/dev/reference/delaunayNetworkParam-class.md)
  : delaunayNetworkParam — Delaunay Network Param
- [`dimObj-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/dimObj-class.md)
  [`dimObj`](https://giotto-suite.github.io/GiottoClass/dev/reference/dimObj-class.md)
  : S4 dimObj Class
- [`exprObj-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/exprObj-class.md)
  [`exprObj`](https://giotto-suite.github.io/GiottoClass/dev/reference/exprObj-class.md)
  : S4 exprObj
- [`featMetaObj-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/featMetaObj-class.md)
  [`featMetaObj`](https://giotto-suite.github.io/GiottoClass/dev/reference/featMetaObj-class.md)
  : S4 featMetaObj
- [`featureNetwork-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/featureNetwork-class.md)
  [`featureNetwork`](https://giotto-suite.github.io/GiottoClass/dev/reference/featureNetwork-class.md)
  : S4 giotto feature network Class
- [`filterParam-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/filterParam-class.md)
  [`filterParam`](https://giotto-suite.github.io/GiottoClass/dev/reference/filterParam-class.md)
  : Parameter Classes for Data Filter Operations
- [`giotto-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto-class.md)
  [`giotto`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto-class.md)
  : S4 giotto Class
- [`giottoAffineImage-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoAffineImage-class.md)
  : S4 giottoAffineImage Class
- [`createGiottoBinPoints()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoBinPoints-class.md)
  : Binned point class
- [`giottoImage-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoImage-class.md)
  [`giottoImage`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoImage-class.md)
  : S4 giottoImage Class
- [`giottoLargeImage-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoLargeImage-class.md)
  [`giottoLargeImage`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoLargeImage-class.md)
  : S4 giottoLargeImage Class
- [`giottoPoints-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoPoints-class.md)
  [`giottoPoints`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoPoints-class.md)
  : S4 giotto points Class
- [`giottoPolygon-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoPolygon-class.md)
  [`giottoPolygon`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoPolygon-class.md)
  : S4 giotto polygon Class
- [`kNNNetworkParam()`](https://giotto-suite.github.io/GiottoClass/dev/reference/kNNNetworkParam-class.md)
  : kNNNetworkParam — k-Nearest-Neighbour Network Param
- [`labelProportionsParam-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/labelProportionsParam-class.md)
  : Label proportions analysis parameter
- [`miscData-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/miscData-class.md)
  : Basic class for additional miscellaneous information
- [`networkParam-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/networkParam-class.md)
  [`networkParam`](https://giotto-suite.github.io/GiottoClass/dev/reference/networkParam-class.md)
  : Parameter Classes for Network Construction
- [`nnNetObj-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/nnNetObj-class.md)
  [`nnNetObj`](https://giotto-suite.github.io/GiottoClass/dev/reference/nnNetObj-class.md)
  : S4 nnNetObj
- [`overlapInfo-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/overlapInfo-class.md)
  : Geometry Overlap Results
- [`` `[`( ``*`<overlapPointDT>`*`,`*`<gIndex>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/overlapPointDT-class.md)
  [`` `[`( ``*`<overlapPointDT>`*`,`*`<missing>`*`,`*`<gIndex>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/overlapPointDT-class.md)
  [`` `[`( ``*`<overlapPointDT>`*`,`*`<gIndex>`*`,`*`<gIndex>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/overlapPointDT-class.md)
  : Polygon and Point Relationships
- [`processParam-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/processParam-class.md)
  [`processParam`](https://giotto-suite.github.io/GiottoClass/dev/reference/processParam-class.md)
  : Parameter Classes for Data Processing Operations
- [`reduceParam-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/reduceParam-class.md)
  [`reduceParam`](https://giotto-suite.github.io/GiottoClass/dev/reference/reduceParam-class.md)
  : Parameter Classes for Data Reduction Operations
- [`sNNNetworkParam()`](https://giotto-suite.github.io/GiottoClass/dev/reference/sNNNetworkParam-class.md)
  : sNNNetworkParam — Shared-Nearest-Neighbour Network Param
- [`spatEnrObj-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatEnrObj-class.md)
  : S4 spatEnrObj Class
- [`spatLocsObj-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatLocsObj-class.md)
  [`spatLocsObj`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatLocsObj-class.md)
  : S4 spatLocsObj Class
- [`spatialGridObj-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatialGridObj-class.md)
  : S4 spatialGridObj Class
- [`spatialNetworkObj-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatialNetworkObj-class.md)
  : S4 spatialNetworkObj Class
- [`terraVectData-class`](https://giotto-suite.github.io/GiottoClass/dev/reference/terraVectData-class.md)
  [`terraVectData`](https://giotto-suite.github.io/GiottoClass/dev/reference/terraVectData-class.md)
  : Basic class for terra SpatVector-based objects

## Networks

Network creation and tools

- [`createNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createNetwork.md)
  : Create a Network
- [`edge_distances()`](https://giotto-suite.github.io/GiottoClass/dev/reference/edge_distances.md)
  : Calculate network edge euclidean distances
- [`createSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createSpatialNetwork.md)
  : Create spatial network
- [`createSpatialDelaunayNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createSpatialDelaunayNetwork.md)
  : Create a spatial Delaunay network
- [`createSpatialFeaturesKNNnetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createSpatialFeaturesKNNnetwork.md)
  : Create kNN spatial feature network
- [`createSpatialKNNnetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createSpatialKNNnetwork.md)
  : createSpatialKNNnetwork
- [`createSpatialWeightMatrix()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createSpatialWeightMatrix.md)
  : Create a spatial weight matrix
- [`spat_net_to_igraph()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spat_net_to_igraph.md)
  : Convert spatialNetworkObj to igraph
- [`createNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createNearestNetwork.md)
  : createNearestNetwork
- [`addNetworkLayout()`](https://giotto-suite.github.io/GiottoClass/dev/reference/addNetworkLayout.md)
  : addNetworkLayout

## Spatial

### Manipulation

Spatial manipulation and terra-based functions

- [`ext(`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  [`ext(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  [`ext(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  [`ext(`*`<giottoLargeImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  [`ext(`*`<giottoImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  [`ext(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  [`ext(`*`<giottoAffineImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  [`ext(`*`<affine2d>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  [`` `ext<-`( ``*`<spatLocsObj>`*`,`*`<SpatExtent>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  [`` `ext<-`( ``*`<spatialNetworkObj>`*`,`*`<SpatExtent>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  [`` `ext<-`( ``*`<giottoPoints>`*`,`*`<SpatExtent>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  [`` `ext<-`( ``*`<giottoPolygon>`*`,`*`<SpatExtent>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  [`` `ext<-`( ``*`<giottoLargeImage>`*`,`*`<SpatExtent>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  [`` `ext<-`( ``*`<giottoAffineImage>`*`,`*`<SpatExtent>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  [`` `ext<-`( ``*`<ANY>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  [`` `ext<-`( ``*`<giottoImage>`*`,`*`<SpatExtent>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  [`` `ext<-`( ``*`<affine2d>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  : Get a SpatExtent
- [`expanse(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/expanse.md)
  [`area(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/expanse.md)
  : Get the area of individual polygons
- [`XY(`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/XY.md)
  [`` `XY<-`( ``*`<spatLocsObj>`*`,`*`<matrix>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/XY.md)
  [`XY(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/XY.md)
  [`` `XY<-`( ``*`<giottoPoints>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/XY.md)
  [`XY(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/XY.md)
  [`` `XY<-`( ``*`<giottoPolygon>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/XY.md)
  [`XY(`*`<SpatVector>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/XY.md)
  [`` `XY<-`( ``*`<SpatVector>`*`,`*`<matrix>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/XY.md)
  : Spatial coordinates
- [`flip(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/flip.md)
  [`flip(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/flip.md)
  [`flip(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/flip.md)
  [`flip(`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/flip.md)
  [`flip(`*`<giottoLargeImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/flip.md)
  [`flip(`*`<SpatExtent>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/flip.md)
  [`flip(`*`<giottoAffineImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/flip.md)
  [`flip(`*`<affine2d>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/flip.md)
  : Flip an object
- [`crop(`*`<giottoBinPoints>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md)
  [`crop(`*`<giottoLargeImage>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md)
  [`crop(`*`<giottoAffineImage>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md)
  [`crop(`*`<spatLocsObj>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md)
  [`crop(`*`<giottoPoints>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md)
  [`crop(`*`<giottoPolygon>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md)
  : Crop to a spatial subset
- [`t(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/transpose.md)
  [`t(`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/transpose.md)
  [`t(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/transpose.md)
  [`t(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/transpose.md)
  [`t(`*`<giottoLargeImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/transpose.md)
  [`t(`*`<giottoAffineImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/transpose.md)
  [`t(`*`<affine2d>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/transpose.md)
  : Transpose
- [`spin(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spin.md)
  [`spin(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spin.md)
  [`spin(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spin.md)
  [`spin(`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spin.md)
  [`spin(`*`<data.frame>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spin.md)
  [`spin(`*`<giottoLargeImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spin.md)
  [`spin(`*`<giottoAffineImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spin.md)
  [`spin(`*`<affine2d>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spin.md)
  : Spin an object
- [`spatShift(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatShift.md)
  [`spatShift(`*`<SpatExtent>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatShift.md)
  [`spatShift(`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatShift.md)
  [`spatShift(`*`<data.frame>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatShift.md)
  [`spatShift(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatShift.md)
  [`spatShift(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatShift.md)
  [`spatShift(`*`<giottoLargeImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatShift.md)
  [`spatShift(`*`<giottoImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatShift.md)
  [`spatShift(`*`<giottoAffineImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatShift.md)
  [`spatShift(`*`<affine2d>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatShift.md)
  : Spatially shift an object
- [`rescale(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/rescale.md)
  [`rescale(`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/rescale.md)
  [`rescale(`*`<data.frame>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/rescale.md)
  [`rescale(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/rescale.md)
  [`rescale(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/rescale.md)
  [`rescale(`*`<giottoImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/rescale.md)
  [`rescale(`*`<giottoLargeImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/rescale.md)
  [`rescale(`*`<giottoAffineImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/rescale.md)
  [`rescale(`*`<affine2d>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/rescale.md)
  : Rescale an object
- [`shear(`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/shear.md)
  [`shear(`*`<SpatVector>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/shear.md)
  [`shear(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/shear.md)
  [`shear(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/shear.md)
  [`shear(`*`<giottoLargeImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/shear.md)
  [`shear(`*`<giottoAffineImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/shear.md)
  [`shear(`*`<affine2d>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/shear.md)
  : Apply a shear tranform
- [`affine(`*`<giotto>`*`,`*`<matrix>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md)
  [`affine(`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md)
  [`affine(`*`<ANY>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md)
  [`affine(`*`<ANY>`*`,`*`<affine2d>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md)
  [`affine(`*`<SpatVector>`*`,`*`<matrix>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md)
  [`affine(`*`<giottoPoints>`*`,`*`<matrix>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md)
  [`affine(`*`<giottoPolygon>`*`,`*`<matrix>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md)
  [`affine(`*`<spatLocsObj>`*`,`*`<matrix>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md)
  [`affine(`*`<giottoLargeImage>`*`,`*`<matrix>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md)
  [`affine(`*`<giottoAffineImage>`*`,`*`<matrix>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md)
  [`affine(`*`<affine2d>`*`,`*`<matrix>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md)
  : Affine transformations
- [`hull(`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/hull.md)
  [`hull(`*`<giottoSpatial>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/hull.md)
  [`minRect()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hull.md)
  [`minCircle()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hull.md)
  [`convHull()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hull.md)
  : Convex, concave, rectangular and circular hulls
- [`buffer(`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/buffer.md)
  [`buffer(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/buffer.md)
  [`buffer(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/buffer.md)
  : Create a buffer around vector geometries
- [`settleGeom(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/settleGeom.md)
  [`settleGeom(`*`<SpatVector>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/settleGeom.md)
  : Settle polygon bounds
- [`combineGeom(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/combine_split_geoms.md)
  [`splitGeom(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/combine_split_geoms.md)
  [`combineGeom(`*`<SpatVector>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/combine_split_geoms.md)
  [`splitGeom(`*`<SpatVector>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/combine_split_geoms.md)
  : Combine or Split Complex Geometries
- [`relate(`*`<giottoSpatial>`*`,`*`<giottoSpatial>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/relate.md)
  [`relate(`*`<giotto>`*`,`*`<giottoSpatial>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/relate.md)
  : Spatial relationships between geometries
- [`erase(`*`<spatialClasses>`*`,`*`<spatialClasses>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatial_binary_ops.md)
  [`snap(`*`<giottoSpatial>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatial_binary_ops.md)
  [`symdif(`*`<spatialClasses>`*`,`*`<spatialClasses>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatial_binary_ops.md)
  [`union(`*`<spatialClasses>`*`,`*`<spatialClasses>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatial_binary_ops.md)
  [`intersect(`*`<spatialClasses>`*`,`*`<spatialClasses>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatial_binary_ops.md)
  : Spatial binary operations
- [`rescalePolygons()`](https://giotto-suite.github.io/GiottoClass/dev/reference/rescalePolygons.md)
  : rescalePolygons
- [`wrap(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/wrap.md)
  [`wrap(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/wrap.md)
  [`wrap(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/wrap.md)
  [`vect(`*`<packedGiottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/wrap.md)
  [`vect(`*`<packedGiottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/wrap.md)
  [`vect(`*`<packedGiotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/wrap.md)
  : Wrap giotto terra pointer information

### Querying

Spatial querying

- [`spatQuery()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatQuery.md)
  [`spatQueryGiottoPolygons()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatQuery.md)
  : Spatial Query

## Object generics

Standard generics for Giotto exported classes

- [`plot(`*`<giottoImage>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  [`plot(`*`<giottoLargeImage>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  [`plot(`*`<giottoAffineImage>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  [`plot(`*`<giottoPolygon>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  [`plot(`*`<giottoPoints>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  [`plot(`*`<spatLocsObj>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  [`plot(`*`<dimObj>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  [`plot(`*`<spatialNetworkObj>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  [`plot(`*`<affine2d>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  : Preview a Giotto spatial object

- [`` `[`( ``*`<giottoBinPoints>`*`,`*`<logical>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<gdtData>`*`,`*`<gIndex>`*`,`*`<gIndex>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<gdtData>`*`,`*`<logical>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<gdtData>`*`,`*`<character>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<gdtData>`*`,`*`<missing>`*`,`*`<numeric>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<gdtData>`*`,`*`<missing>`*`,`*`<logical>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<coordDataDT>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<coordDataDT>`*`,`*`<missing>`*`,`*`<ANY>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<coordDataDT>`*`,`*`<missing>`*`,`*`<character>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<coordDataDT>`*`,`*`<ANY>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<coordDataDT>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<giottoPoints>`*`,`*`<gIndex>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<metaData>`*`,`*`<missing>`*`,`*`<ANY>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<metaData>`*`,`*`<missing>`*`,`*`<character>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<metaData>`*`,`*`<ANY>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<metaData>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<dimObj>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<dimObj>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<exprData>`*`,`*`<missing>`*`,`*`<ANY>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<exprData>`*`,`*`<ANY>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<exprData>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<exprData>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<spatNetData>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<nnData>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<enrData>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<enrData>`*`,`*`<ANY>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<enrData>`*`,`*`<missing>`*`,`*`<ANY>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<enrData>`*`,`*`<missing>`*`,`*`<character>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<spatGridData>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<giottoPoints>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<giottoPoints>`*`,`*`<character>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<giottoPoints>`*`,`*`<missing>`*`,`*`<gIndex>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<giottoPolygon>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<giottoPolygon>`*`,`*`<gIndex>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<giottoPolygon>`*`,`*`<character>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<giottoPolygon>`*`,`*`<missing>`*`,`*`<gIndex>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<terraVectData>`*`,`*`<gIndex>`*`,`*`<gIndex>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<giottoLargeImage>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[[`( ``*`<giottoLargeImage>`*`,`*`<gIndex>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<giottoImage>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<affine2d>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  [`` `[`( ``*`<processParam>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
  :

  Subset part of an object with `[` or `[[`

- [`` `[<-`( ``*`<coordDataDT>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_bracket.md)
  [`` `[<-`( ``*`<metaData>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_bracket.md)
  [`` `[<-`( ``*`<dimObj>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_bracket.md)
  [`` `[<-`( ``*`<exprData>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_bracket.md)
  [`` `[<-`( ``*`<spatNetData>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_bracket.md)
  [`` `[<-`( ``*`<nnData>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_bracket.md)
  [`` `[<-`( ``*`<enrData>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_bracket.md)
  [`` `[<-`( ``*`<spatGridData>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_bracket.md)
  [`` `[<-`( ``*`<giottoPoints>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_bracket.md)
  [`` `[<-`( ``*`<giottoPolygon>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_bracket.md)
  [`` `[<-`( ``*`<giottoLargeImage>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_bracket.md)
  [`` `[<-`( ``*`<giottoImage>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_bracket.md)
  [`` `[<-`( ``*`<affine2d>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_bracket.md)
  [`` `[<-`( ``*`<processParam>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<list>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_bracket.md)
  :

  Replace part of an object with `[<-`

- [`` `[`( ``*`<giotto>`*`,`*`<gIndex>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_giotto.md)
  [`` `[`( ``*`<giotto>`*`,`*`<missing>`*`,`*`<gIndex>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_giotto.md)
  [`` `[`( ``*`<giotto>`*`,`*`<gIndex>`*`,`*`<gIndex>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_giotto.md)
  [`` `[`( ``*`<giotto>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_giotto.md)
  [`subset(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_giotto.md)
  :

  Subset a `giotto` object

- [`` `[[`( ``*`<giotto>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_giotto_subobjects.md)
  [`` `[[`( ``*`<giotto>`*`,`*`<character>`*`,`*`<missing>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_giotto_subobjects.md)
  [`` `[[`( ``*`<giotto>`*`,`*`<missing>`*`,`*`<character>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_giotto_subobjects.md)
  [`` `[[`( ``*`<giotto>`*`,`*`<character>`*`,`*`<character>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_giotto_subobjects.md)
  :

  Subset `giotto` subobjects

- [`` `$`( ``*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_dollar.md)
  [`` `$`( ``*`<coordDataDT>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_dollar.md)
  [`` `$`( ``*`<spatEnrObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_dollar.md)
  [`` `$`( ``*`<dimObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_dollar.md)
  [`` `$`( ``*`<metaData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_dollar.md)
  [`` `$`( ``*`<terraVectData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_dollar.md)
  [`` `$`( ``*`<affine2d>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_dollar.md)
  [`` `$`( ``*`<processParam>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_dollar.md)
  [`` `$`( ``*`<analyzeParam>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_dollar.md)
  [`` `$`( ``*`<filterParam>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_dollar.md)
  [`` `$`( ``*`<reduceParam>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_dollar.md)
  :

  Subset part of an object with `$`

- [`` `$<-`( ``*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_dollar.md)
  [`` `$<-`( ``*`<coordDataDT>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_dollar.md)
  [`` `$<-`( ``*`<spatEnrObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_dollar.md)
  [`` `$<-`( ``*`<dimObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_dollar.md)
  [`` `$<-`( ``*`<metaData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_dollar.md)
  [`` `$<-`( ``*`<terraVectData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_dollar.md)
  [`` `$<-`( ``*`<processParam>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_dollar.md)
  [`` `$<-`( ``*`<analyzeParam>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_dollar.md)
  [`` `$<-`( ``*`<filterParam>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_dollar.md)
  [`` `$<-`( ``*`<reduceParam>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_dollar.md)
  :

  Replace part of an object with `$<-`

- [`rbind2(`*`<giottoBinPoints>`*`,`*`<giottoBinPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/rbind-generic.md)
  [`rbind2(`*`<cellMetaObj>`*`,`*`<cellMetaObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/rbind-generic.md)
  [`rbind2(`*`<featMetaObj>`*`,`*`<featMetaObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/rbind-generic.md)
  [`rbind2(`*`<spatLocsObj>`*`,`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/rbind-generic.md)
  [`rbind2(`*`<giottoPolygon>`*`,`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/rbind-generic.md)
  [`rbind2(`*`<giottoPoints>`*`,`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/rbind-generic.md)
  [`rbind2(`*`<overlapPointDT>`*`,`*`<overlapPointDT>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/rbind-generic.md)
  : Combine objects by rows (Giotto-related)

- [`colnames(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/row-plus-colnames-generic.md)
  [`colnames(`*`<exprObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/row-plus-colnames-generic.md)
  [`colnames(`*`<cellMetaObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/row-plus-colnames-generic.md)
  [`colnames(`*`<featMetaObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/row-plus-colnames-generic.md)
  [`colnames(`*`<spatEnrObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/row-plus-colnames-generic.md)
  [`colnames(`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/row-plus-colnames-generic.md)
  [`colnames(`*`<dimObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/row-plus-colnames-generic.md)
  [`rownames(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/row-plus-colnames-generic.md)
  [`rownames(`*`<exprObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/row-plus-colnames-generic.md)
  [`rownames(`*`<dimObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/row-plus-colnames-generic.md)
  [`rownames(`*`<metaData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/row-plus-colnames-generic.md)
  : Row and column names

- [`dimnames(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dimnames.md)
  [`dimnames(`*`<exprObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dimnames.md)
  [`dimnames(`*`<dimObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dimnames.md)
  [`dimnames(`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dimnames.md)
  [`dimnames(`*`<metaData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dimnames.md)
  [`dimnames(`*`<enrData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dimnames.md)
  : Dimnames of an object

- [`nrow(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`nrow(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`nrow(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`nrow(`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`nrow(`*`<exprData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`nrow(`*`<metaData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`nrow(`*`<spatialNetworkObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`nrow(`*`<enrData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`nrow(`*`<dimObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`nrow(`*`<overlapPointDT>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`nrow(`*`<overlapIntensityDT>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`ncol(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`ncol(`*`<exprData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`ncol(`*`<metaData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`ncol(`*`<enrData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`ncol(`*`<dimObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`ncol(`*`<overlapPointDT>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`ncol(`*`<overlapIntensityDT>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`dim(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`dim(`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`dim(`*`<exprData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`dim(`*`<metaData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`dim(`*`<enrData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`dim(`*`<giottoLargeImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`dim(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`dim(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`dim(`*`<overlapPointDT>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  [`dim(`*`<overlapIntensityDT>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/dims-generic.md)
  : Dimensions of giotto objects

- [`copy(`*`<coordDataDT>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/copy.md)
  [`copy(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/copy.md)
  [`copy(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/copy.md)
  [`copy(`*`<giottoLargeImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/copy.md)
  : Copy an entire object

### Hierarchical

Hierarchical tagging generics for Giotto exported classes

- [`activeFeatType(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/activeFeatType-generic.md)
  [`` `activeFeatType<-`( ``*`<giotto>`*`,`*`<character>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/activeFeatType-generic.md)
  : Active feature type
- [`activeSpatUnit(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/activeSpatUnit-generic.md)
  [`` `activeSpatUnit<-`( ``*`<giotto>`*`,`*`<character>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/activeSpatUnit-generic.md)
  : Active spatial unit
- [`spatUnit(`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`spatUnit(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`spatUnit(`*`<list>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`spatUnit(`*`<spatData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`spatUnit(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`` `spatUnit<-`( ``*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`` `spatUnit<-`( ``*`<spatData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`` `spatUnit<-`( ``*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`` `spatUnit<-`( ``*`<list>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`` `spatUnit<-`( ``*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`featType(`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`featType(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`featType(`*`<list>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`featType(`*`<featData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`` `featType<-`( ``*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`` `featType<-`( ``*`<featData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`` `featType<-`( ``*`<list>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`` `featType<-`( ``*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`objName(`*`<ANY>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`objName(`*`<list>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`objName(`*`<nameData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`objName(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`objName(`*`<giottoLargeImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`objName(`*`<giottoImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`` `objName<-`( ``*`<list>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`` `objName<-`( ``*`<nameData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`` `objName<-`( ``*`<giottoImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`` `objName<-`( ``*`<giottoLargeImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`` `objName<-`( ``*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`prov(`*`<provData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  [`` `prov<-`( ``*`<provData>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  : Giotto schema
- [`featIDs(`*`<giottoBinPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  [`spatIDs(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  [`spatIDs(`*`<exprObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  [`spatIDs(`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  [`spatIDs(`*`<cellMetaObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  [`spatIDs(`*`<spatialNetworkObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  [`spatIDs(`*`<dimObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  [`spatIDs(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  [`` `spatIDs<-`( ``*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  [`spatIDs(`*`<spatEnrObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  [`spatIDs(`*`<nnNetObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  [`featIDs(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  [`featIDs(`*`<exprObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  [`featIDs(`*`<featMetaObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  [`featIDs(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  [`featIDs(`*`<spatEnrObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  : Spatial and feature IDs
- [`overlaps(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/overlaps-generic.md)
  : overlaps-generic

## Save/Load

Saving and loading of the Giotto Object

- [`loadGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/loadGiotto.md)
  : loadGiotto
- [`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)
  : saveGiotto
- [`reconnectGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/reconnectGiottoImage.md)
  : Reconnect images with dead pointers

## As

As coercion functions

- [`as.list(`*`<giotto>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.list.md)
  : Coerce to a list
- [`as.points(`*`<data.frame>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.points.md)
  [`as.points(`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.points.md)
  : Coerce to SpatVector points
- [`as.polygons(`*`<data.frame>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.polygons.md)
  : Coerce to SpatVector polygons
- [`as.data.table(`*`<giottoBinPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.data.table.md)
  [`as.data.table(`*`<SpatVector>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.data.table.md)
  [`as.data.table(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.data.table.md)
  [`as.data.table(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.data.table.md)
  [`as.data.frame(`*`<overlapPointDT>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.data.table.md)
  [`as.data.frame(`*`<overlapIntensityDT>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.data.table.md)
  : Coerce to data.table
- [`as.matrix(`*`<spatLocsObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.matrix.md)
  [`as.matrix(`*`<overlapPointDT>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.matrix.md)
  [`as.matrix(`*`<overlapIntensityDT>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.matrix.md)
  [`as.matrix(`*`<nnNetObj>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.matrix.md)
  : Coerce to matrix
- [`as.character(`*`<giottoImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.character.md)
  [`as.character(`*`<svkey>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.character.md)
  [`as.character(`*`<giottoLargeImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.character.md)
  : Create a text representation of an object
- [`as.sp(`*`<sf>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.sp(`*`<SpatVector>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.sp(`*`<stars>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.sp(`*`<Spatial>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.sp(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.sp(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.sf(`*`<SpatVector>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.sf(`*`<Spatial>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.sf(`*`<stars>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.sf(`*`<sf>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.sf(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.sf(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.stars(`*`<SpatVector>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.stars(`*`<sf>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.stars(`*`<Spatial>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.stars(`*`<stars>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.stars(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.stars(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.terra(`*`<SpatVector>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.terra(`*`<sf>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.terra(`*`<stars>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.terra(`*`<Spatial>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.terra(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  [`as.terra(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  : R spatial conversions

## Python

Giotto python environment

- [`checkGiottoEnvironment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_python.md)
  [`installGiottoEnvironment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_python.md)
  [`removeGiottoEnvironment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_python.md)
  [`set_giotto_python_path()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_python.md)
  : Giotto python environment
- [`checkPythonPackage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/checkPythonPackage.md)
  : Check Python Package Installation

## Coordinate stitching

Coordinate stitching

- [`stitchFieldCoordinates()`](https://giotto-suite.github.io/GiottoClass/dev/reference/stitchFieldCoordinates.md)
  : stitchFieldCoordinates
- [`stitchTileCoordinates()`](https://giotto-suite.github.io/GiottoClass/dev/reference/stitchTileCoordinates.md)
  : stitchTileCoordinates

## Images

Image tools

- [`stitchGiottoLargeImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/stitchGiottoLargeImage.md)
  : Stitch multiple giottoLargeImage objects into a single
  giottoLargeImage object
- [`convertGiottoLargeImageToMG()`](https://giotto-suite.github.io/GiottoClass/dev/reference/convertGiottoLargeImageToMG.md)
  : convertGiottoLargeImageToMG
- [`estimateImageBg()`](https://giotto-suite.github.io/GiottoClass/dev/reference/estimateImageBg.md)
  : estimateImageBg
- [`distGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/distGiottoImage.md)
  : Plot distribution of image intensity values
- [`density(`*`<giottoLargeImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/density.md)
  : Density plot
- [`hist(`*`<giottoLargeImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/hist.md)
  : Histogram
- [`to_simple_tif()`](https://giotto-suite.github.io/GiottoClass/dev/reference/to_simple_tif.md)
  [`ometif_to_tif()`](https://giotto-suite.github.io/GiottoClass/dev/reference/to_simple_tif.md)
  : Convert Specialized TIF Formats to Basic TIF
- [`tif_metadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/tif_metadata.md)
  [`ometif_metadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/tif_metadata.md)
  : Read Metadata of a Specialized tif
- [`reconnect(`*`<giottoAffineImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/reconnect.md)
  [`reconnect(`*`<giottoLargeImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/reconnect.md)
  [`reconnect(`*`<giottoImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/reconnect.md)
  : Reconnect a GiottoClass object
- [`changeImageBg()`](https://giotto-suite.github.io/GiottoClass/dev/reference/changeImageBg.md)
  : changeImageBg
- [`writeGiottoLargeImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/writeGiottoLargeImage.md)
  : writeGiottoLargeImage
- [`updateGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/updateGiottoImage.md)
  : updateGiottoImage
- [`updateGiottoImageMG()`](https://giotto-suite.github.io/GiottoClass/dev/reference/updateGiottoImageMG.md)
  : updateGiottoImageMG
- [`updateGiottoLargeImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/updateGiottoLargeImage.md)
  : updateGiottoLargeImage
- [`add_img_array_alpha()`](https://giotto-suite.github.io/GiottoClass/dev/reference/add_img_array_alpha.md)
  : Add alpha channel to image array

## History

Functions for logging Giotto Object history

- [`objHistory()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objHistory.md)
  : Giotto object history
- [`update_giotto_params()`](https://giotto-suite.github.io/GiottoClass/dev/reference/update_giotto_params.md)
  : Update giotto parameters
- [`showProcessingSteps()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showProcessingSteps.md)
  : showProcessingSteps

## Utilities

Utility functions that are used in other code

- [`polygon_to_raster()`](https://giotto-suite.github.io/GiottoClass/dev/reference/polygon_to_raster.md)
  : Convert polygon to raster
- [`smoothGiottoPolygons()`](https://giotto-suite.github.io/GiottoClass/dev/reference/smoothGiottoPolygons.md)
  : smoothGiottoPolygons
- [`doDeferred(`*`<giottoAffineImage>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/doDeferred.md)
  : Perform deferred/lazy operations

## Deprecated

Deprecated and/or less supported older functions. Some of these
functions are here for compatibility and internal purposes.

- [`getSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialGrid.md)
  : Get spatial grid
- [`get_distance()`](https://giotto-suite.github.io/GiottoClass/dev/reference/get_distance.md)
  : get_distance
- [`get_multiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/get_multiomics.md)
  : Get multiomics integration results
- [`createSpatialDefaultGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createSpatialDefaultGrid.md)
  : createSpatialDefaultGrid
- [`createSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createSpatialGrid.md)
  : createSpatialGrid
- [`setSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialGrid.md)
  : Set spatial grid
- [`set_multiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/set_multiomics.md)
  : Set multiomics integration results
- [`triGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/generate_grid.md)
  [`orthoGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/generate_grid.md)
  : Spatial grids
- [`cropGiottoLargeImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/cropGiottoLargeImage.md)
  : Crop a giotto largeImage object
- [`plotGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/plotGiottoImage.md)
  : Plot a giotto image object
- [`showGiottoSpatGrids()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatGrids.md)
  : showGiottoSpatGrids
- [`showGrids()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGrids.md)
  : Show Spatial Grids
- [`showNetworks()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showNetworks.md)
  : Show networks
- [`addGiottoPoints()`](https://giotto-suite.github.io/GiottoClass/dev/reference/addGiottoPoints.md)
  [`addGiottoPoints3D()`](https://giotto-suite.github.io/GiottoClass/dev/reference/addGiottoPoints.md)
  : Add subcellular giotto points object to giotto object
- [`addGiottoPolygons()`](https://giotto-suite.github.io/GiottoClass/dev/reference/addGiottoPolygons.md)
  : Add giotto polygons to giotto object
- [`annotateSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/annotateSpatialGrid.md)
  : annotateSpatialGrid
- [`annotate_spatlocs_with_spatgrid_2D()`](https://giotto-suite.github.io/GiottoClass/dev/reference/annotate_spatlocs_with_spatgrid_2D.md)
  : annotate_spatlocs_with_spatgrid_2D
- [`annotate_spatlocs_with_spatgrid_3D()`](https://giotto-suite.github.io/GiottoClass/dev/reference/annotate_spatlocs_with_spatgrid_3D.md)
  : annotate_spatlocs_with_spatgrid_3D

## New on the gsource line

Generics and helpers introduced on the gsource development line. Not
present in the released version.

- [`analyzeData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeData.md)
  : Data Analysis
- [`filterData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/filterData.md)
  : Data Filter
- [`reduceData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/reduceData.md)
  : Data Reduction
- [`networkParam()`](https://giotto-suite.github.io/GiottoClass/dev/reference/networkParam.md)
  : networkParam — Dispatcher constructor
- [`labelProportionsParam()`](https://giotto-suite.github.io/GiottoClass/dev/reference/labelProportionsParam.md)
  : Construct a labelProportionsParam
- [`hnswKNN()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hnswKNN.md)
  : Approximate k-nearest neighbors via HNSW
- [`spatRelate(`*`<giottoSpatial>`*`,`*`<giottoSpatial>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatRelate.md)
  : Spatial relationship as a filter
- [`subset(`*`<giottoPolygon>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_terravectdata.md)
  [`subset(`*`<giottoPoints>`*`)`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_terravectdata.md)
  : Subset terraVectData subobjects
