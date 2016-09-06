library(data.table)
library(dplyr)
library(uuid)
library(digest)
library(httr)
library(xml2)

loadDwCTerms = function () {
  dwcEventPage = GET("http://tools.gbif.org/dwca-validator/extension.do?id=dwc:Event")
  assign(
    'dwcEventTerms',
    gsub('\n|\t| ', '', xml_text(xml_find_all(x=read_html(dwcEventPage), xpath='//div[@class="definition"]//div[@class="title"]//div[@class="head"]'))),
    envir = parent.env(environment())
  )
  
  dwcMeasurementOrFactPage = GET("http://tools.gbif.org/dwca-validator/extension.do?id=http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact")
  assign(
    'dwcMeasurementOrFactTerms',
    gsub('\n|\t| ', '', xml_text(xml_find_all(x=read_html(dwcMeasurementOrFactPage), xpath='//div[@class="definition"]//div[@class="title"]//div[@class="head"]'))),
    envir = parent.env(environment())
  )
  
  dwcOccurrencePage = GET("http://tools.gbif.org/dwca-validator/extension.do?id=dwc:Occurrence")
  assign(
    'dwcOccurrenceTerms',
    gsub('\n|\t| ', '', xml_text(xml_find_all(x=read_html(dwcOccurrencePage), xpath='//div[@class="definition"]//div[@class="title"]//div[@class="head"]'))),
    envir = parent.env(environment())
  )
}


#dwcEventTerms = c(
#  "parentEventID",
#  "eventID",
#  "sampleSizeValue",
#  "sampleSizeUnit",
#  "samplingProtocol",
#  "samplingEffort",
#  "eventDate",
#  "locality",
#  "locationID",
#  "higherGeography",
#  "fieldNumber",
#  "county",
#  "country",
#  "decimalLatitude",
#  "decimalLongitude",
#  "eventRemarks",
#  "year"
#)

#dwcMeasurementOrFactTerms = c(
#  "eventID",
#  "measurementType",
#  "measurementValue",
#  "measurementAccuracy",
#  "measurementUnit",
#  "measurementDeterminedDate",
#  "measurementDeterminedBy",
#  "measurementMethod",
#  "measurementRemarks"
#)

#dwcOccurrenceTerms = c(
#  "eventID",
#  "occurrenceID",
#  "recordNumber",
#  "recordedBy",
#  "catalogNumber",
#  "family",
#  "scientificName",
#  "vernacularFaName",
#  "individualCount",
#  "datasetName",
#  "license",
#  "references",
#  "occurrenceRemarks"
#)


rnSubset = function (dt, wanted, mapped) {
  dt = dt[, .SD, .SDcols = wanted]
  
  ## map column names to dwc vocab (just do your best)
  setnames(dt, 
           old=wanted,
           new=mapped
  )
  dt
}


getDwCTable = function (dt_name, vocab, ext='o', ...) {
  
  argg <- c(as.list(environment()), list(...))
  
  if (!is.character(dt_name)) {
    dt_name = deparse(substitute(dt_name))
  }
  
  if (exists(dt_name)) {
    dt_orig = get(dt_name)
    
  }
  else {
    dt_orig = NULL
  }
  
  if (!is.null(dt_orig) & is.data.table(dt_orig)) {
    dt = copy(get(dt_name))
  }
  else if (!is.null(dt_orig) & is.data.frame(dt_orig)) {
    dt = as.data.table(copy(get(dt_name)))
  }
  else {
    dt = data.table('dummy' = NA_character_)
  }
  
  dtnames = names(dt)
  
  if (ext == 'o') {
    sapply(vocab, FUN = function (n) {
      if ((n %in% dtnames) == F) {
        # print(argg[n][[1]])
        if (!is.null(argg[n][[1]])) {
          dt[, eval(n) := get(argg[n][[1]])]
        }
        else {
          dt[, eval(n) := NA_character_]
        }
      }
    })
    #dt = dt[[1]]
  }
  else if (ext == 'i') {
    sapply(vocab, FUN = function (n) {
      if ((n %in% dtnames) == F) {
        # print(argg[n][[1]])
        if (!is.null(argg[n][[1]])) {
          dt[, eval(n) := get(argg[n][[1]])]
        }
      }
    })
    
    dt = dt[, .SD, .SDcols = intersect(colnames(dt), vocab)]
  }
  else {
    sapply(vocab, FUN = function (n) {
      if ((n %in% dtnames) == F) {
        # print(argg[n][[1]])
        if (!is.null(argg[n][[1]])) {
          dt[, eval(n) := get(argg[n][[1]])]
        }
        else {
          dt[, eval(n) := NA_character_]
        }
      }
    })
    dt = dt[, .SD, .SDcols = intersect(colnames(dt), vocab)]
    
  }
  
  if ('dummy' %in% dtnames) {
    dt[, .SD, .SDcols=-c('dummy')]
  }
  else {
    dt
  }
  
}


getSpecialVarPattern = function () {
  eventSpecialVarPattern = "(MeasUnit|measUnitVar|DetBy|detByVar|_uuid)$"
  eventSpecialVarPattern
}

getIDVars = function (dt) {
  eventSpecialVarPattern = getSpecialVarPattern()
  idVars = append(
    intersect(
      Reduce(c, list(dwcOccurrenceTerms, dwcMeasurementOrFactTerms, dwcEventTerms)),
      colnames(dt)
    ),
    grep(colnames(dt), pattern = eventSpecialVarPattern, value = T)
  )
  idVars
}

getMeasVars = function (dt) {
  measVars = setdiff(colnames(dt), getIDVars(dt))
  measVars
}

createUUID = function (dt_orig, typeLevel, cols) {
  
  uuid_colname = paste0(typeLevel, '_uuid')
  
  print(cols)
  dt = copy(dt_orig)
  dt[, 'md5'] = apply(as.matrix(dt[, .SD, .SDcols=cols]), MARGIN = 1, digest, algo='md5')
  print(nrow(dt))
  
  if (uuid_colname %in% colnames(dt)) {
    dt_idmap = unique(dt[complete.cases(dt[, .SD, .SD=c('md5', uuid_colname)]), .SD, .SD=c('md5', uuid_colname)])
    #should check if one md5 map to multiple uuid
    print(dt_idmap)
    setkey(dt_idmap, 'md5')
  }
  
  dt_type_lvl = unique(dt[, .SD, .SDcols=c(cols, 'md5')])
  
  dt_type_lvl[, uuid_colname] = sapply(X = rep(NA, times=nrow(dt_type_lvl)), UUIDgenerate)
  print(nrow(dt_type_lvl))
  
  setkey(dt, 'md5')
  setkey(dt_type_lvl, 'md5')
  
  dt_uuided = dt_type_lvl[, .SD, .SDcols=c('md5', uuid_colname)][dt]
  if (uuid_colname %in% colnames(dt)) {
    dt_uuided = dt_uuided[, .SD, .SDcols=-paste0('i.', uuid_colname)]
    dt_uuided = dt_idmap[dt_uuided]
    dt_uuided = dt_uuided[, .SD, .SDcols=-paste0('i.', uuid_colname)]
  }
  
  dt_uuided[, .SD, .SDcols=-"md5"]
  
}

#rktw[3, 'fbGroupSetup_uuid'] = NA
#head(rktw)
#createUUID(rktw, 'fbGroupSetup', c('fbGroup'))

makeMeasurementTpl = function (dt_orig, measVars = NA, ...) {
  dt = copy(dt_orig)
  
  eventSpecialVarPattern = getSpecialVarPattern()
  
  meas_meta_suffix = c(
    'DetBy',
    'MeasUnit',
    'MeasAcc',
    'DetDate',
    'MeasMethod',
    'MeasRemarks'
  )
  
  meas_meta_full = c(
    'measurementDeterminedBy',
    'measurementUnit',
    'measurementAccuracy',
    'measurementDeterminedDate',
    'measurementMethod',
    'measurementRemarks'
  )
  
  idVars = getIDVars(dt)
  
  dt_nrow = nrow(dt)
  
  if (is.na(measVars)) {
    measVars = setdiff(colnames(dt), idVars)
  }
  
  dt.molten = melt.data.table (
    dt, 
    id.vars = idVars,
    measure.vars = measVars,
    variable.name = "measurementType",
    value.name = "measurementValue"
  )
  
  meas_vars = unique(dt.molten$measurementType)
  tpl = copy(dt)
  meas_meta_var_colname = as.matrix(sapply(meas_vars, function(mv){
    meas_meta_full = paste0(mv, meas_meta_suffix)
    tpl.list = setNames(as.list(rep(NA, times=length(meas_meta_full))), meas_meta_full)
    assign(x = 'tpl', value = data.frame(append(tpl, tpl.list, after=match(mv, names(tpl)))), envir = parent.env(environment()))
  }))
  
  tpl
  
}

makeMeasurement = function (dt_orig, measVars = NA, ...) {
  
  argg <- c(as.list(environment()), list(...))
  dt = copy(dt_orig)
  
  eventSpecialVarPattern = getSpecialVarPattern()
  
  meas_meta_suffix = c(
    'DetBy',
    'MeasUnit',
    'MeasAcc',
    'DetDate',
    'MeasMethod',
    'MeasRemarks'
  )
  
  meas_meta_full = c(
    'measurementDeterminedBy',
    'measurementUnit',
    'measurementAccuracy',
    'measurementDeterminedDate',
    'measurementMethod',
    'measurementRemarks'
  )
  
  idVars = getIDVars(dt)
  #append(
  #    intersect(
  #        Reduce(c, list(dwcOccurrenceTerms, dwcMeasurementOrFactTerms, dwcEventTerms)),
  #        colnames(dt)
  #    ),
  #    grep(colnames(dt), pattern = eventSpecialVarPattern, value = T)
  #)
  
  dt_nrow = nrow(dt)
  
  if (is.na(measVars)) {
    measVars = setdiff(colnames(dt), idVars)
  }
  
  dt = melt.data.table (
    dt, 
    id.vars = idVars,
    measure.vars = measVars,
    variable.name = "measurementType",
    value.name = "measurementValue"
  )
  
  #invisible(dt[, detBy:=paste0(measurementType, "DetBy")][, measUnit:=paste0(measurementType, "MeasUnit")])
  
  
  #print(unique(dt$detBy))
  dtnames = colnames(dt)
  
  meas_vars = unique(dt$measurementType)
  meas_meta_var_colname = as.matrix(sapply(meas_vars, function(mv){
    paste0(mv, meas_meta_suffix)
  }))
  #print(meas_meta_var_colname)
  
  
  qq = apply(meas_meta_var_colname, MARGIN = 1, function(tpl) {
    #print(tpl)
    meta = sapply(tpl, function(metaVar) {
      #print(metaVar)
      if (metaVar %in% dtnames) {
        dt[c(1:dt_nrow), get(eval(metaVar))]
      }
      else {
        rep(x=NA_character_, times = dt_nrow)
      }
    })
    as.vector(as.matrix(meta))
  })
  qq = as.data.table(qq)
  colnames(qq) <- meas_meta_full
  dt = cbind(dt, qq)
  
  meas = dt[, .SD, .SDcols = intersect(dwcMeasurementOrFactTerms, colnames(dt))]
  meas[, measurementIDMaterial := paste0('meas_',make.names(measurementType))]
  
  meas_names = colnames(meas)
  
  sapply(dwcMeasurementOrFactTerms, FUN = function (n) {
    if ((n %in% meas_names) == F) {
      if (!is.null(argg[n][[1]])) {
        meas[, eval(n) := dt[, argg[n][[1]], with=F]]
      }
      else {
        meas[, eval(n) := NA_character_]
      }
    }
    else {
      if (!is.null(argg[n][[1]])) {
        meas[, eval(n) := dt[, argg[n][[1]], with=F]]
      }
    }
  })        
  
  # meas[, 'measurementID'] = sapply(X = rep(NA, times=nrow(meas)), UUIDgenerate)
  meas
  
}