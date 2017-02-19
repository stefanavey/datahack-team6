function (obj) {
    loc.obj <- obj
    net.SUID = as.character(loc.obj@window.id)
    version = pluginVersion(loc.obj)
    if (length(edgeNames(loc.obj@graph)) == 0) {
        write("NOTICE in RCy3::sendEdges():\n\t no edges in graph >> function returns", 
            stderr())
        return()
    }
    write(sprintf("transforming (%d) graph edges to nodePairTable", 
        length(edgeNames(loc.obj@graph))), stderr())
    if (loc.obj@collectTimings) {
        start.time = Sys.time()
    }
    if (is.classic.graph(loc.obj@graph)) {
        tbl.edges = .classicGraphToNodePairTable(loc.obj@graph)
    }
    else if (is.multiGraph(loc.obj@graph)) {
        tbl.edges = .multiGraphToNodePairTable(loc.obj@graph)
    }
    if (loc.obj@collectTimings) {
        write(sprintf(" *** create node pair table: %f secs", 
            difftime(Sys.time(), start.time, units = "secs")), 
            stderr())
    }
    in.graph.edge.names = unname(cy2.edge.names(loc.obj@graph))
    existing.edge.names = sapply(loc.obj@edge.suid.name.dict, 
        function(n) {
            return(n$name)
        })
    diff.edges = setdiff(in.graph.edge.names, existing.edge.names)
    if (length(diff.edges) > 0) {
        write(sprintf("sending %d edges", nrow(tbl.edges)), stderr())
        source.nodes = tbl.edges$source
        target.nodes = tbl.edges$target
        edge.type = tbl.edges$edgeType
        directed = rep(TRUE, length(source.nodes))
        suid.name.dict.df = data.frame(matrix(unlist(loc.obj@suid.name.dict), 
            nrow = length(loc.obj@suid.name.dict), byrow = TRUE), 
            stringsAsFactors = FALSE)
        colnames(suid.name.dict.df) <- c("name", "SUID")
        source.node.SUIDs = .nodeNameToNodeSUID(loc.obj, source.nodes)
        target.node.SUIDs = .nodeNameToNodeSUID(loc.obj, target.nodes)
        edge.tbl.records = apply(cbind(source.node.SUIDs, target.node.SUIDs, 
            directed, edge.type), MARGIN = 1, FUN = function(r) {
            list(source = unname(r[[1]]), target = unname(r[[2]]), 
                directed = unname(r[[3]]), interaction = unname(r[[4]]))
        })
        edge.tbl.records.JSON = toJSON(edge.tbl.records)
        resource.uri = paste(loc.obj@uri, pluginVersion(loc.obj), 
            "networks", net.SUID, "edges", sep = "/")
        request.res = POST(url = resource.uri, body = edge.tbl.records.JSON, 
            encode = "json")
        request.res.edge.data = fromJSON(rawToChar(request.res$content))
        edge.names = cy2.edge.names(obj@graph)
        edge.names.tbl.records = apply(unname(cbind(unname(t(sapply(request.res.edge.data, 
            unlist))), edge.names)), MARGIN = 1, FUN = function(r) {
            list(SUID = as.numeric(unname(r[[1]])), value = unname(r[[4]]), 
                source.node = as.numeric(unname(r[[2]])), target.node = as.numeric(unname(r[[3]])))
        })
        for (i in 1:length(edge.names.tbl.records)) {
            loc.obj@edge.suid.name.dict[[length(loc.obj@edge.suid.name.dict) + 
                1]] = list(SUID = edge.names.tbl.records[[i]]$SUID, 
                name = edge.names.tbl.records[[i]]$value, source.node = edge.names.tbl.records[[i]]$source.node, 
                target.node = edge.names.tbl.records[[i]]$target.node)
        }
        invisible(request.res)
    }
    else {
        write(sprintf("NOTICE in RCy3::sendEdges():\n\t all %d edges already exist in Cytoscape - nothing new to add >> function returns", 
            length(in.graph.edge.names)), stderr())
        return()
    }
    eval.parent(substitute(obj <- loc.obj))
}
