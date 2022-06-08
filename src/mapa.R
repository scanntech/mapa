library(dplyr)
library(data.table)

mapa <- function(ventas, proveedor_objetivo, particiones_pdvs, particiones_productos){
  columnas <- c(particiones_pdvs, particiones_productos, "tiene_proveedor")

  rbind(
    ventas[, c("tiene_proveedor", "imp_total_ticket") := .(fifelse(any(proveedor==proveedor_objetivo), 1, 0), sum(imp_vta)), by = "id_ticket"] %>%
      .[, imp_proveedor := fifelse(proveedor == proveedor_objetivo, imp_vta, 0)] %>%
      .[, .(
        importe_total_ticket = sum(imp_total_ticket),
        importe_items_proveedor = sum(imp_proveedor),
        cant_pdvs = uniqueN(pdv_codigo),
        cant_tickets = uniqueN(id_ticket)
      ), by = columnas],
    ventas[, c("tiene_proveedor", "imp_total_ticket") := .(fifelse(any(proveedor==proveedor_objetivo), 1, 0), sum(imp_vta)), by = "id_ticket"] %>%
      .[, imp_proveedor := fifelse(proveedor == proveedor_objetivo, imp_vta, 0)] %>%
      .[, .(
        importe_total_ticket = sum(imp_total_ticket),
        importe_items_proveedor = sum(imp_proveedor),
        cant_pdvs = uniqueN(pdv_codigo),
        cant_tickets = uniqueN(id_ticket)
      ), by = c("tiene_proveedor", particiones_pdvs)] %>%
      .[, c(particiones_productos) := .(rep("TODOS", length(particiones_productos)))]
  )

}
