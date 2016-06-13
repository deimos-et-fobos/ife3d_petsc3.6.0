#include "metis.h"
#include "petscsys.h"

void metis_nodend_f2c_(idx_t *nvtxs, idx_t *xadj, idx_t *adjncy,
          idx_t *perm, idx_t *iperm) 
{
  idx_t *vwgt=NULL;
  idx_t *options=NULL;

  METIS_NodeND(nvtxs, xadj, adjncy, vwgt, options, perm, iperm); 
}
