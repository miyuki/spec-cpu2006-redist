 /*@@
   @header    pughi.h
   @date      Tue Apr 18 21:22:42 2000
   @author    Tom Goodale
   @desc 
   Internal stuff for PUGH
   @enddesc 
   @version $Header: /cactus/CactusPUGH/PUGH/src/include/pughi.h,v 1.11 2001/10/07 10:06:32 goodale Exp $
 @@*/

#ifndef _PUGHI_H_
#define _PUGHI_H_ 1

#ifdef __cplusplus
extern "C" 
{
#endif

pConnectivity *PUGH_SetupConnectivity(int dim, 
                                      int total_procs, 
                                      int *nprocs,
                                      int *perme);

pGExtras *PUGH_SetupPGExtras(int dim, 
                             int *perme,
                             int stagger,
                             int *sh,
                             int *nghosts,
                             int total_procs,
                             int *nprocs,
                             int this_proc);

pComm *PUGH_SetupGArrayGroupComm(pGH *pughGH,
                                 int dim,
                                 int first_var,
                                 int n_vars,
                                 int sync_timelevel,
                                 int vartype,
                                 pGExtras *extras);

void pGH_DumpInfo(pGH *pughGH);
int  PUGH_SetupDefaultTopology(int dim, int *nprocs);
void PUGH_DestroyGArray(pGA **GA);
void PUGH_DestroyComm(pComm **comm);
void PUGH_DestroyConnectivity(pConnectivity **conn);
void PUGH_DestroyPGExtras(pGExtras **PGExtras);
void PUGH_DestroyPGH(pGH **pughGH);

int PUGH_GenerateTopology(int dim, int total_procs, int *nprocs);

int PUGH_GenerateNeighbours(int dim, 
                            int total_procs, 
                            int *nprocs, 
                            int **neighbours, 
                            int *perme);

int PUGH_DecomposeIJK(int dim, int ijk, int *nprocs, int *pos);

int PUGH_ComposeIJK(int dim, 
                    int *nprocs, 
                    int *pos);

int PUGH_SetupPGExtrasMemory(int dim, 
                             int total_procs,
                             int *nprocs,
                             pGExtras *this);

int PUGH_SetupPGExtrasSizes(int dim, 
                            int *perme,
                            int stagger,
                            int *sh,
                            int *nghosts,
                            int total_procs,
                            int *nprocs,
                            int this_proc,
                            pGExtras *this);

int PUGH_SetupPGExtrasOwnership(int dim, 
                               int *perme,
                               int stagger,
                               int *sh,
                               int *nghosts,
                               int total_procs,
                               int *nprocs,
                               int this_proc,
                               pGExtras *this);

int PUGH_SetupBoundingBox(int dim, 
                          int *perme,
                          int stagger,
                          int *sh,
                          int *nghosts,
                          int total_procs,
                          int *nprocs,
                          pGExtras *this);

int PUGH_SetupRemoteSizes(int dim, 
                          int *perme,
                          int stagger,
                          int *sh,
                          int *nghosts,
                          int total_procs,
                          int *nprocs,
                          pGExtras *this);

int PUGH_SetupPGExtrasStaggering(int dim,
                                 int *perme,
                                 int stagger,
                                 int *sh,
                                 int *nghosts,
                                 int total_procs,
                                 int *nprocs,
                                 int this_proc,
                                 pGExtras *this);

pGA *PUGH_SetupGArray(void *parent,
                      pGExtras *extras,
                      pConnectivity *connectivity,
                      pComm *groupcomm,
                      const char *name,
                      int id,
                      int arrayid,
                      int varsize, 
                      int vtype, 
                      int stagger,
                      int vector_size,
                      int vector_entry,
                      pGA *vector_base);

pComm *PUGH_SetupGArrayComm(pGH *pughGH,
                            int dim,
                            int var,
                            int sync_timelevel,
                            int vartype,
                            pGExtras *extras);

int PUGH_EnableGArrayDataStorage(pGA *GA,
                                 int this_proc,
                                 const char *initialize_memory,
                                 int padding_active,
                                 int padding_cacheline_bits,
                                 int padding_size,
                                 int padding_address_spacing);

int PUGH_EnableGArrayComm(pGA *GA,
                          int commflag);

int PUGH_DisableGArrayDataStorage(pGA *GA);

int PUGH_DisableGArrayComm(pGA *GA);

int PUGH_DisableGArrayGroupComm(pGH *pughGH,
                                int first_var,
                                pComm *groupcomm);

int PUGH_SyncGArray(pGA *GA);

#ifdef CCTK_MPI
void PostSendGA     (pGH *pughGH, int dir, pComm *comm);
void PostReceiveGA  (pGH *pughGH, int dir, pComm *comm);
void FinishReceiveGA(pGH *pughGH, int dir, pComm *comm);
#endif

#ifdef __cplusplus
}
#endif

#endif /* defined _PUGHI_H_ */
