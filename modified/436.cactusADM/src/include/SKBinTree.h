 /*@@
   @header    BinaryTree.h
   @date      Mon Oct  5 11:01:20 1998
   @author    Tom Goodale
   @desc 
   Prototypes and data definitions for binary tree routines.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/SKBinTree.h,v 1.5 2001/06/04 17:22:13 goodale Exp $
 @@*/

#ifndef _SKBINTREE_H_
#define _SKBINTREE_H_

typedef struct T_SKTREE
{
  struct T_SKTREE *left;
  struct T_SKTREE *right;
  struct T_SKTREE *next;
  struct T_SKTREE *last;
  char *key;

  void *data;
} t_sktree;

#ifdef _cplusplus
extern "C" {
#endif

t_sktree *SKTreeStoreData(t_sktree *root, 
                          t_sktree *subtree, 
                          const char *key, 
                          void *data);

int SKTreeTraverseInorder(t_sktree *root, int (*process)(const char *,void *, void *), void *info);

int SKTreeTraversePreorder(t_sktree *root, int (*process)(const char *,void *, void *), void *info);

int SKTreeTraversePostorder(t_sktree *root, int (*process)(const char *,void *, void *), void *info);

void SKTreePrintNodes(t_sktree *root, int depth, void (*print_node)(const char *,void *, int));

t_sktree *SKTreeFindNode(t_sktree *root, const char *key);

t_sktree *SKTreeFindFirst(t_sktree *root);

void SKTreeDebugNodes(t_sktree *root, int depth);

#ifdef _cplusplus
}
#endif

#endif
