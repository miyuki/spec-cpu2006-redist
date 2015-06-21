#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      SKBinTree.c
   @date      Mon Oct  5 11:00:01 1998
   @author    Tom Goodale
   @desc 
   Routines to deal with binary trees keyed by strings.
   The tree is a threaded tree, i.e. it can also be
   traversed, inorder, like a linked list.
   @enddesc 
 @@*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "SKBinTree.h"
#include "cctk_Flesh.h"

int STR_cmpi(const char *string1, const char *string2);

#define STR_CMP(a,b) STR_cmpi(a,b)

static const char *rcsid = "$Header: /cactus/Cactus/src/util/SKBinTree.c,v 1.10 2001/06/04 17:22:14 goodale Exp $";

CCTK_FILEVERSION(util_SKBinTree_c)

 /*@@
   @routine    SKTreeStoreData
   @date       Mon Oct  5 11:04:55 1998
   @author     Tom Goodale
   @desc 
   Stores data in string-keyed binary tree.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
t_sktree *SKTreeStoreData(t_sktree *root, t_sktree *subtree, 
                          const char *key, void *data)
{
  int order;
  t_sktree *newsubtree;

  if(!subtree)
  {
    /* Create a new element. */ 
    newsubtree = (t_sktree *)malloc(sizeof(t_sktree));
    if(newsubtree)
    {
      newsubtree->left=NULL;
      newsubtree->right=NULL;
      newsubtree->next=NULL;
      newsubtree->last=NULL;

      newsubtree->data = data;

      newsubtree->key= (char *)malloc(sizeof(char)*(strlen(key)+1));
      strcpy(newsubtree->key, key);
      if(root) 
      {
        if((order = STR_CMP(key, root->key)) < 0)
        {
          root->left = newsubtree;
          newsubtree->next = root;
          newsubtree->last = root->last;
          if(newsubtree->last)
          {
            newsubtree->last->next = newsubtree;
          }

          /*          printf("Added %s, NEXT: %s\n", newsubtree->key, root->key); */
        }
        else
        {
          root->right = newsubtree;
          newsubtree->next = root->next;
          newsubtree->last = root;
          root->next = newsubtree;
          
          /*printf("Added %s, NEXT: %s\n", newsubtree->key, newsubtree->next ? newsubtree->next->key : "(none)");*/
          /*printf("Modified %s NEXT\n", root->key);*/
        }

      }
    }
  }
  else
  {
    /* Go down left or right branch. */
    if((order = STR_CMP(key, subtree->key)) < 0)
    {
      newsubtree = SKTreeStoreData(subtree, subtree->left, key, data);        
    }
    else if(order > 0) 
    {
      newsubtree = SKTreeStoreData(subtree, subtree->right, key, data);
    }
    else
    {
      /* Duplicate key. */
      newsubtree = NULL;
    }

  }

  /* Return either the new node, or NULL if either a duplicate value or
   * memory failure.
   */

  return newsubtree;

}

 /*@@
   @routine    SKTreeTraverseInorder
   @date       Mon Oct  5 11:05:54 1998
   @author     Tom Goodale
   @desc 
   Traverse a tree 'inorder'
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int SKTreeTraverseInorder(t_sktree *root, int (*process)(const char *, void *, void *), void *info)
{
  int terminate;

  terminate = 0;

  if(root)
  {
    terminate = SKTreeTraverseInorder(root->left, process, info);
    if(!terminate) terminate = process(root->key,root->data,info);
    if(!terminate) terminate = SKTreeTraverseInorder(root->right, process, info);
  }

  return terminate;
}

 /*@@
   @routine    SKTreeTraversePreorder
   @date       Mon Oct  5 11:05:54 1998
   @author     Tom Goodale
   @desc 
   Traverse a tree 'preorder'
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int SKTreeTraversePreorder(t_sktree *root, int (*process)(const char *,void *, void *), void *info)
{
  int terminate;

  terminate = 0;

  if(root)
  {
    terminate = process(root->key,root->data, info);
    if(!terminate) terminate = SKTreeTraversePreorder(root->left, process, info);
    if(!terminate) terminate = SKTreeTraversePreorder(root->right, process,info);
  }

  return terminate;
}
                     
 /*@@
   @routine    SKTreeTraversePostorder
   @date       Mon Oct  5 11:05:54 1998
   @author     Tom Goodale
   @desc 
   Traverse a tree 'postorder'
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int SKTreeTraversePostorder(t_sktree *root, int (*process)(const char *, void *, void *), void *info)
{
  int terminate;

  terminate = 0;

  if(root)
  {
    terminate = SKTreeTraversePostorder(root->left, process, info);
    if(!terminate) terminate = SKTreeTraversePostorder(root->right, process, info);
    if(!terminate) terminate = process(root->key,root->data, info);
  }

  return terminate;
}

 /*@@
   @routine    SKTreePrintNodes
   @date       Mon Oct  5 11:06:52 1998
   @author     Tom Goodale
   @desc 
   Allows a binary tree to be printed out.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
void SKTreePrintNodes(t_sktree *root, int depth, void (*print_node)(const char *,void *, int))
{
  if(root)
  {
    SKTreePrintNodes(root->left, depth+1,print_node);
    print_node(root->key,root->data,depth);
    SKTreePrintNodes(root->right, depth+1, print_node);
  }
}

void SKTreeDebugNodes(t_sktree *root, int depth)
{
  if(root)
  {
    SKTreeDebugNodes(root->left, depth+1);

    printf("KEY:   %s\n", root->key);
    root->left  ? printf("LEFT:  %s\n", root->left->key)  : printf("LEFT:  (none)\n");
    root->right ? printf("RIGHT: %s\n", root->right->key) : printf("RIGHT: (none)\n");
    root->next  ? printf("NEXT:  %s\n", root->next->key)  : printf("NEXT: (none)\n");

    SKTreeDebugNodes(root->right, depth+1);
  }
}


 /*@@
   @routine    SKTreeFindNode
   @date       Mon Jul  5 10:09:30 1999
   @author     Tom Goodale
   @desc 
   Finds a given node in the tree.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
t_sktree *SKTreeFindNode(t_sktree *root, const char *key)
{
  int order;

  t_sktree *node;

  if(root)
  {
    /* Go down left or right branch. */
    if((order = STR_CMP(key, root->key)) < 0)
    {
      node = SKTreeFindNode(root->left, key);
    }
    else if(order > 0) 
    {
      node = SKTreeFindNode(root->right, key);
    }
    else
    {
      /* Found it. */
      node = root;
    }
  }
  else
  {
    node = NULL;
  }

  return node;
}  

 /*@@
   @routine    SKTreeFindFirst
   @date       Mon Jul  5 10:09:57 1999
   @author     Tom Goodale
   @desc 
   Finds the first node in the tree (the leftmost one).
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
t_sktree *SKTreeFindFirst(t_sktree *root)
{
  for(; root->left ; root = root->left);

  return root;
}




 /*@@
   @routine    STR_cmpi
   @date       Mon Jul  5 01:19:00 1999
   @author     Tom Goodale
   @desc 
   Case independent strcmp
   @enddesc 
   @calls     
   @calledby   
   @history 
   @hdate Wed Oct 13 15:30:57 1999 @hauthor Tom Goodale
   @hdesc Checks the length of the two string first. 
   @endhistory 

@@*/
int STR_cmpi(const char *string1, const char *string2)
{
  int retval;
  int position;

  retval = 0;

  if(! retval)
  {
    for(position = 0; 
        string1[position] && string2[position];
        position++)
    {
      if((retval = (tolower(string1[position]) - tolower(string2[position]))))
      {
        break;
      }
    }
  }

  if(! retval)
  {
    retval = strlen(string1) - strlen(string2);
  }


  return retval;
}
