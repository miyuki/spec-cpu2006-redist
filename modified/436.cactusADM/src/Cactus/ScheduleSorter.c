#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      ScheduleSorter.c
   @date      Mon Aug 30 11:36:35 1999
   @author    Tom Goodale
   @desc 
   Sorter for scheduled routines.
   @enddesc 
   @version $Header: /cactus/Cactus/src/schedule/ScheduleSorter.c,v 1.8 2001/11/05 14:58:55 tradke Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>

#include "cctk_Flesh.h"
#include "Schedule.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/schedule/ScheduleSorter.c,v 1.8 2001/11/05 14:58:55 tradke Exp $";

CCTK_FILEVERSION(schedule_ScheduleSorter_c)


/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

static void ScheduleSwap(int size, signed char **array, int *order, int row, int column);

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/


 /*@@
   @routine    CCTKi_ScheduleSort
   @date       Mon Aug 30 11:44:35 1999
   @author     Tom Goodale
   @desc 
   Sorts the array into sort order
   @enddesc 
   @calls     ScheduleSwap
   @calledby   
   @history 
 
   @endhistory 
   @var     size
   @vdesc   size of array
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     array
   @vdesc   The schedule array
   @vtype   signed char **
   @vio     inout
   @vcomment 
 
   @endvar 
   @var     order
   @vdesc   the sort order
   @vtype   int *
   @vio     inout
   @vcomment 
 
   @endvar 
   @returntype int
   @returndesc 
   0 - success
   @endreturndesc
@@*/

int CCTKi_ScheduleSort(int size, signed char **array, int *order)
{
  int iter;
  int row, column;
  int retval;

  retval = 0;

  for(iter=0; iter < size*(size-1)/2; iter++)
  {

    /* Search for the first +ve entry in the matrix */
    for(row = 0; row < size; row++)
    {
      for(column = row+1; column < size ; column++)
      {
        if(array[row][column] > 0) break;
      }
      if(column < size && array[row][column] > 0) break;
    }
    
    /* If beyond end of matrix, must be finished */
    if(row >= size) break;

    /* Swap the rows and columns */
    ScheduleSwap(size, array, order, row, column);

  }

  /* Search for +ve entries in the matrix */
  for(row = 0; row < size; row++)
  {
    for(column = row+1; column <size ; column++)
    {
        if(array[row][column] > 0) retval -= 1;
    }
  }

  return retval;
}

 /*@@
   @routine    CCTKi_ScheduleAddRow
   @date       Wed Sep 15 22:28:09 1999
   @author     Tom Goodale
   @desc 
   Adds a row to the scheduling array, and fills in the corresponding column entries.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     size
   @vdesc   size of array
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     array
   @vdesc   schedule array
   @vtype   signed char **
   @vio     inout
   @vcomment 
 
   @endvar 
   @var     item
   @vdesc   location in array
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     thisorders
   @vdesc   the relative sort order list of this item
   @vtype   int *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc 
   0 - success
   @endreturndesc
@@*/
int CCTKi_ScheduleAddRow(int size, 
                         signed char **array, 
                         int *order, 
                         int item, 
                         int *thisorders)
{
  int retval;

  int row;
  int column;

  retval = 0;

  order[item]=item;

  row = item;

  for(column=0; column < size; column++)
  {
    if(thisorders[column])
    {
      if(array[row][column] && array[row][column] != thisorders[column]) retval--;
      array[row][column] =   thisorders[column];
      array[column][row] = - thisorders[column];
    }
  }
    
  return retval;
}

 /*@@
   @routine    CCTKi_ScheduleCreateArray
   @date       Wed Sep 15 22:28:50 1999
   @author     Tom Goodale
   @desc 
   Creates a scheduling array.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     size
   @vdesc   the size of the array to be created
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype signed char **
   @returndesc 
   the new schedule array or NULL on memory failure
   @endreturndesc
@@*/
signed char **CCTKi_ScheduleCreateArray(int size)
{
  int i, j;
  signed char **array;

  array = (signed char **)malloc(size*sizeof(signed char *));

  if(array)
  {
    for(i=0; i < size; i++)
    {
      array[i] = (signed char *)malloc(size*sizeof(signed char));
      if(!array[i]) break;
    }

    /* Check for errors */
    if(i < size)
    {
      /* Free already allocated memory */
      for(i--; i >=0; i--)
      {
        free(array[i]);
      }
      free(array);
      array = NULL;
    }
  }

  /* Initialise all entries to zero. */
  if(array)
  {
    for(i=0; i < size; i++)
    {
      for(j=0; j < size; j++)
      {
        array[i][j] = 0;
      }
    }
  }

  return array;
}

 /*@@
   @routine    CCTKi_ScheduleDestroyArray
   @date       Wed Sep 15 22:29:10 1999
   @author     Tom Goodale
   @desc 
   Destroys a scheduling array.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     size
   @vdesc   the size of the array
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     array
   @vdesc   the schedule array
   @vtype   signed char **
   @vio     inout
   @vcomment 
 
   @endvar 

@@*/
void CCTKi_ScheduleDestroyArray(int size, signed char **array)
{
  int i;

  if(array)
  {
    for(i=size-1; i >=0; i--)
    {
      free(array[i]);
    }
    free(array);
  }
}

 /*@@
   @routine    CCTKi_ScheduleCreateIVec
   @date       Wed Sep 15 22:29:57 1999
   @author     Tom Goodale
   @desc 
   Creates a vector of integers.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     size
   @vdesc   Size of vector
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int *
   @returndesc 
   the new integer vector or NULL on memory failure
   @endreturndesc
@@*/
int *CCTKi_ScheduleCreateIVec(int size)
{
  int i;
  int *vector;

  vector = (int *)malloc(size*sizeof(int));

  if(vector)
  {
    for(i=0; i < size; i++)
    {
      vector[i] = 0;
    }
  }

  return vector;
}

 /*@@
   @routine    CCTKi_ScheduleDestroyIVec
   @date       Wed Sep 15 22:29:29 1999
   @author     Tom Goodale
   @desc 
   Destroys a vector of integers.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     size
   @vdesc   size of the vector
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     vector
   @vdesc   the vector
   @vtype   int *
   @vio     inout
   @vcomment 
 
   @endvar 

@@*/
void CCTKi_ScheduleDestroyIVec(int size, int *vector)
{
  size = size;
  free(vector);
}

 /*@@
   @routine    ScheduleSwap
   @date       Wed Sep 15 22:26:50 1999
   @author     Tom Goodale
   @desc 
   Swaps two rows and columns in the scheduling array.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     size
   @vdesc   The size of the problem
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     array
   @vdesc   the sort array
   @vtype   signed char **
   @vio     inout
   @vcomment 
 
   @endvar 
   @var     order
   @vdesc   the order of routines
   @vtype   int *
   @vio     inout
   @vcomment 
 
   @endvar 
   @var     row
   @vdesc   the row to swap
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     column
   @vdesc   the column to swap
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 

@@*/
static void ScheduleSwap(int size, signed char **array, int *order, int row, int column)
{
  signed char *tmp;
  signed char tmp_char;
  int tmp_int;
  int this_row;

  /* Swap the rows */
  tmp = array[row];
  array[row] = array[column];
  array[column] = tmp;

  /* Swap the columns */
  for(this_row = 0; this_row < size; this_row++)
  {
    tmp_char = array[this_row][column];
    array[this_row][column] = array[this_row][row];
    array[this_row][row]=tmp_char;
  }

  /* Swap routine orders */
  tmp_int = order[column];
  order[column]=order[row];
  order[row] = tmp_int;

}

/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

/********************************************************************
 ********************************************************************
 ********************************************************************/

#ifdef TEST_SORTER
int main(int argc, char *argv[])
{
  int i, j;
  int size;
  float weight;
  int *order;
  int errcode;
  signed char **array;
  signed char val;

  if(argc < 2)
  {
    printf("usage: %s size [wieght]\n", argv[0]);
    exit(0);
  }

  size = atoi(argv[1]);

  if(argc > 2)
  {
    weight = atof(argv[2]);
  }
  else
  {
    weight = 3.0;
  }

  if(weight <= 1)
  {
    fprintf(stderr, "Weight must be greater than 1 !  Resetting to 3\n");
    weight = 3.0;
  }
  
  if(size < 1)
  {
    fprintf(stderr, "size must be 1 or more, setting to 5\n");
    size = 5;
  }
  
  order = CCTKi_ScheduleCreateIVec(size);

  array = CCTKi_ScheduleCreateArray(size);

  for(i=0; i < size; i++)
  {
    order[i] = i;
  }

  /* Populate the array */
  for(i=0; i < size; i++)
  {
    for(j=i; j < size; j++)
    {
      if(i==j)
      {
        array[i][i]=0;
      }
      else
      {

        val = (signed char)((int)(weight*rand()/(RAND_MAX+1.0))-2);

        /* Normalise */
        if(val) val /= abs(val);

        array[i][j] = val;
        array[j][i] = -val;
      }
    }
  }

  printf("Initial array is...\n");
  for(i=0; i < size; i++)
  {
    for(j=0; j < size; j++)
    {
      printf("  %d", (int)array[i][j]);
    }

    printf("\n");
  }

  printf("Initial order is...\n");
  for(i=0; i < size; i++)
  {
    printf("  %d", order[i]);
  }
  printf("\n");
  
  printf("Sorting array...\n");

  errcode = CCTKi_ScheduleSort(size, array, order);

  if(errcode)
  {
    fprintf(stderr, "Schedule sort failed with error code %d\n", errcode);
  }

  printf("Final array is...\n");
  for(i=0; i < size; i++)
  {
    for(j=0; j < size; j++)
    {
      printf("  %d", (int)array[i][j]);
    }

    printf("\n");
  }

  printf("Final order is...\n");
  for(i=0; i < size; i++)
  {
    printf("  %d", order[i]);
  }
  printf("\n");

  printf("\n All done.\n");

  return 0;
}
#endif
