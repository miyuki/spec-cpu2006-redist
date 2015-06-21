#ifdef SPEC_CPU
# define THORN_IS_Einstein
#endif /* SPEC_CPU */
#include <stdio.h>

#include "cctki_ActiveThorns.h"


int CCTKi_BindingsThorn_Einstein(void);

int CCTKi_BindingsThorn_Einstein(void)
{

  int retval;

  const char *name[] = {"Einstein",0};
  const char *implementation[]={"einstein",0};
  const char *ancestors[]=
  {
    "GRID",
    "TIME",
    0,
  };

  const char *friends[]=
  {
    0,
  };

  /* Should be able to do below with a constant initialiser but sr8000 compiler complains
   * So have to laboriously assign values to each member of array.
   */
  struct iAttributeList attributes[5];

  attributes[0].attribute =              "name";
  attributes[0].AttributeData.StringList = name;
  attributes[1].attribute =              "implementation";
  attributes[1].AttributeData.StringList = implementation;
  attributes[2].attribute =              "ancestors";
  attributes[2].AttributeData.StringList = ancestors;
  attributes[3].attribute =              "friends";
  attributes[3].AttributeData.StringList = friends;
  attributes[4].attribute =                0;
  attributes[4].AttributeData.StringList = 0;


  retval = CCTKi_RegisterThorn(attributes);

  return retval;
}
