#if 0

not used at present

#if defined(__GNUC__)
#define HAVE_LONG_LONG_INT
#endif

#if ! defined(STD_API)
 #undef STD_API
 #ifdef _BUILD_STD
  #define STD_API __declspec(dllexport)
 #else
  #define STD_API __declspec(dllimport)
 #endif
 #define XML_API STD_API
#endif

#endif
