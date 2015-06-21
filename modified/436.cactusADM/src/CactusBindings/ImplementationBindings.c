#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
int CCTKi_BindingsThorn_BenchADM(void);

int CCTKi_BindingsThorn_Boundary(void);

int CCTKi_BindingsThorn_Cactus(void);

int CCTKi_BindingsThorn_CartGrid3D(void);

int CCTKi_BindingsThorn_Einstein(void);

int CCTKi_BindingsThorn_IDLinearWaves(void);

int CCTKi_BindingsThorn_IOASCII(void);

int CCTKi_BindingsThorn_IOBasic(void);

int CCTKi_BindingsThorn_IOUtil(void);

int CCTKi_BindingsThorn_PUGH(void);

int CCTKi_BindingsThorn_PUGHReduce(void);

int CCTKi_BindingsThorn_PUGHSlab(void);

int CCTKi_BindingsThorn_Time(void);

int CCTKi_BindingsImplementationsInitialise(void)
{

  CCTKi_BindingsThorn_BenchADM();

  CCTKi_BindingsThorn_Boundary();

  CCTKi_BindingsThorn_Cactus();

  CCTKi_BindingsThorn_CartGrid3D();

  CCTKi_BindingsThorn_Einstein();

  CCTKi_BindingsThorn_IDLinearWaves();

  CCTKi_BindingsThorn_IOASCII();

  CCTKi_BindingsThorn_IOBasic();

  CCTKi_BindingsThorn_IOUtil();

  CCTKi_BindingsThorn_PUGH();

  CCTKi_BindingsThorn_PUGHReduce();

  CCTKi_BindingsThorn_PUGHSlab();

  CCTKi_BindingsThorn_Time();


 return 0;
}
