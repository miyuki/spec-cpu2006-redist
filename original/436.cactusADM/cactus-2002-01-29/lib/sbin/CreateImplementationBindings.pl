#/*@@
#  @file      CreateImplementationBindings.pl
#  @date      Sun Jul  4 17:09:54 1999
#  @author    Tom Goodale
#  @desc 
#  
#  @enddesc 
#@@*/

sub CreateImplementationBindings
{
  my($bindings_dir, $rhparameter_db, $rhinterface_db) = @_;
  my($start_dir);
  my($thorn);
  my(@data);

  if(! $build_dir)
  {
    $build_dir = "$bindings_dir/build";
  }

  if(! -d $bindings_dir)
  {
    mkdir("$bindings_dir", 0755) || die "Unable to create $bindings_dir";
  }
  $start_dir = `pwd`;

  chdir $bindings_dir;

  if(! -d "Implementations")
  {
    mkdir("Implementations", 0755) || die "Unable to create Implementations directory";
  }

  if(! -d "include")
  {
    mkdir("include", 0755) || die "Unable to create include directory";
  }

  chdir "Implementations";

  @data = ();

  foreach $thorn (sort split(" ", $rhinterface_db->{"THORNS"}))
  {
    push(@data, "int CCTKi_BindingsThorn_$thorn(void);\n") 
  }

  push(@data, "int CCTKi_BindingsImplementationsInitialise(void)\n{\n");

  foreach $thorn (sort split(" ", $rhinterface_db->{"THORNS"}))
  {
    push(@data, "  CCTKi_BindingsThorn_$thorn();\n") 
  }

  push(@data, "\n return 0;\n}\n");

  &OutputFile(".", "ImplementationBindings.c", @data);


  $dataout = "";
  $dataout .= "\n";
  $dataout .= "SRCS = ImplementationBindings.c\n\n";
 
  &WriteFile("make.code.defn",\$dataout);

  if(! -d "$build_dir")
  {
    mkdir("$build_dir", 0755) || die "Unable to create $build_dir";
  }
  
  chdir "$build_dir";

  foreach $thorn (sort split(" ", $rhinterface_db->{"THORNS"}))
  {

    if(! -d "$thorn")
    {
      mkdir("$thorn", 0755) || die "Unable to create $build_dir/$thorn";
    }
  
    chdir "$thorn";

    $myimp = $rhinterface_db->{"\U$thorn\E IMPLEMENTS"};

    @data = ();

    push(@data, "#include <stdio.h>\n");
    push(@data, "#include \"cctki_ActiveThorns.h\"\n\n");

   push(@data, "int CCTKi_BindingsThorn_${thorn}(void);\n");
   push(@data, "int CCTKi_BindingsThorn_${thorn}(void)\n{\n");

    push(@data, "  int retval;\n");

    push(@data, "  const char *name[] = {\"$thorn\",0};");
    push(@data, "  const char *implementation[]={\"$myimp\",0};");

    push(@data, "  const char *ancestors[]=\n  {");
    foreach $ancestor (split(" ",$rhinterface_db->{"IMPLEMENTATION \U$myimp\E ANCESTORS"}))
    {
      push(@data, "    \"$ancestor\",");
    }
    push(@data, "    0,");
    push(@data, "  };\n");

    # Just pass the ones this thorn has declared itself to be friends with.
    push(@data, "  const char *friends[]=\n  {");
    foreach $friend (split(" ",$rhinterface_db->{"\U$thorn\E FRIEND"}))
    {
      push(@data, "    \"$friend\",");
    }
    push(@data, "    0,");
    push(@data, "  };\n");

    push(@data, "  /* Should be able to do below with a constant initialiser but sr8000 compiler complains");
    push(@data, "   * So have to laboriously assign values to each member of array.");
    push(@data, "   */");
    push(@data, "  struct iAttributeList attributes[5];");
    push(@data, "");
    push(@data, "  attributes[0].attribute =              \"name\";");
    push(@data, "  attributes[0].AttributeData.StringList = name;");
    push(@data, "  attributes[1].attribute =              \"implementation\";");
    push(@data, "  attributes[1].AttributeData.StringList = implementation;");
    push(@data, "  attributes[2].attribute =              \"ancestors\";");
    push(@data, "  attributes[2].AttributeData.StringList = ancestors;");
    push(@data, "  attributes[3].attribute =              \"friends\";");
    push(@data, "  attributes[3].AttributeData.StringList = friends;");
    push(@data, "  attributes[4].attribute =                0;");
    push(@data, "  attributes[4].AttributeData.StringList = 0;");
    push(@data, "\n");

    push(@data, "  retval = CCTKi_RegisterThorn(attributes);");

    push(@data, "\n  return retval;\n}\n");

    &OutputFile(".", "cctk_ThornBindings.c", @data);


    $dataout = "";
    $dataout .= "\n";
    $dataout .= "SRCS = cctk_ThornBindings.c\n\n";
 
    &WriteFile("make.code.defn",\$dataout);

    chdir "..";
  }
    
}

1;
