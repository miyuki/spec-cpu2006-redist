
sub fortran_name
{
  local($old_name) = @_;
  local($new_name);

  $new_name = "\L$old_name\E";
  
  if($new_name =~ m:_: ) 
  {
    $new_name = $new_name."__";
  }
  else
  {
    $new_name = $new_name."_";
  }

  return $new_name;
}



sub fortran_common_name
{
    local($old_name) = @_;
    local($new_name);

    $new_name = "\L$old_name\E";

    if($new_name =~ m:_: ) 
    {
	$new_name = $new_name."__";
    }
    else
    {
	$new_name = $new_name."_";
    }

    return "".$new_name;
}


1;
