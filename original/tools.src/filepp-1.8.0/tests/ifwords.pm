AddKeyword("testif", "Filepp::If");
AddIfword("testif");
AddKeyword("testelse", "Filepp::Else");
AddElseword("testelse");
AddKeyword("testendif", "Filepp::Endif");
AddEndifword("testendif");

RemoveIfword("if");
RemoveElseword("else");
RemoveEndifword("endif");

return 1;
