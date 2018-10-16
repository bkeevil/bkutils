unit bkutilsreg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, PasswordMan;

procedure Register;

implementation

procedure Register;
begin
  {$I passwordman_icon.lrs}
  RegisterComponents('Bond',[TPasswordManager]);
end;

end.
