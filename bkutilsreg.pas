unit bkutilsreg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  WhatsMyIP, PasswordMan;

procedure Register;

implementation

procedure Register;
begin
  {$I passwordman_icon.lrs}
  {$I whatsmyip_icon.lrs}
  RegisterComponents('Bond',[TPasswordManager,TWhatsMyIP]);
end;

end.
