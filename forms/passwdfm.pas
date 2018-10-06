unit PasswdFM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TPasswordForm }

  TPasswordForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Password: TEdit;
    LBPassword: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  PasswordForm: TPasswordForm;

function GetPassword(var Password: String): Boolean;

implementation

function GetPassword(var Password: String): Boolean;
begin
  Result := False;
  PasswordForm.Password.Text := '';
  PasswordForm.ActiveControl := PasswordForm.Password;
  if PasswordForm.ShowModal = mrOK then
    begin
      Password := PasswordForm.Password.Text;
      Result := Password > '';
    end
  else
    begin
      Password := '';
      Result := False;
    end;
end;

{$R *.lfm}

end.

