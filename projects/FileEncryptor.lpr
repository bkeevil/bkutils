program FileEncryptor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fileencryptorfm, securityfm, files,
  progressfm, passwdfm;

{$R *.res}

begin
  Application.Title:='Encrypted File Storage';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFileContainerForm, FileContainerForm);
  Application.CreateForm(TProgressForm, ProgressForm);
  Application.CreateForm(TSecurityDlg, SecurityDlg);
  Application.CreateForm(TPasswordForm, PasswordForm);
  Application.Run;
end.

