program passman;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, memdslaz, passmanfm, PassManParamsFM, createpasswordfm
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TPassManForm, PassManForm);
  Application.CreateForm(TParamsForm, ParamsForm);
  Application.CreateForm(TCreatePasswordDialog, CreatePasswordDialog);
  Application.Run;
end.

