program FileContainer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, filecontainerfm, ContainerFM, files, btreetesthelper, HexEditFM,
  texteditfm, pagemapfm, fatfm, progressfm, debuglogfm, passwdfm;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFileContainerForm, FileContainerForm);
  Application.CreateForm(THexEditForm, HexEditForm);
  Application.CreateForm(TTextEditForm, TextEditForm);
  Application.CreateForm(TPageMapForm, PageMapForm);
  Application.CreateForm(TFatInspectorForm, FatInspectorForm);
  Application.CreateForm(TProgressForm, ProgressForm);
  Application.CreateForm(TContainerPropertiesDlg, ContainerPropertiesDlg);
  Application.CreateForm(TPasswordForm, PasswordForm);
  Application.Run;
end.

