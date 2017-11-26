program UnitTests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, rbtree, test_rbtree,
  GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

