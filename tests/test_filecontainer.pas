unit test_filecontainer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, files;

type

  { TTFileContainer }

  TTFileContainer= class(TTestCase)
  private
    function GetFileSize: Integer;
  published
    procedure CreateDestroyFileContainer;
    procedure CreateNewFileContainer;
    procedure OpenExistingFileContainer;
  end;

implementation

procedure TTFileContainer.CreateDestroyFileContainer;
var
  F: TFileContainer;
begin
  F := TFileContainer.Create;
  F.Free;
end;

procedure TTFileContainer.CreateNewFileContainer;
var
  F: TTestableFileContainer;
begin
  DeleteFile('test.dat');
  F := TTestableFileContainer.Create;
  try
    F.Filename := 'test.dat';
    F.Active := True;
    AssertTrue('Active',F.Active);
    AssertEquals('PageCount',1,F.PageCount);
    F.Active := False;
  finally
    F.Free;
  end;
  AssertEquals('FileSize',4096,GetFileSize);
end;

procedure TTFileContainer.OpenExistingFileContainer;
var
  F: TTestableFileContainer;
begin
  CreateNewFileContainer;
  F := TTestableFileContainer.Create;
  try
    F.Filename := 'test.dat';
    F.Active := True;
    AssertTrue('Active',F.Active);
    AssertEquals('PageCount',1,F.PageCount);
    F.Active := False;
  finally
    F.Free;
  end;
  AssertEquals('FileSize',4096,GetFileSize);
end;

function TTFileContainer.GetFileSize: Integer;
var
  F: File of Byte;
begin
  Assign(F,'test.dat');
  Reset(F);
  Result := FileSize(F);
  Close(F);
end;

initialization
  RegisterTest('files',TTFileContainer);
end.

