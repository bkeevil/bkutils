unit test_fileinfos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, files, testregistry;

type

  TTFileInfos= class(TTestCase)
  protected
    F: TTestableFileContainer;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test;
  end;

implementation

procedure TTFileInfos.Test;
begin
  AssertEquals(-1,F.FileInfos.Find('Filename1'));
  AssertNotNull(F.FileInfos.Insert('Filename1',F.FileInfos.GetID));
  AssertNotNull(F.FileInfos.Insert('Filename2',2));
  AssertNotNull(F.FileInfos.Insert('Filename3',F.FileInfos.GetID));
  AssertNotNull(F.FileInfos.Insert('Filename4',4));
  AssertNotNull(F.FileInfos.Insert('Filename5',F.FileInfos.GetID));
  AssertEquals(5,F.FileInfos.Count);
  AssertEquals(0,F.FileInfos.Find('Filename1'));
  AssertEquals(1,F.FileInfos.Find('Filename2'));
  AssertEquals(2,F.FileInfos.Find('Filename3'));
  AssertEquals(3,F.FileInfos.Find('Filename4'));
  AssertEquals(4,F.FileInfos.Find('Filename5'));
  AssertEquals(-1,F.FileInfos.Find('Filename6'));
  AssertFalse(F.FileInfos.Remove('Filename0'));
  AssertTrue(F.FileInfos.Remove('Filename1'));
  AssertTrue(F.FileInfos.Remove('Filename5'));
  AssertTrue(F.FileInfos.Remove('Filename2'));
  AssertTrue(F.FileInfos.Remove('Filename4'));
  AssertTrue(F.FileInfos.Remove('Filename3'));
  AssertFalse(F.FileInfos.Remove('Filename6'));
end;

procedure TTFileInfos.SetUp;
begin
  F := TTestableFileContainer.Create;
end;

procedure TTFileInfos.TearDown;
begin
  F.Free;
end;

initialization
  RegisterTest('files',TTFileInfos);
end.
