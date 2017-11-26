unit test_streamfilehandle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, files;

type

  { TTStreamFileHandle }

  TTStreamFileHandle= class(TTestCase)
  private
    function GetFileSize: Integer;
  protected
    F: TTestableFileContainer;
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure CreateDestroyFile;
    procedure ReadWriteShort;
    procedure ReadWriteLong;
    procedure ReadWriteSegments;
  end;

implementation

var
  Str62: String = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz';

procedure TTStreamFileHandle.CreateDestroyFile;
var
  A: TStreamFileHandle;
begin
  A := F.CreateFile('A') as TStreamFileHandle;
  AssertEquals(0,A.Size);
  AssertEquals(0,A.Position);
  A.Free;
end;

procedure TTStreamFileHandle.ReadWriteShort;
var
  A: TStreamFileHandle;
  S: String;
begin
  A := F.CreateFile('A') as TStreamFileHandle;
  try
    A.Write(Str62);
    AssertEquals(62,A.Size);
    AssertEquals(62,A.Position);
    AssertEquals(1,A.PageCount);
    A.Write(Str62);
    AssertEquals(124,A.Size);
    AssertEquals(124,A.Position);
    A.Seek(0);
    AssertEquals(0,A.Position);
    S := A.Read(62);
    AssertEquals(Str62,S);
    AssertEquals(62,A.Position);
    S := A.Read(62);
    AssertEquals(Str62,S);
    AssertEquals(124,A.Position);
    AssertEquals(124,A.Size);
    A.Truncate(62);
    AssertEquals(62,A.Position);
    AssertEquals(62,A.Size);
    A.Seek(0);
    S := A.Read(62);
    AssertEquals(Str62,S);
    AssertEquals(62,A.Position);
    AssertEquals(1,A.PageCount);
    A.Flush;
    A.Truncate(0);
    AssertEquals(0,A.Position);
    AssertEquals(0,A.Size);
    AssertEquals(0,A.PageCount);
    A.Write(Str62+Str62+Str62);
    AssertEquals(62*3,A.Position);
    AssertEquals(62*3,A.Size);
  finally
    A.Free;
  end;
end;

procedure TTStreamFileHandle.ReadWriteLong;
var
  A: TStreamFileHandle;
  S: String;
  P: array[0..8245] of Char;
  X: Integer;
begin
  A := F.CreateFile('A') as TStreamFileHandle;
  try
    S := Str62;
    for X := 1 to 132 do
      S := S + Str62;
    P := S;
    A.Write(P,8246);
    A.Flush;
    AssertEquals(8246,A.Position);
    AssertEquals(8246,A.Size);
    AssertEquals(3,A.PageCount);
    AssertEquals(5,F.PageCount);
    A.Position := 0;
    AssertEquals(0,A.Position);
    A.Read(P,8246);
    AssertTrue('Strings should equal',CompareStr(S,P)=0);
    A.Truncate(8192);
    AssertEquals('File.FileSize',8192,A.Size);
    AssertEquals('File.PageCount',2,A.PageCount);
    AssertEquals('FileContainer.PageCount',4,F.PageCount);
    A.Truncate(4096);
    AssertEquals('File.FileSize',4096,A.Size);
    AssertEquals('File.PageCount',1,A.PageCount);
    AssertEquals('FileContainer.PageCount',3,F.PageCount);
    A.Truncate(0);
    AssertEquals('File.FileSize',0,A.Size);
    AssertEquals('File.PageCount',0,A.PageCount);
    AssertEquals('FileContainer.PageCount',1,F.PageCount);
  finally
    A.Free;
  end;
end;

procedure TTStreamFileHandle.ReadWriteSegments;
var
  A: TStreamFileHandle;
  X: Integer;
  C: Cardinal;
  S: String;
begin
  A := F.CreateFile('A') as TStreamFileHandle;
  try

    for X := 1 to 25600 do
      A.Write(Str62);

    C := 25600*62 div 4096;
    if 25600 * 62 mod 4096 > 0 then
      inc(C);

    AssertEquals(C,A.PageCount);
    AssertEquals(C+1,F.PageCount);
    F.Close;
    F.Open;
    AssertEquals(C+3,F.PageCount);
    A := F.CreateFile('A') as TStreamFileHandle;
    AssertEquals(C,A.PageCount);
    for X := 1 to 25600 do
      begin
        S := A.Read(62);
        AssertEquals(Str62,S);
      end;
    A.Truncate(0);
    AssertEquals(0,A.Position);
    AssertEquals(0,A.PageCount);
    F.Pack;
    AssertEquals(3,F.PageCount);
  finally
    A.Free;
  end;
end;

procedure TTStreamFileHandle.SetUp;
begin
  DeleteFile('test.dat');
  F := TTestableFileContainer.Create;
  F.Filename := 'test.dat';
  F.Active := True;
end;

procedure TTStreamFileHandle.TearDown;
begin
  F.Close;
  F.Free;
end;

function TTStreamFileHandle.GetFileSize: Integer;
var
  FL: File of Byte;
begin
  Assign(FL,'test.dat');
  Reset(FL);
  Result := FileSize(FL);
  Close(FL);
end;

initialization
  RegisterTest('files',TTStreamFileHandle);
end.

