unit test_metadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, files;

type

  { TTMetadata }

  TTMetadata= class(TTestCase)
  private
    function GetFileSize: Integer;
  published
    procedure ReadWriteMetadataShort;
    procedure ReadWriteMetadataLong;
    procedure ReadWriteMetadataSegments;
  end;

implementation

var
  Str62: String = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz';
  Str64: String = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz_ ';

procedure TTMetadata.ReadWriteMetadataShort;
var
  F: TTestableFileContainer;
  S: array[0..63] of Char;
begin
  DeleteFile('test.dat');
  F := TTestableFileContainer.Create;
  try
    F.Filename := 'test.dat';
    F.Active := True;
    AssertNotNull(F.Metadata);
    F.Metadata.Write(Str64);
    AssertEquals('Metadata.Size',64,F.Metadata.Size);
    AssertEquals('Metadata.Position',64,F.Metadata.Position);
    AssertEquals('Metadata.PageCount',1,F.Metadata.PageCount);
    AssertEquals('FileContainer.PageCount',2,F.PageCount);
    F.Metadata.Position := 0;
    AssertEquals('Metadata.Position',0,F.Metadata.Position);
    F.Metadata.Read(S,64);
    AssertEquals('Metadata.Position',64,F.Metadata.Position);
    AssertEquals('Strings should equal',Str64,S);
    F.Metadata.Truncate(0);
    AssertEquals('Metadata.PageCount',0,F.Metadata.PageCount);
    AssertEquals('FileContainer.PageCount',1,F.PageCount);
  finally
    F.Destroy;
  end;
  AssertEquals('FileSize',4096,GetFileSize);
end;

procedure TTMetadata.ReadWriteMetadataLong;
var
  F: TTestableFileContainer;
  S,R: array[0..8192] of Char;
  X: Integer;
begin
  S := Str64;
  for X := 2 to 128 do
    S := S + Str64;
  S := S+'$';
  F := TTestableFileContainer.Create;
  try
    F.Filename := 'test.dat';
    F.Active := True;
    AssertNotNull(F.Metadata);
    AssertEquals('F.PageCount',1,F.PageCount);
    F.Metadata.Write(S,8193);
    AssertEquals('Metadata.Size',8193,F.Metadata.Size);
    AssertEquals('Metadata.Position',8193,F.Metadata.Position);
    AssertEquals(4,F.PageCount);
    F.Flush;
    F.Metadata.Position := 0;
    AssertEquals('Metadata.Position',0,F.Metadata.Position);
    F.Metadata.Read(R,8193);
    AssertEquals('Metadata.Position',8193,F.Metadata.Position);
    AssertTrue('Strings should equal',CompareStr(S,R)=0);
    AssertEquals(3,F.Metadata.PageCount);
    AssertEquals(4,F.PageCount);
    F.Metadata.Truncate(8192);
    AssertEquals(8192,F.Metadata.Size);
    AssertEquals(2,F.Metadata.PageCount);
    AssertEquals(3,F.PageCount);
    F.Flush;
    AssertEquals(8192,F.Metadata.Size);
    AssertEquals(2,F.Metadata.PageCount);
    AssertEquals(3,F.PageCount);
    F.Metadata.Truncate(4000);
    AssertEquals(4000,F.Metadata.Size);
    AssertEquals(1,F.Metadata.PageCount);
    AssertEquals(2,F.PageCount);
    F.Metadata.Truncate(0);
    AssertEquals(0,F.Metadata.Size);
    AssertEquals(0,F.Metadata.PageCount);
    AssertEquals(1,F.PageCount);
  finally
    F.Free;
  end;
  AssertEquals('FileSize',4096,GetFileSize);
end;

procedure TTMetadata.ReadWriteMetadataSegments;
var
  F: TTestableFileContainer;
  X: Integer;
  C: Cardinal;
  S: array[0..61] of Char;
begin
  F := TTestableFileContainer.Create;
  try
    F.Filename := 'test.dat';
    F.Open;

    for X := 1 to 2560 do
      F.Metadata.Write(PChar(Str62)^,62);

    C := 2560*62 div 4096;
    if 2560 * 62 mod 4096 > 0 then
      inc(C);

    AssertEquals(C+1,F.PageCount);
    F.Flush;

    F.Metadata.Position := 0;
    for X := 1 to 2560 do
      begin
        F.Metadata.Read(S,62);
        AssertEquals(Str62,S);
      end;

    F.Metadata.Truncate(0);
    AssertEquals(1,F.PageCount);
    AssertEquals(0,F.Metadata.Position);
    F.Close;
  finally
    F.Free;
  end;
  AssertEquals('FileSize',4096,GetFileSize);
end;

function TTMetadata.GetFileSize: Integer;
var
  F: File of Byte;
begin
  Assign(F,'test.dat');
  Reset(F);
  Result := FileSize(F);
  Close(F);
end;

initialization
  RegisterTest('files',TTMetadata);
end.

