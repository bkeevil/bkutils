unit test_fileoperations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, files;

type

  TByteArray = array[0..PAGE_SIZE - 1] of Byte;
  { TTFileOperations }

  TTFileOperations= class(TTestCase)
  private
    procedure CheckData(var Data: TByteArray; C: Char);
    procedure CheckFile(Filename: String);
    procedure CheckPageFile(Filename: String);
    procedure CheckStream(S: TStream; Filename: String);
    procedure CreateFile(Filename: String);
    procedure CreatePageFile(Filename: String);
    procedure FillStream(S: TStream; C: Char);
    //procedure DeleteFileAAAA;
  protected
    F: TTestableFileContainer;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CreateOpenSingleStream;
    procedure CreateOpenSinglePageFile;
    procedure CreateOpenMultiStream;
    procedure CreateOpenMultiPageFile;
    procedure TestRenameFile;
    procedure TestDeleteFile;
    //procedure DeleteOpenFile;        // Works but causes an exception to be thrown as expected
    procedure TestImportExportFile;
  end;

implementation

{
procedure ImportFile(Stream: TStream; Filename: String);
procedure ExportFile(Stream: TStream; Filename: String);
}

procedure TTFileOperations.CreateOpenSingleStream;
begin
  CreateFile('AAAA');
  AssertEquals(6,F.PageCount);
  AssertTrue(F.FileExists('AAAA'));
  AssertFalse(F.FileExists('CCCC'));
  CheckFile('AAAA');
  AssertEquals(6,F.PageCount);
  F.Close;
  Sleep(50);
  F.Open;
  CheckFile('AAAA');
  AssertEquals(7,F.PageCount);
end;

procedure TTFileOperations.CreateOpenSinglePageFile;
begin
  CreatePageFile('AAAA');
  AssertEquals(6,F.PageCount);
  AssertTrue(F.FileExists('AAAA'));
  AssertFalse(F.FileExists('CCCC'));
  CheckPageFile('AAAA');
  AssertEquals(6,F.PageCount);
  F.Close;
  Sleep(50);
  F.Open;
  CheckPageFile('AAAA');
  AssertEquals(7,F.PageCount);
end;

procedure TTFileOperations.CreateOpenMultiStream;
begin
  CreateFile('AAAA');
  AssertEquals(6,F.PageCount);
  CreateFile('BBBB');
  AssertEquals(11,F.PageCount);
  CreateFile('CCCC');
  AssertEquals(16,F.PageCount);
  CreateFile('DDDD');
  AssertEquals(21,F.PageCount);
  //
  CheckFile('AAAA');
  CheckFile('BBBB');
  CheckFile('CCCC');
  CheckFile('DDDD');
  //
  F.Close;
  Sleep(50);
  F.Open;
  //
  AssertEquals(22,F.PageCount);   // Includes a page of metadata now.
  CheckFile('AAAA');
  CheckFile('BBBB');
  CheckFile('CCCC');
  CheckFile('DDDD');
end;

procedure TTFileOperations.CreateOpenMultiPageFile;
begin
  CreatePageFile('AAAA');
  AssertEquals(6,F.PageCount);
  CreatePageFile('BBBB');
  AssertEquals(11,F.PageCount);
  CreatePageFile('CCCC');
  AssertEquals(16,F.PageCount);
  CreatePageFile('DDDD');
  AssertEquals(21,F.PageCount);
  //
  CheckPageFile('AAAA');
  CheckPageFile('BBBB');
  CheckPageFile('CCCC');
  CheckPageFile('DDDD');
  //
  F.Close;
  Sleep(50);
  F.Open;
  //
  AssertEquals(22,F.PageCount);   // Includes a page of metadata now.
  CheckPageFile('AAAA');
  CheckPageFile('BBBB');
  CheckPageFile('CCCC');
  CheckPageFile('DDDD');
end;

procedure TTFileOperations.TestRenameFile;
begin
  CreateFile('AAAA');
  CreateFile('BBBB');
  CreateFile('CCCC');
  CreateFile('DDDD');
  //
  AssertTrue(F.RenameFile('AAAA','1111'));
  AssertTrue(F.RenameFile('BBBB','2222'));
  AssertTrue(F.RenameFile('CCCC','3333'));
  AssertTrue(F.RenameFile('DDDD','4444'));
  AssertEquals(4,F.FileInfos.Count);
  //
  AssertTrue(F.FileExists('1111'));
  AssertTrue(F.FileExists('2222'));
  AssertTrue(F.FileExists('3333'));
  AssertTrue(F.FileExists('4444'));
  AssertFalse(F.FileExists('AAAA'));
  AssertFalse(F.FileExists('BBBB'));
  AssertFalse(F.FileExists('CCCC'));
  AssertFalse(F.FileExists('DDDD'));
  //
  AssertEquals(4,F.FileInfos.Count);
  F.Close;
  Sleep(50);
  F.Open;
  AssertEquals(4,F.FileInfos.Count);
  //
  AssertTrue(F.FileExists('1111'));
  AssertTrue(F.FileExists('2222'));
  AssertTrue(F.FileExists('3333'));
  AssertTrue(F.FileExists('4444'));
  AssertFalse(F.FileExists('AAAA'));
  AssertFalse(F.FileExists('BBBB'));
  AssertFalse(F.FileExists('CCCC'));
  AssertFalse(F.FileExists('DDDD'));
  //
  AssertTrue(F.RenameFile('1111','AAAA'));
  AssertTrue(F.RenameFile('2222','BBBB'));
  AssertTrue(F.RenameFile('3333','CCCC'));
  AssertTrue(F.RenameFile('4444','DDDD'));
  //
  AssertFalse(F.FileExists('1111'));
  AssertFalse(F.FileExists('2222'));
  AssertFalse(F.FileExists('3333'));
  AssertFalse(F.FileExists('4444'));
  AssertTrue(F.FileExists('AAAA'));
  AssertTrue(F.FileExists('BBBB'));
  AssertTrue(F.FileExists('CCCC'));
  AssertTrue(F.FileExists('DDDD'));
  //
  CheckFile('AAAA');
  CheckFile('BBBB');
  CheckFile('CCCC');
  CheckFile('DDDD');
  //
  F.Close;
  Sleep(250);
  F.Open;
  //
  CheckFile('AAAA');
  CheckFile('BBBB');
  CheckFile('CCCC');
  CheckFile('DDDD');
end;

procedure TTFileOperations.TestDeleteFile;
begin
  CreateFile('AAAA');
  CreateFile('BBBB');
  CreateFile('CCCC');
  CreateFile('DDDD');
  AssertTrue(F.DeleteFile('AAAA'));
  AssertTrue(F.DeleteFile('BBBB'));
  AssertFalse(F.DeleteFile('ASDF'));
  //
  AssertEquals(2,F.FileInfos.Count);
  AssertFalse(F.FileExists('AAAA'));
  AssertFalse(F.FileExists('BBBB'));
  AssertTrue(F.FileExists('CCCC'));
  AssertTrue(F.FileExists('DDDD'));
  //
  F.Close;
  Sleep(50);
  F.Open;
  //
  AssertEquals(2,F.FileInfos.Count);
  AssertFalse(F.FileExists('AAAA'));
  AssertFalse(F.FileExists('BBBB'));
  AssertTrue(F.FileExists('CCCC'));
  AssertTrue(F.FileExists('DDDD'));
  //
  AssertFalse(F.DeleteFile('AAAA'));
  AssertFalse(F.DeleteFile('BBBB'));
  AssertTrue(F.DeleteFile('CCCC'));
  AssertTrue(F.DeleteFile('DDDD'));
  AssertEquals(0,F.FileInfos.Count);
  //
  F.Close;
  Sleep(250);
  F.Open;
  //
  AssertEquals(0,F.FileInfos.Count);
end;

procedure TTFileOperations.FillStream(S: TStream; C: Char);
var
  I: Integer;
begin
  for I := 1 to 15000 do
    S.Write(C,1);
end;

procedure TTFileOperations.CheckStream(S: TStream; Filename: String);
var
  H: TStreamFileHandle;
  I: Integer;
  C1,C2: Char;
begin
  S.Position := 0;
  try
    H := F.OpenFile(Filename) as TStreamFileHandle;
    try
      AssertEquals(S.Size,H.Size);
      for I := 1 to S.Size do
        begin
          S.Read(C1,SizeOf(C1));
          H.Read(C2,SizeOf(C2));
          AssertEquals(C1,C2);
        end;
    finally
      H.Destroy;
    end;
  finally
  end;
end;

procedure TTFileOperations.TestImportExportFile;
var
  M1,M2: TMemoryStream;
begin
  M1 := TMemoryStream.Create;
  M2 := TMemoryStream.Create;
  try
    FillStream(M1,'A');
    FillStream(M2,'B');
    F.ImportFile(M1,'A');
    F.ImportFile(M2,'B');
    AssertEquals(2,F.FileInfos.Count);
    AssertEquals(0,F.Files.Count);
    CheckStream(M1,'A');
    CheckStream(M2,'B');
    M1.Clear;
    M2.Clear;
    F.ExportFile(M2,'A');
    F.ExportFile(M1,'B');
    CheckStream(M2,'A');
    CheckStream(M1,'B');
    AssertEquals(2,F.FileInfos.Count);
    AssertEquals(0,F.Files.Count);
  finally
    M1.Free;
    M2.Free;
  end;
end;

{procedure TTFileOperations.DeleteFileAAAA;
begin
  F.DeleteFile('AAAA');
end;

procedure TTFileOperations.DeleteOpenFile;
var
  S: TStreamFileHandle;
begin
  S := F.CreateFile('AAAA');
  S.WriteStr('Hello');
  S.Flush;
  AssertException(EFileError,@DeleteFileAAAA);
end;   }

procedure TTFileOperations.CreateFile(Filename: String);
var
  S: TStreamFileHandle;
  I: Integer;
begin
  AssertFalse(F.FileExists(Filename));
  S := F.CreateFile(Filename,fkStream) as TStreamFileHandle;
  for I := 1 to 4096 do
    S.Write(Filename);
  AssertEquals(Length(Filename),S.PageCount);
  S.Destroy;
end;

procedure TTFileOperations.CreatePageFile(Filename: String);
var
  P: PBufferedPage;
  S: TPageFileHandle;
  I: Integer;
begin
  AssertFalse(F.FileExists(Filename));
  S := F.CreateFile(Filename,fkPage) as TPageFileHandle;
  for I := 1 to 4 do
    begin
      P := S.Alloc;
      FillChar(P^.Data^,PAGE_SIZE,Filename[1]);
      S.Release(P);
    end;
  AssertEquals(4,S.PageCount);
  S.Destroy;
end;

procedure TTFileOperations.CheckFile(Filename: String);
var
  S: TStreamFileHandle;
  I: Integer;
  L: String;
begin
  AssertTrue(F.FileExists(Filename));
  S := F.OpenFile(Filename) as TStreamFileHandle;
  for I := 1 to 4096 do
    begin
      L := S.Read(Length(Filename));
      AssertEquals(Filename,L);
    end;
  AssertEquals(Length(Filename),S.PageCount);
  S.Destroy;
end;

procedure TTFileOperations.CheckData(var Data: TByteArray; C: Char);
var
  X: Integer;
begin
  for X := 0 to PAGE_SIZE - 1 do
    if Data[X] <> ord(C) then
      begin
        Fail('Page does not contain expected data');
        Exit;
      end;
end;

procedure TTFileOperations.CheckPageFile(Filename: String);
var
  P: PBufferedPage;
  S: TPageFileHandle;
  I: Integer;
begin
  AssertTrue(F.FileExists(Filename));
  S := F.OpenFile(Filename) as TPageFileHandle;
  for I := 0 to 3 do
    begin
      P := S.Fetch(I);
      CheckData(TByteArray(P^.Data^),Filename[1]);
      S.Release(P); // Try omitting this to make sure pages that are not explicitly freed are eventually freed when Close is called
    end;
  AssertEquals(4,S.PageCount);
  S.Destroy;
end;

procedure TTFileOperations.SetUp;
begin
  DeleteFile('test.dat');
  F := TTestableFileContainer.Create;
  F.Filename := 'test.dat';
  F.Active := True;
end;

procedure TTFileOperations.TearDown;
begin
  F.Active := False;
  F.Destroy;
end;

initialization
  RegisterTest('files',TTFileOperations);
end.

