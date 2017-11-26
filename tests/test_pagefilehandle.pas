unit test_pagefilehandle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, files;

type

  { TTPageFileHandle }

  TTPageFileHandle= class(TTestCase)
  protected
    F: TTestableFileContainer;
    P1,P2,P3: TPageFileHandle;
    D: Pointer;
    B1,B2,B3: PBufferedPage;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure InsertSampleData;
    procedure CheckSampleData;
    procedure RemoveSampleData;
    procedure PackTest;
  end;

implementation

procedure TTPageFileHandle.InsertSampleData;
begin
  B1 := P1.Alloc;
  B2 := P2.Alloc;
  B3 := P3.Alloc;
  FillChar(B1^.Data^,PAGE_SIZE,'1');
  FillChar(B2^.Data^,PAGE_SIZE,'4');
  FillChar(B3^.Data^,PAGE_SIZE,'7');
  B1^.Modified := True;
  B2^.Modified := True;
  B3^.Modified := True;
  P1.Release(B1);
  P2.Release(B2);
  P3.Release(B3);

  AssertEquals(1,P1.PageCount);
  AssertEquals(1,P2.PageCount);
  AssertEquals(1,P3.PageCount);
  AssertEquals(4,F.PageCount);
  F.Flush;
  AssertEquals(8,F.PageCount);

  B1 := P1.Alloc;
  B2 := P2.Alloc;
  B3 := P3.Alloc;
  FillChar(B1^.Data^,PAGE_SIZE,'2');
  FillChar(B2^.Data^,PAGE_SIZE,'5');
  FillChar(B3^.Data^,PAGE_SIZE,'8');
  P1.Release(B1);
  P2.Release(B2);
  P3.Release(B3);

  AssertEquals(2,P1.PageCount);
  AssertEquals(2,P2.PageCount);
  AssertEquals(2,P3.PageCount);
  AssertEquals(11,F.PageCount);
  F.Flush;
  AssertEquals(11,F.PageCount);

  B1 := P1.Alloc;
  B2 := P2.Alloc;
  B3 := P3.Alloc;
  FillChar(B1^.Data^,PAGE_SIZE,'3');
  FillChar(B2^.Data^,PAGE_SIZE,'6');
  FillChar(B3^.Data^,PAGE_SIZE,'9');
  P1.Release(B1);
  P2.Release(B2);
  P3.Release(B3);

  AssertEquals(3,P1.PageCount);
  AssertEquals(3,P2.PageCount);
  AssertEquals(3,P3.PageCount);
  AssertEquals(14,F.PageCount);  // 13 Free Pages  + 1 meta page
end;

procedure TTPageFileHandle.CheckSampleData;
var
  P: array[0..PAGE_SIZE -1] of Char;
begin
  InsertSampleData;
  B1 := P1.Fetch(0);
  P := PChar(B1^.Data);
  AssertEquals('1',P[12]);
  P1.Release(B1);
  B1 := P1.Fetch(1);
  P := PChar(B1^.Data);
  AssertEquals('2',P[56]);
  P1.Release(B1);
  B1 := P1.Fetch(2);
  P := PChar(B1^.Data);
  AssertEquals('3',P[1000]);
  P1.Release(B1);

  B2 := P2.Fetch(2);
  P := PChar(B2^.Data);
  AssertEquals('6',P[12]);
  P2.Release(B2);
  B2 := P2.Fetch(1);
  P := PChar(B2^.Data);
  AssertEquals('5',P[56]);
  P2.Release(B1);
  B2 := P2.Fetch(0);
  P := PChar(B2^.Data);
  AssertEquals('4',P[1000]);
  P2.Release(B2);

  B3 := P3.Fetch(1);
  P := PChar(B3^.Data);
  AssertEquals('8',P[12]);
  P3.Release(B3);
  B3 := P3.Fetch(0);
  P := PChar(B3^.Data);
  AssertEquals('7',P[56]);
  P3.Release(B3);
  B3 := P3.Fetch(2);
  P := PChar(B3^.Data);
  AssertEquals('9',P[1000]);
  P3.Release(B3);
end;

procedure TTPageFileHandle.RemoveSampleData;
begin
  F.AutoPack := True;
  InsertSampleData;
  P1.Delete(1);
  P2.Delete(0);
  P3.Delete(2);
  AssertEquals(11,F.PageCount);
  AssertEquals(3,F.FreePages.Count);
  P1.Delete(0);
  P2.Delete(1);
  P3.Delete(1);
  AssertEquals(8,F.PageCount);
  AssertEquals(3,F.FreePages.Count);
  P1.Delete(0);
  P2.Delete(0);
  P3.Delete(0);
  AssertEquals(5,F.PageCount);
  AssertEquals(3,F.FreePages.Count);
  F.Flush;
  AssertEquals(2,F.PageCount);
end;

procedure TTPageFileHandle.PackTest;
begin
  InsertSampleData;
  P1.Delete(1);
  P2.Delete(1);
  P3.Delete(1);
  AssertEquals(6,F.FreePages.Count);
  F.Flush;
  AssertEquals(3,F.FreePages.Count);
  F.Pack;
  AssertEquals(0,F.FreePages.Count);
end;

procedure TTPageFileHandle.SetUp;
begin
  DeleteFile('test.dat');
  F := TTestableFileContainer.Create;
  F.Filename := 'test.dat';
  F.Active := True;
  P1 := F.CreateFile('A',fkPage) as TPageFileHandle;
  P2 := F.CreateFile('B',fkPage) as TPageFileHandle;
  P3 := F.CreateFile('C',fkPage) as TPageFileHandle;
  D := GetMem(PAGE_SIZE);
end;

procedure TTPageFileHandle.TearDown;
begin
  FreeMem(D,PAGE_SIZE);
  P3.Free;
  P2.Free;
  P1.Free;
  F.Free;
end;

initialization
  RegisterTest('files',TTPageFileHandle);
end.

