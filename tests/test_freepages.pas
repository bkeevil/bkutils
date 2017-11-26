unit test_freepages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, files;

type

  TTFreePages= class(TTestCase)
  protected
    C: TTestableFileContainer;
    F: TFreePages;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AddGet;
  end;

implementation

procedure TTFreePages.AddGet;
begin
  F.Add(1);
  F.Add(2);
  F.Add(3);
  F.Add(4);
  F.Add(5);
  AssertEquals('Count',5,F.Count);
  AssertEquals(1,F.Get);
  AssertEquals(2,F.Get);
  AssertEquals(3,F.Get);
  AssertEquals(4,F.Get);
  AssertEquals(5,F.Get);
  AssertEquals('Count',0,F.Count);
  F.Add(2);
  F.Add(4);
  F.Add(3);
  F.Add(1);
  F.Add(5);
  AssertEquals('Count',5,F.Count);
  AssertEquals(1,F.Get);
  AssertEquals(2,F.Get);
  AssertEquals(3,F.Get);
  AssertEquals(4,F.Get);
  AssertEquals(5,F.Get);
  AssertEquals('Count',0,F.Count);
  F.Add(5);
  F.Add(4);
  F.Add(3);
  F.Add(2);
  F.Add(1);
  AssertEquals('Count',5,F.Count);
  AssertEquals(1,F.Get);
  AssertEquals(2,F.Get);
  AssertEquals(3,F.Get);
  AssertEquals(4,F.Get);
  AssertEquals(5,F.Get);
  AssertEquals('Count',0,F.Count);
end;

procedure TTFreePages.SetUp;
begin
  C := TTestableFileContainer.Create;
  F := C.FreePages;
end;

procedure TTFreePages.TearDown;
begin
  C.Free;
end;

initialization
  RegisterTest('files',TTFreePages);
end.
