unit test_btree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, btree;

type

  { TTBTree }

  TTBTree= class(TTestCase)
  private
  protected
    Tree: TBTree;
    C: Integer;
    L: Integer;
    N: Integer;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure _SequentialInsert(Count: Integer; Descending: Boolean = False);
    procedure _SequentialRemove(Count: Integer; Descending: Boolean);
    procedure _RandomInsert(Count: Integer);
    procedure OnCheckData(Key, Data: Pointer);
  published
    procedure CreateDestroy;
    procedure SequentialInsert;
    procedure RandomInsert;
    procedure SequentialSearch;
    procedure RandomSearch;
    procedure SequentialRemove;
    procedure RandomRemove;
  end;

implementation

procedure TTBTree.CreateDestroy;
var
  T: TBTree;
begin
  T := TBTree.Create(@IntegerCompareFunc);
  _SequentialInsert(1000,False);
  T.Free;
end;

procedure TTBTree.SequentialInsert;
begin
  _SequentialInsert(1000,False);
  AssertEquals('Record Count doesn''t equal calculated record count',Tree.RecordCount,Tree.CalcRecordCount);
  AssertEquals('Wrong number of records',1000,Tree.RecordCount);
  Tree.Iterate(@OnCheckData);
  AssertEquals('Didn''t iterate through all records',C,1000);
  Tree.Clear;
  L := -MaxInt; C := 0;
  AssertEquals('Record count should be zero',Tree.RecordCount,0);
  AssertEquals('Record count should be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(1000,True);
  AssertEquals('Record Count doesn''t equal calculated record count',Tree.RecordCount,Tree.CalcRecordCount);
  AssertEquals('Wrong number of records',1000,Tree.RecordCount);
  Tree.Iterate(@OnCheckData);
  AssertEquals('Didn''t iterate through all records',C,1000);
end;

procedure TTBTree.RandomInsert;
begin
  _RandomInsert(1000);
  AssertEquals('Record Count doesn''t equal calculated record count',Tree.RecordCount,Tree.CalcRecordCount);
  AssertEquals('Wrong number of records',N,Tree.RecordCount);
  Tree.Iterate(@OnCheckData);
  AssertEquals('Didn''t iterate through all records',C,Tree.RecordCount);
end;

procedure TTBTree.SequentialSearch;
var
  X: Integer;
  D: Pointer;
begin
  _SequentialInsert(1000,False);
  for X := 1 to 1000 do
    begin
      AssertTrue('Expected to find node in tree',Tree.Search(X,D));
      AssertEquals('Data should equal key',X,PtrInt(D));
    end;
  Tree.Clear;
  AssertEquals('Expected record count to be zero',Tree.RecordCount,0);
  AssertEquals('Expected record count to be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(1000,True);
  for X := 1 to 1000 do
    begin
      AssertTrue('Expected to find node in tree',Tree.Search(X,D));
      AssertEquals('Data should equal key',X,PtrInt(D));
    end;
  Tree.Clear;
  AssertEquals('Expected record count to be zero',Tree.RecordCount,0);
  AssertEquals('Expected record count to be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(1000,False);
   for X := 1000 downto 1 do
     begin
       AssertTrue('Expected to find node in tree',Tree.Search(X,D));
       AssertEquals('Data should equal key',X,PtrInt(D));
     end;
  Tree.Clear;
  AssertEquals('Expected record count to be zero',Tree.RecordCount,0);
  AssertEquals('Expected record count to be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(1000,True);
   for X := 1000 downto 1 do
     begin
       AssertTrue('Expected to find node in tree',Tree.Search(X,D));
       AssertEquals('Data should equal key',X,PtrInt(D));
     end;
  Tree.Clear;
  AssertEquals('Expected record count to be zero',Tree.RecordCount,0);
  AssertEquals('Expected record count to be zero',Tree.CalcRecordCount,0);
end;

procedure TTBTree.RandomSearch;
var
  X, R: Integer;
  D: Pointer;
begin
  R := 0;
  _RandomInsert(1000);
  for X := 1 to 1000 do
    if Tree.Search(X,D) then inc(R);
  AssertEquals('Number of records is different',N,R);
  Tree.Clear;
  N := 0; R := 0;
  _RandomInsert(1000);
  for X := 1000 downto 1 do
    if Tree.Search(X,D) then inc(R);
  AssertEquals('Number of records is different',N,R);
end;

procedure TTBTree.SequentialRemove;
begin
  _SequentialInsert(1000,False);
  _SequentialRemove(1000,False);
  AssertEquals('Record Count should be zero',Tree.RecordCount,0);
  AssertEquals('Record Count should be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(1000,True);
  _SequentialRemove(1000,False);
  AssertEquals('Record Count should be zero',Tree.RecordCount,0);
  AssertEquals('Record Count should be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(1000,False);
  _SequentialRemove(1000,True);
  AssertEquals('Record Count should be zero',Tree.RecordCount,0);
  AssertEquals('Record Count should be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(1000,True);
  _SequentialRemove(1000,True);
  AssertEquals('Record Count should be zero',Tree.RecordCount,0);
  AssertEquals('Record Count should be zero',Tree.CalcRecordCount,0);
end;

procedure TTBTree.RandomRemove;
var
  X,R: Integer;
begin
  _RandomInsert(1000);
  R := 0;
  for X := 1 to 1000 do
    if Tree.Remove(X) then
      inc(R);
  AssertEquals('Didn''t remove the expected number of records',N,R);
end;

procedure TTBTree.SetUp;
begin
  Randomize;
  C := 0; L := -MaxInt; N := 0;
  Tree := TBTree.Create(@IntegerCompareFunc);
end;

procedure TTBTree.TearDown;
begin
  Tree.Free;
end;

procedure TTBTree._SequentialInsert(Count: Integer; Descending: Boolean);
var
  I: Integer;
begin
  if Descending then
    for I := Count downto 1 do
      Tree.Insert(I,PPtrInt(I))
  else
    for I := 1 to Count do
      Tree.Insert(I,PPtrInt(I));
end;

procedure TTBTree._SequentialRemove(Count: Integer; Descending: Boolean);
var
  I: Integer;
begin
  if Descending then
    for I := Count downto 1 do
      Tree.Remove(I)
  else
    for I := 1 to Count do
      Tree.Remove(I);
end;

procedure TTBTree._RandomInsert(Count: Integer);
var
  I: Integer;
  R: Integer;
begin
  for I := 1 to 1000 do
    begin
      R := Random(1000) + 1;
      if Tree.Insert(R,PPtrInt(R)) then inc(N);
    end;
end;

procedure TTBTree.OnCheckData(Key, Data: Pointer);
begin
  inc(C);
  AssertEquals('Key and data should be equal',PtrInt(Key),PtrInt(Data));
  AssertTrue('Data out of order',PtrInt(Key) > L);
  L := PtrInt(Key);
end;

initialization

  RegisterTest('Trees',TTBTree);
end.

