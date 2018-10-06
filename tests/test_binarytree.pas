unit test_binarytree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, binarytree;

type

  { TTBinaryTree }

  TTBinaryTree= class(TTestCase)
  private
  protected
    Tree: TBinaryTree;
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

procedure TTBinaryTree.CreateDestroy;
var
  T: TBinaryTree;
begin
  T := TBinaryTree.Create(@IntegerCompareFunc);
  _SequentialInsert(100,False);
  T.Free;
end;

procedure TTBinaryTree.SequentialInsert;
begin
  _SequentialInsert(100,False);
  AssertEquals('Record Count doesn''t equal calculated record count',Tree.RecordCount,Tree.CalcRecordCount);
  AssertEquals('Wrong number of records',100,Tree.RecordCount);
  Tree.Iterate(@OnCheckData);
  AssertEquals('Didn''t iterate through all records',C,100);
  Tree.Clear;
  L := -MaxInt; C := 0;
  AssertEquals('Record count should be zero',Tree.RecordCount,0);
  AssertEquals('Record count should be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(100,True);
  AssertEquals('Record Count doesn''t equal calculated record count',Tree.RecordCount,Tree.CalcRecordCount);
  AssertEquals('Wrong number of records',100,Tree.RecordCount);
  Tree.Iterate(@OnCheckData);
  AssertEquals('Didn''t iterate through all records',C,100);
end;

procedure TTBinaryTree.RandomInsert;
begin
  _RandomInsert(100);
  AssertEquals('Record Count doesn''t equal calculated record count',Tree.RecordCount,Tree.CalcRecordCount);
  AssertEquals('Wrong number of records',N,Tree.RecordCount);
  Tree.Iterate(@OnCheckData);
  AssertEquals('Didn''t iterate through all records',C,Tree.RecordCount);
end;

procedure TTBinaryTree.SequentialSearch;
var
  X: Integer;
  D: Pointer;
begin
  _SequentialInsert(100,False);
  for X := 1 to 100 do
    begin
      AssertTrue('Expected to find node in tree',Tree.Search(X,D));
      AssertEquals('Data should equal key',X,PtrInt(D));
    end;
  Tree.Clear;
  AssertEquals('Expected record count to be zero',Tree.RecordCount,0);
  AssertEquals('Expected record count to be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(100,True);
  for X := 1 to 100 do
    begin
      AssertTrue('Expected to find node in tree',Tree.Search(X,D));
      AssertEquals('Data should equal key',X,PtrInt(D));
    end;
  Tree.Clear;
  AssertEquals('Expected record count to be zero',Tree.RecordCount,0);
  AssertEquals('Expected record count to be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(100,False);
   for X := 100 downto 1 do
     begin
       AssertTrue('Expected to find node in tree',Tree.Search(X,D));
       AssertEquals('Data should equal key',X,PtrInt(D));
     end;
  Tree.Clear;
  AssertEquals('Expected record count to be zero',Tree.RecordCount,0);
  AssertEquals('Expected record count to be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(100,True);
   for X := 100 downto 1 do
     begin
       AssertTrue('Expected to find node in tree',Tree.Search(X,D));
       AssertEquals('Data should equal key',X,PtrInt(D));
     end;
  Tree.Clear;
  AssertEquals('Expected record count to be zero',Tree.RecordCount,0);
  AssertEquals('Expected record count to be zero',Tree.CalcRecordCount,0);
end;

procedure TTBinaryTree.RandomSearch;
var
  X, R: Integer;
  D: Pointer;
begin
  R := 0;
  _RandomInsert(100);
  for X := 1 to 100 do
    if Tree.Search(X,D) then inc(R);
  AssertEquals('Number of records is different',N,R);
  Tree.Clear;
  N := 0; R := 0;
  _RandomInsert(100);
  for X := 100 downto 1 do
    if Tree.Search(X,D) then inc(R);
  AssertEquals('Number of records is different',N,R);
end;

procedure TTBinaryTree.SequentialRemove;
begin
  _SequentialInsert(100,False);
  _SequentialRemove(100,False);
  AssertEquals('Record Count should be zero',Tree.RecordCount,0);
  AssertEquals('Record Count should be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(100,True);
  _SequentialRemove(100,False);
  AssertEquals('Record Count should be zero',Tree.RecordCount,0);
  AssertEquals('Record Count should be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(100,False);
  _SequentialRemove(100,True);
  AssertEquals('Record Count should be zero',Tree.RecordCount,0);
  AssertEquals('Record Count should be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(100,True);
  _SequentialRemove(100,True);
  AssertEquals('Record Count should be zero',Tree.RecordCount,0);
  AssertEquals('Record Count should be zero',Tree.CalcRecordCount,0);
end;

procedure TTBinaryTree.RandomRemove;
var
  X,R: Integer;
begin
  _RandomInsert(100);
  R := 0;
  for X := 1 to 100 do
    if Tree.Remove(X) then
      inc(R);
  AssertEquals('Didn''t remove the expected number of records',N,R);
end;

procedure TTBinaryTree.SetUp;
begin
  Randomize;
  C := 0; L := -MaxInt; N := 0;
  Tree := TBinaryTree.Create(@IntegerCompareFunc);
end;

procedure TTBinaryTree.TearDown;
begin
  Tree.Free;
end;

procedure TTBinaryTree._SequentialInsert(Count: Integer; Descending: Boolean);
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

procedure TTBinaryTree._SequentialRemove(Count: Integer; Descending: Boolean);
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

procedure TTBinaryTree._RandomInsert(Count: Integer);
var
  I: Integer;
  R: Integer;
begin
  for I := 1 to 100 do
    begin
      R := Random(100) + 1;
      if Tree.Insert(R,PPtrInt(R)) then inc(N);
    end;
end;

procedure TTBinaryTree.OnCheckData(Key, Data: Pointer);
begin
  inc(C);
  AssertEquals('Key and data should be equal',PtrInt(Key),PtrInt(Data));
  AssertTrue('Data out of order',PtrInt(Key) > L);
  L := PtrInt(Key);
end;

initialization

  RegisterTest('Trees',TTBinaryTree);
end.

