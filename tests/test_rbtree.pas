unit test_rbtree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, rbtree;

type
  TRBTreeData = record
    Key: Integer;
    Data: Integer;
  end;
  PRBTreeData = ^TRBTreeData;

  { TTRBTree }

  TTRBTree= class(TTestCase)
  private
    function CompareFunc(Data1, Data2: Pointer): Integer;
  protected
    Tree: TRBTree;
    C: Integer;
    L: Integer;
    N: Integer;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure _SequentialInsert(Count: Integer; Descending: Boolean = False);
    procedure _SequentialRemove(Count: Integer; Descending: Boolean);
    procedure _RandomInsert(Count: Integer);
    procedure OnCheckData(Data: Pointer; var Abort: Boolean);
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

function TTRBTree.CompareFunc(Data1, Data2: Pointer): Integer;
begin
  if PRBTreeData(Data1)^.Key > PRBTreeData(Data2)^.Key then
    Result := 1
  else
    if PRBTreeData(Data1)^.Key < PRBTreeData(Data2)^.Key then
      Result := -1
    else
      Result := 0;
end;

procedure TTRBTree.CreateDestroy;
var
  T: TRBTree;
begin
  T := TRBTree.Create(@CompareFunc,SizeOf(TRBTreeData));
  _SequentialInsert(100,False);
  T.Free;
end;

procedure TTRBTree.SequentialInsert;
begin
  _SequentialInsert(100,False);
  AssertEquals('Record Count doesn''t equal calculated record count',Tree.RecordCount,Tree.CalcRecordCount);
  AssertEquals('Wrong number of records',100,Tree.RecordCount);
  L := -MaxInt; C := 0;
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

procedure TTRBTree.RandomInsert;
begin
  _RandomInsert(100);
  AssertEquals('Record Count doesn''t equal calculated record count',Tree.RecordCount,Tree.CalcRecordCount);
  AssertEquals('Wrong number of records',N,Tree.RecordCount);
  Tree.Iterate(@OnCheckData);
  AssertEquals('Didn''t iterate through all records',C,Tree.RecordCount);
end;

procedure TTRBTree.SequentialSearch;
var
  X: Integer;
  D: Pointer;
  R: TRBTreeData;
begin
  _SequentialInsert(100,False);
  for X := 1 to 100 do
    begin
      R.Key := X;
      R.Data := -1;
      AssertTrue('Expected to find node in tree',Tree.Search(@R));
      AssertEquals('Data should equal key',R.Key,R.Data);
    end;
  Tree.Clear;
  AssertEquals('Expected record count to be zero',Tree.RecordCount,0);
  AssertEquals('Expected record count to be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(100,True);
  for X := 1 to 100 do
    begin
      R.Key := X;
      R.Data := -1;
      AssertTrue('Expected to find node in tree',Tree.Search(@R));
      AssertEquals('Data should equal key',R.Key,R.Data);
    end;
  Tree.Clear;
  AssertEquals('Expected record count to be zero',Tree.RecordCount,0);
  AssertEquals('Expected record count to be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(100,False);
   for X := 100 downto 1 do
     begin
       R.Key := X;
       R.Data := -1;
       AssertTrue('Expected to find node in tree',Tree.Search(@R));
       AssertEquals('Data should equal key',R.Key,R.Data);
     end;
  Tree.Clear;
  AssertEquals('Expected record count to be zero',Tree.RecordCount,0);
  AssertEquals('Expected record count to be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(100,True);
   for X := 100 downto 1 do
     begin
       R.Key := X;
       R.Data := -1;
       AssertTrue('Expected to find node in tree',Tree.Search(@R));
       AssertEquals('Data should equal key',R.Key,R.Data);
     end;
  Tree.Clear;
  AssertEquals('Expected record count to be zero',Tree.RecordCount,0);
  AssertEquals('Expected record count to be zero',Tree.CalcRecordCount,0);
end;

procedure TTRBTree.RandomSearch;
var
  X, Cnt: Integer;
  R: TRBTreeData;
  D: Pointer;
begin
  Cnt := 0;
  _RandomInsert(100);
  for X := 1 to 100 do
    begin
      R.Key := X;
      if Tree.Search(@R) then
        inc(Cnt)
    end;
  AssertEquals('Number of records is different',N,Cnt);
  Tree.Clear;
  N := 0; Cnt := 0;
  _RandomInsert(100);
  for X := 100 downto 1 do
    begin
      R.Key := X;
      if Tree.Search(@R) then
        inc(Cnt);
    end;
  AssertEquals('Number of records is different',N,Cnt);
end;

procedure TTRBTree.SequentialRemove;
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

procedure TTRBTree.RandomRemove;
var
  X,Cnt: Integer;
  R: TRBTreeData;
begin
  _RandomInsert(100);
  Cnt := 0;
  for X := 1 to 100 do
    begin
      R.Key := X;
      if Tree.Remove(@R) then
        inc(Cnt);
    end;
  AssertEquals('Didn''t remove the expected number of records',N,Cnt);
end;

procedure TTRBTree.SetUp;
begin
  Randomize;
  C := 0; L := -MaxInt; N := 0;
  Tree := TRBTree.Create(@CompareFunc,SizeOf(TRBTreeData));
end;

procedure TTRBTree.TearDown;
begin
  Tree.Free;
end;

procedure TTRBTree._SequentialInsert(Count: Integer; Descending: Boolean);
var
  I: Integer;
  R: TRBTreeData;
begin
  if Descending then
    for I := Count downto 1 do
      begin
        R.Key := I;
        R.Data := I;
        Tree.Insert(@R);
      end
  else
    for I := 1 to Count do
      begin
        R.Key := I;
        R.Data := I;
        Tree.Insert(@R);
      end;
end;

procedure TTRBTree._SequentialRemove(Count: Integer; Descending: Boolean);
var
  I: Integer;
begin
  if Descending then
    for I := Count downto 1 do
      Tree.Remove(@I)
  else
    for I := 1 to Count do
      Tree.Remove(@I);
end;

procedure TTRBTree._RandomInsert(Count: Integer);
var
  I: Integer;
  R: TRBTreeData;
begin
  for I := 1 to Count do
    begin
      R.Key := Random(100) + 1;
      R.Data := R.Key;
      if Tree.Insert(@R) then inc(N);
    end;
end;

procedure TTRBTree.OnCheckData(Data: Pointer; var Abort: Boolean);
begin
  inc(C);
  AssertEquals('Key and data should be equal',PRBTreeData(Data)^.Key,PRBTreeData(Data)^.Data);
  AssertTrue('Data out of order',PRBTreeData(Data)^.Key > L);
  L := PRBTreeData(Data)^.Key;
end;

initialization
  RegisterTest('Trees',TTRBTree);
end.

