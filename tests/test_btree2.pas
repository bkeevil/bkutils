unit test_btree2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, btree;

type

  TRec = record
    Key: Integer;
    Data: Integer;
  end;
  PRec = ^TRec;

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
    procedure OnCheckData(Rec: Pointer);
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
  T := TBTree.Create(@IntegerCompareFunc,8,6);
  AssertEquals(6,T.Order);
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
  Rec: TRec;
  X: Integer;
begin
  _SequentialInsert(1000,False);
  for X := 1 to 1000 do
    begin
      Rec.Key := X;
      AssertTrue('Expected to find node in tree',Tree.Search(@Rec));
      AssertEquals('Data should equal key',Rec.Key,Rec.Data);
    end;
  Tree.Clear;
  AssertEquals('Expected record count to be zero',Tree.RecordCount,0);
  AssertEquals('Expected record count to be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(1000,True);
  for X := 1 to 1000 do
    begin
      Rec.Key := X;
      AssertTrue('Expected to find node in tree',Tree.Search(@Rec));
      AssertEquals('Data should equal key',Rec.Key,Rec.Data);
    end;
  Tree.Clear;
  AssertEquals('Expected record count to be zero',Tree.RecordCount,0);
  AssertEquals('Expected record count to be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(1000,False);
   for X := 1000 downto 1 do
     begin
       Rec.Key := X;
       AssertTrue('Expected to find node in tree',Tree.Search(@Rec));
       AssertEquals('Data should equal key',Rec.Key,Rec.Data);
     end;
  Tree.Clear;
  AssertEquals('Expected record count to be zero',Tree.RecordCount,0);
  AssertEquals('Expected record count to be zero',Tree.CalcRecordCount,0);
  _SequentialInsert(1000,True);
   for X := 1000 downto 1 do
     begin
       Rec.Key := X;
       AssertTrue('Expected to find node in tree',Tree.Search(@Rec));
       AssertEquals('Data should equal key',Rec.Key,Rec.Data);
     end;
  Tree.Clear;
  AssertEquals('Expected record count to be zero',Tree.RecordCount,0);
  AssertEquals('Expected record count to be zero',Tree.CalcRecordCount,0);
end;

procedure TTBTree.RandomSearch;
var
  X,R: Integer;
  Rec: TRec;
begin
  R := 0;
  _RandomInsert(1000);
  Tree.Iterate(@OnCheckData);
  for X := 1 to 1000 do
    begin
      Rec.Key := X;
      Rec.Data := 0;
      if Tree.Search(@Rec) then
        begin
          inc(R);
          AssertEquals(Rec.Key,Rec.Data);
        end;
    end;
  AssertEquals('Number of records is different',N,R);
  L := -MaxInt;
  Tree.Iterate(@OnCheckData);
  Tree.Clear;
  AssertEquals(0,Tree.RecordCount);
  AssertEquals(0,Tree.CalcRecordCount);
  R := 0; N := 0; L := -MaxInt;
  _RandomInsert(1000);
  Tree.Iterate(@OnCheckData);
  for X := 1000 downto 1 do
    begin
      Rec.Key := X;
      Rec.Data := 0;
      if Tree.Search(@Rec) then
        begin
          inc(R);
          AssertEquals(Rec.Key,Rec.Data);
        end;
    end;
  L := -MaxInt;
  Tree.Iterate(@OnCheckData);
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
    if Tree.Remove(@X) then
      inc(R);
  AssertEquals('Didn''t remove the expected number of records',N,R);
end;

procedure TTBTree.SetUp;
begin
  Randomize;
  C := 0; L := -MaxInt; N := 0;
  Tree := TBTree.Create(@IntegerCompareFunc,8,6);
end;

procedure TTBTree.TearDown;
begin
  Tree.Free;
end;

procedure TTBTree._SequentialInsert(Count: Integer; Descending: Boolean);
var
  I: Integer;
  Rec: TRec;
begin
  if Descending then
    for I := Count downto 1 do
      begin
        Rec.Key := I;
        Rec.Data := I;
        Tree.Insert(@Rec);
      end
  else
    for I := 1 to Count do
      begin
        Rec.Key := I;
        Rec.Data := I;
        Tree.Insert(@Rec);
      end
end;

procedure TTBTree._SequentialRemove(Count: Integer; Descending: Boolean);
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

procedure TTBTree._RandomInsert(Count: Integer);
var
  I: Integer;
  R: Integer;
  Rec: TRec;
begin
  for I := 1 to 1000 do
    begin
      R := Random(1000) + 1;
      Rec.Key := R;
      Rec.Data := R;
      if Tree.Insert(@Rec) then inc(N);
    end;
end;

procedure TTBTree.OnCheckData(Rec: Pointer);
begin
  inc(C);
  AssertEquals('Key and data should be equal',PRec(Rec)^.Key,PRec(Rec)^.Data);
  AssertTrue('Data out of order',PRec(Rec)^.Key > L);
  L := PRec(Rec)^.Key;
end;

initialization
  RegisterTest('Trees',TTBTree);
end.

