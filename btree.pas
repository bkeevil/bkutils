unit btree;
{ btree.pas - A unit that provides a memory-based BTree
  Copyright (C) 2013  H. Bond Keevil

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.

  DOCUMENTATION IS HERE:
  http://prognosti.com/index.php?option=com_content&view=article&id=17

  For assistance or support: bkeevil@prognosti.com }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TBTree = class;

  { TBTreeNode }

  TBTreeNode = class(TObject)
    private
      FTree: TBTree;
      FBuffer: Pointer;
      function GetCount: Word;
      procedure SetCount(AValue: Word);
      function GetData(Index: Word): Pointer;
      function GetNext(Index: Word): TBTreeNode;
      procedure SetNext(Index: Word; AValue: TBTreeNode);
    public
      constructor Create(ATree: TBTree);
      destructor Destroy; override;
      property Tree: TBTree read FTree;
      property Count: Word read GetCount write SetCount;
      property Data[Index: Word]: Pointer read GetData;
      property Next[Index: Word]: TBTreeNode read GetNext write SetNext;
  end;

  TIteratorProc = procedure (Rec: Pointer) of object;
  TCompareFunc = function (Rec1, Rec2: Pointer): Integer;

type
  { TBTree }
  EBTree = class(Exception);
  TBTree = class(TObject)
    private
      FNodeCount: Integer;
      FRecordCount: Integer;
      FRecordSize: Word;
      FPageSize: Word;
      FOrder: Word;
      _Compare: TCompareFunc;
      procedure NodeSearch(Rec: Pointer; Node: TBTreeNode; var Found: Boolean; var Location: Integer);
      procedure InsertHere(Root: TBTreeNode; Rec: Pointer; Location: Integer; var Fixup: TBTreeNode);
      procedure _Insert(Root: TBTreeNode; Rec: Pointer; var Found: Boolean; var Fixup: TBTreeNode);
      procedure Adjust(var Root: TBTreeNode; Location: Integer);
      procedure DeleteHere(Root: TBTreeNode; Location: Integer);
      procedure _Remove(Root: TBTreeNode; Rec: Pointer; var Found: Boolean);
    protected
      procedure B_Search(Rec: Pointer; P: TBTreeNode; var Found: Boolean; var Node: TBTreeNode; var Location: Integer);
      procedure B_Insert(var Root: TBTreeNode; Rec: Pointer; var Found: Boolean);
      procedure B_Remove(var Root: TBTreeNode; Rec: Pointer; var Found: Boolean);
      function B_Count(Root: TBTreeNode): Integer;
      procedure B_Iterate(Root: TBTreeNode; proc: TIteratorProc);
      procedure B_Clear(Root: TBTreeNode);
    public
      FRoot: TBTreeNode;
      constructor Create(Compare: TCompareFunc; RecordSize, Order: Word);
      destructor Destroy; override;
      class function CalculateOrder(RecordSize: Word; PageSize: Word): Word;
      class function CalculatePageSize(RecordSize: Word; Order: Word): Word;
      function Search(Rec: Pointer): Boolean;
      function Insert(Rec: Pointer): Boolean;
      function Update(Rec: Pointer): Boolean;
      function Remove(Rec: Pointer): Boolean;
      procedure Iterate(Proc: TIteratorProc);
      procedure Clear;
      function CalcRecordCount: Integer;
      property RecordCount: Integer read FRecordCount;
      property NodeCount: Integer read FNodeCount;
      property Order: Word read FOrder;
      property RecordSize: Word read FRecordSize;
      property PageSize: Word read FPageSize;
  end;

function IntegerCompareFunc(Rec1, Rec2: Pointer): Integer;

implementation

// Overflow save integer compare
function IntegerCompareFunc(Rec1, Rec2: Pointer): Integer;
begin
  if PInteger(Rec1)^ > PInteger(Rec2)^ then
    Result := 1
  else
    if PInteger(Rec1)^ < PInteger(Rec2)^ then
      Result := -1
    else
      Result := 0;
end;

{ TBTreeNode }

constructor TBTreeNode.Create(ATree: TBTree);
begin
  FTree := ATree;
  inc(FTree.FNodeCount);
  GetMem(FBuffer,FTree.FPageSize);
  FillChar(FBuffer^,FTree.FPageSize,0);
end;

destructor TBTreeNode.Destroy;
begin
  dec(FTree.FNodeCount);
  FreeMem(FBuffer,FTree.FPageSize);
  inherited Destroy;
end;

function TBTreeNode.GetCount: Word;
begin
  Result := PWord(FBuffer)^;
end;

procedure TBTreeNode.SetCount(AValue: Word);
begin
  PWord(FBuffer)^ := AValue;
end;

function TBTreeNode.GetData(Index: Word): Pointer;
var
  Offset: PtrInt;
begin
  Offset := SizeOf(Word) + ((Index - 1) * FTree.FRecordSize);
  Result := Pointer(FBuffer + Offset);
end;

function TBTreeNode.GetNext(Index: Word): TBTreeNode;
var
  Offset: PtrInt;
  P: Pointer;
begin
  Offset := SizeOf(Word) + (FTree.FOrder * FTree.FRecordSize) + (Index * SizeOf(Pointer));
  P := FBuffer+Offset;
  Result := TBTreeNode(P^);
end;

procedure TBTreeNode.SetNext(Index: Word; AValue: TBTreeNode);
var
  Offset: PtrInt;
begin
  Offset := SizeOf(Word) + (FTree.FOrder * FTree.FRecordSize) + (Index * SizeOf(Pointer));
  PPointer(FBuffer+Offset)^ := AValue;
end;

{ TBTree }

constructor TBTree.Create(Compare: TCompareFunc; RecordSize, Order: Word);
begin
  inherited Create;
  if Order < 6 then
    raise EBTree.Create('BTree order must be at least 6');
  _Compare := Compare;
  FRecordSize := RecordSize;
  FOrder := Order;
  FPageSize := ((SizeOf(Pointer) + FRecordSize) * FOrder) + 2 + SizeOf(Pointer);
end;

destructor TBTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

class function TBTree.CalculateOrder(RecordSize: Word; PageSize: Word): Word;
begin
  Result := (PageSize - SizeOf(Word) - SizeOf(Pointer)) div (RecordSize + SizeOf(Pointer));
end;

class function TBTree.CalculatePageSize(RecordSize: Word; Order: Word): Word;
begin
  Result := SizeOf(Word) + SizeOf(Pointer) + ((SizeOf(Pointer) + RecordSize) * Order);
end;

function TBTree.Search(Rec: Pointer): Boolean;
var
  Node: TBTreeNode;
  Location: Integer;
begin
  B_Search(Rec,FRoot,Result,Node,Location);
  if Result then
    begin
      Assert(Assigned(Node),'Found a BTree node but node was nil');
      Move(Node.Data[Location]^,Rec^,FRecordSize);
    end;
end;

function TBTree.Insert(Rec: Pointer): Boolean;
begin
  B_Insert(FRoot,Rec,Result);
  Result := not Result;
end;

function TBTree.Update(Rec: Pointer): Boolean;
var
  Node: TBTreeNode = nil;
  Location: Integer;
begin
  if (FRoot = nil) then
    FRoot := TBTreeNode.Create(Self);
  B_Search(Rec,FRoot,Result,Node,Location);
  if Result then
    begin
      Assert(Assigned(Node),'Found a BTree node but node was nil');
      Move(Rec^,Node.Data[Location]^,RecordSize);
      if Node <> FRoot then
        Node.Destroy;
    end;
end;

function TBTree.Remove(Rec: Pointer): Boolean;
begin
  B_Remove(FRoot,Rec,Result);
end;

procedure TBTree.Iterate(Proc: TIteratorProc);
begin
  B_Iterate(FRoot,Proc);
end;

procedure TBTree.Clear;
begin
  B_Clear(FRoot);
  FRoot := nil;
  Assert(FNodeCount = 0,'NodeCount should be zero');
  Assert(FRecordCount = 0,'RecordCount should be zero');
end;

function TBTree.CalcRecordCount: Integer;
begin
  Result := B_Count(FRoot);
end;

function TBTree.B_Count(Root: TBTreeNode): Integer;
var
  I: Integer;
  N: TBTreeNode;
begin
  if Root = nil then
    begin
      Result := 0;
      Exit;
    end
  else
    begin
      Result := Root.Count;
      for I := 0 to Result do
        begin
          N := Root.Next[I];
          if N <> nil then
            Result := Result + B_Count(N);
        end;
    end;
end;

procedure TBTree.B_Iterate(Root: TBTreeNode; Proc: TIteratorProc);
var
  I: Integer;
  N: TBTreeNode;
begin
  if Root = nil then Exit;
  for I := 1 to Root.Count do
    begin
      N := Root.Next[I-1];
      if N <> nil then
        B_Iterate(N,Proc);
      Proc(Root.Data[I]);
    end;
  N := Root.Next[Root.Count];
  if N <> nil then
    B_Iterate(N,Proc);
end;

procedure TBTree.B_Clear(Root: TBTreeNode);
var
  I: Integer;
  N: TBTreeNode;
begin
  if Root = nil then Exit;
  for I := 0 to Root.Count do
    begin
      N := Root.Next[I];
      if N <> nil then
        B_Clear(N);
    end;
  FRecordCount := FRecordCount - Root.Count;
  Root.Free;
end;

procedure TBTree.NodeSearch(Rec: Pointer; Node: TBTreeNode; var Found: Boolean; var Location: Integer);
{Searches node for target}
var
  First, Mid, Last: Integer;
  CompareResult: Integer;
begin
  Found := False;
  Location := -1;
  with Node do
    if Count > 0 then   { Don't search empty list }
      begin
        First := 1;
        Last := Count;
        while Last > First do   { Usual binary search }
          begin
            Mid := (Last + First) div 2;
            CompareResult := _Compare(Rec,Data[Mid]);
            if CompareResult > 0 then
              First := Mid + 1
            else
              Last := Mid;
          end;
        Location := Last;
        CompareResult := _Compare(Rec,Data[Last]);
        if CompareResult < 0 then
          Location := Last - 1  {Adjust subscript }
        else
          if CompareResult = 0 then
            Found := True;     {Indicate Success }
      end;
end;

procedure TBTree.B_Search(Rec: Pointer; P: TBTreeNode; var found: Boolean; var Node: TBTreeNode; var Location: Integer);
{Searches B-Tree for target, reports success or failure in Found; returns node and Location if found }
begin
  if P = nil then
    Found := False
  else
    begin
      NodeSearch(Rec,P,Found,Location);   { try current node }
      if Found then
        Node := P  { return root as node }
      else         { make recursive call }
        B_Search(Rec,P.Next[Location],Found,Node,Location);
    end;
end;

procedure TBTree.InsertHere(Root: TBTreeNode; Rec: Pointer; Location: Integer; var Fixup: TBTreeNode);
{Inserts record in this node, splits node if it overflows.  If split occured, returns with Fixup pointing to new node and with entry holding record to be moved up }
var
  I,J,Middle: Integer;
  TempRec: Pointer;         { overflow }
  TempPtr: TBTreeNode;      { area }
begin
  GetMem(TempRec,FRecordSize);
  with Root do
    begin
      if Location > Count+1 then  { Allow caller to append without knowing count }
        Location := Count + 1;
      Move(Data[FOrder]^,TempRec^,FRecordSize);
      TempPtr := Next[FOrder];

      for I := FOrder downto Location + 1 do
        begin
          Move(Data[I-1]^,Data[I]^,FRecordSize);
          Next[I] := Next[I-1];
        end;

      if Location <= FOrder then
        begin                  { insert in node }
          Move(Rec^,Data[Location]^,FRecordSize);
          Next[Location] := Fixup;
        end
      else
        begin                  { Save for split }
          Move(Rec^,TempRec^,FRecordSize);
          TempPtr := Fixup;
        end;
      Count := Count + 1;
      if Count <= FOrder then
        Fixup := nil               { overflow? }
      else
        begin
          Middle := FOrder div 2 + 1;    { --yes: split node }
          Count := Middle - 1;
          Fixup := TBTreeNode.Create(Self);
          Fixup.Next[0] := Next[Middle];
          J := 1;
          for I := Middle + 1 to FOrder do
            begin
              Move(Data[I]^,Fixup.Data[J]^,FRecordSize);
              Fixup.Next[J] := Next[I];
              Next[I] := nil;
              FillChar(Data[I]^,FRecordSize,0);
              J := J + 1;
            end;
          Next[Middle] := nil;
          Move(TempRec^,Fixup.Data[J]^,FRecordSize);
          Fixup.Next[J] := TempPtr;
          Fixup.Count := J;
          Move(Data[Middle]^,Rec^,FRecordSize);
          FillChar(Data[Middle]^,FRecordSize,0);
        end;
    end;
  FreeMem(TempRec,FRecordSize);
end;

procedure TBTree._Insert(Root: TBTreeNode; Rec: Pointer; var Found: Boolean; var Fixup: TBTreeNode);
var  { procedure for entering new item in a node }
  Location: Integer;
begin
  NodeSearch(Rec,Root,Found,Location); { look for it here }
  if not Found then  { if found we're in trouble }
    with Root do
      begin
        if Next[Location] = nil then    { at a leaf? }
          InsertHere(Root,Rec,Location+1,Fixup)
        else
          begin  { no: recurse }
            _Insert(Next[Location],Rec,Found,Fixup);
            if Fixup <> nil then      { Insertion split node?}
              InsertHere(Root,Rec,Location+1,Fixup);   { -- yes: insert record }
          end;
      end;
end;

procedure TBTree.B_Insert(var Root: TBTreeNode; Rec: Pointer; var Found: Boolean);
var           { main insertion routine for b-trees }
  P, Fixup: TBTreeNode;
begin
  Found := False;
  Fixup := nil;
  if Root <> nil then
    begin                       { tree already exists: insert record in it }
      _Insert(Root,Rec,Found,Fixup);
      P := Root;
    end;
  if (Root = nil) or (Fixup <> nil) then
    begin                           { either no root yet or must have split }
      P := Root;
      Root := TBTreeNode.Create(Self);
      Root.Count := 1;
      Move(Rec^,Root.Data[1]^,FRecordSize);
      Root.Next[0] := P;
      Root.Next[1] := Fixup;
    end;
  if not Found then inc(FRecordCount);
end;

procedure TBTree.Adjust(var Root: TBTreeNode; Location: Integer);
{ Fixes up nodes with too few records }
var
  T, Temp: TBTreeNode;
  Fixup, Q, R: TBTreeNode;
  Rec1, Rec2: Pointer;
  I,LeftCount,RightCount: Integer;
  Min: Word;
begin
  GetMem(Rec1,FRecordSize);
  GetMem(Rec2,FRecordSize);
  Min := FOrder div 2;
  with Root do
    begin
      if Location = 0 then
        LeftCount := 0                { no left neighbour }
      else
        begin
          T := Next[Location-1];
          LeftCount := T.Count;
        end;

      if Location = Count then             { no right neighbour }
        RightCount := 0
      else
        begin
          T := Next[Location+1];
          RightCount := T.Count;
        end;

      if LeftCount > Min then         { rotate right }
        begin
          Fixup := Next[Location].Next[0];
          Q := Next[Location - 1].Next[LeftCount];
          Move(Next[Location-1].Data[LeftCount]^,Rec1^,FRecordSize);
          Move(Data[Location]^,Rec2^,FRecordSize);
          InsertHere(Next[Location],Rec2,1,Fixup);
          DeleteHere(Next[Location-1],MaxInt);
          Next[Location].Next[0] := Q;
          Move(Rec1^,Data[Location]^,FRecordSize);
        end
      else
        if RightCount > Min then    { Rotate left }
          begin
            Fixup := Next[Location+1].Next[0];
            Move(Next[Location+1].Data[1]^,Rec1^,FRecordSize);
            Move(Data[Location+1]^,Rec2^,FRecordSize);
            InsertHere(Next[Location],Rec2,MaxInt,Fixup);
            Q := Next[Location+1].Next[1];
            DeleteHere(Next[Location+1],1);
            Next[Location+1].Next[0] := Q;
            Move(Rec1^,Data[Location+1]^,FRecordSize);
          end
        else
          begin  { Merge }
            if LeftCount > RightCount then
              Location := Location - 1;
            Q := Next[Location];
            R := Next[Location+1];
            Fixup := R.Next[0];
            InsertHere(Q,Data[Location+1],MaxInt,Fixup);
            for I := 1 to R.Count do
              begin
                Temp := R.Next[I];
                InsertHere(Q,R.Data[I],MaxInt,Temp);
              end;
            R.Free;
            R := nil;
            DeleteHere(Root,Location+1);
          end;
    end;
  FreeMem(Rec1,FRecordSize);
  FreeMem(Rec2,FRecordSize);
end;

procedure TBTree.DeleteHere(Root: TBTreeNode; Location: Integer);
{ Removes record at Location and adjusts count }
var
  I: Integer;
begin
  with Root do
    begin
      if Location > Count then {Allows caller to access end without knowing count }
        Location := Count;
      for I := Location to FOrder - 1 do
        begin
          Move(Data[I+1]^,Data[I]^,FRecordSize);
          Next[I] := Next[I+1];
        end;
      FillChar(Data[FOrder]^,FRecordSize,0);
      Next[FOrder] := nil;
      Count := Count - 1;
    end;
end;

procedure TBTree._Remove(Root: TBTreeNode; Rec: Pointer; var Found: Boolean);
{ Recursive B-tree deletion procedure: finds node and calls delete_here for final removal }
var
  P: TBTreeNode;
  Location: Integer;
begin
  NodeSearch(Rec,Root,Found,Location);   { look for it here }
  with Root do
    begin
      if Found then
        if Next[Location - 1] = nil then   { if leaf, delete }
          DeleteHere(Root,Location)
        else
          begin
            p := Next[Location];
            while P.Next[0] <> nil do   { else find successor }
              P := P.Next[0];
            Move(P.Data[1]^,Data[Location]^,FRecordSize);
            _Remove(Next[Location],Data[Location],Found);
          end
      else       { not Found }        { Recursive Call }
        if Next[Location] <> nil then
          _Remove(Next[Location],Rec,Found);
      {Fixup if necessary }
      if (Next[Location] <> nil) and (Next[Location].Count < (FOrder div 2)) then
        Adjust(Root,Location);
    end;
end;

procedure TBTree.B_Remove(var Root: TBTreeNode; Rec: Pointer; var Found: Boolean);
{main procedure for deletion from a B-Tree }
var
  P: TBTreeNode;
begin
  Found := False;
  if Root <> nil then        { Don't delete from an empty tree }
    begin
      _Remove(Root,Rec,Found);
      if Root.Count = 0 then   { root empty?}
        begin
          P := Root;   { yes: Next level down is root }
          Root := Root.Next[0];
          P.Free;
        end;
    end;
  if Found then
    begin
      dec(FRecordCount);
      Assert(FRecordCount >= 0,'BTree record count less than zero');
    end;
end;

end.

